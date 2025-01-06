/*
Copyright (c) 2009-2011 Martin Logan, Eric Merritt, Richard Carlsson

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to
deal in the Software without restriction, including without limitation the
rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
IN THE SOFTWARE.

Copyright (c) 2012 Grzegorz Junka
Change to which native Erlang types JSON arrays and objects are parsed.

License and the original sources based on chapter 12 of repository:
https://github.com/erlware/Erlang-and-OTP-in-Action-Source
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <erl_nif.h>

#include <yajl/yajl_parse.h>

#define ERR_BUF_SIZE 256

/* yajl callback prototypes */
static int handle_null(void *ctx);
static int handle_boolean(void *ctx, int boolVal);
static int handle_integer(void *ctx, long long integerVal);
static int handle_double(void *ctx, double doubleVal);
static int handle_string(void *ctx, const unsigned char *stringVal, size_t stringLen);
static int handle_map_key(void *ctx, const unsigned char *stringVal, size_t stringLen);
static int handle_start_map(void *ctx);
static int handle_end_map(void *ctx);
static int handle_start_array(void *ctx);
static int handle_end_array(void *ctx);


static yajl_callbacks callbacks = {
  handle_null,
  handle_boolean,
  handle_integer, /* note: only handles long integers, not bignums */
  handle_double,
  NULL, /* any number - if defined, integer/double are not used */
  handle_string,
  handle_start_map,
  handle_map_key,
  handle_end_map,
  handle_start_array,
  handle_end_array
};


typedef struct container_t {
  int count; /* number of elements */
  int arraysz; /* size of elements array */
  ERL_NIF_TERM *array; /* elements array */
  int key; /* true if last term was a key */
  struct container_t *next;
} container_t;

typedef struct {
  ErlNifEnv *env; /* NIF environment */
  container_t *c; /* innermost container */
  char errmsg[ERR_BUF_SIZE];
} state_t;

static void cleanup_containers(container_t *c) {
    while (c != NULL && c->next != NULL) { 
        container_t *next = c->next;
        if (c->array) {
            enif_free(c->array);
        }
        enif_free(c);
        c = next;
    }
}


static void *alloc_func(void *ctx, size_t sz)
{
  return enif_alloc(sz);
}

static void *realloc_func(void *ctx, void *ptr, size_t sz)
{
  return enif_realloc(ptr, sz);
}

static void free_func(void *ctx, void *ptr)
{
  enif_free(ptr);
}

static yajl_alloc_funcs alloc_funcs = {
  alloc_func,
  realloc_func,
  free_func,
  NULL /* must be initialized below */
};


static const char *parse_json(state_t *st, unsigned char *buf, size_t len)
{
    yajl_status ys;
    const char *err = NULL;
    
    alloc_funcs.ctx = st->env;
    yajl_handle yh = yajl_alloc(&callbacks, &alloc_funcs, st);
    if (!yh) {
        err =  "Failed to allocate YAJL handle";
    }
    
    yajl_config(yh, yajl_dont_validate_strings, 1);
    ys = yajl_parse(yh, buf, len);

    if (ys == yajl_status_ok) {
        ys = yajl_complete_parse(yh);
    }

    if (ys != yajl_status_ok) {
        unsigned char *msg = yajl_get_error(yh, 0, NULL, 0);
        if (msg) {
            strncpy(st->errmsg, (char *)msg, ERR_BUF_SIZE-1);
            st->errmsg[ERR_BUF_SIZE-1] = '\0';
            yajl_free_error(yh, msg);
            err = st->errmsg;
        } else {
            err = "Unknown YAJL error";
        }
        cleanup_containers(st->c);  
    }
    
    yajl_free(yh);
    return err;
}

static ERL_NIF_TERM decode_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  /* initialize the state, with a dummy top level container */
  state_t st;
  st.env = env; /* keep NIF environment in state */
  ERL_NIF_TERM term;
  container_t c = { 0, 1, &term, 0, NULL };
  st.c = &c;
  
  ErlNifBinary bin;
  if (argc != 1 || !enif_is_binary(env, argv[0]))
    return enif_make_badarg(env);
  if (!enif_inspect_binary(env, argv[0], &bin))
    return enif_make_badarg(env);
  
  const char *err;
  if ((err = parse_json(&st, bin.data, bin.size)) != NULL) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, err, ERL_NIF_LATIN1));
  }
  return enif_make_tuple2(env, enif_make_atom(env, "ok"), term);
}


static ErlNifFunc json_parser_NIFs[] = {
  {"decode", 1, &decode_1}
};

ERL_NIF_INIT(yajler, json_parser_NIFs, NULL, NULL, NULL, NULL);


/*
 * JSON Erlang json() representation
 * ---- ----------------------------
 * true 'true'
 * false 'false'
 * null 'undefined'
 * number (integers and floats) number()
 * string: "..." binary()
 * array/list: [ value, ... ] {json()}
 * map: { label: value, ... } [{binary(), json()}]
 */

static void add_element(state_t *st, ERL_NIF_TERM t)
{
  container_t *c = st->c;
  if (c != NULL) {
    if (c->count >= c->arraysz) {
      c->arraysz *= 2;
      c->array = enif_realloc(c->array, c->arraysz*sizeof(ERL_NIF_TERM));
    }
    if (c->key) {
      /* the previous stored element was a key, so replace the entry
	 with a key/value pair, and don't count it twice */
      c->array[c->count-1] = enif_make_tuple2(st->env, c->array[c->count-1], t);
      c->key = 0;
    } else {
      c->array[c->count] = t;
      ++(c->count);
    }
  }
}

static int handle_null(void *ctx)
{
  state_t *st = (state_t *)ctx;
  add_element(st, enif_make_atom(st->env, "undefined"));
  return 1;
}

static int handle_boolean(void *ctx, int boolVal)
{
  state_t *st = (state_t *)ctx;
  if (boolVal) {
    add_element(st, enif_make_atom(st->env, "true"));
  } else {
    add_element(st, enif_make_atom(st->env, "false"));
  }
  return 1;
}

static int handle_integer(void *ctx, long long integerVal)
{
  state_t *st = (state_t *)ctx;
  add_element(st, enif_make_long(st->env, (long)integerVal));
  return 1;
}

static int handle_double(void *ctx, double doubleVal)
{
  state_t *st = (state_t *)ctx;
  add_element(st, enif_make_double(st->env, doubleVal));
  return 1;
}

static int handle_string(void *ctx, const unsigned char *stringVal, size_t stringLen)
{
    state_t *st = (state_t *)ctx;
    ErlNifBinary bin;
    if (!enif_alloc_binary(stringLen, &bin)) {
        return 0;  // Allocation failed
    }
    strncpy((char *)bin.data, (char *)stringVal, stringLen);
    add_element(st, enif_make_binary(st->env, &bin));
    enif_release_binary(&bin);  // Safe to release after enif_make_binary
    return 1;
}

static int handle_map_key(void *ctx, const unsigned char *stringVal, size_t stringLen)
{
  state_t *st = (state_t *)ctx;
  ErlNifBinary bin;
  enif_alloc_binary(stringLen, &bin);
  strncpy((char *)bin.data, (char *)stringVal, stringLen);
  add_element(st, enif_make_binary(st->env, &bin));
  st->c->key = 1; /* note that the next term will be the value */
  return 1;
}

static int handle_start(void *ctx)
{
  state_t *st = (state_t *)ctx;
  container_t *c = enif_alloc(sizeof(container_t));
  /* link and initialize container struct */
  c->next = st->c;
  st->c = c;
  c->key = 0;
  c->count = 0;
  c->arraysz = 16; /* initial term buffer size */
  c->array = enif_alloc(c->arraysz*sizeof(ERL_NIF_TERM));
  return 1;
}

static int handle_start_map(void *ctx)
{
  return handle_start(ctx);
}

static int handle_start_array(void *ctx)
{
  return handle_start(ctx);
}

static int handle_end(void *ctx)
{
  state_t *st = (state_t *)ctx;
  container_t *c = st->c;
  /* unlink container struct from state */
  st->c = c->next;
  /* create and add container term */

  add_element(st, enif_make_list_from_array(st->env, c->array, c->count));
  /* decallocate used container struct */
  if (c->array) {
        enif_free(c->array);
    }
  enif_free(c);

  return 1;
}

static int handle_end_map(void *ctx)
{
  return handle_end(ctx);
}

static int handle_end_array(void *ctx)
{
  return handle_end(ctx);
}
