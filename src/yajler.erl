%% Copyright (c) 2015, Grzegorz Junka
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% Redistributions of source code must retain the above copyright notice,
%% this list of conditions and the following disclaimer.
%% Redistributions in binary form must reproduce the above copyright notice,
%% this list of conditions and the following disclaimer in the documentation
%% and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
%% EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(yajler).

-include_lib("eunit/include/eunit.hrl").

-export([decode/1, encode/1, escape/1]).
-export([to_proplist/2]).

-on_load(init/0).

init() ->
    erlang:load_nif(filename:join([code:priv_dir(yajler), "yajler_nif"]), 0).

decode(_Data) ->
    erlang:nif_error(nif_not_loaded).


%% encodes:
%% primitives: atom, integer, float, binary -> their binary representation enclosed with ""
%% in curlies: {atom}, {integer}, {float} -> native json types (NOT enclosed with "")
%% string: {"erlang string"} -> binary representation of the string without ""
%% key-value-pair: {Key, Value} -> "Key":Value where Value can be any primitive, object or array
%% object: [key-value-pair, key-value-pair, ...] -> {key-value-pair, key-value-pair, ...}
%% array: [V1, V2, V3, ...] -> [V1, V2, V3, ...] where VX can be any primitive, object or array

%% Possible error explanations:
%% not_an_obj - Can't add a key-value pair to an array. This may happen if the first element in the
%%              Erlang list was not a kay-value pair and the parser treats the list as an array.

-spec encode(list()) -> list().
encode(X) ->
    lists:reverse(encode([], X)).

encode(Acc, {String}) when is_list(String) ->
    [<<"\"">>, list_to_binary(String), <<"\"">> | Acc];
encode(Acc, {X}) ->
    encode_basic(Acc, X);
encode(Acc, [{_K, _V}|_T] = L) ->
    encode_obj(Acc, L, <<"{">>);
encode(Acc, [{}]) ->
    [<<"{}">>|Acc];
encode(Acc, [_X|_T] = L) ->
    encode_arr(Acc, L, <<"[">>);
encode(Acc, []) ->
    [<<"[]">>|Acc];
encode(Acc, X) ->
    [<<"\"">>|encode_basic([<<"\"">>|Acc], X)].

encode_basic(Acc, X) when is_atom(X) ->
    [atom_to_binary(X, utf8)|Acc];
encode_basic(Acc, X) when is_integer(X) ->
    [list_to_binary(integer_to_list(X))|Acc];
encode_basic(Acc, X) when is_float(X) ->
    [list_to_binary(io_lib:format("~w", [X]))|Acc];
encode_basic(Acc, X) when is_binary(X) ->
    [X|Acc];
encode_basic(_, {_Key, _Value} = Elem) ->
    throw({parse_error, {not_an_obj, Elem}});
encode_basic(_, Value) ->
    throw({parse_error, {encode_basic, Value}}).

encode_obj(Acc, [], _S) ->
    [<<"}">>|Acc];
encode_obj(_Acc, [{K, V}|_T], _S) when is_list(K) ->
    throw({parse_error, {encode_obj, {K, V}}});
encode_obj(Acc, [{K, V}|T], S) ->
    encode_obj( encode([<<":">>|encode([S|Acc], K)], V), T, <<",">>);
encode_obj(_Acc, L, _S) ->
    throw({parse_error, {encode_obj, L}}).

encode_arr(Acc, [], _S) ->
    [<<"]">>|Acc];
encode_arr(Acc, [X|T], S) ->
    encode_arr(encode([S|Acc], X), T, <<",">>);
encode_arr(_Acc, L, _S) ->
    throw({parse_error, {encode_arr, L}}).

-spec escape(list()) -> binary().
escape(List) ->
    list_to_binary(lists:reverse(escape(List, []))).

escape([List|T], Acc) when is_list(List) ->
    escape(T, escape(List, Acc));
escape([Char|T], Acc) when
      Char == 34;  % "
      Char == 38;  % &
      Char == 39;  % '
      Char == 60;  % <
      Char == 62;  % >
      Char == 91;  % [
      Char == 92;  % \
      Char == 93;  % ]
      Char == 123; % {
      Char == 125  % }
      ->
    BString = list_to_binary(integer_to_list(Char)),
    escape(T, [<< <<"&#">>/binary, BString/binary, <<";">>/binary >>|Acc]);
escape([7|T], Acc) ->
    escape(T, [<<"\\a">>|Acc]);
escape([8|T], Acc) ->
    escape(T, [<<"\\b">>|Acc]);
escape([9|T], Acc) ->
    escape(T, [<<"\\t">>|Acc]);
escape([10|T], Acc) ->
    escape(T, [<<"\\n">>|Acc]);
escape([11|T], Acc) ->
    escape(T, [<<"\\v">>|Acc]);
escape([12|T], Acc) ->
    escape(T, [<<"\\f">>|Acc]);
escape([13|T], Acc) ->
    escape(T, [<<"\\r">>|Acc]);
escape([Elem|T], Acc) ->
    escape(T, [Elem|Acc]);
escape([], Acc) ->
    Acc.

to_proplist(Fields, Record) ->
    zip(Fields, tl(tuple_to_list(Record))).

zip([_ | Xs], [undefined | Ys]) -> zip(Xs, Ys);
zip([X | Xs], [Y | Ys]) when
      is_boolean(Y); is_number(Y) -> [{X, {Y}} | zip(Xs, Ys)];
zip([X | Xs], [[] | Ys]) -> [{X, [{}]} | zip(Xs, Ys)];
zip([X | Xs], [Y | Ys]) -> [{X, Y} | zip(Xs, Ys)];
zip([], []) -> [].

%%% Some unit tests

execute(Term) -> list_to_binary(encode(Term)).

execute_throw(Term) ->
    try execute(Term) catch throw:{parse_error, _} -> caught end.

object_atom_test() ->
    <<"{\"key\":\"value\"}">> = execute( [{key,value}] ).

object_int_test() ->
    <<"{\"1\":\"2\"}">> = execute( [{1,2}] ).

object_float_binary_test() ->
    <<"{\"1.2\":\"binary\"}">> = execute( [{1.2,<<"binary">>}] ).

object_string_string_test() ->
    <<"{\"key\":\"value\"}">> = execute( [{{"key"},{"value"}}] ).

object_of_2_test() ->
    <<"{\"k1\":\"v1\",\"2\":\"v2\"}">> = execute( [{{"k1"},<<"v1">>},{2,{"v2"}}] ).

object_of_3_test() ->
    <<"{\"1\":\"v1\",\"2\":\"2.2\",\"3\":\"3\"}">> = execute( [{1,{"v1"}},{2,2.2},{3,3}] ).

object_raw1_test() ->
    <<"{1:{2:{atom:\"txt\"}}}">> = execute( [{{1},[{{2},[{{atom},txt}]}]}] ).

empty_array_test() ->
    <<"[]">> = execute( [] ).

emtpy_object_test() ->
    <<"{}">> = execute( [{}] ).

array_test() ->
    <<"[\"string\",\"atom\",\"999\",\"9.99\"]">> = execute( [{"string"}, atom, 999, 9.99] ).

array_raw_int_test() ->
    <<"[1,2,\"3\",\"4\",5,\"6\",7,8,9]">> = execute( [{1},{2},3,4,{5},6,{7},{8},{9}] ).

array_raw_atom_test() ->
    <<"[atom1,\"true\"]">> = execute( [{atom1}, true] ).

array_raw_binary_test() ->
    <<"[\"1\",binary]">> = execute( [1,{<<"binary">>}] ).

object_array_test() ->
    <<"{\"key\":[\"e1\",\"e2\",\"3\"]}">> = execute( [{<<"key">>, [<<"e1">>, {"e2"}, 3]}] ).

object_object_test() ->
    <<"{\"1\":{\"2\":{\"3\":\"4\"}}}">> = execute( [{1,[{2,[{3,4}]}]}] ).

array_of_arrays_test() ->
    <<"[[\"1\",\"2\"],[\"3\",\"4\"]]">> = execute( [[1,2],[3,4]] ).

array_of_objects_test() ->
    <<"[{\"1\":\"2\",\"3\":\"4\"},{\"k3\":\"v3\",\"k4\":\"v4\"}]">>
        = execute( [[{1,2},{3,4}],[{<<"k3">>,<<"v3">>},{{"k4"},{"v4"}}]] ).

array_with_empty_array_test() ->
    <<"[\"1\",\"2\",[]]">> = execute( [1,2,[]] ).

err_object1_test() ->
    caught = execute_throw( [{1,2},{1,2,3}] ).

err_object2_test() ->
    caught = execute_throw( [{1,2},[1,2,3]] ).

err_object3_test() ->
    caught = execute_throw( {1,2} ).

err_object4_test() ->
    caught = execute_throw( [{1,2},3] ).

err_object5_test() ->
    caught = execute_throw( [{1,2},{3}] ).

err_object6_test() ->
    caught = execute_throw( [1,{2,3}] ).

err_object7_test() ->
    caught = execute_throw( [{1,{2,3}}] ).

err_array_as_key1_test() ->
    caught = execute_throw( [{[1,2],3}] ).

err_object_as_key1_test() ->
    caught = execute_throw( [{[{1,2}],3}] ).

err_tuple_as_key1_test() ->
    caught = execute_throw( [{{1,2},3}] ).
