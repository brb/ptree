%% @doc Prunable prefix tree implementation in Erlang tuples for the purpose of
%% an efficiency comparison (mem usage and cpu utilization).

%% @author Martynas Pumputis <martynas.pumputis@gmail.com>

-module(ctree).

-compile(export_all).

-define(NIL, n).
-define(EXTERNAL, e).
-define(MAX_NODES, 10).
-define(FIRST, 0).
-define(LAST, 9).
-define(NO_CHILDREN, 
    {?NIL, ?NIL, ?NIL, ?NIL, ?NIL, ?NIL, ?NIL, ?NIL, ?NIL, ?NIL}).

%% ============================================================================

empty() -> ?NO_CHILDREN.

-spec get(ptree:el(), term()) -> 'undefined' | ptree:val().
get(Element, Children) ->
    get_1(Element, {<<>>, Children}).

get_1(_, ?NIL) -> undefined;

get_1([], Node) ->
    case Node of
        ?NIL -> undefined;
        {Val, _} -> Val
    end;

get_1([E|Element], {_, Children}) ->
    Children1 = case Children of
        ?EXTERNAL -> ?NO_CHILDREN;
        _ when is_tuple(Children) -> Children
    end,

    get_1(Element, element(E+1, Children1)).


-spec insert(ptree:el(), ptree:val(), term()) -> term().
insert(Element, Value, Children) ->
    element(2, insert_1(Element, Value, {<<>>, Children})).

insert_1(_, _, {_, Children}) when Children == ?EXTERNAL ->
    throw(element_exist);

insert_1([], Val, {<<>>, ?NO_CHILDREN}) ->
    {Val, ?EXTERNAL};

insert_1([], _, _) ->
    throw(element_exist);

insert_1([E|Element], Val, {Val0, Children0}) ->
    Node1 =
        case element(E+1, Children0) of
            ?NIL -> {<<>>, ?NO_CHILDREN};
            C when is_tuple(C) -> C;
            _ -> throw(element_exist)
        end,

    Node2 = insert_1(Element, Val, Node1),

    {Val0, setelement(E+1, Children0, Node2)}.

%% ============================================================================
%% Tests
%% ============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

insert_test() ->
    ElVals = [
        {[4, 2], <<"42">>},
        {[6, 6, 6], <<"666">>}
    ],
    T = lists:foldl(fun ({El, Val}, Acc) -> insert(El, Val, Acc) end, empty(),
        ElVals),

    [?assertEqual(V, get(E, T)) || {E, V} <- ElVals],
    ?assertEqual(undefined, get([4, 3], T)),

    ?assertThrow(element_exist, insert([4, 2], <<"omg">>, T)),
    ?assertThrow(element_exist, insert([4, 2, 3], <<"omg">>, T)).
    
-endif.
