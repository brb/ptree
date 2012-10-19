%% @doc Prunable prefix tree implementation in Erlang binary.
%%
%% @author Martynas Pumputis <martynas.pumputis@gmail.com>

-module(ptree).

-define(ROOT, "ptr33").

% All following field size values are in bits:
-define(NODE_S, 32).
-define(KEY_S, 8).
-define(TYPE_S, 2).
-define(VALUE_S, 8).
-define(CHILDREN_S, 32).

-define(MAX_NODE_S, 4294967295). % kind of optimization, 2^32-1

% Node types:
-define(REGULAR_T, 0).
-define(LEAF_T, 1).
-define(PRUNED_T, 2). % count should be stored within children field
-define(ROOT_T, 3).

-define(info(M), error_logger:info_report(M)).

-compile(export_all).

-type key() :: integer().
-type val() :: bitstring().
-type tree() :: bitstring().
-type node_t() :: ?REGULAR_T | ?LEAF_T | ?PRUNED_T | ?ROOT_T.
-type children() :: bitstring().
-type el() :: [key()].
-type pnode() :: bitstring().

-export_type([el/0, val/0]).

%% ============================================================================

%% @doc Return empty tree.
-spec empty() -> tree().
empty() -> <<?ROOT>>.

%% @doc Encode node.
%% Each node is repressented as (in bits):
%% | NODE_SIZE | KEY | TYPE | VALUE_SIZE | VALUE | CHILDREN_SIZE | CHILDREN |
%% @throws {max_node_size, integer()}.
-spec node(key(), node_t(), val(), binary()) -> binary().
node(Key, Type, Value, Children) when is_integer(Key), is_bitstring(Value),
        is_bitstring(Children) ->
    N = <<Key:?KEY_S/integer, Type:?TYPE_S/integer,
        (bit_size(Value)):?VALUE_S, Value/bits,
        (bit_size(Children)):?CHILDREN_S, Children/bits>>,
    NodeS = bit_size(N) + ?NODE_S,

    if
        NodeS =< ?MAX_NODE_S -> 
            <<NodeS:?NODE_S, N/bits>>;
        NodeS > ?MAX_NODE_S ->
            throw({max_node_size, NodeS})
    end.

%% @doc Insert Element with its Value into a tree.
%% @throws 'element_exist'.
%%
%% @todo throws {element_exist, el(), val()}.
%% @todo prune children.
%% @todo check value by properties.
-spec insert(el(), val(), tree()) -> tree(). 
insert(Element, Value, <<?ROOT, Nodes/bits>>) ->
    Children = get_children(
        insert_1(Element, Value, node(0, ?ROOT_T, <<>>, Nodes))),

    <<?ROOT, Children/bits>>.

insert_1([], _, <<_:?NODE_S, _:?KEY_S, ?LEAF_T:?TYPE_S, _/bits>>) ->
    throw(element_exist);

insert_1([E|Element], Value,
        <<_:?NODE_S, Key:?KEY_S, Type:?TYPE_S, ValS:?VALUE_S,
            Rest1/bits>>) ->
    if
        Type =:= ?LEAF_T -> throw(element_exist);
        Type =/= ?LEAF_T -> ok
    end,

    <<Val:ValS/bits, ChildrenS:?CHILDREN_S, Rest2/bits>> = Rest1,
    <<Children:ChildrenS/bits, _/bits>> = Rest2,

    {PreChildren, Node0, PostChildren} = split_children(E, Children),

    Node1 = case Node0 of
        undefined -> insert_2([E|Element], Value);
        _ -> insert_1(Element, Value, Node0)
    end,

    Children1 = <<PreChildren/bits, Node1/bits, PostChildren/bits>>,

    node(Key, Type, Val, Children1).

insert_2([E], Value) ->
    node(E, ?LEAF_T, Value, <<>>);

insert_2([E|Element], Value) ->
    node(E, ?REGULAR_T, <<>>, insert_2(Element, Value)).

%% @doc Retrieve the value stored with Element in tree. 
-spec get(el(), tree()) -> val() | 'undefined'.
get(Element, <<?ROOT, Children/bits>>) ->
    case get_1(Element, node(0, ?ROOT_T, <<>>, Children)) of
        undefined -> undefined;
        Node -> get_value(Node)
    end.

get_1([], Node) -> Node;

get_1([E|Element], Node) ->
    case split_children(E, get_children(Node)) of
        {_, undefined, _} -> undefined;
        {_, Node1, _} -> get_1(Element, Node1) 
    end.

%% ============================================================================
%% Helpers
%% ============================================================================

%% @doc Return tuple of three: children which key is less than K, child with K
%% (if such doesn't exist, then 'undefined'), children with key greater than K.
%% Assumes that children list is sorted in asc order.
-spec split_children(key(), children()) ->
        {children(), 'undefined' | pnode(), children()}.
split_children(K, Children) ->
    split_children(K, Children, <<>>).

split_children(K, <<NodeS:?NODE_S, Key:?KEY_S, _/bits>> =Children, Acc)
        when K == Key ->
    <<Node:NodeS/bits, Rest2/bits>> = Children,

    {Acc, Node, Rest2};

split_children(K, <<NodeS:?NODE_S, Key:?KEY_S, _/bits>> =Children, Acc)
        when K > Key ->
    <<Node:NodeS/bits, Rest2/bits>> = Children,
    split_children(K, Rest2, <<Acc/bits, Node/bits>>);

split_children(K, <<_:?NODE_S, Key:?KEY_S, _/bits>> =Children, Acc)
        when K < Key ->
    {Acc, undefined, Children};

split_children(_, <<>>, Acc) -> {Acc, undefined, <<>>}.

%% @doc Return node children.
-spec get_children(pnode()) -> children().
get_children(<<_:?NODE_S, _:?KEY_S, _:?TYPE_S, ValS:?VALUE_S, Rest1/bits>>) ->
    <<_:ValS, ChildrenS:?CHILDREN_S, Rest2/bits>> = Rest1,
    <<Children:ChildrenS/bits, _/bits>> = Rest2,

    Children.
    
%% @doc Return node value.
-spec get_value(pnode()) -> val().
get_value(<<_:?NODE_S, _:?KEY_S, _:?TYPE_S, ValS:?VALUE_S, Rest1/bits>>) ->
    <<Val:ValS/bits, _/bits>> = Rest1,

    Val.

%% ============================================================================
%% Tests
%% ============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

split_children_test() ->
    Node0 = node(1, 1, <<"a">>, <<>>),
    Node1 = node(3, 0, <<"b">>, node(1, 1, <<"d">>, <<>>)),
    Node2 = node(4, 1, <<"c">>, <<>>),
    Children = <<Node0/bits, Node1/bits, Node2/bits>>,

    ?assertEqual({Node0, Node1, Node2}, split_children(3, Children)),
    ?assertEqual({Children, undefined, <<>>}, split_children(10, Children)),
    ?assertEqual({<<>>, undefined, Children}, split_children(0, Children)).

insert_test() ->
    ElVals = [
        {[4, 2], <<"42">>},
        {[6, 6, 6], <<"666">>},
        {[6, 0, 9], <<"609">>}
    ],
    T = lists:foldl(fun ({El, Val}, Acc) -> insert(El, Val, Acc) end, empty(),
        ElVals),

    [?assertEqual(V, get(E, T)) || {E, V} <- ElVals],
    ?assertEqual(undefined, get([4, 3], T)),

    ?assertThrow(element_exist, insert([4, 2], <<"omg">>, T)),
    ?assertThrow(element_exist, insert([4, 2, 3], <<"omg">>, T)).
    
-endif.
