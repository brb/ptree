%% @doc

%% @author Martynas Pumputis <martynas.pumputis@gmail.com>

-module(ptree_bench).

-compile(export_all).

%% ============================================================================
%% Benchmarks
%% ============================================================================

do(Mod, Len) ->
    spawn_link(
        fun () ->
            El = num(Len),

            erlang:statistics(wall_clock),

            T = create(Mod, El),
            Size = case Mod of
                ctree -> erts_debug:flat_size(T);
                ptree -> erlang:bit_size(T)
            end,

            {_, Ms} = erlang:statistics(wall_clock),
            io:format("Size: ~p, Time: ~p ms.~n", [Size, Ms]),

            loop(T)
        end).

loop(T) ->
    receive From -> From ! T end,
    loop(T).

%% ============================================================================
%% Helpers
%% ============================================================================

-spec create('ctree' | 'ptree', [ptree:el()]) -> term().
create(Mod, El) ->
    lists:foldl(
        fun (E, Acc) -> Mod:insert(E, list_to_binary(E), Acc) end,
        Mod:empty(),
        El
    ).

num(Len) ->
    El = lists:seq(0, 9),
    num(El, Len, []).

num(_, 0, Acc) -> lists:sort(Acc);

num(El, Len, []) -> 
    num(El, Len-1, [[E] || E <- El]);

num(El, Len, Acc) ->
    num(El, Len-1,
        lists:foldl(fun (A, L) -> [[E|A] || E <- El] ++ L end, [], Acc)).
