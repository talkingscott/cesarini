% Chapter 3
-module(recursion).
-export([reverse/1, member/2, sum/1, bump/1, even/1, average/1, len/1,
        sum_to/1, sum_to/2, create/1, reverse_create/1, print_numbers/1,
        filter/2, concatenate/1, flatten/1, quicksort/1, mergesort/1]).

reverse(L) -> reverse_acc(L, []).

reverse_acc([], Acc) -> Acc;
reverse_acc([H|T], Acc) -> reverse_acc(T, [H | Acc]).

member(_, []) -> false;
member(H, [H|_T]) -> true;
member(V, [_H|T]) -> member(V, T).

% direct recursion
% sum([]) -> 0;
% sum([H|T]) -> H + sum(T).

sum(L) -> sum_acc(L, 0).

sum_acc([], Sum) -> Sum;
sum_acc([H|T], Sum) -> sum_acc(T, H+Sum).

% direct recursion
%bump([]) -> [];
%bump([H|T]) -> [H+1 | bump(T)].

bump(L) -> reverse(bump_acc(L, [])).

bump_acc([], Acc) -> Acc;
bump_acc([H|T], Acc) -> bump_acc(T, [H+1 | Acc]).

% direct recursion
%even([]) -> [];
%even([H|T]) when H rem 2 == 0 -> [H | even(T)];
%even([_|T]) -> even(T).

even(L) -> reverse(even_acc(L, [])).

even_acc([], Acc) -> Acc;
even_acc([H|T], Acc) when H rem 2 == 0 -> even_acc(T, [H | Acc]);
even_acc([_|T], Acc) -> even_acc(T, Acc).

% function composition
%average(L) -> sum(L) / len(L).

average(L) -> average_acc(L, 0, 0).

average_acc([], _, 0) -> 0;
average_acc([], Sum, Len) -> Sum / Len;
average_acc([H|T], Sum, Len) -> average_acc(T, Sum+H, Len+1).

% direct recursion
%len([]) -> 0;
%len([_|T]) -> 1 + len(T).

len(L) -> len_acc(L, 0).

len_acc([], Acc) -> Acc;
len_acc([_H|T], Acc) -> len_acc(T, Acc+1).

sum_to(Boundary) -> sum_to_acc(1, Boundary, 0).

sum_to_acc(Index, Boundary, Sum) when Index =< Boundary ->
  sum_to_acc(Index + 1, Boundary, Sum + Index);
sum_to_acc(_, _, Sum) -> Sum.

% Exercise 3-1
sum_to(From, To) when From =< To -> sum_to_acc(From, To, 0).

% Exercise 3-2
create(Boundary) -> reverse(create_acc(1, Boundary, [])).
reverse_create(Boundary) -> create_acc(1, Boundary, []).

create_acc(Index, Boundary, Acc) when Index =< Boundary ->
  create_acc(Index+1, Boundary, [Index | Acc]);
create_acc(_, _, Acc) -> Acc.

% Exercise 3-3
print_numbers(N) -> print_numbers(1, N).

print_numbers(Index, Boundary) when Index < Boundary ->
  io:format("Number: ~p~n", [Index]),
  print_numbers(Index+1, Boundary);
print_numbers(Index, Boundary) when Index =:= Boundary ->
  io:format("Number: ~p~n", [Index]).

% Exercise 3-5
filter(L, N) -> reverse(filter_acc(L, N, [])).

filter_acc([], _N, Acc) -> Acc;
filter_acc([H|T], N, Acc) when H =< N ->
  filter_acc(T, N, [H|Acc]);
filter_acc([_H|T], N, Acc) -> filter_acc(T, N, Acc).

concatenate(LL) -> reverse(concatenate_acc(LL, [])).

concatenate_acc([], Acc) -> Acc;
concatenate_acc([H|T], Acc) -> concatenate_acc(T, element_acc(H, Acc)).

element_acc([], Acc) -> Acc;
element_acc([H|T], Acc) -> element_acc(T, [H|Acc]).

flatten(LL) -> reverse(flatten_acc(LL, [])).

flatten_acc([], Acc) -> Acc;
flatten_acc([H|T], Acc) when is_list(H) ->
  flatten_acc(T, flatten_acc(H, Acc));
flatten_acc([H|T], Acc) ->
  flatten_acc(T, [H|Acc]).

% Exercise 3-7
quicksort([]) -> [];
quicksort([H|T]) ->
  Less = filter(T, H),
  quicksort(Less) ++ [H] ++ quicksort(T -- Less).

mergesort([]) -> [];
mergesort([A]) -> [A];
mergesort([A,B]) when A =< B -> [A,B];
mergesort([A,B]) -> [B,A];
mergesort(L) ->
  {L1,L2} = split(L),
  merge(mergesort(L1), mergesort(L2)).

split(L) -> split_acc(L, 1, len(L), [], []).

split_acc([], _, _, L1, L2) -> {L1,L2};
split_acc([H|T], Index, N, L1, L2) when Index =< N div 2 ->
  split_acc(T, Index + 1, N, [H|L1], L2);
split_acc([H|T], Index, N, L1, L2) ->
  split_acc(T, Index + 1, N, L1, [H|L2]).

merge(L1, L2) -> reverse(merge_acc(L1, L2, [])).

merge_acc([], [], Acc) -> Acc;
merge_acc([H|T], [], Acc) -> merge_acc(T, [], [H|Acc]);
merge_acc([], [H|T], Acc) -> merge_acc([], T, [H|Acc]);
merge_acc([H1|T1], [H2|T2], Acc) when H1 =< H2 ->
  merge_acc(T1, [H2|T2], [H1|Acc]);
merge_acc([H1|T1], [H2|T2], Acc) ->
  merge_acc([H1|T1], T2, [H2|Acc]).
