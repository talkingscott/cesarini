% Exercise 3-4
-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

reverse(L) -> reverse_acc(L, []).

reverse_acc([], Acc) -> Acc;
reverse_acc([H|T], Acc) -> reverse_acc(T, [H|Acc]).

new() -> [].

destroy(_Db) -> ok.

write(Key, Element, Db) -> [{Key, Element} | delete(Key, Db)].

delete(Key, Db) -> delete_acc(Key, Db, []).

delete_acc(_, [], Acc) -> reverse(Acc);
delete_acc(Key, [{Key, _}|T], Acc) -> reverse(Acc) ++ T;
delete_acc(Key, [H|T], Acc) -> delete_acc(Key, T, [H|Acc]).

read(Key, Db) -> find(Key, Db).

find(_Key, []) -> {error, instance};
find(Key, [{Key, Element}|_T]) -> {ok, Element};
find(Key, [_H|T]) -> find(Key, T).

match(Element, Db) -> reverse(search(Element, Db, [])).

search(_Element, [], Acc) -> Acc;
search(Element, [{Key, Element}|T], Acc) -> search(Element, T, [Key|Acc]);
search(Element, [_H|T], Acc) -> search(Element, T, Acc).
