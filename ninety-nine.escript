#!/usr/bin/env escript

% S-99: Ninety-Nine Scala Problems
% http://aperiodic.net/phil/scala/s-99/

main(_) ->
% ;P01 (*) Find the last element of a list.
% Example:
% scala> last(List(1, 1, 2, 3, 5, 8))
% res0: Int = 8
	io:format("01-1 ~w~n", [last([1, 1, 2, 3, 5, 8])]),
	io:format("01-2 ~w~n", [last2([1, 1, 2, 3, 5, 8])]),

% P02 (*) Find the last but one element of a list.
% Example:
% scala> penultimate(List(1, 1, 2, 3, 5, 8))
% res0: Int = 5
	io:format("02-1 ~w~n", [penultimate([1, 1, 2, 3, 5, 8])]),

% P03 (*) Find the Kth element of a list.
% By convention, the first element in the list is element 0.
% Example:
%
% scala> nth(2, List(1, 1, 2, 3, 5, 8))
% res0: Int = 2

	io:format("03-1 ~w~n", [nth(2, [1, 1, 2, 3, 5, 8])]),

% P04 (*) Find the number of elements of a list.
% Example:
% scala> length(List(1, 1, 2, 3, 5, 8))
% res0: Int = 6

	io:format("04-1 ~w~n", [my_len([1, 1, 2, 3, 5, 8])]),
	io:format("04-2 ~w~n", [my_len2([1, 1, 2, 3, 5, 8])]),

% P05 (*) Reverse a list.
% Example:
% scala> reverse(List(1, 1, 2, 3, 5, 8))
% res0: List[Int] = List(8, 5, 3, 2, 1, 1)

	io:format("05-1 ~w~n", [my_reverse([1, 1, 2, 3, 5, 8])]),
	io:format("05-2 ~w~n", [my_reverse2([1, 1, 2, 3, 5, 8])]),

% P06 (*) Find out whether a list is a palindrome.
% Example:
% scala> isPalindrome(List(1, 2, 3, 2, 1))
% res0: Boolean = true

	io:format("06-1 ~w~n", [is_palindrome([1, 2, 3, 2, 1])]),

% P07 (**) Flatten a nested list structure.
% Example:
% scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
% res0: List[Any] = List(1, 1, 2, 3, 5, 8)

	io:format("07-1 ~w~n", [my_flatten([[1,1], 2, [3, [5, 8]]])]),

% P08 (**) Eliminate consecutive duplicates of list elements.
% If a list contains repeated elements they should be replaced with a single copy of the element.
% The order of the elements should not be changed.
% Example:
% scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
% res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

	io:format("08-1 ~w~n", [my_compress([a, a, a, a, b, c, c, a, a, d, e, e, e, e])]),
	io:format("08-2 ~w~n", [my_compress2([a, a, a, a, b, c, c, a, a, d, e, e, e, e])]),

% P09 (**) Pack consecutive duplicates of list elements into sublists.
% If a list contains repeated elements they should be placed in separate sublists.
% Example:
% scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
% res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

	io:format("09-1 ~w~n", [my_pack([a, a, a, a, b, c, c, a, a, d, e, e, e, e])]),
	io:format("09-2 ~w~n", [my_pack2([a, a, a, a, b, c, c, a, a, d, e, e, e, e])]),
	io:format("09-3 ~w~n", [my_pack3([a, a, a, a, b, c, c, a, a, d, e, e, e, e])]),

% P10 (*) Run-length encoding of a list.
% Use the result of problem P09 to implement the so-called run-length encoding data compression method.
% Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
% Example:
% scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
% res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
	io:format("10-1 ~w~n", [my_encode([a, a, a, a, b, c, c, a, a, d, e, e, e, e])]),
	io:format("10-2 ~w~n", [my_encode2([a, a, a, a, b, c, c, a, a, d, e, e, e, e])]),

% P11 (*) Modified run-length encoding.
% Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list.
% Only elements with duplicates are transferred as (N, E) terms.
% Example:
% scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
% res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
	io:format("11-1 ~w~n", [my_encode_modified([a, a, a, a, b, c, c, a, a, d, e, e, e, e])]),

% P12 (**) Decode a run-length encoded list.
% Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
% Example:
% scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
% res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
	io:format("12-1 ~w~n", [my_decode([{4,a},{1,b},{2,c},{2,a},{1,d},{4,e}])]),

% P13 (**) Run-length encoding of a list (direct solution).
% Implement the so-called run-length encoding data compression method directly.
% I.e. don't use other methods you've written (like P09's pack); do all the work directly.
% Example:
% scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
% res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
	io:format("13-1 ~w~n", [my_encode_direct([a, a, a, a, b, c, c, a, a, d, e, e, e, e])]),

% P14 (*) Duplicate the elements of a list.
% Example:
% scala> duplicate(List('a, 'b, 'c, 'c, 'd))
% res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
	io:format("14-1 ~w~n", [my_dup([a, b, c, c, d])]),

% P15 (**) Duplicate the elements of a list a given number of times.
% Example:
% scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
% res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
	io:format("15-1 ~w~n", [my_dupn(3, [a, b, c, c, d])]),

% P16 (**) Drop every Nth element from a list.
% Example:
% scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
% res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
	io:format("16-1 ~w~n", [my_drop(3, [a, b, c, d, e, f, g, h, i, j, k])]),

% P17 (*) Split a list into two parts.
% The length of the first part is given. Use a Tuple for your result.
% Example:
% scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
% res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	io:format("17-1 ~w~n", [my_split(3, [a, b, c, d, e, f, g, h, i, j, k])]),
	io:format("17-2 ~w~n", [my_split2(3, [a, b, c, d, e, f, g, h, i, j, k])]),
	io:format("17-3 ~w~n", [my_split3(3, [a, b, c, d, e, f, g, h, i, j, k])]),
	io:format("17-4 ~w~n", [my_split4(3, [a, b, c, d, e, f, g, h, i, j, k])]),

% P18 (**) Extract a slice from a list.
% Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
% Example:
% scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
% res0: List[Symbol] = List('d, 'e, 'f, 'g)
	io:format("18-1 ~w~n", [my_slice(3, 7, [a, b, c, d, e, f, g, h, i, j, k])]),
	io:format("18-2 ~w~n", [my_slice2(3, 7, [a, b, c, d, e, f, g, h, i, j, k])]),

% P19 (**) Rotate a list N places to the left.
% Examples:
% scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
% res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
% scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
% res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
	io:format("19-1 ~w~n", [my_rotate(3, [a, b, c, d, e, f, g, h, i, j, k])]),
	io:format("19-1 ~w~n", [my_rotate(-2, [a, b, c, d, e, f, g, h, i, j, k])]),
	io:format("19-2 ~w~n", [my_rotate2(3, [a, b, c, d, e, f, g, h, i, j, k])]),
	io:format("19-2 ~w~n", [my_rotate2(-2, [a, b, c, d, e, f, g, h, i, j, k])]),

% last-empty-op
	io:format("").

% procs-here::
last([]) -> throw(out_of_bounds1);
last([H|[]]) -> H;
last([_|T]) -> last(T).

last2(A) -> lists:nth(length(A), A).

penultimate([]) -> throw(out_of_bounds1);
penultimate([_ | []]) -> throw(out_of_bounds2);
penultimate([H | [_ | []]]) -> H;
penultimate([_|T]) -> penultimate(T).

nth(_, []) -> throw(out_of_bounds1);
nth(N, A) when length(A)<N+1 -> throw(out_of_bounds2);
nth(N, A) -> nth(ok, N+1, lists:reverse(A)).

nth(ok, 0, [H|_]) -> H;
nth(ok, N, [_|T]) -> nth(ok, N-1, T).

my_len(A) -> my_len(0, A).
my_len(N, []) -> N;
my_len(N, [_|T]) -> my_len(N+1, T).

my_len2(A) -> lists:foldl(fun(_, Sum) -> Sum+1 end, 0, A).

my_reverse(A) -> my_reverse([], A).
my_reverse(A, []) -> A;
my_reverse(A, [H|T]) -> my_reverse([H|A], T).

my_reverse2(A) -> lists:foldl(fun(C, D) -> [C|D] end, [], A).

is_palindrome(A) -> my_reverse(A) == A.

my_flatten(A) -> lists:flatmap(
	fun(X) ->
		if
			is_list(X) -> my_flatten(X);
			true -> [X]
		end
	end, A
).

my_compress(A) -> lists:reverse(lists:foldl(
	fun
		(X, [Ah|At]) when Ah == X -> [Ah|At];
		(X, Acc) -> [X|Acc]
	end, [], A
)).

my_compress2([H|T]) -> my_compress2([H],H,T).
my_compress2(Acc, _, []) -> lists:reverse(Acc);
my_compress2(Acc, Last, [H|T]) when Last==H -> my_compress2(Acc, Last, T);
my_compress2(Acc, _Last, [H|T]) -> my_compress2([H|Acc], H, T).

my_pack([Hb|Tb]) -> lists:reverse(lists:foldl(
	fun
		(X, [H|T]) ->
			[H2|_T2] = H,
			if
				H2==X -> [[X|H]|T];
				true -> [[X]|[H|T]]
			end
	end, [[Hb]], Tb
)).

my_pack2(A) -> my_pack2([], nil, A).
my_pack2(Acc, _Last, []) -> lists:reverse(Acc);
my_pack2([Ah|At], Last, [H|T]) when Last == H -> my_pack2([[Last|Ah]|At], Last, T);
my_pack2(A, _Last, [H|T]) -> my_pack2([[H]|A], H, T).

my_pack3([]) -> [];
my_pack3([H|T]) ->
	{X,Y} = lists:splitwith(fun(X) -> X == H end, [H|T]),
	[X|my_pack3(Y)].

my_encode([]) -> [];
my_encode([H|T]) ->
	{X,Y} = lists:splitwith(fun(X) -> X == H end, [H|T]),
	[{length(X),H}|my_encode(Y)].

% 10-2
my_encode2(A) -> [ {length(T)+1, H} || [H|T] <- my_pack(A)].

% 11-1
my_encode_modified(A) ->
	lists:map(fun
		([H|T]) when T == [] -> H;
		([H|T]) -> {length(T)+1, H}
	end, my_pack(A)
).

my_decode(A) ->	[  [ Y || _ <- lists:seq(1,X)] || {X, Y}  <- A ].

% 13-1
my_encode_direct([H|T]) -> my_encode_direct([{1,H}], T).
my_encode_direct(Acc, []) -> lists:reverse(Acc);
my_encode_direct([{Sum, Sign}|At], [H|T]) when Sign == H -> my_encode_direct([{Sum+1, Sign}|At], T);
my_encode_direct([{Sum, Sign}|At], [H|T]) -> my_encode_direct([{1,H}|[{Sum, Sign}|At]], T).

% 14-1
my_dup(A) -> lists:flatmap(fun(X) -> [X,X] end, A).

% 15-1
my_dupn(N, A) -> lists:flatmap(fun(X) -> [ X || _ <- lists:seq(1,N) ] end, A).

% 16-1
my_drop(N, A) -> my_drop([], 1, N, A).
my_drop(Acc, _, _, []) -> lists:reverse(Acc);
my_drop(Acc, Num, N, [_H|T]) when Num == N -> my_drop(Acc, 1, N, T);
my_drop(Acc, Num, N, [H|T]) -> my_drop([H|Acc], Num+1, N, T).

% 17-1
my_split(N, A) -> my_split([], N, A).
my_split(A, 0, B) -> [lists:reverse(A), B];
my_split(A, N, [H|T]) -> my_split([H|A], N-1, T).

% 17-2
my_split2(N, A) -> [ lists:sublist(A, N), lists:nthtail(N, A)].

% 17-3 standart
my_split3(N, A) ->
    {F, S} = lists:split(N, A),
    [F, S].

% 17-4 non-recursive
my_split4(0, A) -> [[], A];
my_split4(N, [H|T]) ->
    [H1, P1] = my_split4(N-1, T),
    [[H|H1], P1].

% 18-1 recursive
my_slice(Start, End, List) -> my_slice(Start, End, List, []).
my_slice(0, 0, _, Res) -> lists:reverse(Res);
my_slice(0, End, [H|T], Res) -> my_slice(0, End-1, T, [H|Res]);
my_slice(Start, End, [_H|T], Res) -> my_slice(Start-1, End-1, T, Res).

% 18-2 stupid
my_slice2(Start, End, List) ->
    {_, B} = lists:split(Start, List),
    {C, _} = lists:split(End - Start, B),
    C.

% 19-1 recursive
my_rotate(N, L) when N>0 -> my_rotate(left, N rem length(L), L);
my_rotate(N, L) -> my_rotate(right, -N rem length(L), lists:reverse(L)).

my_rotate(left, 0, L) -> L;
my_rotate(right,0, L) -> lists:reverse(L);
my_rotate(Dir, N, [H|T]) -> my_rotate(Dir, N-1, lists:append(T, [H])).

% 19-2 no-cycle, from sample
my_rotate2(N, L) ->
    N2 = N rem length(L),
    if
        N2<0 -> my_rotate2(N2+length(L), L);
        true ->
            {A, B} = lists:split(N2, L),
            lists:append(B,A)
    end.
