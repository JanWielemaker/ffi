:- module(test_qsort,
          [ test_qsort/0
          ]).
:- use_module(library(plunit)).
:- use_module('../prolog/ffi').

test_qsort :-
    run_tests([ qsort ]).

:- c_import("#include <stdlib.h>",
            [ libc ],
            [ qsort(*void, int, int, qcompare(*void, *void, [int]))
            ]).

:- begin_tests(qsort).

test(qsort, Sorted == List) :-
    numlist(1, 10, List),
    random_permutation(List, Unsorted),
    sort_int_list(Unsorted, Sorted).

:- end_tests(qsort).

sort_int_list(List, Sorted) :-
    length(List, Count),
    c_alloc(Data, int[Count]),
    fill_array(List, 0, Data),
    c_sizeof(int, ESize),
    qsort(Data, Count, ESize),
    read_array(0, Count, Data, Sorted).

qcompare(Ptr1, Ptr2, Diff) :-
    c_load(Ptr1, 0, int, I1),
    c_load(Ptr2, 0, int, I2),
    compare(DiffA, I1, I2),
    diff_int(DiffA, Diff).

diff_int(<,-1).
diff_int(=, 0).
diff_int(>, 1).

fill_array([], _, _).
fill_array([H|T], I, Data) :-
    c_store(Data[I], H),
    I2 is I + 1,
    fill_array(T, I2, Data).

read_array(I, Count, Data, [H|T]) :-
    I < Count,
    !,
    c_load(Data[I], H),
    I2 is I + 1,
    read_array(I2, Count, Data, T).
read_array(_, _, _, []).

