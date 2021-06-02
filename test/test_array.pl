:- module(test_array,
          [ test_array/0
          ]).
:- use_module('../prolog/ffi').
:- use_module(library(plunit)).

/** <module> Test C array conversion

This test demonstrates C array conversion to
a prolog list or a compound.
*/

:- c_import("#include \"test_array.c\"",
            [ test_array ],
            [ test_intarr([*int]),
              test_fltarr(*float,size_t,[void]),
              test_structarr([*struct(struct_s)]),
              test_enumarr([*enum(enum_e)]),
	      test_unionarr([*union(union_u)])
              % lists of lists
              % test_intarrarr([*(*int)]),
              % test_fltarrarr(*(*float),size_t,[void]),
              % test_structarrarr([*(*struct(struct_s))]),
              % test_enumarrarr([*(*enum(enum_e))]),
	      % test_unionarrarr([*(*union(union_u))])
            ]).

test_array :-
    run_tests([ array_to_list,
                list_to_array,
                array_to_compound,
                compound_to_array
              ]).


:- begin_tests(array_to_list).

% int, float
test(sized_cptr, R == [3,1,0,2]) :-
    c_alloc(CPtr, int[]=[3,1,0,2]),
    c_array_to_list(CPtr,R).

test(sized_cptr_count, R == [  [3,1,0,2],
                               [3,1]
			    ]) :-
    c_alloc(CPtr, int[]=[3,1,0,2]),
    c_array_to_list(CPtr,4,R1),
    c_array_to_list(CPtr,2,R2),
    R = [ R1, R2 ].

test(sized_cptr_overflow_count,
     error(domain_error(count_less_or_equal_to_size,5),_) ) :-
    c_alloc(CPtr, long[]=[3,1,0,2]),
    c_array_to_list(CPtr,5,_).

test(sized_cptr_underflow_count,
     error(domain_error(positive_count,0),_) ) :-
    c_alloc(CPtr, int[]=[3,1,0,2]),
    c_array_to_list(CPtr,0,_).

test(unsized_int_rawptr, R == [3,1,0,2,5]) :-
    test_intarr(Ptr),
    c_array_to_list(Ptr,5,R).

test(unsized_float_rawptr, R == [1.0,0.0,1.0]) :-
    c_alloc(Arr,float[3]),
    test_fltarr(Arr,3),
    c_array_to_list(Arr,3,R).

test(unsized_rawptr_no_count,
     error(type_error(fixed_size_array,_),_) ) :-
    test_intarr(Ptr),
    c_array_to_list(Ptr,_).




 % Structs, enums, unions
 test(struct_array,
      R = [[1,2.0],[3,5.0],[7,8.0]] ) :-
    test_structarr(Ptr),
    c_array_to_list(Ptr,3,[A,B,C]),
    c_load(A[fa], Aa), c_load(A[fb],Ab),
    c_load(B[fa], Ba), c_load(B[fb],Bb),
    c_load(C[fa], Ca), c_load(C[fb],Cb),
    R = [ [Aa,Ab], [Ba,Bb], [Ca,Cb] ].

 test(union_array,
      R = [[55,'7'],[33,'!'],[88,'X']] ) :-
    test_unionarr(Ptr),
    c_array_to_list(Ptr,3,[A,B,C]),
    c_load(A[ua], Aa), c_load(A[ub], Ab), c_load_string(Ab,Aba,atom,text),
    c_load(B[ua], Ba), c_load(B[ub], Bb), c_load_string(Bb,Bba,atom,text),
    c_load(C[ua], Ca), c_load(C[ub], Cb), c_load_string(Cb,Cba,atom,text),
    R = [ [Aa,Aba], [Ba,Bba], [Ca,Cba] ].

 test(enum_array,
      R == [ ec, eb, ea ] ) :-
    test_enumarr(Ptr),
    c_array_to_list(Ptr,3,[A,B,C]),
    R = [ A, B, C ].

:- end_tests(array_to_list).


:- begin_tests(array_to_compound).

% int, float
test(sized_cptr_compound, R == c(3,1,0,2)) :-
    c_alloc(CPtr, int[]=[3,1,0,2]),
    c_array_to_compound(CPtr,c,R).

test(sized_cptr_count_compound, R == [  c(3,1,0,2),
                                        c(3,1)
			             ]) :-
    c_alloc(CPtr, int[]=[3,1,0,2]),
    c_array_to_compound(CPtr,4,c,R1),
    c_array_to_compound(CPtr,2,c,R2),
    R = [ R1, R2 ].

test(unsized_int_rawptr_compound, R == c(3,1,0,2,5)) :-
    test_intarr(Ptr),
    c_array_to_compound(Ptr,5,c,R).

test(unsized_float_rawptr_compound, R == c(1.0,0.0,1.0)) :-
    c_alloc(Arr,float[3]),
    test_fltarr(Arr,3),
    c_array_to_compound(Arr,3,c,R).



 % Structs, enums, unions
test(struct_array_compound,
      R = [[1,2.0],[3,5.0],[7,8.0]] ) :-
    test_structarr(Ptr),
    c_array_to_compound(Ptr,3,c,c(A,B,C)),
    c_load(A[fa], Aa), c_load(A[fb],Ab),
    c_load(B[fa], Ba), c_load(B[fb],Bb),
    c_load(C[fa], Ca), c_load(C[fb],Cb),
    R = [ [Aa,Ab], [Ba,Bb], [Ca,Cb] ].

test(union_array_compound,
      R = [[55,'7'],[33,'!'],[88,'X']] ) :-
    test_unionarr(Ptr),
    c_array_to_compound(Ptr,3,c,c(A,B,C)),
    c_load(A[ua], Aa), c_load(A[ub], Ab), c_load_string(Ab,Aba,atom,text),
    c_load(B[ua], Ba), c_load(B[ub], Bb), c_load_string(Bb,Bba,atom,text),
    c_load(C[ua], Ca), c_load(C[ub], Cb), c_load_string(Cb,Cba,atom,text),
    R = [ [Aa,Aba], [Ba,Bba], [Ca,Cba] ].

 test(enum_array_compound,
      R == [ ec, eb, ea ] ) :-
    test_enumarr(Ptr),
    c_array_to_compound(Ptr,3,c,c(A,B,C)),
    R = [ A, B, C ].

:- end_tests(array_to_compound).


:- begin_tests(list_to_array).

% int, float
test(int_list, R == [3,1,0,2]) :-
    c_array_from_list(CPtr,[3,1,0,2]),
    c_array_to_list(CPtr,R).

test(float_list, R == [3.0,1.0,0.0,2.0]) :-
   c_array_from_list(CPtr,[3.0,1.0,0.0,2.0]),
   c_array_to_list(CPtr,R).

test(float_list_count, R == [3.0,1.0,0.0]) :-
   c_array_from_list(CPtr,3,[3.0,1.0,0.0,2.0]),
   c_array_to_list(CPtr,R).


:- end_tests(list_to_array).


:- begin_tests(compound_to_array).

% int, float
test(int_compound, R == c(3,1,0,2)) :-
    c_array_from_compound(CPtr,c(3,1,0,2)),
    c_array_to_compound(CPtr,c,R).

test(float_compound, R == c(3.0,1.0,0.0,2.0)) :-
   c_array_from_compound(CPtr,c(3.0,1.0,0.0,2.0)),
   c_array_to_compound(CPtr,c,R).

:- end_tests(compound_to_array).
