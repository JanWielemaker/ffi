:- use_module(library(plunit)).
:- use_module('../prolog/ffi').
:- use_module('../prolog/cerror').

test_libc :-
    run_tests([c_libc]).

cpp_const('_STAT_VER').

:- c_import("#include <sys/types.h>
             #include <sys/stat.h>
             #include <unistd.h>",
            [ libc ],
            [ ['__xstat'(+int,+string,-struct(stat),[-int])],
              [stat(+string,-struct(stat),[-int])]
            ]).

% __has_include(Header) is gcc 5
:- c_import("#if __has_include(<sys/vfs.h>)
             #include <sys/vfs.h>
             #endif",
            [ libc ],
            [ [statfs(+string, -struct(statfs), [-int])]
            ]).

:- c_import("#include <math.h>",
            [ libm ],
            [ sin(+double, [-double])
            ]).

:- c_import("#include <ctype.h>",
            [ libc ],
            [ toupper(+int, [-int])
            ]).

:- c_import("#include <wchar.h>",
            [ libc ],
            [ wcslen(+string(wchar_t), [-int])
            ]).

:- begin_tests(c_libc).

test(sin, Native == V) :-
    sin(4.5, V),
    Native is sin(4.5).
test(stat, Size == Native) :-
    once(source_file(_:stat(_,_), File)),
    stat(File, Stat),
    c_load(Stat[st_size], Size),
    size_file(File, Native).
test(upper, A == 0'A) :-
    toupper(0'a, A).
test(strupr, Upper == "HELLO") :-
    strupr("hello", Upper).
test(wcslen, Len == 5) :-
    wcslen("hello", Len).
test(wcslen, Len == 50) :-
    numlist(1001, 1050, L),
    wcslen(L, Len).

:- end_tests(c_libc).

:- if(current_predicate('__xstat'/4)).
stat(File, Stat) :-
    '__xstat'('_STAT_VER', File, Stat, Status),
    posix_status(Status, stat, file, File).
:- elif(current_predicate(stat/3)).
stat(File, Stat) :-
    stat(File, Stat, Status),
    posix_status(Status, stat, file, File).
:- endif.

statfs(File, FsStat) :-
    statfs(File, FsStat, Status),
    posix_status(Status, statfs, file, File).

strupr(In, Out) :-
    c_alloc_string(Ptr, In, text),
    between(0, infinite, I),
        c_load(Ptr[I], C),
        (   C == 0
        ->  !,
            c_load_string(Ptr, Out, string, text)
        ;   toupper(C, U),
            c_store(Ptr[I], U),
            fail
        ).
