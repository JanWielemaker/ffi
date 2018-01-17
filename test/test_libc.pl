:- use_module(library(plunit)).
:- use_module('../prolog/cinvoke').
:- use_module('../prolog/cerror').

test_libc :-
    run_tests([c_libc]).

cpp_const('_STAT_VER').

:- c_import("#include <sys/types.h>
             #include <sys/stat.h>
             #include <unistd.h>",
            [ libc ],
            [ '__xstat'(+int,+string,-struct(stat),[-int])
            ]).

:- c_import("#include <sys/vfs.h>",
            [ libc ],
            [ statfs(+string, -struct(statfs), [-int])
            ]).

:- c_import("#include <math.h>",
            [ libm ],
            [ sin(+double, [-double])
            ]).

:- c_import("#include <ctype.h>",
            [ libc ],
            [ toupper(+int, [-int])
            ]).

:- begin_tests(c_libc).

test(sin, Native == V) :-
    sin(4.5, V),
    Native is sin(4.5).
test(stat, Native == Size) :-
    once(source_file(_:stat(_,_), File)),
    stat(File, Stat),
    c_load(Stat[st_size], Size),
    size_file(File, Native).
test(upper, A == 0'A) :-
    toupper(0'a, A).
test(strupr, Upper == "HELLO") :-
    strupr("hello", Upper).

:- end_tests(c_libc).

stat(File, Stat) :-
    '__xstat'('_STAT_VER', File, Stat, Status),
    posix_status(Status, stat, file, File).

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
