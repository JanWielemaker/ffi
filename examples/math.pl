:- module(ffi_math,
          [ sin/2,
            sincos/3
          ]).
:- use_module(library(ffi)).

/** <module> Math examples

Almost all foreign interfaces start with   math  examples. Not having to
deal with allocation greatly simplifies the code.
*/

:- c_import("#define _GNU_SOURCE
             #include <math.h>",
            [ '-lm' ],
            [ sin(float, [float]),
              [sincos(float, -float, -float)]
            ]).

%!  sin(+X, -SinX) is det.
%
%   True when SinX is sin(X). This is  a really simple example. Note the
%   argument is declared as `float`. That is  the _Prolog_ type. The ffi
%   library figures out that the actual   type  is `double` and installs
%   the necessary conversion.

%!  sincos(+X, -SinX, -CosX) is det.
%
%   True when SinX is sin(X) and CosX  is   cos(X).  This  is a bit more
%   chalenging as we now have output  arguments   and  sincos() is a GNU
%   extension and thus might not be in   your '-lm'. The output argument
%   is  denoted  as  `-float`.  The  declaration  is  in  a  list  ([]),
%   indicating the library that it  is  not   an  error  if the function
%   cannot be found. We can use that to define our replacement.

:- if(\+current_predicate(sincos/3)).
sincos(X, SinX, CosX) :-
    SinX is sin(X),
    CosX is cos(X).
:- endif.
