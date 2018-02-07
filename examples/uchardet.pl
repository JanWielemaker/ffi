/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(uchardet,
          [ uchardet/2                  % +Data, -CharSet
          ]).
:- use_module(library(ffi)).

/** <module>

This example binds `libuchardet` that can be used to guess the character
encoding from a sequence of bytes.  On Debian based systems the required
library is installed using

    apt-get install libuchardet-dev
*/

:- c_import("#include <uchardet.h>",
            [ pkg_config(uchardet, '--cflags', '--libs') ],
            [ uchardet_new([*struct(uchardet)]),
              uchardet_delete(*struct(uchardet)),
              uchardet_handle_data(*struct(uchardet), string(octet), int, [int]),
              uchardet_data_end(*struct(uchardet)),
              uchardet_reset(*struct(uchardet)),
              uchardet_get_charset(*struct(uchardet), [string])
            ]).

%!  uchardet(+Data, -CharSet) is det.
%
%   Use the `uchardet` library to guess   the character encoding used by
%   the sequence of octets provided by  the string Data. Typically, Data
%   is provided by read_string/3 or peek_string/3 on a binary stream.

uchardet(Data, CharSet) :-
    string_length(Data, Len),
    setup_call_cleanup(
        uchardet_new(UD),
        (   uchardet_handle_data(UD, Data, Len, _RC),
            uchardet_data_end(UD),
            uchardet_get_charset(UD, CharSet)
        ),
        uchardet_delete(UD)).

