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

:- module(iconv,
          [ iconv_open/4                % +Input, +Mode, +Encoding, -Output
          ]).
:- use_module(library(ffi)).
:- use_module(library(cerror)).
:- use_module(library(error)).
:- use_module(library(prolog_stream)).

/** <module> Wrap GNU iconv to read/write aribrary encodings

This module illustrates how the GNU iconv   library can be wrapped using
the ffi interface to read and write streams using any encoding supported
by iconv. The library  defines  iconv_open/3   which  creates  a _filter
stream_ that makes the data  accessible   as  UTF-8  stream from Prolog,
regardless of the original encoding.

The example illustrates handling C memory using indirect pointers.
*/

		 /*******************************
		 *            C IMPORT		*
		 *******************************/

cpp_const('E2BIG').                     % output buffer is too small
cpp_const('EILSEQ').                    % illegal multibyte sequence
cpp_const('EINVAL').                    % unknown encoding

:- c_import("#include <iconv.h>
             #include <errno.h>",
            [ libc ],
            [ iconv_open(+string, +string, [*(void, iconv_close)]) as iconv_open_,
              iconv(*void, *(*char), *int, *(*char), *int, [-int])
            ]).

iconv_open(To, From, CD) :-
    iconv_open_(To, From, CD),
    (   c_address(CD, -1)               % returns (iconv_t)-1
    ->  posix_raise_error(open, iconv, To-From)
    ;   true
    ).

		 /*******************************
		 *      HIGH LEVEL WRAPPER	*
		 *******************************/

%!  iconv_strings(+CD, +InString, +InEnc, +OutEnc, -BytesLeft,
%!                -OutString)
%
%   Convert BytesString to OutString. BytesLeft is unified with a string
%   holding the bytes of the last incomplete multibyte sequence.

iconv_strings(CD, InString, InEnc, OutEnc, Left, OutString) :-
    c_alloc_string(In, InString, InEnc),
    c_dim(In, ILen1, _),
    ILen is ILen1-1,
    OBufSize is ILen*6,                 % UTF-8 is max 6 bytes long
    c_alloc(IPtr, (*char)[1]),
    c_store(IPtr[0], In),
    c_alloc(Out, char[OBufSize]),
    c_alloc(OPtr, (*char)[1]),
    c_store(OPtr[0], Out),
    c_alloc(ILeft, ulong[1]),
    c_store(ILeft[0], ILen),
    c_alloc(OLeft, ulong[1]),
    c_store(OLeft[0], OBufSize),
    iconv(CD, IPtr, ILeft, OPtr, OLeft, RC),
    (   size_t_1(RC),
        \+ c_errno('EINVAL')            % EINVAL: incomplete last sequence
    ->  posix_raise_error
    ;   true
    ),
    c_load(ILeft[0], NewILeft),
    ileft(NewILeft, ILen, In, Left),
    c_load(OLeft[0], NewLeft),
    OLen is OBufSize-NewLeft,
    c_load_string(Out, OLen, OutString, string, OutEnc).

ileft(0, _, _, "") :-
    !.
ileft(N, ILen, In, Left) :-
    Offset is ILen - N,
    ileft_codes(Offset, ILen, In, Bytes),
    string_codes(Left, Bytes).

ileft_codes(Offset, Len, In, [H|T]) :-
    Offset < Len,
    !,
    c_load(In, Offset, uchar, H),
    Offset1 is Offset+1,
    ileft_codes(Offset1, Len, In, T).
ileft_codes(_, _, _, []).

%!  size_t_1(+Int) is semidet.
%
%   Succeed if Int is (size_t)-1;

size_t_1(0xffffffffffffffff) :-
    c_sizeof(pointer, 8),
    !.
size_t_1(0xffffffff) :-
    c_sizeof(pointer, 4).


		 /*******************************
		 *            STREAM		*
		 *******************************/

%!  iconv_open(+RawData, ?Mode, +IConvEncoding, -UTF8Stream) is det.
%
%   Create an UTF-8 encoded stream to read  or write RawData.
%
%   @arg RawData is
%   either a stream, a _stream pair_ or the name of a file.
%   @arg Mode is one of `read` or `write`. If RawData is a plain stream,
%   Mode may be left unbound and will be bound depending on whether
%   RawData is an input or output stream.
%   @arg IConvEncoding is an encoding supported by the iconv() library.
%   Available encodings may be listed using `iconv -l`.
%   @arg UTF8Stream is an UTF-8 encoded string and may thus be used
%   to read or write arbitrary Unicode from Prolog.

:- dynamic
    iconv_stream/4,                     % UTF8Stream, RawStream, CD, Options
    iconv_status/3.                     % UTF8Stream, Status, Context

iconv_open(Stream, Mode, Encoding, UTF8Stream) :-
    is_stream(Stream),
    stream_pair(Stream, In, Out),
    select_stream(Mode, Stream, In, Out, Raw),
    !,
    iconv_open_(Raw, Mode, Encoding, UTF8Stream, false).
iconv_open(File, Mode, Encoding, UTF8Stream) :-
    open(File, Mode, Raw, [encoding(octet)]),
    iconv_open_(Raw, Mode, Encoding, UTF8Stream, true).

select_stream(read,  _, Raw, _, Raw) :- nonvar(Raw), !.
select_stream(write, _, _, Raw, Raw) :- nonvar(Raw), !.
select_stream(Mode,  S, _, _, _) :-
    domain_error(stream(Mode), S).

iconv_open_(Stream, Mode, Encoding, UTF8Stream, CloseParent) :-
    iconv_mode_open(Mode, Encoding, CD),
    asserta(iconv_stream(UTF8Stream, Stream, CD, CloseParent)),
    context_module(Me),
    open_prolog_stream(Me, Mode, UTF8Stream, []).

iconv_mode_open(read, Encoding, CD) :-
    iconv_open(utf8, Encoding, CD).
iconv_mode_open(write, Encoding, CD) :-
    iconv_open(Encoding, utf8, CD).

:- public
    stream_read/2,
    stream_write/2,
    stream_close/1.

%!  stream_read(+UTF8Stream, -Data)
%
%   Read a chunk from the input stream.   This first reads 4K bytes from
%   the raw stream, calls iconv() to translate   this to UTF-8 and loads
%   that. Note that an input chunk may end inside a multibyte sequence.

stream_read(UTF8Stream, Data) :-
    iconv_status(UTF8Stream, eof, _),
    !,
    Data = "".
stream_read(UTF8Stream, Data) :-
    iconv_stream(UTF8Stream, RawStream, CD, _),
    stream_property(RawStream, buffer_size(Size)),
    read_string(RawStream, Size, IChunk),
    string_length(IChunk, ILen),
    add_left(UTF8Stream, IChunk, IChunk2),
    iconv_strings(CD, IChunk2, octet, utf8, Left2, Data),
    store_status(UTF8Stream, Left2, ILen, Size).

add_left(UTF8Stream, Chunk0, Chunk) :-
    retract(iconv_status(UTF8Stream, left, Left)),
    !,
    atomics_to_string([Left,Chunk0], Chunk).
add_left(_, Chunk, Chunk).

store_status(_, "", ILen, ILen) :-
    !.
store_status(UTF8Stream, Left, ILen, ILen) :-
    !,
    asserta(iconv_status(UTF8Stream, left, Left)).
store_status(UTF8Stream, "", _, _) :-
    !,
    asserta(iconv_status(UTF8Stream, eof, true)).
store_status(_UTF8Stream, Left, _, _) :-
    syntax_error(incomplete_multibyte_sequence(Left)).

%!  stream_write(+UTF8String, +Data)
%
%   Handle a write from the UTF8String.  Data   is  a Unicode string. We
%   convert this to an UTF-8 string and   read  the converted data as an
%   octet string.

stream_write(UTF8Stream, Data) :-
    iconv_stream(UTF8Stream, RawStream, CD, _),
    iconv_strings(CD, Data, utf8, octet, Left, Wire),
    assertion(Left == ""),
    format(RawStream, '~s', [Wire]).

%!  stream_close(+UTF8Stream)
%
%   Discard the Prolog stream. Note that we close the parent stream, but
%   leave all FFI objects to the garbage collector.

stream_close(UTF8Stream) :-
    retractall(iconv_status(UTF8Stream, _, _)),
    retract(iconv_stream(UTF8Stream, Raw, _, CloseParent)),
    !,
    (   CloseParent == true
    ->  close(Raw)
    ;   true
    ).
