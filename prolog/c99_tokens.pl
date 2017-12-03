/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
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

:- module(c99,
          [ c99_tokens//1,              % -List
            c99_token//1                % -Token
          ]).
:- use_module(library(dcg/basics), [blanks//0, string//1, string_without//2]).

%!  c99_tokens(-Tokens)// is semidet.
%
%   Tokenize an input according to the C99 rules.  Tokens are:
%
%     - Keywords and punctuators are rokenized as their atom
%     - Identifiers are id(Atom)
%     - Constants are one of
%       - Integers:
%         - i(V)			<ddd>
%         - l(V)			<ddd>l
%         - ll(V)			<ddd>ll
%         - u(V)			<ddd>u
%         - ul(V)			<ddd>ul
%         - ull(V)			<ddd>ull
%       - Floats
%         - float(V)			<lexical float>f
%         - double(V)			<lexical float>[l]
%       - enum_value(Name)		Identifie
%       - char(Codes)			'...'
%       - wchar(Codes)			L'...'
%       - str(String)			"..."
%       - wstr(String)			L"..."
%     - Preprocessing stuff:
%       - header(ab, Atom)		<assert.h>
%       - header(dq, Atom)		"file.h"
%       - pp(String)			pp-number?


c99_tokens([H|T]) -->
    c99_token(H), !,
    c99_tokens(T).
c99_tokens([]) -->
    blanks.

c99_token(Token) -->
    blanks,
    token(Token0),
    (   {Token0 = pp(_)}
    ->  c99_token(Token)
    ;   {Token0 = Token}
    ).

%!  token(-Token)//
%
%   A1: recognise a C99 token.

token(T) --> keyword(T), \+ identifier_cont_char(_).
token(T) --> identifier(T).
token(T) --> constant(T).
token(T) --> string_literal(T).
token(T) --> pp_line(T).
token(T) --> punctuator(T).
token(T) --> header_name(T).
token(T) --> pp_number(T).

keyword(auto)         --> "auto".
keyword(break)        --> "break".
keyword(case)         --> "case".
keyword(char)         --> "char".
keyword(const)        --> "const".
keyword(continue)     --> "continue".
keyword(default)      --> "default".
keyword(do)           --> "do".
keyword(double)       --> "double".
keyword(else)         --> "else".
keyword(enum)         --> "enum".
keyword(extern)       --> "extern".
keyword(float)        --> "float".
keyword(for)          --> "for".
keyword(goto)         --> "goto".
keyword(if)           --> "if".
keyword(inline)       --> "inline".
keyword(int)          --> "int".
keyword(long)         --> "long".
keyword(register)     --> "register".
keyword(restrict)     --> "restrict".
keyword(return)       --> "return".
keyword(short)        --> "short".
keyword(signed)       --> "signed".
keyword(sizeof)       --> "sizeof".
keyword(static)       --> "static".
keyword(struct)       --> "struct".
keyword(switch)       --> "switch".
keyword(typedef)      --> "typedef".
keyword(union)        --> "union".
keyword(unsigned)     --> "unsigned".
keyword(void)         --> "void".
keyword(volatile)     --> "volatile".
keyword(while)        --> "while".
keyword('_Bool')      --> "_Bool".
keyword('_Complex')   --> "_Complex".
keyword('_Imaginary') --> "_Imaginary".
keyword('__attribute__') --> "__attribute__".   % GCC
keyword('__restrict__') --> "__restrict__".
keyword('__restrict__') --> "__restrict".
keyword('__extension__') --> "__extension__".
keyword(inline) --> "__inline__".
keyword(inline) --> "__inline".
keyword('__builtin_va_list') --> "__builtin_va_list".
keyword('__gnuc_va_list') --> "__gnuc_va_list".
keyword('__asm__') --> "__asm__".
keyword('__alignof__') --> "__alignof__".

identifier(Id) --> identifier_nondigit(H), identifier_cont(T),
		   {atom_chars(I, [H|T]), Id = id(I)}.

identifier_cont([H|T]) -->
    identifier_cont_char(H), !,
    identifier_cont(T).
identifier_cont([]) --> [].

identifier_cont_char(H) -->
    identifier_nondigit(H), !.
identifier_cont_char(H) -->
    digit(H).

identifier_nondigit(I) --> nondigit(I).
identifier_nondigit(I) --> universal_character_name(I).
%identifier_nondigit(I) --> other_implementation_defined_characters(I).

term_expansion((nondigit(_) --> "_"), Clauses) :-
    findall((nondigit(C) --> S), nondigit_code(C, S), Clauses).

nondigit_code(Char, [C]) :-
    (   C = 0'_ ; between(0'a,0'z,C) ; between(0'A,0'Z,C) ),
    char_code(Char, C).

nondigit(_) --> "_".

digit('0') --> "0".
digit('1') --> "1".
digit('2') --> "2".
digit('3') --> "3".
digit('4') --> "4".
digit('5') --> "5".
digit('6') --> "6".
digit('7') --> "7".
digit('8') --> "8".
digit('9') --> "9".

universal_character_name(C) -->
    "\\u", hex_quad(V),
    { char_code(C, V) }.
universal_character_name(C) -->
    "\\U", hex_quad(V), hex_quad(W),
    { Code is (V<<32) + W, char_code(C, Code) }.

hex_quad(V) -->
    hexadecimal_digit(X1),
    hexadecimal_digit(X2),
    hexadecimal_digit(X3),
    hexadecimal_digit(X4),
    { V is (X1<<12) + (X2<<8) + (X3<<4) + X4 }.


constant(C) --> integer_constant(C).
constant(C) --> floating_constant(C).
constant(C) --> enumeration_constant(C).
constant(C) --> character_constant(C).

integer_constant(C) -->
    decimal_constant(D), opt_integer_suffix(S), {mkic(S,D,C)}.
integer_constant(C) -->
    octal_constant(D), opt_integer_suffix(S), {mkic(S,D,C)}.
integer_constant(C) -->
    hexadecimal_constant(D), opt_integer_suffix(S), {mkic(S,D,C)}.

mkic(Suffix, Value, Token) :- Token =.. [Suffix,Value].

decimal_constant(D) -->
    nonzero_digit(D0), digits(DT), { number_chars(D, [D0|DT]) }.

octal_constant(D) -->
    "0", octal_digits(L), { L == [] -> D = 0 ; number_chars(D, ['0',o|L]) }.

digits([H|T]) --> digit(H), !, digits(T).
digits([]) --> [].

octal_digits([H|T]) --> octal_digit(H), !, octal_digits(T).
octal_digits([]) --> [].

hexadecimal_constant(D) -->
    hexadecimal_prefix,
    hexadecimal_digits(0, D).

hexadecimal_prefix --> "0x".
hexadecimal_prefix --> "0X".

hexadecimal_digits(V0, V) -->
    hexadecimal_digit(D),
    !,
    { V1 is V0*16+D },
    hexadecimal_digits(V1, V).
hexadecimal_digits(V, V) -->
    [].

nonzero_digit('1') --> "1".
nonzero_digit('2') --> "2".
nonzero_digit('3') --> "3".
nonzero_digit('4') --> "4".
nonzero_digit('5') --> "5".
nonzero_digit('6') --> "6".
nonzero_digit('7') --> "7".
nonzero_digit('8') --> "8".
nonzero_digit('9') --> "9".

octal_digit('0') --> "1".
octal_digit('1') --> "1".
octal_digit('2') --> "2".
octal_digit('3') --> "3".
octal_digit('4') --> "4".
octal_digit('5') --> "5".
octal_digit('6') --> "6".
octal_digit('7') --> "7".

hexadecimal_digit(0)  --> "0".
hexadecimal_digit(1)  --> "1".
hexadecimal_digit(2)  --> "2".
hexadecimal_digit(3)  --> "3".
hexadecimal_digit(4)  --> "4".
hexadecimal_digit(5)  --> "5".
hexadecimal_digit(6)  --> "6".
hexadecimal_digit(7)  --> "7".
hexadecimal_digit(8)  --> "8".
hexadecimal_digit(9)  --> "9".
hexadecimal_digit(10) --> "a".
hexadecimal_digit(11) --> "b".
hexadecimal_digit(12) --> "c".
hexadecimal_digit(13) --> "d".
hexadecimal_digit(14) --> "e".
hexadecimal_digit(15) --> "f".
hexadecimal_digit(10) --> "A".
hexadecimal_digit(11) --> "B".
hexadecimal_digit(12) --> "C".
hexadecimal_digit(13) --> "D".
hexadecimal_digit(14) --> "E".
hexadecimal_digit(15) --> "F".

%!  opt_integer_suffix(-Suffix)//
%
%   Bind Suffix to one of `i`, `u`, `l`, `ll`, `ul`, `ull`

opt_integer_suffix(S) --> unsigned_suffix, long_suffix(L), !, {mkuis(L, S)}.
opt_integer_suffix(S) --> long_suffix(L), unsigned_suffix, !, {mkuis(L, S)}.
opt_integer_suffix(u) --> unsigned_suffix, !.
opt_integer_suffix(L) --> long_suffix(L), !.
opt_integer_suffix(i) --> [].

mkuis(l, ul).
mkuis(ll, ull).

unsigned_suffix --> "u".
unsigned_suffix --> "U".

long_suffix(ll) --> "ll".
long_suffix(ll) --> "LL".
long_suffix(l)  --> "l".
long_suffix(l)  --> "L".

floating_constant(F) --> decimal_floating_constant(F).
floating_constant(F) --> hexadecimal_floating_constant(F).

decimal_floating_constant(F) -->
    fractional_constant(FC),
    opt_exponent_part(E),
    floating_suffix(FS),
    { mkf(FS, FC, E, F) }.
decimal_floating_constant(F) -->
    digit_sequence_value(FC),
    opt_exponent_part(E),
    floating_suffix(FS),
    { mkf(FS, FC, E, F) }.

hexadecimal_floating_constant(F) -->
    hexadecimal_prefix,
    (   "."
    ->  hexadecimal_fractional_part(FC)
    ;   hexadecimal_digits(0, IC),
        ".",
        hexadecimal_fractional_part(FCP),
        { FC is IC+FCP }
    ),
    binary_exponent_part(E),
    floating_suffix(FS),
    { mkf(FS, FC, E, F) }.

mkf(float, FC, E, float(V)) :- V is FC*E.
mkf(double, FC, E, double(V)) :- V is FC*E.

fractional_constant(FC) -->
    digit_sequence(DC1), ".",
    digits(DC2),
    {   DC2 == ""
    ->  number_chars(FC, DC1)
    ;   append(DC1, [.|DC2], S),
        number_chars(FC, S)
    }.

digit_sequence([D0|DL]) -->
    digit(D0), digits(DL).

digit_sequence_value(Value) -->
    digit_sequence(S),
    { number_chars(Value, S) }.

hexadecimal_fractional_part(V) -->
    hexadecimal_fractional_part(10, 0, V).

hexadecimal_fractional_part(I, V0, V) -->
    hexadecimal_digit(D), !,
    { V1 is V0+D/I,
      I2 is I/10
    },
    hexadecimal_fractional_part(I2, V1, V).
hexadecimal_fractional_part(_, V, V) --> [].

opt_exponent_part(M) -->
    exp_e,
    sign(S),
    digit_sequence_value(V),
    { M is 10**(S*V) }.

binary_exponent_part(M) -->
    bin_e,
    sign(S),
    digit_sequence_value(V),
    { M is 10**(S*V) }.

exp_e --> "e".
exp_e --> "E".

bin_e --> "p".
bin_e --> "P".

sign(-1) --> "-", !.
sign(1)  --> "+", !.
sign(1)  --> "".

floating_suffix(float)  --> "f", !.
floating_suffix(float)  --> "F", !.
floating_suffix(double) --> "l", !.
floating_suffix(double) --> "L", !.
floating_suffix(double) --> "".         % TBD: correct?

%!  enumeration_constant(-Enum)//

enumeration_constant(enum_value(ID)) -->
    identifier(ID).

character_constant(C) -->
    "'", c_char_sequence(V), "'",
    { C = char(V) }.
character_constant(C) -->
    "L'", c_char_sequence(V), "'",
    { C = wchar(V) }.

c_char_sequence([H|T]) -->
    c_char(H),
    c_char_sequence_z(T).

c_char_sequence_z([H|T]) --> c_char(H), !, c_char_sequence_z(T).
c_char_sequence_z([]) --> "".

c_char(C) --> [C], { \+ no_c_char(C) }, !.
c_char(C) --> escape_sequence(C).

no_c_char(0'\').
no_c_char(0'\\).
no_c_char(0'\n).

escape_sequence(C) --> simple_escape_sequence(C).
escape_sequence(C) --> octal_escape_sequence(C).
escape_sequence(C) --> hexadecimal_escape_sequence(C).
escape_sequence(C) --> universal_character_name(C).

simple_escape_sequence('\'') --> "\\'".
simple_escape_sequence('\"') --> "\\\"".
simple_escape_sequence('??') --> "\\?".		% TBD: what is this?
simple_escape_sequence('\a') --> "\\a".
simple_escape_sequence('\b') --> "\\b".
simple_escape_sequence('\f') --> "\\f".
simple_escape_sequence('\n') --> "\\n".
simple_escape_sequence('\r') --> "\\r".
simple_escape_sequence('\t') --> "\\t".
simple_escape_sequence('\v') --> "\\v".

octal_escape_sequence(C) -->
    "\\",
    octal_digit(D0),
    (   octal_digit(D1)
    ->  (   octal_digit(D2)
        ->  {number_chars(C, ['0',o,D0,D1,D2])}
        ;   {number_chars(C, ['0',o,D0,D1])}
        )
    ;   {number_chars(C, ['0',o,D0])}
    ).

hexadecimal_escape_sequence(C) -->
    "\\x",
    hexadecimal_digits(0, Code),
    { char_code(C, Code) }.

string_literal(S) -->
    "\"", s_char_sequence(Chars), "\"",
    { string_chars(Str,Chars), S = str(Str) }.
string_literal(S) -->
    "L\"", s_char_sequence(Chars), "\"",
    { string_chars(Str,Chars), S = wstr(Str) }.

s_char_sequence([H|T]) --> s_char(H), !, s_char_sequence(T).
s_char_sequence([]) --> "".

s_char(C) --> [C], { \+ no_s_char(C) }, !.
s_char(C) --> escape_sequence(C).

no_s_char(0'\").
no_s_char(0'\\).
no_s_char(0'\n).


%!  punctuator(-Punct)//
%
%   6.4.6

punctuator('[') --> "[".
punctuator(']') --> "]".
punctuator('(') --> "(".
punctuator(')') --> ")".
punctuator('{') --> "{".
punctuator('}') --> "}".
punctuator('...') --> "...".
punctuator('.') --> ".".
punctuator('->') --> "->".
punctuator('++') --> "++".
punctuator('--') --> "--".
punctuator('&') --> "&".
punctuator('*') --> "*".
punctuator('+') --> "+".
punctuator('-') --> "-".
punctuator('~') --> "~".
punctuator('!') --> "!".
punctuator('/') --> "/".
punctuator('%') --> "%".
punctuator('<<') --> "<<".
punctuator('>>') --> ">>".
punctuator('<') --> "<".
punctuator('>') --> ">".
punctuator('<=') --> "<=".
punctuator('>=') --> ">=".
punctuator('?') --> "?".
punctuator(':') --> ":".
punctuator(';') --> ";".
punctuator('=') --> "=".
punctuator('*=') --> "*=".
punctuator('/=') --> "/=".
punctuator('%=') --> "%=".
punctuator('+=') --> "+=".
punctuator('-=') --> "-=".
punctuator('<<=') --> "<<=".
punctuator(',') --> ",".
punctuator('#') --> "#".
punctuator('##') --> "##".
punctuator('<:') --> "<:".
punctuator(':>') --> ":>".
punctuator('<%') --> "<%".
punctuator('%>') --> "%>".
punctuator('%:') --> "%:".
punctuator('%:%:') --> "%:%:".
punctuator('==') --> "==".
punctuator('>>=') --> ">>=".
punctuator('!=') --> "!=".
punctuator('&=') --> "&=".
punctuator('^') --> "^".
punctuator('|') --> "|".
punctuator('^=') --> "^=".
punctuator('&&') --> "&&".
punctuator('||') --> "||".
punctuator('|=') --> "|=".

header_name(Header) -->
    "<", string_without(">\n", Codes), ">", !,
    { atom_codes(Name, Codes),
      Header = header(ab, Name)
    }.
header_name(Header) -->
    "\"", string_without("\"\n", Codes), "\"", !,
    { atom_codes(Name, Codes),
      Header = header(dq, Name)
    }.

pp_number(PP) -->
    digits(D1a),
    (   ".",
        digits(D2a)
    ->  {append(D1a, [.|D2a], D1)}
    ;   {D1 = D1a}
    ),
    (   identifier_nondigit(NDa)
    ->  {D2 = [NDa]}
    ;   {D2 = []}
    ),
    pp_e(D3),
    (   "."
    ->  {D4 = [.]}
    ;   {D4 = []}
    ),
    { append([D1,D2,D3,D4], D),
      string_chars(S, D),
      PP = pp(S)
    }.

pp_e([C1|C2]) -->
    pp_ee(C1),
    pp_sign(C2).

pp_ee(e) --> "e".
pp_ee('E') --> "e".
pp_ee(p) --> "p".
pp_ee('P') --> "P".

pp_sign('-') --> "-".
pp_sign('+') --> "+".

pp_line(pp(Line)) -->
    "#", string(Codes), "\n", !,
    { string_codes(Line, [0'#|Codes]) }.

