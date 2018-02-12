# SWI-Prolog FFI examples

This directory contains both simple and real examples. Please also study
the test cases in the test directory.

  - *math.pl* is a real starter example, illustrating connecting a
    couple of functions from `-lm`.

  - *printf.pl* illustrates the (limited) handling of variadic
    functions like printf().

  - *mmap.pl* provides file mapping.  It shows handling C preprocessor
    constants, handling the variadic open() call and type casting.

  - *test_mmap.pl* illustrates the use of *mmap.pl* to create a C
    compatible binary file holding an array of structs.

  - *uchardet.pl* connects `-luchardet`.  This a fairly simple example,
    slightly complicated due to the involved character encoding.

  - *iconv.pl* connects the iconv() function to realise a _wrapper
    stream_ in Prolog that can be used to perform I/O from any
    encoding supported by iconv() to the internal Prolog Unicode
    representation.  This is a complicated example due to the
    extensive use of _in_out_ arguments as well as the interaction
    with the Prolog stream library.

