# Accessing C data

Interaction with native C functions requires the ability to work with C
data structures from Prolog. Arithmetic types (various sizes of integers
and floating point numbers) are simple as such parameters are easily
converted from Prolog numbers and the return value is easily converted
back. Arithmetic types have no interesting internal structure and need
no memory management such as malloc() and free().

Arrays and structures however do have internal structure and typically
do need memory management. Managing such data is achieved in two layers.
The high level layer reasons in terms of abstract types, while the low
level layer deals with pointers and access to primitive C (scalar) data
types.

## Pointers

The core of the memory access functions is formed by a SWI-Prolog _blob_
of type `c_ptr`.  Such a blob wraps a C pointer.  It has the following
properties:

  - The *type* is an atom or a term struct(Name), union(Name) or
    enum(Name) that represents the C type of an element.
  - The *size* is an integer representing the size of an element
    in bytes.
  - The *count* represents the number of elements.  It is `-1` if
    this is not know.
  - An *indirection level*.  If 0 (zero), it is a pointer to an
    (array of) object(s) of the indicated type.  If 1 it is a
    pointer to a pointer of objects of the indicated type, etc.
  - An optionally associated _free_ function is called if the blob
    is garbage collected by the atom garbage collector.

Pointer blobs are created using predicates c_alloc/2, c_cast/3 and
c_load/2 if the addressed object is not a scalar type. Pointer blobs are
also created by C functions if the return value is a pointer or an
argument is declared as an _output_ argument returning a pointer. A
pointer allocated with c_alloc/2 _owns_ the pointer, reclaiming the
associated memory as the blob is garbage collected.  Function return
and output pointers may be declared to _own_ the pointe using the
`~(Type,Free)` type declaration.  See c_import/3.

Pointer blobs are subject to (atom) garbage collection. Atom reference
counts are used to avoid collecting of pointers that depend on other
pointers. Notable c_load/2 references the original pointer if it returns
a pointer inside the area of the original pointer and c_store/2
references the _Value_ if the _Value_ is a pointer.


## Types

A type is either a primitive type or a constructed type.

### Basic types

The following basic types are identified:

  $ Signed integers :
  `char`, `short`, `int`, `long` and `longlong`
  $ Unsigned integers :
  `uchar`, `ushort`, `uint`, `ulong` and `ulonglong`
  $ Floats :
  `float` and `double`
  $ Pointers :
  `*(Type)`, `closure` (pointer to a function)

In addition, the type =wchar_t= is recognised by the library to
facilitate portable exchange of Unicode text represented as wide
character strings.

### Constructed types

The constructed types are struct(Name), union(Name) and enum(Name). The
c_import/3 directive extracts types that are (transitively) reachable
from imported functions to the current module. In addition, types can be
defined using c_struct/1 and c_union/1. Such declarations can be used to
create and access C binary data without using library functions.

  - [[c_struct/2]]
  - [[c_union/2]]

The defined types may be examined using the following interface:

  - [[c_current_enum/3]]
  - [[c_current_struct/1]]
  - [[c_current_struct/3]]
  - [[c_current_struct_field/4]]
  - [[c_current_union/1]]
  - [[c_current_union/3]]
  - [[c_current_union_field/3]]
  - [[c_current_typedef/2]]
  - [[c_expand_type/2]]
  - [[c_type_size_align/3]]

### The high level interface

  - [[c_alloc/2]]
  - [[c_cast/3]]
  - [[c_load/2]]
  - [[c_store/2]]
  - [[c_nil/1]]
  - [[c_is_nil/1]]

### The low level interface

The low-level interface is build around a SWI-Prolog _blob_ that
represents a C pointer with some metadata. A _blob_ is similar to a
Prolog atom, but blobs are typed and they are intended to deal with
binary data.

  - [[c_calloc/4]]
  - [[c_free/1]]
  - [[c_disown/1]]
  - [[c_alloc_string/3]]
  - [[c_load/4]]
  - [[c_load_string/4]]
  - [[c_load_string/5]]
  - [[c_store/4]]
  - [[c_offset/6]]
  - [[c_typeof/2]]
  - [[c_sizeof/2]]
  - [[c_alignof/2]]
  - [[c_address/2]]
  - [[c_dim/3]]
