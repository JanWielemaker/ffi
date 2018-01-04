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

## The high level interface

  - [[c_alloc/2]]

## The low level interface

The low-level interface is build around a SWI-Prolog _blob_ that
represents a C pointer with some metadata. A _blob_ is similar to a
Prolog atom, but blobs are typed and they are intended to deal with
binary data.

  - [[c_calloc/4]]
  - [[c_free/1]]
  - [[c_load/4]]
  - [[c_store/4]]
  - [[c_offset/6]]
  - [[c_typeof/2]]
  - [[c_sizeof/2]]
  - [[c_alignof/2]]
