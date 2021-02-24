---
date: '2020-05-15'
title: Jumble
---

What is it?
===========

Jumble is a library of various utilities in Fortran 2003.

Detailed content of Jumble
==========================

Procedure | Description
--------- | ----------
[argwhere](Doxygen_Jumble/html/argwhere_8f90_source.html) | Finds the indices of true elements.
[avg\_mag](Doxygen_Jumble/html/avg__mag_8f90_source.html) | Computes average magnitude of an array: average of decimal log of absolute value, where not 0.
[averge](Doxygen_Jumble/html/averge_8f90_source.html) | Computes the arithmetic average of arithmetic and geometric averages.
[compare](Doxygen_Jumble/html/compare_8f90_source.html) | Prints maximum absolute difference and maximum relative difference between two real arrays, with the location of this maximum.
[count\_lines](Doxygen_Jumble/html/count__lines_8f90_source.html) | Counts the number of lines in an external file.
[csvread](Doxygen_Jumble/html/csvread_8f90_source.html) | Reads numeric values from a CSV (comma-separated values) file.
[determin](Doxygen_Jumble/html/determin_8f90_source.html) | Computes the determinant of a (2, 2) real matrix.
[divisors](Doxygen_Jumble/html/divisors_8f90_source.html) | Returns all the divisors of a given integer.
[ediff1d](Doxygen_Jumble/html/ediff1d_8f90_source.html) | Computes the differences between consecutive elements of an array.
[eigval](Doxygen_Jumble/html/eigval_8f90_source.html) | Computes real eigenvalues of a (2, 2) real matrix.
[eigvect](Doxygen_Jumble/html/eigvect_8f90_source.html) | Computes the matrix of eigenvectors of a (2, 2) real matrix.
[get\_command\_arg\_dyn](Doxygen_Jumble/html/get__command__arg__dyn_8f90_source.html) | Wrapper for get\_command\_argument with automatic allocation of the character argumet.
[greg2jd](Doxygen_Jumble/html/greg2jd_8f90_source.html) | Converts a date on the Gregorian or Julian calendars to a Julian date. Adapted from David G. Simpson.
[inv\_mat](Doxygen_Jumble/html/inv__mat_8f90_source.html) | Computes the inverse of a (2, 2) real matrix.
[iso\_varying\_string](Doxygen_Jumble/html/iso__varying__string_8f90_source.html) | Varying-length strings, conformant to ISO Fortran 95 extension, by Rich Townsend.
[new\_unit](Doxygen_Jumble/html/new__unit_8f90_source.html) | Provides a licit and not already opened external file unit.
[opt\_merge](Doxygen_Jumble/html/opt__merge_8f90_source.html) | Merges an optional parameter and a default value.
[pack\_indices](Doxygen_Jumble/html/pack__indices_8f90_source.html) | Given a two-dimensional integer array and a list of array values to be excluded, returns the indices of the elements not excluded
[point](Doxygen_Jumble/html/point_8f90_source.html) | Returns the element of an array, given the vector of its indices.
[pr\_matrix](Doxygen_Jumble/html/pr__matrix_8f90_source.html) | Pretty prints a real matrix.
[quadrat](Doxygen_Jumble/html/quadrat_8f90_source.html) | Computes the real roots of a quadratic equation with real coefficients.
[read\_column](Doxygen_Jumble/html/read__column_8f90_source.html) | Reads a column of values in an external file.
[rectsph](Doxygen_Jumble/html/spherical_8f90_source.html) | Converts rectangular coordinates to spherical coordinates.
[set2lin](Doxygen_Jumble/html/set2lin_8f90_source.html) | Solves a set of 2 linear equations.
[sphbase](Doxygen_Jumble/html/spherical_8f90_source.html) | Returns the matrix of the spherical base: (radial vector, colatitude vector, azimuthal vector) in the cartesian vector base: (x, y, z).
[sphrect](Doxygen_Jumble/html/spherical_8f90_source.html) | Converts spherical coordinates to rectangular coordinates.
[uniq](Doxygen_Jumble/html/uniq_8f90_source.html) | Reports or omits repeated elements of an array.

[Doxygen Documentation](Doxygen_Jumble/html/index.html)

Installation
============

1.  Jumble uses the library
    [NR\_util](https://www.lmd.jussieu.fr/~lguez/NR_util_site/index.html)
    so install NR\_util first (with the same compiler than the one you
    want to use for Jumble).
2.  Get [Jumble from Github](https://github.com/lguez/Jumble). The
    directory you get could be called Jumble or Jumble-master (depending
    on whether you cloned or downloaded a ZIP file).
3.  Open the file `GNUmakefile` with a text editor. Locate the line:

        nr_util_inc_dir =


    Complete it with the directory containing `nr_util.mod` (installed
    in step 1 above).

4.  By default, the GNUmakefile is set to use the GNU compiler,
    gfortran. Optionally, you can use another Fortran 2003 compiler:
    change the value of `FC`, in `GNUmakefile` (for example, you may
    want to set it to ifort, pgfortran...).
5.  Optionally, you may choose additional compiler options by adding to
    the variable `FFLAGS`. You could also change optimization options
    from the default `-O2`.
6.  On MacOS, remove the letter U in the value of the variable
    `ARFLAGS`, in `GNUmakefile`.
7.  In a terminal, go to the Jumble directory, for example:

        cd ~/Downloads/Jumble-master

    The makefile is written for GNU make. The command invoking GNU make
    may be `make` or `gmake`. So, for example, type in the terminal:

        make

With most compilers, `libjumble.a` and `jumble.mod` are the only files
you will need, but with some compilers, you may need all the `.mod`
files produced by the compilation of Jumble. (Keep them all to be on the
safe side, or experiment with your compiler to see what you need to keep
(see usage below).) You can trash everything else (`.o` files, Fortran
source files, etc.) if you want.

Usage
=====

The name of the module that you must use in your Fortran program is
`jumble`. For example:

    use jumble, only: new_unit

If your program uses Jumble, it will require `jumble.mod` (and possibly
other `.mod` files produced by compilation of Jumble) at compile time
and `libnr_util.a` and `libjumble.a` at link time. For most compilers,
the options you will need to add are:

    -I$jumble_inc_dir

at compile time and:

    -L$nr_util_lib_dir -L$jumble_lib_dir -ljumble -lnr_util

at link time, where:

-   `$jumble_inc_dir` is the name of the directory where you put `.mod`
    files produced by compilation of Jumble;
-   `$nr_util_lib_dir` is the name of the directory where you put
    `libnr_util.a`;
-   `$jumble_lib_dir` is the name of the directory where you put
    `libjumble.a`.
