---
date: '2021-10-5'
title: 'NR\_util'
---

What is it?
===========

NR\_util is a Fortran 2003 library. It contains basic mathematical
constants and basic procedures for numerical tasks.

NR\_util originates from the "utility routines" of [Numerical
Recipes](http://numerical.recipes) in Fortran 90 (Press et al., 1996,
Cambridge University Press, appendix C1), version `2.10a`, contained in
files [`nrtype.f90` and
`nrutil.f90`](http://numerical.recipes/public-domain.html). These files
of Numerical Recipes were put in the public domain by Press et al.

Why not just use the original files of Press et al.? The motivations
were: [better treatment of kind
attribute](#rationale-for-treatment-of-kind-attribute); more
comfortable and secure interface taking advantage of the evolution of
the Fortran language. (Press et al. have not released any new version
of their code since their version 2.10a, in 2002.)

Author: [Lionel GUEZ](https://www.lmd.jussieu.fr/~lguez)

Detailed content of NR\_util
============================

Mathematical constants defined in NR\_util: π, π / 2, 2 π, √2, Euler
constant, π / 180, 180 / π. See module
[nrtype](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/nrtype_8_f90_source.html).

Procedure | Description
--------- | ----------
[array\_copy](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/array__copy_8f90_source.html) | Copies to a destination array dest the one-dimensional array src, or as much of src as will fit in dest.
[arth](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/arth_8f90_source.html) | Arithmetic progression.
[assert_eq](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/assert__eq_8f90_source.html) | Exit with error message if integer arguments not all equal.
[assert](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/assert_8f90_source.html) | Exit with error message if any assertion is false.
[cumprod](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/cumprod_8f90_source.html) | Cumulative product on an array, with optional multiplicative seed.
[cumsum](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/cumsum_8f90_source.html) | Cumulative sums of one-dimensional array, with optional seed value.
[diagadd](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/diagadd_8f90_source.html) | Adds vector to diagonal of a matrix.
[diagmult](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/diagmult_8f90_source.html) | Multiplies vector into diagonal of a matrix.
[geop](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/geop_8f90_source.html) | Return a geometrical progression as an array.
[get_diag](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/get__diag_8f90_source.html) | Gets diagonal of a matrix.
[ifirstloc](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/ifirstloc_8f90_source.html) | Location of first true value in a logical array, returned as an integer.
[lower_triangle](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/lower__triangle_8f90_source.html) | Returns a lower triangular logical mask.
[nrerror](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/nrerror_8f90_source.html) | Exit with error message.
[outerand](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/outerand_8f90_source.html) | Returns the outer logical and of two vectors.
[outerdiff](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/outerdiff_8f90_source.html) | Returns a matrix that is the outer difference of two vectors.
[outerdiv](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/outerdiv_8f90_source.html) | Returns a matrix that is the outer quotient of two vectors.
[outerprod](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/outerprod_8f90_source.html) | Returns the outer product of two vectors.
[outersum](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/outersum_8f90_source.html) | Returns the outer sum of two vectors.
[poly](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/poly_8f90_source.html) | Evaluate a polynomial P(x) for one or more values x, with optional mask.
[poly_term](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/poly__term_8f90_source.html) | Returns partial cumulants of a polynomial, equivalent to synthetic division.
[put_diag](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/put__diag_8f90_source.html) | Sets diagonal of a matrix.
[reallocate](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/reallocate_8f90_source.html) | Reallocate pointer to new size, preserving its contents.
[scatter_add](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/scatter__add_8f90_source.html) | Scatter-adds source vector to specified components of destination vector.
[scatter_max](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/scatter__max_8f90_source.html) | Scatter-max source vector to specified components of destination vector.
[swap](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/swap_8f90_source.html) | Swap elements of two input arguments corresponding to input mask.
[unit_matrix](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/unit__matrix_8f90_source.html) | Sets matrix to be a unit matrix.
[upper_triangle](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/upper__triangle_8f90_source.html) | Returns an upper triangular logical mask.
[vabs](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/vabs_8f90_source.html) | Length of a vector in L2 norm.
[zroots_unity](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/zroots__unity_8f90_source.html) | Returns nn consecutive powers of the nth root of unity.

[Doxygen Documentation](https://www.lmd.jussieu.fr/~lguez/NR_util_Doxygen_html/index.html)

Installation
===

Get [NR\_util from Github](https://github.com/lguez/NR_util). The
directory you get could be called NR\_util or NR\_util-master
(depending on whether you cloned or downloaded a ZIP file).

Installation with CMake
---

This is the recommended way.

Dependency: you must first install [CMake](https://cmake.org/download)
(version ≥ 3.14).

2.  Type:

        cd the-directory-you-just-downloaded
        mkdir build
        cd build

3.  Choose the installation directory `CMAKE_INSTALL_PREFIX` and type
    the command below with your choice after `-DCMAKE_INSTALL_PREFIX=`
    (enter an absolute path). For example, you could choose
    `-DCMAKE_INSTALL_PREFIX=~/.local`.

        cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/wherever

4.  Type:

        make install

Optionally: you can change the working precision for real numbers. By
default, the working precision for real numbers in NR\_util is set to
be the default kind of the real type of the Fortran compiler you will
use. You can change the value to any real kind you want. For example:

	cmake .. -DNR_util_CPP_WP='kind(0d0)'
	
or you could choose `selected_real_kind(10)` for example, etc.

Installation directly with make
---

This is the (old) less automated way, not recommended.

2.  By default, the working precision for real numbers in NR\_util is
    set to be the default kind of the real type of the Fortran compiler
    you will use. Optionally, you can change this working precision for
    real numbers. Open the file `GNUmakefile` with a text editor, change
    the value of the macro `CPP_WP` to any real kind you want (for
    example, `kind(0d0)`, or `selected_real_kind(10)`...).
3.  By default, the GNUmakefile is set to use the GNU compiler,
    gfortran. Optionally, you can use another Fortran 2003 compiler.
    Open then the file `GNUmakefile` with a text editor and change the
    value of `FC` (for example, you may want to set it to ifort,
    pgfortran...).
4.  Optionally, you may choose additional compiler options by adding to
    the variable `FFLAGS`, in `GNUmakefile`. You could also change
    optimization options from the default `-O2`.
5.  In a terminal, go to the NR\_util directory, for example:

        cd ~/Downloads/NR_util-master

    The makefile is written for GNU make. The command invoking GNU make
    may be `make` or `gmake`. So, for example, type in the terminal:

        make

With most compilers, `libnr_util.a` and `nr_util.mod` are the only files
you will need, but with some compilers, you may need all the `.mod`
files produced by the compilation of NR\_util. (Keep them all to be on
the safe side, or experiment with your compiler to see what you need to
keep (see usage below).) You can trash everything else (`.o` files,
Fortran source files, etc.) if you want.

Usage
=====

The name of the module that you must use in your Fortran program is
`nr_util`. For example:

    use nr_util, only: assert, twopi

If your program uses NR\_util, it will require `nr_util.mod` (and
possibly other `.mod` files produced by compilation of NR\_util) at
compile time and `libnr_util.a` at link time. For most compilers, the
options you will need to add are:

    -I$nr_util_inc_dir

at compile time and:

    -L$nr_util_lib_dir -lnr_util

at link time, where `$nr_util_inc_dir` is the name of the directory
where you put `.mod` files produced by compilation of NR\_util and
`$nr_util_lib_dir` is the name of the directory where you put
`libnr_util.a`.

Difference with the original code of Numerical Recipes
======================================================

The names of the procedures of NR\_util are the same as the original
ones. The content of some procedures is changed but no public interface
has been added.

As mentioned above, one of the differences between the original
routines and NR\_util is the treatment of kind attributes. Whereas
Numerical Recipes used three explicit integer kinds (i4b, i2b and
i1b), two explicit real kinds (sp and dp), two explicit complex kinds
(spc and dpc) and one explicit logical kind (lgt), NR\_util only uses
one explicit real kind (which is named wp) (used also for complex
variables). Some procedures of NR\_util are coded with this
parameterized real kind. Other procedures (those which are simple
enough) are duplicated for default real kind and double precision,
with a generic interface to these two versions. You can [see below the
rationale](#rationale-for-treatment-of-kind-attribute) for this
modification if you are interested.

Other differences between the original routines and NR\_util: comments
in source code; one file for each procedure not part of a generic
procedure; one file for each generic procedure, with all its specific
procedures; module variables distributed in relevant module files (e. g.
`npar_arth`) or made local (NPAR\_CUMPROD); removed [stat arguments to
allocate](https://www.lmd.jussieu.fr/~lguez/Pelican/argument-stat-de-linstruction-allocate.html); removed tabulation
characters (they are not allowed by Fortran standard); replaced stop
character string by print and stop 1.

Still other differences between the original routines and NR\_util, on
details, made before git log: renamed some specific procedures
associated to the generic swap; made uniform the interfaces of specific
procedures for swap and made mask an optional argument, so the interface
name masked\_swap no longer exists; added specific cumsum\_d to generic
cumsum; corrected repeated call to size in get\_diag; faster algorithm
in ifirstloc and added optional argument my\_lbound; simplified imaxloc;
simplified iminloc and added specific procedure iminloc\_d; bug fix in
scatter\_max, intent must be inout, not out.

Rationale for treatment of kind attribute
=========================================

The treatment of kind attributes is different in NR\_util than in the
original "utility routines" of Numerical Recipes. The procedures in
NR\_util do not essentially require a large precision. For example,
they do not require by themselves real double precision. But, if a
procedure of NR\_util has a real argument, we want to allow a user to
call this procedure with an actual argument of all possible real kinds
(depending on the needs of the calling program). It does not make
sense to define several named constants for several real kinds (sp and
dp in the original routines): either you duplicate the routines or you
parameterize them with a single named constant. See the page on
[Managing kind parameters for numeric types in Fortran
2003](https://www.lmd.jussieu.fr/~lguez/Pelican/managing-kind-parameters-for-numeric-types-in-fortran-2003.html).

For integer data objects, we have removed the i4b kind parameterization.
This parameterization did not make any sense for the routines of
NR\_util (it was used needlessly for local loop counters, for example).
All the integer data objects of NR\_util are of default integer kind.
