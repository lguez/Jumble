---
date: '2022-02-16'
---

# Origin of NR_util

The NR\_util part of Jumble originates from the "utility routines" of
[Numerical Recipes](http://numerical.recipes) in Fortran 90 (Press et
al., 1996, Cambridge University Press, appendix C1), version `2.10a`,
contained in files [`nrtype.f90` and
`nrutil.f90`](http://numerical.recipes/public-domain.html). These
files of Numerical Recipes were put in the public domain by Press et
al.

Why not just use the original files of Press et al.? The motivations
were: [better treatment of kind
attribute](#rationale-for-treatment-of-kind-attribute); more
comfortable and secure interface taking advantage of the evolution of
the Fortran language. (Press et al. have not released any new version
of their code since their version 2.10a, in 2002.)

## Difference with the original code of Numerical Recipes

The names of the procedures in sub-directory NR\_util are the same as
the original ones. The content of some procedures is changed but no
public interface has been added.

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
`npar_arth`) or made local (`NPAR_CUMPROD`); removed [stat arguments to
allocate](https://www.lmd.jussieu.fr/~lguez/Pelican/argument-stat-de-linstruction-allocate.html); removed tabulation
characters (they are not allowed by Fortran standard); replaced stop
character string by print and stop 1.

Still other differences between the original routines and NR\_util, on
details, made before git log: renamed some specific procedures
associated to the generic swap; made uniform the interfaces of specific
procedures for swap and made mask an optional argument, so the interface
name `masked_swap` no longer exists; added specific `cumsum_d` to generic
cumsum; corrected repeated call to size in `get_diag`; faster algorithm
in ifirstloc and added optional argument `my_lbound`; simplified imaxloc;
simplified iminloc and added specific procedure `iminloc_d`; bug fix in
`scatter_max`, intent must be inout, not out.

## Rationale for treatment of kind attribute

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
