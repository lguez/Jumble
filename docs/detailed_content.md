# Detailed content of Jumble

Mathematical constants defined in Jumble: π, π / 2, 2 π, √2, Euler
constant, π / 180, 180 / π. See module
[nrtype](https://github.com/lguez/Jumble/blob/master/NR_util/nrtype.f90).

Find below a list of procedures with a short description for each and
a link to the source code.

## From NR\_util

- [array\_copy](https://github.com/lguez/Jumble/blob/master/NR_util/array_copy.f90): Copies to a destination array dest the one-dimensional array src, or as much of src as will fit in dest.

- [arth](https://github.com/lguez/Jumble/blob/master/NR_util/arth.f90): Arithmetic progression.

- [assert\_eq](https://github.com/lguez/Jumble/blob/master/NR_util/assert_eq.f90): Exit with error message if integer arguments not all equal.

- [assert](https://github.com/lguez/Jumble/blob/master/NR_util/assert.f90): Exit with error message if any assertion is false.

- [cumprod](https://github.com/lguez/Jumble/blob/master/NR_util/cumprod.f90): Cumulative product on an array, with optional multiplicative seed.

- [cumsum](https://github.com/lguez/Jumble/blob/master/NR_util/cumsum.f90): Cumulative sums of one-dimensional array, with optional seed value.

- [diagadd](https://github.com/lguez/Jumble/blob/master/NR_util/diagadd.f90): Adds vector to diagonal of a matrix.

- [diagmult](https://github.com/lguez/Jumble/blob/master/NR_util/diagmult.f90): Multiplies vector into diagonal of a matrix.

- [geop](https://github.com/lguez/Jumble/blob/master/NR_util/geop.f90): Return a geometrical progression as an array.

- [get\_diag](https://github.com/lguez/Jumble/blob/master/NR_util/get_diag.f90): Gets diagonal of a matrix.

- [ifirstloc](https://github.com/lguez/Jumble/blob/master/NR_util/ifirstloc.f90): Location of first true value in a logical array, returned as an integer.

- [lower\_triangle](https://github.com/lguez/Jumble/blob/master/NR_util/lower_triangle.f90): Returns a lower triangular logical mask.

- [nrerror](https://github.com/lguez/Jumble/blob/master/NR_util/nrerror.f90): Exit with error message.

- [outerand](https://github.com/lguez/Jumble/blob/master/NR_util/outerand.f90): Returns the outer logical and of two vectors.

- [outerdiff](https://github.com/lguez/Jumble/blob/master/NR_util/outerdiff.f90): Returns a matrix that is the outer difference of two vectors.

- [outerdiv](https://github.com/lguez/Jumble/blob/master/NR_util/outerdiv.f90): Returns a matrix that is the outer quotient of two vectors.

- [outerprod](https://github.com/lguez/Jumble/blob/master/NR_util/outerprod.f90): Returns the outer product of two vectors.

- [outersum](https://github.com/lguez/Jumble/blob/master/NR_util/outersum.f90): Returns the outer sum of two vectors.

- [poly](https://github.com/lguez/Jumble/blob/master/NR_util/poly.f90): Evaluate a polynomial P(x) for one or more values x, with optional mask.

- [poly\_term](https://github.com/lguez/Jumble/blob/master/NR_util/poly_term.f90): Returns partial cumulants of a polynomial, equivalent to synthetic division.

- [put\_diag](https://github.com/lguez/Jumble/blob/master/NR_util/put_diag.f90): Sets diagonal of a matrix.

- [reallocate](https://github.com/lguez/Jumble/blob/master/NR_util/reallocate.f90): Reallocate pointer to new size, preserving its contents.

- [scatter\_add](https://github.com/lguez/Jumble/blob/master/NR_util/scatter_add.f90): Scatter-adds source vector to specified components of destination vector.

- [scatter\_max](https://github.com/lguez/Jumble/blob/master/NR_util/scatter_max.f90): Scatter-max source vector to specified components of destination vector.

- [swap](https://github.com/lguez/Jumble/blob/master/NR_util/swap.f90): Swap elements of two input arguments corresponding to input mask.

- [unit\_matrix](https://github.com/lguez/Jumble/blob/master/NR_util/unit_matrix.f90): Sets matrix to be a unit matrix.

- [upper\_triangle](https://github.com/lguez/Jumble/blob/master/NR_util/upper_triangle.f90): Returns an upper triangular logical mask.

- [vabs](https://github.com/lguez/Jumble/blob/master/NR_util/vabs.f90): Length of a vector in L2 norm.

- [zroots\_unity](https://github.com/lguez/Jumble/blob/master/NR_util/zroots_unity.f90): Returns nn consecutive powers of the nth root of unity.

## Input-output

- [compare](https://github.com/lguez/Jumble/blob/master/compare.f90): Prints maximum absolute difference and maximum relative difference between two real arrays, with the location of this maximum.

- [count\_lines](https://github.com/lguez/Jumble/blob/master/count_lines.f90): Counts the number of lines in an external file.

- [csvread](https://github.com/lguez/Jumble/blob/master/csvread.f90): Reads numeric values from a CSV (comma-separated values) file.

- [get\_command\_arg\_dyn](https://github.com/lguez/Jumble/blob/master/get_command_arg_dyn.f90): Wrapper for get\_command\_argument with automatic allocation of the character argument.

- [new\_unit](https://github.com/lguez/Jumble/blob/master/new_unit.f90): Provides a licit and not already opened external file unit.

- [pr\_matrix](https://github.com/lguez/Jumble/blob/master/pr_matrix.f90): Pretty prints a real matrix.

- [read\_column](https://github.com/lguez/Jumble/blob/master/read_column.f90): Reads a column of values in an external file.

## Linear algebra in 2 dimensions on real numbers

- [determin](https://github.com/lguez/Jumble/blob/master/Numerical/Lin_2d_real/determin.f90): Computes the determinant of a (2, 2) real matrix.

- [eigval](https://github.com/lguez/Jumble/blob/master/Numerical/Lin_2d_real/eigval.f90): Computes real eigenvalues of a (2, 2) real matrix.

- [eigvect](https://github.com/lguez/Jumble/blob/master/Numerical/Lin_2d_real/eigvect.f90): Computes the matrix of eigenvectors of a (2, 2) real matrix.

- [inv\_mat](https://github.com/lguez/Jumble/blob/master/Numerical/Lin_2d_real/inv_mat.f90): Computes the inverse of a (2, 2) real matrix.

- [set2lin](https://github.com/lguez/Jumble/blob/master/Numerical/Lin_2d_real/set2lin.f90): Solves a set of 2 linear equations.

## Other

- [argwhere](https://github.com/lguez/Jumble/blob/master/Numerical/argwhere.f90):
  Finds the indices of true elements.
  
- [avg\_mag](https://github.com/lguez/Jumble/blob/master/avg_mag.f90):
  Computes average magnitude of an array: average of decimal log of
  absolute value, where not 0.
  
- [averge](https://github.com/lguez/Jumble/blob/master/Numerical/averge.f90): Computes the arithmetic average of arithmetic and geometric averages.

- [divisors](https://github.com/lguez/Jumble/blob/master/Numerical/divisors.f90): Returns all the divisors of a given integer.

- [ediff1d](https://github.com/lguez/Jumble/blob/master/Numerical/ediff1d.f90): Computes the differences between consecutive elements of an array.

- [greg2jd](https://github.com/lguez/Jumble/blob/master/greg2jd.f90): Converts a date on the Gregorian or Julian calendar to a Julian date. Adapted from David G. Simpson.

- [iso\_varying\_string](https://github.com/lguez/Jumble/blob/master/iso_varying_string.f90): Varying-length strings, conformant to ISO Fortran 95 extension, by Rich Townsend.

- [opt\_merge](https://github.com/lguez/Jumble/blob/master/opt_merge.f90): Merges an optional parameter and a default value.

- [pack\_indices](https://github.com/lguez/Jumble/blob/master/Numerical/pack_indices.f90): Given a two-dimensional integer array and a list of array values to be excluded, returns the indices of the elements not excluded

- [point](https://github.com/lguez/Jumble/blob/master/point.f90): Returns the element of an array, given the vector of its indices.

- [quadrat](https://github.com/lguez/Jumble/blob/master/Numerical/quadrat.f90): Computes the real roots of a quadratic equation with real coefficients.

- [rectsph](https://github.com/lguez/Jumble/blob/master/Numerical/spherical.f90): Converts rectangular coordinates to spherical coordinates.

- [sphbase](https://github.com/lguez/Jumble/blob/master/Numerical/spherical.f90): Returns the matrix of the spherical base: (radial vector, colatitude vector, azimuthal vector) in the cartesian vector base: (x, y, z).

- [sphrect](https://github.com/lguez/Jumble/blob/master/Numerical/spherical.f90): Converts spherical coordinates to rectangular coordinates.

- [uniq](https://github.com/lguez/Jumble/blob/master/uniq.f90): Reports or omits repeated elements of an array.
