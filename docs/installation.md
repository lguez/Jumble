# Installation

Get [Jumble from Github](https://github.com/lguez/Jumble). The
directory you get could be called Jumble or Jumble-master (depending
on whether you cloned or downloaded a ZIP file).

## Installation with CMake

This is the recommended way.

Dependency: you must first install [CMake](https://cmake.org/download)
(version ≥ 3.16)[^1].

2.  Type:

        cd the-directory-you-just-downloaded
        mkdir build
        cd build

3.  Decide in which directory you want to install Jumble after
    compilation and type the command below with your choice after
    `-DCMAKE_INSTALL_PREFIX=` (enter an absolute path). For example,
    a good choice could be `-DCMAKE_INSTALL_PREFIX=~/.local`.

        cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/wherever

4.  Type:

        make install

Optionally: you can change the working precision for real numbers. By
default, the working precision for real numbers in Jumble is set to
be the default kind of the real type of the Fortran compiler you will
use. You can change the value to any real kind you want. For example:

	cmake .. -DJumble_CPP_WP='kind(0d0)'
	
or you could choose `selected_real_kind(10)` for example, etc.

## Installation directly with make

This is the (old) less automated way, not recommended.

1.  Optionally, you can change the working precision for real
    numbers. Open the file `GNUmakefile` with a text editor, change
    the value of the macro `CPP_WP` to any real kind you want (for
    example, `kind(0d0)`, or `selected_real_kind(10)`...).
	
1.  By default, the GNUmakefile is set to use the GNU compiler,
    gfortran. Optionally, you can use another Fortran 2003 compiler:
    change the value of `FC`, in `GNUmakefile` (for example, you may
    want to set it to ifort, pgfortran...).
	
5.  Optionally, you may choose additional compiler options by adding
    to the variable `FFLAGS`, in `GNUmakefile`. You could also change
    optimization options from the default `-O2`.
	
6.  In a terminal, go to the Jumble directory, for example:

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

[^1]: On Mac OS, after downloading the application from the CMake web
    site, run it, then click on "How to Install For Command Line Use"
    in the Tools menu.
