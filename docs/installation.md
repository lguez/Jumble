# Installation

## Dependencies

- [CMake](https://cmake.org/download) (version ≥ 3.16)[^1].

- A Fortran compiler.

## Instructions

1. Get [Jumble from Github](https://github.com/lguez/Jumble). The
directory you get could be called Jumble or Jumble-master (depending
on whether you cloned or downloaded a ZIP file).

2.  Create a build subdirectory in the directory you have just downloaded:

        cd the-directory-you-have-just-downloaded
        mkdir build
        cd build

3.  Decide in which directory you want to install Jumble after
    compilation and type the command below with your choice after
    `-DCMAKE_INSTALL_PREFIX=` (enter an absolute path). For example:

        cmake .. -DCMAKE_INSTALL_PREFIX=~/.local

4.  Type:

        make install

You do not need to keep the downloaded directory (nor the build
directory) after installation.

## Advanced instructions

- You can choose any name and any location for the build
  directory. You have to refer to the source directory when you run
  cmake from the build directory:

		mkdir /wherever/any/name
		cd /wherever/any/name
		cmake /where/I/downloaded/Jumble -DCMAKE_INSTALL_PREFIX=~/.local

- Optionally, you can change the precision for real numbers in
  procedures `zroots_unity`, `outersum`, `cumprod`, `outerdiv` and
  `vabs`. By default, the precision for real numbers in Jumble is set
  to be the default kind of the real type of the Fortran compiler you
  will use. You can change the value for the procedures listed above
  to any real kind you want. For example:

		cmake .. -DJumble_CPP_WP='kind(0d0)'

	or you could choose `selected_real_kind(10)` for example, etc.

[^1]: On Mac OS, after downloading the application from the CMake web
    site, run it, then click on "How to Install For Command Line Use"
    in the Tools menu.
