# Installation

## Dependencies

- [CMake](https://cmake.org/download) (version ≥ 3.16)[^1].

- A Fortran compiler.

## Instructions

1. Download
   [Jumble](https://github.com/lguez/Jumble/archive/refs/heads/master.zip).

2. Unzip it:

		unzip Jumble-master.zip

3. Decide in which directory you want to install Jumble and type the
   command below with your choice after `-DCMAKE_INSTALL_PREFIX=`
   (enter an absolute path). For example:

		cmake -B build -S Jumble-master -DCMAKE_INSTALL_PREFIX=~/.local

	The command above is the configuration command. It will create a
	directory named build.

4. Compile:

		cmake --build build

5. Install:

        cmake --install build

You do not need to keep the downloaded directory Jumble-master nor the
build directory after installation.

## Advanced instructions

- You can choose any name and any location for the build
  directory:

		cmake -B /wherever/any/name -S Jumble-master -DCMAKE_INSTALL_PREFIX=~/.local

- Optionally, in the configuration command, you can change the
  precision for real numbers of procedures `zroots_unity`, `outersum`,
  `cumprod`, `outerdiv` and `vabs`. By default, the precision for real
  numbers in Jumble is set to be the default kind of the real type of
  the Fortran compiler you will use. You can change the value for the
  procedures listed above to any real kind you want. For example:

		cmake -B build -S Jumble-master -DJumble_CPP_WP='kind(0d0)'

	or you could choose `selected_real_kind(10)` for example, etc.

[^1]: On Mac OS, after downloading the application from the CMake web
    site, run it, then click on "How to Install For Command Line Use"
    in the Tools menu.
