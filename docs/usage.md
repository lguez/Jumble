# Usage

## In the source code of the consumer program

The name of the module that you must use in your Fortran program is
`jumble`. For example:

    use jumble, only: new_unit

## Building the consumer program with CMake

If your program using Jumble is built with CMake, then add the
following lines to the file `CMakeLists.txt` for your program:

```
find_package(Jumble)
target_link_libraries(my_program PRIVATE Jumble::jumble)
```

## Building the consumer program without CMake

If you do not build the consumer program with CMake, take into account
that your program will require `jumble.mod` (and possibly other `.mod`
files produced by compilation of Jumble) at compile time and
`libjumble.a` at link time. For most compilers, the options you will
need to add are:

    -I$jumble_install_dir/include

at compile time and:

    -L$jumble_install_dir/lib -ljumble

at link time, where `$jumble_install_dir` is the name of the directory
where you installed Jumble (the one you specified after
`-DCMAKE_INSTALL_PREFIX=`).
