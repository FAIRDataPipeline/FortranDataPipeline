# FortranDataPipeline

[![FDP Fortran API](https://github.com/FAIRDataPipeline/FortranDataPipeline/actions/workflows/test.yaml/badge.svg)](https://github.com/FAIRDataPipeline/cppDataPipeline/actions/workflows/test.yaml)
[![License: LGPL v3](https://img.shields.io/badge/License-LGPL_v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8178773.svg)](https://doi.org/10.5281/zenodo.8178773)


Fortan implementation of the FAIR Data Pipeline API


## Contents
  - [Installation](#installation)
  - [Outline](#outline)
  - [Testing](#testing)

## Installation

You can build and test the library using CMake. This implementation requires a compiler
suite that supports C++11 and Fortran 2003.

It is recommended that you install [CURL](https://curl.se/libcurl/) prior to
installation of this API.

Compile the library by running:

```
$ cmake -Bbuild
$ cmake --build build
```

The library can be installed system-wide using:

```
$ cmake --build build --target install
```

After doing so, the library can be discovered and utilised in other CMake projects
using:

```
find_package(fdpfort)
target_link_libraries(my_project PRIVATE fdpfort::fdpfort)
```

## Outline

The Fortran API depends on the C++ API, and links into it via the C API. The main
object the user will interact with is `DataPipeline`, which must be passed to
methods such as `fdp_link_read` etc. This object should be initialised prior to use
using `fdp_init`, and finalised at the end of a program run using `fdp_finalise`.

A logger has been used to give as much feedback to the user as possible. The verbosity
is handled by a log level argument to the function `fdp_log`. The environment variable
`FDP_LOG_LEVEL=[TRACE:DEBUG:INFO:WARN:ERROR:CRITICAL:OFF]` can be set to specify the
logging output level.

## Testing

The unit tests use the local registry. This needs to be running prior to running the
tests. See the [CLI docs](https://github.com/FAIRDataPipeline/FAIR-CLI#registry) for
more information.

Then build the project with the flag `FDPFORT_BUILD_TESTS=ON`, and run using the
target `run-tests`:

```bash
$ cmake -Bbuild . -DFDPFORT_BUILD_TESTS=ON
$ cmake --build build --target run-tests
```
