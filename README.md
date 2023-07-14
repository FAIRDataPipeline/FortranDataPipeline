# FortranDataPipeline

Fortan implementation of the FAIR Data Pipeline API

## Testing

The automatic fetching and installation of pFUnit is currently non-functional, so
pFUnit must be installed manually:

```bash
$ git clone --recursive https://github.com/Goddard-Fortran-Ecosystem/pFUnit
$ cd pFUnit
$ export FC=gfortran
$ cmake -Bbuild -DCMAKE_INSTALL_PREFIX=../install -DSKIP_OPENMP=ON -DSKIP_MPI=ON
$ cmake --build build --target install
```

Then build the project with the flag `FDPFORT_BUILD_TESTS=ON`, and run using the
target `run-tests`:

```bash
$ cmake -Bbuild -DCMAKE_PREFIX_PATH=./install -DFDPFORT_BUILD_TESTS=ON
$ cmake --build build --target run-tests
```
