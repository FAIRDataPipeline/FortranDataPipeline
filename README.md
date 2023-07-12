# FortranDataPipeline

Fortan implementation of the FAIR Data Pipeline API

## Build

First build `pFUnit` and install it to `./install`. Then:

```bash
$ cmake -B build -DCMAKE_INSTALL_PREFIX=./install -DCMAKE_PREFIX_PATH=./install -DFDPFORT_BUILD_TESTS=ON
$ cmake --build build --target install
$ cmake --build build --target run-tests
```
