cmake_minimum_required(VERSION 3.20)

project(fdpfort
  VERSION 0.1.0
  DESCRIPTION "Fortran for the FAIR Data Pipeline"
  HOMEPAGE_URL "https://github.com/FAIRDataPipeline/FortranDataPipeline"
  LANGUAGES Fortran C CXX
)

# Ensure we can build a Fortran/C interface
include(FortranCInterface)
FortranCInterface_VERIFY()

option(FDPFORT_BUILD_TESTS  "Build unit tests" OFF)
option(FDPFORT_WITH_INSTALL "Allow project to be installable" ON)
option(FDPFORT_NO_FETCH_C_API "Don't use FetchContent to get the FDP C/C++ API" OFF)
option(FDPFORT_NO_FETCH_PFUNIT "Don't use FetchContent to get pFUnit" OFF)

add_subdirectory(src)

if(FDPFORT_BUILD_TESTS)
  add_subdirectory(tests)
endif()
