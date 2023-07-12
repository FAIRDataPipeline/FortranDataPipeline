find_package(PFUNIT QUIET)

if(NOT PFUNIT_FOUND)
  if(FDPFORT_NO_FETCH_PFUNIT)
    message(FATAL_ERROR "pFUnit not found.")
  endif()
  FetchContent_Declare(
      pfunit
      GIT_REPOSITORY "https://github.com/Goddard-Fortran-Ecosystem/pFUnit"
      GIT_TAG main
    )
  set(SKIP_OPENMP YES CACHE BOOL "" FORCE)
  set(SKIP_MPI YES CACHE BOOL "" FORCE)
  if (NOT CMAKE_Fortran_FLAGS)
    if (CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
      # Workaround for an issue in pFUnit: preprocessor macro
      # expansion of the current source directory, which may
      # truncate the line
      set(CMAKE_Fortran_FLAGS "-ffree-line-length-none" CACHE STRING "" FORCE)
      message(STATUS "Setting CMAKE_Fortran_FLAGS = ${CMAKE_Fortran_FLAGS}")
    endif()
  endif()
  FetchContent_MakeAvailable(pfunit)
endif()

include(CTest)
enable_testing()

add_custom_target(build-tests)
set(FDPFORT_CTEST_CASES)

# Helper C lib
set(FDPFORT_TEST_HELPER_SOURCE_FILES
  ./test_c_utils.h
  ./test_c_utils.c
)
add_library(fdpfort_test_helpers ${FDPFORT_TEST_HELPER_SOURCE_FILES})
add_library(fdpfort::fdpfort_test_helpers ALIAS fdpfort_test_helpers)

function(fdpfort_add_test test_source test_name)
  add_pfunit_ctest(
    fdpfort_test_${test_name}
    TEST_SOURCES ${test_source}
    LINK_LIBRARIES fdpfort::fdpfort fdpfort::fdpfort_test_helpers
    )
  list(APPEND FDPFORT_CTEST_CASES fdpfort_test_${test_name})
  set(FDPFORT_CTEST_CASES ${FDPFORT_CTEST_CASES} PARENT_SCOPE)
  add_dependencies(build-tests fdpfort_test_${test_name})
endfunction() 

fdpfort_add_test(./test_c_utils.pf c_utils)

add_custom_target(
  run-tests 
  COMMAND ctest --output-on-failure
  DEPENDS ${FDPFORT_CTEST_CASES}
)
add_dependencies(run-tests build-tests)
