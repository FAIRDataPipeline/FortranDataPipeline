find_package(PFUNIT QUIET)
if(NOT PFUNIT_FOUND)
  if(FDPFORT_NO_FETCH_PFUNIT)
    message(FATAL_ERROR "pFUnit not found.")
  endif()
  FetchContent_Declare(
      pfunit
      GIT_REPOSITORY "https://github.com/Goddard-Fortran-Ecosystem/pFUnit.git"
      GIT_TAG v4.8.0
    )
  set(SKIP_OPENMP YES CACHE BOOL "" FORCE)
  set(SKIP_MPI YES CACHE BOOL "" FORCE)
  set(SKIP_FHAMCREST YES CACHE BOOL "" FORCE)
  set(SKIP_ROBUST YES CACHE BOOL "" FORCE)
  FetchContent_MakeAvailable(pfunit)
endif()

include(CTest)
enable_testing()

# Helper C lib
set(FDPFORT_TEST_HELPER_SOURCE_FILES
  ./test_c_utils.h
  ./test_c_utils.c
)
add_library(fdpfort_test_helpers ${FDPFORT_TEST_HELPER_SOURCE_FILES})
add_library(fdpfort::test_helpers ALIAS fdpfort_test_helpers)

# Test creation utility function
set(FDPFORT_CTEST_CASES)
add_custom_target(build-tests)
function(fdpfort_add_test test_name test_source)
  add_pfunit_ctest(fdpfort_test_${test_name} 
    TEST_SOURCES "${test_source}"
    LINK_LIBRARIES fdpfort::fdpfort fdpfort::test_helpers
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
    )
  list(APPEND FDPFORT_CTEST_CASES fdpfort_test_${test_name})
  add_dependencies(build-tests fdpfort_test_${test_name})
endfunction()

fdpfort_add_test(c_utils "test_c_utils.pf")
fdpfort_add_test(fdp "test_fdp.pf")

add_custom_target(
  run-tests
  COMMAND ctest --output-on-failure
  DEPENDS ${FDPFORT_CTEST_CASES}
)
add_dependencies(run-tests build-tests)
