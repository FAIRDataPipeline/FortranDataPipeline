include(GNUInstallDirs)
include(CMakePackageConfigHelpers)
include(FetchContent)

# Get C/C++ API dependency
find_package(fdpapi QUIET)
if(NOT fdpapi_FOUND)
  if(FDPFORT_NO_FETCH_C_API)
    message(FATAL_ERROR "FDP C/C++ API not found.")
  endif()
  FetchContent_Declare(
    fdpapi
    GIT_REPOSITORY https://github.com/FAIRDataPipeline/cppDataPipeline
    GIT_TAG main
    )
  FetchContent_MakeAvailable(fdpapi)
endif()


set(FDPFORT_SOURCE_FILES
  ./fdp_c_utils.f90
  ./fdp.f90
)
add_library(fdpfort ${FDPFORT_SOURCE_FILES})
add_library(fdpfort::fdpfort ALIAS fdpfort)

target_link_libraries(fdpfort PRIVATE fdpapi::fdpapi)

# Location of built .mod files
set(FDPFORT_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/mod")
set(FDPFORT_MODULE_INSTALL_DIRECTORY "${CMAKE_INSTALL_INCLUDEDIR}")

set_target_properties(fdpfort PROPERTIES
  Fortran_FORMAT FREE
  Fortran_MODULE_DIRECTORY "${FDPFORT_MODULE_DIRECTORY}"
  LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib"
  ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib"
  DEBUG_POSTFIX "d"
  )

target_include_directories(
  fdpfort
  PUBLIC
  $<BUILD_INTERFACE:${FDPFORT_MODULE_DIRECTORY}>
  $<INSTALL_INTERFACE:${FDPFORT_MODULE_INSTALL_DIRECTORY}>
  )

# Install bits
if(FDPFORT_WITH_INSTALL)
  message(STATUS "Building install components")

  # TODO Add pkgconfig setup

  if(NOT CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_INSTALL_LIBDIR "${CMAKE_INSTALL_PREFIX}/lib" )
    set(CMAKE_INSTALL_BINDIR "${CMAKE_INSTALL_PREFIX}/bin" )
    set(CMAKE_INSTALL_INCLUDEDIR "${CMAKE_INSTALL_PREFIX}/include" )
    message("installing includes to ${CMAKE_INSTALL_INCLUDEDIR}")
  endif()

  set(fdpfort_targets fdpfort)

  install(TARGETS ${fdpfort_targets}
    EXPORT fdpfortTargets
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
  )

  # Install export file, which contains code to allow other projects to import this project
  install(
    EXPORT fdpfortTargets
    FILE fdpfortTargets.cmake
    NAMESPACE fdpfort::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/fdpfort
  )

  # Install mod files
  install(DIRECTORY "${FDPFORT_MODULE_DIRECTORY}/"
    DESTINATION "${FDPFORT_MODULE_INSTALL_DIRECTORY}"
    FILES_MATCHING PATTERN "*.mod"
  )

  # Create files that allow installed project to be discovered using find_package
  configure_package_config_file(
    "${PROJECT_SOURCE_DIR}/fdpfortConfig.cmake.in"
    "${PROJECT_BINARY_DIR}/fdpfortConfig.cmake"
    INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/fdpfort"
  )

  write_basic_package_version_file(
    "${PROJECT_BINARY_DIR}/fdpfortConfigVersion.cmake"
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMinorVersion
  )

  # Specify where to install the config files we generated
  install(
    FILES 
      "${PROJECT_BINARY_DIR}/fdpfortConfig.cmake"
      "${PROJECT_BINARY_DIR}/fdpfortConfigVersion.cmake"
    DESTINATION 
      "${CMAKE_INSTALL_LIBDIR}/cmake/fdpfort"
  )
endif()
