include(GNUInstallDirs)
include(CMakePackageConfigHelpers)
include(FetchContent)

set(FDPFORT_SOURCE_FILES
  ./fdp_c_utils.f90
)

add_library(fdpfort ${FDPFORT_SOURCE_FILES})
add_library(fdpfort::fdpfort ALIAS fdpfort)


# Set language standards (CMake doesn't implement this like for C/C++, need a hack)
target_compile_options(fdpfort PRIVATE
  $<$<Fortran_COMPILER_ID:GNU>:
  -std=f2003>
  )

# Set debugging options
set_target_properties(fdpfort PROPERTIES DEBUG_POSTFIX "d")
if (CMAKE_BUILD_TYPE STREQUAL "Debug")
  target_compile_options(fdpfort PRIVATE
  $<$<Fortran_COMPILER_ID:GNU>:
  -g -Wall -fimplicit-none -fbounds-check -Wimplicit-interface -Wimplicit-procedure>
  $<$<Fortran_COMPILER_ID:Intel>:
  -g -implicitnone -warn all -nogen-interfaces -CB -traceback>
  )
endif()

# Install bits
if(FDPFORT_WITH_INSTALL)
  message(STATUS "Building install components")

  # TODO Add pkgconfig setup

  if(NOT CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_INSTALL_LIBDIR ${CMAKE_INSTALL_PREFIX}/lib )
    set(CMAKE_INSTALL_BINDIR ${CMAKE_INSTALL_PREFIX}/bin )
    set(CMAKE_INSTALL_INCLUDEDIR ${CMAKE_INSTALL_PREFIX}/include )
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

  # Create files that allow installed project to be discovered using find_package
  configure_package_config_file(
    "${PROJECT_SOURCE_DIR}/cmake/fdpfortConfig.cmake.in"
    "${PROJECT_BINARY_DIR}/fdpfortConfig.cmake"
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/fdpfort
  )

  write_basic_package_version_file(
    "${PROJECT_BINARY_DIR}/fdpfortConfigVersion.cmake"
    VERSION  ${PROJECT_VERSION}
    COMPATIBILITY SameMinorVersion
  )

  # Specify where to install the config files we generated
  install(
    FILES 
      "${PROJECT_BINARY_DIR}/fdpfortConfig.cmake"
      "${PROJECT_BINARY_DIR}/fdpfortConfigVersion.cmake"
    DESTINATION 
      ${CMAKE_INSTALL_LIBDIR}/cmake/fdpfort
  )
endif()
