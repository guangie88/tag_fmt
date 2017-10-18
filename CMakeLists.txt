cmake_minimum_required(VERSION 3.3)

# project variables
project(tag_fmt VERSION 0.1.0 LANGUAGES CXX)

# general fixed compiler settings
if(${MSVC})
  set(DEBUG_FLAGS "/W4;")
else()
  set(DEBUG_FLAGS "-Wall;")
endif()

add_compile_options("$<$<CONFIG:DEBUG>:${DEBUG_FLAGS}>")
include(CMakeToolsHelpers OPTIONAL)

# fmt and Catch do not have modular CMake mechanism
add_subdirectory(deps/rustfp)

# tag_fmt
add_library(tag_fmt INTERFACE)

target_include_directories(tag_fmt
  INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/deps/fmt>
    $<INSTALL_INTERFACE:include>)

target_link_libraries(tag_fmt
  INTERFACE
    rustfp)

install(TARGETS tag_fmt EXPORT tag_fmt)
install(FILES include/tag_fmt.h DESTINATION include)
install(DIRECTORY deps/fmt/fmt DESTINATION include
  PATTERN deps/fmt/fmt/*.txt EXCLUDE)

install(EXPORT tag_fmt
  FILE tag_fmt-targets.cmake
  NAMESPACE tag_fmt::
  DESTINATION lib/cmake/tag_fmt)

include(CMakePackageConfigHelpers)

configure_package_config_file(
  cmake/tag_fmt-config.cmake.in
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/tag_fmt-config.cmake"
  INSTALL_DESTINATION lib/cmake/tag_fmt
  NO_CHECK_REQUIRED_COMPONENTS_MACRO)

write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/tag_fmt-config-version.cmake"
  VERSION ${tag_fmt_VERSION}
  COMPATIBILITY SameMajorVersion)

install(
  FILES
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/tag_fmt-config.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/tag_fmt-config-version.cmake"
  DESTINATION lib/cmake/tag_fmt)

# tag_fmt_unit_test
if(TAG_FMT_INCLUDE_UNIT_TESTS)
  add_executable(tag_fmt_unit_test
    src/unit_test/unit_test.cpp)

  set_property(TARGET tag_fmt_unit_test PROPERTY CXX_STANDARD 14)

  target_include_directories(tag_fmt_unit_test
    PRIVATE
      $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/deps/Catch/include>)

  target_link_libraries(tag_fmt_unit_test
    PRIVATE
      tag_fmt)
endif()
