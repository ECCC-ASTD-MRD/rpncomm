cmake_minimum_required(VERSION 3.16)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

include(ec_init)
include(ec_parse_manifest)

project(${NAME} VERSION ${VERSION} DESCRIPTION "${DESCRIPTION}") 

set(RPN_COMM_VERSION_STRING "${VERSION}")
string(REPLACE "." "" RPN_COMM_VERSION_MARKER ${VERSION})

include(dump_cmake_variables)
# dump_cmake_variables()

enable_language(Fortran)
enable_language(C)
find_package(MPI)
include(compiler_presets)
set(CMAKE_Fortran_COMPILER ${MPI_Fortran_COMPILER})
set(CMAKE_C_COMPILER ${MPI_C_COMPILER})

add_subdirectory(src)