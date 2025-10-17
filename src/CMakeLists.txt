cmake_minimum_required(VERSION 3.16)

include(CheckSourceCompiles)

option(RPN_BUILD_TESTS "Whether to build the test programs associated with the RPN_Comm library")
option(RPN_BUILD_STUBS "Whether to build the stubs and, if building the serial version of RPN_Comm, include them in the library")

# Files to compile
set(RPN_VERSION_FILE ${CMAKE_CURRENT_BINARY_DIR}/RPN_COMM_version.F90)
file(GLOB PROJECT_INCLUDE_FILES RPN_*.h RPN_COMM*.inc RPN_*.hf c_binding_extras.hf iso_c_binding_extras.hf)
file(GLOB PROJECT_C_FILES RPN_*.c)
file(GLOB PROJECT_F_FILES RPN_COMM*.F90  RPN_COMM*.f90)
file(GLOB PROJECT_MPI_F_FILES RPN_MPI*.F90)
list(APPEND PROJECT_F_FILES ${RPN_VERSION_FILE})

#message(STATUS "(EC) RPN_COMM_VERSION_STRING = ${RPN_COMM_VERSION_STRING}")
#message(STATUS "(EC) RPN_COMM_VERSION_MARKER = ${RPN_COMM_VERSION_MARKER}")

if (RPN_BUILD_STUBS)
   add_subdirectory(stubs)
   set(PROJECT_STUB_FILES "${RPN_STUB_SOURCE_FILES}")
endif()

# Main target and some custom compilation configuration
add_library(${PROJECT_NAME} STATIC
   ${PROJECT_INCLUDE_FILES}
   ${PROJECT_C_FILES}
   ${PROJECT_F_FILES}
   ${PROJECT_MPI_F_FILES}
   ${PROJECT_STUB_FILES}
)

target_link_libraries(${PROJECT_NAME} PUBLIC MPI::MPI_C MPI::MPI_Fortran)
add_library(${PROJECT_NAME}::${PROJECT_NAME} ALIAS ${PROJECT_NAME})

configure_file(RPN_COMM_version.F90.in ${RPN_VERSION_FILE} @ONLY)
target_compile_definitions(${PROJECT_NAME} PUBLIC MARKER=RPN_COMM_marker_${RPN_COMM_VERSION_MARKER} HAVE_RPNCOMM="${PROJECT_VERSION}")
target_include_directories(${PROJECT_NAME} PUBLIC
      $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
      $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/generated>
      $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>)

# Check whether we need our own MPI interfaces
set(CMAKE_REQUIRED_INCLUDES ${MPI_Fortran_INCLUDE_DIRS})
check_source_compiles(Fortran "use mpi, only: mpi_bcast; end" HAS_EXTRA_MPI_ITF SRC_EXT F90)
if (NOT (${HAS_EXTRA_MPI_ITF}))
   target_compile_definitions(${PROJECT_NAME} PRIVATE NEED_EXTRA_MPI_ITF)
endif()

set(C_BINDINGS_FILE ${CMAKE_CURRENT_BINARY_DIR}/c_binding_extras.hf)
add_custom_command(
  OUTPUT  ${C_BINDINGS_FILE}
  COMMAND ${CMAKE_C_COMPILER} ${CMAKE_CURRENT_SOURCE_DIR}/c_binding_extras.c -o ${CMAKE_CURRENT_BINARY_DIR}/c_binding_extras
  COMMAND ${CMAKE_CURRENT_BINARY_DIR}/c_binding_extras > ${C_BINDINGS_FILE}
  COMMENT "Generating c_binding_extras.hf"
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/c_binding_extras.c
  BYPRODUCTS ${C_BINDINGS_FILE}
  USES_TERMINAL
)
add_custom_target(c_binding_extras_include DEPENDS ${C_BINDINGS_FILE})
add_dependencies(${PROJECT_NAME} c_binding_extras_include)

set(GEN_INTERFACES_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/../scripts/gen_interfaces.sh)
set(RPN_COMM_ITF_FILE ${CMAKE_CURRENT_SOURCE_DIR}/generated/RPN_COMM_interfaces.hf)
add_custom_command(
   OUTPUT ${RPN_COMM_ITF_FILE}
   COMMAND ${GEN_INTERFACES_SCRIPT} ${PROJECT_F_FILES} > "${RPN_COMM_ITF_FILE}.tmp"
   COMMAND mv "${RPN_COMM_ITF_FILE}.tmp" ${RPN_COMM_ITF_FILE}
   COMMENT "Generating ${RPN_COMM_ITF_FILE}"
   DEPENDS ${GEN_INTERFACES_SCRIPT} ${PROJECT_F_FILES}
)

set(RPN_COMM_ITF_FILE_INC ${CMAKE_CURRENT_SOURCE_DIR}/generated/RPN_COMM_interfaces.inc)
add_custom_command(
   OUTPUT ${RPN_COMM_ITF_FILE_INC}
   COMMAND cat ${RPN_COMM_ITF_FILE} | grep -v '^\#' | sed -e '/! PREPROCESSOR INTERFACES/q'> "${RPN_COMM_ITF_FILE_INC}.tmp"
   COMMAND mv "${RPN_COMM_ITF_FILE_INC}.tmp" ${RPN_COMM_ITF_FILE_INC}
   COMMENT "Generating RPN_COMM_interfaces.inc"
   DEPENDS ${RPN_COMM_ITF_FILE}
)
add_custom_target(RPN_COMM_interfaces DEPENDS ${RPN_COMM_ITF_FILE_INC})
add_dependencies(${PROJECT_NAME} RPN_COMM_interfaces)

set(RPN_MPI_ITF_FILE ${CMAKE_CURRENT_SOURCE_DIR}/generated/RPN_MPI_interfaces_gen.hf)
add_custom_command(
   OUTPUT ${RPN_MPI_ITF_FILE}
   COMMAND ${GEN_INTERFACES_SCRIPT} ${PROJECT_MPI_F_FILES} > "${RPN_MPI_ITF_FILE}.tmp"
   COMMAND mv "${RPN_MPI_ITF_FILE}.tmp" ${RPN_MPI_ITF_FILE}
   COMMENT "Generating ${RPN_MPI_ITF_FILE}"
   DEPENDS ${GEN_INTERFACES_SCRIPT} ${PROJECT_MPI_F_FILES}
)
add_custom_target(RPN_MPI_interfaces DEPENDS ${RPN_MPI_ITF_FILE})
add_dependencies(${PROJECT_NAME} RPN_MPI_interfaces)

set(FULL_LIB_NAME ${PROJECT_NAME}_${PROJECT_VERSION}${MPI_LIBRARY_VERSION})
set(SHORT_LIB_FILE_NAME lib${PROJECT_NAME}${MPI_LIBRARY_VERSION}.a)
# Adjust library name to include MPI version
set_target_properties(${PROJECT_NAME} PROPERTIES OUTPUT_NAME ${FULL_LIB_NAME})

# Symbolic link for rpn_comm.inc on Linux only, as some systems are case-insensitive
if (CMAKE_SYSTEM_NAME MATCHES "Linux")
   add_custom_command(
      TARGET ${PROJECT_NAME}
      PRE_BUILD
      COMMAND cp -p ${CMAKE_CURRENT_SOURCE_DIR}/RPN_COMM.inc ${CMAKE_CURRENT_BINARY_DIR}/RPN_COMM.inc
      COMMAND ln -sfv RPN_COMM.inc rpn_comm.inc
   )
   add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
      COMMAND ln -sfv RPN_COMM.inc rpn_comm.inc
   )
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/rpn_comm.inc DESTINATION include)
endif()

add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
   # Sanity check for library content
   COMMAND ar t $<TARGET_FILE_NAME:${PROJECT_NAME}> | sort -u > objects.lst
   COMMAND sort -u ${CMAKE_CURRENT_SOURCE_DIR}/REFERENCE.lst > sorted_reference.lst
   COMMAND diff sorted_reference.lst objects.lst

   # Symbolic link to latest build
   COMMAND ln -sfv $<TARGET_FILE_NAME:${PROJECT_NAME}> ${SHORT_LIB_FILE_NAME}
   # Symbolic link to rpncomm.a
   COMMAND ln -sfv $<TARGET_FILE_NAME:${PROJECT_NAME}> librpncomm.a
   # Symbolic link to rpn_comm.a for backward compatibility
   COMMAND ln -sfv $<TARGET_FILE_NAME:${PROJECT_NAME}> librpn_comm.a
   # Old include name (backward compatibility)
   COMMAND ln -sfv RPN_COMM_interfaces.hf RPN_COMM_interfaces_int.inc
)

# Installation
set(DISTINCLUDES
   RPN_COMM.inc
   RPN_COMM_types.inc
   RPN_COMM_constants.inc
   RPN_COMM_ftoc.inc
   RPN_COMM_is_null.inc
   ${RPN_COMM_ITF_FILE}
   ${RPN_COMM_ITF_FILE_INC}
   ${RPN_MPI_ITF_FILE}
   
   RPN_MPI.hf
   RPN_MPI_mpi_layout.hf
   RPN_MPI_macros.hf
   RPN_MPI_mpi_definitions.hf
   RPN_MPI_nulls.hf
   RPN_MPI_mpi_symbols.hf
   RPN_MPI_COMM_interfaces.hf
   RPN_MPI_interfaces.hf

   ${CMAKE_CURRENT_BINARY_DIR}/RPN_COMM_interfaces_int.inc
   ${CMAKE_CURRENT_BINARY_DIR}/rpn_comm.mod
   ${CMAKE_CURRENT_BINARY_DIR}/rpn_comm_globals.mod
   ${CMAKE_CURRENT_BINARY_DIR}/rpn_comm_mpi.mod
)

install(TARGETS ${PROJECT_NAME}
        EXPORT ${PROJECT_NAME}-targets
        INCLUDES DESTINATION include)
install(FILES ${DISTINCLUDES} DESTINATION include)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${SHORT_LIB_FILE_NAME} DESTINATION lib)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/librpn_comm.a DESTINATION lib)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/librpncomm.a DESTINATION lib)

# The tests
if (RPN_BUILD_TESTS)
   set(TEST_SOURCES
      TEST_000.F90
      TEST_001.F90
      TEST_002.F90
      # TEST_003.F90
      TEST_004.F90
      TEST_005.F90
      TEST_006.F90
      TEST_007.F90
      TEST_008.F90
      TEST_009.F90
      TEST_010.F90
      TEST_011.F90
      TEST_012.F90
      # TEST_013.F90
      #TEST_014.F90
      TEST_015.F90
      TEST_016.F90
      TEST_017.F90
      TEST_018.F90
   )

   function(get_test_name SOURCE_FILE TEST_NAME)
      get_filename_component(BASE_NAME ${SOURCE_FILE} NAME_WLE)
      set(${TEST_NAME} ${PROJECT_NAME}_${BASE_NAME} PARENT_SCOPE)
   endfunction(get_test_name)

   # Build the tests
   foreach(TEST_SOURCE ${TEST_SOURCES})
      get_test_name(${TEST_SOURCE} TEST_TARGET)


      set(CMAKE_RPN_TEST_FUNCTION_NAME ${TEST_TARGET})
      set(TEST_PROGRAM_FILE ${CMAKE_CURRENT_BINARY_DIR}/${TEST_TARGET}_main.F90)
      file(WRITE ${TEST_PROGRAM_FILE} "program MAIN_TEST_PROGRAM\n  call ${CMAKE_RPN_TEST_FUNCTION_NAME}\n  stop\nend program MAIN_TEST_PROGRAM\n")

      add_executable(${TEST_TARGET} ${TEST_SOURCE} ${TEST_PROGRAM_FILE})
      target_link_libraries(${TEST_TARGET} ${PROJECT_NAME})
   endforeach()
endif()
