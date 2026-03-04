cmake_minimum_required(VERSION 3.16)

add_custom_target(test_execs)
add_dependencies(check test_execs)

# The tests
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

set(TEST_SOURCES_2
    format_comma.F90
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

    add_executable(${TEST_TARGET} EXCLUDE_FROM_ALL ${TEST_SOURCE} ${TEST_PROGRAM_FILE})
    target_link_libraries(${TEST_TARGET} ${PROJECT_NAME})
    add_dependencies(test_execs ${TEST_TARGET})
endforeach()

foreach(TEST_SOURCE ${TEST_SOURCES_2})
    get_test_name(${TEST_SOURCE} TEST_TARGET)
    add_executable(${TEST_TARGET} ${TEST_SOURCE})
    target_link_libraries(${TEST_TARGET} ${PROJECT_NAME})
    add_dependencies(test_execs ${TEST_TARGET})
    add_test(NAME ${TEST_TARGET} COMMAND $<TARGET_FILE:${TEST_TARGET}>)
endforeach()


