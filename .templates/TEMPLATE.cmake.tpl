project((>>>ProjectName<<<))

cmake_minimum_required(VERSION 2.8.12)
message(STATUS "This is BINARY dir " ${PROJECT_BINARY_DIR})
message(STATUS "This is SOURCE dir " ${PROJECT_SOURCE_DIR})
# convert build type to upper case letters
if(CMAKE_BUILD_TYPE)
  string(TOUPPER ${CMAKE_BUILD_TYPE} CMAKE_BUILD_TYPE_UPPER)
endif()

if(CMAKE_BUILD_TYPE_UPPER MATCHES "DEBUG")
  set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS_DEBUG_INIT})
else()
  set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS_RELEASE_INIT})
endif()

if(NOT ${CMAKE_VERSION} VERSION_LESS 3.1)
  set(CMAKE_CXX_STANDARD 11)
else()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
endif()

if(APPLE AND CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -stdlib=libc++")
endif()

if(NOT DEFINED LIB_SUFFIX)
  set(LIB_SUFFIX "")
endif()
set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX}")

# Add a compiler flag to check the output for insane values if we are in debug mode.
if(CMAKE_BUILD_TYPE_UPPER MATCHES "DEBUG" OR CMAKE_BUILD_TYPE_UPPER MATCHES "RELWITHDEBINFO")
  message(STATUS "Building debug release of Project.")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wextra -O0 -g -fno-omit-frame-pointer")
  add_definitions(-DASSERT_INSANE_OUTPUT)
  add_definitions(-DUSE_CPU_TIME)
  add_definitions(-DDEBUG)
endif()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall") # Add warnings

option (ENABLE_MORE_COMPILER_OPTIMIZATION_FLAGS
  "Enable more optimization flags" ON)
if (ENABLE_MORE_COMPILER_OPTIMIZATION_FLAGS)
  message (STATUS "Compile with more optimization flags")
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS} -Ofast -funroll-loops")
endif ()

if(NOT APPLE AND NOT WIN32)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -static-libstdc++")
endif()

set(MyLib (>>>LIBNAME<<<))
set(SOURCE_FILES # Except main.cpp.
  #add source files , .cpp files
  ${PROJECT_SOURCE_DIR}/src/(>>>POINT<<<)
  )


set(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/bin)
set(LIBRARY_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/lib)
include_directories(${PROJECT_SOURCE_DIR}/src)

# library SHARED or STATIC
add_library(${MyLib} STATIC ${SOURCE_FILES}) #First compile all of CuraEngine as library, allowing this to be re-used for tests.


# https://github.com/ttroy50/cmake-examples/tree/master/01-basic/C-static-library
add_executable(${PROJECT_NAME}
  ${PROJECT_SOURCE_DIR}/src/main.cpp
  )

target_link_libraries( ${PROJECT_NAME}
  PRIVATE
  ${MyLib})

###########################
# Use pre-compiled library#
###########################
# Sometimes you get a compiled library,you can use it in your build

# find_library(TOOLS
#   NAMES  (>>>PRE_COMPILED_LIB<<<)
#   PATHS ${LIBRARY_OUTPUT_PATH})
# # use it for linking
# target_link_libraries(${PROJECT_NAME} ${TOOLS})

###########################
#   Add Gtest with CMake  #
###########################
# add folder tests to cmake project
enable_testing()
add_subdirectory(tests)
# configure tests
add_subdirectory(/usr/src/gtest ${PROJECT_BINARY_DIR}/gtest)
include(CTest)
set(TEST_BINARY ${PROJECT_NAME}_test)
add_executable(${TEST_BINARY} (>>>TEST.cpp<<))

target_link_libraries(${TEST_BINARY}
  (>>>LIB_NAME<<<)   #Library we are testing
  gtest gtest_main  )
#add gtest to be able to run ctest
add_test(
  NAME ${TEST_BINARY}
  COMMAND ${EXECUTABLE_OUTPUT_PATH}/${TEST_BINARY})



# install binary
install(TARGETS ${PROJECT_NAME} DESTINATION ${PROJECT_BINARY_DIR}/bin)

# # install libs
# install(TARGETS mylib_shared DESTINATION cmake-example/lib)
# install(TARGETS mylib_static DESTINATION cmake-example/lib)

# # install headers
# install(FILES ${PROJECT_SOURCE_DIR}/src/mylib.h DESTINATION cmake-example/include) #
