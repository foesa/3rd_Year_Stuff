# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.16

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /home/foesa/programs/clion-2020.1.1/bin/cmake/linux/bin/cmake

# The command to remove a file.
RM = /home/foesa/programs/clion-2020.1.1/bin/cmake/linux/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = "/home/foesa/Documents/3rd_Year_Stuff/CS3014 Concurrent Systems/assignments/FinalExam/csu33014-annual-partA(3)/csu33014-annual-partA"

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = "/home/foesa/Documents/3rd_Year_Stuff/CS3014 Concurrent Systems/assignments/FinalExam/csu33014-annual-partA(3)/csu33014-annual-partA/cmake-build-debug"

# Include any dependencies generated for this target.
include CMakeFiles/csu33014_annual_partA.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/csu33014_annual_partA.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/csu33014_annual_partA.dir/flags.make

CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-code.c.o: CMakeFiles/csu33014_annual_partA.dir/flags.make
CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-code.c.o: ../csu33014-annual-partA-code.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir="/home/foesa/Documents/3rd_Year_Stuff/CS3014 Concurrent Systems/assignments/FinalExam/csu33014-annual-partA(3)/csu33014-annual-partA/cmake-build-debug/CMakeFiles" --progress-num=$(CMAKE_PROGRESS_1) "Building C object CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-code.c.o"
	/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-code.c.o   -c "/home/foesa/Documents/3rd_Year_Stuff/CS3014 Concurrent Systems/assignments/FinalExam/csu33014-annual-partA(3)/csu33014-annual-partA/csu33014-annual-partA-code.c"

CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-code.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-code.c.i"
	/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E "/home/foesa/Documents/3rd_Year_Stuff/CS3014 Concurrent Systems/assignments/FinalExam/csu33014-annual-partA(3)/csu33014-annual-partA/csu33014-annual-partA-code.c" > CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-code.c.i

CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-code.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-code.c.s"
	/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S "/home/foesa/Documents/3rd_Year_Stuff/CS3014 Concurrent Systems/assignments/FinalExam/csu33014-annual-partA(3)/csu33014-annual-partA/csu33014-annual-partA-code.c" -o CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-code.c.s

CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-main.c.o: CMakeFiles/csu33014_annual_partA.dir/flags.make
CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-main.c.o: ../csu33014-annual-partA-main.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir="/home/foesa/Documents/3rd_Year_Stuff/CS3014 Concurrent Systems/assignments/FinalExam/csu33014-annual-partA(3)/csu33014-annual-partA/cmake-build-debug/CMakeFiles" --progress-num=$(CMAKE_PROGRESS_2) "Building C object CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-main.c.o"
	/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-main.c.o   -c "/home/foesa/Documents/3rd_Year_Stuff/CS3014 Concurrent Systems/assignments/FinalExam/csu33014-annual-partA(3)/csu33014-annual-partA/csu33014-annual-partA-main.c"

CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-main.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-main.c.i"
	/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E "/home/foesa/Documents/3rd_Year_Stuff/CS3014 Concurrent Systems/assignments/FinalExam/csu33014-annual-partA(3)/csu33014-annual-partA/csu33014-annual-partA-main.c" > CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-main.c.i

CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-main.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-main.c.s"
	/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S "/home/foesa/Documents/3rd_Year_Stuff/CS3014 Concurrent Systems/assignments/FinalExam/csu33014-annual-partA(3)/csu33014-annual-partA/csu33014-annual-partA-main.c" -o CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-main.c.s

# Object files for target csu33014_annual_partA
csu33014_annual_partA_OBJECTS = \
"CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-code.c.o" \
"CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-main.c.o"

# External object files for target csu33014_annual_partA
csu33014_annual_partA_EXTERNAL_OBJECTS =

csu33014_annual_partA: CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-code.c.o
csu33014_annual_partA: CMakeFiles/csu33014_annual_partA.dir/csu33014-annual-partA-main.c.o
csu33014_annual_partA: CMakeFiles/csu33014_annual_partA.dir/build.make
csu33014_annual_partA: CMakeFiles/csu33014_annual_partA.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir="/home/foesa/Documents/3rd_Year_Stuff/CS3014 Concurrent Systems/assignments/FinalExam/csu33014-annual-partA(3)/csu33014-annual-partA/cmake-build-debug/CMakeFiles" --progress-num=$(CMAKE_PROGRESS_3) "Linking C executable csu33014_annual_partA"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/csu33014_annual_partA.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/csu33014_annual_partA.dir/build: csu33014_annual_partA

.PHONY : CMakeFiles/csu33014_annual_partA.dir/build

CMakeFiles/csu33014_annual_partA.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/csu33014_annual_partA.dir/cmake_clean.cmake
.PHONY : CMakeFiles/csu33014_annual_partA.dir/clean

CMakeFiles/csu33014_annual_partA.dir/depend:
	cd "/home/foesa/Documents/3rd_Year_Stuff/CS3014 Concurrent Systems/assignments/FinalExam/csu33014-annual-partA(3)/csu33014-annual-partA/cmake-build-debug" && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" "/home/foesa/Documents/3rd_Year_Stuff/CS3014 Concurrent Systems/assignments/FinalExam/csu33014-annual-partA(3)/csu33014-annual-partA" "/home/foesa/Documents/3rd_Year_Stuff/CS3014 Concurrent Systems/assignments/FinalExam/csu33014-annual-partA(3)/csu33014-annual-partA" "/home/foesa/Documents/3rd_Year_Stuff/CS3014 Concurrent Systems/assignments/FinalExam/csu33014-annual-partA(3)/csu33014-annual-partA/cmake-build-debug" "/home/foesa/Documents/3rd_Year_Stuff/CS3014 Concurrent Systems/assignments/FinalExam/csu33014-annual-partA(3)/csu33014-annual-partA/cmake-build-debug" "/home/foesa/Documents/3rd_Year_Stuff/CS3014 Concurrent Systems/assignments/FinalExam/csu33014-annual-partA(3)/csu33014-annual-partA/cmake-build-debug/CMakeFiles/csu33014_annual_partA.dir/DependInfo.cmake" --color=$(COLOR)
.PHONY : CMakeFiles/csu33014_annual_partA.dir/depend
