# ChrysaLisp for iOS and Android: the GUI host program, src/host, on SDL3,
# run in the VP64 emulator. See mobile/README.md.
#
# iOS:     mobile/ios/build.sh
# Android: mobile/android, a Gradle project that builds this with the NDK

cmake_minimum_required(VERSION 3.22)
project(ChrysaLisp LANGUAGES C CXX)

set(CL_ROOT ${CMAKE_CURRENT_SOURCE_DIR}/..)
set(CL_HOST ${CL_ROOT}/src/host)
set(SDL_DIR ${CMAKE_CURRENT_SOURCE_DIR}/deps/SDL CACHE PATH "SDL3 source, mobile/fetch_sdl.sh")
set(CL_BUNDLE ${CMAKE_CURRENT_SOURCE_DIR}/build/assets/chrysalisp.tar CACHE FILEPATH "mobile/bundle.sh makes it")
set(CL_BUNDLE_ID "org.chrysalisp.app" CACHE STRING "the app's id, iOS")
set(CL_TEAM "" CACHE STRING "the Apple development team that signs it, for a device")

if(NOT EXISTS ${SDL_DIR}/CMakeLists.txt)
	message(FATAL_ERROR "No SDL3 source in ${SDL_DIR}, run mobile/fetch_sdl.sh")
endif()

# SDL3 itself. Android's SDLActivity loads libSDL3.so then libmain.so, iOS
# links it in
set(SDL_TEST_LIBRARY OFF CACHE BOOL "" FORCE)
set(SDL_EXAMPLES OFF CACHE BOOL "" FORCE)
set(SDL_TESTS OFF CACHE BOOL "" FORCE)
if(ANDROID)
	set(SDL_SHARED ON CACHE BOOL "" FORCE)
	set(SDL_STATIC OFF CACHE BOOL "" FORCE)
else()
	set(SDL_SHARED OFF CACHE BOOL "" FORCE)
	set(SDL_STATIC ON CACHE BOOL "" FORCE)
endif()
add_subdirectory(${SDL_DIR} SDL EXCLUDE_FROM_ALL)

set(HOST_SOURCES
	${CL_HOST}/main.cpp
	${CL_HOST}/vp64.cpp
	${CL_HOST}/net.cpp
	${CL_HOST}/gui_sdl3.cpp
	${CL_HOST}/audio_sdl3.cpp
	${CL_HOST}/video.cpp
	src/mobile_main.cpp)
if(ANDROID)
	list(APPEND HOST_SOURCES ${CL_HOST}/pii_linux.cpp src/android_shm.cpp)
	# bionic has no shm_open, android_shm.h says it is there
	set_source_files_properties(${CL_HOST}/pii_linux.cpp PROPERTIES
		COMPILE_OPTIONS "-include;${CMAKE_CURRENT_SOURCE_DIR}/src/android_shm.h")
else()
	list(APPEND HOST_SOURCES ${CL_HOST}/pii_darwin.cpp)
endif()

# the host program's main is called by mobile_main.cpp, SDL has the real one
set_source_files_properties(${CL_HOST}/main.cpp PROPERTIES COMPILE_DEFINITIONS "main=host_main")

if(ANDROID)
	add_library(main SHARED ${HOST_SOURCES})
else()
	add_executable(main MACOSX_BUNDLE ${HOST_SOURCES} ${CL_BUNDLE})
endif()

set_target_properties(main PROPERTIES CXX_STANDARD 14 CXX_STANDARD_REQUIRED ON)
target_include_directories(main PRIVATE ${CL_HOST})
# the sdl3 GUI and AUDIO drivers, as make GUI=sdl3 has them
target_compile_definitions(main PRIVATE _HOST_GUI=3 _HOST_AUDIO=1)
target_compile_options(main PRIVATE -O3 -fno-exceptions -Wno-unused-result)
target_link_libraries(main PRIVATE SDL3::SDL3)

if(ANDROID)
	target_link_libraries(main PRIVATE log android)
else()
	# the tar of the system's files goes in the app, at its top
	set_source_files_properties(${CL_BUNDLE} PROPERTIES MACOSX_PACKAGE_LOCATION Resources)
	target_link_libraries(main PRIVATE
		"-framework VideoToolbox" "-framework CoreMedia" "-framework CoreVideo" "-framework CoreFoundation")
	set_target_properties(main PROPERTIES
		OUTPUT_NAME ChrysaLisp
		MACOSX_BUNDLE_INFO_PLIST ${CMAKE_CURRENT_SOURCE_DIR}/ios/Info.plist.in
		MACOSX_BUNDLE_GUI_IDENTIFIER ${CL_BUNDLE_ID}
		MACOSX_BUNDLE_BUNDLE_NAME ChrysaLisp
		MACOSX_BUNDLE_SHORT_VERSION_STRING "1.0"
		MACOSX_BUNDLE_BUNDLE_VERSION "1"
		XCODE_ATTRIBUTE_PRODUCT_BUNDLE_IDENTIFIER ${CL_BUNDLE_ID}
		XCODE_ATTRIBUTE_TARGETED_DEVICE_FAMILY "1,2"
		XCODE_ATTRIBUTE_IPHONEOS_DEPLOYMENT_TARGET "15.0"
		XCODE_ATTRIBUTE_DEVELOPMENT_TEAM "${CL_TEAM}"
		XCODE_ATTRIBUTE_CODE_SIGN_STYLE Automatic)
endif()
