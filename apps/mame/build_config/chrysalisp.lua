--
-- MAME Build Configuration for ChrysaLisp Target
--
-- This file configures MAME to build for ChrysaLisp as the target platform.
-- It defines compiler flags, OSD source files, and library dependencies.
--
-- Installation:
--   Copy this file to: <mame-source>/scripts/target/chrysalisp/chrysalisp.lua
--
-- Usage:
--   make SUBTARGET=chrysalisp SOURCES=src/mame/drivers/pacman.cpp
--

-- Verify we're running in GENie build environment
if not _OPTIONS["SUBTARGET"] then
	_OPTIONS["SUBTARGET"] = "chrysalisp"
end

-- Target name
MAME_TARGET = "chrysalisp"

-- ChrysaLisp installation path
-- This should be set via environment variable: CHRYSALISP_HOME
local chrysalisp_home = os.getenv("CHRYSALISP_HOME") or "../../../.."

--
-- Platform Configuration
--
newoption {
	trigger = "chrysalisp-home",
	description = "Path to ChrysaLisp installation",
}

if _OPTIONS["chrysalisp-home"] then
	chrysalisp_home = _OPTIONS["chrysalisp-home"]
end

--
-- OSD Module Configuration
--
function osdmodulesbuild()

	-- Core OSD files
	files {
		MAME_DIR .. "src/osd/chrysalisp/chrysalispmain.cpp",
		MAME_DIR .. "src/osd/chrysalisp/chrysalispfile.cpp",
		MAME_DIR .. "src/osd/chrysalisp/chrysalispvideo.cpp",
		MAME_DIR .. "src/osd/chrysalisp/chrysalispaudio.cpp",
		MAME_DIR .. "src/osd/chrysalisp/chrysalispinput.cpp",
	}

	-- PII Adapter layer
	files {
		MAME_DIR .. "src/osd/chrysalisp/../../../apps/mame/src/adapters/**.cpp",
	}
end

--
-- OSD Module Targets
--
function osdmodulestarget()

	-- Include paths
	includedirs {
		MAME_DIR .. "src/osd/chrysalisp",
		MAME_DIR .. "apps/mame/include",
		chrysalisp_home .. "/src",
	}

	-- Library paths
	libdirs {
		chrysalisp_home .. "/lib",
	}

	-- Link libraries
	links {
		"SDL2",
		"pthread",
		"m",
		"dl",
	}

	-- Build flags
	buildoptions {
		"-std=c++14",
		"-fno-rtti",
		"-fno-exceptions",
	}
end

--
-- Executable Configuration
--
function createProjects_mame_chrysalisp(_target, _subtarget)

	project ("mame_" .. _subtarget)
	targetsubdir(_target .. "_" .. _subtarget)
	kind "ConsoleApp"
	uuid (os.uuid("mame_" .. _target .. "_" .. _subtarget))

	configuration { "Release" }
		targetsuffix ""
		optimize "Speed"

	configuration { "Debug" }
		targetsuffix "d"
		optimize "Off"
		symbols "On"

	configuration { }

	-- Output name
	targetname ("mame_chrysalisp")

	-- Include OSD modules
	osdmodulesbuild()

	-- MAME core files (these come from MAME's main build scripts)
	dofile(MAME_DIR .. "scripts/src/main.lua")
	dofile(MAME_DIR .. "scripts/src/osd/modules.lua")

	-- OSD-specific settings
	osdmodulestarget()

	-- Compiler flags for ChrysaLisp compatibility
	buildoptions {
		"-DSDL_MAIN_HANDLED",       -- We handle main() ourselves
		"-DPTR64=" .. tostring(_OPTIONS["PTR64"] or "1"),
	}

	-- Linker flags
	linkoptions {
		"-Wl,--export-dynamic",     -- Export symbols for PII
	}

	-- Platform-specific settings
	configuration { "linux" }
		links {
			"util",
			"rt",
		}

	configuration { "macosx" }
		links {
			"CoreFoundation.framework",
			"CoreAudio.framework",
			"AudioUnit.framework",
		}

	configuration { }
end

--
-- Custom Build Actions
--
newaction {
	trigger = "chrysalisp-prepare",
	description = "Prepare ChrysaLisp OSD files",
	execute = function()
		print("Copying ChrysaLisp OSD files...")

		-- Create OSD directory structure
		os.mkdir("src/osd/chrysalisp")

		-- This would copy files from apps/mame/src/osd/chrysalisp
		-- In practice, we'll use symlinks or copy manually
		print("Done. Please ensure apps/mame OSD files are accessible.")
	end
}

--
-- Build Profiles
--
-- Minimal build (single driver for testing)
if _OPTIONS["MINIMAL"] then
	SOURCES = "src/mame/drivers/pacman.cpp"
end

-- No threading (Phase 1 compatibility)
if _OPTIONS["NOTHREADS"] then
	defines {
		"NO_USE_MIDI",
		"NO_USE_PORTMIDI",
		"USE_NETWORK=0",
	}

	-- Remove threading requirements
	removelinks {
		"pthread",
	}
end

--
-- Driver Lists
--
-- For minimal builds, we can specify a single driver
if _OPTIONS["SOURCES"] then
	SOURCES = _OPTIONS["SOURCES"]
else
	-- Default to a small set of classic arcade drivers
	SOURCES = {
		"src/mame/drivers/pacman.cpp",
		"src/mame/drivers/galaxian.cpp",
		"src/mame/drivers/donkeykong.cpp",
		"src/mame/drivers/mario.cpp",
	}
end

--
-- Additional Options
--
newoption {
	trigger = "MINIMAL",
	description = "Build minimal MAME with single driver (Pac-Man)",
}

newoption {
	trigger = "NOTHREADS",
	description = "Build without threading support (Phase 1)",
}

newoption {
	trigger = "SOURCES",
	description = "Specify driver source files to include",
}

--
-- Integration Notes
--
-- To build MAME with ChrysaLisp OSD:
--
-- 1. Set CHRYSALISP_HOME environment variable:
--    export CHRYSALISP_HOME=/path/to/ChrysaLisp
--
-- 2. Copy OSD files:
--    cp -r apps/mame/src/osd/chrysalisp mame-src/src/osd/
--
-- 3. Build minimal MAME:
--    cd mame-src
--    make SUBTARGET=chrysalisp MINIMAL=1 NOTHREADS=1
--
-- 4. Or build with specific drivers:
--    make SUBTARGET=chrysalisp SOURCES=src/mame/drivers/pacman.cpp NOTHREADS=1
--
