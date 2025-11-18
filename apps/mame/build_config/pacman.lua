-- license:BSD-3-Clause
-- copyright-holders:MAMEdev Team

---------------------------------------------------------------------------
--
--   pacman.lua
--
--   Ultra-minimal MAME configuration for Pac-Man only
--   Use: make SUBTARGET=chrysalisp SOURCES=pacman
--
---------------------------------------------------------------------------

-- Specify only the CPU cores needed for Pac-Man
CPUS["Z80"] = true

-- Specify only the sound cores needed for Pac-Man
SOUNDS["NAMCO"] = true  -- Pac-Man uses Namco WSG sound
SOUNDS["DAC"] = true    -- Digital-to-analog converter

-- Video cores
-- Pac-Man uses custom video hardware, no standard chips

-- Machine cores needed for Pac-Man
MACHINES["GEN_LATCH"] = true
MACHINES["WATCHDOG"] = true

-- Create minimal project for Pac-Man
function createProjects_mame_pacman(_target, _subtarget)
	project ("mame_pacman")
	targetsubdir(_target .."_" .. _subtarget)
	kind (LIBTYPE)
	uuid (os.uuid("drv-mame-pacman"))
	addprojectflags()
	precompiledheaders_novs()

	includedirs {
		MAME_DIR .. "src/osd",
		MAME_DIR .. "src/emu",
		MAME_DIR .. "src/devices",
		MAME_DIR .. "src/mame/shared",
		MAME_DIR .. "src/lib",
		MAME_DIR .. "src/lib/util",
		MAME_DIR .. "3rdparty",
		GEN_DIR  .. "mame/layout",
	}

	-- Pac-Man driver files
	files{
		MAME_DIR .. "src/mame/namco/pacman.cpp",
		MAME_DIR .. "src/mame/namco/pacman.h",
	}
end

function linkProjects_mame_pacman(_target, _subtarget)
	links {
		"mame_pacman",
	}
end
