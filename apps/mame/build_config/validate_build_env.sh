#!/bin/bash
#
# MAME Build Environment Validation Script
#
# This script checks that all prerequisites for building MAME are met.
# Run this before attempting to build MAME to catch issues early.
#

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

ERRORS=0
WARNINGS=0

echo -e "${BLUE}=== MAME Build Environment Validation ===${NC}\n"

# Check function - exits 0 if OK, 1 if error, 2 if warning
check() {
    local name="$1"
    local command="$2"
    local required="$3"
    local min_version="$4"

    printf "Checking %-30s" "$name..."

    if command -v $command &> /dev/null; then
        version=$($command --version 2>&1 | head -1)
        echo -e "${GREEN}✓${NC} Found: $version"
        return 0
    else
        if [ "$required" = "yes" ]; then
            echo -e "${RED}✗ MISSING (REQUIRED)${NC}"
            ERRORS=$((ERRORS + 1))
            return 1
        else
            echo -e "${YELLOW}⚠ Not found (optional)${NC}"
            WARNINGS=$((WARNINGS + 1))
            return 2
        fi
    fi
}

check_pkg() {
    local name="$1"
    local pkg="$2"
    local required="$3"

    printf "Checking %-30s" "$name..."

    if pkg-config --exists $pkg 2>/dev/null; then
        version=$(pkg-config --modversion $pkg)
        echo -e "${GREEN}✓${NC} Found: $version"
        return 0
    else
        if [ "$required" = "yes" ]; then
            echo -e "${RED}✗ MISSING (REQUIRED)${NC}"
            ERRORS=$((ERRORS + 1))
            return 1
        else
            echo -e "${YELLOW}⚠ Not found (optional)${NC}"
            WARNINGS=$((WARNINGS + 1))
            return 2
        fi
    fi
}

echo "=== Required Tools ==="
check "Git" "git" "yes"
check "Make" "make" "yes"
check "Python 3" "python3" "yes"
check "G++" "g++" "yes"
check "GCC" "gcc" "yes"

echo ""
echo "=== Compiler Version Check ==="
if command -v g++ &> /dev/null; then
    GCC_VERSION=$(g++ -dumpversion)
    GCC_MAJOR=$(echo $GCC_VERSION | cut -d. -f1)

    printf "G++ version: $GCC_VERSION ... "
    if [ "$GCC_MAJOR" -ge 5 ]; then
        echo -e "${GREEN}✓${NC} (>= 5.0 required)"
    else
        echo -e "${RED}✗${NC} Too old (>= 5.0 required)"
        ERRORS=$((ERRORS + 1))
    fi
fi

echo ""
echo "=== Required Libraries ==="
check_pkg "SDL2" "sdl2" "yes"
check_pkg "pthread" "pthread-stubs" "no"  # Usually built-in

echo ""
echo "=== Optional but Recommended ==="
check "pkg-config" "pkg-config" "no"
check_pkg "Fontconfig" "fontconfig" "no"
check_pkg "ALSA" "alsa" "no"

echo ""
echo "=== Build Tools ==="
check "ccache" "ccache" "no"
check "ninja" "ninja" "no"

echo ""
echo "=== System Resources ==="

# Check disk space
printf "Checking %-30s" "Disk space..."
AVAILABLE=$(df -h . | awk 'NR==2 {print $4}')
AVAILABLE_GB=$(df -BG . | awk 'NR==2 {print $4}' | sed 's/G//')
if [ "$AVAILABLE_GB" -ge 10 ]; then
    echo -e "${GREEN}✓${NC} $AVAILABLE available (10GB+ recommended)"
else
    echo -e "${YELLOW}⚠${NC} Only $AVAILABLE available (10GB+ recommended)"
    WARNINGS=$((WARNINGS + 1))
fi

# Check RAM
printf "Checking %-30s" "Memory..."
if command -v free &> /dev/null; then
    TOTAL_RAM=$(free -g | awk 'NR==2 {print $2}')
    if [ "$TOTAL_RAM" -ge 4 ]; then
        echo -e "${GREEN}✓${NC} ${TOTAL_RAM}GB total (4GB+ recommended)"
    else
        echo -e "${YELLOW}⚠${NC} Only ${TOTAL_RAM}GB total (4GB+ recommended)"
        WARNINGS=$((WARNINGS + 1))
    fi
else
    echo -e "${YELLOW}⚠${NC} Cannot determine (4GB+ recommended)"
fi

# Check CPU cores
printf "Checking %-30s" "CPU cores..."
if command -v nproc &> /dev/null; then
    CORES=$(nproc)
    echo -e "${GREEN}✓${NC} $CORES cores (will use -j$CORES for parallel build)"
else
    echo -e "${YELLOW}⚠${NC} Cannot determine"
fi

echo ""
echo "=== MAME Source Status ==="
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MAME_APP_DIR="$(dirname "$SCRIPT_DIR")"
MAME_SRC_DIR="$MAME_APP_DIR/mame-src"

printf "Checking %-30s" "MAME source..."
if [ -d "$MAME_SRC_DIR" ]; then
    if [ -f "$MAME_SRC_DIR/makefile" ]; then
        echo -e "${GREEN}✓${NC} Found at $MAME_SRC_DIR"
    else
        echo -e "${YELLOW}⚠${NC} Directory exists but incomplete"
        WARNINGS=$((WARNINGS + 1))
    fi
else
    echo -e "${YELLOW}⚠${NC} Not found (run setup_mame.sh first)"
    WARNINGS=$((WARNINGS + 1))
fi

printf "Checking %-30s" "OSD files..."
if [ -d "$MAME_SRC_DIR/src/osd/chrysalisp" ]; then
    FILE_COUNT=$(ls -1 "$MAME_SRC_DIR/src/osd/chrysalisp"/*.cpp 2>/dev/null | wc -l)
    if [ "$FILE_COUNT" -ge 5 ]; then
        echo -e "${GREEN}✓${NC} $FILE_COUNT files found"
    else
        echo -e "${YELLOW}⚠${NC} Only $FILE_COUNT files (expected 5+)"
        WARNINGS=$((WARNINGS + 1))
    fi
else
    echo -e "${RED}✗${NC} Not found (run setup_mame.sh)"
    ERRORS=$((ERRORS + 1))
fi

printf "Checking %-30s" "Build config..."
if [ -f "$MAME_SRC_DIR/scripts/src/osd/chrysalisp.lua" ]; then
    echo -e "${GREEN}✓${NC} Found"
else
    echo -e "${RED}✗${NC} Missing chrysalisp.lua"
    ERRORS=$((ERRORS + 1))
fi

echo ""
echo "=== ChrysaLisp Environment ==="

if [ -n "$CHRYSALISP_HOME" ]; then
    printf "CHRYSALISP_HOME: "
    echo -e "${GREEN}✓${NC} $CHRYSALISP_HOME"
else
    printf "CHRYSALISP_HOME: "
    echo -e "${YELLOW}⚠${NC} Not set (will use default)"
fi

echo ""
echo "=== Summary ==="
echo -e "Errors:   ${RED}$ERRORS${NC}"
echo -e "Warnings: ${YELLOW}$WARNINGS${NC}"

if [ $ERRORS -eq 0 ]; then
    echo ""
    echo -e "${GREEN}✓ Build environment is ready!${NC}"
    echo ""
    echo "You can now build MAME with:"
    echo "  cd $MAME_SRC_DIR"
    echo "  make SUBTARGET=chrysalisp MINIMAL=1 NOTHREADS=1 -j\$(nproc)"
    exit 0
else
    echo ""
    echo -e "${RED}✗ Build environment has errors. Please fix them first.${NC}"
    echo ""
    echo "Common fixes:"
    echo "  - Install missing packages: sudo apt-get install <package>"
    echo "  - Run setup script: ./build_config/setup_mame.sh"
    echo "  - Update compiler: sudo apt-get install g++-9"
    exit 1
fi
