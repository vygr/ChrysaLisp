#!/bin/bash
#
# MAME ChrysaLisp Integration Setup Script
#
# This script prepares the MAME source tree for building with ChrysaLisp OSD.
# It clones MAME (if needed), copies OSD files, and sets up the build configuration.
#

set -e  # Exit on error

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MAME_APP_DIR="$(dirname "$SCRIPT_DIR")"
CHRYSALISP_ROOT="$(cd "$MAME_APP_DIR/../../.." && pwd)"
MAME_SRC_DIR="$MAME_APP_DIR/mame-src"
MAME_REPO="https://github.com/mamedev/mame.git"
MAME_VERSION="mame0261"  # Stable version, adjust as needed

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

echo_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

echo_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check prerequisites
check_prerequisites() {
    echo_info "Checking prerequisites..."

    local missing=""

    if ! command -v git &> /dev/null; then
        missing="$missing git"
    fi

    if ! command -v make &> /dev/null; then
        missing="$missing make"
    fi

    if ! command -v g++ &> /dev/null && ! command -v clang++ &> /dev/null; then
        missing="$missing g++/clang++"
    fi

    if ! command -v python3 &> /dev/null; then
        missing="$missing python3"
    fi

    if [ -n "$missing" ]; then
        echo_error "Missing required tools:$missing"
        echo_error "Please install them and try again."
        exit 1
    fi

    # Check for SDL2
    if ! pkg-config --exists sdl2 2>/dev/null; then
        echo_warn "SDL2 development libraries not found via pkg-config"
        echo_warn "You may need to install: libsdl2-dev (Debian/Ubuntu) or SDL2-devel (Fedora)"
    fi

    echo_info "Prerequisites check passed"
}

# Clone MAME source
clone_mame() {
    if [ -d "$MAME_SRC_DIR" ]; then
        echo_info "MAME source directory already exists: $MAME_SRC_DIR"
        read -p "Remove and re-clone? (y/N) " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            rm -rf "$MAME_SRC_DIR"
        else
            echo_info "Using existing MAME source"
            return 0
        fi
    fi

    echo_info "Cloning MAME repository (this may take several minutes)..."
    echo_info "Repository: $MAME_REPO"
    echo_info "Version: $MAME_VERSION"

    # Clone with shallow history for faster download
    git clone --depth 1 --branch "$MAME_VERSION" "$MAME_REPO" "$MAME_SRC_DIR"

    if [ $? -ne 0 ]; then
        echo_error "Failed to clone MAME repository"
        exit 1
    fi

    echo_info "MAME source cloned successfully"
}

# Copy OSD files
setup_osd_files() {
    echo_info "Setting up ChrysaLisp OSD files..."

    local osd_dest="$MAME_SRC_DIR/src/osd/chrysalisp"

    # Create OSD directory
    mkdir -p "$osd_dest"

    # Copy OSD implementation files
    cp "$MAME_APP_DIR/src/osd/chrysalisp/"*.cpp "$osd_dest/"

    if [ $? -ne 0 ]; then
        echo_error "Failed to copy OSD files"
        exit 1
    fi

    echo_info "OSD files copied to: $osd_dest"

    # Create symlink to adapter layer (avoid duplication)
    ln -sf "$MAME_APP_DIR/src/adapters" "$osd_dest/adapters"
    ln -sf "$MAME_APP_DIR/include" "$osd_dest/include"

    echo_info "Created symlinks to adapter layer and headers"
}

# Setup build configuration
setup_build_config() {
    echo_info "Setting up build configuration..."

    local target_dir="$MAME_SRC_DIR/scripts/target/chrysalisp"

    # Create target directory
    mkdir -p "$target_dir"

    # Copy build configuration
    cp "$SCRIPT_DIR/chrysalisp.lua" "$target_dir/"

    if [ $? -ne 0 ]; then
        echo_error "Failed to copy build configuration"
        exit 1
    fi

    echo_info "Build configuration installed to: $target_dir"
}

# Create environment setup script
create_env_script() {
    echo_info "Creating environment setup script..."

    cat > "$MAME_APP_DIR/env_mame.sh" <<'EOF'
#!/bin/bash
# Source this file to set up MAME build environment
# Usage: source apps/mame/env_mame.sh

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export CHRYSALISP_HOME="$(cd "$SCRIPT_DIR/../.." && pwd)"
export MAME_SRC="$SCRIPT_DIR/mame-src"

echo "ChrysaLisp MAME Build Environment"
echo "  CHRYSALISP_HOME = $CHRYSALISP_HOME"
echo "  MAME_SRC = $MAME_SRC"
echo ""
echo "To build minimal MAME:"
echo "  cd \$MAME_SRC"
echo "  make SUBTARGET=chrysalisp MINIMAL=1 NOTHREADS=1 -j\$(nproc)"
EOF

    chmod +x "$MAME_APP_DIR/env_mame.sh"

    echo_info "Environment script created: $MAME_APP_DIR/env_mame.sh"
}

# Generate makefile wrapper
create_build_script() {
    echo_info "Creating build script..."

    cat > "$MAME_APP_DIR/build_mame.sh" <<'EOF'
#!/bin/bash
# Build MAME with ChrysaLisp OSD
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/env_mame.sh"

cd "$MAME_SRC"

# Default build options
MINIMAL=${MINIMAL:-1}
NOTHREADS=${NOTHREADS:-1}
JOBS=${JOBS:-$(nproc)}

echo "Building MAME for ChrysaLisp..."
echo "  MINIMAL=$MINIMAL"
echo "  NOTHREADS=$NOTHREADS"
echo "  JOBS=$JOBS"
echo ""

make SUBTARGET=chrysalisp \
     MINIMAL=$MINIMAL \
     NOTHREADS=$NOTHREADS \
     -j$JOBS \
     "$@"

if [ $? -eq 0 ]; then
    echo ""
    echo "Build successful!"
    echo "Binary: $MAME_SRC/mame_chrysalisp"
else
    echo ""
    echo "Build failed. Check errors above."
    exit 1
fi
EOF

    chmod +x "$MAME_APP_DIR/build_mame.sh"

    echo_info "Build script created: $MAME_APP_DIR/build_mame.sh"
}

# Main setup process
main() {
    echo_info "===== MAME ChrysaLisp Integration Setup ====="
    echo_info "ChrysaLisp root: $CHRYSALISP_ROOT"
    echo_info "MAME app directory: $MAME_APP_DIR"
    echo ""

    check_prerequisites
    clone_mame
    setup_osd_files
    setup_build_config
    create_env_script
    create_build_script

    echo ""
    echo_info "===== Setup Complete ====="
    echo_info ""
    echo_info "Next steps:"
    echo_info "  1. Source the environment: source apps/mame/env_mame.sh"
    echo_info "  2. Build MAME: apps/mame/build_mame.sh"
    echo_info "  3. Or build manually:"
    echo_info "       cd apps/mame/mame-src"
    echo_info "       make SUBTARGET=chrysalisp MINIMAL=1 NOTHREADS=1 -j\$(nproc)"
    echo_info ""
    echo_info "For more information, see: apps/mame/docs/BUILD.md"
}

main "$@"
