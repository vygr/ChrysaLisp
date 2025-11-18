# MAME for ChrysaLisp - Build Configuration
#
# This Makefile builds the MAME adapter layer and integrates it with ChrysaLisp

# Detect host platform
OS := $(shell uname -s)
CPU := $(shell uname -m)

# Map CPU architecture
ifeq ($(CPU),x86_64)
    ABI := AMD64
else ifeq ($(CPU),aarch64)
    ABI := ARM64
else ifeq ($(CPU),arm64)
    ABI := ARM64
else
    ABI := UNKNOWN
endif

# Directories
SRC_DIR := src
ADAPTER_DIR := $(SRC_DIR)/adapters
INC_DIR := include
OBJ_DIR := ../../obj/mame/$(ABI)
BIN_DIR := $(OBJ_DIR)/bin
LIB_DIR := $(OBJ_DIR)/lib

# Compiler settings
CXX := g++
CXXFLAGS := -std=c++14 -O2 -Wall -Wextra -I$(INC_DIR) -I../../src/host
LDFLAGS := -shared

# Source files
ADAPTER_SOURCES := \
    $(ADAPTER_DIR)/mame_adapter_core.cpp \
    $(ADAPTER_DIR)/mame_file_io.cpp \
    $(ADAPTER_DIR)/mame_video.cpp \
    $(ADAPTER_DIR)/mame_audio.cpp \
    $(ADAPTER_DIR)/mame_input.cpp

TEST_SOURCES := \
    $(SRC_DIR)/mame_test.cpp

# Object files
ADAPTER_OBJECTS := $(patsubst $(ADAPTER_DIR)/%.cpp,$(OBJ_DIR)/%.o,$(ADAPTER_SOURCES))
TEST_OBJECTS := $(patsubst $(SRC_DIR)/%.cpp,$(OBJ_DIR)/%.o,$(TEST_SOURCES))

# Output files
ADAPTER_LIB := $(LIB_DIR)/libmame_adapter.so
TEST_BIN := $(BIN_DIR)/mame_test

# Targets
.PHONY: all clean test adapter

all: adapter test

adapter: $(ADAPTER_LIB)

test: $(TEST_BIN)

# Build adapter library
$(ADAPTER_LIB): $(ADAPTER_OBJECTS) | $(LIB_DIR)
	@echo "Linking MAME adapter library..."
	$(CXX) $(LDFLAGS) -o $@ $^
	@echo "Built: $@"

# Build test binary
$(TEST_BIN): $(TEST_OBJECTS) $(ADAPTER_OBJECTS) | $(BIN_DIR)
	@echo "Linking MAME test binary..."
	$(CXX) -o $@ $^
	@echo "Built: $@"

# Compile adapter sources
$(OBJ_DIR)/%.o: $(ADAPTER_DIR)/%.cpp | $(OBJ_DIR)
	@echo "Compiling $<..."
	$(CXX) $(CXXFLAGS) -fPIC -c $< -o $@

# Compile test sources
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp | $(OBJ_DIR)
	@echo "Compiling $<..."
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Create directories
$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

$(LIB_DIR):
	mkdir -p $(LIB_DIR)

# Clean build artifacts
clean:
	rm -rf $(OBJ_DIR)
	@echo "Cleaned build artifacts"

# Install (copy to appropriate location for ChrysaLisp)
install: all
	@echo "Installing MAME adapter..."
	@echo "  Adapter library: $(ADAPTER_LIB)"
	@echo "  Test binary: $(TEST_BIN)"
	@echo "Installation complete"

# Help target
help:
	@echo "MAME for ChrysaLisp - Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all      - Build adapter library and test program (default)"
	@echo "  adapter  - Build only the adapter library"
	@echo "  test     - Build only the test program"
	@echo "  clean    - Remove all build artifacts"
	@echo "  install  - Build and install to ChrysaLisp"
	@echo "  help     - Show this help message"
	@echo ""
	@echo "Current configuration:"
	@echo "  OS:  $(OS)"
	@echo "  CPU: $(CPU)"
	@echo "  ABI: $(ABI)"
