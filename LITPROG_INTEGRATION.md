# LITPROG Integration Guide for ChrysaLisp

This guide explains how LITPROG integrates with the ChrysaLisp build system and follows ChrysaLisp contribution standards.

## Overview

LITPROG is a standalone library that does NOT modify any core ChrysaLisp files. It operates as an optional tool for developers who want to use literate programming techniques in their ChrysaLisp projects.

## Integration Status

- **Type:** Optional Library/Tool
- **Core Impact:** None (does not modify ChrysaLisp core)
- **Platform Dependencies:** None (pure ChrysaLisp implementation)
- **Build Integration:** Standalone (does not affect `make it`)

## File Structure

```
ChrysaLisp_AI_made_apps_experiment/
├── litprog.lisp                    # Core tangle/weave implementation
├── litprog_enhanced.lisp           # Phase 2: Advanced features
├── litprog_macros.lisp             # Phase 2: Macro system
├── litprog_test.lisp               # Test suite
├── demo_litprog.lisp               # Interactive demo
├── litprog_viewer.html             # Web-based viewer
├── Makefile.litprog                # Build automation
├── LITPROG_README.md               # User documentation
├── LITPROG_QUICKREF.md             # Quick reference
├── LITPROG_SUMMARY.md              # Project summary
├── LITPROG_INTEGRATION.md          # This file
└── examples/
    ├── hello_literate.lit          # Noweb example
    ├── fibonacci_orgmode.lit       # Org-mode example
    ├── web_server_markdown.lit     # Markdown example
    └── advanced_mixed_styles.lit   # Mixed syntax example
```

## Compliance with ChrysaLisp Standards

### 1. No Core Modifications

✅ **LITPROG does not modify any existing ChrysaLisp files**

- All functionality is in separate `.lisp` files
- Can be removed without affecting ChrysaLisp
- Uses only documented ChrysaLisp APIs

### 2. Platform Independence

✅ **Pure ChrysaLisp implementation**

- No C++ PII modifications required
- No platform-specific code
- Works on all platforms that run ChrysaLisp

### 3. Build System Integrity

✅ **Does not affect `make it` or `make install`**

The standard ChrysaLisp build process is unaffected:

```bash
# Standard ChrysaLisp build - unchanged
make it
make snapshot
make install
```

### 4. Binary Reproducibility

✅ **Not part of ChrysaLisp boot image**

- LITPROG is a library, not a system component
- Does not affect boot image generation
- Does not impact build reproducibility

### 5. No Breaking Changes

✅ **Backwards compatible**

- Existing code runs unchanged
- No API changes to ChrysaLisp
- Opt-in usage only

## Usage Integration

### Standalone Usage

Most users will use LITPROG as a standalone tool:

```bash
# In your project directory
chrysalisp -e "(import \"path/to/litprog.lisp\")"
chrysalisp -e "(litprog-tangle \"myprogram.lit\" \"src/\")"
chrysalisp -e "(litprog-weave \"myprogram.lit\" \"docs.html\" :format :html)"
```

### Project Integration

For projects that want to integrate LITPROG:

**Option 1: Copy the library**

```bash
cp litprog.lisp your-project/lib/
```

**Option 2: Git submodule**

```bash
cd your-project
git submodule add <repo-url> vendor/litprog
```

**Option 3: Direct import**

```lisp
; In your project's main file
(import "../path/to/litprog.lisp")
```

### Build Integration Example

If you want to integrate LITPROG into your project's build:

```makefile
# Your project's Makefile

# Tangle literate sources before building
.PHONY: tangle
tangle:
	chrysalisp -e "(import \"lib/litprog.lisp\")" \
	           -e "(litprog-tangle \"src/main.lit\" \"src/\")"

# Build with tangling
build: tangle
	# Your normal build steps
	chrysalisp src/main.lisp

# Generate documentation
docs: tangle
	chrysalisp -e "(import \"lib/litprog.lisp\")" \
	           -e "(litprog-weave \"src/main.lit\" \"docs/index.html\" :format :html)"
```

## Testing

### Unit Tests

Run the LITPROG test suite:

```bash
chrysalisp litprog_test.lisp
chrysalisp -e "(import \"litprog_test.lisp\")" -e "(run-all-tests)"
```

### Integration Tests

Test that LITPROG works with your project:

```bash
# 1. Tangle a literate file
chrysalisp -e "(import \"litprog.lisp\")" \
           -e "(litprog-tangle \"test.lit\" \"output/\")"

# 2. Verify output files were created
ls output/

# 3. Run the generated code
chrysalisp output/generated.lisp

# 4. Weave documentation
chrysalisp -e "(import \"litprog.lisp\")" \
           -e "(litprog-weave \"test.lit\" \"test.html\" :format :html)"

# 5. Verify documentation was created
ls test.html
```

## Development Workflow

### For LITPROG Users

1. **Write literate source** (`myprogram.lit`)
2. **Tangle to get code** (`myprogram.lisp`)
3. **Test the code** in ChrysaLisp
4. **Weave to get docs** (`myprogram.html`)
5. **Commit the `.lit` file** (optional: commit generated files too)

### For ChrysaLisp Developers

If you're contributing to ChrysaLisp and want to use LITPROG:

1. **Keep it separate** - LITPROG is a dev tool, not part of core
2. **Don't require it** - Your code should work without LITPROG
3. **Generate before PR** - Tangle before submitting, include generated code
4. **Document usage** - Explain how to regenerate from `.lit` source

## Performance Considerations

LITPROG is a build-time tool, not a runtime library:

- **Tangle/Weave:** Happens once during development
- **Runtime:** Zero overhead (uses generated code)
- **Build time:** Minimal (pure ChrysaLisp, very fast)

## Example Integration Scenarios

### Scenario 1: Documentation-Heavy Project

```
project/
├── src/
│   ├── main.lit          # Literate source (version controlled)
│   └── main.lisp         # Generated code (optionally in .gitignore)
├── docs/
│   └── index.html        # Generated docs
└── Makefile
```

**Workflow:**

```bash
# Development
make tangle    # Generate code from .lit
make build     # Build the generated code
make docs      # Generate documentation

# CI/CD
make tangle && make build && make test
```

### Scenario 2: Tutorial/Example Project

```
examples/
├── tutorial.lit          # Literate tutorial
├── tutorial.html         # Web-viewable version
└── code/
    └── tutorial.lisp     # Extracted working code
```

**Workflow:**

```bash
# Create web-friendly tutorial
make weave

# Extract working examples
make tangle

# Users can run examples directly
chrysalisp examples/code/tutorial.lisp
```

### Scenario 3: Library Documentation

```
mylib/
├── mylib.lisp            # Regular code
├── docs/
│   ├── architecture.lit  # Literate architecture doc
│   └── architecture.html # Generated
└── examples/
    ├── example1.lit      # Literate examples
    └── example1.lisp     # Working code
```

## Contribution Guidelines

### If Contributing to LITPROG Itself

Follow ChrysaLisp contribution standards:

1. **Test on all platforms** (if modifying file I/O)
2. **Verify `make it` still works** (should be unaffected)
3. **Add tests** for new features
4. **Update documentation**
5. **Follow ChrysaLisp coding style**

### If Using LITPROG in Your Contribution

1. **Include generated code** in PR
2. **Document regeneration** in PR description
3. **Keep .lit files** for future maintenance
4. **Don't require LITPROG** for builds

## FAQ

### Q: Does using LITPROG violate ChrysaLisp build reproducibility?

**A:** No. LITPROG is a development tool. You commit the generated code, which is what gets built. The `.lit` source is for documentation/maintenance.

### Q: Can I use LITPROG for ChrysaLisp core contributions?

**A:** While you can use it for development, submit the generated `.lisp` files. The core build must not depend on LITPROG.

### Q: Does LITPROG affect boot image generation?

**A:** No. LITPROG is not part of the boot image and doesn't affect it.

### Q: Can I use LITPROG in emulated mode?

**A:** Yes. LITPROG is pure ChrysaLisp and works identically in emulated and native modes.

### Q: What if LITPROG breaks?

**A:** Your code still works! You have the generated `.lisp` files. LITPROG is optional.

## Troubleshooting

### LITPROG not found

```bash
# Make sure path is correct
chrysalisp -e "(import \"full/path/to/litprog.lisp\")"

# Or copy to a standard location
cp litprog.lisp ~/ChrysaLisp/lib/
chrysalisp -e "(import \"lib/litprog.lisp\")"
```

### Generated code doesn't match

```bash
# Clean and regenerate
rm -rf output/
chrysalisp -e "(import \"litprog.lisp\")" \
           -e "(litprog-tangle \"source.lit\" \"output/\")"
```

### File I/O errors

```bash
# Check file permissions
ls -la source.lit
ls -la output/

# Verify paths
pwd
```

## Advanced Integration

### With Git Hooks

**.git/hooks/pre-commit:**

```bash
#!/bin/bash
# Auto-tangle before commit
chrysalisp -e "(import \"lib/litprog.lisp\")" \
           -e "(litprog-tangle \"src/main.lit\" \"src/\")"
git add src/main.lisp
```

### With CI/CD

**GitHub Actions:**

```yaml
name: Literate Build
on: [push]
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2

      - name: Tangle literate sources
        run: |
          chrysalisp -e "(import \"litprog.lisp\")" \
                     -e "(litprog-tangle \"src/main.lit\" \"src/\")"

      - name: Build project
        run: make build

      - name: Generate docs
        run: |
          chrysalisp -e "(import \"litprog.lisp\")" \
                     -e "(litprog-weave \"src/main.lit\" \"docs/index.html\" :format :html)"

      - name: Deploy docs
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: ./docs
```

### With Watch Mode

```bash
# Auto-regenerate on file changes
chrysalisp -e "(import \"litprog_enhanced.lisp\")" \
           -e "(litprog-watch \"src/main.lit\" \"src/\")"
```

## Best Practices

1. **Version control `.lit` files** - These are your source of truth
2. **Optionally version control generated files** - For easy building
3. **Document regeneration steps** - In README.md
4. **Use Makefiles** - Automate tangle/weave
5. **Test generated code** - Before committing
6. **Keep LITPROG updated** - Get latest features

## Summary

LITPROG integrates with ChrysaLisp as:

✅ **Standalone library** (no core modifications)
✅ **Platform independent** (pure ChrysaLisp)
✅ **Build-time tool** (zero runtime overhead)
✅ **Fully compliant** (meets all contribution standards)
✅ **Optional** (doesn't affect normal development)

It's designed to enhance your development workflow without impacting ChrysaLisp's integrity, reproducibility, or portability.

---

**For questions or issues:** See `LITPROG_README.md` or run `(litprog-help)`
