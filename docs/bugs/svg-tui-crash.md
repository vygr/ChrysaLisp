# Bug Report: SVG Text Rendering Crashes in TUI Mode

## Summary

The `SVG-Canvas` function in `lib/xml/svg.inc` crashes when rendering SVG files
containing `<text>` elements in TUI (non-GUI) mode because `create-font` is not
bound. Additionally, missing imports caused other constants to be unbound.

## Affected Version

Commit: 7f0ebd320dff9516fe80d0da121b8f82f5beea46 (and earlier)

## Symptoms

When running any command that loads an SVG with text elements via TUI mode
(e.g., `./run_tui.sh`), the system crashes with an unbound symbol error for
`create-font`.

## Root Cause

Two issues were identified:

1. **Missing Imports**: The `lib/xml/svg.inc` file was missing explicit imports
   for `lib/consts/colors.inc` and `gui/path/lisp.inc`. These constants were
   transitively available in GUI mode but not in TUI mode.

2. **Unguarded Font Usage**: In lines 269-290, the text rendering lambda
   unconditionally calls `create-font`, which is only available when the GUI
   subsystem is fully loaded.

## Reproduction

```bash
./run_tui.sh -n 1 -f -s tests/svg_tui_crash_repro.lisp
```

See `tests/svg_tui_crash_repro.lisp` for minimal reproduction script.

## Fix

Two changes to `lib/xml/svg.inc`:

### 1. Add missing imports (after line 5):

```lisp
(import "lib/consts/colors.inc")
(import "gui/path/lisp.inc")
```

### 2. Guard text rendering (lines ~269-290):

```lisp
(lambda (text)
    ;render text (skip in TUI mode where fonts unavailable)
    (when (def? 'create-font)
        ;; existing text rendering code here
        ...))
```

## Impact

- Low severity for GUI users (not affected)
- Was blocking for TUI/headless usage of SVG parsing
- After fix: SVG text elements are gracefully skipped in TUI mode

## Testing

After applying the fix:
```
$ ./run_tui.sh -n 1 -f -s tests/svg_tui_crash_repro.lisp
Attempting to parse SVG with text element...
Success! Canvas: @gui/canvas/vtable
Test complete. Shutting down...
```
