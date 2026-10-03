# ChrysaLisp Testing Rules

*	**Token Conservation:** ALWAYS pipe automated test suite runs through `grep` (e.g. `grep "\[FAIL\]"` or `grep -E "\[FAIL\]|\[SKIP\]|Passed:|Failed:|RESULT"`) when invoking tests via shell commands to prevent ~1,600 verbose `[PASS]` lines from exhausting LLM context tokens.
*	**No GUI in Headless/TUI:** Never run or instantiate GUI objects (`View`, `Window`, `Vdu`, `Flow`, etc.) in standalone test scripts or TUI boot images (`./run_tui.sh`).
*	**REPL Snippets:** To try raw ChrysaLisp code use `echo "lisp -r (print (* 123 456))" | ./run_tui.sh -n 1 -f` (TUI only), or `./run.sh -n 1 -f` when GUI classes are needed. Use `{}` for strings. A View tree can be dumped for inspection with `(ui-save stream view)`. Start with one simple expression and build up.
*	**Scratch Files:** Never place test scratch files in the workspace root; keep temporary files in `tests/scratch/` or use `tmp_*.txt` in `tests/` and clean them up after testing.
