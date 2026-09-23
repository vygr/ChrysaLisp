# ChrysaLisp Testing Rules

*	**Token Conservation:** ALWAYS pipe automated test suite runs through `grep` (e.g. `grep "\[FAIL\]"` or `grep -E "\[FAIL\]|\[SKIP\]|Passed:|Failed:|RESULT"`) when invoking tests via shell commands to prevent ~1,600 verbose `[PASS]` lines from exhausting LLM context tokens.
*	**No GUI in Headless/TUI:** Never run or instantiate GUI objects (`View`, `Window`, `Vdu`, `Flow`, etc.) in standalone test scripts or TUI boot images (`./run_tui.sh`).
*	**Scratch Files:** Never place test scratch files in the workspace root; keep temporary files in `tests/scratch/` or use `tmp_*.txt` in `tests/` and clean them up after testing.
