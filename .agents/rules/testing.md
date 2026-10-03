# ChrysaLisp Testing Rules

*	**Running Tests:** Run the suite with `echo "tests" | ./run_tui.sh -f`. It prints only failures and a summary, so needs no `grep`. `tests -m str` runs the matching modules, `tests -l` lists them. Never use `tests -v` for the whole suite, it prints over 2,000 lines.
*	**Time Limits:** A build is under a second and the suite a couple of seconds, so guard runs with a limit of seconds, not minutes. Hitting it means a hang or crash to investigate.
*	**No GUI in Headless/TUI:** Never run or instantiate GUI objects (`View`, `Window`, `Vdu`, `Flow`, etc.) in standalone test scripts or TUI boot images (`./run_tui.sh`).
*	**REPL Snippets:** To try raw ChrysaLisp code use `echo "lisp -r (print (* 123 456))" | ./run_tui.sh -n 1 -f` (TUI only), or `./run.sh -n 1 -f` when GUI classes are needed. Use `{}` for strings. A View tree can be dumped for inspection with `(ui-save stream view)`. Start with one simple expression and build up.
*	**Scratch Files:** Never place test scratch files in the workspace root; keep temporary files in `tests/scratch/` or use `tmp_*.txt` in `tests/` and clean them up after testing.
