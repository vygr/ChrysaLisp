# ChrysaLisp Testing Rules

The `chrysalisp-tests` skill has the detail; these hold for every session.

*	**Running Tests:** `echo "tests" | ./run_tui.sh -n 0 -u Test -a -f` runs what the results cache says is stale, `tests -a` every module, `tests path ...` or `tests -m str` the ones named. It prints only failures, a summary and its `=p=` progress lines, so needs no `grep`. On the machines of a rack, `./rack.sh run 'rack "tests"'`, as the `chrysalisp-dev-cycle` skill says.
*	**Focused Runs:** while working, run the modules the change touches. The whole suite is for kernel, VP, translator and host changes, a tag, or when asked for. One run at a time; stop a run of code that is about to change.
*	**Time Limits:** a build, a module or a cached run takes seconds, so guard it by seconds; a full run a little over its usual time. Hitting a limit is a hang or crash to look into. A limit wrapper kills only the script: kill the nodes the run left, by pid, never `./stop.sh`.
*	**Watch:** a long run's output goes to a file that is watched as it grows, never through `| tail` or `| head` alone.
*	**Tests Pass Anywhere:** a test waits for the event it tests, never on a time limit, and touches only what it made.
*	**Node Counts:** run on a multi node network, `-n 0`, as well as on one node, `-n 1`. Timing and routing faults only show with several nodes.
*	**No GUI in Headless/TUI:** Never run or instantiate GUI objects (`View`, `Window`, `Vdu`, `Flow`, etc.) in standalone test scripts or TUI boot images (`./run_tui.sh`).
*	**REPL Snippets:** To try raw ChrysaLisp code use `echo "lisp -r (print (* 123 456))" | ./run_tui.sh -n 1 -u Test -a -f` (TUI only), or `./run.sh -n 1 -u Test -a -f` when GUI classes are needed. Use `{}` for strings. A View tree can be dumped for inspection with `(ui-save stream view)`. Start with one simple expression and build up.
*	**Scratch Files:** keep temporary files in `tests/scratch/` and clean them up; never in the workspace root, never under `obj/`, which is for code only.
