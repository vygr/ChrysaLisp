## The ChrysaLisp Distributed Pipe System and Build Commands

ChrysaLisp features a powerful command-line environment that extends the
familiar concept of Unix-like pipes into a distributed computing paradigm. This
allows for complex data processing and system building tasks to be orchestrated
across multiple VP (Virtual Processor) nodes in a network. This document
explores the core concepts, key components, options processing, and provides
detailed examples using the `grep`, `forward`, and `make` commands.

### Core Concepts

1. **Individual Command Applications:**

    * Most command-line tools in ChrysaLisp are implemented as Lisp scripts
    (e.g., `cmd/grep.lisp`, `cmd/make.lisp`).

    * They typically follow the standard model of reading from standard input
    (stdin), writing to standard output (stdout), and writing errors to
    standard error (stderr).

    * They accept command-line arguments, which are parsed using a common
    options processing library.

    * When a command application is invoked, it runs as a ChrysaLisp task.

2. **Piping (`|`):**

    * Like Unix shells, ChrysaLisp allows the stdout of one command to be
    "piped" to the stdin of another. For example: `cat file.txt | grep
    "pattern" | sort`.

    * This creates a pipeline where data flows sequentially through a series of
    processing stages.

3. **Distribution:**

    * **Pipeline Distribution:** Each command in a pipeline can potentially run
    as a separate task on a *different VP node* in the ChrysaLisp network. The
    system handles the transparent routing of data (stdout/stdin) between these
    distributed tasks.

    * **Task Distribution within Commands:** Some commands, particularly those
    designed for parallel processing (like `make` or `grep` in certain modes),
    can themselves spawn multiple worker tasks that may be distributed across
    available nodes to perform sub-operations in parallel.

### Key Components of the Pipe System

The distributed pipe functionality is primarily managed by components found in
`lib/task/pipe.inc` and `lib/task/pipefarm.inc`.

1. **`pipe-split cmdline_string -> list_of_commands`** (from
`lib/task/pipe.inc`):

    * This crucial Lisp function is responsible for parsing a raw command-line
    string into a list of individual command segments.

    * It correctly handles quoted arguments, ensuring that pipe symbols (`|`)
    or other special characters within quotes are treated as part of an
    argument and not as pipeline separators.

    * For example, `pipe-split "grep -e \"hello | world\" file.txt | sort"`
    would likely produce `("grep -e \"hello | world\" file.txt" "sort")`.

2. **The `Pipe` Lisp Class** (from `lib/task/pipe.inc`):

    * This class is the Lisp-level abstraction for creating and managing a
    single, potentially distributed, pipeline.

    * **Constructor `(Pipe cmdline_string [&optional
    user_select_mailboxes])`:**

        * Takes the full command line string (e.g., "cmd1 | cmd2").

        * Uses `pipe-split` to break it into individual commands.

        * For each command segment:

            * It constructs the path to the command's Lisp script (e.g.,
            "cmd/cmd1.lisp").

            * It calls `(open-pipe list_of_command_scripts)`, which launches
            each command as a new ChrysaLisp task. These tasks can be scheduled
            on any available node. `open-pipe` returns a list of `net_id`s,
            which are the *stdin* mailboxes for each launched command task.

            * **Wiring the Pipeline:** This is the most complex part. The
            constructor then iterates *backwards* through the launched tasks.
            For each task `N` and `N-1`:

                * It sends an initialization message to task `N`. This message
                tells task `N` what mailbox to use for its stdout (which will
                be the stdin mailbox of task `N+1`, or a final output mailbox
                for the last command) and what mailbox to use for its stderr.

                * Task `N` acknowledges this setup by sending its *actual*
                stdout mailbox ID back.

                * This acknowledged stdout ID of task `N` is then used as the
                target stdout for task `N-1`. This process effectively connects
                `stdout` of task `N-1` to `stdin` of task `N`.

            * The `Pipe` object keeps track of the stderr streams of all
            commands in the pipeline and the final stdout stream of the last
            command.

    * **Methods:**

        * `:poll()`: Checks if there's any output available from any of the
        stderr streams or the final stdout stream, or from any
        `user_select_mailboxes`.

        * `:read()`: Reads available data from the pipeline (either an error
        message from a command's stderr or data from the final stdout). Returns
        `:t` if a user-supplied mailbox (from `user_select`) has data. Returns
        `:nil` if the pipe is closed.

        * `:write(string_data)`: Writes `string_data` to the stdin of the
        *first* command in the pipeline.

        * `:close()`: Initiates a graceful shutdown of the pipeline, waiting
        for all stderr streams to report they have stopped.

3. **`pipe-run cmdline_string [&optional output_function]`** (from
`lib/task/pipe.inc`):

    * A convenience wrapper that creates a `Pipe` instance for the given
    `cmdline_string`, reads all data from its final stdout until the pipe
    closes, and calls `output_function` (defaulting to `print`) for each chunk
    of data received.

4. **`pipe-farm jobs_list [&optional retry_timeout]`** (from
`lib/task/pipefarm.inc`):

    * **Purpose:** To execute a list of *independent* command-line jobs in
    parallel, distributing them across available nodes using a farm of worker
    tasks. This is different from the `Pipe` class, which manages a
    *sequential* flow of data through stages.

    * **Worker Task:** It uses a generic worker script, `lib/task/cmd.lisp`.

    * **Operation:**

        * It creates a `Local` farm (similar to the `Farm` class described in
        `lib/task/farm.inc`).

        * The `create` callback for the farm launches instances of
        `lib/task/cmd.lisp` on available nodes.

        * `dispatch-job` sends individual command strings from the `jobs_list`
        to available worker tasks. The message includes a reply mailbox and a
        unique key.

        * The `lib/task/cmd.lisp` worker executes the received command (using
        `pipe-run` internally, so each job can itself be a pipeline), captures
        its stdout/stderr, prefixes it with the key, and sends it back.

        * `pipe-farm` collects these results.

        * It includes fault tolerance: if a worker task becomes unresponsive
        (detected by a timer and the farm's `:refresh` mechanism), its job is
        requeued by the `destroy` callback and reassigned.

    * This is ideal for batch processing tasks that can run independently, like
    compiling multiple files or searching multiple files.

5. **`lib/task/cmd.lisp` (Generic Command Worker):**

    * The worker script launched by `pipe-farm` and potentially by other
    task-spawning mechanisms.

    * It waits for a message containing a command string, a reply key, and a
    reply mailbox.

    * It executes the command string using `(pipe-run command_string
    output_to_string_stream_lambda)`.

    * It captures all output (stdout and stderr from `pipe-run` if it errors)
    into a string.

    * It prepends the `reply_key` (followed by a newline) to this captured
    output string.

    * It sends the combined string back to the specified reply mailbox.

    * It has an inactivity timeout; if it doesn't receive a job for a period,
    it exits.

### Options Processing (`lib/options/options.inc`)

ChrysaLisp command-line applications use a standardized library for parsing
options.

* **`optlist_definition`:** Each command defines a list that specifies its
valid options. An entry in this list typically looks like:

    * `'( (list_of_aliases) handler_or_help_string )`

    * Example: `'( (("-h" "--help") "This help text.") (("-f" "--file")
    handle_file_option_func) )`

* **`options stdio_obj optlist_definition -> list_of_non_option_args | :nil`:**

    * This is the main parsing function.

    * It takes the application's `stdio` object (to get command-line arguments
    via `stdio-get-args`) and its `optlist_definition`.

    * It uses `options-split` (which respects quoted arguments) to tokenize the
    raw argument string.

    * It iterates through the tokenized arguments:

        * If an argument starts with `-`, it's treated as an option.

        * `options-find` is used to look up the option in the
        `optlist_definition`. If not found, it typically defaults to looking
        for `-h`.

        * If the handler part of the found option entry is a string, it's
        printed as help text (using `options-print`), and parsing usually stops
        (returning `:nil`).

        * If the handler is a Lisp function, that function is called with the
        remaining arguments and the option string itself. The handler function
        is responsible for consuming any values associated with the option
        (e.g., the filename for `-o <filename>`) from the remaining arguments
        list and must return the updated list of remaining arguments.

    * Arguments not starting with `-` are collected as non-option arguments
    (e.g., file paths).

    * It returns the list of collected non-option arguments, or `:nil` if help
    was displayed or a parsing error occurred.

### Detailed Command Examples

1. **`grep` (`cmd/grep.lisp`)**

    * **Purpose:** Searches for patterns in text.

    * **Options Processing:** Defines an extensive `usage` list for options
    like `-e pattern`, `-f` (file mode), `-w` (whole words), `-r` (regexp
    mode), `-c` (coded pattern), `-m` (markdown mode). Handler lambdas in the
    `usage` list set global flags (e.g., `file_flag`, `regexp_flag`) or the
    `pattern` variable.

    * **Core Logic:**

        * Uses `(query pattern words_flag regexp_flag)` to get a search engine
        instance (`Substr` or `Regexp`) and processed pattern/metadata.

        * `grep-stream`: Reads a stream line by line, applies `(. search
        :match? line pattern meta)`, and prints matching lines. Handles `-m`
        (markdown mode) by skipping "```" code blocks.

        * `grep-file`: Reads a file line by line. If a match is found, prints
        the filename and stops for that file. Also handles `-m`.

    * **Distribution:**

        * If `-f` (file mode) is used with multiple files, `grep` uses
        `pipe-farm`. It constructs new `grep` command lines for each input file
        (e.g., `grep -c -f -e <coded_pattern> <filepath>`) and passes these to
        `pipe-farm`. The `-c` flag ensures the pattern is passed in an encoded
        way to the worker `grep` instances. Each worker `grep` then runs in
        file mode on its assigned file and prints the filename if it matches.
        The main `grep` collects these filenames.

        * If not in `-f` mode, it processes files sequentially or stdin
        directly.

2. **`forward` (`cmd/forward.lisp`)**

    * **Purpose:** Scans Lisp source files for forward references to functions
    or macros.

    * **Options Processing:** Has a simple `usage` list, primarily for `-h`.

    * **Core Logic (`work` function):**

        * Performs a two-pass scan of a single Lisp file.

        * Pass 1: Collects all `(defun ...)` and `(defmacro ...)` definitions
        into `defs_map` (name -> line number) and all potential function/macro
        uses into `uses_map` (name -> list of usage line numbers) using regex
        matching.

        * Pass 2: Iterates through `uses_map`. For each used name, if it's in
        `defs_map`, it checks if any usage line number is less than its
        definition line number. If so, it prints a forward reference warning.

    * **Distribution:**

        * If multiple files are specified (or read from stdin), `forward` uses
        `pipe-farm`.

        * It constructs new `forward` command lines for each input file (e.g.,
        `forward <filepath>`).

        * Each worker `forward` instance processes its assigned file using the
        `work` function and prints any warnings. The main `forward` collects
        and prints this output.

3. **`make` (`cmd/make.lisp`)**

    * **Purpose:** The primary build tool for ChrysaLisp; compiles VP files,
    creates boot images, generates documentation.

    * **Options Processing:** Defines a `usage` list for targets like `all`,
    `boot`, `platforms`, `docs`, `it` (all-inclusive), `test`.

    * **Core Logic (Dispatch):** The `main` function parses these target flags
    and calls appropriate underlying functions, most of which are imported from
    `lib/asm/asm.inc` (e.g., `make`, `make-all`, `remake-all-platforms`) or
    defined locally (`make-docs`).

    * **Distribution:**

        * **Compilation (`compile` function from `lib/asm/asm.inc`):** The core
        `compile` function (called by various `make` targets) uses a `Local`
        farm (very similar to `pipe-farm`) to distribute the compilation of
        individual `.vp` files across available nodes. Worker tasks for
        compilation are instances of `lib/asm/asm.lisp`.

        * **Documentation Generation (`make-docs` subcommand):**

            * The `make-docs` function within `cmd/make.lisp` itself uses
            `pipe-farm` to execute other command-line tools (like `grep -h`,
            `cat -h`, etc.) with the `-h` option to capture their help output
            for inclusion in the `commands.md` documentation.

            * The scanning of source files for doc strings is done sequentially
            by the main `make-docs` process.

        * **Cross-Platform Builds (`make-platforms`, etc.):** These functions
        iterate through supported CPU/ABI combinations and invoke the
        single-platform build functions, effectively distributing the build
        process for different targets if the underlying `compile` uses the farm
        for each.

### Advanced Distribution Concepts

* **Layered Distribution:** ChrysaLisp demonstrates multiple layers of
distribution:

    * **Pipeline Stages:** The `Pipe` class can distribute stages of a `cmd1 |
    cmd2` pipeline to different nodes.

    * **Independent Job Farming:** `pipe-farm` (and the `Local` farm used by
    `compile`) distributes a set of independent jobs (command executions or
    file compilations) to a pool of worker tasks running on various nodes.

    * **Internal Task Spawning by Commands:** A command executed as part of a
    pipeline or by `pipe-farm` can *itself* be a complex application that
    spawns its own worker tasks for internal parallelism (e.g., a hypothetical
    parallel sorting command).

* **Network Transparency:** The underlying messaging system (`sys_mail`) and
task management (`sys_task`, `open-task`) abstract away the network details,
allowing tasks to communicate via `net_id`s regardless of their physical
location on the VP network.

* **`Local` / `Farm` Classes:** These Lisp-level classes (from
`lib/task/local.inc` or `lib/task/farm.inc`) provide a convenient abstraction
for managing a pool of worker tasks, including launching, monitoring (via
timestamps and `:refresh`), and restarting them.

### Error Handling and Robustness

* **`Pipe` Class:** If a command in the pipeline fails to launch during `Pipe`
construction, it attempts to send abort signals to already launched commands.
The `Pipe`'s `:read` method can detect when a stream component (stderr/stdout)
has closed, allowing the controlling application to react.

* **`pipe-farm` / `Local` Farm:** The `destroy` callback (which requeues jobs)
and the `:refresh` method (which detects timeouts and triggers restarts)
provide a degree_ of fault tolerance for worker tasks.

* **`lib/task/cmd.lisp` (Worker):** Uses `(catch ...)` around `pipe-run` to
capture errors from the executed command and send them back as part of the
result string. It also has an inactivity timeout to self-terminate if orphaned.

* **Individual Commands:** Often print error messages to stderr or return
specific exit codes/statuses, which can be captured if they are run within a
`pipe-run` managed by `lib/task/cmd.lisp`.

### Conclusion

The ChrysaLisp distributed pipe system, coupled with its command-line
applications and options processing, provides a remarkably flexible and
powerful environment for both interactive use and complex, automated system
building tasks. The ability to seamlessly distribute pipeline stages or
parallelize independent jobs across a network, combined with robust options
parsing and fault-tolerant farm management, makes it a sophisticated platform.
Commands like `grep`, `forward`, and especially `make` serve as excellent
examples of how these distributed capabilities are leveraged to perform
significant work efficiently.