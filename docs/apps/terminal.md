# Terminal

The `Terminal` application is a simple text terminal with a familiar feel to
traditional pipeing terminals.

The commands you use come from the `cmd/` folder. You can see the list of
available commands in the `commands.md` document.

Command history is saved and loaded to your user folder under the name
`terminal.tre`. The cursor up and down keys let you scroll through the
history, just hit enter when you find the command you wish to run. Or type in a
new one. Only successfully started pipes of commands will be saved in the
history file.

If you hover the mouse over the embedded UI below you can see the kind of
features available. There are more features available through the key bindings
which can be found in the `keys.md` documentation.

## UI

```widget
apps/terminal/widgets.inc *window* 512 512
```

## Overview

ChrysaLisp provides a rudimentary console shell. This is made possible from the
Terminal application, which is the default application running in the text UI
(TUI) or as a window in the graphical UI (GUI).

The shell application comes ready to run `console commands` at it's prompt. For
example, a familiar command `echo` prints text after the command name back to
the shell's stdout:

```
>echo this is a command
this is a command
>
```

### Usage

```
command [arg | command arg ...]
```

Where `command` is the name of the command and is either followed by an
argument to pass to the command or the piped `|` input from the output of
another command.

## Command Applications

Commands, located in the `cmd/` directory, are all Lisp applications that can be
invoked directly at the shell prompt. They typically have a `.lisp` extension
however; you only need to specify the base command script name in order for it
to execute.

For example:

* `lisp` - The lisp REPL

* `echo` - Echo input to standout

* `files` - Lists files

are but a few of the commands that can be invoked from the shell.

Note that some of the commands may include optional arguments to be passed to
the command. Typically, commands do provide argument help and can be displayed
by `command -h`.

You can find additional command information in `docs/reference/commands.md`.

## Library Files

Commands are lisp programs and, as such, have support for including other files.
This enables resolution and re-use of terms and functions that enhance and
enable the processing of the command. The defacto standard for ChrysaLisp
reusable libraries to locate them under the `lib/` folder where each library has
its own subfolder. Within these subfolders you will find one or more files that
have an `.inc` extension.

## Hello World example

C'mon, there always is a 'Hello World' example isn't there? The following is a
bare bones command but is useful to call out some particulars to get you going
in the right direction. To see it run, just copy this into a file called
`hw.lisp` and place it in the `cmd/` folder:

```code
01: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
02: ; hw.lisp - hello world example
03: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
04:
05: 06: (import "class/lisp.inc")
06:
07:
08: ; Command entry point
09: (defun main ()
10:     ;initialize pipe details and command args, abort on error
11:     (when (defq stdio (create-stdio))
12:         ; create-stdio connects:
13:         ; stdin  - Console input stream
14:         ; stdout - Console output stream
15:         ; stderr - Console error stream
16:         ; args   - List of command line arguments
17:
18:         ; Get the args and omit command name at position 0
19:         (defq args (rest (stdio-get-args stdio)))
20:         ; prin to stdout without CR
21:         (prin "Hello World")
22:         ; Test for greeting
23:         (if (> (length args) 0)
24:             (print ": " (join args " "))
25:             (print "!"))))
```

Of note:

* Line  6 - Import to use functions from the Lisp class include

* Line  9 - Main entry point of command

* Line 11 - Creates `stdin stdout stderr and args` in the command environment
and tests for success

* Line 19 - Fetches the arguments list passed to the command and omit the first
entry which is the name of the command

* Lines 21 through 25 - The body of the command

This is the `dump` command that outputs a nicely formatted hex dump of `stdin`
or files given on the command args.

```file
cmd/dump.lisp
```

## Implementation Study

The ChrysaLisp Terminal, found in `apps/terminal/`, is an interactive
command-line interface. It demonstrates a powerful architectural pattern within
the ChrysaLisp ecosystem: the reuse of general-purpose GUI components,
controlled by a specialized logic layer to create a unique user experience. It
combines the text rendering capabilities of the `Edit` widget with the system's
process and I/O management via the `Pipe` class to provide a fully functional
shell.

### 1. Core Architecture and Components

The Terminal's architecture cleverly uses the same core Model and View
components as the Editor and Viewer but wraps them in a Controller that enforces
line-oriented, command-and-response behavior.

*   **Model (State Management):**

    * **`Buffer` Class (`lib/text/buffer.inc`):** A single `Buffer` instance is
        used to hold the *entire terminal session*. This includes past commands,
        their output, and the current, editable command line. The application's
        controller logic is responsible for treating the majority of this buffer
        as read-only.

    * **`Pipe` Class (`lib/task/pipe.inc`):** This is a critical component of
        the model when a command is executing. An instance of `Pipe` is created
        in `app_impl.lisp` when the user presses Enter. It represents the
        running child process, managing its `stdin`, `stdout`, and `stderr`
        streams. The `*pipe*` variable holds the current `Pipe` object or `:nil`
        if no command is running.

    * **Command History (`*meta_map*`):** A simplified version of the Editor's
        state management is used. `apps/terminal/state.inc` defines logic to
        save and load a `:history` list (a Lisp `list` of strings) into
        `"terminal.tre"` in the user's home directory.

*   **View (User Interface):**

    * **`*window*` (`apps/terminal/widgets.inc`):** The main application
        `Window`. The UI is simpler than the Editor's, consisting of a title
        bar, a main content area, and scrollbars.

    * **`Terminal-edit` Class (`apps/terminal/ui.inc`):** This is a custom class
        that inherits from `Viewer-edit` (which in turn inherits from `Edit`).
        The instance, `*edit*`, is the main text area. By inheriting, it gets
        all the advanced text rendering, selection, and scrolling capabilities
        for free, but overrides mouse behavior for a terminal-like feel.

    * **`Vdu` Widget (`gui/vdu/lisp.inc`):** The underlying character-grid
        rendering engine used by the `*edit*` widget.

*   **Controller (Application Logic and Event Handling):**

    * **`apps/terminal/app.lisp`:** Contains the `main` function and the core
        event loop. Its primary responsibility is to arbitrate between two
        sources of input: user interaction from the GUI and text output from a
        running `Pipe`.

    * **`apps/terminal/actions.inc` (and includes):** Defines the `Fmap`s that
        map events and keyboard shortcuts to terminal-specific handler
        functions.

    * **Action Handlers (`cursor.inc`, `edit.inc`):** This is where the
        Terminal's unique behavior is implemented. Functions like `action-home`
        and `action-backspace` are overridden to respect the command prompt and
        prevent the user from editing the session's history.

    * **`apps/tui/tui_child.lisp`:** While part of the `tui` app, this file is
        relevant as it shows the other side of a `Pipe`. The `cmd/` applications
        write to `stdout`, which the Terminal's `Pipe` object reads.

### 2. The Command Execution Lifecycle (The `Pipe`)

The most distinctive feature of the Terminal is its ability to execute external
commands. This is orchestrated by the `Pipe` class.

1.  **User Input:** The user types a command at the prompt. This text is
    inserted into the shared `Buffer` object by `action-insert`.

2.  **Execution (`action-break` in `edit.inc`):** When the user presses Enter,
    `action-break` is triggered.

3.  **Command Extraction:** The function retrieves the text from the current
    line and strips the prompt (`*env_terminal_prompt*`) to get the raw command
    string (e.g., `"ls -l"`).

4.  **Pipe Creation:** A new `Pipe` object is created with
    `(Pipe cmdline *select*)`. This:

    * Parses the command line.

    * Launches the command as a new task using `(open-pipe)`.

    * Redirects the child task's `stdout` and `stderr` to a new mailbox.

    * Adds this new mailbox to the `*select*` list, allowing the main event loop
        to listen to it.

    * The `*pipe*` variable in `app.lisp` is set to this new `Pipe` instance.

5.  **Reading Output:** The main event loop's `(input-select)` now listens for
    messages on the pipe's mailbox (`idx = +select_pipe`).

    * `(. *pipe* :read)` is called. If it returns text data, that data is
        appended to the `Buffer` using `action-insert`. This is how the
        command's output appears on screen.

6.  **Pipe Termination:** When the command finishes, `(. *pipe* :read)` returns
    `:nil`.

    * The `Pipe` is closed with `(. *pipe* :close)`.

    * `*pipe*` is set back to `:nil`.

    * A new, fresh prompt is printed to the `Buffer`.

### 3. The Read-Only/Editable Buffer Illusion

The Terminal cleverly creates the illusion of a read-only history area and an
editable command line using a single, standard `Buffer` widget. This entire
mechanism is implemented in the Controller logic, not the View.

*   **No Special Widget:** The `Terminal-edit` widget does not have any built-in
    "line-editing" mode. It is a full-text editing component by default.

*   **Action Handler Logic:** The illusion is enforced within the action
    handlers in `apps/terminal/cursor.inc` and `edit.inc`.

*   **`input-cursor` Macro (`utils.inc`):** This helper macro is the key. It
    wraps most cursor-moving and editing actions. It ensures that before any
    action is taken, the cursor is moved to the user's last known logical
    position (`*cursor_x*`, `*cursor_y*`) within the buffer. After the action,
    it saves the new position.

*   **Constrained Movement:** Action handlers like `action-left` and
    `action-home` contain explicit logic:

    ```vdu
    (input-cursor
        (cond
            (*pipe* ...) ; If a pipe is running, the line is read-only
            ((if (> x (length *env_terminal_prompt*)) (. buffer :left)))))
    ```

    This code checks if a pipe is active. If not, it allows the cursor to move
    left only if its `x` position is greater than the length of the prompt. The
    user is physically prevented from moving the cursor into the prompt or the
    history above it. `action-backspace` has similar logic to prevent deletion
    of the prompt.

### 4. Command History and Auto-Completion

*   **History (`cursor.inc`):**

    * `action-up` and `action-down` handle history navigation.

    * They access the `:history` list stored in `*meta_map*` and use
        `*history_idx*` to track the current position.

    * The current command line is cleared and replaced with the selected history
        entry, prefixed by the prompt.

*   **Tab Completion (`edit.inc`):**

    * `action-tab` provides file and directory completion.

    * It calls `(url-ext ...)` from `lib/files/urls.inc`, which analyzes the
        current input, lists matching directory/file entries, and completes the
        input to the longest common prefix.

### 5. Summary of Specializations

The Terminal app is a masterclass in adapting general components for a specific
task:

1.  **Reuses `Edit` Widget:** Inherits from `Viewer-edit` to get advanced
    rendering and selection for free.

2.  **Constrains Input via Controller:** Implements read-only history not in the
    widget, but in the action handlers that respond to user key presses.

3.  **Integrates Process Management:** Uses the `Pipe` class to seamlessly run
    external commands and capture their output.

4.  **Manages a Dual-Input Event Loop:** The `main` function's `(mail-select)`
    loop is configured to listen to both GUI events and `Pipe` output, correctly
    arbitrating between user input and command output.

## Conclusion

The ChrysaLisp Terminal demonstrates that a complex, interactive application can
be built by composing existing, general-purpose components. By using a standard
`Buffer` and a subclass of `Edit`, it avoids reimplementing a text view from
scratch. Its "terminal" behavior is an emergent property of the specialized
controller logic that manages user input, constrains cursor movement, and
integrates with the system's `Pipe` facility for command execution. It is a
powerful example of how to build a classic tool in a non-traditional,
message-passing GUI environment.