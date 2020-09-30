# Terminal

## Overview

ChrysaLisp provides a rudimentary console shell. This is made possible from the
Terminal application, which is the default application running in the text UI
(TUI) or as a window in the graphical UI (GUI).

The shell application comes ready to run **console commands** at it's prompt.
For example, a familiar command `echo` prints text after the command name back
to the shell's stdout:

```
>echo this is a command

this is a command

>
```

### Usage

```
command [arg | command arg ...]
```

Where ***command*** is the name of the command and is either followed by an
argument to pass to the command or the piped `|` input from the output of
another command.

## Command Applications

Commands, located in the `cmd/` directory, are all Lisp applications that can
be invoked directly at the shell prompt. They typically have a `.lisp`
extension however; you only need to specify the base command script name in
order for it to execute.

For example:

* lisp - The lisp REPL
* echo - Echo input to standout
* files - Lists files

are but a few of the commands that can be invoked from the shell.

Note that some of the commands may include optional arguments to be passed to
the command. Typically, commands do provide argument help and can be displayed
by `command -h`.

You can find additional command information in [cmds/](./COMMANDS.md)

## Library Files

Commands are lisp programs and, as such, have support for including other
files. This enables resolution and re-use of terms and functions that enhance
and enable the processing of the command. The defacto standard for ChrysaLisp reusable
libraries to locate them under the `./lib` folder where each library has it's own
subfolder. Within these subfolders you will find one or more files that have an
`.inc` extension.

### Hello World example

C'mon, there always is a 'Hello World' example isn't there? The following is a
bare bones command but is useful to call out some particulars to get you going
in the right direction. To see it run, just copy this into a file called
`hw.lisp` and place it in the `cmd/` folder:

```lisp
01: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
02: ; hw.lisp - hello world example
03: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
04:
05: ;imports
06: (import "class/lisp.inc")
07:
08: ; Command entry point
09: (defun main ()
10:   ;initialize pipe details and command args, abort on error
11:   (when (defq stdio (create-stdio))
12:     ; create-stdio connects:
13:     ; stdin  - Console input stream
14:     ; stdout - Console output stream
15:     ; stderr - Console error stream
16:     ; args   - List of command line arguments
17:
18:     ; Get the args and omit command name at position 0
19:     (defq args (slice 1 -1 (stdio-get-args stdio)))
20:     ; prin to stdout without CR
21:     (prin "Hello World")
22:     ; Test for greeting
23:     (if (> (length args) 0)
24:         (print ": " (join args " "))
25:         (print "!"))))
```

Of note:

* Line  6 - Import to use functions from the Lisp class include
* Line  9 - Main entry point of command
* Line 11 - Creates `stdin stdout stderr and args` in the command environment and tests for success
* Line 19 - Fetches the arguments list passed to the command and omit the first entry which is the name of the command
* Lines 21 through 25 - The body of the command
