# Terminal

UNDER CONSTRUCTION

## Overview
ChrysaLisp currently provides a rudimentry terminal ***shell*** application. The shell is available in both the GUI as well as TUI invocations.

The shell application comes ready to run:

* Console commands
* Lisp REPL
* ***etc***

## Command Applications
Commands, located in the `cmd/` directory, are all Lisp applications that can be invoked directly at the shell command line. They typically have a `.lisp` extension however; you only need to specifcy the base command script name in order for it to execute.

A few command examples:
* lisp
* echo
* files
* cat
* dump

Are all commands that can be invoked from the shell command line. Note that some of the commands may include optional arguments to be passed to the command. Typically, commands do provide argument help and can be displayed by `command-name -h` where ***command-name*** is the name of the command.

## Include Files
Commands are lisp programs and, as such, have support for including other files. This enables resolution and re-use of terms and functions that enhance and enable the processing of the command. While not required, new include files co-resident with commands in the `cmd/` folder follow a naming convention with a file extension of `.inc`.

### Examples

