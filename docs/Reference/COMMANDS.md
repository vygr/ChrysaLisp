## cat
```code
Usage: cat [options] [path] ...

	options:
		-h --help: this help info.

	If no paths given on command line
	then paths are read from stdin.

```
## dump
```code
Usage: dump [options] [path] ...

	options:
		-h --help: this help info.
		-c --chunk num: chunk size, default 8.

	If no paths given on command line
	then will dump stdin.

```
## echo
```code
Usage: echo [options] arg ...

	options:
		-h --help: this help info.

```
## files
```code
Usage: file [options] [prefix] [postfix]

	options:
		-h --help: this help info.

	Find all files that match the prefix and postfix.

		prefix default "."
		postfix default ""

	eg.
	files ./apps/wallpaper/ .tga

```
## grep
```code
Usage: grep [options] [pattern] [path] ...

	options:
		-h --help: this help info.
		-e --exp pattern: regular expression.

	pattern:
		\s	space
		\q	double quote
		\t	tab
		\r	return
		\f	form feed
		\n	line feed
		\w	white space
		\b	black space
		\\	esc for \ etc
		.	any char
		+	one or more
		*	zero or more
		?	zero or one
		|	or
		^	start of line
		$	end of line
		[]	class, [0-9], [abc123]
		()	group

	If no paths given on command line
	then will grep stdin.

```
## gui
```code
Usage: gui [node ...]

	options:
		-h --help: this help info.

	Launch a GUI on nodes.

	If none present on command line then
	will read from stdin.

```
## head
```code
Usage: head [options file]

	options:
		-h --help: this help info.
		-c --count num: line count, default 10.

	Returns lines from start of file or stdin.

	Defaults to first 10 lines.

```
## link
```code
Usage: link [options] 000-000 ...

	options:
		-h --help: this help info.

	Start SHMEM link driver/s.

	If no links names given on command line
	then names are read from stdin.

```
## lisp
```code
Usage: lisp [options] [path] ...

	options:
		-h --help: this help info.

	If no paths given on command line
	then will REPL from stdin.

```
## make
```code
Usage: make [options] [all] [boot] [platforms] [doc] [it] [test]

	options:
		-h --help: this help info.

	all:		include all .vp files.
	boot:		create a boot image.
	platforms:	for all platforms not just the host.
	docs:		scan source files and create documentation.
	it:			all of the above !
	test:		test make timings.

```
## nodes
```code
Usage: nodes [options]

	options:
		-h --help: this help info.

```
## null
```code
Usage: null [options]

	options:
		-h --help: this help info.

```
## sdir
```code
Usage: sdir [options] [prefix]

	options:
		-h --help: this help info.

```
## shuffle
```code
Usage: shuffle [options] [line] ...

	options:
		-h --help: this help info.

	If no lines given on command line
	then will shuffle lines from stdin.

```
## slice
```code
Usage: slice [options]

	options:
		-h --help: this help info.
		-s --start num: start char index, default 0.
		-e --end num: end char index, default -1.

	Slice the lines from stdin to stdout.

```
## sort
```code
Usage: sort [options] [line] ...

	options:
		-h --help: this help info.

	If no lines given on command line
	then will sort lines from stdin.

```
## split
```code
Usage: split [options]

	options:
		-h --help: this help info.
		-s --sep separator: default ,.
		-e --sel num: selected element, default :nil.

	Split the lines from stdin to stdout.

	Optionaly select a specific element of
	the split.

```
## tail
```code
Usage: tail [options file]

	options:
		-h --help: this help info.
		-c --count num: line count, default 10.

	Returns lines from end of file or stdin.

	Defaults to last 10 lines.

```
## tee
```code
Usage: tee [options] [path] ...

	options:
		-h --help: this help info.

	Read from stdin, write to stdout and all given paths.

```
## time
```code
Usage: time [options]

	options:
		-h --help: this help info.

	Time the duration of the stdin stream.

	Print result to stderr.

```
## tocpm
```code
Usage: tocmp [options] [path] ...

	options:
		-h --help: this help info.
		-f --format 1|8|12|15|16|24|32: pixel format, default 32.

	Load the images and save as .cpm images.

	If no paths given on command line
	then paths are read from stdin.

```
## unique
```code
Usage: unique [options] [line] ...

	options:
		-h --help: this help info.

	If no lines given on command line
	then will read lines from stdin.

```
