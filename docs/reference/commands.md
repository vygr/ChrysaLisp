## cat
```code
Usage: cat [options] [path] ...

	options:
		-h --help: this help info.
		-f --file: prepend file name.

	If no paths given on command line
	then paths are read from stdin.
```
## cp
```code
Usage: cp [options] path1 path2

	options:
		-h --help: this help info.

	Copy file path1 to path2.
```
## diff
```code
Usage: diff [options] file_a [file_b]

	options:
		-h --help: this help info.
		-s --swap: swap sources.

	Calculate patch between text file a and text file b.
	If no second file is given it will be read from stdin.
```
## docs
```code
Usage: docs [options] [path] ...

	options:
		-h --help: this help info.
		-j --jobs num: max jobs per batch, default 1.

	Scan for documentation in files, creates
	a merged tree of all the information.

	If no paths given on command line
	then will take paths from stdin.
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
Usage: files [options] [prefix] [postfix]

	options:
		-h --help: this help info.
		-i --imm: immediate dependencies.
		-a --all: all dependencies.
		-d --dirs: directories.

	Find all paths that match the prefix and postfix.

		prefix default "."
		postfix default ""

	eg.
	files -a apps/ .lisp
```
## forward
```code
Usage: forward [options] [path] ...

	options:
		-h --help: this help info.

	Scan source files for use of forward
	references to functions or macros.

	If no paths given on command line
	then will test files from stdin.
```
## grep
```code
Usage: grep [options] [pattern] [path] ...

	options:
		-h --help: this help info.
		-e --exp pattern: regular expression.
		-f --file: file mode, default :nil.
		-w --words: whole words mode, default :nil.
		-r --regexp: regexp mode, default :nil.
		-c --coded: encoded pattern mode, default :nil.
		-m --md: md doc mode, default :nil.

	pattern:
		^  start of line
		$  end of line
		!  start/end of word
		.  any char
		+  one or more
		*  zero or more
		?  zero or one
		|  or
		[] class, [0-9], [abc123]
		() group
		\r return
		\f form feed
		\v vertical tab
		\n line feed
		\q double quote
		\t tab
		\s [ \t]
		\S [^ \r\f\v\n\t]
		\d [0-9]
		\D [^0-9]
		\l [a-z]
		\u [A-Z]
		\a [A-Za-z]
		\p [A-Za-z0-9]
		\w [A-Za-z0-9_]
		\W [^A-Za-z0-9_]
		\x [A-Fa-f0-9]
		\\ esc for \ etc

	If no paths given on command line
	then will grep from stdin.
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
Usage: link [options] CLB-L1 CLB-L2 000-000 ...

	options:
		-h --help: this help info.

	Start SHMEM link driver/s.

	`CLB-L1 CLB-L2`, are the names of the ChrysaLib `-shm` links.
	If you're bridging Lisp subnets over CLB.

	Internal Lisp subnet links are of the form `001-002`, or if
	connecting local Lisp subnets the recommended form is
	`000-000` for subnet bridge 1, `001-001` for subnet bridge 2 etc.

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

	all:        include all .vp files.
	boot:       create a boot image.
	platforms:  for all platforms not just the host.
	docs:       scan source files and create documentation.
	it:         all of the above !
	test:       test make timings.
```
## mv
```code
Usage: mv [options] path1 path2

	options:
		-h --help: this help info.

	Move file path1 to path2.
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
## patch
```code
Usage: patch [options] file_a [file_b]

	options:
		-h --help: this help info.
		-s --swap: swap sources.

	Patch text file a with text file b.
	If no second file is given it will be read from stdin.
```
## repeat
```code
Usage: repeat [options] command_line

	options:
		-h --help: this help info.
		-c --count: count, default 10.

	Repeat run command line.
```
## rm
```code
Usage: rm [options] [path] ...

	options:
		-h --help: this help info.

	If no paths given on command line
	then paths are read from stdin.
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

	Optionally select a specific element of
	the split.
```
## stats
```code
Usage: stats [options]

	options:
		-h --help: this help info.

	Some simple object statistics.
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
## template
```code
Usage: template [options] [path] ...

	options:
		-h --help: this help info.
		-j --jobs num: max jobs per batch, default 1.

	Template command app for you to copy as
	a starting point.

	Add your description here.

	If no paths given on command line
	then will take paths from stdin.
```
## test
```code
Usage: test [options]

	options:
		-h --help: this help info.

	Simple timing test framework.
	
	To be stable and accurate this should be
	run on a single node !
	
	./run_tui.sh -n 1
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
Usage: tocpm [options] [path] ...

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
## vpstats
```code
Usage: vpstats [options] [path] ...

	options:
		-h --help: this help info.

	Scan for VP instruction usage stats.

	If no paths given on command line
	then will take paths from stdin.
```
## wc
```code
Usage: wc [options] [path] ...

	options:
		-h --help: this help info.
		-wc: count words.
		-lc: count lines.
		-pc: count paragraphs.

	If no count options are given, defaults
	to all (words, lines, paragraphs).

	If no paths given on command line
	then paths are read from stdin.
```
