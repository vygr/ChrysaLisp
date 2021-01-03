## cat
 
```lisp
Usage: cat [options] [path] ...
	options:
		-h --help: this help info.
	If no paths given on command line
	then paths are read from stdin.

```
## dump
 
```lisp
Usage: dump [options] [path] ...
	options:
		-h --help: this help info.
	If no paths given on command line
	then will dump stdin.

```
## echo
 
```lisp
Usage: echo [options] arg ...
	options:
		-h --help: this help info.

```
## files
 
```lisp
Usage: file [options] [prefix] [postfix]
	options:
		-h --help: this help info.
	Find all files that match the prefix and postfix.
		prefix default ".".
		postfix default "".
	eg.
	files ./apps/wallpaper/ .tga

```
## gui
 
```lisp
Usage: gui [node ...]
	options:
		-h --help: this help info.
	Launch a GUI on nodes. If none present
	on command line will read from stdin.

```
## head
 
```lisp
Usage: head [options file]
	options:
		-h --help: this help info.
		-c --count num: default 10.
	Returns lines from start of file or stdin.
	Defaults to first 10 lines.

```
## lisp
 
```lisp
Usage: lisp [options] [path] ...
	options:
		-h --help: this help info.
	If no paths given on command line
	then will REPL from stdin.

```
## make
 
```lisp
Usage: make [options] [all] [boot] [platforms] [doc] [syms] [it]
    options:
        -h --help: this help info.
    all: include all .vp files.
    boot: create a boot image.
    platforms: for all platforms not just the host.
    docs: scan source files and create documentation.
    syms: scan source files and create VP sys/symbols.inc.
    it: all of the above !

```
## nodes
 
```lisp
Usage: nodes [options]
	options:
		-h --help: this help info.

```
## null
 
```lisp
Usage: null [options]
	options:
		-h --help: this help info.

```
## sdir
 
```lisp
Usage: sdir [options] [prefix]
	options:
		-h --help: this help info.

```
## shuffle
 
```lisp
Usage: shuffle [options] [line] ...
	options:
		-h --help: this help info.
	If no lines given on command line
	then will shuffle lines from stdin.

```
## slice
 
```lisp
Usage: slice [options]
	options:
		-h --help: this help info.
		-s --start num: default 0.
		-e --end num: default -1.
	Slice the lines from stdin to stdout.

```
## sort
 
```lisp
Usage: sort [options] [line] ...
	options:
		-h --help: this help info.
	If no lines given on command line
	then will sort lines from stdin.

```
## split
 
```lisp
Usage: split [options]
	options:
		-h --help: this help info.
		-s --sep separator: default ,.
		-e --sel: element default nil.
	Split the lines from stdin to stdout.
	Optionaly select a specific element of
	the split.

```
## tail
 
```lisp
Usage: tail [options file]
	options:
		-h --help: this help info.
		-c --count num: default 10.
	Returns lines from end of file or stdin.
	Defaults to last 10 lines.

```
## tee
 
```lisp
Usage: tee [options] [path] ...
	options:
		-h --help: this help info.
	Read from stdin, write to stdout and all given paths.

```
## tocpm
 
```lisp
Usage: tocmp [options] [path] ...
	options:
		-h --help: this help info.
		-f --format 1|8|16|15|24|32: default 32.
	Load the images and save as .cpm images.
	If no paths given on command line
	then paths are read from stdin.

```
## unique
 
```lisp
Usage: unique [options] [line] ...
	options:
		-h --help: this help info.
	If no lines given on command line
	then will read lines from stdin.

```
## wc
 
```lisp

usage: wc [-h] [-v] [-f :file] [-l] [-w] [-c]

details:

        -h   - displays help and exits
        -v   - displays app version and exits
        -f   - file to summarize (default: stdin)
        -l   - include line count in summary (default: nil)
        -w   - include word count in summary (default: nil)
        -c   - include character count in summary (default: nil)

```
## wcseg
 
```lisp
usage: wcseg [-h] [-v] [-f] [-l] [-w] [-c] {} ...

word counter

optional arguments:
    -h,--help                 displays application help and exits
    -v,--version              displays application version and exits
    -f,--file                 file input to wc
    -l,--lines                display count of lines
    -w,--words                display count of words
    -c,--chars                display count of characters


```
