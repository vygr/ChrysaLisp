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
## head 
```lisp
usage: head [-h] [-v] [-c] {} ...

returns lines from beginning of file, defaults to first 10 lines

optional arguments:
    -h,--help                 displays application help and exits
    -v,--version              displays application version and exits
    -c,--count                count of lines from top of file to display


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
Usage: make [options] [all] [boot] [platforms] [doc] [syms]
    options:
        -h --help: this help info.
    all: include all .vp files.
    boot: create a boot image.
    platforms: for all platforms not just the host.
    docs: scan source files and create documentation.
    syms: scan source files and create VP sys/symbols.inc.

```
## null 
```lisp
Usage: null [options]
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
## sort 
```lisp
Usage: sort [options] [line] ...
	options:
		-h --help: this help info.
	If no lines given on command line
	then will sort lines from stdin.

```
## tail 
```lisp
usage: tail [-h] [-v] [-c] {} ...

returns lines from end of file, defaults to last 10 lines

optional arguments:
    -h,--help                 displays application help and exits
    -v,--version              displays application version and exits
    -c,--count                count of lines from bottom of file to display


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
		-f --format 1|8|16|24|32: default 32.
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
usage: wc [-h] [-v] [-f] [-l] [-w] [-c] {} ...

word counter

optional arguments:
    -h,--help                 displays application help and exits
    -v,--version              displays application version and exits
    -f,--file                 file input to wc
    -l,--lines                display count of lines
    -w,--words                display count of words
    -c,--chars                display count of characters


```
