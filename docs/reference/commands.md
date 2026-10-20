## bench
```code
Usage: bench [options] [path] ...

    options:
        -h --help: this help info.
        -m --match str: only the modules with str in their path.
        -l --list: list the modules, do not run them.
        -r --runs num: the goes each benchmark gets, default 5.
            The best of them is what is shown.
        -s --save file: write what was timed to the file.
        -c --compare file: show each against what a file written
            by -s has for it, and how much it has changed.

    Run the benchmarks, tests/bench/bench_<name>.lisp, or just
    the module paths given, one after another in this task, and
    print what each thing took, in ns.

    They are not tests and (tests) does not run them. They are
    to hold a change to: 'bench -s before.txt', make the
    change and build, 'bench -c before.txt'.

    A time is of this machine, as loaded as it is. Run them on
    a quiet one, with one node, and a change of a few percent
    in a time of a few ns is as likely the machine as the code.
```
## brackets
```code
Usage: brackets [options] [path] ...

    options:
        -h --help: this help info.
        -j --jobs num: max jobs per batch, default 8.
        -v --verbosity [level]: verbosity level 0..3 (default 0, bare -v is 1).
            0: standard (file: OK or error).
            1: summary (total bracket count, max nesting depth).
            2: type breakdown (parens, square, braces, depth, top forms).
            3: deep diagnostic with source line metrics.
        -q --quiet: quiet mode, only report errors.

    Scan source files for bracket matching (parentheses,
    square brackets, and braces) using syntax-aware scanning.
    Comments and string literals are safely ignored.

    What is in a {} string is code being made, and its
    brackets must pair too, but by themselves. A {}
    string ends at its first }, so a name can be put
    into such code, {(def-func 
```
## cat
```code
Usage: cat [options] [path] ...

    options:
        -h --help: this help info.
        -f --file: prepend file name.

    If no paths given on command line
    then paths are read from stdin.
```
## check
```code
Usage: check [options] [path] ...

    options:
        -h --help: this help info.
        -j --jobs num: max jobs per batch, default 1.
        -c --checks names: only these checks, a , between
            them, default all of them.

    Scan source files for what can be seen to be wrong,
    or better said another way, from the source alone.
    Each file is read as a tree, lib/text/tree.inc, so
    what is in a comment or a string is not looked at.
    Each thing found is a line: the file, the line, the
    check and what it found. Nothing is said of a line
    that has, or comes after, a comment that begins
    ;check off

    progn   a (progn) where there is one already, round
            the else forms of an (if) or an (ifn), or as
            the whole body of a (when), (unless), (while)
            or (until).
    class   a class of characters, written as a string,
            that is not in order: (bskip), (bfind), (trim)
            and (split) look one up by halves, and find
            only some of it. (char-class) puts it in order.
    zero    a number written with a point that is too
            small for a fixed to hold, and so is 0.
    shadow  a variable with the name of a function or a
            macro of the system, as a test of an (and), an
            (or) or a clause of a (cond): there it is taken
            as the function, and is always true.
    const   a + constant given a list of strings or numbers
            quoted once: it is run as a form where the
            constant is used, and is no form.
    made    a + constant given a list made as it runs, by
            (map), (filter), (list) or the like: a + name
            is put in where it is used, as the file is read,
            and a list put in is run as a form there. Make
            it a name of *stars*.
    inner   (const f) of a function made inside another: it
            is not there yet as the file is read, and the
            (const) has nothing to take.
    eqlist  (eql x '(...)) of a quoted list with a str or a
            list in it: two lists
            are eql if their elements are the same objects in
            the same order. A number or a symbol is, a str or
            a list in a quoted list is its own and is never the
            same object as one made elsewhere. Compare what
            they print as.
    again   a (defun) inside another, that uses a name the
            other made with a (defq) before it, and a (defq)
            of that name again after it, in the other: the
            (defq) is to the one name, in the other's
            environment, and what the inner function had by
            it is gone, from then on.
    unquote ,f, ,(const f), ,(f ...) or ~(f ...) in a
            backquote in a module,
            f a function of the module not exported: it is
            looked for as the form is made, when the
            function runs, and the module's names are gone.
    ignore  a name after &ignore in a (bind) list or a list
            of parameters, or a list in one: &ignore ends
            the list, and the name is never bound. & is the
            place that takes a thing and binds nothing.
    param   a list of parameters, or a list in one, whose
            first name is a macro of the system, (defun f
            (bits) ...): it is expanded as a call of it.
    empty   () given to a call, (list () x): it is run as
            a form, and calls nothing. (list) or '().
    past    (inc) of an (rfind), which gives the place after
            what it found already: one more is past it.
    shift   (>> (- a b) n), a logical shift of a difference:
            one less than 0 is made a large number, the place
            in a table of an AC-3 decoder 15 bits not 0.
            (>>>) shifts it as a signed number.
    unbound a call of what is no function or macro of the
            system, nor named by the file or anything it
            imports, at any depth: a program, or a module.
            What runs it may give it more, the tests their
            suite, the diagrams make's; the assembler, which
            runs in an env of many files, is not looked at.
    early   a + constant that is a quoted list, used in the
            (defq) that makes it: there it is not yet a
            constant, and is the quote and the list, two things.
    copy    (each f (slice a s e) (slice b s e)) or the like,
            every sequence given to (each), (map), (some),
            (every), (notany), (notevery), (filter) or a
            (reduce) with its init, a (slice), a (rest) or a
            (most) of the same part: each is copied to be
            walked once. The ! form walks that part of the
            one it is given, (each! f (list a b) s e), and (!)
            in it is then the index in the whole. A (reverse)
            is the part from the end, -1 0, and (reach),
            (rmap), (rsome), (rreduce) and (rfilter) walk from
            the end. A (rest) or a
            (most) of an empty one is (), where the ! form
            from 1, or to -2, is an error: if it can be empty
            it is not the same.
    index   (elem-get x (!)) in the function given to an
            iterator, (each), (map), (reduce), (some) and the
            rest, and their ! forms, not in an iterator inside
            it: x is walked beside what it is given, (each
            (lambda (a b) ...) as x). Not of an x the function
            makes, nor one whose (length x) it looks at, which
            may be shorter: walked beside, the walk would end
            at its end.
    collect (defq out (list)) and then at once (each (# (push
            out x)) seq), or of a (lambda), its whole function
            one (push) of one thing to that name: it is (defq
            out (map (# x) seq)), which is quicker.
    reverse (reverse (slice x s e)), two copies, is (slice x e s),
            one. Of a (rest) or a (most), (slice x -1 1) or
            (slice x -2 0), which of an empty one is an error.
    width   (char x n) of a number n written there that is
            not 1 to 8: (char) makes no more than 8 bytes,
            (char 0 32) is 8 of them, not 32.
    cut     (slice (str x) 0 n), a string of something cut to
            a number written there: one that is shorter than
            that is an error, (min n (length s)).
    style   (if tst (progn ...)) with no else, a (when);
            (if (not tst) form else ...), an (ifn), and
            with no else an (unless); the same of an (ifn),
            the other way about; a (cond) or a (condn) of
            one clause; and (when (and ...) form), or an
            (if) of one with no else, the (and) with the
            form as the last of it.
    doc     a ; (name args) -> result comment under a
            (defun) or (defmacro) that is of another name,
            or of more or fewer things than it takes.
    enum    a field of an (enums) named size: the name it
            makes is the one the (enums) makes for how many
            fields there are.
    bracket brackets that do not pair, more opened than
            closed or more closed than opened. The file does
            not load.
    stray   a name by itself at the top of a file, in no form.
            A bracket closed a form before its end, and the
            rest of it is run by itself.
    args    a (# ...) given to (map), (each), (filter), (some),
            (reduce), (sort), (lines!) or the like, or the :each
            of a map or a set, that takes another number than
            it is called with: one a %n in it, none of no %n,
            and a %n in an inner (# ) its own; an iterator
            gives an element of each sequence, reduce and sort
            two; an :each, of a map two and a set one, only one
            of no %n.
    global  a *name* made at the top of a module, not
            exported, used in a function of it: it is looked
            for when the function runs, and is gone by then.
    self    in the method :m of a class, a (call :c :m '(:r0
            ...)) of the same method of this class or one
            above it, that is not static: a call goes by the
            vtable of the object, and of one of this class
            that is this method again, for ever. s-call for
            the one above, d-call for a class's own.
    held    a (create-stdio) that is not kept in a name, by a
            (defq). It is let go of as soon as it has been
            looked at, its streams say they are stopping, and
            what is written after may not be read.
    struct  in a module, a file with an (env-push), a field
            of a structure, in (getf), (setf) or (setf->),
            that no (structure), (enums) or (defq) gives, of
            this file, of what it imports, and of what those
            import, nor of what root.inc imports. A module that
            works only because what loaded it first had
            imported the structure fails on its own, and on a
            release build its class is not made at all. And
            in any file, a (structure +x) where root.inc's
            imports have a +x already.
    vpname  a + name given by a (defq) in a .vp file that a
            (defq) of another .vp file gives as well. The
            assembler reads the files of its jobs into the one
            environment, and a + name is put in as it is read:
            where the two are read by the one task, the second
            file's (defq) of it is of the first's value, a number
            where a name is to be. A prefix of the file's own.
    capture a function written where it is given to another
            of this file, or of a file it imports, that uses
            a name the other has as a parameter, or makes
            with a (defq). A name is found where a function
            is called from, so it is the other's that is had.

    A path that is a folder is the source files in it, at
    any depth. If no paths given on command line then will
    test files from stdin.
```
## cluster
```code
Usage: cluster [options]

    options:
        -h --help: this help info.
        -t --timeout ms: response timeout in milliseconds (default: 5000).
        -v --verbose: display per-node probe progress details.

    Probe kernel statistics and services across all known cluster nodes.
```
## cp
```code
Usage: cp [options] path1 path2

    options:
        -h --help: this help info.

    Copy file path1 to path2.
```
## ctf
```code
Usage: ctf [options] [path] ...

    options:
        -h --help: this help info.
        -v --verbosity num: verbosity level, default 0.
        -c --ctf: convert/upgrade font file to latest .ctf spec.
        -r --range num num: add start end char codes, default '().
        -p --pack num: with -c, the glyphs one after another from
            this char code, in the order of their codes, in one
            page, no codes between them that have none.
        -j --jobs num: max jobs per batch, default 1.

    Inspects and outputs information about ChrysaLisp Vector Font (.ctf)
    or OpenType/TrueType (.otf/.ttf) files. If no files are specified on the
    command line, file paths are read from stdin.
```
## curl
```code
Usage: curl [options] <url>

    options:
        -h --help: this help info.
        -i --include: include protocol response headers in output.
        -I --head: fetch headers only (HTTP HEAD).
        -s --silent: silent mode (suppress error/diagnostic messages).
        -X --request cmd: specify request command to use (GET, POST, HEAD).
        -H --header line: custom header to pass to server.
        -d --data str: HTTP POST data.
        -k --insecure: https, do not check who the server is.

    Fetch and display content from an HTTP URL.
```
## cwb
```code
Usage: cwb [options] file.cwb

    options:
        -h --help: this help info.
        -n --new size: an empty document of that size, 800x600, in
            place of what the file has, if there is one.
        -e --eval lisp: do this to it. The Lisp has board, the board
            of the document, lib/cwb/board.inc, and doc, the document,
            lib/cwb/doc.inc. Quote it for the shell.
        -s --script path: as -e, the Lisp is in a file.
        -p --pointers path: play a file of pointer events to the
            board, as a pen, a mouse and fingers would give them.
        -i --info: list what is in it.
        -o --out path: draw it to a picture, a .tga or a .cpm.
        -z --zoom num: the picture is that many times the size, 1.
        -b --back colour: the picture has that behind it, a number,
            0xffffffff is white. Default what the document has, which
            is nothing unless it was given one.
        -k --keep: do not save the file, whatever was done to it.

    Make, change, look at and draw a whiteboard document without a
    whiteboard. What is done is done in the order above, and then the
    file is saved if -n, -e, -s or -p changed it.

    A shape is an SVG path, text. Colours are 0xAARRGGBB.

        cwb -n 400x300 a.cwb -e "(cwb-add doc (cwb-shape (cwb-d-rect 20 20 200 120 12) :fill 0xffffd070))"
        cwb a.cwb -e "(cwb-add doc (cwb-text {Start} 60 80))" -i
        cwb a.cwb -o a.tga -z 2 -b 0xffffffff

    A line of a pointers file is the events of one moment, one or more,
    with ; between: id kind buttons x y. kind is mouse, pen, eraser or
    touch. buttons is 0 for up. # starts a note. Each line is a sixtieth
    of a second after the last, for what on the board moves by itself,
    and a line that is wait and a number is that many thousandths more.

        1 pen 1 100 100
        1 pen 1 180 140 ; 7 touch 1 400 300
        1 pen 0 180 140 ; 7 touch 0 400 300
        wait 500

    A hand that taps where there is nothing opens a palette there, as
    on the app's board, the right button of a mouse, or a finger.
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
        -w --width num: chunk width, default 8.
        -o --offset: toggle byte offset column, default :t.
        -c --chars: toggle chars column, default :t.

    If no paths given on command line
    then will dump stdin.
```
## echo
```code
Usage: echo [options] arg ...

    options:
        -h --help: this help info.
```
## edit
```code
Usage: edit [options] [path] ...

    options:
        -h --help: this help info.
        -j --jobs num: max jobs per batch, default 1.
        -c --cmd '...': commands to execute.
        -s --script path: file containing command to execute.
        -q --quiet: quiet mode, no output except from (edit-print).

    Command line text editor.

    The `edit-script` is compiled and executed in a custom environment
    populated with editing primitives.

    With the -c option your script commands will be auto wrapped into an
    `(defun edit-script () ...)` lambda, before execution.

    With the -s option your script is assumed to use advanced ChrysaLisp features,
    such as macros, and as such a simple wrapping will not suffice.
    So the assumption is that you will provide the `(defun edit-script () ...)`
    within the script file !

    If you specify both -c and -s, the -c option is compiled as if it was
    in front of the -s script ! So it could be used to set configuration or
    provide specific functions that the main script binds to etc.

    Available Commands:

    Search:     (edit-find pattern [:w :x :i]) -> :nil | buffer_found
                (edit-find-next) -> :nil | buffer
                (edit-find-prev) -> :nil | buffer
                (edit-find-add-next) -> :nil | buffer

    Cursors:    (edit-cursors) (edit-add-cursors) (edit-primary)

    Focus:      (edit-get-focus) (edit-set-focus [csr]) (edit-filter-cursors)

    Selection:  (edit-select-all) (edit-select-line) (edit-select-word)
                (edit-select-block) (edit-select-form) (edit-select-paragraph)
                (edit-select-ws-left) (edit-select-ws-right)
                (edit-select-bracket-left) (edit-select-bracket-right)
                (edit-select-home) (edit-select-end)
                (edit-select-top) (edit-select-bottom)
                (edit-select-left [cnt]) (edit-select-right [cnt])
                (edit-select-up [cnt]) (edit-select-down [cnt])

    Navigation: (edit-top) (edit-bottom) (edit-home) (edit-end)
                (edit-bracket-left) (edit-bracket-right)
                (edit-ws-left) (edit-ws-right)
                (edit-up [cnt]) (edit-down [cnt])
                (edit-left [cnt]) (edit-right [cnt])

    Mutation:   (edit-insert txt) (edit-paste txt) (edit-replace pattern)
                (edit-delete [cnt]) (edit-backspace [cnt])
                (edit-trim) (edit-sort) (edit-unique) (edit-upper)
                (edit-lower) (edit-reflow) (edit-split) (edit-comment)
                (edit-indent) (edit-outdent) (edit-cut) (edit-break)

    Properties: (edit-copy) -> txt
                (edit-get-text) -> txt
                (edit-get-primary-text) -> txt
                (edit-get-filename) -> txt

    Utilities:  (edit-split-text txt [cls]) -> (txt ...)
                (edit-join-text (txt ...) [cls]) -> txt
                (edit-print ...)
                (edit-eof?) -> :t | :nil
                (edit-eof [csr]) -> cnt
                (edit-sof [csr]) -> cnt
                (edit-cx) -> cx
                (edit-cy) -> cy

    Example - Numbering lines:

    edit -c
        "(until (edit-eof?)
            (edit-insert (str (inc (edit-cy)) ": "))
            (edit-down)
            (edit-home))"
        file.txt
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

        prefix default 
```
## fmt
```code
Usage: fmt [options] [path] ...

    options:
        -h --help: this help info.
        -j --jobs num: max jobs per batch, default 4.
        -w --write: overwrite files in-place, default :nil.
        -c --check: list the files that need formatting.
        -l --limit num: line length that brings pressure to
            break, default 80. VP assembler lines get half as
            much again. 0 keeps your line breaks, and only
            indents and tidies.

    Formats ChrysaLisp source code. Only white space between
    tokens is ever changed, so what the reader sees, and so
    what gets built, is the same before and after.

    The layout is made from the code alone. The line breaks
    inside a form are not kept, the form is laid out afresh.

    Indentation, with tabs, from the structure alone. A line
    is one tab in from the line its enclosing form opened on.
    Where several forms open on the one line, each is a tab
    further in than the one around it, so the indent shows
    which form owns a line. VP block forms, (vpif)
    (loop-start) and so on, indent the lines between them.

    A form is one line if it fits. A definition always has its
    body on lines of its own, as does a (cond) or (case) each
    of its clauses. A (when) (while) and the like is one line
    only with a single body form. An (if) keeps its test and
    then form on the opening line, and its else form too if
    there is just the one, else each else form has a line.

    The limit is pressure to break, not an order to. A line
    over it is broken where the structure has a place for it,
    at the outermost form that can be. A form with a body
    then has each body form on a line of its own, bindings
    break before a name, and are filled. A line with no such
    place is left, until it is half as long again, and is
    then broken between arguments.

    Tidying. One space between tokens, no trailing white
    space, no runs of blank lines, no line starts with a close
    bracket, and the file ends with one newline.

    What is kept from the source. Strings, comments and the
    lines they are on, blank lines, and the text of a
    (defq usage ...) form.

    The source scanners and the doc builder read a line at a
    time. A form they look for, (def-method) (dec-method)
    (import) (ffi) a VP instruction and the like, starts a
    line if and only if it did in the source, and is never
    broken, nor is the opening line of a definition. Comments
    right under such a line stay right under it, and the lines
    of a key map are kept.

    As a guard, the result must read as the same forms as the
    source, or the file is left alone.

    Restricts targets to unique .lisp, .inc, and .vp files. If
    no paths are given on the command line, paths are read from
    stdin.
```
## forward
```code
Usage: forward [options] [path] ...

    options:
        -h --help: this help info.
        -j --jobs num: max jobs per batch, default 1.

    Scan source files for a function or macro that is
    used above where it is defined, and for a function
    that calls itself.

    Such a use is not bound to the function as the code
    is read, the name is looked up each time it is run.
    In a module the name is not there to find. And a
    function that calls itself can run out of stack, a
    list is the stack to use, as (flatten) does.

    What is in a comment or a string is not looked at.

    If no paths given on command line
    then will test files from stdin.
```
## grab
```code
Usage: grab [options] file

    options:
        -h --help: this help info.
        -t --title str: the window whose title has this in it,
            default all of the screen.
        -f --find key=value: only the first view in it whose key
            is value, type=Radiobar, tip_text=snap, depth first,
            the part of it that can be seen.
        -w --wait num: seconds to wait for the window, as an app
            just launched has none yet, default 0.

    What is on the GUI's screen now, of a window, of a view in
    it, or of all of it, written to the file, as its ending
    says, .png or .cpm. It is read by the GUI from its back
    buffer, the last frame it drew, by a GUI of this network.
    The less there is of it the less it is to look at: a bar,
    not the window, if the bar is the question.
```
## grep
```code
Usage: grep [options] [pattern] [path] ...

    options:
        -h --help: this help info.
        -e --exp pattern: regular expression.
        -f --file: file mode, default :nil.
        -w --words: whole words mode, default :nil.
        -x --regexp: regexp mode, default :nil.
        -c --coded: encoded pattern mode, default :nil.
        -m --md: md doc mode, default :nil.
        -j --jobs num: max jobs per batch, default 1.
        -v --inverse: invert match, select non-matching lines, default :nil.
        -i --ignore-case: case-insensitive mode, default :nil.
        -n --line-number: prefix line numbers, default :nil.

    pattern:
        ^  start of line
        $  end of line
        !  start/end of word
        .  any char
        +  one or more
        *  zero or more
        ?  zero or one
        +? lazy one or more
        *? lazy zero or more
        ?? lazy zero or one
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
## hbook
```code
Usage: hbook [options] [path] ...

    options:
        -h --help: this help info.
        -j --jobs num: max jobs per batch, default 1.
        -t --tbits num: bit size for data tokens, default 8.
        -c --codebook path: codebook filename, default :nil.

    Scan files for Huffman frequency information, creates
    a merged tree of all the information.

    Optionally create and save a codebook for use with
    the static huffman library.

    If no paths given on command line
    then will take paths from stdin.
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
## huff
```code
Usage: huff [options] [file]

    options:
        -h --help: this help info.
        -t --tbits num: bit size for data tokens, default 8.
        -c --codebook path: codebook filename, default :nil.

    Compresses a file with Huffman codes, a block of
    65536 tokens at a time, each block with a code made
    for it. With a codebook, made by hbook, the code of
    the book is used for the whole file, and none is
    written; a token the book has no code for is an error.

    If no file is given, it reads from stdin.
    Output is written to stdout.
```
## imports
```code
Usage: imports [options] [path] ...

    options:
        -h --help: this help info.
        -j --jobs num: max jobs per batch, default 8.
        -w --write: write new file, default :nil.

    Scan for import statements in .vp, .inc, .lisp files,
    replacing any import lines with an optimal relative or
    absolute file path.

    If no paths given on command line
    then will take paths from stdin.
```
## includes
```code
Usage: includes [options] [path] ...

    options:
        -h --help: this help info.
        -j --jobs num: max jobs per batch, default 8.
        -d --defs defs: class definitions map, default :nil.
        -w --write: write new file, default :nil.

    Scan for needed includes in .vp files, optionally
    edits the file rewriting the include block.

    A file is needed for the classes whose methods are called,
    for the constants that are used, and for the inline functions
    and macros of a class.inc that are used. What such an inline
    needs in turn, its class.inc must include for itself.

    If no paths given on command line
    then will take paths from stdin.
```
## input
```code
Usage: input [options] action ...

    options:
        -h --help: this help info.
        -t --title str: the window whose title has this in it,
            default all of the screen.
        -f --find key=value: the first view in it whose key is
            value, type=Button, text=Save, depth first.
        -w --wait num: seconds to wait for the window, as an app
            just launched has none yet, default 0.

    actions, done in order. A place, x y, is from the top left of
    the window, or of the view found, and is its middle if left
    out. A view out of sight, scrolled away, is not clicked; turn
    the wheel over what it is in first, as a user would. The window
    of -t is put in front of the others first, as a click on it would.
    A place that is not on that window, outside it or under one kept
    in front, is not clicked: it says what window the place is on,
    and does nothing more:
        move x y: the mouse to there.
        click [x y], rclick [x y], dclick [x y]: a click of the
            left button, of the right, two of the left.
        drag x y x1 y1: the left button down at the one place,
            moved to the other and let go there.
        wheel way n: n steps of the wheel, where the mouse is,
            up, down, left or right.
        key name: a key down and up, by its name in
            lib/consts/scodes.inc, return, a, pagedown, with any
            of shift+ ctrl+ alt+ cmd+ before it.
        type text: the keys of the text, as the keyboard map of
            the user has them.
        finger n down x y, finger n to x y, finger n up: contact n
            of a touch panel lands, moves there, lifts. Many may be
            down at once, each its own n.
        pen down x y [barrel], pen to x y, pen up: the pen, its tip
            touching, with the button of its barrel held if said.
        eraser down x y, eraser to x y, eraser up: the other end of
            the pen.
        A move of a contact goes there in steps from where it was,
        if it was down by this command, so a line is drawn.

    Given to the GUI of this network, as if from its mouse and
    keyboard, so to the windows of any app, as they would be.
```
## key
```code
Usage: key [options] [name] [file]

    options:
        -h --help: this help info.
        -n --new: make a key of that name, 32 bytes at random.
        -u --use: use that key from now on: it is put in
            mesh_key, and the Net service ends its links and
            joins the machines that have it.
        -k --keep: keep the key in use by that name, one
            that was put in mesh_key by hand.
        -i --import: a key someone gave you, in a file of the
            host, into the keyring by that name.
        -x --export: say the key of that name, to give it to
            someone. Give it only to those you trust.
        -r --rename: the key of that name by the new name, the
            file given.
        -d --delete: forget the key of that name. If it is the
            one in use it stays in use.

    With no option, the keys this machine holds, each with its
    fingerprint, and the one in use. Two machines with the same
    fingerprint have the same key, and can join each other: say
    the fingerprint, never the key, to check.

    The keys are the files of keys/, the key in use mesh_key,
    the machine's own, never committed nor taken by a sync.
    A key is 64 hex digits, or any other text, a passphrase.
```
## launch
```code
Usage: launch app ...

    options:
        -h --help: this help info.

    Open each app on the node of the GUI of this system, as the
    Launcher does, desktop/eyes for apps/desktop/eyes/app.lisp,
    or a path to the .lisp of it. An app's windows are views of
    that node, so it is opened there, wherever this runs.
```
## link
```code
Usage: link [options] [host[:port] ...]

    options:
        -h --help: this help info.
        -l --listen [port]: listen for incoming TCP network link (default: 3333).
        -a --auto: auto-discovery mode (beacon when listening, discover when client).
        -m --mesh: link to the peers of whoever is linked to, with no discovery.
        -k --key: make a key for this machine, own, and use it, if it has none.
        -s --seal: seal the links to the hosts given, as one over the internet is.
        -v --verbose: verbose output.

    Start TCP network link driver/s.

    Network links:
        link -l 3333          ; Listen on port 3333 for incoming network links
        link -l 3333 -a       ; Listen on port 3333 and beacon for auto-discovery
        link -a               ; Auto-discover LAN peers and connect
        link -m 192.168.1.100 ; Connect to a peer, and to each peer it knows
        link 192.168.1.100    ; Connect to peer on default port 3333
        link 127.0.0.1:3333   ; Connect to peer on specified port
        link server.local     ; Connect to peer by DNS/mDNS hostname

    A machine does the first two of these by itself as it starts, the
    mesh command says how it stands. Every machine that does finds the
    others and has one link to each, none is on the way between two.

    A machine links only to machines with the same key, the file mesh_key
    at the root of the tree, each end proves it has it before the link
    carries anything. An install makes one, and link -k does if there is
    none, 64 hex digits. Put the same file on every machine of the
    network, by hand, it is never sent. The Net service sees the file
    change: it ends the links it has and joins those with the new key.
    With no key a machine has no links, it is off the network.

    A link to a host outside the private addresses, over the internet, is
    sealed: each message on it encrypted and signed. One on the LAN is
    not, and costs nothing more, unless -s says it is to be.

    If no host names given on command line and -l/-a/-m not passed,
    then names are read from stdin.
```
## lint
```code
Usage: lint [options]

    options:
        -h --help: this help info.
        -k --keep: leave the debug build, do not make the
            release build again after.
        -v --verbose: say what each step took.

    The lint of the VP source, all of it in the one go.

    The trace lint works out what each function really
    trashes, and says where that is not what its header has
    written down. It is only right on a debug build, so this
    makes one, 'make vp debug' then 'make apps debug', runs
    'files obj/vp/ | trace -i -l', and puts the builds
    back as they are with no mode, 'make vp', 'make apps'
    then 'make all boot'.

    Then 'relies -q', on the same debug build: where a
    function reads a register after a call that is declared
    to trash it, which the trace does not look for.

    Prints what the lint and the builds have to say, which
    is nothing when all is well, then a line to say so.
```
## lisp
```code
Usage: lisp [options] [path] ...

    options:
        -h --help: this help info.
        -r --repl ...: read code from remainder of command line into REPL.

    If no paths given on command line
    then will REPL from stdin.
```
## locals
```code
Usage: locals [options] [function] ...

    options:
        -h --help: this help info.
        -v --verbose: a line for every function that has
            locals, not only the sum.
        -t --top num: the functions that move most to and
            from the stack, that many of them, default 10.
        -c --cost num: what the methods called cost their
            callers, the worst that many of them: for each,
            the calls of it from functions with locals, the
            locals wanted after those calls, how many of
            them there is no register for that the call
            leaves alone and the caller does not use, and
            how many of those it is the call that keeps
            out: there would be a register if it trashed
            less. Then the registers it trashes that would
            each be a home for one more local, and at how
            many of those calls. To trash less is safe for
            every caller, and lint says if the code does.

    What the named locals of each VP function cost, and what
    of that a compiler that kept them in registers could save.
    It reads the instructions of a function as they were
    assembled, obj/vp/, which a build of the VP64 leaves,
    'make vp', and sees the function whole: where its frame
    is made and let go of, each place in the frame that is
    read or written, a local, which of them have their
    address taken, and, by where control goes, which are
    still wanted after each call.

    A local that never has its address taken could be kept
    in a register. One that is not wanted after any call
    could be in any. One that is has to be in a register the
    call leaves alone, or be put by for the call.

    With no function named, all there are.
```
## locks
```code
Usage: locks [options]

    options:
        -h --help: this help info.

    Print the recent lock service history.
```
## lz4
```code
Usage: lz4 [options] [file]

    options:
        -h --help: this help info.
        -w --window num: max window size, default 65536.

    Compresses a file using standard LZ4 Framed encoding.

    If no file is given, it reads from stdin.
    Output is written to stdout.
```
## make
```code
Usage: make [options] [all] [boot] [platforms] [doc] [it] [apps]
    [vp] [release] [debug] [validate] [test] [fmt]

    options:
        -h --help: this help info.
        -v --verbosity num: how much info, default 0.

    all:        include all .vp files.
    boot:       create a boot image.
    platforms:  for all platforms not just the host.
    docs:       scan source files and create documentation.
    vp:         only the VP64 and obj/vp/ outputs.
    it:         all of the above, and then a check of the include
                and import lists, what includes and imports would
                change is printed.
    apps:       only the apps !
    release:    release mode, no error checks.
    debug:      debug mode, with error checks.
    validate:   validate mode, debug and more checks as it runs.
    test:       test make timings.
    fmt:        format all the source files, with the fmt command.

    A mode goes with any build, and all that is built is built
    in it. With none it is debug for all, and the VP64 is in
    release: so for 'it', 'apps' and 'vp', and the others
    build this platform, in debug.
```
## mesh
```code
Usage: mesh [options]

    options:
        -h --help: this help info.
        -j --join: join this machine to the others on the network, now.
            It does by itself as it starts, this is to ask again.
        -t --to host: join by way of one machine, its name or address,
            for when the others are not found by themselves.
        -k --key: make a key, own, and use it, if there is none. An
            install has made one.

    The machines of a network that have the same key join into one, by
    themselves. This says how it stands.

    With no options, say how it stands: if this machine has a key, if it
    has joined, and each machine there is, how many nodes it has, and
    what it is. If something looks wrong it says what to look at.

    A machine has a key from its install, its own, and is by itself. To
    join it to the others take their key into its keyring and use it,
    key -i and key -u, the key command. It sees the key change, and
    joins. With no key in use it is off the network.

    Of the sessions of one machine, a desktop and a terminal say, the
    first to start is its member of the mesh. Another takes its place
    when it has gone.

        mesh              ; who is there ?
        mesh -t address   ; join by way of that machine

    docs/intro/mesh.md is the guide.
```
## mic
```code
Usage: mic [options]

    options:
        -h --help: this help info.
        -s --seconds num: how long to listen, default 3.
        -o --out path: what was heard kept in a sound file, .wav,
            default tests/scratch/mic.wav.
        -p --play: what was heard played back.
        -e --echo file: the sound file played on the speaker while
            the microphone is heard, from its second second, for as
            long as it lasts and 2 more, and how loud each second was
            said: the echo canceller of this machine tried alone. The
            room quiet, what is heard after the first second should
            be near what it was in it.

    Listen to the microphone of the Audio service of this machine, and
    say what was heard: how long, how loud at its loudest and on the
    whole. What is heard is 16 bit mono at 16000 a second. It is a sound
    pipeline, lib/audio/stages.inc, the microphone, how loud, to the
    file; and to play it, the file to the speaker. A desktop that is
    never seen, -hidden, hears a tone of 440 a second, not a device.

    On macOS the program the host runs under, the terminal, must be let
    use the microphone, or it hears nothing.
```
## mp4
```code
Usage: mp4 [options] [path] ...

    options:
        -h --help: this help info.
        -s --samples num: the first num samples of the video track
            too, where each is, how long, when, and which are keys.

    What is in a film of boxes, .mp4 .m4v .mov: each track, its
    kind and codec, its size or its sound, and how many samples
    it has; and of the first video track, the frames a second
    its times come to and how many keys. A track whose edit list
    starts it later says from what time of its own, and after how
    long of nothing.

    If no paths given on command line
    then paths are read from stdin.
```
## mv
```code
Usage: mv [options] path1 path2

    options:
        -h --help: this help info.

    Move file path1 to path2.
```
## nettest
```code
Usage: nettest [options] [url|host] [port]

    options:
        -h --help: this help info.

    Simple HTTP / Net service test.
    Examples:
        nettest http://example.com/
        nettest http://httpbin.org/get?msg=hello+world
```
## nodes
```code
Usage: nodes [options]

    options:
        -h --help: this help info.
        -a --add num: start num more nodes on this machine,
            linked to this node and to each other.
        -g --gui num: start num more nodes, each a GUI desktop.
        -t --tui num: start num more nodes on the TUI host,
            which is the lighter, it has no GUI.
        -s --shape name: add a network of that shape, hung from this
            node: full, ring, star, tree, mesh or cube. It is given a
            name to stop it by.
        -n --num cnt: how many nodes the shape has, with this one, or
            how wide a mesh or a cube is. Sized to the machine if not
            given.
        -o --own: with -s, the shape is a system of its own. It has a
            system id that is not this machine's, this node is not
            one of it, and one link from here is the way in.
        -x --stop name: stop a network that was added with -s, all of
            its nodes at once, or all for every one there is.
        -i --info: this node's process id, and the processors
            and memory of its machine.

    List the nodes known to this node, and the networks added by name.

        nodes -s ring -n 8    ; a ring of 8, this node one of them
        nodes -s cube -n 2 -o ; a cube of 8, a system of its own
        nodes                 ; the nodes, and the networks
        nodes -x k3f9         ; stop that ring
```
## null
```code
Usage: null [options]

    options:
        -h --help: this help info.
```
## onslaught
```code
Usage: onslaught [options]

    options:
        -h --help: this help info.
        -s --state: print the full game state.
        -k --keys num: set the held control keys mask.
        -b --bot secs: let the bot play for secs seconds.
        -q --quit: quit the game.
        -i --id: print the mailbox id of the game's service.
        -m --mbox id: the game to play, by the mailbox id of
            its service, as -i prints it.

    Remote play a running Onslaught game, on any node,
    via its @Onslaught service.

    That name only finds a game on this machine. To play
    one on another machine, get its id there with -i, and
    give it here with -m.

    Key mask bits: up 1, down 2, left 4, right 8, fire 16.

    With no options prints a one line summary.
```
## patch
```code
Usage: patch [options] file_a [file_b]

    options:
        -h --help: this help info.
        -s --swap: swap sources.

    Patch text file a with text file b.

    If no second file is given it will
    be read from stdin.
```
## rack
```code
Usage: rack [options] "command line"

    options:
        -h --help: this help info.
        -b --build: make, and make all boot, on each machine first, in a
            session before the one that runs the command.
        -l --leave machines: machines to bring level and not run on, as
            sync lists them, arm64/Linux, with a , between.
        -x --skip machines: machines not to touch at all, not brought
            level and not run on, a guest's in the mesh say, the same way.
        -o --only ids: the only machines to touch, the start of the
            system id of each, mesh lists them, with a , between: one's
            own rig, with machines of others in the mesh.
        -d --delete paths: files to remove on the other machines, with
            a : between.
        -u --user name: the sessions are that user's, a folder of usr/,
            and not whoever last signed on to each machine.
        -g --gui: each session is a desktop, its GUI up before the
            command is run, on the machine's own screen.
        -v --verbose: every line each session says, as it says
            it, not only those of how the tests went.

    Run a command line on every machine of the mesh. Each machine that
    takes a sync, sync -a, is first made the same as this one. Then each,
    and this one, starts a session of its own, new, sized to itself, runs
    the command, and the session ends. Command lines with a ; between
    are run one after another in the one session.

    A line comes back for each machine.

        rack "tests -a"     ; every test, on every machine
        rack -b tests       ; after a change to the VP code
```
## relies
```code
Usage: relies [options] [function] ...

    options:
        -h --help: this help info.
        -v --verbose: every place, not only the sums by
            what is called.
        -q --quiet: every place and no more, nothing if
            there is none. For lint.
        -s --saves: the other way round: where a register is
            put by before a call and brought back after it,
            pushed and popped, or written to the stack and
            read back, and the call is not declared to trash
            it. Such a save was needed when what is called
            trashed more, and is for nothing now.

    Where a function goes by more than is declared of what it
    calls: a register it reads after a call that is declared
    to trash it, and does not give it.

    What a method trashes is in its declaration, and lint
    says that is what the code may do. It is all of the ways
    through the method. Code that calls it may know which way
    it will go, a push to an array that was just given the
    room, and count on a register the short way leaves. That
    works, and nothing checks it: what is called can be
    changed inside what it declares, and what calls it is then
    wrong with nothing to say so.

    This reads each function as it was assembled, obj/vp/,
    'make vp', and works out which registers are still wanted
    after each instruction, backwards from where it ends, by
    what each instruction reads and writes. A call reads what
    its method takes and writes what it gives. A register
    wanted after a call, that the call is declared to trash
    and does not give, is such a place.

    Two things called are left out. What does not come back,
    :sys_pii :exit. And a walk that calls back to what called
    it, :view :forward_tree: it is declared to trash every
    register, it can not know what it is given to call, and
    what gives it the callbacks keeps in registers what they
    work on. That is its own affair and not the walk's.

    To be rid of one, the long way through what is called
    puts by the registers it uses and brings them back, and
    then it is declared to trash less, 'trace -w'.

    With no function named, all there are.
```
## repeat
```code
Usage: repeat [options] command_line

    options:
        -h --help: this help info.
        -c --count num: count, default 10.

    Repeat run command line.
```
## rle
```code
Usage: rle [options] [file]

    options:
        -h --help: this help info.
        -t --tbits num: bit size for data tokens, default 8.
        -r --rbits num: bit size for run length tokens, default 8.

    Compresses a file using Run-Length encoding.

    If no file is given, it reads from stdin.
    Output is written to stdout.
```
## rm
```code
Usage: rm [options] [path] ...

    options:
        -h --help: this help info.

    If no paths given on command line
    then paths are read from stdin.
```
## save
```code
Usage: save [options] [path] ...

    options:
        -h --help: this help info.
        -s --stdout: pass through, default :nil.

    Read from stdin, write to all given paths,
    optionally write to stdout.
```
## sdir
```code
Usage: sdir [options] [prefix]

    options:
        -h --help: this help info.
```
## sed
```code
Usage: sed [options] [path] ...

    options:
        -h --help: this help info.
        -e --expression pattern: search pattern.
        -r --replace string: replacement string, default "".
        -g --global: replace all occurrences.
        -w --words: whole words.
        -x --regexp: treat pattern as regular expression.
        -i --ignore-case: case-insensitive matching.

    Stream editor. Reads from stdin if no files specified.
    Writes to stdout.
```
## shader
```code
Usage: shader [options] file

    options:
        -h --help: this help info.
        -t --target name: what to show, default glsl.
            glsl    GLSL text, for OpenGL and WebGL.
            msl     Metal Shading Language text, for Apple.
            spirv   a SPIR-V module, for Vulkan, as a listing.
            vp      VP assembler source, the native code back end.
                    Of a pixel shader, a vertex shader, or each
                    function of a file of functions for Lisp.
            cpu     the Lisp the CPU back end runs, of any of them.
            tree    the checked, typed tree the back ends are given.
        -v --vertex: the vertex shader that goes with every
            fragment shader, for msl and spirv.
        -p --pair file: the other shader of a pair, a vertex shader
            and a pixel shader that go together, in either order, for
            glsl, msl and spirv. Both halves are shown. With -o they
            are written to the file's name with .vert and .frag on.
        -o --out file: write it to a file. A spirv module is
            then written as the binary a driver, or spirv-dis,
            takes, not as a listing.

    Compiles a shader in the shader language, and shows what
    it is compiled to, or writes it to a file for use outside
    ChrysaLisp. See docs/ai_digest/shader_language.md.

    shader apps/demos/surface/surface.shader
    shader -t vp lib/gpu/shaders/mesh_vertex.shader
    shader -t msl -o surface.metal apps/demos/surface/surface.shader
    shader -t spirv -o surface.spv apps/demos/surface/surface.shader
    shader -p lib/gpu/shaders/mesh_lit.shader lib/gpu/shaders/mesh_vertex.shader
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
        -s --sep separator: default {	 ,}.
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
## storage
```code
Usage: storage [options] [path]

    options:
        -h --help: this help info.
        -j --join: make this machine a member of the store of
            its mesh, and start it. A store is made for it
            in storage/store.img, of 4096 MB at most, which
            takes only what is put in it, and its id is made
            from the mesh key, so every machine with that
            key is of the one store. From then on it starts
            by itself, whenever this machine is up. -m, -i
            and -k say otherwise. It says what it joined
            and the other members, who keep what is put.
        -f --forget: this machine no longer starts its
            store by itself. The store is left as it is.
        -o --on machine: the store of another machine, by the
            start of its system id as mesh lists it, 90D57181:
            looked at, put to or got from as a client, this
            machine no member of it, and none of it kept here.
        -L --travels on|off: this machine is one that is
            taken away from the others, a laptop, or is not.
            With -j, or of a member. One that travels pins
            the area of its user, usr/<user>/, as its store
            starts, so it has all of it when it is away.
        -s --start image: as -j, of the store in that file
            of the host, or disk.
        -m --make megabytes: with -s or -j, if there is no
            store there, make one that big.
        -i --id name: with -m, the id of the new store. The
            mesh's own if it is not said, or store.
        -F --format: with -s and -m, make a store of what is
            there even if it is something else, a disk.
        -k --copies num: with -m, how many members keep a
            block, default 3. With a path and no -s, how
            many are to keep the blocks of that object, or
            of every object whose path begins so: more for
            what must not be lost or is read a lot. 0 is as
            many as the store says.
        -q --query form: send a gopher to path with that
            function, in double quotes,
            "(lambda (state data at count) ...)", which gives
            (state next). What it comes back with, and the
            members it went to.
        -M --members: who the members are, which are
            there now, and what and where each is. With
            +id as path, that one is joined.
        -e --retire id: the member of that id retired, and
            the members said, as -M does.
        -x --stop: stop the service.
        -z --resize megabytes: the store of this member put on
            a volume of that many MB, bigger, or smaller if what
            it has fits: a copy on this machine, beside it, the
            size in its name, storage/store.8192MB.img, and the
            old one gone. This machine starts its store from the
            new one from now on.
        -l --list: the objects whose paths begin with path,
            all of them if none is given.
        -p --put file: path is set to what is in that file
            of the host.
        -g --get file: what path is, to that file of the host,
            to stdout if the file is -.
        -d --delete: there is no object of that path from now.
        -w --sweep: let go of the blocks no object is made of.
        -c --check: say which blocks are not what their names
            say. -C to let go of them too.
        -T --tree dir: every file under that folder of the
            host is put, as a stream, at path and its own
            path, and then each is read back as a stream and
            compared with the file. obj, deps and folders
            whose names begin with a dot are not gone in to,
            and files whose names begin with a dot are not put.
        -V --verify dir: the reading back and comparing of
            -T only, of what was put before, or elsewhere.
        -P --pin: this system holds all of what begins with
            path. -U no longer.
        -n --snapshot name: a snapshot of the objects whose
            paths begin with path, all if none is given:
            snapshots/name/ and their paths, no block copied.
        -N --snapshots: the snapshots there are.
        -R --restore name: the objects whose paths begin
            with path are as they were in that snapshot.
            Those made since are left, and said.
        -D --drop name: the snapshot is gone, its blocks let
            go of by a sweep if nothing else is made of them.
        -t --scene [name]: the scenes there are, in
            tests/storage/, and what each is for. With a
            name, that one is run: a store of its own is
            started, a thing done to it, what happens said
            and what came of it checked. -r file records it
            too.
        -r --record file: what every member of the store does,
            a line each, to that file of the host, - for
            stdout: the time, the member, what was done and
            what to. For -S seconds, 60 if not said.
        -S --secs secs: how long -r records.
        -G --group name: the group of the members -r
            records, *Storage if not said, a test's.

    The Storage service of this system, an object store. With
    no option, what the store is and what is in it.
```
## symbols
```code
Usage: symbols [options] [name] ...

    options:
        -h --help: this help info.
        -l --list: list the symbols, each with its code.
        -p --picture file: a picture of the symbols, .tga or
            .cpm, from lib/font/symbol_set.inc as it is now,
            before the fonts are made. A block for each theme,
            in the order of the themes, Symbols, Light, Bold,
            Sharp: a row at the size of a toolbar, as close as
            on one, then the same, big. Only those whose names
            begin with a name given, media_ say, else all.

    Make the symbol fonts, fonts/Symbols*.ctf, one for each theme, and
    the names of the symbols, lib/consts/symbols.inc, from the symbols
    of lib/font/symbol_set.inc.

    A theme is the same symbols with another weight of stroke, and other
    ends and corners. Run it after a symbol is changed or added.
```
## sync
```code
Usage: sync [options]

    options:
        -h --help: this help info.
        -a --accept: this machine will take a sync, till it is told
            not to or its session ends.
        -x --stop: this machine will no longer take a sync.
        -t --to id: make that machine's tree the same as this one's.
            The start of its id, as the list shows it, or all.
        -c --check: with -t, say what differs and change nothing.
        -d --delete: with -t, remove what is there and not here.
        -r --root path: the root of the tree, default the system's own.
        -v --verbose: name each file.

    Make the files of another machine the same as the files of this one,
    over the links between them. Only what differs is sent. What the
    .gitignore of the tree leaves out is not sent, or removed.

    A machine only takes a sync if it was told to, sync -a, and then
    only from itself, from the machines named in the file sync_from at
    its root, a system id a line, its rig, and by the herd: a tree that
    is exactly at a waypoint, with no .git, from one at a newer one, see
    the waypoint command. From any other it is refused.

    With no options, list the machines that will take one.

        sync -a           ; on the machine to be updated
        sync              ; on this one, who will take a sync ?
        sync -t all -c    ; what would change on them
        sync -t D649      ; send it to the one
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
## tests
```code
Usage: tests [options] [path] ...

    options:
        -h --help: this help info.
        -m --match str: only the modules with str in their path.
        -l --list: list the modules, do not run them.
        -v --verbose: show every test, not just the failures.
        -f --frames: record stack frames, so an error says what
            was running. Slower.
        -j --jobs num: max modules per batch, default 1.
        -c --counts: end with a line of counts, not the summary.
            The task of a batch is run with this.
        -a --all: run every module, whatever the cache has.
        -s --stale: list the modules that would be run, run none.
        -w --watch: say at the end the longest a task was kept
            from running on each node, while the modules ran.
            One that does not give way for a second or two is
            why a test with a deadline fails now and then.
        -e --leaks: run each module alone, one after another, in
            a task of its own on this node, twice, and fail one that
            leaves the node holding more after its second run than
            before it, once what a task's end lets go of is let go
            of, (kernel-stats :t): what grows each time is a leak,
            what is made once and kept is not. Slower.

    Run the unit tests, tests/<category>/test_<name>.lisp, or
    just the module paths given.

    The modules run in parallel, a batch to a task, over the
    nodes. If they all fit in one batch they run in this task,
    one after another, so a large -j is a serial run.

    A module in tests/solo/ changes the network, so those are
    run one at a time, after all the others. One in tests/release/
    is run only with -a, for a release, after those.

    The results are a cache. A module is run again only when
    something it stands on has changed, what it imports, a file or
    a command it names, the boot image or a host program. If not,
    its counts are as they were. -a runs them all, for a release.

    Prints the failures and a summary.
```
## time
```code
Usage: time [options]

    options:
        -h --help: this help info.
        -s --stdout: pass through, default :nil.

    Time the duration of the stdin stream.

    Print result to stderr.
```
## tocpm
```code
Usage: tocpm [options] [path] ...

    options:
        -h --help: this help info.
        -f --format 1|8|12|15|16|24|32: pixel format, default 32.
        -r --rle: enable run-length encoding, default :nil.
        -l --lz4: enable lz4 compression, default :nil.

    Load the images and save as .cpm images.

    If no paths given on command line
    then paths are read from stdin.
```
## toflm
```code
Usage: toflm [options] [path] ...

    options:
        -h --help: this help info.
        -f --format 1|8|12|15|16|24|32: pixel format, default 32.
        -n --name path: output film filename, default film.flm.
        -r --rate fps: frames a second, default 30.
        -c --convert: the films named, of an older version, made
            this version in place: each a packet a frame, the rate
            in the header.

    Convert images to a .flm animation.

    If no paths given on command line
    then paths are read from stdin.
```
## trace
```code
Usage: trace [options] [function_name] ...

    options:
        -h --help: this help info.
        -v --verbosity num: how much info, default 0.
        -l --lint: lint documented vs calculated trace.
        -i --integrity: check register and instruction integrity.
        -w --write: write back calculated trashes to source
            files on mismatch, to the comment of the function
            and to the declaration of the method.

    Calculate and trace active transitive register clobber state for
    virtual methods and static functions. Analyses compiled instructions
    directly via symbolic execution and traces live registers.

    Shaders assembled as they ran, lib/gpu/jit/, are left out.
```
## unhuff
```code
Usage: unhuff [options] [file]

    options:
        -h --help: this help info.
        -c --codebook path: codebook filename, default :nil.

    Decompresses a file huff made, with the codebook it
    was made with, if it was.

    If no file is given, it reads from stdin.
    Output is written to stdout.
```
## unique
```code
Usage: unique [options] [line] ...

    options:
        -h --help: this help info.

    If no lines given on command line
    then will read lines from stdin.
```
## unlz4
```code
Usage: unlz4 [options] [file]

    options:
        -h --help: this help info.
        -w --window num: max window size, default 65536.

    Decompresses a standard LZ4 Framed encoded file.

    If no file is given, it reads from stdin.
    Output is written to stdout.
```
## unrle
```code
Usage: unrle [options] [file]

    options:
        -h --help: this help info.
        -t --tbits num: bit size for data tokens, default 8.
        -r --rbits num: bit size for run length tokens, default 8.

    Decompresses a file using Run-Length encoding.

    If no file is given, it reads from stdin.
    Output is written to stdout.
```
## unzip
```code
Usage: unzip [options] archive [name ...]

    options:
        -h --help: this help info.
        -l --list: what is in it, not taken out.
        -d --dir folder: where to put what is taken out, default
            where it is run.

    Take what a .zip holds out of it, each to its name in the
    folder, or only those named, by the name in the zip or the
    last part of it. Each is checked against the sum it was
    made with. A name that would put a file outside the folder,
    one that begins with / or has .. in it, is not taken out.
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
## waypoint
```code
Usage: waypoint [options]

    options:
        -h --help: this help info.
        -c --check: say where the tree is, and change nothing.

    Note the waypoint this machine's tree is at, the file waypoint at
    its root: the commit git has it at, when that was made, read from
    .git, and the top of its tree of hashes. Run it when the tree is
    exactly a commit of master or of a release, after a git pull; it is
    not noted if it is not.

    Only a tree that is exactly at its waypoint takes part in the herd,
    a sync to or from a machine that is not one of its own: a tree with
    a change of its own since is not, and nothing is pushed to it or
    taken from it. Pull and note the waypoint again and it is back.
    lib/sync/sync.inc, Who takes a sync.

    It is exactly the commit if git's index is the commit's tree, each
    file is the size, time and mode the index has, and there is no file
    git does not know of and is not told to leave out. A file only
    touched is taken as changed till git status has looked at it.

    If this machine takes a sync, sync -a, the waypoint is offered to
    the others that do, and goes round the herd by itself.

    With -c, the waypoint it was last at and if it is still exactly that.
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
## widgets
```code
Usage: widgets [options]

    options:
        -h --help: this help info.
        -t --title str: the window whose title has this in it,
            default all of the screen.
        -f --find key=value: only the first view in it whose
            key is value, type=Button, text=Save, depth first.
        -d --depth num: the levels of children, default all; a
            view at the last says how many it has.
        -k --keys key,key: only these properties, as well as
            the type and bounds of each, default all.
        -w --wait num: seconds to wait for the window, as an app
            just launched has none yet, default 0.
        -a --all: every property, the working state of each view
            too, which is left out unless asked for: caches, a
            drag, a pressed state, mailboxes.
        -r --raw: the tree as (ui-save) writes it, not a line a
            view.

    The tree of widgets of a window on the GUI's screen, or of
    the screen, as text, from the GUI of this network: a view a
    line, as deep in as it is, its type, its bounds in its
    parent, and its properties, key=value. A value with bytes
    that are not text in it, a mailbox, is said as <n bytes>.
```
## zip
```code
Usage: zip [options] archive path ...

    options:
        -h --help: this help info.

    Make a .zip of files: each path a file, or a folder and all
    in it, its files named in the zip as they are named here.
    Each is packed by DEFLATE, or stored as it is when that is
    no smaller. The archive is made new, what was there before
    is gone. unzip takes them out again.
```
