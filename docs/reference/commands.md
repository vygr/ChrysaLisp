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

    If no paths given on command line
    then paths are read from stdin.
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
    held    a (create-stdio) that is not kept in a name, by a
            (defq). It is let go of as soon as it has been
            looked at, its streams say they are stopping, and
            what is written after may not be read.
    capture a function written where it is given to another
            of this file, that uses a name the other has as
            a parameter. A name is found where a function is
            called from, so it is the other's that is had.

    If no paths given on command line
    then will test files from stdin.
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

    Compresses a file using static or adaptive Huffman coding.
    If a codebook is provided then it will load that model for
    static operation.

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
## link
```code
Usage: link [options] [host[:port] ...]

    options:
        -h --help: this help info.
        -l --listen [port]: listen for incoming TCP network link (default: 3333).
        -a --auto: auto-discovery mode (beacon when listening, discover when client).
        -m --mesh: link to the peers of whoever is linked to, with no discovery.
        -k --key: make a key for this machine, the file mesh_key, if it has none.
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
        -k --key: make a key, the file mesh_key, if there is none. An
            install has made one.

    The machines of a network that have the same key join into one, by
    themselves. This says how it stands.

    With no options, say how it stands: if this machine has a key, if it
    has joined, and each machine there is, how many nodes it has, and
    what it is. If something looks wrong it says what to look at.

    A machine has a key from its install, its own, and is by itself. To
    join it to the others copy their mesh_key to the same place on it,
    by hand. It sees the file change, and joins. Take the file away and
    it is off the network till it is put back.

    Of the sessions of one machine, a desktop and a terminal say, the
    first to start is its member of the mesh. Another takes its place
    when it has gone.

        mesh              ; who is there ?
        mesh -t address   ; join by way of that machine

    docs/intro/mesh.md is the guide.
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
        -d --delete paths: files to remove on the other machines, with
            a : between.
        -u --user name: the sessions are that user's, a folder of usr/,
            and not whoever last signed on to each machine.

    Run a command line on every machine of the mesh. Each machine that
    takes a sync, sync -a, is first made the same as this one. Then each,
    and this one, starts a session of its own, new, sized to itself, runs
    the command, and the session ends.

    A line comes back for each machine.

        rack "tests -a"     ; every test, on every machine
        rack -b tests       ; after a change to the VP code
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

    shader lib/gpu/shaders/raymarch.shader
    shader -t vp lib/gpu/shaders/mesh_vertex.shader
    shader -t msl -o raymarch.metal lib/gpu/shaders/raymarch.shader
    shader -t spirv -o raymarch.spv lib/gpu/shaders/raymarch.shader
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
        -s --start image: start the service of this system on
            the store in that file of the host, or disk.
        -m --make megabytes: with -s, if there is no store
            there, make one that big.
        -i --id name: with -m, the id of the new store,
            default store.
        -F --format: with -s and -m, make a store of what is
            there even if it is something else, a disk.
        -k --copies num: with -m, how many members keep a
            block, default 3.
        -q --query form: send a gopher to path with that
            function, in double quotes,
            "(lambda (state data at count) ...)", which gives
            (state next). What it comes back with, and the
            members it went to.
        -M --members: who the members are, and which are
            there now. With +id or -id as path, one is
            joined or retired.
        -x --stop: stop the service.
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
            whose names begin with a dot are not gone in to.
        -V --verify dir: the reading back and comparing of
            -T only, of what was put before, or elsewhere.
        -P --pin: this system holds all of what begins with
            path. -U no longer.

    The Storage service of this system, an object store. With
    no option, what the store is and what is in it.
```
## symbols
```code
Usage: symbols [options]

    options:
        -h --help: this help info.
        -l --list: list the symbols, each with its code.

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

    A machine only takes a sync if it was told to, sync -a.

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

    Run the unit tests, tests/<category>/test_<name>.lisp, or
    just the module paths given.

    The modules run in parallel, a batch to a task, over the
    nodes. If they all fit in one batch they run in this task,
    one after another, so a large -j is a serial run.

    A module in tests/solo/ changes the network, so those are
    run one at a time, after all the others.

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
            files on mismatch.

    Calculate and trace active transitive register clobber state for
    virtual methods and static functions. Analyses compiled instructions
    directly via symbolic execution and traces live registers.

    Shaders assembled as they ran, lib/gpu/jit/, are left out.
```
## unhuff
```code
Usage: unrle [options] [file]

    options:
        -h --help: this help info.
        -t --tbits num: bit size for data tokens, default 8.
        -c --codebook path: codebook filename, default :nil.

    Deompresses a file using static or adaptive Huffman coding.
    If a codebook is provided then it will load that model for
    static operation.

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
