: \ source nip >in ! ; immediate
\ the line above lets us do comments like this one !

\ old tib words
: tib #in 2@ drop ;
: #tib #in ;

\ returns the depth of the stack.
: depth		\ ( -- n )
	s0 @ dsp@ -
	8 -		\ adjust because s0 was on the stack when we pushed dsp
	4 /		\ a cell has four bytes
;

\ dictionary search and compiling words
: hide parse-name (find) hidden ;
: here dp @ ;
: allot dp +! ;
: ' word-name find drop ;
: ['] ' lit, , ; immediate
: char parse-name drop c@ ;
: >body xt_body + @ ;
: [char] char lit, , ; immediate
: literal lit, , ; immediate
: 2literal swap lit, , lit, , ; immediate

\ to define an anonymous word, and push its xt on the stack, use :noname ... ;
: :noname
	0 0 header,	\ create a word with no name - we need a dictionary header because ; expects it
	here		\ current dp value is the address of the codeword, ie. the xt
	]		\ go into compile mode
;

\ set compile actions
: >compile	\ ( xt -- )
	latest @ >cfa xt_compile + !
;
: inline ['] inline, >compile ;

\ standard words for booleans.
: true -1 ; inline
: false 0 ; inline
: not 0= ; inline

\ cell and char converstion words
: cells	4 * ; inline
: cell+ 4+ ; inline
: chars ; inline
: char+ 1+ ; inline
: aligned 3 + 3 invert and ;

\ some character constants
: '\n' 10 ; inline
: bl 32 ; inline
: ':' [ char : ] literal ; inline
: ';' [ char ; ] literal ; inline
: '(' [ char ( ] literal ; inline
: ')' [ char ) ] literal ; inline
: '"' [ char " ] literal ; inline
: 'a' [ char a ] literal ; inline
: '0' [ char 0 ] literal ; inline
: '-' [ char - ] literal ; inline
: '.' [ char . ] literal ; inline
: ''' [ char ' ] literal ; inline
: '>' [ char > ] literal ; inline
: '<' [ char < ] literal ; inline

\ parseing words
: source-id sourcefd @ ;

: ischar? 3 pick = ;
: isnotchar? ischar? 0= ;

: parse		\ ( del "name" -- c-addr len )
	source >in @ /string over >r
	['] isnotchar? xt-skip
	2dup 1 min + source drop - >in !
	drop r> tuck - rot drop
;

: word		\ ( del "name" -- c-addr )
	source >in @ /string
	['] ischar? xt-skip over >r
	['] isnotchar? xt-skip
	2dup 1 min + source drop - >in !
	drop r> tuck - rot drop
	wordbuf 2dup c!
	1+ swap cmove
	wordbuf
;

\ compile time print
: .( ')' parse type ; immediate

\ words for manipulating base.
: decimal 10 base ! ;
: hex 16 base ! ;

\ a few standard emit words
: cr '\n' emit ;
: space bl emit ;

\ what type of word is it
: ?hidden	\ ( word -- f )
	[ h_nsize ] literal + c@
	[ f_hidden ] literal and
;
: ?immediate 	\ ( word -- f )
	[ h_nsize ] literal + c@
	[ f_immed ] literal and
;

\ makes a recursive call to the current word that is being compiled.
: recurse
	latest @	\ latest points to the word being compiled at the moment
	>cfa compile,	\ get the codeword and compile it
; immediate

\ constants, variables and arrays
: constant	\ ( x "name" -- )
	create ,
	does> @
;
: 2constant	\ ( n1 n2 "name" -- )
	create , ,
	does> 2@
;

: variable	\ ( "name" -- )
	create 1 cells allot
;

: 2variable	\ ( "name" -- )
	create 2 cells allot
;

: cvariable	\ ( "name" -- )
	create 1 allot
;

: buffer:	\ ( n "name" -- )
	create allot
;

\ pad and temp buffers settings, just place it above the current dictionary entry
: pad here linesize + ;
: tstr1 pad linesize + ;
: tstr2 pad linesize 2* + ;
: tstr3 pad linesize 3 * + ;

\ control words mainly imediate
: location	\ ( -- b_addr )
	here 4-			\ push location of the branch offset
;

: <resolve	\ ( b_addr -- )
	here -			\ calculate the offset from the address saved on the stack
	here 4- !		\ patch the offset
;

: resolve>	\ ( b_addr -- )
	dup
	here swap - 4-		\ calculate the offset from the address saved on the stack
	swap !			\ patch the offset
;

: if	\ ( f -- )
	postpone 0branch
	location
; immediate

: ahead	\ ( -- )
	postpone branch
	location
; immediate

: unless	\ ( f -- )
	postpone 0=
	postpone if
; immediate

: then
	resolve>
; immediate

: else
	postpone branch
	postpone then
	location
; immediate

: begin
	here			\ save location on the stack
; immediate

: again
	postpone branch
	<resolve
; immediate

: until	\ ( f -- )
	postpone 0branch
	<resolve
; immediate

: while	\ ( f -- )
	postpone if
	swap			\ swap for the begin location
; immediate

: repeat
	postpone again
	postpone then
; immediate

: thens		\ ( 0 ... ... b_addr -- )
	begin			\ resolve remaining forward branches
		?dup
	while
		postpone then
 	repeat
;

: list_thens	\ ( l_addr -- )
	@ ?dup
	if
		begin
			dup @ swap	\ ( nba ba )
			postpone then	\ ( nba )
			dup false =
		until
		drop
	then
;

create list_leave 11 cells allot	\ cope with 10 levels deep do loops.
list_leave dup !

: push_leave_list
	list_leave @ 4+
	dup list_leave !	\ new list for leave branches
	false swap !		\ mark bottom of list leave branches
;

: pop_leave_list
	list_leave @ 4-
	list_leave !		\ restore previous leave list
;

: do_setup
	push_leave_list
	postpone 2>r
	postpone begin
;

: do	\ ( limit init -- )
	false			\ mark bottom of stack of forward branches
	do_setup
; immediate

: ?do	\ ( limit init -- )
	false			\ mark bottom of stack of forward branches
	postpone 2dup
	postpone =
	postpone if
		postpone 2drop
	postpone else
	do_setup
; immediate

: leave
	postpone branch
	list_leave @
	dup @ location !
	location swap !		\ add branch to current leave list
; immediate

: ?leave
	postpone if
	postpone leave
	postpone then
; immediate

: unloop
	postpone 2rdrop
; immediate

: loop_end
	postpone -rot		\ ( f l v' )
	postpone 2>r		\ ( f )
	postpone until
	list_leave @
	list_thens		\ resolve any leave forward branches
	pop_leave_list
	postpone 2rdrop		\ ( r: l v -- r: )
	thens			\ reslove any loop precondition branches
;

: loop		\ ( -- )
	postpone 2r>		\ ( l v )
	postpone 1+		\ ( l v' )
	postpone 2dup		\ ( l v' l v')
	postpone =		\ ( l v' f )
	loop_end
; immediate

: (+loop)			\ ( i l v -- l v' f )
	2dup swap - >r
	rot dup >r +		\ ( l v' )
	r> r>			\ ( i d )
	2dup xor >r		\ wraparound?
	dup rot +		\ ( d d' )
	xor			\ crossed limt ?
	r> and 0<		\ ( l v' f )
;

: +loop		\ ( i -- )
	postpone 2r>		\ ( i l v )
	postpone (+loop)	\ ( l v' f )
	loop_end
; immediate

: i 	\ ( -- cnt )
	r@
; inline

: j	\ ( -- cnt )
	rsp@ 8 + @
; inline

: case
	false			\ mark the bottom of the stack of forward branches
; immediate

: of
	postpone over
	postpone =
	postpone if
	postpone drop
; immediate

: endof
	postpone else		\ endof is the same as else
; immediate

: endcase
	postpone drop
	thens			\ resolve any forward branches
; immediate

\ ( c a b -- flag ) true if c >= a and c < b
: within
	>r over <= swap r> < and
;

\ strings
: ,"	\ compile counted string
	'"' parse	\ ( c-addr len )
	dup c,		\ store the length byte
	here swap	\ ( c-addr dp len )
	dup >r		\ save length byte
	cmove		\ apend the string to the dict
	r> allot	\ update dp
; immediate

: s"		\ ( -- c-addr len )
	state @ if	\ compiling?
		postpone slits
		postpone ,"
	else			\ immediate mode
		'"' parse	\ ( c-addr len )
		tstr1		\ ( c-addr len temp )
		swap 2dup 2>r
		cmove 2r>
	then
; immediate

: c"		\ ( -- c-addr )
	state @ if	\ compiling?
		postpone clits
		postpone ,"
	else			\ immediate mode
		'"' parse	\ ( c-addr len )
		tstr1		\ ( c-addr len temp )
		2dup c!
		dup >r 1+ swap
		cmove r>
	then
; immediate

: ."		\ ( -- )
	state @ if	\ compiling?
		postpone s"	\ read the string, and compile slits, etc.
		postpone type	\ compile the final type
	else
		'"' parse type
	then
; immediate

: lf"
	state @ if	\ compiling?
		postpone slits
		1 c, 10 c,
	else
		tstr1 dup
		10 c! 1
	then
; immediate

\ values
: value		\ ( n "name" -- )
	create ,
	does> @
;

: to	\ ( n "name" -- )
	' >body				\ get body
	state @ if			\ compiling?
		postpone literal	\ compile the address of the value
		postpone !		\ compile !
	else				\ immediate mode
		!			\ update it straightaway
	then
; immediate

: +to	\ ( n "name" -- )
	' >body				\ get body
	state @ if			\ compiling?
		postpone literal	\ compile the address of the value
		postpone +!		\ compile +!
	else				\ immediate mode
		+!			\ update it straightaway
	then
; immediate

: 2value	\ ( n1 n2 "name" -- )
	create , ,
	does> 2@
;

\ exceptions
variable handler 0 handler !	\ last exception handler

: catch		\ ( xt -- expn# | 0 )
	dsp@ >r		\ save data stack pointer ( xt )
	handler @ >r	\ and previous handler ( xt )
	rsp@ handler !	\ set current handler ( xt )
	execute		\ execute returns if no throw ( -- )
	r> handler !	\ restore previous handler ( -- )
	r> drop		\ drop saved data stack pointer ( -- )
	0		\ all ok ( 0 )
;

: throw		\ ( ... expn# -- ... expn# )
	?dup if			\ 0 throw is no-op ( expn# )
		handler @ rsp!	\ restore previous return stack pointer ( expn# )
		r> handler !	\ restore previous handler ( expn# )
		r> swap >r	\ ( r: expn# ) ( saved-sp )
		dsp! drop r>	\ restore data stack ( expn# )
	then
;

: ?throw	\ ( flag expn# -- )
	postpone and
	postpone throw
; immediate

: abort		\ ( -- )
	-1 throw
;

variable abortmsg
: abort"	\ ( f "msg" -- )
	postpone c"
	postpone abortmsg
	postpone !
	postpone if
		-2 postpone literal
		postpone throw
	postpone then
; immediate

\ defer stuff
: @execute @ ?dup if execute then ;
: perform @execute ;

: defer@	\ ( xt1 -- xt2 )
	>body @
;

: defer!	\ ( xt2 xt1 -- )
	>body !
;

: defer		\ ( "name" -- )
	create ['] abort ,
	does> @execute
;

: is	\ ( xt "name" -- )
	state @ if			\ compiling?
		postpone [']
		postpone defer!
	else
		' defer!
	then
; immediate

: action-of	\ ( xt "name" -- )
	state @ if			\ compiling?
		postpone [']
		postpone defer@
	else
		' defer@
	then
; immediate

\ write n spaces to stdout.
: spaces	\ ( n -- )
	begin
		dup 0>		\ while n > 0
	while
		space		\ print a space
		1-		\ until we count down to 0
	repeat
	drop
;

\ pictured output
variable pic

: ud/mod	\ ( ud u -- udq ur )
	dup >r
	0 swap um/mod -rot
	r> um/mod -rot
;

: <#	\ ( ud -- ud )
	pad pic !
;

: hold	\ ( char -- )
	-1 pic +!
	pic @ c!
;

: #	\ ( ud1 -- ud2 )
	base @ ud/mod	\ ( udq ur )
	[char] 0 +
	dup [char] 9 >
	if
		[ char a char 9 - 1- ] literal +
	then
	hold
;

: #s	\ ( ud1 -- ud2 )
	begin
		# 2dup d0=
	until
;

: sign	\ ( n -- )
	0< if
		[char] - hold
	then
;

: #>	\ ( ud -- c-addr len )
	2drop
	pic @ dup pad swap -
;

\ numeric output
: u.r	\ ( u width -- )
	swap 0 <# #s #>		\ ( width c-addr len )
	rot over -		\ ( c-addr len spaces )
	spaces type
;

: u.	\ ( u -- )
	0 u.r space
;

: .r	\ ( n width -- )
	swap dup abs 0 <# #s rot sign #>
	rot over -
	spaces type
;

: .	\ ( n -- )
	0 .r space
;

: d.r	\ ( ud width -- )
	-rot swap over dabs <# #s rot sign #>
	rot over -
	spaces type
;

: d.	\ ( d -- )
	0 d.r space
;

\ fetches the integer at an address and prints it.
: ? \ ( c-addr -- )
	@ .
;

\ takes an address of a dictionary entry and prints the word's name.
: id.
	h_nsize +		\ skip to the flags byte
	dup c@			\ get the flags/length byte
	f_lenmask and		\ mask out the flags - just want the length

	begin
		dup 0>		\ length > 0?
	while
		swap 1+		\ ( c-addr len -- len c-addr+1 )
		dup c@		\ ( len c-addr -- len c-addr char | get the next character)
		emit		\ ( len c-addr char -- len c-addr | and print it)
		swap 1-		\ ( len c-addr -- c-addr len-1    | subtract one from length )
	repeat
	2drop			\ ( len c-addr -- )
;

\ simply iterates backwards from latest using the link pointers.
: words
	latest @			\ start at latest dictionary entry
	begin
		?dup			\ while link pointer is not null
	while
		dup ?hidden not
		if			\ ignore hidden words
			dup id.		\ but if not hidden, print the word
			space
		then
		@			\ dereference the link pointer - go to previous word
	repeat
	cr
;

\ cfa> is the opposite of >cfa.  it takes a codeword and tries to find the matching
\ dictionary definition. in truth, it works with any pointer into a word, not just
\ the codeword pointer, and this is needed to do stack traces.
\ this word returns 0 if it doesn't find a match.
: cfa>		\ ( c-addr -- word )
	latest @		\ ( c-addr word )
	begin
		?dup		\ while link pointer is not null
	while
		dup >r 2dup			\ ( c-addr word c-addr word )
		>cfa dup xt_length + @ +	\ ( c-addr word c-addr wordend )
		rot swap			\ ( c-addr c-addr word wordend )
		within r> swap			\ ( c-addr word f )
		if
			nip	\ leave curr dictionary entry on the stack
			exit
		then
		@		\ follow link pointer back
	repeat
	drop			\ restore stack
	0			\ sorry, nothing found
;

: .'	\ ( c-addr -- )
	dup cfa>
	?dup
	if
		dup id.
		>cfa -
		space [char] + emit . cr
		exit
	then
	drop
;

\ .s prints the contents of the stack.  it doesn't alter the stack.
: .s	\ ( -- )
	dsp@			\ get current stack pointer
	s0 @ 8 -		\ pointer to the stack element
	begin
		over over <=	\ compare to current stack pointer
	while
		dup @ .		\ print the stack element
		4-		\ move down
	repeat
	drop drop
;

\ forget has to go down the dictionary and remove all entries from the latest list and the
\ dictionary hash chains as it goes. but not too tricky.
: (forget)	\ ( word -- )
	\ we're going to work our way down to it from latest, unlinking stuff as we go
	latest @
	begin
		2dup <=
	while
		dup h_name +	\ skip to name pointer
		dup 1- c@	\ get the flags/length byte
		f_lenmask and	\ just want the length
		(bucket)	\ ( name len -- hash bucket address )
		over		\ ( fa la hba -- fa la hba la )
		4+ @		\ ( fa la hba la -- fa la hba nla )
		swap !		\ ( fa la ) \ and we have unlinked this hash entry
		@		\ dereference the link pointer - go to previous word
	repeat
	latest !		\ update the latest word
	dp !			\ update dp
;

: forget
	parse-name (find)
	dup 0= -15 ?throw
	(forget)
 ; immediate

: marker
	create does> 4- cfa>
	?dup
	if
		(forget)
	then
;

\ dump out the contents of memory, in the 'traditional' hexdump format.
: dump	\ ( c-addr len -- )
	base @ -rot		\ save the current base at the bottom of the stack
	hex			\ and switch to hexadecimal mode

	begin
		?dup		\ while len > 0
	while
		over 8 u.r	\ print the address
		space

		\ print up to 16 words on this line
		2dup		\ ( c-addr len c-addr len )
		16 min		\ ( c-addr len c-addr linelen )
		begin
			?dup		\ while linelen > 0
		while
			swap		\ ( c-addr len linelen c-addr )
			dup c@		\ ( c-addr len linelen c-addr byte )
			2 u.r space	\ print the byte
			1+ swap 1-	\ ( c-addr len linelen c-addr -- c-addr len c-addr+1 linelen-1 )
		repeat
		drop		\ ( c-addr len )

		\ print the ascii equivalents
		2dup		\ ( c-addr len c-addr len )
		16 min		\ ( c-addr len c-addr linelen )
		16 over -
		3 * spaces
		begin
			?dup			\ while linelen > 0
		while
			swap			\ ( c-addr len linelen c-addr )
			dup c@			\ ( c-addr len linelen c-addr byte )
			dup 32 128 within if	\ 32 <= c < 128?
				emit
			else
				drop '.' emit
			then
			1+ swap 1-		\ ( c-addr len linelen c-addr -- c-addr len c-addr+1 linelen-1 )
		repeat
		drop		\ ( c-addr len )
		cr

		dup 16 min	\ ( c-addr len linelen )
		tuck		\ ( c-addr linelen len linelen )
		-		\ ( c-addr linelen len-linelen )
		>r + r>		\ ( c-addr+linelen len-linelen )
	repeat

	drop			\ restore stack
	base !			\ restore saved base
;

\ dump a word.
: see
	parse-name (find)
	dup 0= -13 ?throw
	." dictionary entry" cr
	dup dup >cfa over - dump
	>cfa ." cfa" cr dup xt_length + @ dump
;

\ z" .." is like s" ..." except that the string is terminated by an ascii nul character.
: ,z"	\ compile counted string with a 0 on the end
	'"' parse	\ ( c-addr len )
	dup 1+ c,	\ store the length byte
	here swap	\ ( c-addr dp len )
	dup >r		\ save length byte
	cmove		\ apend the string to the dict
	r> allot	\ update dp
	0 c,		\ and pop the 0 on the end
; immediate

: z"		\ ( -- c-addr len )
	state @ if	\ compiling?
		postpone slits
		postpone ,z"
	else			\ immediate mode
		'"' parse	\ ( c-addr len )
		tstr1		\ ( c-addr len temp )
		swap 2dup 2>r	\ ( c-addr temp len )
		cmove 2r>	\ ( temp len )
		2dup +		\ ( temp len end )
		0 swap c! 1+	\ pop the 0 on the end
	then
; immediate

: strlen 	\ ( str -- len )
	dup		\ save start address
	begin
		dup c@ 0<>	\ zero byte found?
	while
		1+
	repeat
	swap -		\ calculate the length
;

\ create a c style string at a temp location
: cstring	\ ( c-addr len temp -- c-addr )
	dup >r
	swap 2dup 2>r	\ ( c-addr temp len )
	cmove 2r> +	\ ( temp end )
	0 swap c!	\ pop the 0 on the end
	r>		\ ( temp )
;

\ process arguments and environment available to us on the stack.
\ starting at r0, r0 itself points to argc, the number of command line arguments.
\ r0+4 points to argv[0], r0+8 points to argv[1] etc up to argv[argc-1].
\ argv[argc] is a null pointer.
\ after that the stack contains environment variables, a set of pointers to strings of the
\ form name=value and on until we get to another null pointer.
: argc
	r0 @ @
;

\ n argv gets the nth command line argument.
: argv \ ( n -- str u )
	1+ cells r0 @ +	\ get the address of argv[n] entry
	@		\ get the address of the string
	dup strlen	\ and get its length / turn it into a forth string
;

\ returns the address of the first environment string.  the list of strings ends
\ with a null pointer.
: environ	\ ( -- c-addr )
	argc		\ number of command line parameters on the stack to skip
	2 +		\ skip command line count and null pointer after the command line args
	cells		\ convert to an offset
	r0 @ +		\ add to base stack address
;

\ exits by calling the exit"2" syscall
: bye		\ ( -- )
	0 >r		\ return code (0)
	sys_exit	\ system call number
	syscall
;

\ input spec save/restore
: save-input	\ ( -- xn ... x1 n )
	blk @
	sourcefd @
	>in @
	#in 2@
	5
;

: restore-input \ ( xn ... x1 n -- flag )
	5 =
	if
		#in 2!
		>in !
		sourcefd !
		blk !
		false
	else
		true
	then
;

\ string evaluation
: evaluate	\ ( ... c-addr len -- ... )
	save-input n>r	\ save input spec
	#in 2!		\ set the string as the source
	0 >in !		\ reset buffer offset
	['] interpret catch
	nr> restore-input -37 ?throw
	throw
;

\ file inclution
: r/o \ ( -- fam )
	o_rdonly
;
: r/w \ ( -- fam )
	o_rdwr
;

: bin	\ ( fam -- fam )
; inline

: file-position	\ ( fd -- ud ior )
	1 >r 0 0 >r >r >r	\ ( -- r: fd 0d seek_cur )
	sys_lseek lsyscall
	2rdrop 2rdrop
	2dup d0<		\ ( ud flag )
;

: file-size	\ ( fd -- ud ior )
	here >r >r		\ ( -- r: fd &stat_buf )
	sys_fstat syscall
	2rdrop
	0<			\ ( err -- flag )
	here 48 + 2@ swap rot	\ ( ud flag )
;

: file-status	\ ( c-addr u -- x ior )
	tstr2 cstring
	here >r >r		\ ( -- r: *filename &stat_buf )
	sys_stat syscall
	2rdrop
	0< 0 swap		\ ( 0 flag )
;

: reposition-file	\ ( ud fd -- ior )
	0 >r -rot
	>r >r >r		\ ( -- r: fd ud seek_start )
	sys_lseek lsyscall
	2rdrop 2rdrop
	d0<			\ ( err -- flag )
;

: open-file	\ ( c-addr u fam -- fd ior)
	[ base @ 8 base ! 666 swap base ! ]
	literal >r
	-rot			\ ( fam c-addr u )
	tstr2			\ temp area for cstring
	cstring swap		\ ( cstring fam )
	>r >r			\ ( -- r: 666 fam cstring )
	sys_open syscall	\ open (filename, flags)
	2rdrop rdrop
	dup 0<			\ ( fd -- fd flag )
;

: create-file	\ ( c-addr u fam -- fd ior )
	[ base @ 8 base ! 666 swap base ! ]
	literal >r
	o_creat or o_trunc or
	-rot			\ ( fam c-addr u )
	tstr2			\ temp area for cstring
	cstring swap		\ ( cstring fam )
	>r >r			\ ( -- r: 666 fam cstring )
	sys_open syscall	\ open (filename, flags)
	2rdrop rdrop
	dup 0<			\ ( fd -- fd flag )
;

: read-file	\ ( c-addr u fd -- u2 ior )
	-rot			\ ( fd c-addr u )
	>r >r >r		\ ( -- r: u c-addr fd )
	sys_read syscall
	2rdrop rdrop
	dup 0<			\ ( u2 -- u2 flag )
;

: write-file	\ ( c-addr u fd -- ior )
	-rot			\ ( fd c-addr u )
	>r >r >r		\ ( -- r: u c-addr fd )
	sys_write syscall
	2rdrop rdrop
	0<			\ ( u2 -- flag )
;

: write-line	\ ( c-addr u fd -- ior )
	>r r@
	write-file
	?dup
	unless
		\ linefeed
		lf"
		r@ write-file
	then
	rdrop
;

: delete-file	\ ( c-addr u -- ior )
	tstr2			\ temp area for cstring
	cstring >r
	sys_unlink syscall
	rdrop
	0<			\ ( err -- flag )
;

: rename-file	\ ( c-addr1 u1 c-addr2 u2 -- ior )
	tstr2			\ temp area for cstring
	cstring >r
	tstr3			\ temp area for cstring
	cstring >r
	sys_rename syscall
	2rdrop
	0<			\ ( err -- flag )
;

: resize-file	\ ( ud fd -- ior )
	-rot
	>r >r >r
	sys_ftruncate syscall
	2rdrop rdrop
	0<			\ ( err -- flag )
;

: flush-file	\ ( fd -- ior )
	>r
	sys_fsync syscall
	rdrop
	0<			\ ( err -- flag )
;

: close-file	\ ( fd -- ior )
	>r
	sys_close syscall
	rdrop
	0<			\ ( err -- flag )
;

\ triple alu words
: 2invert	\ ( x1 x2 -- x3 x4 )
	invert >r invert r>
;

: d1+	\ ( d1 -- d2 )
	>r 1+ dup 0= negate r> +
;

: 3invert	\ ( x1 x2 x3 -- x4 x5 x6 )
	invert >r 2invert r>
;

: t1+	\ ( t1 -- t2 )
	>r d1+ 2dup d0= negate r> +
;

: tnegate	\ ( t1 -- t2 )
	3invert t1+
;

: tabs	\ ( t1 -- t2 )
	dup 0<
	if
		tnegate
	then
;

: ud*u	\ ( ud u -- ut )
	tuck um* 2swap um* swap
	>r 0 d+ r> -rot
;

: d*n	\ ( d n -- t )
	2dup xor 0< >r abs >r
	dabs r> ud*u r>
	if
		tnegate
	then
;

: ut/u	\ ( ut u -- ud )
	dup >r um/mod -rot
	r> um/mod nip swap
;

: t/n	\ ( t n -- d )
	2dup xor 0< >r abs >r
	tabs r> ut/u r>
	if
		dnegate
	then
;

: m*/	\ ( d1 n1 +n2 -- d2 )
	>r d*n r> t/n
;

\ line buffers and current pointer
create linebufs linesize 8 * allot
variable >linebuf
linebufs linesize - >linebuf !

\ new refill that can do file reads
: refill
	source-id -1 = if false exit then
	source-id 0= if refill exit then
	\ refill from file
	>linebuf @ linesize source-id
	read-line	\ ( u flag ior )
	if
		\ error or eof
		2drop
		false
	else
		\ got another line
		drop
		>linebuf @ swap
		#in 2! 0 >in !
		true
	then
;

: interpret-file
	begin
		refill		\ grab a lines worth
		source
		tabs>spaces	\ convert tabs
\		dup true =
\		if
\			." <<" source type ." >>" cr
\		then
	while
		interpret	\ interpret current buffer
	repeat
;

\ file inclusion
: include-file	\ ( ... fd -- ... )
	save-input n>r	\ save input spec
	>linebuf @
	linesize +
	>linebuf !	\ push current line buffer
	sourcefd !	\ set source-id to fd
	0 blk !		\ set blk to 0
	['] interpret-file catch
	>linebuf @
	linesize -
	>linebuf !	\ pop current line buffer
	nr> restore-input -37 ?throw
	throw
;

: included	\ ( ... c-addr u -- ... )
	r/o open-file throw
	>r r@
	['] include-file catch
	r> close-file throw
	throw
;

: include	\ ( ... "name" -- ... )
	parse-name
	included
; immediate

\ word called ( which just drops input characters until it hits the corresponding )
: (
	begin
		')' parse + source + =
	while
		refill 0=
	until then
; immediate

\ conditional compilation
: [defined]	\ ( "name" -- true | false )
	word-name find nip
	0<>
; immediate

: [undefined]	\ ( "name" -- true | false )
	word-name find nip
	0=
; immediate

: [else]	\ ( -- )
	1
	begin
		begin
			parse-name
			dup
		while
			2dup s" [if]"
			icompare 0=
			if
				2drop 1+
			else
				2dup s" [else]"
				icompare 0=
				if
					2drop 1-
					dup
					if
						1+
					then
				else
					s" [then]"
					icompare 0=
					if
						1-
					then
				then
			then
			?dup 0=
			if
				exit
			then
		repeat
		2drop
		refill 0=
	until
	drop
; immediate

: [if]	\ ( flag -- )
	0=
	if
		postpone [else]
	then
; immediate

: [then]
; immediate

\ most forths have this word
: place	\ ( addr u dest -- )
	2dup 2>r 1+ swap move 2r> c!
;

\ no op word
: noop
;

\ macros
: macro \ ( "name" <ccccc"> -- )
	create ," immediate
	does> count evaluate
;

\ test words for inlining size
: ti	\ ( size -- )
	latest @			\ start at latest dictionary entry
	begin
		?dup			\ while link pointer is not null
	while
		dup >cfa
		dup xt_length + @		\ get code length
		swap xt_compile + @		\ get compile word
		case
		['] inline, of
			2 pick >
			if
				\ inlined word is greater than size
				''' emit dup id. space '>' emit ''' emit space
			then
		endof
		['] call, of
			2 pick <
			if
				\ called word is less than size
				''' emit dup id. space '<' emit ''' emit space
			then
		endof
			drop
		endcase
		@			\ dereference the link pointer - go to previous word
	repeat
	cr
	drop
;

\ yield to coroutine
: yield r> r> swap >r >r ;

\ implament quit that knows about exceptions
: quit
	r0 @ rsp!
	0 sourcefd !
	postpone [
	begin
		refill
	while
		['] interpret catch ?dup
		if
			case
			-58 of ." [if], [else], or [then] exception" endof
			-57 of ." exception in sending or receiving a character" endof
			-56 of ." quit" endof
			-55 of ." floating-point unidentified fault" endof
			-54 of ." floating-point underflow" endof
			-53 of ." exception stack overflow" endof
			-52 of ." control-flow stack overflow" endof
			-51 of ." compilation word list changed" endof
			-50 of ." search-order underflow" endof
			-49 of ." search-order overflow" endof
			-48 of ." invalid postpone" endof
			-47 of ." compilation word list deleted" endof
			-46 of ." floating-point invalid argument" endof
			-45 of ." floating-point stack underflow" endof
			-44 of ." floating-point stack overflow" endof
			-43 of ." floating-point result out of range" endof
			-42 of ." floating-point divide by zero" endof
			-41 of ." loss of precision" endof
			-40 of ." invalid base for floating point conversion" endof
			-39 of ." unexpected end of file" endof
			-38 of ." non-existent file" endof
			-37 of ." file i/o exception" endof
			-36 of ." invalid file position" endof
			-35 of ." invalid block number" endof
			-34 of ." block write exception" endof
			-33 of ." block read exception" endof
			-32 of ." invalid name argument (e.g., to xxx)" endof
			-31 of ." >body used on non-created definition" endof
			-30 of ." obsolescent feature" endof
			-29 of ." compiler nesting" endof
			-28 of ." user interrupt" endof
			-27 of ." invalid recursion" endof
			-26 of ." loop parameters unavailable" endof
			-25 of ." return stack imbalance" endof
			-24 of ." invalid numeric argument" endof
			-23 of ." address alignment exception" endof
			-22 of ." control structure mismatch" endof
			-21 of ." unsupported operation" endof
			-20 of ." write to a read-only location" endof
			-19 of ." definition name too long" endof
			-18 of ." parsed string overflow" endof
			-17 of ." pictured numeric output string overflow" endof
			-16 of ." attempt to use zero-length string as a name" endof
			-15 of ." invalid forget" endof
			-14 of ." interpreting a compile-only word" endof
			-13 of ." undefined word" endof
			-12 of ." argument type mismatch" endof
			-11 of ." result out of range" endof
			-10 of ." division by zero" endof
			-09 of ." invalid memory address" endof
			-08 of ." dictionary overflow" endof
			-07 of ." do-loops nested too deeply during execution" endof
			-06 of ." return stack underflow" endof
			-05 of ." return stack overflow" endof
			-04 of ." stack underflow" endof
			-03 of ." stack overflow" endof
			-02 of ." abort " abortmsg @ count type endof
			-01 of ." abort" endof
				dup ." exeption # " .
			endcase
			cr
		else
			state @ 0=
			if
				." ok" space
				." ( " .s ." )" cr
			then
		then
	repeat
	bye
;

\ the version
: welcome
	." Forth version " version . cr
	." ok ( )" cr
;

include x86.f
include structure.f
include list.f
include memory.f
include class.f
include heap.f

welcome
hide welcome
marker core	\ marker for only main words, so you can run the test suite etc then come back
\ include tester.f
\ include core.f
quit		\ start the quit loop, so we get last resort exeption handler etc
