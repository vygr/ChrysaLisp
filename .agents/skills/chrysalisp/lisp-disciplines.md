# ChrysaLisp Lisp Disciplines

Part of the `chrysalisp` skill. These rules are mandatory for any Lisp code you
write or review. Each has its own heading, with the reason for it and worked
examples. `SKILL.md` carries a one line summary of each.

## Contents

*	[Tab Style](#tab-style)

*	[Indentation and the `fmt` Command](#indentation-and-the-fmt-command)

*	[Sensible `defq` and `setq` Line Wrapping](#sensible-defq-and-setq-line-wrapping)

*	[Type-Dependent Equality with `eql`](#type-dependent-equality-with-eql)

*	[Multi-Argument (N-ary) Comparisons & Range Checks (`<=`, `=`, `>=`, `<`, `>`, `/=`)](#multi-argument-n-ary-comparisons--range-checks------)

*	[Dynamic Scoping (No Lexical Closures)](#dynamic-scoping-no-lexical-closures)

*	[Manual Scope Control (`env-push` / `env-pop`)](#manual-scope-control-env-push--env-pop)

*	[Destructuring & Tuple Unpacking (`bind` vs Manual `elem-get`)](#destructuring--tuple-unpacking-bind-vs-manual-elem-get)

*	[DO NOT Shadow Built-in Function Names](#do-not-shadow-built-in-function-names)

*	[Sensible Line Wrapping for `defq`, `setq`, and `bind`](#sensible-line-wrapping-for-defq-setq-and-bind)

*	[Static `'()` vs Independent `(list)`](#static--vs-independent-list)

*	[Control Flow and Error Prevention](#control-flow-and-error-prevention)

*	[Conditionals & Branching (`if`, `ifn`, `when`, `unless`)](#conditionals--branching-if-ifn-when-unless)

*	[Lists as LIFO Stacks & The Rocinante Collector Pattern](#lists-as-lifo-stacks--the-rocinante-collector-pattern)

*	[Pragmatic Lambda Usage (`#` vs `lambda`)](#pragmatic-lambda-usage--vs-lambda)

*	[Variable Binding and Shadowing](#variable-binding-and-shadowing)

*	[Object Syntax & Sensible Wrapping](#object-syntax--sensible-wrapping)

*	[Anaphoric Loop Index `(!)`](#anaphoric-loop-index-)

*	[Short Anaphoric Lambdas `(# ...)` vs `(lambda ...)`](#short-anaphoric-lambdas---vs-lambda-)

*	[Compile-Time Constants](#compile-time-constants)

*	[Top-Level `defun` Definitions (Prebinder Rule)](#top-level-defun-definitions-prebinder-rule)

*	[GUI Application Event Loop Pattern](#gui-application-event-loop-pattern)

*	[NEVER Run GUI Code from TUI or Test Environment](#never-run-gui-code-from-tui-or-test-environment)

*	[Short-Circuiting, Embedded Binding, and Branching (`and` / `or`)](#short-circuiting-embedded-binding-and-branching-and--or)

## Tab Style

Always use leading 4-space tab characters for indentation
in source code and documentation, with spaces afterwards if needed.

## Indentation and the `fmt` Command

The indent of a line comes from the structure alone. These are the rules the
`fmt` command applies, and the rules most of the tree already follows, so
write new code this way.

*	A line is one tab in from the line its enclosing form opened on. Not
	lined up under an argument.

	```vdu
	(defun f (a)
		(print a)
		(if a
			(print 1)
			(print 2)))
	```

*	Where several forms open on the one line, each is a tab further in than
	the one around it. The indent then shows which form owns a line. Here
	the body of the `lambda` is two tabs in, and the rest of the arguments
	of the `reduce` one tab in.

	```vdu
	(defq moves (reduce (lambda (out (dx dy key))
			(defq nx (+ x dx) ny (+ y dy))
			(push out (list nx ny key)))
		(list (list -1 0 +fkey_left) (list 1 0 +fkey_right))
		(list)))

	(when (and (first-test a)
			(second-test b))
		(print a))
	```

*	VP block forms, `(vpif)` `(loop-start)` `(switch)` and the like, indent
	the lines between them. `(else)` `(vpcase)` `(vp-label)` sit one back.
	`(errorcase)` `(validatecase)` `(noterrorcase)` add no indent, what is in
	them sits where it would without them.

*	No line starts with a close bracket, they gather on the line above.

`fmt` lays a form out afresh, the line breaks you made inside it are not
kept, and it changes only the white space between tokens. A form is one
line if it fits in 80 columns, 120 for VP assembler. A definition always
has its body on lines of its own, and `cond` and `case` a clause to a line.
A longer line is broken at the outermost form that can be. It never breaks
a string, the opening line of a definition, or a form the source scanners
read as a line, such as `(dec-method)`. Comments, and the lines they are
on, are kept. `fmt -l 0` keeps your line breaks and only indents.

*	`fmt path ...` prints the formatted text, `fmt -c` lists the files that
	need formatting, `fmt -w` writes them. `make fmt` does the whole tree.

*	`(import "lib/text/format.inc")` gives `(format-lisp text [limit wide])`.

*	The source scanners and the doc builder read a line at a time, and take
	a line that starts with `defun`, `defmethod`, `ffi`, `dec-method` and
	such to be one. So never start a line of data, or of a string, with one
	of those words. `fmt` will not create such a line, but you can.

*	The tree as a whole has not yet been run through `fmt`. Until it has, do
	not run `fmt -w` on a file you are making a small change to, the diff
	would bury the change.

## Sensible `defq` and `setq` Line Wrapping

Do not place only one variable setting per line when defining multiple
variables with `(defq ...)` or mutating with `(setq ...)`. Putting one
variable-value pair per line is excessive and inflates line count.
Pack related variable-value pairs onto lines up to a reasonable column width
(~80-100 characters) and flow across lines cleanly:

	; Good: Flowed, logically grouped variable settings
	(defq +font_title (create-font "fonts/OpenSans-Bold.ctf" 14) +font_btn (create-font "fonts/OpenSans-Regular.ctf" 13)
		+font_bold (create-font "fonts/OpenSans-Bold.ctf" 13) +font_small (create-font "fonts/OpenSans-Regular.ctf" 11)
		*config* :nil *config_version* 1 *config_file* (cat *env_home* "news.tre")
		*selected_category* "top" *selected_id* 0 *current_stories* (list))

	; Avoid: 1 variable pair per line bloating vertical space
	(defq
		+font_title (create-font "fonts/OpenSans-Bold.ctf" 14)
		+font_btn (create-font "fonts/OpenSans-Regular.ctf" 13)
		*config* :nil
		*config_version* 1
		*selected_category* "top")

Combine local variable bindings within functions rather than writing multiple
consecutive `(defq ...)` calls:

	; Good: Combined and flowed
	(defq idx (!) is_selected (= id sel_id) card_flow (Flow) title_btn (Button)
		meta_flow (Flow) score_lbl (Label) meta_lbl (Label))

## Type-Dependent Equality with `eql`

*	`eql` performs deep content comparison on scalar numbers, strings,
	and typed numeric vectors (`nums`, `fixeds`, `reals`).

*	`eql` performs pointer identity comparison on general containers
	(`list`, `hmap`). To test list content equivalence, use:

	(and (= (length l1) (length l2)) (every eql l1 l2))

## Multi-Argument (N-ary) Comparisons & Range Checks (`<=`, `=`, `>=`, `<`, `>`, `/=`)

*	All comparison operators accept an arbitrary number of arguments:
	`(= a b c ...)`, `(<= a b c ...)`, `(< a b c ...)`, `(>= a b c ...)`.

*	*Range & "Within" Tests (Never `(and (>= ...) (<= ...))`):*
	Never write multiple comparisons joined by `and`:
	```lisp
	;; ANTI-PATTERN (evaluates id twice, allocates and executes 'and'):
	(and (>= id +event_city_0) (<= id +event_city_5))
	```
	Always use a single chained comparison:
	```lisp
	;; THE CHRYSALISP WAY (single primitive call, evaluates id once):
	(<= +event_city_0 id +event_city_5)
	```

*	*Named Constants for Event Ranges & Capacity Limits:*
	Never scatter magic numbers (e.g. `50`, `20`) inside event range checks or loops.
	Always declare an explicit named constant for the capacity limit (e.g. `+max_stories 50`),
	and evaluate the upper bound at compile time with flat $N$-ary addition `(const (+ +event_story_0 +max_stories -1))`:
	```lisp
	(defq +max_stories 50)
	...
	(<= +event_story_0 id (const (+ +event_story_0 +max_stories -1)))
	```

*	*Monotonic Ordering:*
	*	`(<= a b c)` tests `a <= b <= c`.
	*	`(< a b c)` tests `a < b < c`.
	*	`(= a b c)` tests `a = b = c`.
	*	`(< -1 idx (length items))` tests `0 <= idx < (length items)` in one step!

## Dynamic Scoping (No Lexical Closures)

Functions and lambdas
execute strictly inside the environment of their caller. They do not
capture defining scopes. All context must be supplied explicitly via
arguments or pre-exist in the caller's environment.

## Manual Scope Control (`env-push` / `env-pop`)

*	`(env-push)` pushes a new empty environment as the current scope.
	This is the module pattern: `(env-push)` ... `(export-symbols ...)`
	`(env-pop)`.

*	`(env-push env)` pushes the given environment instead, linking its
	parent to the current scope. The environment must be parentless: a
	fresh `(env 1)`, or one previously popped. Its bindings then read and
	`setq` as plain variables. There is no `env-tuck`; this replaces it.

*	`(env-pop)` takes no arguments. It restores the parent scope, clears
	the popped environment's parent link, and returns the popped
	environment, ready to be pushed again.

*	Every push must be paired with a pop in the same function or file.
	An unbalanced push leaves the wrong environment current when the
	enclosing function returns.

*	While pushed, `defq` and `bind` define into the pushed environment.
	Declare outer variables before the push and update them with `setq`:

	(defq state (env 1) total :nil)
	(def state 'count 0)
	(env-push state)
	(setq total (++ count))
	(env-pop)

## Destructuring & Tuple Unpacking (`bind` vs Manual `elem-get`)

Always use `(bind '(var1 var2 ...) seq)` to unpack lists, tuples, or function return sequences.
Never write multi-line ladders of `(elem-get seq 0)`, `(elem-get seq 1)` in a `defq`:
```lisp
;; ANTI-PATTERN (manual elem-get unpacking ladder in defq):
(defq sym (elem-get selected_coin 0)
	name (elem-get selected_coin 1)
	price (elem-get selected_coin 2)
	change (elem-get selected_coin 3)
	rank (elem-get selected_coin 4)
	spark (elem-get selected_coin 5))

;; THE CHRYSALISP WAY (single bind statement):
(bind '(coin_sym name price change rank spark) selected_coin)
```

*	**`&` (Single Skip) vs `&ignore` (Trailing Discard):**
	- `&` skips **exactly one** element (1-to-1 placeholder).
	- `&ignore` terminates binding immediately and discards **all remaining** elements in the sequence.
	If a sequence has trailing elements not accounted for in the pattern, omitting `&ignore` causes an `Error: (bind (param ...) seq) wrong_num_of_args !`.
	```lisp
	;; WRONG: (date) returns 7 elements (sec min hr day mo yr dotw),
	;; so (& cmin chr &) only consumes 4 elements -> wrong_num_of_args!
	(bind '(& cmin chr &) (date city_sec))

	;; CORRECT: &ignore consumes and discards all remaining elements:
	(bind '(& cmin chr &ignore) (date city_sec))
	```

## DO NOT Shadow Built-in Function Names

Never use built-in function or primitive names as variable or argument identifiers.
Symbols such as `sym`, `str`, `num`, `char`, `type`, `list`, `first`, `rest`,
`last`, `find`, `slice`, `map`, `each`, `eval`, `read`, `print`, `length`,
`format`, `min`, `max`, `abs`, `sort`, `filter`, `range`, `path`, etc., are
core language primitives. Shadowing them breaks lexical/global lookups and
introduces subtle bugs. Always use descriptive names (e.g. `coin_sym`,
`item_name`, `val_str`).

## Sensible Line Wrapping for `defq`, `setq`, and `bind`

Keep code readable within ~80-100 columns. Avoid both extreme vertical
ladders (one pair per line) and sprawling multi-variable lines:
```lisp
;; IDIOMATIC (sensible groupings by category, ~80-100 columns):
(defq *config* :nil *config_version* 1
	*config_file* (cat *env_home* "crypto.tre")
	*selected_symbol* "BTC" *coins_data* (list)
	*canvas_width* 360 *canvas_height* 110
	*btn_coin_0* :nil *btn_coin_1* :nil *btn_coin_2* :nil
	*btn_coin_3* :nil *btn_coin_4* :nil *btn_coin_5* :nil)

;; Wrap long list constructors across multiple lines:
(defq btns (list
	*btn_coin_0* *btn_coin_1* *btn_coin_2*
	*btn_coin_3* *btn_coin_4* *btn_coin_5*))
```

## Static `'()` vs Independent `(list)`

`'()` evaluates to a shared,
static empty list instance. Mutating `'()` (e.g. via `push`) corrupts
every reference in the system. Always use `(list)` to create a fresh,
independent, mutable empty list.

## Control Flow and Error Prevention

There is no `return` keyword;
a function yields the result of its last evaluated expression.
Non-local exits (`catch`, `throw`) are strictly for debugging. Code must
prioritize input validation to prevent invalid states from ever
occurring, rather than catching errors after the fact.

## Conditionals & Branching (`if`, `ifn`, `when`, `unless`)

*	*Implicit Progn on Else Clauses:* Both `(if tst then else_1 else_2 ...)` and
	`(ifn tst then else_1 else_2 ...)` treat the `then` branch as a single form,
	but treat all subsequent expressions in the `else` position as an **implicit `progn`**
	(evaluated sequentially via `:lisp :repl_progn`).

*	*NEVER Wrap Else Clauses in `(progn ...)`:*
	Wrapping the `else` clause in `(progn ...)` is completely redundant:
	```lisp
	;; ANTI-PATTERN: (if (not ...)) + redundant (progn ...) in the else clause:
	(if (not entry)
		(progn
			(defq md (Md))
			(def md :page_width page_w :zoom 1.0 :base_font_size 14)
			(. *right_container* :add_child md)
			(. md :populate_lines '("# Select an algorithm" "" "*No algorithm selected.*")))
		(progn
			(defq md_top (Md))
			(def md_top :page_width page_w :zoom 1.0 :base_font_size 14)
			(. *right_container* :add_child md_top)
			(. md_top :populate_lines (catalog-overview-markdown entry))
			...))
	```
	Instead, use `ifn` with the fallback in the single-form `then` branch, and let the entire main block flow into the `else` clause with zero `progn` wrapper:
	```lisp
	;; THE CHRYSALISP WAY: ifn + implicit progn in else clause + (def (defq ...)):
	(ifn entry
		(progn
			(def (defq md (Md)) :page_width page_w :zoom 1.0 :base_font_size 14)
			(. *right_container* :add_child md)
			(. md :populate_lines '("# Select an algorithm" "" "*No algorithm selected.*")))
		;; ELSE clause has implicit progn - no (progn ...) wrapper!
		(def (defq md_top (Md)) :page_width page_w :zoom 1.0 :base_font_size 14)
		(. *right_container* :add_child md_top)
		(. md_top :populate_lines (catalog-overview-markdown entry))
		(defq code_vdu (create-code-vdu (elem-get entry 7) page_w))
		(. *right_container* :add_child code_vdu)
		(when (defq idm (catalog-idioms-markdown entry))
			(def (defq lbl_space (Label)) :min_height 8 :border 0)
			(. *right_container* :add_child lbl_space)
			(def (defq md_bot (Md)) :page_width page_w :zoom 1.0 :base_font_size 14)
			(. *right_container* :add_child md_bot)
			(. md_bot :populate_lines idm))
		(def (defq lbl_pad (Label)) :min_height 16 :border 0)
		(. *right_container* :add_child lbl_pad))
	```

*	*Test-Result Passthrough:* When no `else` clause is provided:

	*	`(if tst then)` returns `:nil` when `tst` evaluates to falsy.

	*	`(ifn tst then)` returns `tst` (the non-nil test result) when `tst`
		evaluates to truthy.

*	*Macro Architecture of `when` and `unless`:*

	*	Single-form `(when tst form)` expands directly to `(if tst form)`
		for zero-overhead evaluation.

	*	Multi-form `when` expands to `(ifn tst :nil body ...)`, routing the
		body through the else-clause implicit `progn`.

	*	`(unless tst body ...)` expands to `(if tst :nil body ...)` for both
		single- and multi-form bodies.

	*	Both `when` and `unless` strictly guarantee returning `:nil`
		whenever the body is not executed, completely avoiding the
		fallback overhead of intermediate `cond` or `condn` structures.

*	*Idiomatic Negative Conditionals (Never `(if (not ...))`):*
	Never write `(if (not cond) ...)`. Always use ChrysaLisp's native negative
	conditional forms:
	*	Use `(ifn cond then [else ...])` when branching on a falsy condition.
	*	Use `(unless cond body ...)` for single-branch side effects or guard clauses without an `else`.

*	*Short True Branch on Same Line (Else Flows Below):*
	A very common ChrysaLisp idiom is to place a short `then` branch of `if`/`ifn` on the same line as the test, with the `else` (implicit `progn`) block flowing below:
	```lisp
	(ifn (str? price_str) "$0.00"
		(ifn (defq dot (find "." price_str)) (cat "$" price_str ".00")
			(defq int_part (slice price_str 0 dot)
				frac_part (slice price_str (+ dot 1) -1))
			(if (eql int_part "0")
				(cat "$0." (slice (cat frac_part "0000") 0 4))
				(cat "$" int_part "." (slice (cat frac_part "00") 0 2)))))

	(if (starts-with "-" chg_str) (cat chg_str "%")
		(cat "+" chg_str "%"))
	```

*	*Don't Waste Statements When Return Values Can Be Used Directly:*
	In ChrysaLisp, binding and evaluation forms (`defq`, `setq`, `push`, etc.) evaluate directly to their resulting value. Never waste a separate binding statement immediately before a conditional check:
	```lisp
	;; ANTI-PATTERN (wasting statements):
	(defq dot (find "." price_str))
	(ifn dot ...)

	(defq resp (http-get url))
	(when resp ...)

	;; THE CHRYSALISP WAY (embed return value directly in condition):
	(ifn (defq dot (find "." price_str)) (cat "$" price_str ".00")
		...)

	(when (defq resp (http-get url))
		...)

	(when (defq stream (file-stream path))
		...)
	```

*	*Use Built-in `min` and `max` Primitives:*
	ChrysaLisp has built-in `(min num num ...)` and `(max num num ...)` primitives that accept variable arguments:
	```lisp
	;; ANTI-PATTERN (manual comparison branching):
	(each (#
		(if (< %0 min_val) (setq min_val %0))
		(if (> %0 max_val) (setq max_val %0)))
		flt_pts)

	;; THE CHRYSALISP WAY:
	(each (# (setq min_val (min min_val %0) max_val (max max_val %0))) flt_pts)
	```

*	*Single-Line `if` / `ifn` / `when` / `unless` for Simple Forms:*
	When a conditional has a short test and concise form, keep it on a **single line**:
	```lisp
	;; IDIOMATIC (single-line for simple guard/init):
	(ifn *config* (setq *config* (Emap)))
	(unless *syntax* (setq *syntax* (Syntax)))
	(if (nempty? line) (push lines line))

	;; ANTI-PATTERN (vertical sprawl for a trivial branch):
	(ifn *config*
		(setq *config* (Emap)))
	```

## Lists as LIFO Stacks & The Rocinante Collector Pattern

*	`(push list elem ...)` appends to the end of `list` **and returns the list** as its return value!

*	*Avoid Imperative `(each ... (push l ...))` Accumulation:*
	Never pre-allocate an empty list and use an imperative loop to populate it:
	```lisp
	;; ANTI-PATTERN (redundant variables and environment lookups):
	(defq results (list) items '((1 2) (3 4)))
	(each (lambda ((a b)) (push results (calc a b))) items)
	```
	Instead, use the Rocinante collector pattern with `reduce`:
	```lisp
	;; THE ROCINANTE WAY (zero temp bindings, direct pipeline expression):
	(reduce (lambda (p (a b)) (push p (calc a b))) '((1 2) (3 4)) (list))
	```
	Because `(push p ...)` returns `p` directly, the accumulator flows seamlessly through each iteration of `reduce` with no intermediate variables, returning the completed list as the expression's value.

*	*Variadic `push` (Multiple Values in One Statement):*
	`push` accepts multiple elements: `(push list elem0 elem1 ...)`.
	Never write consecutive `(push l a) (push l b)` statements; write `(push l a b)`.
	In `reduce` collectors, you can push multiple items into the accumulator in a single step:
	`(push p item1 item2)`.

*	`(pop list)` removes and returns the final element.

*	`(first list)` is `(elem-get list 0)`.

*	`(last list)` is `(elem-get list -2)`.

*	`(slice list 1 -2)` returns the list minus first and last items.

## Pragmatic Lambda Usage (`#` vs `lambda`)

*	Use `(# ...)` for compact, performance-critical callbacks. Positional
	symbols `%0`, `%1`, etc., are globally interned with permanent
	`str_hashslot` cache indices.

*	Use `(lambda ...)` when destructuring arguments or when explicit
	naming clarifies complex logic.

*	**NEVER** use `(bind ...)` inside an anaphoric `#` lambda
	(anti-pattern: `(# (bind '(k v) %0) ...)`). Doing so introduces
	local symbols that destroy the cache benefits of `%0` while adding
	`bind` overhead. Use `(lambda ((k v)) ...)` instead.

## Variable Binding and Shadowing

*	Do not nest `let` or `let*`. Use `defq` to declare and bind multiple
	variables simultaneously in scope:

	(defq a 1 b 2 c (list))

*	Never use built-in function names as variables (e.g. `path`, `str`).
	Variables, functions, and macros share a single symbol environment.

*	Use `bind` for destructuring sequences and return tuples:

	(bind '(w h) (. view :get_size))
	(bind '(x y &rest tail) my_list)
	(bind '(x y &ignore) my_list)
	(bind '((x0 x1 &ignore) (y0 y1 &ignore) &ignore) nested_list)

	Note: `&` is a 1-to-1 placeholder skipping a single element. `&ignore` terminates binding immediately and ignores all remaining trailing elements.

*	**`bind` Semantics (`(def (env) ...)` vs `(set (env) ...)`):**
	`(bind '(var1 var2 ...) seq)` directly inserts new bindings into the current local environment `(env)`. It performs `(def (env) ...)`, **NOT** `(set (env) ...)`.
	Because it defines variables in the local frame, `bind` does **not** update or mutate existing outer bindings or global variables (`*...*`).
	To update pre-existing outer or global variables, always use `setq`:
	```lisp
	(setq *selected_id* (. *config* :find :selected_id)
		*selected_cat* (. *config* :find :selected_cat)
		*search_query* (. *config* :find :search_query))
	```

*	**Extracting Map Values with `gather`:**
	To extract multiple values from a map or tree in a single call, use `(gather map :key1 :key2 ...)`. It returns a list of the resolved values:
	```lisp
	;; Extract multiple values from a map directly into local bindings:
	(bind '(x y width height) (gather *config* :x :y :width :height))
	(bind '(sx sy buffer) (gather meta :sx :sy :buffer))
	```
	This eliminates repetitive individual `(. map :find :key)` calls when populating local variables.

## Object Syntax & Sensible Wrapping

*	Method call: `(. obj :method arg1 arg2)`.

*	Chained method call returning `this`: `.->` macro:

	(.-> *canvas* (:set_color +argb_black) (:fill 0))

*	Property access: `(get :prop obj)`.

*	Property create or mutate: `(def obj :prop val)`.

*	Property mutate only: `(set obj :prop val)`.

*	**Sensible Wrapping for `(def)` and `(set)`:**
	Avoid the vertical ladder anti-pattern where every single property and value occupies its own indented line. Instead, flow property pairs sensibly across lines, grouping 2 to 3 related pairs per line within ~80–100 columns, or keep the form inline on a single line if compact:
	```lisp
	;; ANTI-PATTERN: Excessive vertical sprawl (1 pair per line):
	(def (defq vdu (Vdu))
		:font +font_code
		:vdu_width 80
		:vdu_height h
		:color 0
		:ink_color +argb_black)

	;; IDIOMATIC: Sensible wrapping (2-3 pairs per line, ~80-100 columns):
	(def (defq vdu (Vdu))
		:font +font_code :vdu_width 80 :vdu_height h
		:color 0 :ink_color +argb_black)

	;; IDIOMATIC: Compact forms kept on a single line:
	(def (defq backdrop (Backdrop)) :color +argb_grey1 :min_width (max pad_w page_w) :min_height rh)
	(def (defq scroll (Scroll +scroll_flag_horizontal)) :min_width page_w :min_height rh)
	```

## Anaphoric Loop Index `(!)`

In `each`, `each!`, `map`, and `lines!`,
the form `(!)` evaluates to the current zero-based loop index:

	(each (# (print "Item " (!) ": " %0)) my_list)

## Short Anaphoric Lambdas `(# ...)` vs `(lambda ...)`

Always use `(# ...)` when using positional arguments (`%0`, `%1`, etc.):

	(map (# (path-transform m %0 (cat %0))) paths)

NEVER write `(lambda (%0) ...)`. The `lambda` form is strictly reserved for
explicitly named parameter lists: `(lambda (item) ...)`.

## Compile-Time Constants

Force compile-time arithmetic or lookups
using `(const ...)`:

	(* x (const (/ 180.0 +fp_pi)))

## Top-Level `defun` Definitions (Prebinder Rule)

**NEVER** nest `defun` inside another `defun`, `progn`, `catch`, or other expressions!
The ChrysaLisp prebinder scans top-level forms to discover functions, prebind symbols, assign frames, and optimize call sites.
If a `defun` is placed inside `(progn ...)`, `(catch ...)`, or another function, the prebinder cannot see it, causing `symbol_not_bound`, `wrong_num_of_args`, or compilation failure.
Always define all functions with `defun` at the top level of your file.

## GUI Application Event Loop Pattern

	(defun main ()
		(defq select (task-mboxes +select_size) *running* :t)
		; ... setup widgets / window ...
		(gui-add-front-rpc *window*)
		(while *running*
			(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
			(cond
				((= idx +select_main)
					(if (= (getf msg +ev_msg_target_id) +event_close)
						(setq *running* :nil)
						(. *window* :event msg)))
				(:t (. *window* :event msg))))
		(gui-sub-rpc *window*))

## NEVER Run GUI Code from TUI or Test Environment

GUI classes (`View`, `Window`, `Vdu`, `Flow`, `Button`, `Label`, `Md`, etc.)
and desktop apps (`apps/desktop/`, etc.) require full system mode with the
GUI subsystem, compositor, and window manager (`./run.sh -f`). They are NOT
present in the headless test environment or the TUI boot image
(`./run_tui.sh`); attempting to reference or instantiate GUI classes from
test scripts or TUI causes immediate `symbol_not_bound` errors.

To run GUI dependent code from an agent, pipe a `lisp -r` snippet to
`./run.sh -n 1 -f`, which gives the GUI boot image with a TUI attached to
the host. A widget tree can be built this way and dumped with
`(ui-save stream view)` for inspection, or a live window opened for the
user to interact with, see the `chrysalisp-gui-apps` skill.

## Short-Circuiting, Embedded Binding, and Branching (`and` / `or`)

*	`and` expands to `condn` (testing for `:nil`), and `or` expands to
	`cond`. Write predicates for zero wasted interpreter evaluations.

*	*Embed `defq` Inside Consumer Predicates:* Do not write standalone
	`(defq ...)` clauses in an `and` chain. Bind variables directly
	inside the comparison that uses them:

	(eql (third %1) (defq d (third %0)))

*	*Order Clauses for Fastest Failure:* Place the most discriminative
	test earliest to lazily abort execution before further bindings.

*	*Leverage `cond` Branch Conditions:* Put `(and ...)` directly as
	the `cond` branch condition; do not wrap the body in `(when ...)`.

*	*Use `when` Naturally for Multi-Statement Blocks:* Inside `case` or
	unconditional blocks, use `(when tst a1 a2)` directly.

*	*Anti-Pattern:*

	(cond
		((eql op0 'emit-cpy-rr)
			(when (and (defq cpy_info (pfind +map (first %1)))
					(defq d (third %0))
					(eql (third %1) d)
					(defq s (second %0))
					(nql s d))
				(case (first cpy_info)
					(:cr
						(elem-set emit_list (!) (list (second cpy_info) (second %1) s d))
						(elem-set emit_list (inc (!)) '(emit-nop)))))))

*	*Correct Pattern:*

	(cond
		((and (eql op0 'emit-cpy-rr)
				(defq cpy_info (pfind +map (first %1)))
				(eql (third %1) (defq d (third %0)))
				(nql d :rsp)
				(nql (defq s (second %0)) d)
				(nql s :rsp))
			(case (first cpy_info)
				(:cr
					(elem-set emit_list (!) (list (second cpy_info) (second %1) s d))
					(elem-set emit_list (inc (!)) '(emit-nop))))))
