# Macros

### #

```code
(# (< %9 %0 %3) ...) -> (lambda (%0 %3 %9) (< %9 %0 %3) ...)
```

### ++

```code
(++ num [num]) -> num
```

### --

```code
(-- num [num]) -> num
```

### .->

```code
(.-> this form ...)
```

### .?

```code
(.? this method) -> :nil | lambda
```

### .super

```code
(.super this :method [arg ...])
```

### aand

```code
(aand [form] ...)
```

### acond

```code
(acond (tst body) ...)
```

### aeach

```code
(aeach seq body)
```

### aif

```code
(aif form form [form])
```

### and

```code
(and [tst] ...) -> :t | :nil | tst
```

### ascii-char

```code
(ascii-char num) -> char
```

### ascii-code

```code
(ascii-code char) -> num
```

### asome

```code
(asome seq body)
```

### awhen

```code
(awhen form body)
```

### awhile

```code
(awhile form body)
```

### bits

```code
(bits name base [(bit field ...)] ...)
```

### bits?

```code
(bits? val mask ...) -> :t | :nil
```

### callback

```code
(callback lambda env arg ...) -> (#eval `(#apply ,lambda '(,arg ...)) env)
```

### case

```code
(case form [(key|(key ...) body)] ...)
```

### const

```code
(const form)
```

### dec

```code
(dec num) -> num
```

### defabstractmethod

```code
(defabstractmethod ([arg ...]) body)

Declare a method as abstract and will
throw an error if invoked. Concrete
classes deriving from a class with abstractions
should provide the concreate handler
```

### defclass

```code
(defclass Name ([arg ...]) (super ...)|:nil body)
```

### deffimethod

```code
(deffimethod name ffi)
```

### defgetmethod

```code
(defgetmethod field)
```

### defmethod

```code
(defmethod name ([arg ...]) body)

(. this :method [arg ...])
```

### defsetmethod

```code
(defsetmethod field)
```

### defun

```code
(defun name ([arg ...]) body)
```

### each

```code
(each lambda seq ...)
```

### enums

```code
(enums name base [(enum field ...)] ...)
```

### every

```code
(every lambda seq ...) -> :nil | form
```

### filter

```code
(filter lambda seq) -> list
```

### get-byte

```code
(get-byte str index) -> num
```

### get-int

```code
(get-int str index) -> num
```

### get-long

```code
(get-long str index) -> num
```

### get-netid

```code
(get-netid str index) -> netid
```

### get-nodeid

```code
(get-nodeid str index) -> nodeid
```

### get-short

```code
(get-short str index) -> num
```

### get-ubyte

```code
(get-ubyte str index) -> num
```

### get-uint

```code
(get-uint str index) -> num
```

### get-ushort

```code
(get-ushort str index) -> num
```

### getf

```code
(getf obj field [offset]) -> value
```

### gui-add-back-rpc

```code
(gui-add-back-rpc view) -> view
```

### gui-add-front-rpc

```code
(gui-add-front-rpc view) -> view
```

### gui-logout-rpc

```code
(gui-logout-rpc) -> :nil
```

### gui-quit-rpc

```code
(gui-quit-rpc) -> :nil
```

### gui-sub-rpc

```code
(gui-sub-rpc view) -> view
```

### inc

```code
(inc num) -> num
```

### let

```code
(let ([(sym val) ...]) body)
```

### let*

```code
(let* ([(sym val) ...]) body)
```

### lower

```code
(lower field | (field sym) ...) -> (set this field sym ...)
```

### macrobind

```code
(macrobind form) -> (prebind (macroexpand form))
```

### map

```code
(map lambda seq ...) -> list
```

### memoize

```code
(memoize key form [num_buckets]) -> (eval form)
```

### not

```code
(not form) -> :t | :nil
```

### notany

```code
(notany lambda seq ...) -> :t | :nil
```

### notevery

```code
(notevery lambda seq ...) -> :t | :nil
```

### or

```code
(or [tst] ...) -> :nil | tst
```

### raise

```code
(raise field | (sym val) ...) -> (defq sym (get field this) ...)
```

### reach

```code
(reach lambda seq ...)
```

### read-int

```code
(read-int stream) -> num
```

### read-long

```code
(read-long stream) -> num
```

### read-short

```code
(read-short stream) -> num
```

### redefmacro

```code
(redefmacro name ([arg ...]) body)
```

### redefun

```code
(redefun name ([arg ...]) body)
```

### reduce

```code
(reduce lambda seq [init]) -> form
```

### reverse

```code
(reverse seq) -> seq
```

### rmap

```code
(rmap lambda seq ...) -> list
```

### rreduce

```code
(rreduce lambda seq [init]) -> form
```

### rsome

```code
(rsome lambda seq ...) -> :nil | form
```

### setd

```code
(setd sym val [sym val] ...)
```

### setf

```code
(setf obj field value [offset]) -> obj
```

### setf->

```code
(setf-> msg form ...)
```

### some

```code
(some lambda seq ...) -> :nil | form
```

### static-q

```code
(static-q form) -> 'form

static quoted
```

### static-qq

```code
(static-qq form) -> `form

static quasi-quoted
```

### static-qqp

```code
(static-qqp form) -> `form

static quasi-quoted, prebind only !
```

### structure

```code
(structure name base [(byte field ...)] ...)
```

### times

```code
(times num body)
```

### ui-backdrop

```code
(ui-backdrop name [props] [body]) -> backdrop
```

### ui-button

```code
(ui-button name [props] [body]) -> button
```

### ui-buttons

```code
(ui-buttons symbols event [props])
```

### ui-canvas

```code
(ui-canvas name width height scale [props]) -> canvas
```

### ui-element

```code
(ui-element name constructor [props] [body]) -> view
```

### ui-flow

```code
(ui-flow name [props] [body]) -> flow
```

### ui-grid

```code
(ui-grid name [props] [body]) -> grid
```

### ui-hchart

```code
(ui-hchart name title num_marks [props]) -> hchart
```

### ui-label

```code
(ui-label name [props] [body]) -> label
```

### ui-progress

```code
(ui-progress name [props]) -> progress
```

### ui-props

```code
(ui-props props [props]) -> props
```

### ui-radio-bar

```code
(ui-radio-bar name symbols [props])
```

### ui-root

```code
(ui-root name constructor [props] [body]) -> view
```

### ui-scroll

```code
(ui-scroll name flags [props] [body]) -> scroll
```

### ui-slider

```code
(ui-slider name [props]) -> slider
```

### ui-spinner

```code
(ui-spinner name [props]) -> spinner
```

### ui-stack

```code
(ui-stack name tabs [props] [body]) -> stack
```

### ui-stroke

```code
(ui-text name [props]) -> stroke
```

### ui-text

```code
(ui-text name [props]) -> text
```

### ui-textfield

```code
(ui-textfield name [props]) -> textfield
```

### ui-title

```code
(ui-title name [props]) -> title
```

### ui-title-bar

```code
(ui-title-bar name title symbols event [props]) -> flow
```

### ui-toggle-bar

```code
(ui-toggle-bar name symbols [props])
```

### ui-tool-bar

```code
(ui-tool-bar name [props] [body]) -> flow
```

### ui-tree

```code
(ui-tree name event [props]) -> tree
```

### ui-vdu

```code
(ui-vdu name [props]) -> vdu
```

### ui-view

```code
(ui-view name [props] [body]) -> view
```

### ui-window

```code
(ui-window name [props] [body]) -> window
```

### unless

```code
(unless tst body)
```

### when

```code
(when tst body)
```

### write-int

```code
(write-int stream list|num) -> bytes
```

### write-long

```code
(write-long stream list|num) -> bytes
```

### write-short

```code
(write-short stream list|num) -> bytes
```

