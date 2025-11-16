# Tree-Based Save: The Elegant Solution

## The Problem with Diff/Merge

The original save approach tried to:
1. Parse original source
2. Generate new UI code
3. **Diff and merge** the two
4. Hope nothing was lost

This is fragile because:
- Comments might be lost
- Whitespace might change
- Imperative code mixed with UI might be removed
- Formatting preferences might not be preserved

## The Lisp Solution: Everything is a Tree

In Lisp, **code is data**. The entire app is already a tree:

```lisp
App Tree
├─ (import "env.inc")
├─ (import "gui/lisp.inc")
├─ (enums +event 0 ...)
├─ (defq *state* 0)
├─ (defun helper () ...)
├─ (ui-window *window* ())  ← This is also a tree!
│   ├─ (ui-button btn1 ...)
│   └─ (ui-button btn2 ...)
├─ (. btn1 :connect ...)
└─ (defun main () ...)
```

## The Elegant Approach: Track Everything

Instead of tracking only UI and trying to preserve the rest:

### Step 1: Track ALL Forms

```lisp
;Track imports
(track-import "gui/lisp.inc")

;Track state
(track-defq "*counter*" "0")

;Track functions
(track-defun "increment" "()" "(++ *counter*)")

;Track UI tree (already doing this!)
(track-ui-tree ui-tree)

;Track expressions
(track-expression "(. btn :connect +event)")

;Track comments
(track-comment ";This increments the counter" line-num)
```

### Step 2: Build Complete App Tree

```lisp
*app-tree* = [
  {:form_type :import :content "gui/lisp.inc"}
  {:form_type :defq :content {:name "*counter*" :value "0"}}
  {:form_type :defun :content {...}}
  {:form_type :ui :content <ui-tree>}
  {:form_type :expression :content "(. btn :connect ...)"}
  {:form_type :defun :content <main-function>}
]
```

### Step 3: Visit and Serialize

```lisp
(defun serialize-app-tree (app-tree)
  (defq result "")
  ;Visit each form in order
  (each (lambda (form)
    (setq result (cat result
      (serialize-form form)
      (ascii-char 10))))
    app-tree)
  result)
```

### Step 4: Save - No Diff Needed!

```lisp
(defun save-app (app-tree filepath)
  ;Just serialize the complete tree
  (defq source (serialize-app-tree app-tree))
  ;Write to file
  (write-to-file filepath source))
```

## Benefits

### ✅ Perfect Preservation

Everything is tracked as forms:
- **Comments** → `:comment` forms
- **Imports** → `:import` forms
- **State** → `:defq` forms
- **Functions** → `:defun` forms
- **UI** → `:ui` forms
- **Expressions** → `:expression` forms

### ✅ No Diff/Merge Needed

The tree **IS** the complete app. We don't need to:
- Parse original source
- Compare old vs new
- Try to preserve sections
- Hope nothing was lost

Just **visit the tree and serialize**.

### ✅ Modification is Natural

```lisp
;Find the form you want to modify
(defq counter-form (find-form app-tree (lambda (f)
  (and (eql (get :form_type f) :defq)
       (eql (get :name (get :content f)) "*counter*")))))

;Modify it
(. (get :content counter-form) :insert :value "10")

;Save - the whole tree is serialized
(save-app app-tree "app.lisp")
```

### ✅ Order is Preserved

The tree is a list in evaluation order:
1. Imports first
2. Then state/enums
3. Then functions
4. Then UI
5. Then main loop

Serialization preserves this order.

### ✅ LLM Can Help Build the Tree

An LLM can parse source and build the complete tree:

```
LLM: "Parse this app into a tree of forms"
Input: app.lisp source code
Output: Complete app-tree with all forms categorized
```

Then we don't need to track during execution - we can load any app!

## How It Works

### Load Any App

```lisp
;Read source
(defq source (read-file "apps/calculator/app.lisp"))

;Parse to tree (LLM could help here)
(defq app-tree (parse-app-to-tree source))

;Now we have:
app-tree = [
  {:form_type :import ...}
  {:form_type :enum ...}
  {:form_type :defq ...}
  {:form_type :ui :content <complete-ui-tree>}
  {:form_type :defun ...}
  ...
]
```

### Edit Visually

```lisp
;Find UI form
(defq ui-form (find-form app-tree :ui))

;Get UI tree
(defq ui-tree (get :content ui-form))

;Edit the UI tree (in visual designer)
(designer-add-button ui-tree "new_btn")

;Update the form
(. ui-form :insert :content ui-tree)
```

### Save

```lisp
;Visit every form and serialize
(defq saved-source (serialize-app-tree app-tree))

;Write to file
(write-file "apps/calculator/app.lisp" saved-source)
```

## Implementation Strategy

### Phase 1: Track During Execution (Current)

```lisp
;Macros track UI as it executes
(import "gui_designer/lisp.inc")
(ui-window ...) ;← Tracked
```

### Phase 2: Track All Forms (New)

```lisp
;Wrap ALL top-level forms, not just UI
(defmacro defq-tracked (name value)
  (progn
    (track-defq name value)
    (defq ,name ,value)))

(defmacro defun-tracked (name args &rest body)
  (progn
    (track-defun name args body)
    (defun ,name ,args ~body)))
```

### Phase 3: Parse Without Execution (Future)

```lisp
;Use Lisp reader to parse source
(defq forms (read-all-forms source))

;Convert to app-tree
(defq app-tree (forms-to-tree forms))

;No execution needed!
```

### Phase 4: LLM-Assisted Parsing (Future)

```
User: Load apps/calculator/app.lisp
System: Reads file, sends to LLM
LLM: Parses and returns structured tree:
  {
    imports: [...]
    enums: [...]
    state: [...]
    functions: [...]
    ui: <ui-tree>
    main: [...]
  }
System: Converts to app-tree
```

## Example: Complete Round Trip

### Original App

```lisp
;Calculator app
(import "gui/lisp.inc")

;State
(defq *value* 0)

;Helper
(defun add (x) (+ *value* x))

;UI
(ui-window *window* ()
  (ui-button btn (:text "+")))

;Connect
(. btn :connect +event)

;Main
(defun main () ...)
```

### Load to Tree

```lisp
app-tree = [
  {:form_type :comment :content ";Calculator app"}
  {:form_type :import :content "gui/lisp.inc"}
  {:form_type :comment :content ";State"}
  {:form_type :defq :content {:name "*value*" :value "0"}}
  {:form_type :comment :content ";Helper"}
  {:form_type :defun :content {...}}
  {:form_type :comment :content ";UI"}
  {:form_type :ui :content <ui-tree>}
  {:form_type :comment :content ";Connect"}
  {:form_type :expression :content "(. btn :connect +event)"}
  {:form_type :comment :content ";Main"}
  {:form_type :defun :content {...}}
]
```

### Modify

```lisp
;Find and modify UI
(defq ui-form (find-form app-tree :ui))
(designer-add-button (get :content ui-form) "btn2")

;Find and modify state
(defq value-form (find-form app-tree (lambda (f)
  (and (eql (get :form_type f) :defq)
       (eql (get :name (get :content f)) "*value*")))))
(. (get :content value-form) :insert :value "10")
```

### Save

```lisp
;Serialize complete tree
(defq saved (serialize-app-tree app-tree))

;Result:
;Calculator app
(import "gui/lisp.inc")

;State
(defq *value* 10)  ← Modified!

;Helper
(defun add (x) (+ *value* x))

;UI
(ui-window *window* ()
  (ui-button btn (:text "+"))
  (ui-button btn2 (:text "New")))  ← Added!

;Connect
(. btn :connect +event)

;Main
(defun main () ...)
```

## Why This is The Right Approach

### In Traditional Languages

Code → Parse → AST → Modify → Generate → Code
- Complex parsing
- Lose comments/formatting
- Need code generator

### In Lisp

Code → Read → Forms → Modify → Print → Code
- Reader is built-in
- Forms ARE the AST
- Printer is trivial (just str)
- **Nothing is lost**

### With Tree Tracking

Code → Execute+Track → Tree → Modify → Visit+Serialize → Code
- Execute captures structure
- Tree is complete representation
- Visit/serialize is simple traversal
- **Everything preserved**

## Conclusion

The tree-based approach is elegant because:

1. **Code is data** - Lisp apps are already trees
2. **Track everything** - Not just UI, but ALL forms
3. **Visit and serialize** - Simple tree traversal
4. **No diff/merge** - The tree IS the app
5. **Perfect preservation** - Everything is a form
6. **Natural modification** - Find form, change form, save
7. **LLM-friendly** - Can parse source to tree

This is the **Lisp way** - embrace that code is data, and the problem becomes trivial.

## Implementation Files

- `gui_designer/full_app_tracking.inc` - Complete app tree tracking
- `apps/designer/demo_full_tree.lisp` - Demo of tree-based save

## Future Work

- Full Lisp reader integration (parse without execution)
- LLM-assisted parsing for complex apps
- Visual tree editor (not just UI, but all forms)
- Interactive REPL for tree modification

The tree is the app. Save the tree, save the app. Simple. Elegant. Lisp. ✨
