# The ChrysaLisp Configuration Standard

## 1. Overview

The ChrysaLisp `config` standard is a simple, robust, and reusable pattern for
managing the persistent state of GUI applications on a per-user basis. It
saves what the user chose and made, the toolbar's settings, the view shown,
the user's content, and restores it the next time the app is opened.

The core philosophies behind this standard are:

*   **Modularity:** The configuration logic is a few functions of the
    application: `config-default`, `config-load`, `config-update` and
    `config-save`. A small app keeps them in `app.lisp`, others in a
    `config.inc` of their own, as `apps/template/config.inc`,
    `apps/desktop/todo/config.inc` and `apps/games/chess/config.inc` do. A new
    app starts from `apps/template`, so it starts with a `config.inc`.

*   **Per-User Storage:** Each user's settings are stored in their own home
    directory (`(get :home *env*)`), ensuring that configurations are isolated
    and do not interfere with one another.

*   **Robustness through Versioning:** A version number (`+config_version`)
    lets the application handle a change to the configuration's shape, so an
    old or a strange file gives safe defaults, not a crash.

*   **Simplicity and Flexibility:** The system uses ChrysaLisp's native
    serialization format (`.tre`) and the flexible `Emap` data structure,
    making it easy to store and retrieve a wide range of key-value data.

*   **State, not place:** An app keeps what the user chose in it, not where
    its window was. A window opens where the screen puts a new one.

## 2. Core Components

The standard is built upon a few key components that work together.

### The Configuration Variables

*   `*config*`: A global variable that holds the application's configuration
    data in an `Emap` during runtime.

*   `+config_version`: An integer constant, the version of the
    configuration's shape. It goes up when a key changes its meaning.

*   `+config_file`: The full path to the user's configuration file,
    `(cat (get :home *env*) "appname.tre")`.

### The `.tre` Data Format

Configuration data is serialized to a `.tre` file, which is a human-readable
representation of a ChrysaLisp `Emap`, by `tree-save` and read back by
`tree-load`. Lists, strings, numbers, keywords and nested collections all
go in, so it can be read and edited by hand.

### The Lock Service

The file is read under `with-read-lock` and written under `with-write-lock`,
from `service/lock/app.inc`, so two copies of an app, or two nodes, never
meet in the file, one reading it half written.

### The Standard Function Interface

These provide a standard API for managing the configuration lifecycle:

*   **`(config-default)`**: Creates and returns a new `Emap` containing the
    application's default settings. This function is the single source of
    truth for a "fresh" configuration and must always include a `:version`
    key matching `+config_version`.

*   **`(config-load)`**: Called at application startup. It reads the user's
    `.tre` file. If the file doesn't exist, is corrupted, or its version is
    not one it can take, `*config*` is `(config-default)`.

*   **`(config-update)`**: Gathers the current runtime state of the
    application, which tab is shown, what a button is set to, the user's
    data, into the in-memory `*config*` `Emap`.

*   **`(config-save)`**: Called when the application is closing. It first
    calls `(config-update)` to ensure the `*config*` map is up-to-date and
    then writes the `Emap` to the `.tre` file.

An app may also have a **`(config-apply)`**, the other way to
`config-update`: it sets the app from `*config*`, so `main` reads as load,
apply, open, and save on close.

## 3. Lifecycle in a Typical Application

1.  **Startup**: The application's `main` function calls `(config-load)`.

```image
docs/diagrams/config_life.cwb
```

2.  **Initialization**: The app reads values from the now-populated `*config*`
    variable to set its initial state, the tab shown, the toolbar's buttons
    lit, the user's data loaded.

3.  **Runtime**: The application runs its main event loop.

4.  **Shutdown**: Upon exiting the main loop, the application calls
    `(config-save)` to persist its final state to the user's home directory.

## 4. Detailed Analysis: The `template` App

`apps/template` is the app a new GUI app is made from, and its `config.inc`
is the model. It keeps which tab of its stack is shown, and the colour of
each of its four buttons.

### `apps/template/config.inc`

```vdu
;;;;;;;;;;;;;;;
; configuration
;;;;;;;;;;;;;;;

(import "service/lock/app.inc")

(defq *config* :nil +config_version 1 +config_file (cat (get :home *env*) "template.tre"))

(defun config-default ()
	;the first time: the main view, and each button its theme's colour, :nil
	(scatter (Emap) :version +config_version :tab 0 :colors (list :nil :nil :nil :nil)))

(defun config-load ()
	;the defaults with what was kept laid over them, so a key added since it
	;was kept has its default, and one no longer used is passed over. A file
	;of a later version than this, or none, gives the defaults
	(setq *config* (config-default))
	(and (defq old (with-read-lock +config_file
				(tree-load (file-stream +config_file))))
		(<= (ifn (. old :find :version) 0) +config_version)
		(. old :each (lambda (key val)
			(unless (eql key :version) (. *config* :insert key val))))))

(defun config-apply ()
	;the app as the config says, the tab shown and the buttons' colours
	(bind '(tab colors) (gather *config* :tab :colors))
	(. *stack_flow* :show_tab tab)
	(each (# (if %1 (def (. %0 :dirty) :paper %1)))
		(list *b1* *b2* *b3* *b4*) colors))

(defun config-update ()
	;the config as the app is, the tab shown and the buttons' colours, a
	;colour set on the button itself, not one it has from what it is in
	(scatter *config*
		:tab (. (. *stack_flow* :get_radio_bar) :get_selected)
		:colors (map (# (def? :paper %0)) (list *b1* *b2* *b3* *b4*))))

(defun config-save ()
	(config-update)
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))
```

*   **`config-default`**: the shape of what is kept, and its values the first
    time. A button's colour is `:nil` till it is set, so it follows the
    theme.

*   **`config-load`**: the migration method of section 5, so a user's
    settings are kept when a version adds a key.

*   **`config-update`**: takes from the widgets only what is needed. A
    view's property is found up its parents by `get`, so a button not
    coloured would give its grid's colour; `def?` gives only what is set on
    the button itself.

### Integration into `apps/template/app.lisp`

```vdu
(import "./widgets.inc")
(import "./config.inc")
...
(defun main ()
	(defq select (task-mboxes +select_size) *running* :t)
	(def *window* :tip_mbox (elem-get select +select_tip))
	;as it was when last closed
	(config-load)
	(config-apply)
	(. *window* :open)
	...
	(while *running*
		...)
	(config-save)
	(. *window* :close))
```

The config is imported after the widgets, as it names them. It is loaded
and applied before the window opens, so the window is first seen as it was
left.

## 5. Discard or Migrate

### The Simple (Discard) Method

Most apps load this way: a file that is missing, can not be read, or is of
another version is replaced by the defaults. When a version adds a key, a
user's other settings are lost with it.

```vdu
(defun config-load ()
	(setq *config* (with-read-lock +config_file
			(tree-load (file-stream +config_file))))
	(if (or (not *config*) (nql (. *config* :find :version) +config_version))
		(setq *config* (config-default))))
```

A key added since a file was saved can also be read with a default of its
own, `(ifn (. *config* :find :mode) 0)`, so the version need not change, as
`apps/science/pcb/app.lisp` does for its toolbar and route settings.

### The Migration Method

The template's `config-load`: the defaults, with what was kept laid over
them.

**Advantages of the Migration Method:**

* **Preserves User Data:** A user's settings and content are not lost just
  because a new setting was added.

* **Handles New Defaults:** A key added in a new version is there from
  `config-default`, while the old settings are kept.

* **Handles Deprecated Settings:** An old key that is no longer read is
  carried along and not used, it causes no error.

* **Refuses the Future:** A file of a later version than the app, from a
  newer copy, gives the defaults, as its keys may mean something else.

## 6. Conclusion

The ChrysaLisp `config` standard provides a consistent, simple, and effective
solution for application state persistence. A few functions, a versioned
`Emap` in a `.tre` file in the user's home, read and written under the lock
service, keep what a user chose from one run to the next. The template app
is the model, and a new app made from it has it from the start.
