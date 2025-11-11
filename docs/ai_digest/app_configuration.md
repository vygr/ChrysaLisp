# The ChrysaLisp Configuration Standard

## 1. Overview

The ChrysaLisp `config` standard is a simple, robust, and reusable pattern for
managing the persistent state of GUI applications on a per-user basis. It is
designed to save and restore user-specific data, such as window position and
size, application settings, and user-generated content, between sessions.

The core philosophies behind this standard are:

*   **Modularity:** The entire configuration logic is encapsulated in a single,
    self-contained `config.inc` file, making it easy to add to any application.

*   **Per-User Storage:** Each user's settings are stored in their own home
    directory (`*env_home*`), ensuring that configurations are isolated and do
    not interfere with one another.

*   **Robustness through Versioning:** A simple version number
    (`*config_version*`) allows the application to gracefully handle changes to
    the configuration format, preventing crashes from outdated files by
    reverting to safe defaults.

*   **Simplicity and Flexibility:** The system uses ChrysaLisp's native
    serialization format (`.tre`) and the flexible `Emap` data structure, making
    it easy to store and retrieve a wide range of key-value data.

## 2. Core Components

The standard is built upon a few key components that work together.

### The `config.inc` File

This is the heart of the pattern. Every application implementing this standard
has a `config.inc` file that defines a consistent set of global variables and
functions.

*   `*config*`: A global variable that holds the application's configuration
    data in an `Emap` during runtime.

*   `*config_version*`: An integer constant that defines the current version of
    the configuration structure.

*   `*config_file*`: The full path to the user's configuration file, typically
    defined as `(cat *env_home* "appname.tre")`.

### The `.tre` Data Format

Configuration data is serialized to a `.tre` file, which is a human-readable
representation of a ChrysaLisp `Emap` (a hash map). This allows for easy
inspection and manual editing if necessary.

### The Standard Function Interface

The `config.inc` file provides a standard API for managing the configuration
lifecycle:

*   **`(config-default)`**: Creates and returns a new `Emap` containing the
    application's default settings. This function is the single source of truth
    for a "fresh" configuration and must always include a `:version` key
    matching `*config_version*`.

*   **`(config-load)`**: Called at application startup. It tries to load the
    user's `.tre` file. If the file doesn't exist, is corrupted, or its version
    number doesn't match `*config_version*`, it calls `(config-default)` to
    ensure the application starts with a valid state.

*   **`(config-update)`**: Gathers the current runtime state of the application
    (e.g., window position, user data) and updates the in-memory `*config*`
    `Emap`.

*   **`(config-save)`**: Called when the application is closing. It first calls
    `(config-update)` to ensure the `*config*` map is up-to-date and then writes
    the `Emap` to the `.tre` file.

## 3. Lifecycle in a Typical Application

1.  **Startup**: The application's `main` function calls `(config-load)`.

2.  **Initialization**: The app reads values from the now-populated `*config*`
    variable to set its initial state (e.g., window position, loading user
    data).

3.  **Runtime**: The application runs its main event loop.

4.  **Shutdown**: Upon exiting the main loop, the application calls
    `(config-save)` to persist its final state to the user's home directory.

## 4. Detailed Analysis: The `todo` App

The `todo` application is an excellent example of this standard in practice, as
it persists not only UI state but also user-generated content.

### `apps/todo/config.inc`

This file defines the structure of the data to be saved.

```vdu
;;;;;;;;;;;;;;;;;;;;;;
; apps/todo/config.inc
;;;;;;;;;;;;;;;;;;;;;;

(defq *config* :nil *config_version* 1
	*config_file* (cat *env_home* "todo.tre"))

(defun config-default ()
	(scatter (Emap)
		:version *config_version*
		:spacing 26
		:todo (list)
		:done (list)
		:deleted (list)))

(defun config-update ()
	(bind '(todo_items done_items deleted_items)
		(map (# (map (# (. (last (. %0 :children)) :get_text)) %0))
			(map (# (. %0 :children)) (list *todo_flow* *done_flow* *deleted_flow*))))
	(scatter *config* :todo todo_items :done done_items :deleted deleted_items))

(defun config-save ()
	(config-update)
	(when (defq stream (file-stream *config_file* +file_open_write))
		(tree-save stream *config*)))
```

*   **`config-default`**: Defines the shape of the saved data. It includes the
    required `:version` key and then three empty lists to hold the text content
    of the items in the "Todo", "Done", and "Deleted" columns.

*   **`config-update`**: This is the most interesting part. It doesn't save the
    UI widgets themselves. Instead, it intelligently extracts only the necessary
    data: the text from the `Textfield` inside each item in each column.

    * It maps over the `*todo_flow*`, `*done_flow*`, and `*deleted_flow*`
        widgets.

    * For each flow, it gets its children (the individual list items).

    * For each item, it gets *its* children (the toolbar and the `Textfield`),
        takes the `last` one (which is the `Textfield`), and calls `:get_text`
        on it.

    * The result is three lists of strings, which are then saved to the
        `*config*` `Emap`.

### Integration into `apps/todo/app_impl.lisp`

The main application logic uses these functions to manage its state.

```vdu
(defun main ()
	...
	*running* :t
	...
	; 1. Load the configuration at startup
	(config-load)
	; 2. Use the loaded data to build the initial UI
	(populate-items)
	(layout-items *min_width* *min_height*)
	...
	; 3. Run the main event loop
	(while *running*
	    (config-update)
		...
	)
	; 4. Save the final state on exit
	(config-save)
	(gui-sub-rpc *window*)
	...)
```

*   **`populate-items`**: This function reads the `:todo`, `:done`, and
    `:deleted` lists from the loaded `*config*` and dynamically creates the UI
    items for each column.

This demonstrates the complete cycle: on exit, the text is extracted and saved.
On startup, the text is loaded and used to reconstruct the UI.

## 5. Best Practices and Evolution: From Discard to Migration

The base implementation found in the source files is robust but has a minor
drawback: when a version mismatch occurs, it discards the user's old settings
entirely. A more user-friendly best practice is to migrate old settings forward.

### The Simple (Discard) Method

```vdu
(defun config-load ()
	(if (defq stream (file-stream *config_file*))
		(setq *config* (tree-load stream)))
	; If the file is missing, invalid, OR the version is wrong,
	; discard everything and start from scratch.
	(if (or (not *config*) (/= (. *config* :find :version) *config_version*))
		(setq *config* (config-default))))
```

### The Improved (Migration) Method

This improved `config-load` function preserves user settings across updates.

```vdu
(defun config-load ()
  (defq old-config nil)
  (if (defq stream (file-stream *config_file*))
      (setq old-config (tree-load stream)))

  ; Check if a migration is needed (no file, or older version).
  (if (or (not old-config) (< (. old-config :find :version 0) *config_version*))
      (progn
        ; 1. Start with the new, complete default configuration.
        (setq *config* (config-default))

        ; 2. If an old config exists, copy its values into the new one.
        (when old-config
          (. old-config :each (lambda (key val)
                               ; Do not carry over the old version number.
                               (unless (eql key :version)
                                 (. *config* :insert key val)))))
        ; 3. Ensure the config version is now up-to-date.
        (. *config* :insert :version *config_version*))
      ; 4. If versions match, just use the loaded config.
      (setq *config* old-config)))
```

**Advantages of the Migration Method:**

*   **Preserves User Data:** A user's window position or todo list items are not
    lost just because a new setting was added.

*   **Handles New Defaults:** If a new version adds a setting (e.g.,
    `apps/eyes/app.lisp` adds `:iris_color`), it will be present from
    `config-default` while old settings like `:x` and `:y` are preserved.

*   **Handles Deprecated Settings:** Old settings that are no longer in the
    default `Emap` are simply not used, effectively being discarded without
    causing errors.

## 6. Conclusion

The ChrysaLisp `config` standard provides a consistent, simple, and effective
solution for application state persistence. By separating configuration logic
into a dedicated file and using a versioned, flexible data format, it allows
developers to easily add persistence to their applications. The `todo` app
serves as a perfect model, demonstrating how to save not just UI geometry but
also dynamic, user-created content. Adopting the migration strategy for
`config-load` further enhances this pattern, making applications more robust and
user-friendly across updates.