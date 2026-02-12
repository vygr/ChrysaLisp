(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/text/document.inc")

(defq usage `(
(("-h" "--help")
"Usage: edit [options] [path] ...

    options:
        -h --help: this help info.
        -j --jobs num: max jobs per batch, default 1.
        -c --cmd '...': command commands to execute.
        -s --script path: file containing commands to execute.

    Command line text editor.
    The script is compiled into a function and executed in a
    custom environment populated with editing primitives.

    Available Commands:
    
    Search:     (find pattern [:w] [:r]) 
    Cursors:    (cursors) (add-cursors)

    Selection:  (select-all) (select-line) (select-word) (select-block)
                (select-form) (select-para)
                (select-left) (select-right) (select-up) (select-down)
                (select-home) (select-end) (select-top) (select-bottom)

    Navigation: (top) (bottom) (up) (down) (left) (right) (home) (end)
                (bracket-left) (bracket-right) (ws-left) (ws-right)

    Mutation:   (insert text) (delete) (backspace) (trim) (sort)
                (unique) (upper) (lower) (reflow) (split) (comment)
                (indent) (outdent)

    Properties: (get-text) (get-filename)

    Example - Numbering lines:
    edit -c \q(top) (defq i 0) (while (down) (home) (insert (str (++ i) ': '))) \q file.txt"
    )
(("-j" "--jobs") ,(opt-num 'opt_j))
(("-c" "--cmd") ,(opt-str 'opt_c))
(("-s" "--script") ,(opt-str 'opt_s))
))

(defun setup-edit-env ()
    ; Programmatically define simple proxy commands
    ; These functions rely on *doc* being defined in the caller's scope
    (each (lambda ((name method))
            (eval (static-qq (defun ,name () (. *doc* ,method)))))
          '((top :top) (bottom :bottom) (up :up) (down :down)
            (left :left) (right :right) (home :home) (end :end)
            (bracket-left :left_bracket) (bracket-right :right_bracket)
            (ws-left :left_white_space) (ws-right :right_white_space)
            
            (select-all :select_all) (select-line :select_line)
            (select-word :select_word) (select-block :select_block)
            (select-form :select_form) (select-para :select_paragraph)
            (select-left :left_select) (select-right :right_select)
            (select-up :up_select) (select-down :down_select)
            (select-home :home_select) (select-end :end_select)
            (select-top :top_select) (select-bottom :bottom_select)

            (delete :delete) (backspace :backspace) (trim :trim)
            (sort :sort) (unique :unique) (upper :to_upper) (lower :to_lower)
            (reflow :reflow) (split :split) (comment :comment)
            (indent :right_tab) (outdent :left_tab)))

    ; Define complex commands
    (defun insert (txt) (. *doc* :insert txt))
    (defun find (pattern &rest flags)
        (. *doc* :find pattern (find :w flags) (find :r flags)))
    (defun cursors ()
        (. *doc* :set_found_cursors (. *doc* :get_buffer_found)))
    (defun add-cursors ()
        (. *doc* :add_found_cursors (. *doc* :get_buffer_found)))
    (defun get-text () (. *doc* :get_select))
    (defun get-filename () *file*)
    (defun print (&rest args)
        (apply print (if args args (list (. *doc* :get_select))))))

(defun work (file)
    ; *doc* and *file* are bound here, visible to edit-script
    ; because edit-script executes in this scope
    (defq *doc* (Document) *file* file)
    (catch
        (progn
            ; Load
            (. *doc* :stream_load (file-stream file))
            ; Run the compiled user script
            (edit-script)
            ; Save
            (. *doc* :stream_save (file-stream file +file_open_write))
            (print "Edited: " file))
        (print "Error editing " file ": " _)))

(defun main ()
    ; Initialize options
    (when (and
            (defq stdio (create-stdio))
            (defq opt_j 1 opt_c :nil opt_s :nil args (options stdio usage)))
        ; Get file list (args or stdin)
        (if (empty? (defq jobs (rest args)))
            (lines! (# (push jobs %0)) (io-stream 'stdin)))
        ; Prepare the script stream
        (defq script_stream (memory-stream))
        (if opt_c (write-blk script_stream opt_c))
        (if opt_s (write-blk script_stream (load opt_s)))
        (when (and (> (stream-seek script_stream 0 2) 0) (nempty? jobs))
            (stream-seek script_stream 0 0)
            ; Parse script into a list of forms
            (defq forms (list))
            (while (defq r (read script_stream)) (push forms (first r)))
            ; Create the editing environment
            (env-push)
            (setup-edit-env)
            ; Compile user script into a function 'edit-script'
            ; (defun edit-script () (progn ...forms...))
            (eval (list 'defun 'edit-script '() (push (reverse forms) 'progn)))
            (if (<= (length jobs) opt_j)
                ; Run locally
                (each (const work) jobs)
                ; Distribute to farm
                (each (lambda ((job result)) (prin result))
                    (pipe-farm (map (# (str (first args)
                            " -j " opt_j
                            (if opt_c (cat " -c \q" opt_c "\q") "")
                            (if opt_s (cat " -s " opt_s) "")
                            " " (slice (str %0) 1 -2)))
                        (partition jobs opt_j)))))
            ; Clean up environment
            (env-pop))))