;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; argparse - ChrysaLisp Argument Processor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/xtras/xtras.inc")

(defq
  +indent2   2
  +indent4   4
  +no_count  0
  +max_count 999)

; Potential utility macros to embed in ChrysaLisp

(defmacro sfind (ss slst)
  `(some (lambda (_tvar)
    (if (eql ,ss _tvar) _)) ,slst))

(defmacro stack-pop (stack)
  ; (stack-pop stack) -> el | nil
  `(pop ,stack))

(defmacro stack-push (stack el)
  ; (stack-push stack) -> el | nil
  `(push ,stack ,el))

(defmacro stack-empty? (stack)
  `(= (length ,stack) 0))

(defmacro stack-peek (stack)
  ; (stack-peek stack) -> el | nil
  `(if (not (stack-empty? ,stack))
    (last ,stack)
    nil))

; Local sugar wrappers

(defun isarg? (arg)
  (starts-with "-" arg))

(defun noop (&rest _) _)

(defun get-class (self)
  ; (get-class self) -> :clz_n | :clz_unknown
  (gets self :clz :clz_unknown))

; Validation

(defun validate-none (inst val)
  ; (validate-none inst val) -> exception
  (throw (str (get-class inst) " not expecting value") val))

(defun validate-count (inst val)
  ; (validate-count arg-clz val) -> t | exception
  (when (= (gets inst :counter) 0)
    (throw (str (get-class inst) " not expecting value") val))
  t)

(defun validate-string (inst arg)
  ; (validate-string arg-clz arg) -> arg | exception
  (validate-count inst arg)
  (when (not (str? arg))
    (throw "Validation: expected string, found" filename))
  arg)

(defun validate-integer-pos (inst arg)
  ; (validate-integer-pos arg-clz arg) -> integer | exception
  (validate-count inst arg)
  (defq
    good :true
    wrk  (copy arg)
    negi (when (or (eql (first arg) "-") (eql (first arg) "+"))
      (setq wrk (slice 1 -1 arg))
      (eql (first arg) "-")))
  (setq good
    (if (or (find "." wrk) negi)
      :false
      (str-is-ints? wrk)))
  (when (eql good :false)
    (throw "Validation: expected positive integer, found" arg))
  (str-as-num wrk))

(defun validate-integer-any (inst arg)
  ; (validate-integer-any arg-clz arg) -> integer | exception
  (validate-count inst arg)
  (defq
    good :true
    wrk  (copy arg)
    negi (when (or (eql (first arg) "-") (eql (first arg) "+"))
      (setq wrk (slice 1 -1 arg))
      (eql (first arg) "-")))
  (setq good
    (if (find "." wrk)
      :false
      (str-is-ints? wrk)))
  (when (eql good :false)
    (throw "Validation: expected integer, found" arg))
  (if negi
    (str-as-num arg)
    (str-as-num wrk)))

(defun validate-float (inst arg)
  ; (validate-float arg-clz arg) -> float | exception
  (validate-count inst arg)
  (print "Float validation not yet implemented")
  arg)

(defun validate-boolean (inst arg)
  ; (validate-boolean arg-clz arg) -> boolean | exception
  (validate-count inst arg)
  (print "Validating as boolean = " (str? arg))
  arg)

(defun validate-file (inst filename)
  ; (validate-file arg-clz filename) -> filename | exception
  (validate-count inst filename)
  (when (or
          (eql filename nil)
          (= (length filename) 0)
          (not (str? filename)))
    (throw "Validation: expected filename, found" filename))
  filename)

(defun validate-file-exists (inst filename)
  ; (validate-file-exists arg-clz filename) -> filename | exception
  (validate-file inst filename)
  (when (= (age filename) 0)
    (throw "Validation: file not found error" filename))
  filename)

(defun validate-file-not-exists (inst filename)
  ; (validate-file-not-exists arg-clz filename) -> filename | exception
  (validate-file inst filename)
  (when (/= (age filename) 0)
    (throw "Validation: file exists error" filename))
  filename)

; (defmacro base-obj (&optional name)
;   (defq nm (if name name (gensym)))
;   `(macroexpand (make-dict ,nm)))

; Main argparse structure template

(defun parser-dict()
  ; (parser-dict) -> dict
  (emap-kv
    :clz :clz_processor   ; Argparse class
    :arguments (list)     ; Collection of arguments
    :commands (list)      ; Collection of commands
    :in_args ""           ; Original string preserv
    :application ""       ; Placeholder for app name
    :version ""           ; Placeholder for version
    :required nil         ; Does it requires free form input?
    :counter +no_count   ; Max freeform input
    :validator validate-none       ; Free form value validator
    :handler noop         ; Handler for resulting parsed input
    :help ""))             ; Placeholder for help text

(defun cmd-dict()
  ; (cmd-dict) -> dict
  (emap-kv
    :clz :clz_command     ; Argparse class
    :arguments (list)     ; Collection of arguments
    :command ""           ; Command string
    :required nil         ; Does it requires free form input?
    :counter +no_count   ; Max freeform input
    :dest :command        ; Results key
    :validator validate-none       ; Validation function
    :handler noop         ; Handler for resulting parsed input
    :help ""))             ; Placeholder for help text

(defun arg-dict()
  ; (arg-dict) -> dict
  (emap-kv
    :clz :clz_argument    ; Argparse class
    :argument nil         ; The :argument flags e.g. ("-s" "--string")
    :required nil         ; Required ?
    :counter +no_count   ; Max count of args following switch
    :dest :argument       ; Results key
    :validator noop       ; Validator function
    :handler noop         ; Handler function
    :help ""))             ; Placeholder for help text

(defun set-properties (self &rest in_props)
  ; (set-properties self [in_prop]) -> self
  (defq props (reverse in_props))
  (if (/= (logand (length props ) 1) 1)
    (while (/= (length props) 0)
           (sets! self (pop props) (pop props)))
    (throw "Uneven property pairs" in_props))
  self)

(defun extend (self property value)
  ; (extend self property value)
  (defq container (gets self property))
  (when container
    (push container value))
  self)

(defun add-argument (self in_argument)
  ; (add-argument self in argument)
  (extend self :arguments in_argument))

(defun add-command (self in_cmd)
  ; (add-command self in_cmd)
  (extend self :commands in_cmd))

(defun container-for (self value container property)
  ; (container-for self value container property)
  ; For a containing type, find a match of
  ; value for a specific properties value
  (defq res (gets self container '()))
  (some (lambda (el)
                (defq argp (gets el property))
                (if (or (sfind value argp) (eql value argp))
                    el))
              res))

(defun get-either-container (self value)
  ; (get-either-container self value) -> command | argument
  (opt
    (container-for self value :commands :command)
    (container-for self value :arguments :argument)))

(defun consume-argument (root self argstack result)
  ; (consume-argument root self argstack result)
  (defq
    myflag (stack-pop argstack)
    argcnt (gets self :counter))
  (push result (gets self :dest))
  (when (> argcnt 0)
    (defq
      cnt 0
      inner (list))
    (while
      (and
        (not (stack-empty? argstack))
        (< cnt argcnt)
        (not (get-either-container root (stack-peek argstack))))
          (push inner ((gets self :validator) self (stack-pop argstack)))
          (setq cnt (inc cnt)))
    (if (= cnt argcnt)
      (merge-obj result inner)
      (throw (str myflag " expected " argcnt " values. Found: ") cnt)))
  argstack)

(defun consume-command (root self argstack result)
  ; (consume-command root self argstack result)
  (stack-pop argstack)
  (if (defq cmdi (gets self :handler))
      (push result cmdi)
      (push result (gets self :dest)))
  (defq cmdres (list))
  (push result (walk self argstack cmdres))
  argstack)

(defun walk (self arglist &optional result)
  ; (walk-arguments self arglist)
  (setd result (list))
  (while (/= (length arglist) 0)
         (defq
          current (stack-peek arglist)
          arg_object (container-for self current :arguments :argument)
          cmd_object (container-for self current :commands :command))
         ; (print "processing " current)
         (cond
          (arg_object
            (setq arglist (consume-argument self arg_object arglist result)))
           (cmd_object
            (setq arglist (consume-command self cmd_object arglist result)))
           ((isarg? current)
            (throw "Unrecognized flag " current))
           (t (progn
            (push result ((gets self :validator) self (stack-pop arglist)))))))
  result)

(defun process-args (self arglist)
  ; (process-args self arglist)
  (defq
    workers (or
      (> (length (gets self :arguments)) 2)
      (> (length (gets self :commands)) 0)
      (> (gets self :counter) +no_count))
    hndlr (gets self :handler)
    rarg  (reverse arglist))
  (if (and arglist workers)
      (progn
        (defq res (walk self arglist))
        (if hndlr (hndlr self res) res))
      (if hndlr (hndlr self rarg) rarg)))

(defun argcontains (self argstring)
  (reduced-reduce
    (lambda (acc el)
      (if (sfind argstring (gets el :argument))
          (reduced el)
          nil))
    (gets self :arguments) '()))

(defun parse (self)
  ; (parse self)
  ; Parses and optionally executes :command line
  (defq inarg (gets self :in_args))
  (cond
    ((opt
       (sfind "-h" inarg)
       (sfind "--help" inarg))
      ((gets (argcontains self "-h") :handler) self))
    ((opt
       (sfind "-v" inarg)
       (sfind "--version" inarg))
      ((gets (argcontains self "-v") :handler) self))
    ((stack-empty? inarg)
       ((gets (argcontains self "-h") :handler) self))
    (t
      (let ((cargs (copy inarg)))
        (catch (process-args self cargs) (print _))))))

; Help dump

(defun format-row (arg arg:help)
  (defq colbuff (-  30 (+ (length arg) +indent4)))
  (str (pad "" +indent4) arg (pad "" colbuff) arg:help))

(defun format-command-str (cmds)
  ; (format-command-str seq) -> "{el [,el]}"
  (if (= (length cmds) 0)
    "{}"
    (str "{" (join (reduce (lambda (acc el)
              (push acc (gets el :command))) cmds (list))
        ",") "}")))

(defun format-usage-str (self arglist cmdstr)
  ; (format-usage-str self arglist cmdstr) -> "Usage ..."
  (str (reduce
         (lambda (acc el)
           (cat acc (str "[" (first (gets el :argument)) "] ")))
         arglist
         (str "Usage: "
          (gets self :application)
          " "))
    cmdstr " ..."))

(defun dump-help (self)
  ; (dump-help self)
  ; Spits the :help tree
  (defq
    arglist (gets self :arguments)
    cmdlist (gets self :commands)
    cmdstr  (format-command-str cmdlist))
  ; Print usage string
  (print (format-usage-str self arglist cmdstr))
  ; Print :application :help
  (print +nl (gets self :help) +nl)
  ; Print argparse :arguments
  (print "optional arguments:")
  (print (reduce (lambda (acc el)
                 (defq args (gets el :argument))
                 (cat acc
                      (format-row (join args ",") (gets el :help))
                      +nl))
                 arglist ""))
  ; Print argparse :commands
  ; TODO: Need additional {cmd [,cmd]} when memory exception fixed
  (when (/= (length cmdlist) 0)
    (print "Actions:" +nl
           (pad "" +indent2) cmdstr +nl
           (reduce (lambda (acc el)
                     (cat acc
                          (format-row
                            (gets el :command)
                            (gets el :help))
                          +nl))
                   cmdlist "")))
  (list "-h"))

(defun dump-version (self)
  ; (dump-version self)
  ; Spit the :version
  (print (gets self :application)
         (pad "" +indent2)
         (gets self :version))
  (list "-v"))

(defun create-argument (args arg_help &optional handler_fn)
  ; Create :argument block
  (sets-pairs! (arg-dict)
    :argument args
    :help arg_help
    :handler (opt handler_fn noop)))

(defun create-command (cmds cmd_help)
  ; Create :command block
  (sets-pairs! (cmd-dict)
    :command cmds
    :help cmd_help))

(defun create-argparse (app_name app_version argstring)
  ; Creates the :argument processor
  (defq
    instance (parser-dict)
    ha (create-argument
                 '("-h" "--help")
                 "displays application help and exits"
                 dump-help)
    va (create-argument
                 '("-v" "--version")
                 "displays application version and exits"
                 dump-version))
  ; Settings (can be overridden)
  (add-argument instance ha)
  (add-argument instance va)
  (sets-pairs! instance
    :application app_name
    :version app_version
    :in_args (reverse argstring)))
