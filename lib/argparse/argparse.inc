;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; argparse - ChrysaLisp Argument Processor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import 'lib/xtras/xtras.inc)

(defq
  +indent2+   2
  +indent4+   4
  +no_count+  0
  +max_count+ 999)

; Potential utility macros to embed in ChrysaLisp

(defmacro sfind (ss slst)
  `(some (lambda (_tvar)
    (if (eql ,ss _tvar) _)) ,slst))

(defmacro stack-pop (stack)
  ; (stack-pop stack) -> el|nil
  `(pop ,stack))

(defmacro stack-push (stack el)
  ; (stack-push stack) -> el|nil
  `(push ,stack ,el))

(defmacro stack-empty? (stack)
  `(= (length ,stack) 0))

(defmacro stack-peek (stack)
  ; (stack-peek stack) -> el|nil
  `(if (not (stack-empty? ,stack))
    (last ,stack)
    nil))

; Local sugar wrappers

(defun-bind isarg? (arg)
  (starts-with "-" arg))

(defun-bind noop (&rest _) _)

(defun-bind dump-argparse-object (object)
  ; (dump-argparse-object object) -> nil
  (defq plist (dict-property-list object))
  (each (lambda (el)
    (defq
      k (first el)
      l (second el))
    (cond
      ((and (lst? l) (eql (first l) 'lambda))
        (print k " function" ))
      (t (print k " " l))))
    nil))

(defun-bind dump-argparse-objects (objects)
  (each dump-argparse-object objects))

(defun-bind get-class (self)
  ; (get-class self) -> :clz_n | :clz_unknown
  (getp self :clz :clz_unknown))

; Validation

(defun-bind validate-none (inst val)
  ; (validate-none inst val) -> exception
  (throw (str (get-class inst) " not expecting value") val))

(defun-bind validate-count (inst val)
  ; (validate-count arg-clz val) -> t | exception
  (when (= (getp inst :counter) 0)
    (throw (str (get-class inst) " not expecting value") val))
  t)

(defun-bind validate-string (inst arg)
  ; (validate-string arg-clz arg) -> arg | exception
  (validate-count inst arg)
  (when (not (str? arg))
    (throw "Validation: expected string, found" filename))
  arg)

(defun-bind validate-integer-pos (inst arg)
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
  (str-to-num wrk))

(defun-bind validate-integer-any (inst arg)
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
    (str-to-num arg)
    (str-to-num wrk)))

(defun-bind validate-float (inst arg)
  ; (validate-float arg-clz arg) -> float | exception
  (validate-count inst arg)
  (print "Float validation not yet implemented")
  arg)

(defun-bind validate-boolean (inst arg)
  ; (validate-boolean arg-clz arg) -> boolean | exception
  (validate-count inst arg)
  (print "Validating as boolean = " (str? arg))
  arg)

(defun-bind validate-file (inst filename)
  ; (validate-file arg-clz filename) -> filename | exception
  (validate-count inst filename)
  (when (or
          (eql filename nil)
          (= (length filename) 0)
          (not (str? filename)))
    (throw "Validation: expected filename, found" filename))
  filename)

(defun-bind validate-file-exists (inst filename)
  ; (validate-file-exists arg-clz filename) -> filename | exception
  (validate-file inst filename)
  (when (= (age filename) 0)
    (throw "Validation: file not found error" filename))
  filename)

(defun-bind validate-file-not-exists (inst filename)
  ; (validate-file-not-exists arg-clz filename) -> filename | exception
  (validate-file inst filename)
  (when (/= (age filename) 0)
    (throw "Validation: file exists error" filename))
  filename)

; (defmacro-bind base-obj (&optional name)
;   (defq nm (if name name (gensym)))
;   `(macroexpand (make-dict ,nm)))

; Main argparse structure template

(defun-bind parser-dict()
  ; (parser-dict) -> dict
  (properties
    :clz :clz_processor   ; Argparse class
    :arguments (list)     ; Collection of arguments
    :commands (list)      ; Collection of commands
    :in_args ""           ; Original string preserv
    :application ""       ; Placeholder for app name
    :version ""           ; Placeholder for version
    :required nil         ; Does it requires free form input?
    :counter +no_count+   ; Max freeform input
    :validator validate-none       ; Free form value validator
    :handler noop         ; Handler for resulting parsed input
    :help ""))             ; Placeholder for help text

(defun-bind cmd-dict()
  ; (cmd-dict) -> dict
  (properties
    :clz :clz_command     ; Argparse class
    :arguments (list)     ; Collection of arguments
    :command ""           ; Command string
    :required nil         ; Does it requires free form input?
    :counter +no_count+   ; Max freeform input
    :dest :command        ; Results key
    :validator validate-none       ; Validation function
    :handler noop         ; Handler for resulting parsed input
    :help ""))             ; Placeholder for help text

(defun-bind arg-dict()
  ; (arg-dict) -> dict
  (properties
    :clz :clz_argument    ; Argparse class
    :argument nil         ; The :argument flags e.g. ("-s" "--string")
    :required nil         ; Required ?
    :counter +no_count+   ; Max count of args following switch
    :dest :argument       ; Results key
    :validator noop       ; Validator function
    :handler noop         ; Handler function
    :help ""))             ; Placeholder for help text

(defun-bind set-properties (self &rest in_props)
  ; (set-properties self [in_prop]) -> self
  (defq props (reverse in_props))
  (if (/= (logand (length props ) 1) 1)
    (while (/= (length props) 0)
           (setp! self (pop props) (pop props) t))
    (throw "Uneven property pairs" in_props))
  self)

(defun-bind extend (self property value)
  ; (extend self property value)
  (defq container (getp self property))
  (when container
    (push container value))
  self)

(defun-bind add-argument (self in_argument)
  ; (add-argument self in argument)
  (extend self :arguments in_argument))

(defun-bind add-command (self in_cmd)
  ; (add-command self in_cmd)
  (extend self :commands in_cmd))

(defun-bind container-for (self value container property)
  ; (container-for self value container property)
  ; For a containing type, find a match of
  ; value for a specific properties value
  (defq res (getp self container '()))
  (some (lambda (el)
                (defq argp (getp el property))
                (if (or (sfind value argp) (eql value argp))
                    el))
              res))

(defun-bind get-either-container (self value)
  ; (get-either-container self value)
  (if (defq res (container-for self value :commands :command))
      res
      (container-for self value :arguments :argument)))

(defun-bind consume-argument (root self argstack result)
  ; (consume-argument root self argstack result)
  (defq
    myflag (stack-pop argstack)
    argcnt (getp self :counter))
  (push result (getp self :dest))
  (when (> argcnt 0)
    (defq
      cnt 0
      inner (list))
    (while
      (and
        (not (stack-empty? argstack))
        (< cnt argcnt)
        (not (get-either-container root (stack-peek argstack))))
          (push inner ((getp self :validator) self (stack-pop argstack)))
          (setq cnt (inc cnt)))
    (if (= cnt argcnt)
      (merge-obj result inner)
      (throw (str myflag " expected " argcnt " values. Found: ") cnt)))
  argstack)

(defun-bind consume-command (root self argstack result)
  ; (consume-command root self argstack result)
  (stack-pop argstack)
  (if (defq cmdi (getp self :handler))
      (push result cmdi)
      (push result (getp self :dest)))
  (defq cmdres (list))
  (push result (walk self argstack cmdres))
  argstack)

(defun-bind walk (self arglist &optional result)
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
            (push result ((getp self :validator) self (stack-pop arglist)))))))
  result)

(defun-bind process-args (self arglist)
  ; (process-args self arglist)
  (defq
    workers (or
      (> (length (getp self :arguments)) 2)
      (> (length (getp self :commands)) 0)
      (> (getp self :counter) +no_count+))
    hndlr (getp self :handler)
    rarg  (reverse arglist))
  (if (and arglist workers)
      (progn
        (defq res (walk self arglist))
        (if hndlr (hndlr self res) res))
      (if hndlr (hndlr self rarg) rarg)))

(defun-bind argcontains (self argstring)
  (reduced-reduce
    (lambda (acc el)
      (if (sfind argstring (getp el :argument))
          (reduced el)
          nil))
    (getp self :arguments) '()))

(defun-bind parse (self)
  ; (parse self)
  ; Parses and optionally executes :command line
  (defq inarg (getp self :in_args))
  (cond
    ((opt
       (sfind "-h" inarg)
       (sfind "--help" inarg))
      ((getp (argcontains self "-h") :handler) self))
    ((opt
       (sfind "-v" inarg)
       (sfind "--version" inarg))
      ((getp (argcontains self "-v") :handler) self))
    ((stack-empty? inarg)
       ((getp (argcontains self "-h") :handler) self))
    (t
      (let ((cargs (copy inarg)))
        (catch (process-args self cargs) (print _))))))

; Help dump

(defun-bind format-row (arg arg:help)
  (defq colbuff (-  30 (+ (length arg) +indent4+)))
  (str (pad "" +indent4+) arg (pad "" colbuff) arg:help))

(defun-bind format-command-str (cmds)
  ; (format-command-str seq) -> "{el [,el]}"
  (if (= (length cmds) 0)
    "{}"
    (str "{" (join (reduce (lambda (acc el)
              (push acc (getp el :command))) cmds (list))
        ",") "}")))

(defun-bind format-usage-str (self arglist cmdstr)
  ; (format-usage-str self arglist cmdstr) -> "usage ..."
  (str (reduce
         (lambda (acc el)
           (cat acc (str "[" (first (getp el :argument)) "] ")))
         arglist
         (str "usage: "
          (getp self :application)
          " "))
    cmdstr " ..."))

(defun-bind dump-help (self)
  ; (dump-help self)
  ; Spits the :help tree
  (defq
    arglist (getp self :arguments)
    cmdlist (getp self :commands)
    cmdstr  (format-command-str cmdlist))
  ; Print usage string
  (print (format-usage-str self arglist cmdstr))
  ; Print :application :help
  (print +nl+ (getp self :help) +nl+)
  ; Print argparse :arguments
  (print "optional arguments:")
  (print (reduce (lambda (acc el)
                 (defq args (getp el :argument))
                 (cat acc
                      (format-row (join args ",") (getp el :help))
                      +nl+))
                 arglist ""))
  ; Print argparse :commands
  ; TODO: Need additional {cmd [,cmd]} when memory exception fixed
  (when (/= (length cmdlist) 0)
    (print "Actions:" +nl+
           (pad "" +indent2+) cmdstr +nl+
           (reduce (lambda (acc el)
                     (cat acc
                          (format-row
                            (getp el :command)
                            (getp el :help))
                          +nl+))
                   cmdlist "")))
  (list "-h"))

(defun-bind dump-version (self)
  ; (dump-version self)
  ; Spit the :version
  (print (getp self :application)
         (pad "" +indent2+)
         (getp self :version))
  (list "-v"))

(defun-bind create-argument (args arg_help &optional handler_fn)
  ; Create :argument block
  (setsp! (arg-dict)
    :argument args
    :help arg_help
    :handler (opt handler_fn noop)))

(defun-bind create-command (cmds cmd_help)
  ; Create :command block
  (setsp! (cmd-dict)
    :command cmds
    :help cmd_help))

(defun-bind create-argparse (app_name app_version argstring)
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
  (setsp! instance
    :application app_name
    :version app_version
    :in_args (reverse argstring)))
