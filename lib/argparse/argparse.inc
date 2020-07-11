;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; argparse - ChrysaLisp Argument Processor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import 'lib/xtras/xtras.inc)

(defq
  +nl+        (const (ascii-char 10))
  +indent2+    (const 2)
  +indent4+    (const 4)
  +no_count+   (const 0)
  +max_count+  (const 999)
  +ztn+        (const "0123456789"))

; Potential utility macros to embed in ChrysaLisp

(defmacro seq? (form)
  ; (seq? form) -> t|nil
  `(or (lst? ,form) (str? ,form)))

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

(defmacro-bind base-obj (&optional name)
  (defq nm (if name name (gensym)))
  `(macroexpand (make-dict ,nm)))

; Main argparse structure template

(defun-bind parser-dict()
  ; (parser-dict) -> dict
  (defq pd (base-obj))
  (def pd
    :clz :clz_processor   ; Argparse class
    :arguments (list)     ; Collection of arguments
    :commands (list)      ; Collection of commands
    :in_args ""           ; Original string preserv
    :application ""       ; Placeholder for app name
    :version ""           ; Placeholder for version
    :required nil         ; Does it requires free form input?
    :counter +no_count+   ; Max freeform input
    :type :type_none      ; Type of freeform input
    :validator noop       ; Free form value validator
    :handler noop         ; Handler for resulting parsed input
    :help "")             ; Placeholder for help text
  pd)

(defun-bind cmd-dict()
  ; (cmd-dict) -> dict
  (defq cd (base-obj))
  (def cd
    :clz :clz_command     ; Argparse class
    :arguments (list)     ; Collection of arguments
    :command ""           ; Command string
    :required nil         ; Does it requires free form input?
    :counter +no_count+   ; Max freeform input
    :type :type_none      ; Type expected of freeform input
    :dest :command        ; Results key
    :validator noop       ; Validation function
    :handler noop         ; Handler for resulting parsed input
    :help "")             ; Placeholder for help text
  cd)

(defun-bind arg-dict()
  ; (arg-dict) -> dict
  (defq ad (base-obj))
  (def ad
    :clz :clz_argument    ; Argparse class
    :argument nil         ; The :argument flags e.g. ("-s" "--string")
    :required nil         ; Required ?
    :counter +no_count+   ; Max count of args following switch
    :type :type_none      ; Type of args
    :dest :argument       ; Results key
    :validator noop       ; Validator function
    :handler noop         ; Handler function
    :help "")             ; Placeholder for help text
  ad)

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
  (dict-get self :clz :clz_unknown))

; Validation

(defun-bind validate-none (inst val)
  ; (validate-none inst val) -> exception
  (throw (str (get-class inst) " not expecting value") val))

(defun-bind validate-count (inst val)
  ; (validate-count arg-clz val) -> t | exception
  (when (= (dict-get inst :counter) 0)
    (throw (str (get-class inst) " not expecting value") val))
  t)

(defun-bind validate-string (inst arg)
  ; (validate-string arg-clz arg) -> arg | exception
  (validate-count inst arg)
  (when (not (str? arg))
    (throw "Validation: expected string, found" filename))
  arg)

(defun str-is-ints? (s)
  ; (str-is-ints? s) -> :true | :false
  (reduced-reduce
      (lambda (acc el)
        (if  (find el +ztn+)
          acc
          (reduced :false))
        ) s :true))

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

(defun-bind set-properties (self &rest in_props)
  ; (set-properties self [in_prop]) -> self
  (defq props (reverse in_props))
  (if (/= (logand (length props ) 1) 1)
    (while (/= (length props) 0)
           (dict-put! self (pop props) (pop props) t))
    (throw "Uneven property pairs" in_props))
  self)

(defun-bind extend (self property value)
  ; (extend self property value)
  (defq container (dict-get self property))
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
  ; For a containing :type, find a match of
  ; value for a specific properties value
  (defq res (dict-get self container '()))
  (some (lambda (el)
                (defq argp (dict-get el property))
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
    argcnt (dict-get self :counter))
  (push result (dict-get self :dest))
  (when (> argcnt 0)
    (defq
      cnt 0
      inner (list))
    (while
      (and
        (not (stack-empty? argstack))
        (< cnt argcnt)
        (not (get-either-container root (stack-peek argstack))))
          (push inner ((dict-get self :validator) self (stack-pop argstack)))
          (setq cnt (inc cnt)))
    (if (= cnt argcnt)
      (merge result inner)
      (throw (str myflag " expected " argcnt " values. Found: ") cnt)))
  argstack)

(defun-bind consume-command (root self argstack result)
  ; (consume-command root self argstack result)
  (stack-pop argstack)
  (if (defq cmdi (dict-get self :handler))
      (push result cmdi)
      (push result (dict-get self :dest)))
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
            (push result ((dict-get self :validator) self (stack-pop arglist)))))))
  result)

(defun-bind process-args (self arglist)
  ; (process-args self arglist)
  (defq
    workers (or
      (> (length (dict-get self :arguments)) 2)
      (> (length (dict-get self :commands)) 0)
      (> (dict-get self :counter) +no_count+))
    hndlr (dict-get self :handler)
    rarg  (reverse arglist))
  (if (and arglist workers)
      (progn
        (defq res (walk self arglist))
        (if hndlr (hndlr self res) res))
      (if hndlr (hndlr self rarg) rarg)))

(defun-bind argcontains (self argstring)
  (reduced-reduce
    (lambda (acc el)
      (if (sfind argstring (dict-get el :argument))
          (reduced el)
          nil))
    (dict-get self :arguments) '()))

(defun-bind parse (self)
  ; (parse self)
  ; Parses and optionally executes :command line
  (defq inarg (dict-get self :in_args))
  (cond
    ((opt
       (sfind "-h" inarg)
       (sfind "--help" inarg))
      ((dict-get (argcontains self "-h") :handler) self))
    ((opt
       (sfind "-v" inarg)
       (sfind "--version" inarg))
      ((dict-get (argcontains self "-v") :handler) self))
    ((stack-empty? inarg)
       ((dict-get (argcontains self "-h") :handler) self))
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
              (push acc (dict-get el :command))) cmds (list))
        ",") "}")))

(defun-bind format-usage-str (self arglist cmdstr)
  ; (format-usage-str self arglist cmdstr) -> "usage ..."
  (str (reduce
         (lambda (acc el)
           (cat acc (str "[" (first (dict-get el :argument)) "] ")))
         arglist
         (str "usage: "
          (dict-get self :application)
          " "))
    cmdstr " ..."))

(defun-bind dump-help (self)
  ; (dump-help self)
  ; Spits the :help tree
  (defq
    arglist (dict-get self :arguments)
    cmdlist (dict-get self :commands)
    cmdstr  (format-command-str cmdlist))
  ; Print usage string
  (print (format-usage-str self arglist cmdstr))
  ; Print :application :help
  (print +nl+ (dict-get self :help) +nl+)
  ; Print argparse :arguments
  (print "optional arguments:")
  (print (reduce (lambda (acc el)
                 (defq args (dict-get el :argument))
                 (cat acc
                      (format-row (join args ",") (dict-get el :help))
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
                            (dict-get el :command)
                            (dict-get el :help))
                          +nl+))
                   cmdlist "")))
  (list "-h"))

(defun-bind dump-version (self)
  ; (dump-version self)
  ; Spit the :version
  (print (dict-get self :application)
         (pad "" +indent2+)
         (dict-get self :version))
  (list "-v"))

(defun-bind create-argument (args arg_help &optional handler_fn)
  ; Create :argument block
  (defq instance (arg-dict))
  (set-properties instance
                  :argument args
                  :help arg_help
                  :handler (opt handler_fn noop)))

(defun-bind create-command (cmds cmd_help)
  ; Create :command block
  (defq instance (cmd-dict))
  (set-properties instance
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
  (add-argument instance (set-properties ha :type :type_none))
  (add-argument instance (set-properties va :type :type_none))
  (set-properties instance
                  :application app_name
                  :version app_version
                  :in_args (reverse argstring)))
