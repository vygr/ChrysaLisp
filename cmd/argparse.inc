;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; argparse - ChrysaLisp Argument Processor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
  +nl+        (ascii-char 10)
  +indent2+    2
  +indent4+    4
  +no_count+   0
  +max_count+  999
  +ztn+        "0123456789")

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

(defun-bind reverse (l)
  (map-rev progn l))

(defun-bind isarg? (arg)
  (starts-with "-" arg))

(defun-bind noop (&rest _) _)

; Main argparse structure template
(defun-bind parser_template ()
      (list
        (list :clz :clz_processor)
        (list :arguments (list))     ; Collection of high level arguments
        (list :commands (list))      ; Collection of high level commands
        (list :in_args nil)          ; Retain the original :command line arguments
        (list :application "")       ; Application name string
        (list :version "")           ; Application :version string
        (list :required nil)         ; Are free form values reqiured?
        (list :counter 0)            ; Number of free form (not arg or cmd) values
        (list :type :type_none)       ; Type for term validation
        (list :validator noop)       ; Argument value :type :validator fn
        (list :handler noop)         ; fn for parsed :command line result
        (list :help "")))            ; Help string

(defun-bind arg_template ()
      (list
        (list :clz :clz_argument)
        (list :argument nil)         ; The :argument flags e.g. ("-s" "--string")
        (list :required nil)         ; Are free form values reqiured?
        (list :counter +no_count+)   ; Number of values accepted for :argument
        (list :type :type_none)      ; Type for term validation
        (list :dest :argument)       ; Results key
        (list :validator noop)       ; Argument value :type :validator fn
        (list :handler noop)         ; fn for parsed :argument
        (list :help "")))            ; Help string

(defun-bind cmd_template ()
      (list
        (list :clz :clz_command)
        (list :arguments (list))     ; Arguments for :command
        (list :command "")           ; Single :command string
        (list :required nil)         ; Are free form values reqiured?
        (list :counter 0)            ; Number of values accepted for :command
        (list :type :type_none)      ; Type for term validation
        (list :dest :command)        ; Result key
        (list :validator noop)       ; Command value :type :validator fn
        (list :handler noop)         ; fn for parsed :command
        (list :help "")))            ; Help string


(defun-bind get-property-container (self prop_id)
  ; (get-property-container self prop_id)
  (some (lambda (il)
          (if (eql prop_id (first il))
              il)) self))

(defun-bind get-property (self prop_id &optional ifnil)
  ; (get-property self prop_id [ifnil])
  (defq propc (get-property-container self prop_id))
  (if (second propc)
      (second propc)
      ifnil))

(defun-bind get-class (self)
  ; (get-class self) -> clz | nil
  (get-property self :clz))

; Validation

(defun-bind validate-none (inst val)
  ; (validate-none inst val) -> exception
  (throw (str (get-class inst) " not expecting value") val))

(defun-bind validate-count (inst val)
  ; (validate-count arg-clz val) -> t | exception
  (when (= (get-property inst :counter) 0)
    (throw (str (get-class inst) " not expecting value") val))
  t)

(defun-bind validate-string (inst arg)
  ; (validate-string arg-clz arg) -> arg | exception
  (validate-count inst arg)
  (when (not (str? arg))
    (throw "Validation: expected string, found" filename))
  arg)

(defun-bind validate-integer (inst arg)
  ; (validate-integer arg-clz arg) -> integer | exception
  (validate-count inst arg)
  (defq good (reduced-reduce
    (lambda (acc el)
      (if  (find el +ztn+)
        acc
        (reduced :false))
      ) arg :true))
  (when (eql good :false)
    (throw "Validation: expected integer, found" arg))
  (str-to-num arg))

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

(defun-bind set-property (self prop_id value)
  ; (set-property self prop_id value) -> self
  (defq property (get-property-container self prop_id))
  (when property
      (elem-set 1 property value))
  self)

(defun-bind set-properties (self &rest in_props)
  ; (set-properties self [in_prop]) -> self
  (defq props (reverse in_props))
  (if (/= (logand (length props ) 1) 1)
    (while (/= (length props) 0)
           (set-property self (pop props) (pop props)))
    (throw "Uneven property pairs" in_props))
  self)

(defun-bind extend (self property value)
  ; (extend self property value)
  (defq container (get-property-container self property))
  (when container
    (push (second container) value))
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
  (defq res (get-property self container '()))
  (some (lambda (el)
                (defq argp (get-property el property))
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
    argcnt (get-property self :counter))
  (push result (get-property self :dest))
  (when (> argcnt 0)
    (defq
      cnt 0
      inner (list))
    (while
      (and
        (not (stack-empty? argstack))
        (< cnt argcnt)
        (not (get-either-container root (stack-peek argstack))))
          (push inner ((get-property self :validator) self (stack-pop argstack)))
          (setq cnt (inc cnt)))
    (if (= cnt argcnt)
      (merge result inner)
      (throw (str myflag " expected " argcnt " values. Found: ") cnt)))
  argstack)

(defun-bind consume-command (root self argstack result)
  ; (consume-command root self argstack result)
  (stack-pop argstack)
  (if (defq cmdi (get-property self :handler))
      (push result cmdi)
      (push result (get-property self :dest)))
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
            (push result ((get-property self :validator) self (stack-pop arglist)))))))
  result)

(defun-bind process-args (self arglist)
  ; (process-args self arglist)
  (if arglist
      (progn
        (defq
          res (walk self arglist)
          invoke (get-property self :handler))
        (if invoke
          (invoke self res)
          res))
      arglist))

(defun-bind argcontains (self argstring)
  (reduced-reduce
    (lambda (acc el)
      (if (sfind argstring (get-property el :argument))
          (reduced el)
          nil))
    (get-property self :arguments) '()))

(defun-bind parse (self)
  ; (parse parser)
  ; Parses and optionally executes :command line
  (defq inarg (get-property self :in_args))
  (cond
    ((opt
       (sfind "-h" inarg)
       (sfind "--help" inarg))
      ((get-property (argcontains self "-h") :handler) self))
    ((opt
       (sfind "-v" inarg)
       (sfind "--version" inarg))
      ((get-property (argcontains self "-v") :handler) self))
    ((not (get-property self inarg))
      (let ((targ (argcontains self "-h")))
       ((get-property targ :handler) self)))
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
              (push acc (get-property el :command))) cmds (list))
        ",") "}")))

(defun-bind format-usage-str (self arglist cmdstr)
  ; (format-usage-str self arglist cmdstr) -> "usage ..."
  (str (reduce
         (lambda (acc el)
           (cat acc (str "[" (first (get-property el :argument)) "] ")))
         arglist
         (str "usage: " (get-property self :application) " "))
    cmdstr " ..."))

(defun-bind dump-help (self)
  ; (dump-help self)
  ; Spits the :help tree
  (defq
    arglist (get-property self :arguments)
    cmdlist (get-property self :commands)
    cmdstr  (format-command-str cmdlist))
  ; Print usage string
  (print (format-usage-str self arglist cmdstr))
  ; Print :application :help
  (print +nl+ (get-property self :help) +nl+)
  ; Print argparse :arguments
  (print "optional :arguments:")
  (print (reduce (lambda (acc el)
                 (defq args (get-property el :argument))
                 (cat acc
                      (format-row (join args ",") (get-property el :help))
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
                            (get-property el :command)
                            (get-property el :help))
                          +nl+))
                   cmdlist ""))))

(defun-bind dump-version (self)
  ; (dump-version self)
  ; Spit the :version
  (print (get-property self :application)
         (pad "" +indent2+)
         (get-property self :version)))

(defun-bind create-argument (args arg_help &optional handler_fn)
  ; Create :argument block
  (defq instance (arg_template))
  (set-properties instance
                  :argument args
                  :help arg_help
                  :handler (opt handler_fn noop)))

(defun-bind create-command (cmds cmd_help)
  ; Create :command block
  (defq instance (cmd_template))
  (set-properties instance
                  :command cmds
                  :help cmd_help))

(defun-bind create-argparse (app_name app_version argstring)
  ; Creates the :argument processor
  (defq
    instance (parser_template)
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
