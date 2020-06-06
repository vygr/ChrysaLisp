;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; argparse - ChrysaLisp Argument Processor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +nl+      (ascii-char 10))
(defq +indent2+    2)
(defq +indent4+    4)

; Potential utility macros to embed in ChrysaLisp

(defmacro seq? (form)
  ; (seq? form) -> t|nil
  `(or (lst? ,form) (str? ,form)))

(defmacro stack-peek (stack)
  ; (stack-peek stack) -> el|nil
  `(last ,stack))

(defmacro stack-pop (stack)
  ; (stack-pop stack) -> el|nil
  `(pop ,stack))

(defmacro stack-push (stack el)
  ; (stack-push stack) -> el|nil
  `(push ,stack ,el))

(defmacro stack-empty? (stack)
  `(= (length ,stack) 0))

(defmacro sfind (ss slst)
    `(some (lambda (_tvar)
           (if (eql ,ss _tvar) _)) ,slst))

; Local sugar wrappers

(defun-bind reverse (l)
  (map-rev progn l))

(defun-bind isarg? (arg)
  (starts-with "-" arg))

; DSL
(def: '(type_none
        type_string
        type_integer
        type_float
        type_boolean
        type_file
        arguments
        in_args
        commands
        application
        version
        help
        argument
        counter
        dest
        type
        validator
        handler
        command) (env))

(defun noop (&rest _) _)

; Main argparse structure template
(defun parser_template ()
      (list
        (list arguments (list))     ; Collection of high level arguments
        (list commands (list))      ; Collection of high level commands
        (list in_args nil)          ; Retain the original command line arguments
        (list application "")       ; Application name string
        (list version "")           ; Application version string
        (list counter 0)            ; Number of free form (not arg or cmd) values
        (list handler noop)         ; fn for parsed command line result
        (list help "")))            ; Help string

;
(defun arg_template ()
      (list
        (list argument nil)         ; The argument flags e.g. ("-s" "--string")
        (list counter 0)            ; Number of values accepted for argument
        (list type type_none)       ; Type for term validation
        (list dest argument)        ; Term for argument parse results
        (list validator noop)       ; Argument value type validator fn
        (list handler noop)         ; fn for parsed argument
        (list help "")))            ; Help string

(defun cmd_template ()
      (list
        (list arguments (list))     ; Arguments for command
        (list command "")           ; Single command string
        (list counter 0)            ; Number of values accepted for command
        (list dest command)         ; Term for command parse results
        (list validator noop)       ; Command value type validator fn
        (list handler noop)         ; fn for parsed command
        (list help "")))            ; Help string

(defun get-property-container (self prop_id)
  ; (get-property-container self prop_id)
  (some (lambda (il)
          (if (eql prop_id (first il))
              il)) self))

(defun get-property (self prop_id &optional ifnil)
  ; (get-property self prop_id [ifnil])
  (defq propc (get-property-container self prop_id))
  (if (second propc)
      (second propc)
      ifnil))

(defun set-property (self prop_id value)
  ; (set-property self prop_id value) -> self
  (defq property (get-property-container self prop_id))
  (when property
      (elem-set 1 property value))
  self)

(defun set-properties (self &rest in_props)
  ; (set-properties self [in_prop]) -> self
  (defq props (reverse in_props))
  (if (/= (logand (length props ) 1) 1)
    (while (/= (length props) 0)
           (set-property self (pop props) (pop props)))
    (throw "Uneven property pairs" in_props))
  self)

(defun extend (self property value)
  ; (extend self property value)
  (defq container (get-property-container self property))
  (when container
    (push (second container) value))
  self)

(defun add-argument (self in_argument)
  ; (add-argument self in_argument)
  (extend self arguments in_argument))

(defun add-command (self in_cmd)
  ; (add-command self in_cmd)
  (extend self commands in_cmd))

(defun container-for (self value container property)
  ; (container-for self value container property)
  ; For a containing type, find a match of
  ; value for a specific properties value
  (defq res (get-property self container '()))
  (some (lambda (el)
                (defq argp (get-property el property))
                (if (or (sfind value argp) (eql value argp))
                    el))
              res))

(defun get-either-container (self value)
  ; (get-either-container self value)
  (if (defq res (container-for self value commands command))
      res
      (container-for self value arguments argument)))

(defun consume-argument (root self argstack result)
  ; (consume-argument root self argstack result)
  (stack-pop argstack)
  (push result (get-property self dest))
  (defq cnt 0)
  (while
    (and
      (< cnt (get-property self counter))
      (not (stack-empty? argstack))
      (not (get-either-container root (stack-peek argstack))))
        (push result (stack-pop argstack))
        (setq cnt (inc cnt)))
  argstack)

(defun consume-command (root self argstack result)
  ; (consume-command root self argstack result)
  (stack-pop argstack)
  (if (defq cmdi (get-property self handler))
      (push result cmdi)
      (push result (get-property self dest)))
  (defq cmdres (list))
  (push result (walk self argstack cmdres))
  argstack)

(defun walk (self arglist &optional res)
  ; (walk-arguments self arglist)
  (defq result (opt res (list)))
  (while (/= (length arglist) 0)
         (defq
          current (stack-peek arglist)
          arg_object (container-for self current arguments argument)
          cmd_object (container-for self current commands command))
         (cond
          (arg_object
            (setq arglist (consume-argument self arg_object arglist result)))
           (cmd_object
            (setq arglist (consume-command self cmd_object arglist result)))
           ((isarg? current)
            (throw "Unrecognized flag " current))
           (t (push result (stack-pop arglist)))))
  result)

(defun process-args (self arglist)
  ; (process-args self arglist)
  (if arglist
      (progn
        (defq
          res (walk self arglist)
          invoke (get-property self handler))
        (if (and invoke (/= (length res) 0))
          (invoke self res)
          res))
      arglist))

(defun argcontains (self argstring)
  (reduced-reduce
    (lambda (acc el)
      (if (sfind argstring (get-property el argument))
          (reduced el)
          nil))
    (get-property self arguments) '()))

(defun parse (self)
  ; (parse parser)
  ; Parses and optinally executes command line
  (cond
    ((opt
       (sfind "-h" (get-property self in_args))
       (sfind "--help" (get-property self in_args)))
      ((get-property (argcontains self "-h") handler) self))
    ((opt
       (sfind "-v" (get-property self in_args))
       (sfind "--version" (get-property self in_args)))
      ((get-property (argcontains self "-v") handler) self))
    ((not (get-property self in_args))
      (let ((targ (argcontains self "-h")))
       ((get-property targ handler) self)))
    (t
      (let ((cargs (copy (get-property self in_args))))
        (process-args self cargs)))))

; Help dump

(defun format-row (arg arghelp)
  (defq colbuff (-  30 (+ (length arg) +indent4+)))
  (str (pad "" +indent4+) arg (pad "" colbuff) arghelp))

(defun format-command-str(cmds)
  ; (format-command-str seq) -> "{el [,el]}"
  (str "{" (join (reduce (lambda (acc el)
              (push acc (get-property el command))) cmds (list))
        ",") "}"))

(defun format-usage-str (self arglist cmdstr)
  ; (format-usage-str self arglist cmdstr) -> "usage ..."
  (str (reduce
         (lambda (acc el)
           (cat acc (str "[" (first (get-property el argument)) "] ")))
         arglist
         (str "usage: " (get-property self application) " "))
    cmdstr " ..."))

(defun dump-help (self)
  ; (dump-help self)
  ; Spits the help tree
  (defq
    arglist (get-property self arguments)
    cmdlist (get-property self commands)
    cmdstr  (format-command-str cmdlist))
  ; Print usage string
  (print (format-usage-str self arglist cmdstr))
  ; Print application help
  (print +nl+ (get-property self help) +nl+)
  ; Print argparse arguments
  (print "optional arguments:")
  (print (reduce (lambda (acc el)
                 (defq args (get-property el argument))
                 (cat acc
                      (format-row (join args ",") (get-property el help))
                      +nl+))
                 arglist ""))
  ; Print argparse commands
  ; TODO: Need additional {cmd [,cmd]} when memory exception fixed
  (when (/= (length cmdlist) 0)
    (print "Actions:" +nl+
           (pad "" +indent2+) cmdstr +nl+
           (reduce (lambda (acc el)
                     (cat acc
                          (format-row
                            (get-property el command)
                            (get-property el help))
                          +nl+))
                   cmdlist ""))))

(defun dump-version (self)
  ; (dump-version self)
  ; Spit the version
  (print (get-property self application)
         (pad "" +indent2+)
         (get-property self version)))

(defun create-argument (args arg_help &optional handler_fn)
  ; Create argument block
  (defq instance (arg_template))
  (set-properties instance
                  argument args
                  help arg_help
                  handler (opt handler_fn noop)))

(defun create-command (cmds cmd_help)
  ; Create command block
  (defq instance (cmd_template))
  (set-properties instance
                  command cmds
                  help cmd_help))

(defun create-argparse (app_name app_version argstring)
  ; Creates the argument processor
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
  (add-argument instance (set-properties ha type type_none))
  (add-argument instance (set-properties va type type_none))
  (set-properties instance
                  application app_name
                  version app_version
                  in_args (reverse argstring)))
