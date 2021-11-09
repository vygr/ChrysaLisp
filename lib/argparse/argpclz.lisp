;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; argpclz - ChrysaLisp Argument Processor
; Classes for argparse, switches, commands
; and constraints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/xtras/xtras.inc")

(defun populate-fields (clzi parms)
  ; (populate-fields argclz parms-map) -> nil
  ; Takes any key/value map and matches key value to
  ; properties on class instance
  (cond
    ((and parms (map? parms))
     (defq ks (keys (into-map (xmap) (entries parms))))
     (each
       (lambda ((_k _v))
         (if (find _k ks)
             (set clzi _k _v)
             (throw "argparse unknown parameter" _k)))
       (entries parms)))
    ((and parms (not (map? parms)))
     (throw "argparse parameters not a map type" parms)))
  )

(defun display-help (_clzi args)
  ; (display-help argclz argstring) -> nil
  (defq +detsep (pad "- " 5 " "))
  (defun build_help (_clzi_children usage detail)
    (reduce
      (lambda (acc (_k _v))
        (defq bstr _k)
        (push detail (str
                       (pad _k 10 " ")
                       +detsep
                       (. _v :help)
                       " (default: " (get :default _v) ")"))
        (cond
          ((and (get :nargs _v) (> (get :nargs _v) 0))
           (push usage (str "[" _k " " (get :type _v) "]")))
          (t
            (push usage (str "[" _k "]"))))
        acc)
      _clzi_children t)
    (list usage detail))

    (bind
      '(summary detail)
      (build_help
        (. _clzi :children)
        (list "[-h]" "[-v]")
        (list
          (str (pad "-h" 10 " ") +detsep "displays help and exits")
          (str (pad "-v" 10 " ") +detsep "displays app version and exits"))))
  (print "")
  (print "Usage: " (. _clzi :unique_id) " " (join summary " "))
  (print "")
  (print "Details:")
  (print "")
  (each print detail)
  nil)

(defun display-version (_clzi args)
  ; (display-version argclz argstring) -> nil
  (print (. _clzi :unique_id) ".lisp " (get :version _clzi))
  nil)

; Built in validation functions
(defun _nop (_clzi)
  ; (_nop argclz) -> nil
  ; Does nothing but returns result
  nil)

; Numeric validators
(defun int-any (_clzi)
  ; (int-any argclz) -> val | exception
  (defq res (get :_result _clzi))
  (when (not (intstr? res))
    (throw "Not an integer" res))
  (set _clzi :_result (to-num res)))

(defun int-positive (_clzi)
  ; (int-positive argclz) -> val | exception
  (defq res (int-any _clzi))
  (when (pos? res)
    (throw "Not a positive integer" res))
  res)

(defun int-negative (_clzi)
  ; (int-negative argclz) -> val | exception
  (defq res (int-any _clzi))
  (when (neg? res)
    (throw "Not a negative integer" res))
  res)

(defun dec-any (_clzi)
  ; (dec-any argclz) -> val | exception
  (defq res (int-any _clzi))
  (when (not (decstr? res))
    (throw "Not a decimal number" res))
  (set _clzi :_result (to-num res)))

; File validators
(defun file-exists? (_clzi)
  ; (file-exists? argclz) -> value | exception
  (defq res (get :_result _clzi))
  (when (str? res)
    (if (zero? (age res))
           (throw "Failed file existence " res)))
  res)

(defun file-not-exist? (_clzi)
  ; (file-not-exists? argclz) -> value | exception
  (defq res (get :_result _clzi))
  (when (str? res)
    (if (> (age res) 0)
           (throw "Failed file not expected to exist " res)))
  res)

; Built in validation jump table
(defq type-validators
      (xmap-kv
        :none           _nop
        :int_any        int-any
        :int_pos        int-positive
        :int_neg        int-negative
        :counter        _nop
        :decimal_any    dec-any
        :str            _nop
        :boolean        _nop
        :file_exist     file-exists?
        :file_not_exit  file-not-exist?))

; Built in argument storage functions
(defun immediate-store (_clzi &optional _v)
  ; (immediate-store argclz list) -> any
  (set _clzi :_result (first _v)))

; Built in storage jump table
(defq action-fns
      (xmap-kv
        :store        immediate-store
        :store_const  (lambda (_clzi &optional v)
                        (immediate-store _clzi '((get :const _clzi))))
        :store_true   (lambda (_clzi &optional v)
                        (immediate-store _clzi '(t)))
        :store_false  (lambda (_clzi &optional v)
                        (immediate-store _clzi '(nil)))
        )
      )

; Base most argparse class
;
(defclass argclz (name) (named-xnode name)
  (def this
       :help    "(no help)"
       :_result nil)

  (defmethod :name  (this)
    (. this :unique_id))
  (defmethod :help  (this)
    (get :help this))
  )

; Primary application class
; Can have both switches and commands
(defclass argparse (name &optional parms) (argclz name)
  (def this
       :version   nil     ; Version string
       :in_args   nil     ; Stores the initial arglist
       :desc      nil     ; Description string
       :usage     nil     ; Usage override string
       :must_have (list)) ; Must have positional commands

  (defmethod :add_action (this arg)
    ; (. argparse :add_action action) -> any
    ; Adds a switch or command to the argument parser
    (when (or (command? arg) (and (switch? arg) (get :required arg)))
        (push (get :must_have this) (. arg :unique_id)))
    (. arg :_set_action)
    (. this :add_node arg))

  (defmethod :parse (this arglist)
    ; (. argparse :parse arglist) -> map | nil
    ; Parses the input argument list and
    ; returns results
    (defq display_immediate nil)
    (cond
      ((or
         (empty? arglist)
         (and (one? (length arglist)) (eql (first arglist) (. this :unique_id)))
         (find "-h" arglist))
       (setq display_immediate display-help))
      ((find "-v" arglist)
        (setq display_immediate display-version))
      (t
        (setq arglist (slice 1 -1 arglist))
        (defq hits (filter (#(nil? (find %0 arglist))) (get :must_have this)))
        (if (nempty? hits)
            (throw "Missing required arguments." hits))))
    (cond
      (display_immediate
        (display_immediate this arglist))
      (t
        ; Build result map
        (set this :_result
             (reduce (lambda (acc (_ _v))
                       (sets! acc (get :dest _v) nil))
                     (. this :children)
                     (xmap)))
        ; Parse arglist
        (let ((index 0) (len (length arglist)))
          (while (< index len)
                 (defq cntxt (. this :child_node (elem-get index arglist)))
                 (if cntxt
                   (setq index (. cntxt :action_parse (inc index) arglist))
                   (throw "No matching action found " (elem-get index arglist)))))
        ; Gather result
        (reduce (lambda (acc (_ _v))
                  (sets! acc
                         (get :dest _v)
                         (. _v :results)))
                (. this :children)
                (get :_result this))))
    )

  ; argparse constructor
  (populate-fields this parms)
  )

; Base action (command/switch) class
(defclass action (name &optional parms) (argclz name)
  (def this
       :required  nil
       :action    nil
       :const     nil
       :nargs     1
       :default   nil
       :dest      nil
       :type      :str
       :validate  :str
       :choices   nil
       :metavar   nil)

  (defmethod :_set_action (this)
    ; (. action :_set_action) -> any
    (set this :action :store))

  (defmethod :validate_and_store (this res)
    ; (. action :validate_and_store parse_result) -> any | exception
    (defq
      act (get :action this)
      val (get :validate this)
      chc (get :choices this))
    ; Choice options in effect
    (when chc
      ; Must match one
      (each (lambda (_v)
              (when (nil? (find _v chc))
                (throw (str _v " is not a valid choice") chc)))
            res))
    ; Store value in :_result
    (cond
      ((lambda? act)
       (act this res))
      (t
        ((gets action-fns act) this res)))
    ; Validate and convert :_result
    (cond
      ((lambda? val)
       (val this))
      (t
        ((gets type-validators val) this)))
    )

  (defmethod :action_parse (this index arglist)
    ; (. action :action_parse index arglist) -> any | exception
    (defq
      res (list)
      cnt (get :nargs this)
      tcnt (+ index cnt))
    (cond
      ; No count, likely a boolean switch
      ((zero? cnt)
       nil)
      ; Count exceeds arglist length
      ((and (> cnt 0) (> tcnt (length arglist)))
       (throw
         (str "Not enough arguments provided for " (. this :unique_id))
         arglist))
      (t
        ; Validate that arguments consumed do not
        ; exceed requirement of action
        (defq
          sibs (each first (. this :siblings)))
        (setq
          res (slice index tcnt arglist))
        (each (lambda (_s)
                (when (find _s res)
                  (throw
                    (str "Can't satisfy arguments expected for "
                         (. this :unique_id))
                    res)))
              sibs)
        (setq
          index tcnt)))
    (. this :validate_and_store res)
    index)

  (defmethod :results (this)
    ; (. action :results) -> value
    (get :_result this))

  ; action constructor
  (populate-fields this parms)
  (set this :_result (get :default this))
  (if (nil? (get :dest this))
      (set this :dest
         (cond
           ((and
              (eql (first name) "-")
              (not (eql (second name) "-")))
            (sym (cat : (slice 1 -1 name))))
           ((starts-with "--")
            (sym (cat : (slice 2 -1 name))))
           (t (sym (cat : name))))))
  )

; Base switch class
(defclass switch (name &optional parms) (action name parms)
  )

; Boolean switch class
(defclass bool-switch (name &optional parms) (switch name parms)
  (set this
       :nargs   0
       :type    :boolean)

  (defmethod :_set_action (this)
    ; (. bool-switch :_set_action) -> any
    ; Override action method to reflect
    ; storage of boolean action
    (set this :action (opt (get :action this) :store_true)))
  )

; Command class
; All commands are required
(defclass command (name &optional parms) (action name parms)
  (set this :required t)
  )
