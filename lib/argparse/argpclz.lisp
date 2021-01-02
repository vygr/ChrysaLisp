;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; argpclz - ChrysaLisp Argument Processor
; Classes for argparse, switches, commands
; and constraints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun populate-fields (clzi parms)
  ; (populate-fields class-instance parms-map) -> nil
  ; Takes any key/value map and matches key value to
  ; properties on class
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

(defun display-help (argp args)
  (print "usage: " (. argp :unique_id))
  nil)

(defun display-version (argp args)
  (print (. argp :unique_id) ".lisp " (get :version argp))
  nil)

(defun _nop (val) t)

(defq type-validators
      (xmap-kv
        :none           _nop
        :int_any        _nop
        :int_pos        _nop
        :int_neg        _nop
        :counter        _nop
        :real           _nop
        :str            _nop
        :boolean        _nop
        :file_exist     _nop
        :file_not_exit  _nop))

(defun immediate-store (_clzi &optional _v)
  (set _clzi :_result _v))

(defq actions
      (xmap-kv
        :store        immediate-store
        :store_const  (#(immediate-store %0 (get :const %0)))
        :store_true   (#(immediate-store %0 t))
        :store_false  (#(immediate-store %0 nil))
        )
      )

; Base most argparse class
;
(defclass argclz (name) (named-xnode name)
  (def this
       :help    nil     ; All can have a help string
       :_result nil)

  (defmethod :name  (this) (. this :unique_id))
  (defmethod :help  (this) (get :help this)))

; Primary application class
; Can have both switches and commands
(defclass argparse (name &optional parms) (argclz name)
  (def this
       :version   nil     ; Version string
       :in_args   nil     ; Stores the initial arglist
       :desc      nil     ; Description string
       :usage     nil     ; Usage override string
       :handler   _nop    ; not sure
       :must_have (list)) ; Must have positional commands

  (defmethod :add_action (this arg)
    ; (. argparse :add_action action) -> any
    ; Adds a switch or command to the argument parser
    (when (or (command? arg) (and (switch? arg) (get :required arg)))
        (push (get :must_have this) (. arg :unique_id)))
    (. this :add_node arg))

  (defmethod :parse (this arglist)
    ; (. argparse :parse arglist) -> argobject | nil
    ; Parses the input argument list and
    ; returns results
    (defq display_immediate nil)
    (cond
      ((or
         (empty? arglist)
         (and (= (length arglist) 1) (eql (first arglist) (. this :unique_id)))
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
        arglist))
    )

  ; argparse constructor
  (populate-fields this parms))

; Base action (command/switch) class
(defclass action (name &optional parms) (argclz name)
  (def this
       :required  nil
       :action    immediate-store
       :const     nil
       :nargs     1
       :default   nil
       :dest      nil
       :type      :str
       :validate  :str
       :handler   _nop
       :count     1
       :choices   '()
       :metavar   nil)

  ; (defmethod perform-action (this &optional _v))

  ; action constructor
  (populate-fields this parms)
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


(defclass switch (name &optional parms) (action name parms)
  )

(defclass bool-switch (name &optional parms) (switch name parms)
  (set this
       :count 0
       :type  :boolean)
  )

(defclass command (name &optional parms) (action name parms)
  (set this :required t)
  (defmethod :add_switch (this arg)
    (if (switch? arg)
        (. this :add_node arg)
        (throw "Unknown type " arg)))
  )
