;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; argpclz - ChrysaLisp Argument Processor
; Classes for argparse, switches, commands
; and constraints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (print "usage: " (. _clzi :unique_id))
  nil)

(defun display-version (_clzi args)
  ; (display-version argclz argstring) -> nil
  (print (. _clzi :unique_id) ".lisp " (get :version _clzi))
  nil)

(defun _nop (_clzi result)
  ; (_nop argclz list) -> t
  t)

(defun file-exist? (_clzi args)
  ; (file-exists? argclz list) -> nil | exception
  (when (str? (first args))
    (if (= (age (first args)) 0)
           (throw "Failed file existence " args))))

(defun file-not-exist? (_clzi args)
  ; (file-not-exists? argclz list) -> nil | exception
  (when (str? (first args))
    (if (> (age (first args)) 0)
           (throw "Failed file not expected to exist " args))))

; Built in validation functions
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
        :file_exist     file-exist?
        :file_not_exit  file-not-exist?))

(defun immediate-store (_clzi &optional _v)
  ; (immediate-store argclz list) -> any
  (set _clzi :_result (first _v)))

; Built in storage functions
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
       :help    nil     ; All can have a help string
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
        ; Build result map
        (set this :_result
             (reduce (lambda (acc (_ _v))
                       (sets! acc (get :dest _v) nil))
                     (. this :children)
                     (xmap)))
        ; Parse arglist
        (let ((index 0) (len (length arglist)))
          (while (< index len)
                 (defq cntxt (. this :child_node (elem index arglist)))
                 (if cntxt
                   (setq index (. cntxt :action_parse (inc index) arglist))
                   (throw "No matching action found " (elem index arglist)))))
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
       :choices   '()
       :metavar   nil)

  (defmethod :_set_action (this)
    (set this :action :store))

  (defmethod :validate_and_store (this res)
    (defq
      act (get :action this)
      val (get :validate this))
    (cond
      ((lambda? val)
       (val this res))
      (t
        ((gets type-validators val) this res)))
    (cond
      ((lambda? act)
       (act this res))
      (t
        ((gets action-fns act) this res))))

  (defmethod :action_parse (this index arglist)
    (defq
      res (list)
      cnt (get :nargs this)
      tcnt (+ index cnt))
    (cond
      ; No count, likely a boolean switch
      ((= cnt 0)
       nil)
      ; Count exceeds arglist length
      ((and (> cnt 0) (> tcnt (length arglist)))
       (throw
         arglist))
      (t
        (setq
          res (slice index tcnt arglist)
          index tcnt)))
    (. this :validate_and_store res)
    index)

  (defmethod :results (this)
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
    (set this :action (opt (get :action this) :store_true)))
  )

; Command class
; All commands are required
(defclass command (name &optional parms) (action name parms)
  (set this :required t)
  )
