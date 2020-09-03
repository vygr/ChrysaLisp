;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; emmiter - ChrysaLisp Object YAML Emmiter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import 'lib/xtras/xtras.inc)
(import 'lib/yaml-data/nodes.lisp)

; Not multi-threaded safe

(defq ywcntrl nil)

;
; (:clz :me_key :value k)
; (:clz :me_value :value v)
; (:clz :prop-me :entries ((:clz :me_key :value k) (:clz :me_value :value v))
;

(defun me-key? (_)
  (eql (getp _ :clz) :me_key))

(defun me-value? (_)
  (eql (getp _ :clz) :me_value))

(defun prop-me? (_)
  (eql (getp _ :clz) :prop_me))

(defun make-me-key (_)
  (list :clz :me_key :value _))

(defun make-me-value (_)
  (list :clz :me_value :value _))

(defun split-entries (p)
  (reduce
    (lambda (acc el)
      (push acc
        (list :clz :prop_me :entries
              (push
                (push (list) (make-me-key (first el)))
                (make-me-value (second el))))))
    (prop-entries p) (list)))

(defun build-nodes (idata)
  ; (build-nodes idata) -> nil
  (cond
    ((lst? idata)
        (cond
          ; Map
          ((props? idata)
           (print "Properties -> Map")
           (each build-nodes (split-entries idata)))
          ; Map Entry
          ((prop-me? idata)
           (print "Map Entry")
           (each build-nodes (getp idata :entries)))
          ; Key
          ((me-key? idata)
           (print "Key")
           (build-nodes (getp idata :value)))
          ; Value
          ((me-value? idata)
           (print "Value")
           (build-nodes (getp idata :value)))
          ; Sequence
          (t
            (print "List -> seq " idata)
            (each build-nodes idata))))
    (
     (or
       (kw? idata)
       (sym? idata)
       (str? idata)
       (num? idata))
     (print "Scalar " idata))))

(defun-bind emit (stream data in-args)
  ; (emit stream data options) -> stream
  ; Converts data to strings and writes to streams
  (setq ywcntrl (pmerge in-args (properties
                                  :level  -1
                                  :indent 0
                                  :stream stream
                                  :nodes  (DocStartNode))))
  (build-nodes data)
  (write stream (str (getp ywcntrl :nodes))))
