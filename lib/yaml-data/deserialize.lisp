;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; deserialize - ChrysaLisp Mini YAML Object De-Serializer
; Converts string-stream to ChyrsaLisp object form
;
; Primary usage is intended to the CPU interchange
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/xtras/xtras.inc")

(defq lu
      (properties
        "{" :mapb
        "}" :mape
        "<" :setb
        ">" :sete
        "[" :lstb
        "]" :lste
        " " :space
        ":" :mkey
        +dblq+ :strng
        "*" :boolean))

(defun set-obj-ctx! (cntxt n)
  ; (set-obj-ctx! context node) -> node
  ; Makes the current context 'node'
  (defq crn (gets cntxt :current))
  (cond
    ((nil? (gets cntxt :root))
      (sets! cntxt :root n))
    ((not (nil? crn))
      (push crn n)))
  ; Stack node in path for un-setting
  (push (gets cntxt :path) n)
  ; Make node current context
  (sets! cntxt :current n)
  n)

(defun unset-obj-ctx! (cntxt)
  ; (unset-obj-ctx! context) -> node | nil
  ; Set's context to most recent in path
  (defq
    npath (gets cntxt :path)
    res   (pop npath))
  (sets! cntxt :current (last npath))
  res)

(defun add-to-obj! (cntxt n)
  ; (add-to-obj! node) -> node | nil
  (when (defq crn (gets cntxt :current))
    (push crn n)))

(defun eat-to-space (ch sst)
  (defq
    res (list ch)
    lst (last sst))
  (until (or
           (eql lst " ")
           (eql lst (char 0))
           (find lst "{}[]:<>"))
    (push res (pop sst))
    (setq lst (last sst)))
  (join res ""))

(defun-bind eat-strng (sst)
  (defq
    res (list)
    lst (last sst))
  (while (not (eql lst +dblq+))
    (push res (pop sst))
    (setq lst (last sst)))
  (pop sst)
  (join res ""))

(defun pull-value (ch sst)
  (defq res (eat-to-space ch sst))
  (when (eql (str-is-ints? res) :true)
    (setq res (str-to-num res)))
  res)

(defq deser_boolean
      (properties
        "*true"   t
        "*false"  nil))

(defun lex-to-object (sst)
  (defq ctx (sets! (Context) :root nil))
  (until (eql (defq ch (pop sst)) (char 0))
    (case (gets lu ch :char)
      ((:space))
      ((:mapb)
       ; (set-obj-ctx! ctx (xmap))
       (set-obj-ctx! ctx (list)))
      ((:lstb)
       (set-obj-ctx! ctx (list)))
      ((:lste)
       (unset-obj-ctx! ctx))
      ((:mape)
       (defq
         tail (unset-obj-ctx! ctx)
         hm   (xmap))
       (each (lambda ((_k _v)) (sets! hm _k _v)) (partition 2 tail)))
      ((:mkey)
       (add-to-obj! ctx (sym (eat-to-space ch sst))))
      ((:boolean)
       (add-to-obj! ctx (gets deser_boolean (eat-to-space ch sst))))
      ((:strng)
       (add-to-obj! ctx (eat-strng sst)))
      ((:char)
       (add-to-obj! ctx (pull-value ch sst)))))
  (gets ctx :root))

(defun-bind deserialize (sstrm)
  ; (deserialize stream) -> object
  (defq
    sst (str sstrm)
    res nil)
  (when (> (length sst) 0)
    (when (not (eql (last sst) (char 0)))
      (setq sst (cat sst (char 0))))
    (setq res (lex-to-object (reverse sst))))
  res)