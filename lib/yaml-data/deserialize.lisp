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
        "<" :hmapb
        ">" :hmape
        "[" :lstb
        "]" :lste
        " " :space
        ":" :mkey
        +dblq+ :strng
        "*" :boolean))

(defun set-obj-ctx! (cntxt n)
  ; (set-obj-ctx! context node) -> node
  ; Makes the current context 'node'
  (defq crn (getp cntxt :current))
  (cond
    ((nil? (getp cntxt :root))
      (setp! cntxt :root n))
    ((not (nil? crn))
      (push crn n)))
  ; Stack node in path for un-setting
  (push (getp cntxt :path) n)
  ; Make node current context
  (setp! cntxt :current n)
  n)

(defun unset-obj-ctx! (cntxt)
  ; (unset-obj-ctx! context) -> node | nil
  ; Set's context to most recent in path
  (defq
    npath (getp cntxt :path)
    res   (pop npath))
  (setp! cntxt :current (last npath))
  res)

(defun add-to-obj! (cntxt n)
  ; (add-to-obj! node) -> node | nil
  (when (defq crn (getp cntxt :current))
    (push crn n)))

(defun eat-to-space (ch sst)
  (defq
    res (list ch)
    lst (last sst))
  (until (and
           (or (eql lst " ") (eql lst (char 0)))
           (not(find lst "{}[]:")))
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
  (defq ctx (setp! (Context) :root nil t))
  (until (eql (defq ch (pop sst)) (char 0))
    (case (getp lu ch :char)
      ((:space))
      ((:mapb)
       (set-obj-ctx! ctx (properties)))
      ((:hmapb)
       (set-obj-ctx! ctx (hmap)))
      ((:lstb)
       (set-obj-ctx! ctx (list)))
      ((:hmape)
       (defq
         hm   (unset-obj-ctx! ctx)
         cnt  (- (length hm) 6))
       (when (> cnt 0)
         (defq tail (take-last cnt hm))
         (each (#(hmap-insert hm (first %0) (second %0)))
               (chunk 2 tail))
         (times cnt (pop hm))))
      ((:mape :lste)
       (unset-obj-ctx! ctx))
      ((:mkey)
       (add-to-obj! ctx (sym (eat-to-space ch sst))))
      ((:boolean)
       (add-to-obj! ctx (getp deser_boolean (eat-to-space ch sst))))
      ((:strng)
       (add-to-obj! ctx (eat-strng sst)))
      ((:char)
       (add-to-obj! ctx (pull-value ch sst)))))
  (getp ctx :root))

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