;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; deserialize - ChrysaLisp Mini YAML Object De-Serializer
; Converts string-stream to ChyrsaLisp object form
;
; Primary usage is intended to the CPU interchange
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import 'lib/xtras/xtras.inc)
(import 'lib/yaml-data/commons.lisp)

(defq lu
      (properties
        "{" :mapb
        "}" :mape
        "[" :lstb
        "]" :lste
        " " :space
        ":" :mkey))

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
  (defq npath (getp cntxt :path))
  (pop npath)
  (setp! cntxt :current (last npath))
  nil)

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

(defun pull-value (ch sst)
  (defq res (eat-to-space ch sst))
  (if (str-is-ints? res)
    (str-to-num res)
    res))

(defun lex-to-object (sst)
  (defq ctx (setp! (Context) :root nil t))
  (until (eql (defq ch (pop sst)) (char 0))
    (case (getp lu ch :char)
      ((:space))
      ((:mapb)
       (set-obj-ctx! ctx (properties)))
      ((:mape)
       (unset-obj-ctx! ctx))
      ((:lstb)
       (set-obj-ctx! ctx (list)))
      ((:lste)
       (unset-obj-ctx! ctx))
      ((:mkey) (add-to-obj! ctx (sym (eat-to-space ch sst))))
      ((:char) (add-to-obj! ctx (pull-value ch sst)))))
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