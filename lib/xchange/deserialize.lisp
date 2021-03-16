;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; deserialize - ChrysaLisp Mini YAML Object De-Serializer
; Converts string-stream to ChyrsaLisp object form
;
; Primary usage is intended to the CPU interchange
; DEPRECATING - Favoring using lib/ast/data_ast.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/xtras/xtras.inc")

(defq lu
      (properties
        "{" :mapb
        "}" :mape
        "#" :emapb
        "#" :emape
        "<" :setb
        ">" :sete
        "[" :lstb
        "]" :lste
        " " :space
        ":" :mkey
        +dblq :strng
        "*" :boolean))

(defun set-obj-ctx! (cntxt n)
  (defq crn (gets cntxt :current))
  ; Sets root if needed
  (cond
    ((nil? (gets cntxt :root))
      (sets! cntxt :root n))
    (t
      (push (gets crn :children) n)))
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
    (push (gets crn :children) n)))

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

(defun eat-strng (sst)
  (defq
    res (list)
    lst (last sst))
  (while (not (eql lst +dblq))
    (push res (pop sst))
    (setq lst (last sst)))
  (pop sst)
  (join res ""))

(defun pull-value (ch sst)
  (defq res (eat-to-space ch sst))
  (if (numstr? res)
      (ScalarNode :number (to-num res))
      (ScalarNode :string res)))

(defq deser_boolean
      (properties
        "*true"   t
        "*false"  nil))


(defun realize-nodes (node)
  ; (realize-nodes ast) -> object
  ; Converts AST to object instance(s)
  (case (gets node :type)
    (:map
      (defq res (xmap))
      (each (lambda ((_k _v))
              (sets! res (realize-nodes _k) (realize-nodes _v)))
            (partition 2 (gets node :children)))
      res)
    (:seq
      (defq res (list))
      (each (lambda (n)
              (push res (realize-nodes n)))
            (gets node :children))
      res)
    (:set
      (defq res (xset))
      (each (lambda (n)
              (sets! res (realize-nodes n)))
            (gets node :children))
      res)
    (:scalar
      (case (gets node :stype)
        (:keyword
          (gets node :value))
        (:string
          (gets node :value))
        (:number
          (gets node :value))
        (:boolean
          (gets node :value))
        (t
          (print "Unknown " (entries node))
          nil)))
    (t
      (print "Unknown " (entries node))
      nil)))

(defun lex-to-object (sst)
  ; (lex-to-object string) -> object
  ; Lex the inbound string to quick AST
  (defq ctx (sets! (Context) :root nil))
  (until (eql (defq ch (pop sst)) (char 0))
    (case (gets lu ch :char)
      (:space)
      (:mapb
        (set-obj-ctx! ctx (MapNode)))
      (:mape
        (unset-obj-ctx! ctx))
      (:lstb
        (set-obj-ctx! ctx (SequenceNode)))
      (:lste
        (unset-obj-ctx! ctx))
      (:setb
        (set-obj-ctx! ctx (SetNode)))
      (:sete
        (unset-obj-ctx! ctx))
      (:mkey
        (add-to-obj! ctx
          (ScalarNode :keyword (sym (eat-to-space ch sst)))))
      (:boolean
        (add-to-obj! ctx
          (ScalarNode :boolean (gets deser_boolean (eat-to-space ch sst)))))
      (:strng
        (add-to-obj! ctx
          (ScalarNode :string (eat-strng sst))))
      (:char
        (add-to-obj! ctx (pull-value ch sst)))))
  (realize-nodes (gets ctx :root)))

(defun deserialize (sstrm)
  ; (deserialize stream) -> object
  (defq
    sst (str sstrm)
    res nil)
  (when (> (length sst) 0)
    (when (not (eql (last sst) (char 0)))
      (setq sst (cat sst (char 0))))
    (setq res (lex-to-object (reverse sst))))
  res)