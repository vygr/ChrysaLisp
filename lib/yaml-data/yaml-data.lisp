;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; yaml-data - ChrysaLisp YAML Data Processor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; imports
(import 'lib/xtras/xtras.inc)
(import 'lib/yaml-data/scanner.lisp)
(import 'lib/yaml-data/parser.lisp)
(import 'lib/yaml-data/constructor.lisp)
(import 'lib/yaml-data/emiter.lisp)

; Reader Options
;   :keys-to-kw   when true, properties keys converted to keywords
;                 if doing so, spaces will be set to underscores and
;                 result will be prefixed with ':' before making
;                 the symbol
;                 Default: False
;   :vals-to-kw   when true, string values that have ':' as first
;                 character will be converted to keyword
;                 Default: False
;   :vals-to-num  when true, will attempt to detect entire string value
;                 is either integer of real and convert it to such

(defq +reader-properties+
      (properties
        :keys-to-kw  nil    ; Converts keys to keywords
        :vals-to-kw  nil    ; Converts values with ':' prefix to keywords
        :vals-to-num nil))  ; Converts numerics to native (int, real, nums)

; Writer Options
;   :kw-to-str    when true, any keyword will be quoted as YAML
;                 parses tend to choke otherwise. If false the
;                 prefix ':' will be dropped
;                 Default: True

(defq +writer-properties+
      (properties
        :kw-to-str  t       ; Quotes keywords otherwise strips ':'
        ))

; Reader

(defun-bind yaml-construct (ast &rest in-args)
  ; (yaml-construct tokens [in-args]) -> list | exception | nil
  ; Parses yaml tokensers and returns ChyrsaLisp objects
  (construct ast (strip-rest in-args)))

(defun-bind yaml-parse (tokens &rest in-args)
  ; (yaml-parse tokens [in-args]) -> list | exception | nil
  ; Parses yaml tokensers and returns yaml AST
  (parse tokens (strip-rest in-args)))

(defun-bind yaml-scan (ystring)
  ; (yaml-scan string) -> list | exception | nil
  ; Performs scan on string returning list of
  ; lexical yaml tokens
  (scan ystring))

(defun-bind yaml-read-string (ystring &rest in-args)
  ; (yaml-read-string string [in-args]) -> list | exception | nil
  ; Converts YAML string to ChyrsaLisp data structures
  (defq ops (strip-rest in-args))
  (yaml-construct (yaml-parse (yaml-scan ystring) ops) ops))

(defun-bind yaml-read (fname &rest in-args)
  ; (yaml-read fname [in-args]) -> list | exception | nil
  ; Opens and reads in a YAML file and returns
  ; native ChyrsaLisp data structures

  (if (zero? (age fname))
    (throw (str fname " not found") t)
    (yaml-read-string (load fname) in-args)))

; Writer

(defun-bind yaml-emit (stream obj in-args)
  (emit stream obj (strip-rest in-args)))

(defun-bind yaml-from-obj (obj &rest in-args)
  (str (yaml-emit (string-stream (cat "")) obj (strip-rest in-args))))

(defun-bind yaml-write (fname obj &rest in-args)
  (defq base-args (properties :kw-to-str t))
  (defq res (yaml-from-obj obj base-args)))
