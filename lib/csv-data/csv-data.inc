;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; csv-data - ChrysaLisp CSV Data Processor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Utilities

(defun comma-indexes (s)
  ; (comma-indexes s) -> seq
  ; Returns a sequence of positions of commas in
  ; a string, skipping embedded commas in quotes
  (defq in_q nil)
  (reduce (lambda (acc el)
    (elem-set 0 acc (inc (first acc)))
    (cond
      ((eql (ascii-char 34) el)
        (setq in_q (if in_q nil t)) acc)
      ((and (eql "," el) (not in_q))
        (push (second acc) (first acc)) acc)
      (t acc))) s (list -1 (list 0))))

(defmacro get-property (csv-seq prop_id)
  ; (get-property seq propid) ->  val | nil
  ; Returns the property (prop_id) value from
  ; the csv structure
  `(elem (inc (find ,prop_id ,csv-seq)) ,csv-seq))

(defmacro set-property (csv-seq prop_id val)
  ; (set-property csv-seq prop_id val) -> val
  ; Set's a property (prop_id) to a new
  ; value (val) on a csv structure
  `(elem-set (inc (find ,prop_id ,csv-seq)) ,csv-seq ,val))

(defun csv-ifind (elval seq)
  ; (csv-ifind elval seq) -> index elval in sequence
  ; used against a sequence of numeric values to
  ; find it's index
  (second (reduced-reduce (lambda (acc el)
    (when (= el (first acc))
      (reduced (second acc)))
    (elem-set 1 acc (inc (second acc)))
    acc) seq (list elval -1))))

(defmacro csv-cell-value (s findx lindx)
  ; (csv-cell-value s findx tindx)
  ; Retrieves the value in a (s)tring
  ; between to indexes
  `(slice ,findx ,lindx ,s))

(defun csv-parse-row (s)
  ; (csv-parse-row s) -> csv_row
  ; Construces a csv-row from a
  ; comma delimited input string
  (setd s "")
  (defq
    row (list
      :clz        :clz_csv_row
      :vcount     0
      :rpos       0
      :row        (list))
    fpos  0
    clpos (second (comma-indexes s)))
  (while (/= fpos -1)
    (defq lpos -1)
    (when (/= fpos (last clpos))
      (setq lpos (elem (inc (csv-ifind fpos clpos)) clpos)))
    (when (/= fpos 0)
      (setq fpos (inc fpos)))
    (push (get-property row :row) (csv-cell-value s fpos lpos))
    (setq fpos lpos))
  (set-property row :vcount (length (get-property row :row)))
  row)

(defun csv-cells (rr)
  ; (cells rr) -> list
  ; Returns the list of cell values from a row
  (if (eql (get-property rr :clz) :clz_csv_row)
    (get-property rr :row)
    (throw "csv-data: cells expects :clz_csv_row class, found "
      (get-property rr :clz))))

(defun csv-cell (rr index)
  ; (cell rr index)
  ; Returns a row's specific cell value indicated by index
  (setd index 0)
  (elem index (get-property rr :row)))

(defun csv-first-cell (rr)
  ; (first-cell rr) -> val
  ; Returns the value from the first cell in a row
  (first (get-property rr :row)))

(defun csv-last-cell (rr)
  ; (last-cell rr) -> val
  ; Returns the value from the last cell in a row
  (last (get-property rr :row)))

(defun csv-next-cell! (rr)
  ; (next-cell! rr) -> value
  ; A cheap iterator to use in while loops to
  ; iterate across all cells in a row
  (defq
    ipos    (get-property rr :rpos)   ; Next starting pos
    rvals   (csv-cells rr))           ; Row value list
  (cond
    ((= ipos -1) nil)
    (t
      (defq cv (elem ipos rvals))
      ;(if (= (eval (incdec ipos)) (length rvals))
      (if (= (inc ipos) (length rvals))
        (set-property rr :rpos -1)
        (set-property rr :rpos (inc ipos)))
      cv)))

; Document functions
(defun csv-parse-document (dname strm options)
  ; (csv-parse-document dname strm options) -> document
  (setd options '())
  (defq
    doc (list
      :clz        :clz_csv_document
      :name       dname
      :header     nil
      :rpos       0
      :rows       (list)))
  ; Construct rows
  (while (defq ln (read-line strm))
    (push (get-property doc :rows) (csv-parse-row ln)))
  ; Header options: mutually exclusive
  (cond
    ; First row is header
    ((find :first_row_header options)
      (defq drows (get-property doc :rows))
      (set-property doc :header (first drows))
      (set-property doc :rows (slice 1 -1 drows)))
    ; Header provided
    ((find :use_header options)
      (set-property doc :header (elem (inc (find :use_header options)) options))))
  (when (find :validate options)
    (defq
      rrs (get-property doc :rows)
      vcnt (get-property (first rrs) :vcount))
    (each (lambda (el)
      (when (/= vcnt (get-property el :vcount))
        (throw
          (str "csv-data: inconsistent row values, exptect " vcnt " found ")
          (get-property el :vcount)))) rrs))
  doc)

(defun csv-rows (doc)
  ; (rows doc) -> list of rows
  (if (eql (get-property doc :clz) :clz_csv_document)
    (get-property doc :rows)
    (throw "csv-data:rows expects : clz_dsv_doument class, found"
      (get-property doc :clz))))

(defun csv-row (doc index)
  ; (row doc index) -> row
  (setd index 0)
  (elem index (get-property doc :rows)))

(defun csv-next-row! (doc))

(defun csv-first-row (doc)
  ; (first-row doc) -> row
  (first (get-property doc :rows)))

(defun csv-last-row (doc)
  ; (last-row doc) -> row
  (last (get-property doc :rows)))

(defun csv-headers (doc)
  ; (headers doc) -> list of column headers | nil
  (get-property (get-property doc :header) :row))

(defun csv-add-row! (doc csv-str)
  ; (csv-add-row doc csv-str) -> doc | exception
  ; Parses a new csv string and appends to the
  ; document rows
  )

(defun csv-reset-itr! (csv-seq)
  ; (csv-reset-itr! csv-seq) -> 0
  (set-property csv-seq :rpos 0))

(defun csv-drop-row! (doc csv-row-index)
  ; (csv-drop-row doc csv-row-index) -> doc | exception
  ; Drops a row from the csv document
  )

(defun csv-read (fname &rest options)
  ; (csv-read fname &rest options) -> document | exception
  (csv-parse-document fname (file-stream fname) options))

(defun csv-write (doc)
  ; (csv-write doc) -> nil | exception
  )

; Test stuff
(defq mdoc
  (csv-read
    '../LTEST/tmp.csv
    :first_row_header
    :validate))

(defq r1 (first (csv-rows mdoc)))

