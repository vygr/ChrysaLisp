;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; csv-data - ChrysaLisp CSV Data Processor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Utilities

(defun comma-indexes (s delim)
  ; (comma-indexes s) -> seq
  ; Returns a sequence of positions of commas in
  ; a string, skipping embedded commas in quotes
  (defq in_q nil)
  (reduce (lambda (acc el)
    (elem-set 0 acc (inc (first acc)))
    (cond
      ((eql (ascii-char 34) el)
        (setq in_q (if in_q nil t)) acc)
      ((and (eql delim el) (not in_q))
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
  ; (csv-cell-value s findx tindx) -> value
  ; Retrieves the value in a (s)tring
  ; between to indexes
  `(slice ,findx ,lindx ,s))

(defun csv-validate-clz (art expected)
  ; (csv-validate-clz artifact expected_clz) -> artifact | exception
  (defq aclz (get-property art :clz))
  (if (eql aclz expected)
    art
    (throw (str "csv-data: class mismatch, expected " expected " found") aclz)))

(defun csv-parse-row (s delim)
  ; (csv-parse-row s) -> csv_row
  ; Construces a csv-row from a
  ; comma delimited input string
  (setd s "" delim ",")
  (defq
    row (list
      :clz        :clz_csv_row
      :vcount     0
      :row        (list))
    fpos  0
    clpos (second (comma-indexes s delim)))
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

(defun csv-cell-count (rr)
  ; (csv-cell-count rr) -> count of cells | exception
  (csv-validate-clz rr :clz_csv_row)
  (length (get-property doc :row)))

(defun csv-cells (rr)
  ; (cells rr) -> list | exception
  ; Returns the list of cell values from a row
  (csv-validate-clz rr :clz_csv_row)
  (get-property rr :row))

(defun csv-cell (rr index)
  ; (cell rr index) -> cell content | exception
  ; Returns a row's specific cell value indicated by index
  (csv-validate-clz rr :clz_csv_row)
  (setd index 0)
  (elem index (get-property rr :row)))

(defun csv-first-cell (rr)
  ; (first-cell rr) -> cell content | exception
  ; Returns the value from the first cell in a row
  (csv-validate-clz rr :clz_csv_row)
  (first (get-property rr :row)))

(defun csv-last-cell (rr)
  ; (last-cell rr) -> cell content | exception
  ; Returns the value from the last cell in a row
  (csv-validate-clz rr :clz_csv_row)
  (last (get-property rr :row)))

; Document functions
(defun csv-parse-document (dname strm options)
  ; (csv-parse-document dname strm options) -> document
  (setd options '())
  (defq
    delim   (opt (find :delimiter options) ",")
    doc (list
      :clz        :clz_csv_document
      :name       dname
      :delim      delim
      :header     nil
      :rows       (list)))
  ; Construct rows
  (while (defq ln (read-line strm))
    (push (get-property doc :rows) (csv-parse-row ln delim)))
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

(defun csv-row-count (doc)
  ; (csv-row-count doc) -> row count | exception
  (csv-validate-clz doc :clz_csv_document)
  (length (get-property doc :rows)))

(defun csv-rows (doc)
  ; (rows doc) -> list of rows | exception
  (csv-validate-clz doc :clz_csv_document)
  (get-property doc :rows))

(defun csv-row (doc index)
  ; (row doc index) -> row | exception
  (csv-validate-clz doc :clz_csv_document)
  (setd index 0)
  (elem index (get-property doc :rows)))

(defun csv-first-row (doc)
  ; (first-row doc) -> row | exception
  (csv-validate-clz doc :clz_csv_document)
  (first (get-property doc :rows)))

(defun csv-last-row (doc)
  ; (last-row doc) -> row | exception
  (csv-validate-clz doc :clz_csv_document)
  (last (get-property doc :rows)))

(defun csv-headers (doc)
  ; (headers doc) -> list of column headers | nil
  (csv-validate-clz doc :clz_csv_document)
  (defq hdr (get-property doc :header))
  (when hdr
    (get-property hdr :row)))


(defun csv-add-row! (doc csv-str &optional delim)
  ; (csv-add-row doc csv-str) -> doc | exception
  ; Parses a new csv string and appends to the
  ; document rows
  (csv-validate-clz doc :clz_csv_document)
  (setd delim ",")
  (push (get-property doc :rows) (csv-parse-row csv-str delim))
  doc)

(defun csv-drop-row! (doc csv_row_index)
  ; (csv-drop-row doc csv-row-index) -> doc | exception
  ; Drops a row from the csv document
  (csv-validate-clz doc :clz_csv_document)
  (defq
    rws (get-property doc :rows)
    rwl (length rws))
  (if (< csv_row_index rwl)
    (cond
      ; single row or last element index
      ((or (and (= csv_row_index 0) (= rwl 1)) (= csv_row_index (dec rwl)))
        (pop rws))
      ; first element
      ((= csv_row_index 0)
        (set-property doc :rows (slice 1 -1 rws)))
      ; else
      (t
        (set-property doc :rows
          (cat
            (slice 0 csv_row_index rws)
            (slice (inc csv_row_index) -1 rws)))))
    (throw (str "csv-data: row index out of range " (length rws)) csv_row_index))
  doc)

(defun csv-read (fname &rest options)
  ; (csv-read fname &rest options) -> document | exception
  (csv-parse-document fname (file-stream fname) options))

(defun csv-write (doc &optional fname)
  ; (csv-write doc) -> t | exception
  (csv-validate-clz doc :clz_csv_document)
  (setd fname (get-property doc :name))
  (defq
    stream (string-stream (cat ""))
    delim (get-property doc :delim)
    hdr   (csv-headers doc))
  (when (and hdr (/= (length hdr) 0))
    (write-line stream (join hdr delim)))
  (each (lambda (r)
    (write-line stream (join (csv-cells r) delim))) (csv-rows doc))
  (save (str stream) fname))

; Test
(defq mdoc
  (csv-read "../LTEST/tmp.csv" :validate :first_row_header))
(csv-add-row! mdoc "funky,frank,80")
