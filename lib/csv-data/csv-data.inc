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

(defun csv-validate-clz (csv-seq expected)
  ; (csv-validate-clz artifact expected_clz) -> artifact | exception
  (defq aclz (get :clz csv-seq))
  (if (eql aclz expected)
    csv-seq
    (throw (str "csv-data: class mismatch, expected " expected " found") aclz)))

(defun csv-parse-row (s delim)
  ; (csv-parse-row s) -> csv_row
  ; Construces a csv-row from a
  ; comma delimited input string
  (setd s "" delim ",")
  (defq
    fpos  0
    row (env -101)
    clpos (second (comma-indexes s delim)))
  (def row
      :clz        :clz_csv_row
      :vcount     0
      :row        (list))
  (while (/= fpos -1)
    (defq lpos -1)
    (when (/= fpos (last clpos))
      (setq lpos (elem (inc (csv-ifind fpos clpos)) clpos)))
    (when (/= fpos 0)
      (setq fpos (inc fpos)))
    (push (get :row row) (csv-cell-value s fpos lpos))
    (setq fpos lpos))
  (set row :vcount (length (get :row row)))
  row)

(defun csv-cell-count (rr)
  ; (csv-cell-count rr) -> count of cells | exception
  (csv-validate-clz rr :clz_csv_row)
  (length (get :row doc)))

(defun csv-cells (rr)
  ; (cells rr) -> list | exception
  ; Returns the list of cell values from a row
  (csv-validate-clz rr :clz_csv_row)
  (get :row rr))

(defun csv-cell (rr index)
  ; (cell rr index) -> cell content | exception
  ; Returns a row's specific cell value indicated by index
  (csv-validate-clz rr :clz_csv_row)
  (setd index 0)
  (elem index (get :row rr)))

(defun csv-first-cell (rr)
  ; (first-cell rr) -> cell content | exception
  ; Returns the value from the first cell in a row
  (csv-validate-clz rr :clz_csv_row)
  (first (get :row rr)))

(defun csv-last-cell (rr)
  ; (last-cell rr) -> cell content | exception
  ; Returns the value from the last cell in a row
  (csv-validate-clz rr :clz_csv_row)
  (last (get :row rr)))

; Document functions
(defun csv-parse-document (dname strm options)
  ; (csv-parse-document dname strm options) -> document
  (setd options '())
  (defq
    delim   (opt (find :delimiter options) ",")
    doc (env -101))
  (def doc
      :clz        :clz_csv_document
      :name       dname
      :delim      delim
      :header     nil
      :rows       (list))
  ; Construct rows
  (while (defq ln (read-line strm))
    (push (get :rows doc) (csv-parse-row ln delim)))
  ; Header options: mutually exclusive
  (cond
    ; First row is header
    ((find :first_row_header options)
      (defq drows (get :rows doc))
      (set doc :header (first drows))
      (set doc :rows (slice 1 -1 drows)))
    ; Header provided
    ((find :use_header options)
      (set doc :header (elem (inc (find :use_header options)) options))))
  (when (find :validate options)
    (defq
      rrs (get :rows doc)
      vcnt (get :vcount (first rrs)))
    (each (lambda (el)
      (when (/= vcnt (get :vcount el))
        (throw
          (str "csv-data: inconsistent row values, exptect " vcnt " found ")
          (get :vcount el)))) rrs))
  doc)

(defun csv-row-count (doc)
  ; (csv-row-count doc) -> row count | exception
  (csv-validate-clz doc :clz_csv_document)
  (length (get :rows doc)))

(defun csv-rows (doc)
  ; (rows doc) -> list of rows | exception
  (csv-validate-clz doc :clz_csv_document)
  (get :rows doc))

(defun csv-row (doc index)
  ; (row doc index) -> row | exception
  (csv-validate-clz doc :clz_csv_document)
  (setd index 0)
  (elem index (get :rows doc)))

(defun csv-first-row (doc)
  ; (first-row doc) -> row | exception
  (csv-validate-clz doc :clz_csv_document)
  (first (get :rows doc)))

(defun csv-last-row (doc)
  ; (last-row doc) -> row | exception
  (csv-validate-clz doc :clz_csv_document)
  (last (get :rows doc)))

(defun csv-headers (doc)
  ; (headers doc) -> list of column headers | nil
  (csv-validate-clz doc :clz_csv_document)
  (defq hdr (get :header doc))
  (when hdr
    (get :row hdr)))


(defun csv-add-row! (doc csv-str &optional delim)
  ; (csv-add-row doc csv-str) -> doc | exception
  ; Parses a new csv string and appends to the
  ; document rows
  (csv-validate-clz doc :clz_csv_document)
  (setd delim ",")
  (push (get :rows doc) (csv-parse-row csv-str delim))
  doc)

(defun csv-drop-row! (doc csv_row_index)
  ; (csv-drop-row doc csv-row-index) -> doc | exception
  ; Drops a row from the csv document
  (csv-validate-clz doc :clz_csv_document)
  (defq
    rws (get :rows doc)
    rwl (length rws))
  (if (< csv_row_index rwl)
    (cond
      ; single row or last element index
      ((or (and (= csv_row_index 0) (= rwl 1)) (= csv_row_index (dec rwl)))
        (pop rws))
      ; first element
      ((= csv_row_index 0)
        (set doc :rows (slice 1 -1 rws)))
      ; else
      (t
        (set doc :rows
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
  (setd fname (get :name doc))
  (defq
    stream (string-stream (cat ""))
    delim (get :delim doc)
    hdr   (csv-headers doc))
  (when (and hdr (/= (length hdr) 0))
    (write-line stream (join hdr delim)))
  (each (lambda (r)
    (write-line stream (join (csv-cells r) delim))) (csv-rows doc))
  (save (str stream) fname))
