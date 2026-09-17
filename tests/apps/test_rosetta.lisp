(import "usr/env.inc")
(import "apps/desktop/rosetta/catalog.inc")
(import "././utils.inc")

(report-header "Rosetta Algorithm & CS Explorer Tests")

(defq *config* :nil *config_version* 1
	*config_file* (cat *env_home* "rosetta_test.tre")
	*selected_id* :quicksort
	*selected_cat* "All"
	*search_query* "")

(defun config-default ()
	(scatter (Emap)
		:version *config_version*
		:selected_id :quicksort
		:selected_cat "All"
		:search_query ""))

(defun config-save (id cat_name query)
	(if (not *config*)
		(setq *config* (Emap)))
	(scatter *config*
		:version *config_version*
		:selected_id id
		:selected_cat cat_name
		:search_query query)
	(when (defq stream (file-stream *config_file* +file_open_write))
		(tree-save stream *config*)))

(defun config-load ()
	(defq old_config :nil)
	(if (defq stream (file-stream *config_file*))
		(setq old_config (tree-load stream)))
	(if (or (not old_config) (/= (. old_config :find :version) *config_version*))
		(setq *config* (config-default))
		(setq *config* old_config))
	(setq *selected_id* (. *config* :find :selected_id))
	(if (not (sym? *selected_id*)) (setq *selected_id* :quicksort))
	(setq *selected_cat* (. *config* :find :selected_cat))
	(if (not (str? *selected_cat*)) (setq *selected_cat* "All"))
	(setq *search_query* (. *config* :find :search_query))
	(if (not (str? *search_query*)) (setq *search_query* "")))

; Test 1: Catalog structure and completeness
(defq all_algos (catalog-all))
(assert-true "catalog non-empty" (nempty? all_algos))
(assert-eq "catalog has 16 algorithms" 16 (length all_algos))

; Test 2: Categories enumeration
(defq cats (catalog-categories))
(assert-true "categories include All" (find "All" cats))
(assert-true "categories include Sorting" (find "Sorting" cats))
(assert-true "categories include Systems" (find "Systems" cats))

; Test 3: Category filtering
(defq sort_algos (catalog-filter "Sorting" ""))
(assert-eq "sorting category count" 3 (length sort_algos))
(assert-true "sorting contains quicksort" (some (# (eql (first %0) :quicksort)) sort_algos))
(assert-true "sorting contains bsearch" (some (# (eql (first %0) :bsearch)) sort_algos))
(assert-true "sorting contains shuffle" (some (# (eql (first %0) :shuffle)) sort_algos))

(defq math_algos (catalog-filter "Math" ""))
(assert-eq "math category count" 3 (length math_algos))
(assert-true "math contains fft" (some (# (eql (first %0) :fft)) math_algos))
(assert-true "math contains mandelbrot" (some (# (eql (first %0) :mandelbrot)) math_algos))

; Test 4: Text search query
(defq search_lisp (catalog-filter "All" "quicksort"))
(assert-eq "search quicksort matches 1" 1 (length search_lisp))
(assert-eq "search quicksort is :quicksort" :quicksort (first (first search_lisp)))

(defq search_o1 (catalog-filter "All" "O(1)"))
(assert-true "search O(1) finds multiple" (> (length search_o1) 1))

; Test 5: Catalog lookup by ID
(defq entry_lz4 (catalog-find :lz4))
(assert-true "find :lz4 returns entry" (if entry_lz4 :t :nil))
(assert-eq "entry title is LZ4" "LZ4 Streaming Compression" (second entry_lz4))

; Test 6: Markdown conversion
(defq md_lines (catalog-to-markdown entry_lz4))
(assert-true "md has title header" (some (# (starts-with "# LZ4 Streaming Compression" %0)) md_lines))
(assert-true "md has code block open" (some (# (eql "```lisp" %0)) md_lines))
(assert-true "md has code block close" (some (# (eql "```" %0)) md_lines))
(assert-true "md has complexity table" (some (# (find "Complexity Analysis" %0)) md_lines))

; Test 6b: Modular markdown helpers
(defq overview_lines (catalog-overview-markdown entry_lz4))
(assert-true "overview has title header" (some (# (starts-with "# LZ4 Streaming Compression" %0)) overview_lines))
(assert-true "overview has complexity table" (some (# (find "Complexity Analysis" %0)) overview_lines))
(assert-true "overview has implementation header" (some (# (find "ChrysaLisp Implementation" %0)) overview_lines))
(defq idioms_lines (catalog-idioms-markdown entry_lz4))
(assert-true "idioms non-nil for lz4" (if idioms_lines :t :nil))
(assert-true "idioms has header" (some (# (find "Key ChrysaLisp Idioms" %0)) idioms_lines))

; Test 7: Config persistence roundtrip
(config-save :mandelbrot "Math" "fractal")
(setq *selected_id* :nil *selected_cat* :nil *search_query* :nil *config* :nil)
(config-load)
(assert-eq "config roundtrip id" :mandelbrot *selected_id*)
(assert-eq "config roundtrip category" "Math" *selected_cat*)
(assert-eq "config roundtrip search" "fractal" *search_query*)
(pii-remove *config_file*)

(print-summary)

(stream-flush (io-stream "stdout"))
(task-sleep 500000)
(pii-exit)
