;;;;;;;;;;;;;;;;;
; state load/save
;;;;;;;;;;;;;;;;;

(defun config-save ()
	;save editor state
	(defq key (const (cat *env_home* +state_filename)))
	(with-write-lock key
		(bind '(x y width height) (. *window* :get_bounds))
		(scatter *meta_map*
			:version 1
			:x x :y y :width width :height height
			:zoom (get :zoom *window*)
			:find (. *find_text* :get_text)
			:replace (. *replace_text* :get_text)
			:whole_words *whole_words*
			:regexp *regexp*
			:ignore_case *ignore_case*
			:macros (reduce (# (. %0 :insert %1 (macro-encode %1))) +range_0_10 (Lmap)))
		(tree-save (file-stream key +file_open_write) *meta_map* '(":nil" :buffer :nil))))

(defun config-load ()
	;load editor state
	(defq key (const (cat *env_home* +state_filename))
		default (scatter (Emap) :files (Fmap 101)))
	(setq *meta_map* default *open_files* (list))
	(with-read-lock key
		(catch (when (defq data (tree-load (file-stream key)))
			(setq *meta_map* data))
			(setq *meta_map* default))
		(bind '(x y width height version file
				find_txt replace_txt files macros wwords regx icase zoom)
			(gather *meta_map* :x :y :width :height :version
				:file :find :replace :files :macros :whole_words :regexp :ignore_case :zoom))
		(if (and x y) (setq *x* x *y* y))
		(if (and width height) (setq *width* width *height* height))
		(if zoom (def *window* :zoom zoom))
		(if find_txt (. *find_text* :set_text find_txt))
		(if replace_txt (. *replace_text* :set_text replace_txt))
		(if wwords (setq *whole_words* wwords))
		(if regx (setq *regexp* regx))
		(if icase (setq *ignore_case* icase))
		(if macros (. macros :each (lambda (k v) (macro-decode k v))))
		(cond
			(files
				(defq dead_files (list))
				(. files :each (lambda (file meta)
					(cond
						((= (age file) 0)
							;no such file
							(push dead_files file))
						(:t (bind '(cx cy ax ay sx sy)
								(gather meta :cx :cy :ax :ay :sx :sy))
							(push *open_files* file)
							(. meta :insert :buffer :nil)
							(populate-buffer file cx cy ax ay sx sy)))))
				(each (# (. files :erase %0)) dead_files))
			((setq *meta_map* default)))
		(update-find-toolbar)
		(if (find file *open_files*) file)))
