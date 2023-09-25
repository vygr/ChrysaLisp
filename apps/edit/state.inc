;;;;;;;;;;;;;;;;;
; state load/save
;;;;;;;;;;;;;;;;;

(defun state-save ()
	;save editor state
	(when (defq stream (file-stream (cat *env_home* +state_filename) +file_open_write))
		(scatter *meta_map*
			:find (id-encode (. *find_text* :get_text))
			:replace (id-encode (. *replace_text* :get_text))
			:whole_words *whole_words*
			:regexp *regexp*
			:macros (reduce (# (. %0 :insert %1 (macro-encode %1)))
						+range_0_10 (Lmap)))
		(tree-save stream *meta_map* '(":nil" :buffer :nil))))

(defun state-load ()
	;load editor state
	(setq *meta_map* (Lmap-kv :files (Fmap 101)) *open_files* (list))
	(when (defq stream (file-stream (cat *env_home* +state_filename)))
		(setq *meta_map* (tree-load stream))
		(bind '(file find_txt replace_txt files macros wwords regx)
			(gather *meta_map* :file :find :replace :files
				:macros :whole_words :regexp))
		(if find_txt (. *find_text* :set_text (id-decode find_txt)))
		(if replace_txt (. *replace_text* :set_text (id-decode replace_txt)))
		(if wwords (setq *whole_words* wwords))
		(if regx (setq *regexp* regx))
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
			((setq *meta_map* (Lmap-kv :files (Fmap 101)))))
		(update-find-toolbar)
		(if (find file *open_files*) file)))
