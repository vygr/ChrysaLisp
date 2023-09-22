;;;;;;;;;;;;;;;;;
; state load/save
;;;;;;;;;;;;;;;;;

(defun save-state ()
	;save editor state
	(when (defq stream (file-stream (cat *env_home* +state_filename) +file_open_write))
		(defq macros (Fmap))
		(each (# (. macros :insert %0 (encode-macro %0))) +range_0_10)
		(scatter *meta_map*
			:find (id-encode (. *find_text* :get_text))
			:replace (id-encode (. *replace_text* :get_text))
			:macros macros)
		(maptree-save stream *meta_map* '(":nil" :buffer :nil))))

(defun load-state ()
	;load editor state
	(setq *meta_map* (Fmap-kv :files (Fmap)) *open_files* (list))
	(when (defq stream (file-stream (cat *env_home* +state_filename)))
		(setq *meta_map* (maptree-load stream))
		(bind '(file find_txt replace_txt files macros)
			(gather *meta_map* :file :find :replace :files :macros))
		(if find_txt (. *find_text* :set_text (id-decode find_txt)))
		(if replace_txt (. *replace_text* :set_text (id-decode replace_txt)))
		(if macros (. macros :each (lambda (k v) (decode-macro k v))))
		(cond
			(files
				(defq dead_files (list))
				(.-> files (:resize 101) (:each (lambda (file meta)
					(cond
						((= (age file) 0)
							;no such file
							(push dead_files file))
						(:t (bind '(cx cy ax ay sx sy)
								(gather meta :cx :cy :ax :ay :sx :sy))
							(push *open_files* file)
							(. meta :insert :buffer :nil)
							(populate-buffer file cx cy ax ay sx sy))))))
				(each (# (. files :erase %0)) dead_files))
			((setq *meta_map* (Fmap-kv :files (Fmap)))))
		(if (find file *open_files*) file)))
