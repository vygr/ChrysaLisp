(defun save-kv (kv)
	(cond
		((sym? kv) kv)
		((num? kv) (str kv))
		((cat "{" (str kv) "}"))))

(defun save-map (stream m i)
	(defq ws (pad "" i (ascii-char 9)))
	(write-line stream "")
	(write-line stream (cat ws "( ;map"))
	(. m :each (lambda (k v)
		(unless (or (eql k :buffer) (eql k ":nil"))
			(write stream (cat ws (save-kv k) " "))
			(if (map? v)
				(save-map stream v (inc i))
				(write stream (save-kv v)))
			(write-line stream ""))))
	(write stream (cat ws ")")))

(defun save-state ()
	;save editor state
	(when (defq stream (file-stream (cat *env_home* +state_filename) +file_open_write))
		(save-map stream *meta_map* 0)))

(defun load-map (stream)
	(defq root (Fmap) stack (list (first (read stream (ascii-code " "))) root))
	(while (defq m (pop stack) form (pop stack))
		(each (lambda ((k v))
			(if (list? v) (push stack v (setq v (Fmap))))
			(. m :insert k v)) (partition 2 form)))
	root)

(defun load-state ()
	;load editor state
	(setq *meta_map* (Fmap-kv :files (Fmap)) *open_files* (list))
	(when (defq stream (file-stream (cat *env_home* +state_filename)))
		(setq *meta_map* (load-map stream))
		(bind '(file find_txt replace_txt files)
			(gather *meta_map* :file :find :replace :files))
		(. *find_text* :set_text find_txt)
		(. *replace_text* :set_text replace_txt)
		(defq dead_files (list))
		(. files :each (lambda (file meta)
			(cond
				((= (age file) 0)
					;no such file
					(push dead_files file))
				(:t (bind '(cx cy ax ay sx sy) (gather meta :cx :cy :ax :ay :sx :sy))
					(unless (find file *open_files*) (push *open_files* file))
					(. meta :insert :buffer :nil)
					(populate-buffer file cx cy ax ay sx sy)))))
		(each (# (. files :erase %0)) dead_files)
		(if (find file *open_files*) file)))