;module
(env-push)

(defun handler (state page line)
	; (handler state page line) -> state
	(cond
		((match? line "^\\s*```")
			:text)
		(:t (bind '(file &optional start_exp end_exp)
				(first (read (string-stream (cat "(" line ")")))))
			;skip to start and write directly to a memory-stream
			(defq stream (file-stream file))
			(setq *mem_stream* (memory-stream))
			(while (and start_exp (defq line (read-line stream)))
				(when (found? line start_exp)
					(write-line *mem_stream* (trim line "\r"))
					(setq start_exp :nil)))
			;skip to end
			(cond
				((eql end_exp "")
					(while (and end_exp (defq line (read-line stream)))
						(if (or (eql line "") (eql line "\r"))
							(setq end_exp :nil)
							(write-line *mem_stream* (trim line "\r")))))
				(end_exp
					(while (and end_exp (defq line (read-line stream)))
						(if (found? line end_exp)
							(setq end_exp :nil)
							(write-line *mem_stream* (trim line "\r")))))
				(:t (while (defq line (read-line stream))
						(write-line *mem_stream* (trim line "\r")))))
			(stream-seek *mem_stream* 0 0)

			;load the memory-stream into a plain-text Document
			;trim and frame it nicely
			(defq buffer (Document +buffer_flag_syntax syntax))
			(.-> buffer (:stream_load *mem_stream*) :trim
				(:insert "\n") :bottom (:insert "\n"))

			;calculate the minimum margin directly from the pre-expanded buffer lines
			(defq margin +max_int lines (. buffer :get_buffer_lines))
			(each (# (task-slice)
					(and (defq m (find "(" %0)) (setq margin (min m margin))))
				lines)
			;strip the common margin in-place
			(when (and (/= margin 0) (/= margin +max_int))
				(each (lambda (&ignore) (task-slice)
						(. buffer :idelete 0 (!)
							(min margin (length (. buffer :get_text_line (!)))) (!)))
					lines))

			;measure and configure the Vdu
			(bind '(w h) (. buffer :get_size))
			(def (defq vdu (Vdu))
				:font (get :font_term_small page)
				:vdu_width 80
				:vdu_height h
				:color 0
				:ink_color +argb_black)
			(bind '(tw th) (. vdu :pref_size))
			(def vdu :vdu_width w)
			(bind '(rw rh) (. vdu :pref_size))
			(.-> vdu (:change 0 0 rw rh))
			(.-> buffer (:set_cursor 0 100000)
				(:vdu_load vdu 0 0 :text))

			;layout container backdrop and optional scroll
			(def (defq backdrop (Backdrop))
				:color +argb_grey1
				:min_width rw
				:min_height rh)
			(def (defq text (Text)) :text "")
			(. backdrop :add_child vdu)
			(when (> rw tw)
				(def (defq scroll (Scroll +scroll_flag_horizontal))
					:min_width tw
					:min_height rh)
				(. scroll :add_child (. backdrop :change 0 0 rw rh))
				(setq backdrop scroll))
			(.-> page (:add_child backdrop) (:add_child text))

			;reset the stream for the next handler
			(setq *mem_stream* :nil)
			state)))

;module
(export-symbols '(handler))
(env-pop)
