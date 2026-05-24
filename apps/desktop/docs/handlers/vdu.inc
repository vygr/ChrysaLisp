;module
(env-push)

(defun handler (state page line)
	; (handler state page line) -> state
	(cond
		((match? line "^\\s*```")
			(when *mem_stream*
				(stream-seek *mem_stream* 0 0)

				;load the memory-stream into a Document and trim it
				(defq buffer (Document +buffer_flag_syntax syntax))
				(.-> buffer (:stream_load *mem_stream*) :trim)

				;insert a blank line at the front and end of the buffer
				(. buffer :iinsert 0 0 "\n")
				(bind '(& h) (. buffer :get_size))
				(. buffer :iinsert (length (. buffer :get_text_line h)) h "\n")

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

				;reset the stream for the next code block
				(setq *mem_stream* :nil))
			:text)
		(:t (ifn *mem_stream* (setq *mem_stream* (memory-stream)))
			(write-line *mem_stream* line)
			state)))

;module
(export-symbols '(handler))
(env-pop)
