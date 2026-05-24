;module
(env-push)

(defun handler (state page line)
	; (handler state page line) -> state
	(cond
		((match? line "^\\s*```")
			(when *mem_stream*
				(stream-seek *mem_stream* 0 0)
				
				;load the memory-stream into a plain-text Document and trim it
				(defq buffer (Document 0 :nil))
				(.-> buffer (:stream_load *mem_stream*) :trim)
				
				;insert a blank line at the front and end of the buffer
				(. buffer :iinsert 0 0 "\n")
				(bind '(& h) (. buffer :get_size))
				(. buffer :iinsert (length (. buffer :get_text_line h)) h "\n")
				
				;measure and configure the Vdu
				(bind '(w h) (. buffer :get_size))
				(def (defq vdu (Vdu))
					:font (get :font_term_small page)
					:vdu_width w
					:vdu_height h
					:color 0
					:ink_color +argb_blue)
				(bind '(tw th) (. vdu :pref_size))
				(.-> vdu (:change 0 0 tw th))
				(.-> buffer (:set_cursor 0 100000)
					(:vdu_load vdu 0 0 :text))
				
				(def (defq text (Text)) :text "")
				(.-> page (:add_child vdu) (:add_child text))
				
				;reset the stream for the next code block
				(setq *mem_stream* :nil))
			:text)
		(:t (ifn *mem_stream* (setq *mem_stream* (memory-stream)))
			(write-line *mem_stream* (expand line 4 0))
			state)))

;module
(export-symbols '(handler))
(env-pop)