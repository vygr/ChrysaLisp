;module
(env-push)

(defun handler (state page line)
	; (handler state page line) -> state
	(cond
		((match? line "^\\s*```")
			(when *mem_stream*
				(stream-seek *mem_stream* 0 0)

				;load the memory-stream into a plain-text Document
				;trim and frame it nicely
				(.-> (defq buffer (Document 0 :nil))
					(:stream_load *mem_stream*) :trim
					(:insert "\n") :bottom (:insert "\n"))

				;measure and configure the Vdu
				(bind '(w h) (. buffer :get_size))
				(def (defq vdu (Vdu))
					:font (get :font_terminal_small page) :vdu_width 80 :vdu_height h
					:color 0 :ink_color +argb_blue)
				(bind '(tw th) (. vdu :pref_size))
				(def vdu :vdu_width w)
				(bind '(rw rh) (. vdu :pref_size))
				(.-> vdu (:change 0 0 rw rh))
				(.-> buffer (:set_cursor 0 100000) (:vdu_load vdu 0 0 :text))

				;embed in horizontal scroll only if wider than standard width
				(defq widget vdu)
				(when (> rw tw)
					(def (defq scroll (Scroll +scroll_flag_horizontal))
						:min_width tw :min_height rh)
					(. scroll :add_child (. vdu :change 0 0 rw rh))
					(setq widget scroll))
				(def (defq text (Text)) :text "")
				(.-> page (:add_child widget) (:add_child text))

				;reset the stream for the next code block
				(setq *mem_stream* :nil))
			:text)
		(:t (ifn *mem_stream* (setq *mem_stream* (memory-stream)))
			(write-line *mem_stream* (expand line 4 0))
			state)))

;module
(export-symbols '(handler))
(env-pop)
