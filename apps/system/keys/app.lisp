(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/net/keys.inc")

;The keys of this machine, its keyring, service/net/keys.inc. A row for
;each key, its name, its fingerprint, and a mark on the one in use. Press
;a row to choose it, then use it, give it, rename it or forget it. A new
;key is made by the name written; one given is taken in by the name
;written, from the words pasted below it, or from a file if there are
;none. The key command does the same, key -h.

(enums +select 0
	(enum main picker tip))

(enums +event 0
	(enum close)
	(enum use new take give rename forget)
	(enum pick))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Keys" (+sym_close) +event_close)
	(ui-tool-bar *tools* ()
		(ui-buttons (+sym_done +sym_new +sym_open +sym_save +sym_text +sym_delete) +event_use))
	(ui-textfield *name* (:paper +argb_white :hint_text "name" :clear_text "" :min_width 300))
	(ui-textfield *words* (:paper +argb_white :hint_text "a key given, its words" :clear_text "" :min_width 300))
	(ui-grid *rows* (:grid_width 1))
	(ui-label *status* (:text "" :font :font_body)))

(ui-tool-tips *tools* '("use" "new" "take in" "give" "rename" "forget"))

(defun say (&rest text)
	(set *status* :text (apply (const cat) (map (const str) text)))
	(.-> *status* :layout :dirty))

(defun layout ()
	(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h))

(defun rows ()
	;a row for each key, made again from the keyring
	(each (# (. %0 :sub)) (. *rows* :children))
	(defq using (keys-in-use) key (door-key))
	(setq *names* (keys-names))
	(each (lambda (name)
		(defq row (Flow) button (Button) print (Label))
		(def row :flow_flags +flow_right_fill)
		(def button :text (cat (if (eql name using) "* " "  ") name) :min_width 120)
		(view-variant button :font :font_button)
		(view-variant button :border :border_button)
		(def print :text (cat (keys-print (door-key (keys-file name))) (if (eql name *chosen*) "  chosen" "")) :border 0)
		(view-variant print :font :font_terminal)
		(. button :connect (+ +event_pick (!)))
		(.-> row (:add_child button) (:add_child print))
		(. *rows* :add_child row))
		*names*)
	(.-> *rows* :layout :dirty_all)
	(layout)
	(cond
		((and key using) (say "In use: " using ", " (keys-print key) "."))
		(key (say "In use: one in no keyring file. Write a name and take it in to keep it."))
		((say "No key in use, this machine is off the network. Make one."))))

(defun name-written ()
	(trim (. *name* :get_text)))

(defun chosen? ()
	;the key chosen, :nil and said if none
	(cond
		((and *chosen* (find *chosen* *names*)) *chosen*)
		((say "Choose a key first.") :nil)))

(defun named? ()
	;the name written, :nil and said if it is none a key may have
	(cond
		((keys-name? (defq name (name-written))) name)
		((say "Write a name: letters, digits, - and _.") :nil)))

(defun picker (save)
	;the Files app's picker, for a file a key comes from or goes to
	(if *picker* (mail-send *picker* ""))
	(mail-send (setq *picker_save* save *picker* (open-child "apps/system/files/child.lisp" +kn_call_pin))
		(cat (elem-get *select* +select_picker) (if save "Give key to..." "Take key from...") "|" ".")))

(defun action (id)
	(case id
		(+event_use
			(when (defq name (chosen?))
				(cond
					((and (door-key) (not (keys-in-use)))
						(say "The key in use is in no keyring file, and would be lost. Take it in first."))
					((keys-use name) (say "Using " name ". This machine joins those that have it."))
					((say name " is no key.")))))
		(+event_new
			(when (defq name (named?))
				(if (keys-new name) (setq *chosen* name) (say "There is a key " name " already."))))
		(+event_take
			(when (defq name (named?))
				(defq words (. *words* :get_text))
				(cond
					((pii-fstat (keys-file name)) (say "There is a key " name " already."))
					((nempty? (trim words))
						(cond
							((keys-take-text name words)
								(setq *chosen* name)
								(.-> *words* (:set_text "") :layout :dirty_all))
							((say "Those words are no key."))))
					((and (door-key) (not (keys-in-use)))
						;none pasted: the one in use, in no keyring file, kept
						(keys-keep name) (setq *chosen* name))
					(:t (setq *take_name* name) (picker :nil)))))
		(+event_give
			(when (defq name (chosen?)) (setq *give_name* name) (picker :t)))
		(+event_rename
			(and (defq old (chosen?)) (defq new (named?))
				(if (keys-rename old new) (setq *chosen* new) (say "There is a key " new " already."))))
		(+event_forget
			(when (defq name (chosen?))
				(defq using (eql name (keys-in-use)))
				(keys-remove name)
				(setq *chosen* :nil)
				(if using (say name " forgotten. It is still the one in use."))))))

(defun main ()
	(defq id :t select (task-mboxes +select_size)
		*select* select *names* (list) *chosen* (keys-in-use)
		*picker* :nil *picker_save* :nil *take_name* :nil *give_name* :nil)
	(def *window* :tip_mbox (elem-get select +select_tip))
	(rows)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					((and (>= id +event_pick) (< id (+ +event_pick (length *names*)))
							(= (getf msg +ev_msg_type) +ev_type_action))
						(setq *chosen* (elem-get *names* (- id +event_pick)))
						(.-> *name* (:set_text *chosen*) :layout :dirty_all)
						(rows))
					((and (>= id +event_use) (<= id +event_forget)
							(= (getf msg +ev_msg_type) +ev_type_action))
						;the rows made again, and what it said, if it said anything
						(defq said (get :text *status*))
						(action id)
						(defq now (get :text *status*))
						(rows)
						(unless (eql said now) (say now) (layout)))
					((. *window* :event msg))))
			(+select_tip
				;what a button is for
				(if (defq view (. *window* :find_id (getf msg +mail_timeout_id)))
					(. view :show_tip)))
			(+select_picker
				;the file chosen, or "" closed
				(mail-send *picker* "")
				(setq *picker* :nil msg (trim msg))
				(cond
					((eql msg ""))
					(*picker_save*
						(save (load (keys-file *give_name*)) msg)
						(say "Key " *give_name* " given to " msg ". Give it only to those you trust."))
					((keys-take *take_name* msg)
						(setq *chosen* *take_name*)
						(rows))
					((say "No key in " msg "."))))))
	(if *picker* (mail-send *picker* ""))
	(gui-sub-rpc *window*))
