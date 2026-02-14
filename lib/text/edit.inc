; generic edit commands, these assume your environment contains
; a Document object bound to *edit* and a file related to that
; called *file*

(import "lib/text/document.inc")

;module
(env-push)

; define proxy commands
(redefmacro gen-edit (n m) `(defun ,(sym (str "edit-" n)) () (. *edit* ,m)))
(gen-edit top :top) (gen-edit bottom :bottom) (gen-edit home :home) (gen-edit end :end)
(gen-edit bracket-left :left_bracket) (gen-edit bracket-right :right_bracket)
(gen-edit ws-left :left_white_space) (gen-edit ws-right :right_white_space)
(gen-edit select-all :select_all) (gen-edit select-line :select_line)
(gen-edit select-word :select_word) (gen-edit select-block :select_block)
(gen-edit select-form :select_form) (gen-edit select-paragraph :select_paragraph)
(gen-edit select-ws-left :left_white_space_select) (gen-edit select-ws-right :right_white_space_select)
(gen-edit select-bracket-left :left_bracket_select) (gen-edit select-bracket-right :right_bracket_select)
(gen-edit select-home :home_select) (gen-edit select-end :end_select)
(gen-edit select-top :top_select) (gen-edit select-bottom :bottom_select)
(gen-edit primary :primary_cursor)
(gen-edit trim :trim) (gen-edit sort :sort) (gen-edit unique :unique)
(gen-edit upper :to_upper) (gen-edit lower :to_lower)
(gen-edit reflow :reflow) (gen-edit split :split) (gen-edit comment :comment)
(gen-edit cut :cut) (gen-edit copy :copy)

; define proxy commands with optional repeat
(redefmacro gen-edit (n m) `(defun ,(sym (str "edit-" n)) (&optional c) (times (ifn c 1) (. *edit* ,m))))
(gen-edit up :up) (gen-edit down :down) (gen-edit left :left) (gen-edit right :right)
(gen-edit select-left :left_select) (gen-edit select-right :right_select)
(gen-edit select-up :up_select) (gen-edit select-down :down_select)
(gen-edit delete :delete) (gen-edit backspace :backspace)
(gen-edit indent :right_tab) (gen-edit outdent :left_tab)

; more complex commands
(defun edit-get-filename () *file*)
(defun edit-insert (txt) (. *edit* :insert txt))
(defun edit-paste (txt) (. *edit* :paste txt))
(defun edit-find (pattern &rest flags) (. *edit* :find pattern (find :w flags) (find :r flags)))
(defun edit-find-next () (. *edit* :find_next))
(defun edit-find-prev () (. *edit* :find_prev))
(defun edit-find-add-next () (. *edit* :find_add_next))
(defun edit-cursors () (. *edit* :set_found_cursors (. *edit* :get_buffer_found)))
(defun edit-add-cursors () (. *edit* :add_found_cursors (. *edit* :get_buffer_found)))
(defun edit-split-text (txt &optional cls) (split txt (ifn cls "\n\f")))
(defun edit-join-text (txts &optional cls) (if (empty? txts) "" (join txts (ifn cls "\n"))))
(defun edit-get-text () (edit-join-text (edit-split-text (. *edit* :copy))))
(defun edit-print (&rest args) (apply (const print) (if (nempty? args) args (list (edit-get-text)))))
(defun edit-eof? () (= (second (. *edit* :get_cursor)) (second (. *edit* :get_size))))
(defun edit-cx () (first (. *edit* :get_cursor)))
(defun edit-cy () (second (. *edit* :get_cursor)))

; fancy replace
(defun edit-replace (pattern)
	(bind '(regexp wmode rmode) (. *edit* :get_last_find))
	(unless (eql regexp "")
		(defq csr_text (split (. *edit* :copy) "\f") pattern (replace-compile pattern))
		(bind '(engine meta &ignore) (query regexp wmode rmode))
		(defq csr_match (map (# (. engine :search %0 meta)) csr_text)
			new_text (map (# (replace-matches %0 %1 pattern)) csr_text csr_match))
		(. *edit* :paste (join new_text "\f"))))

;module
(export-symbols '(
edit-add-cursors edit-backspace edit-bottom edit-bracket-left
edit-bracket-right edit-comment edit-copy edit-cursors edit-cut edit-cx edit-cy
edit-delete edit-down edit-end edit-eof? edit-find edit-find-add-next
edit-find-next edit-find-prev edit-get-filename edit-get-text
 edit-home edit-indent edit-insert edit-join-text edit-left
edit-lower edit-outdent edit-paste edit-print edit-primary edit-reflow edit-replace
edit-right edit-select-all edit-select-block edit-select-bottom
edit-select-bracket-left edit-select-bracket-right edit-select-down
edit-select-end edit-select-form edit-select-home edit-select-left
edit-select-line edit-select-paragraph edit-select-right edit-select-top
edit-select-up edit-select-word edit-select-ws-left edit-select-ws-right
edit-sort edit-split edit-split-text edit-top edit-trim edit-unique edit-up
edit-upper edit-ws-left edit-ws-right
))
(env-pop)
