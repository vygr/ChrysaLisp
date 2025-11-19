
;; Browser Navigation Tests
;; Tests for Back/Forward navigation and history tracking

(import "lib/test/unittest.inc")

(deftest-suite "Browser Navigation Tests")

;; Mock HTML-Canvas class for testing navigation
(defclass Test-Browser nil
	(defq
		history (list)
		history_position -1
		can_go_back :nil
		can_go_forward :nil
		current_file ""))

(defmethod :load_file Test-Browser (filepath &optional from_history)
	; Simulate loading a file
	(set this :current_file filepath)

	; Add to history if not navigating from history
	(unless from_history
		(. this :add_to_history filepath))
	this)

(defmethod :add_to_history Test-Browser (url)
	; Add URL to history
	(defq history (get :history this))
	(defq pos (get :history_position this))

	; If we're in the middle of history, truncate forward entries
	(when (< pos (dec (length history)))
		(set this :history (slice history 0 (inc pos))))

	; Add new URL
	(push (get :history this) url)
	(set this :history_position (dec (length (get :history this))))

	; Update navigation state
	(. this :update_history_state)
	this)

(defmethod :update_history_state Test-Browser ()
	; Update can_go_back and can_go_forward flags
	(defq pos (get :history_position this))
	(defq hist_len (length (get :history this)))

	(set this :can_go_back (> pos 0))
	(set this :can_go_forward (< pos (dec hist_len)))
	this)

(defmethod :go_back Test-Browser ()
	; Navigate back in history
	(when (get :can_go_back this)
		(defq pos (get :history_position this))
		(defq new_pos (dec pos))
		(set this :history_position new_pos)
		(defq url (elem new_pos (get :history this)))
		(. this :load_file url :t)
		(. this :update_history_state)
		:t))

(defmethod :go_forward Test-Browser ()
	; Navigate forward in history
	(when (get :can_go_forward this)
		(defq pos (get :history_position this))
		(defq new_pos (inc pos))
		(set this :history_position new_pos)
		(defq url (elem new_pos (get :history this)))
		(. this :load_file url :t)
		(. this :update_history_state)
		:t))

(defmethod :get_current_url Test-Browser ()
	; Get current URL
	(get :current_file this))

