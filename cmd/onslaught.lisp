(import "lib/options/options.inc")
(import "apps/games/onslaught/enums.inc")
(import "apps/games/onslaught/app.inc")

(defq usage `(
(("-h" "--help")
"Usage: onslaught [options]

    options:
        -h --help: this help info.
        -s --state: print the full game state.
        -k --keys num: set the held control keys mask.
        -b --bot secs: let the bot play for secs seconds.
        -q --quit: quit the game.

    Remote play a running Onslaught game, on any node,
    via its @Onslaught service.

    Key mask bits: up 1, down 2, left 4, right 8, fire 16.

    With no options prints a one line summary.")
(("-s" "--state") ,(opt-flag 'opt_s))
(("-k" "--keys") ,(opt-num 'opt_k))
(("-b" "--bot") ,(opt-num 'opt_b))
(("-q" "--quit") ,(opt-flag 'opt_q))
))

(defun enemy-tile? (tile)
	(and tile (/= tile +frm_campain_player) (>= tile +frm_campain_plague)))

(defun bot-map (state)
	; attack an enemy location, else head for one via our own lands
	(bind '(x y) (. state :find :location))
	(bind '(ox oy) (. state :find :back))
	(defq tiles (. state :find :map)
		moves (reduce (lambda (out (dx dy key))
				(defq nx (+ x dx) ny (+ y dy))
				(if (and (<= 0 nx 15) (<= 0 ny 15))
					(push out (list nx ny key (elem-get tiles (+ nx (* ny 16)))))
					out))
			(list (list -1 0 +fkey_left) (list 1 0 +fkey_right)
				(list 0 -1 +fkey_up) (list 0 1 +fkey_down)) (list)))
	(cond
		((enemy-tile? (elem-get tiles (+ x (* y 16)))) +fkey_keya)
		((/= ox -1)
			;only legal move is back
			(some (lambda ((nx ny key tile)) (if (and (= nx ox) (= ny oy)) key)) moves))
		((some (lambda ((nx ny key tile)) (if (enemy-tile? tile) key)) moves))
		(:t (third (elem-get moves (random (length moves)))))))

(defun bot-toward (px tx)
	; (bot-toward px tx) -> key
	(if (< px tx) +fkey_right +fkey_left))

(defun land-flags (col row)
	; off the map is solid, as it is in the game
	(if (and (<= 0 col 127) (<= 0 row 15))
		(code land 1 (+ col (* row 128)))
		+fmap_stand))

(defun supported? (col row)
	; can the man, two tiles wide, stand with his feet at the top of this row
	(or (bits? (land-flags col row) +fmap_stand +fmap_climb)
		(bits? (land-flags (inc col) row) +fmap_stand +fmap_climb)))

(defun solid? (col row)
	; is there proper floor, not just a ladder, under either foot
	(or (= (logand (land-flags col row) (const (+ +fmap_stand +fmap_climb))) +fmap_stand)
		(= (logand (land-flags (inc col) row) (const (+ +fmap_stand +fmap_climb))) +fmap_stand)))

(defun ladder? (col row)
	(and (bits? (land-flags col row) +fmap_climb)
		(bits? (land-flags (inc col) row) +fmap_climb)))

(defun bot-moves (col row)
	; (bot-moves col row) -> ((col row move) ...), where walking, climbing
	; and jumping can take the man from this cell
	(defq out (list) can_climb (and (> row 0) (ladder? col (dec row))))
	(each (lambda ((dir key))
			(when (<= 0 (defq ncol (+ col dir)) 126)
				;walk, and fall if nothing is there
				(defq nrow row)
				(while (not (supported? ncol nrow)) (++ nrow))
				(push out (list ncol nrow key)))
			;jump up a tile, or across a gap, but only from proper floor where
			;up would not climb, the game climbs if it can, onto proper floor
			(unless (or can_climb (not (solid? col row)))
				(each (lambda ((dc dr))
						(defq ncol (+ col (* dir dc)) nrow (+ row dr))
						;a level jump is only for crossing a real gap
						(and (<= 0 ncol 126) (>= nrow 0) (solid? ncol nrow)
							(or (/= dr 0) (not (supported? (+ col dir) row)))
							(push out (list ncol nrow (list key)))))
					'((1 -1) (2 -1) (3 0)))))
		(list (list -1 +fkey_left) (list 1 +fkey_right)))
	(if can_climb (push out (list col (dec row) +fkey_up)))
	(if (ladder? col row) (push out (list col (inc row) +fkey_down)))
	out)

(defun bot-route (goal_col goal_row)
	; (bot-route goal_col goal_row) -> route, the move to make from each cell
	;to reach the goal, :t at the goal, :nil if it cannot be reached.
	;done once per battle, searching back from the goal over reversed moves
	(defq cells (range 0 (* 127 17)) route (map (lambda (i) :nil) cells)
		back (map (lambda (i) (list)) cells) queue (list) qi -1)
	(each (lambda (i)
			(defq col (% i 127) row (/ i 127))
			(when (supported? col row)
				(each (lambda ((ncol nrow move))
						(push (elem-get back (+ ncol (* nrow 127))) (list i move)))
					(bot-moves col row)))
			;be a good neighbour, this is a long job
			(if (= (logand i 63) 0) (task-slice)))
		cells)
	(each (lambda (col)
			(elem-set route (defq i (+ col (* goal_row 127))) :t)
			(push queue i))
		(list (dec goal_col) goal_col))
	(while (< (++ qi) (length queue))
		(each (lambda ((i move))
				(unless (elem-get route i)
					(elem-set route i move)
					(push queue i)))
			(elem-get back (elem-get queue qi))))
	route)

(defun bot-battle (state tick)
	; fight what is close, else follow the path to the enemy banner
	(ifn (defq man (. state :find :man)) 0
		(bind '(px py pw ph &ignore) man)
		;the enemy banner is the one furthest right, capture needs feet level with its base
		(bind '(& & bx by) (reduce (lambda (best ban)
				(if (and (eql (first ban) :Banner) (or (not best) (> (third ban) (third best)))) ban best))
			(. state :find :bans) :nil))
		(defq col (/ (+ px 8) 16) row (/ (+ py ph) 16)
			overrun (. state :find :stack))
		;too many enemies have got past, 16 loses the battle, so turn and
		;face left to draw them back out and deal with them
		(cond
			((>= overrun 8) (setq retreat :t))
			((<= overrun 2) (setq retreat :nil)))
		(cond
			((and (= (logand tick 3) 0)
					(some (lambda ((& & ex ey)) (and (< (abs (- ex px)) 64) (< (abs (- ey py)) 48)))
						(. state :find :enemies)))
				+fkey_keya)
			(retreat (if (< (logand tick 7) 2) +fkey_left 0))
			((empty? land) (bot-toward px bx))
			;in the air, keep going
			((not (supported? col row)) last_keys)
			(:t (unless route (setq route (bot-route (/ bx 16) (/ (+ by 48) 16))))
				(defq move (elem-get route (+ col (* row 127))))
				(setq last_keys (cond
					((eql move :t) (bot-toward px bx))
					((not move) (bot-toward px bx))
					((list? move)
						;jump, walk a couple of frames to get moving, then leap
						(if (= (% tick 3) 2) +fkey_up (first move)))
					((or (= move +fkey_up) (= move +fkey_down))
						;line up with the ladder first
						(if (<= (abs (- px (* col 16))) 4) move (bot-toward px (* col 16))))
					(:t move)))))))

(defun bot-keys (state tick)
	; (bot-keys state tick) -> keys, fire and menu moves trigger on release
	(defq tap (= (logand tick 1) 0))
	(case (. state :find :state)
		((:title :scores :credits :oracle)
			(if tap +fkey_keya 0))
		(:hiscore
			;if asked for initials, spin round to the ] and enter
			(cond
				((not tap) 0)
				((eql (ifn (. state :find :letter) "]") "]") +fkey_keya)
				(:t +fkey_right)))
		(:menu
			(cond
				((not tap) 0)
				((= (. state :find :menu) +menu_start) +fkey_keya)
				(:t +fkey_up)))
		;the map state can be seen a frame before the map is ready
		(:map (if (and tap (nempty? (. state :find :map))) (bot-map state) 0))
		(:battle (bot-battle state tick))
		(:mind (logior +fkey_left (if tap +fkey_keya 0)))
		(:t 0)))

(defun summary (state)
	(print (. state :find :state) " level " (. state :find :level)
		" glory " (. state :find :score) " power " (. state :find :power)
		" strength " (. state :find :strength) " territory " (. state :find :territory)
		" at " (. state :find :location) " army " (. state :find :army)
		" enemies " (length (. state :find :enemies)) " overrun " (. state :find :stack)
		" max stack " (. state :find :max_stack))
	(stream-flush (io-stream 'stdout)))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_s :nil opt_k :nil opt_b :nil opt_q :nil args (options stdio usage)))
		(cond
			((not (onslaught-service))
				(print "No Onslaught game running !"))
			(opt_q (onslaught-quit-rpc))
			(opt_k (onslaught-keys-rpc opt_k))
			(opt_b
				;poll at 20Hz, the game frame rate
				(defq tick 0 last_keys 0 last_state :nil land "" route :nil retreat :nil
					end (+ (pii-time) (* opt_b 1000000)))
				(while (and (< (pii-time) end) (defq state (onslaught-state-rpc)))
					(++ tick)
					;report each change of state, and every 10 seconds
					(when (or (nql last_state (defq name (. state :find :state)))
							(= (% tick 200) 0))
						(setq last_state name)
						(summary state))
					;fetch the terrain once the battle is set up, the man is there
					(cond
						((nql name :battle) (setq land "" route :nil))
						((and (empty? land) (. state :find :man))
							(setq land (ifn (onslaught-land-rpc) ""))))
					(onslaught-keys-rpc (bot-keys state tick))
					(task-sleep 50000))
				(onslaught-keys-rpc 0)
				(if (defq state (onslaught-state-rpc))
					(summary state)
					(print "Game stopped responding !")))
			((defq state (onslaught-state-rpc))
				(if opt_s
					(. state :each (lambda (k v) (print k " " v)))
					(summary state))))))
