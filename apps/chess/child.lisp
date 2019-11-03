;imports
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)

;piece map accses
(defmacro piece-map (_ i)
	`(elem (find ,i (elem 0 ,_)) (elem 1 ,_)))

;evaluation score and board combination
(structure 'score_board 0
	(byte 'score 'bias 'brd))

;description of a pieces movement and capture action
(structure 'move 0
	(byte 'dx 'dy 'len 'flag))

;description of a pieces check influence
(structure 'vector 0
	(byte 'dx 'dy 'len))

;check test, array of pieces that must not be on this vectors from the king
(structure 'test 0
	(byte 'pieces 'vectors))

;control paramaters
(defq max_ply 20 max_chess_moves (/ 218 2) max_score_entries 100000)

;piece values, in centipawns
(defq king_value 20000 queen_value 900 rook_value 500 bishop_value 330
	knight_value 320 pawn_value 100 mate_value (* king_value 10) timeout_value (* mate_value 2))

;board square/piece types
(defq white 1 empty 0 black -1)

;piece capture actions, per vector
(defq no_capture 0 may_capture 1 must_capture 2)

;map board square contents to piece type/colour
(defq piece_type_map (list "prnbkqPRNBKQ "
	(list black black black black black black white white white white white white empty)))

;piece move vectors and capture actions
(defq black_pawn_moves (list
		(list 0 1 0 no_capture) (list -1 1 1 must_capture) (list 1 1 1 must_capture))
	white_pawn_moves (list
		(list 0 -1 0 no_capture) (list -1 -1 1 must_capture) (list 1 -1 1 must_capture))
	rook_moves (list
		(list 0 -1 7 may_capture) (list -1 0 7 may_capture) (list 0 1 7 may_capture) (list 1 0 7 may_capture))
	bishop_moves (list
		(list -1 -1 7 may_capture) (list 1 1 7 may_capture) (list -1 1 7 may_capture) (list 1 -1 7 may_capture))
	knight_moves (list
		(list -2 1 1 may_capture) (list 2 -1 1 may_capture) (list 2 1 1 may_capture) (list -2 -1 1 may_capture)
		(list -1 -2 1 may_capture) (list -1 2 1 may_capture) (list 1 -2 1 may_capture) (list 1 2 1 may_capture))
	queen_moves (list
		(list 0 -1 7 may_capture) (list -1 0 7 may_capture) (list 0 1 7 may_capture) (list 1 0 7 may_capture)
		(list -1 -1 7 may_capture) (list 1 1 7 may_capture) (list -1 1 7 may_capture) (list 1 -1 7 may_capture))
	king_moves (list
		(list 0 -1 1 may_capture) (list -1 0 1 may_capture) (list 0 1 1 may_capture) (list 1 0 1 may_capture)
		(list -1 -1 1 may_capture) (list 1 1 1 may_capture) (list -1 1 1 may_capture) (list 1 -1 1 may_capture)))

;map piece to its movement possibilities
(defq moves_map (list "pPrRbBnNqQkK"
	(list black_pawn_moves white_pawn_moves rook_moves rook_moves bishop_moves bishop_moves
		knight_moves knight_moves queen_moves queen_moves king_moves king_moves)))

;piece check vectors, king is tested for being on these vectors for check tests
(defq black_pawn_vectors '((-1 1 1) (1 1 1))
	white_pawn_vectors '((-1 -1 1) (1 -1 1))
	bishop_vectors '((-1 -1 7) (1 1 7) (-1 1 7) (1 -1 7))
	rook_vectors '((0 -1 7) (-1 0 7) (0 1 7) (1 0 7))
	knight_vectors '((-1 -2 1) (-1 2 1) (-2 -1 1) (-2 1 1) (1 -2 1) (1 2 1) (2 -1 1) (2 1 1))
	queen_vectors '((-1 -1 7) (1 1 7) (-1 1 7) (1 -1 7) (0 -1 7) (-1 0 7) (0 1 7) (1 0 7))
	king_vectors '((-1 -1 1) (1 1 1) (-1 1 1) (1 -1 1) (0 -1 1) (-1 0 1) (0 1 1) (1 0 1)))

;check tests, piece types given can not be on the vectors given
(defq white_tests (list
		(list "qb" bishop_vectors) (list "qr" rook_vectors) (list "n" knight_vectors)
		(list "k" king_vectors) (list "p" white_pawn_vectors))
	black_tests (list
		(list "QB" bishop_vectors) (list "QR" rook_vectors) (list "N" knight_vectors)
		(list "K" king_vectors) (list "P" black_pawn_vectors)))

;map piece to black/white scores for board evaluation
(defq piece_values_map (list "kKqQrRbBnNpP"
	(list (list king_value 0) (list 0 king_value) (list queen_value 0) (list 0 queen_value)
		(list rook_value 0) (list 0 rook_value) (list bishop_value 0) (list 0 bishop_value)
		(list knight_value 0) (list 0 knight_value) (list pawn_value 0) (list 0 pawn_value))))

;pawn values for position in board evaluation
(defq pawn_position_values '(
	0 0 0 0 0 0 0 0
	50 50 50 50 50 50 50 50
	10 10 20 30 30 20 10 10
	5 5 10 25 25 10 5 5
	0 0 0 20 20 0 0 0
	5 -5 -10 0 0 -10 -5 5
	5 10 10 -20 -20 10 10 5
	0 0 0 0 0 0 0 0))

;knight values for position in board evaluation
(defq knight_position_values '(
	-50 -40 -30 -30 -30 -30 -40 -50
	-40 -20 0 0 0 0 -20 -40
	-30 0 10 15 15 10 0 -30
	-30 5 15 20 20 15 5 -30
	-30 0 15 20 20 15 0 -30
	-30 5 10 15 15 10 5 -30
	-40 -20 0 5 5 0 -20 -40
	-50 -40 -30 -30 -30 -30 -40 -50))

;bishop values for position in board evaluation
(defq bishop_position_values '(
	-20 -10 -10 -10 -10 -10 -10 -20
	-10 0 0 0 0 0 0 -10
	-10 0 5 10 10 5 0 -10
	-10 5 5 10 10 5 5 -10
	-10 0 10 10 10 10 0 -10
	-10 10 10 10 10 10 10 -10
	-10 5 0 0 0 0 5 -10
	-20 -10 -10 -10 -10 -10 -10 -20))

;rook values for position in board evaluation
(defq rook_position_values '(
	0 0 0 0 0 0 0 0
	5 10 10 10 10 10 10 5
	-5 0 0 0 0 0 0 -5
	-5 0 0 0 0 0 0 -5
	-5 0 0 0 0 0 0 -5
	-5 0 0 0 0 0 0 -5
	-5 0 0 0 0 0 0 -5
	0 0 0 5 5 0 0 0))

;queen values for position in board evaluation
(defq queen_position_values '(
	-20 -10 -10 -5 -5 -10 -10 -20
	-10 0 0 0 0 0 0 -10
	-10 0 5 5 5 5 0 -10
	-5 0 5 5 5 5 0 -5
	0 0 5 5 5 5 0 -5
	-10 5 5 5 5 5 0 -10
	-10 0 5 0 0 0 0 -10
	-20 -10 -10 -5 -5 -10 -10 -20))

;king values for position in board evaluation
(defq king_position_values '(
	-30 -40 -40 -50 -50 -40 -40 -30
	-30 -40 -40 -50 -50 -40 -40 -30
	-30 -40 -40 -50 -50 -40 -40 -30
	-30 -40 -40 -50 -50 -40 -40 -30
	-20 -30 -30 -40 -40 -30 -30 -20
	-10 -20 -20 -20 -20 -20 -20 -10
	20 20 0 0 0 0 20 20
	20 30 10 0 0 10 30 20))

;map piece to position value table
(defq piece_positions_map (list "kKqQrRbBnNpP"
	(list king_position_values king_position_values queen_position_values queen_position_values
		rook_position_values rook_position_values bishop_position_values bishop_position_values
		knight_position_values knight_position_values pawn_position_values pawn_position_values)))

;generate all first hit pieces from index position along given vectors
(defun-bind piece-scans (brd index vectors)
	(defq yield (list) cx (% index 8) cy (/ index 8))
	(each (lambda ((dx dy len))
		(defq x cx y cy)
		(while (>= (setq len (dec len)) 0)
			(setq x (+ x dx) y (+ y dy))
			(when (and (<= 0 x 7) (<= 0 y 7))
				;still on the board
				(defq piece (elem (+ (* y 8) x) brd))
				(unless (eql piece " ")
					;not empty square so yield piece
					(push yield piece)
					(setq len 0))))) vectors)
	(if (= (length yield) 0) "" (apply cat yield)))

;test if king of given colour is in check
(defun-bind in-check (brd colour)
	(if (= colour black)
		(defq king_piece "k" tests black_tests)
		(defq king_piece "K" tests white_tests))
	;find king index on board
	(unless (eql (elem king_index brd) king_piece)
		(setq king_index (find king_piece brd)))
	(some (lambda ((pieces vectors))
		(defq hit_pieces (piece-scans brd king_index vectors))
		(some (lambda (piece)
			(find piece hit_pieces)) pieces)) tests))

;evaluate (score) a board for the colour given
(defun-bind evaluate (brd colour)
	(defq black_score 0 white_score 0)
	(each (lambda (piece)
		;add score for position on the board, near center, clear lines etc
		(unless (eql piece " ")
			(if (> (code piece) (const (code "Z")))
				(setq black_score (+ black_score (elem (- 63 _) (piece-map piece_positions_map piece))))
				(setq white_score (+ white_score (elem _ (piece-map piece_positions_map piece)))))
			;add score for piece type, queen, rook etc
			(defq values (piece-map piece_values_map piece))
			(setq black_score (+ black_score (elem 0 values))
				white_score (+ white_score (elem 1 values))))) brd)
	(* (- white_score black_score) colour))

;generate all boards for a piece index and moves possibility, filtering out boards where king is in check
(defun-bind piece-moves (yield brd index colour moves)
	(defq piece (elem index brd) cx (% index 8) cy (/ index 8)
		promote (if (= colour white) "QRBN" "qrbn"))
	(each (lambda ((dx dy len flag))
		(defq x cx y cy)
		;special length for pawns so we can adjust for starting 2 hop
		(when (= len 0)
			(setq len 1)
			(if (eql piece "p")
				(if (= y 1) (setq len 2))
				(if (= y 6) (setq len 2))))
		(while (>= (setq len (dec len)) 0)
			(setq x (+ x dx) y (+ y dy))
			(cond
				((and (<= 0 x 7) (<= 0 y 7))
					(defq newindex (+ (* y 8) x) newpiece (elem newindex brd)
						newtype (piece-map piece_type_map newpiece))
					(cond
						((= newtype colour)
							;hit one of our own piece type (black hit black etc)
							(setq len 0))
						((and (= flag no_capture) (/= newtype empty))
							;not suposed to capture and not empty square
							(setq len 0))
						((and (= flag must_capture) (= newtype empty))
							;must capture and got empty square
							(setq len 0))
						(t
							(defq newbrd (cat (slice 0 index brd) " " (slice (inc index) -1 brd)))
							(cond
								((and (or (= y 0) (= y 7)) (or (eql piece "P") (eql piece "p")))
									;try all the pawn promotion possibilities
									(each (lambda (promote_piece)
										(setq newbrd (cat (slice 0 newindex newbrd) promote_piece (slice (inc newindex) -1 newbrd)))
										(unless (in-check newbrd colour)
											(push yield (list (evaluate newbrd colour) 0 newbrd)))) promote))
								(t
									;generate this as a possible move
									(setq newbrd (cat (slice 0 newindex newbrd) piece (slice (inc newindex) -1 newbrd)))
									(unless (in-check newbrd colour)
										(push yield (list (evaluate newbrd colour) 0 newbrd)))))
							(if (and (= flag may_capture) (/= newtype empty))
								;may capture and we did so !
								(setq len 0)))))
				(t ;gone off the board
					(setq len 0))))) moves))

;generate all moves (boards) for the given colours turn
(defun-bind all-moves (brd colour)
	;enumarate the board square by square
	(task-sleep 0)
	(defq yield (list) king_index 0 is_black (= colour black))
	(each (lambda (piece)
		(unless (eql piece " ")
			(when (eql (> (code piece) (const (code "Z"))) is_black)
				;one of our pieces ! so gather all boards from possible moves of this piece
				(piece-moves yield brd _ colour (piece-map moves_map piece))))) brd) yield)

;pvs alpha/beta pruning minmax search for given ply
(defun-bind score-impl (sbrd colour alpha beta ply)
	(if (= ply 0)
		(neg (elem score_board_score sbrd))
		(progn
			(defq next_boards (all-moves (elem score_board_brd sbrd) colour) mate t result nil)
			(when (/= (length next_boards) 0)
				(if (> ply 1)
					(sort (lambda (x y)
						(> (elem score_board_score x) (elem score_board_score y))) next_boards))
				(setq result (some (lambda (score_board)
					(cond
						((not mate)
							;not first child so null search window
							(defq value (neg (score-impl score_board (neg colour) (dec (neg alpha)) (neg alpha) (dec ply))))
							(and (< alpha value) (< value beta)
								;failed high, so full re-search
								(setq value (neg (score-impl score_board (neg colour) (neg beta) (neg alpha) (dec ply))))))
						(t
							(defq value (neg (score-impl score_board (neg colour) (neg beta) (neg alpha) (dec ply))))))
					(setq mate nil)
					(cond
						((> (- (time) start_time) max_time_per_move)
							;time has expired for this move
							timeout_value)
						((>= value mate_value)
							;early return if mate
							value)
						((>= value beta)
							;fail hard beta cutoff
							beta)
						((> value alpha)
							(setq alpha value)
							nil))) next_boards)))
			(cond
				(result result)
				((not mate) alpha)
				((in-check (elem score_board_brd sbrd) colour)
					;check mate
					(- (neg mate_value) ply))
				(t ;stale mate
					mate_value)))))

;best move for given board position for given colour
(defun-bind best-move (brd colour history)
	;first ply of boards
	(defq next_boards (all-moves brd colour))
	(each (lambda (sbrd)
		(elem-set score_board_bias sbrd (neg (* (reduce (lambda (cnt past_brd)
			(if (eql past_brd brd) (inc cnt) cnt)) history 0) queen_value)))) next_boards)
	(cond
		((= (length next_boards) 0) "")
		((= (length next_boards) 1)
			(elem score_board_brd (elem 0 next_boards)))
		(t
			(sort (lambda (x y)
				(> (elem score_board_score x) (elem score_board_score y))) next_boards)
			;start move timer
			(defq start_time (time))
			(some (lambda (ply)
				;iterative deepening of ply so we allways have a best move to go with if the timer expires
				(vdu-print vdu (str (ascii-char 10) "Ply = " ply (ascii-char 10)))
				(defq best_index nil alpha (* mate_value -10) beta (* mate_value 10))
				(some (lambda (score_board)
					(defq score (score-impl score_board (neg colour) (neg beta) (neg alpha) ply))
					(cond
						((eql score timeout_value))
						(t
							(elem-set score_board_score score_board
								(setq score (+ (neg score) (elem score_board_bias score_board))))
							(cond
								((> score alpha)
									;got a better board than last best
									(setq alpha score best_index _)
									(vdu-print vdu "*"))
								(t
									;just tick off another board
									(vdu-print vdu ".")))
							nil))) next_boards)
				(when best_index
					;promote board to PV
					(setq next_boards (cat
						(slice best_index (inc best_index) next_boards)
						(slice 0 best_index next_boards)
						(slice (inc best_index) -1 next_boards))))
				;don't look further ahead if we allready can force mate
				(or (>= alpha mate_value) (<= alpha (neg mate_value)))) (range 1 max_ply))
			(elem score_board_brd (elem 0 next_boards)))))

(defun-bind display-board (board)
	(defq d (range 0 8))
	(vdu-print vdu (const (str (ascii-char 10) (ascii-char 10) "  a   b   c   d   e   f   g   h" (ascii-char 10))))
	(vdu-print vdu (str "+---+---+---+---+---+---+---+---+" (ascii-char 10)))
	(each (lambda (row)
		(vdu-print vdu (str (apply cat (map (lambda (col)
			(cat "| " (elem (+ (* 8 row) col) board) " ")) d)) "| " (- 8 row) (ascii-char 10)))
		(if (/= row 7)
			(vdu-print vdu (str "|---|---|---|---|---|---|---|---|" (ascii-char 10))))) d)
	(vdu-print vdu (str "+---+---+---+---+---+---+---+---+" (ascii-char 10))))

(defun-bind time-in-seconds (_)
		(str (/ _ 1000000) "." (pad (% _ 1000000) 6 "00000")))

(defun-bind main ()
	;read args from parent
	(bind '(vdu max_time_per_move) (mail-mymail))
	(defq brd "rnbqkbnrpppppppp                                PPPPPPPPRNBQKBNR"
		history (list) colour white game_start_time (time) king_index 0 quit nil)
	(until (or (defq msg (mail-trymail)) quit)
		(display-board brd)
		(defq elapsed_time (- (time) game_start_time))
		(vdu-print vdu (str (ascii-char 10) "Elapsed Time: " (time-in-seconds elapsed_time) (ascii-char 10)))
		(if (= colour white)
			(vdu-print vdu (str "White to move:" (ascii-char 10)))
			(vdu-print vdu (str "Black to move:" (ascii-char 10))))
		(defq new_brd (best-move brd colour history))
		(when (eql new_brd "")
			(if (in-check brd colour)
				(vdu-print vdu (str (ascii-char 10) "** Checkmate **" (ascii-char 10) (ascii-char 10)))
				(vdu-print vdu (str (ascii-char 10) "** Stalemate **" (ascii-char 10) (ascii-char 10))))
			(setq quit t))
		(when (>= (reduce (lambda (cnt past_brd)
				(if (eql past_brd brd) (inc cnt) cnt)) history 0) 3)
			(vdu-print vdu (str (ascii-char 10) "** Draw **" (ascii-char 10) (ascii-char 10)))
			(setq quit t))
		(push history new_brd)
		(setq colour (neg colour) brd new_brd))
	(unless msg (mail-mymail)))

(main)
