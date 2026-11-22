(import "lib/media/module.inc")
(import "lib/media/tracker.inc")
(import "apps/media/jukebox/library.inc")

(report-header "Media: Tracker Player")

;songs made here byte by byte, a MOD and an IT, none of anyone's music

(defun tt-bytes (vals)
	; (tt-bytes vals) -> str
	(apply (const cat) (map (# (char (logand %0 0xff) 1)) vals)))

(defun tt-le (n width)
	; (tt-le n width) -> str
	(char (logand n (dec (<< 1 (* 8 width)))) width))

(defun tt-be16 (n)
	; (tt-be16 n) -> str
	(tt-bytes (list (>> n 8) n)))

(defun tt-zeros (n)
	; (tt-zeros n) -> str
	(pad "" n "\x00"))

(defun tt-text (text n)
	; (tt-text text n) -> str
	(cat text (tt-zeros (- n (length text)))))

(defun tt-mod (cells)
	; (tt-mod cells) -> str
	;a MOD of 4 channels, one pattern, one sample of 32 bytes, its cells
	;((row channel period sample effect param) ...)
	(defq pattern (map! (# 0) '() 0 1024))
	(each (lambda ((row ch period smp fx param))
			(defq at (* 4 (+ (* row 4) ch)))
			(elem-set pattern at (logior (logand smp 0xf0) (>> period 8)))
			(elem-set pattern (+ at 1) (logand period 255))
			(elem-set pattern (+ at 2) (logior (<< (logand smp 15) 4) fx))
			(elem-set pattern (+ at 3) param))
		cells)
	(cat (tt-text "test" 20)
		(tt-zeros 22) (tt-be16 16) (tt-bytes '(0 64)) (tt-be16 0) (tt-be16 1)
		(tt-zeros (* 30 30))
		(tt-bytes '(1 127)) (tt-zeros 128) "M.K."
		(tt-bytes pattern) (tt-bytes (map! (# 64) '() 0 32))))

(defun tt-it (events &optional nna ifc ifr cwt cmwt param0)
	; (tt-it events [nna ifc ifr cwt cmwt param0]) -> str
	;an IT of one instrument, one sample of 64 bytes looped, one pattern of
	;4 rows, its events ((row channel note ins fx param) ...), note 60 C-5;
	;param0 the text of a MIDI macro of the song's own for SF0
	(setd nna 0 ifc 0 ifr 0 cwt 0x214 cmwt 0x214)
	(defq midi (if param0 (cat (tt-zeros (* 9 32)) (tt-text param0 32) (tt-zeros (* 143 32))) "")
		ins_at (+ 206 (length midi)) smp_at (+ ins_at 554) pat_at (+ smp_at 80) data "")
	(each! (lambda ()
			;the row, as (!) in the each is of the events
			(defq at_row (!))
			(each (lambda ((row ch note ins fx param))
					(when (= row at_row)
						(setq data (cat data (tt-bytes (list (logior (inc ch) 128) (if (> fx 0) 11 3) note ins))
							(if (> fx 0) (tt-bytes (list fx param)) "")))))
				events)
			(setq data (cat data (tt-bytes '(0)))))
		'() 0 4)
	(defq pattern (cat (tt-le (length data) 2) (tt-le 4 2) (tt-zeros 4) data))
	(cat "IMPM" (tt-text "test" 26) (tt-zeros 2)
		(tt-le 2 2) (tt-le 1 2) (tt-le 1 2) (tt-le 1 2) (tt-le cwt 2) (tt-le cmwt 2)
		(tt-le (if param0 (+ 13 128) 13) 2) (tt-le 0 2)
		(tt-bytes '(128 48 6 125 128 0)) (tt-le 0 2) (tt-le 0 4) (tt-le 0 4)
		(tt-bytes (map! (# 32) '() 0 64)) (tt-bytes (map! (# 64) '() 0 64))
		(tt-bytes '(0 255)) (tt-le ins_at 4) (tt-le smp_at 4) (tt-le pat_at 4)
		midi
		;the instrument
		"IMPI" (tt-zeros 13) (tt-bytes (list nna 0 0)) (tt-le 0 2) (tt-bytes '(0 0 128 128 0 0))
		(tt-zeros 4) (tt-text "ins" 26) (tt-bytes (list ifc ifr 0 0)) (tt-zeros 2)
		(tt-bytes (apply (const cat) (map! (# (list (!) 1)) '() 0 120)))
		(tt-zeros (+ (* 3 82) 4))
		;the sample, and the pattern, then its bytes
		"IMPS" (tt-zeros 13) (tt-bytes '(64 17 64)) (tt-text "smp" 26) (tt-bytes '(1 0))
		(tt-le 64 4) (tt-le 0 4) (tt-le 64 4) (tt-le 8363 4) (tt-le 0 4) (tt-le 0 4)
		(tt-le (+ pat_at (length pattern)) 4) (tt-zeros 4)
		pattern (tt-bytes (map! (# 64) '() 0 64))))

(defun tt-ticks (player n)
	; (tt-ticks player n) -> player
	(times n (tracker-tick player))
	player)

(defun tt-played (song)
	; (tt-played song) -> frames
	;how long a song is played, every tick of every channel, as (tracker-length)
	;finds it with only the ticks that move the song on
	(defq player (tracker-player song 44100) frames 0)
	(until (. player :find :ended) (setq frames (+ frames (tracker-tick player))))
	frames)

; --- MOD ---
(defq song (module-read (tt-mod '())))
(assert-eq "MOD read, its title" "test" (. song :find :title))
(assert-eq "MOD read, its channels" 4 (. song :find :channels))
(assert-eq "MOD read, its sample" 32 (. (first (. song :find :samples)) :find :length))
(assert-eq "MOD length, 64 rows of 6 ticks of 882 frames" 338688 (tracker-length song 44100))
(assert-eq "MOD length, F03 speed 3" 169344 (tracker-length (module-read (tt-mod '((0 0 0 0 15 3)))) 44100))
(assert-eq "MOD length, D00 at row 15" 84672 (tracker-length (module-read (tt-mod '((15 0 0 0 13 0)))) 44100))
(defun tt-from (player frames)
	; (tt-from player frames) -> frames
	;how long a player goes on from so many frames in, to its end
	(until (. player :find :ended) (setq frames (+ frames (tracker-tick player))))
	frames)
(defq mod_song (module-read (tt-mod '((0 0 428 1 15 3)))) measure (tracker-measure mod_song 44100))
(until (tracker-measure-step measure 1000000))
(assert-eq "MOD order starts, the one order, before its row sets speed 3" "((0 0 0 6 125 64 0))" (str (. measure :find :starts)))
(assert-eq "MOD from an order start, as long" (tracker-length mod_song 44100)
	(tt-from (tracker-at mod_song 44100 (first (. measure :find :starts))) 0))
(each (lambda (cells)
		(defq mod_song (module-read (tt-mod cells)))
		(assert-eq "MOD measured as played" (tt-played mod_song) (tracker-length mod_song 44100)))
	'(() ((0 0 428 1 15 3) (1 1 428 1 4 0x44)) ((15 0 428 1 13 0) (3 1 428 1 14 0xe2))))
(assert-eq "MOD A01, a row's 5 slides down"
	59 (. (first (. (tt-ticks (tracker-player (module-read (tt-mod '((0 0 428 1 10 1)))) 44100) 6)
		:find :chans)) :find :volume))

; --- IT ---
(defq notes '((0 0 60 1 0 0) (1 0 62 1 0 0) (2 0 64 1 0 0)))
(setq song (module-read (tt-it notes)))
(assert-eq "IT read, its mode" :it (. song :find :mode))
(assert-eq "IT length, 4 rows of 6 ticks" 21168 (tracker-length song 44100))
(each (lambda ((what events))
		(defq it_song (module-read (tt-it events)))
		(assert-eq (cat "IT measured as played, " what) (tt-played it_song) (tracker-length it_song 44100)))
	(list (list "notes" notes) (list "a tempo slide, T18" '((0 0 60 1 20 0x18) (1 1 62 1 0 0)))
		(list "a break, C00 at row 1" '((0 0 60 1 0 0) (1 1 62 1 3 0)))
		(list "a speed, A03 beside notes" '((0 0 60 1 1 3) (0 1 62 1 8 0x44) (2 1 64 1 0 0)))
		(list "a note delay, SD3" '((1 0 60 1 19 0xd3) (2 1 62 1 20 0x12)))))
(assert-eq "IT NNA cut, no past notes"
	0 (length (. (tt-ticks (tracker-player song 44100) 13) :find :past)))
(defq player (tt-ticks (tracker-player (module-read (tt-it notes 1)) 44100) 7))
(assert-eq "IT NNA continue, a past note at the second note" 1 (length (. player :find :past)))
(assert-eq "IT NNA continue, two at the third" 2 (length (. (tt-ticks player 6) :find :past)))
(assert-eq "IT S73, the note it is of cut by the next"
	1 (length (. (tt-ticks (tracker-player (module-read (tt-it
		'((0 0 60 1 0 0) (1 0 62 1 19 0x73) (2 0 64 1 0 0)) 1)) 44100) 13) :find :past)))

;the filter, against libxmp 4.7.4's coefficients, within its float's last
(defun tt-near (a b) (<= (abs (- a b)) 1))
(setq player (tt-ticks (tracker-player (module-read (tt-it notes 0 (+ 128 64))) 44100) 1))
(defq chan (first (. player :find :chans)))
(assert-true "IT filter, the instrument's cutoff 128" (tt-near (. chan :find :fa0) 51874))
(assert-true "IT filter, its b0 and b1"
	(and (tt-near (. chan :find :fb0) 7846511) (tt-near (. chan :find :fb1) -3704081)))
(assert-eq "IT filter, given the voice" (. chan :find :fa0) (elem-get (. player :find :voices) 8))
(assert-eq "IT no filter, a0 0"
	0 (. (first (. (tt-ticks (tracker-player song 44100) 1) :find :chans)) :find :fa0))
(setq chan (first (. (tt-ticks (tracker-player (module-read (tt-it '((0 0 60 1 26 0x40)))) 44100) 1) :find :chans)))
(assert-true "IT Z40, the cutoff 128" (and (= (. chan :find :f_cutoff) 128) (tt-near (. chan :find :fa0) 51874)))
(setq chan (first (. (tt-ticks (tracker-player (module-read (tt-it '((0 0 60 1 26 0x85)))) 44100) 1) :find :chans)))
(assert-true "IT Z85, the resonance 80" (and (= (. chan :find :f_res) 80) (tt-near (. chan :find :fa0) 1490513)))
(setq song (module-read (tt-it '((0 0 60 1 26 0x40)) 0 0 0 0x214 0x214 "F0F001z")))
(assert-eq "IT macro of its own, SF0 the resonance" "F0F001z" (first (first (. song :find :macros))))
(assert-eq "IT macro of its own, Z40 the resonance 128"
	128 (. (first (. (tt-ticks (tracker-player song 44100) 1) :find :chans)) :find :f_res))

;ModPlug Tracker 1.16's mix volume, turned down by its channels
(assert-eq "IT mix volume as it is" 48 (. (module-read (tt-it notes)) :find :mix))
(assert-eq "IT ModPlug 1.16, 16 channels, 48 by 96 over 160"
	28 (. (module-read (tt-it '((0 15 60 1 0 0)) 0 0 0 0x217 0x200)) :find :mix))

;the mixer's filter, a square of 8000 at cutoff 64, resonance 100, as
;libxmp's sums of it
(defq smp (apply (const cat) (map! (# (char (logand (if (< (% (!) 16) 8) 8000 -8000) 0xffff) 2)) '() 0 64))
	out (media-mix (str-alloc (* 4 8)) (fill (nums) 16 0) (list smp)
		(nums 0 0 65536 65536 65536 -1 64 0 9098 8310578 -4125373 0 0 0 -1 0 0)))
(assert-list-eq "media-mix filtered, as libxmp"
	'(17 51 102 170 253 352 465 593) (map! (# (code out 2 (* 4 (!)))) '() 0 8))

;the sample it is at, a MOD's, or the nearest, the others', at half speed
;round a loop of four
(defun tt-mix-round (round)
	; (tt-mix-round round) -> (left ...)
	(defq out (media-mix (str-alloc (* 4 8)) (fill (nums) 16 0)
		(list (apply (const cat) (map (# (char %0 2)) '(100 200 300 400))))
		(nums 0 0 32768 65536 65536 0 4 0 0 0 0 0 0 round -1 0 0)))
	(map! (# (code out 2 (* 4 (!)))) '() 0 8))
(assert-list-eq "media-mix the sample it is at" '(100 100 200 200 300 300 400 400) (tt-mix-round 0))
(assert-list-eq "media-mix the nearest sample" '(100 200 200 300 300 400 400 100) (tt-mix-round 0x8000))

;a ring, HivelyTracker's: a sample of 64 times a ring of 127 -128 64 0 at
;three steps a frame round the loop of four, a byte each, down 7
(defq out (media-mix (str-alloc (* 4 8)) (fill (nums) 16 0)
	(list (apply (const cat) (map (# (char (<< %0 8) 2)) '(64 64 64 64)))
		(apply (const cat) (map (# (char (logand (<< %0 8) 0xffff) 2)) '(127 -128 64 0))))
	(nums 0 0 65536 65536 65536 0 4 0 0 0 0 0 0 0 1 0 196608)))
(assert-list-eq "media-mix rung" '(16128 0 8192 -16384 16128 0 8192 -16384)
	(map! (# (defq v (code out 2 (* 4 (!)))) (if (> v 32767) (- v 65536) v)) '() 0 8))

; --- MED ---
(defun tt-be32 (n)
	; (tt-be32 n) -> str
	(tt-bytes (list (>> n 24) (>> n 16) (>> n 8) n)))

(defun tt-med (cells &optional rows tempo)
	; (tt-med cells [rows tempo]) -> str
	;an MMD1 of 4 tracks, one block of so many rows, one sample of 64 bytes
	;looped, its cells ((row track note ins cmd param) ...), speed 6
	(setd rows 4 tempo 125)
	(defq block (map! (# 0) '() 0 (* rows 16)) blocks 840 block_at 848 smpl_at (+ 860 (* rows 16)))
	(each (lambda ((row tr note ins cmd param))
			(defq at (* 4 (+ (* row 4) tr)))
			(elem-set block at note)
			(elem-set block (+ at 1) ins)
			(elem-set block (+ at 2) cmd)
			(elem-set block (+ at 3) param))
		cells)
	(cat "MMD1" (tt-be32 0) (tt-be32 52) (tt-zeros 4) (tt-be32 blocks) (tt-zeros 4) (tt-be32 (- smpl_at 4))
		(tt-zeros 4) (tt-be32 0) (tt-zeros 16)
		;the song: its one sample, looped and at 64, the rest none
		(tt-be16 0) (tt-be16 32) (tt-bytes '(0 0 64 0)) (tt-zeros (* 62 8))
		(tt-be16 1) (tt-be16 1) (tt-zeros 256) (tt-be16 tempo) (tt-bytes '(0 0 0 6))
		(tt-bytes (map! (# 64) '() 0 16)) (tt-bytes '(64 1))
		(tt-be32 block_at) (tt-zeros 4)
		(tt-be16 4) (tt-be16 (dec rows)) (tt-zeros 4) (tt-bytes block)
		(tt-be32 smpl_at) (tt-be32 64) (tt-be16 0) (tt-bytes (map! (# (if (< (!) 32) 64 -64)) '() 0 64))))

(defq song (module-read (tt-med '((0 0 13 1 0 0)))))
(assert-list-eq "MED read, its kind, mode and tracks" '("MMD1" :med 4) (list (. song :find :kind) (. song :find :mode) (. song :find :channels)))
(bind '(tt_s & tt_v &ignore) (first (. (first (. song :find :instruments)) :find :subs)))
(defq tt_smp (elem-get (. song :find :samples) tt_s))
(assert-list-eq "MED read, its sample looped, at 64" '(64 0 64 64)
	(list (. tt_smp :find :length) (. tt_smp :find :loop_start) (. tt_smp :find :loop_end) tt_v))
;a tick is the rate times 2.64 times 250 over the bpm and 1000, in doubles
;as libxmp has it, whole frames: 232 at 125, 882 at 33
(assert-eq "MED length, 4 rows of 6 ticks of 232 frames" 5568 (tracker-length song 44100))
(assert-eq "MED 0F21, a tempo of 33" (* 24 882) (tracker-length (module-read (tt-med '((0 0 0 0 15 0x21)))) 44100))
(assert-eq "MED 09, a speed of 3" (* 12 232) (tracker-length (module-read (tt-med '((0 0 0 0 9 3)))) 44100))
(assert-eq "MED F00 at row 1, the block's end" (* 12 232) (tracker-length (module-read (tt-med '((1 0 0 0 15 0)))) 44100))
(assert-eq "MED 1E02 at row 0, the row three times" (* 36 232) (tracker-length (module-read (tt-med '((0 0 0 0 0x1e 2)))) 44100))
(assert-eq "MED FFF a cut of the note" +note_cut (elem-get (first (. (module-read (tt-med '((1 0 0 0 15 255)))) :find :patterns)) 20))
(each (lambda ((what cells))
		(defq med_song (module-read (tt-med cells 8)))
		(assert-eq (cat "MED measured as played, " what) (tt-played med_song) (tracker-length med_song 44100)))
	'(("a note" ((0 0 13 1 0 0)))
		("a loop of a row twice" ((2 1 0 0 0x16 0) (3 1 0 0 0x16 2)))
		("a jump on to the end" ((1 2 0 0 11 0)))
		("a tempo and a delay" ((0 0 13 1 15 0x40) (2 1 13 1 15 0xf2) (4 3 0 0 0x1e 1)))))
(assert-list-eq "MED commands as libxmp has them: 0D a volume slide, 15FF finetune -1, 0F10 a BPM of 16, 1F with no note none"
	'((10 0x12) (17 0x70) (23 16) (-1 0))
	(list (med-xlat 13 13 0x12 song) (med-xlat 13 0x15 0xff song) (med-xlat 13 15 0x10 song) (med-xlat 0 0x1f 0x12 song)))
(defq player (tt-ticks (tracker-player (module-read (tt-med '((0 0 13 1 0 0) (0 1 25 1 0x0c 0x20)))) 44100) 1)
	chans (. player :find :chans))
(assert-list-eq "MED a note's volume, its sample's, and 0C20 its own"
	'(64 32) (list (. (first chans) :find :volume) (. (second chans) :find :volume)))
(assert-eq "MED C-1 an octave below C-2" (* 2 (. (second chans) :find :period)) (. (first chans) :find :period))

; --- AHX ---
(defun tt-ahx (steps)
	; (tt-ahx steps) -> str
	;an AHX of one position of 4 rows, voice 0 on track 1, the others on
	;track 0, none; one instrument, a saw; its steps ((row note ins fx param) ...)
	(defq trk (map! (# 0) '() 0 12))
	(each (lambda ((row note ins fx param))
			(defq at (* row 3))
			(elem-set trk at (logior (<< note 2) (>> ins 4)))
			(elem-set trk (inc at) (logior (<< (logand ins 15) 4) fx))
			(elem-set trk (+ at 2) param))
		steps)
	(cat "THX" (tt-bytes '(0 0 60 0x80 1 0 0 4 1 1 0 1 0 0 0 0 0 0 0)) (tt-bytes trk)
		(tt-bytes '(64 3 1 64 1 64 10 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0)) "t\x00i\x00"))

(defq song (module-read (tt-ahx '((0 25 1 0 0)))))
(assert-list-eq "AHX read, its kind, mode, voices and title" '("AHX" :ahx 4 "t")
	(list (. song :find :kind) (. song :find :mode) (. song :find :channels) (. song :find :title)))
(assert-eq "AHX read, its instrument's name" "i" (. (first (. song :find :samples)) :find :name))
;the lengths HivelyTracker's replayer gives these, a frame 882 at 50 a second
(assert-eq "AHX length, 4 rows of 6 frames" 21168 (tracker-length song 44100))
(assert-eq "AHX F03, a speed of 3" 10584 (tracker-length (module-read (tt-ahx '((0 0 0 15 3)))) 44100))
(assert-eq "AHX D00 at row 1, the position's end" 10584 (tracker-length (module-read (tt-ahx '((1 0 0 13 0)))) 44100))
(each (lambda ((what steps))
		(defq ahx_song (module-read (tt-ahx steps)))
		(assert-eq (cat "AHX measured as played, " what) (tt-played ahx_song) (tracker-length ahx_song 44100)))
	'(("a note" ((0 25 1 0 0))) ("a speed" ((0 25 1 15 3))) ("a break" ((1 0 0 13 0)))))
(defq chans (. (tt-ticks (tracker-player song 44100) 2) :find :chans))
(assert-list-eq "AHX the note's voice sounds, the others not" '(:t :nil)
	(list (> (. (first chans) :find :volume_out) 0) (> (. (second chans) :find :volume_out) 0)))

; --- HivelyTracker ---
(defun tt-hvl (channels stereo steps &optional line)
	; (tt-hvl channels stereo steps [line]) -> str
	;a HivelyTracker module of one position of 4 rows, voice 0 on track 1,
	;the others on track 0, none; one instrument, a saw, its list's line
	;(fx0 fx1 param0 param1), and its steps ((row note ins fx param fxb paramb) ...),
	;a row of none a byte
	(defq rows (map! (# '(0x3f)) '() 0 4))
	(each (lambda ((row note ins fx param fxb paramb))
			(elem-set rows row (list note ins (logior (<< fx 4) fxb) param paramb)))
		steps)
	(bind '(f0 f1 p0 p1) (ifn line '(0 0 0 0)))
	(defq trk (apply (const cat) rows)
		pos (cat '(1 0) (map! (# 0) '() 0 (* 2 (dec channels))))
		body (cat pos trk '(64 3 1 64 1 64 10 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (list f0 (logior (<< f1 3) 2) 0 p0 p1)))
	(cat "HVL" (tt-bytes (list 0 0 (+ 16 (length body)) 0x80 1 (<< (- channels 4) 2) 0 4 1 1 0 100 stereo))
		(tt-bytes body) "h\x00i\x00"))

(defq song (module-read (tt-hvl 6 4 '((0 25 1 0 0 0 0)))))
(assert-list-eq "HVL read, its kind, mode, voices and title" '("HivelyTracker" :ahx 6 "h")
	(list (. song :find :kind) (. song :find :mode) (. song :find :channels) (. song :find :title)))
(assert-eq "HVL read, a row of none a byte" 0 (elem-get (elem-get (. song :find :tracks) 1) 6))
(assert-list-eq "HVL its voices' places, left right right left" '(0 255 255 0 0 255) (. song :find :pans))
(assert-list-eq "HVL and of a stereo of 0, the middle" '(128 128 128 128)
	(. (module-read (tt-hvl 4 0 '((0 25 1 0 0 0 0)))) :find :pans))
(assert-eq "HVL its gain, 100 of 256" 256 (. song :find :gain))
(assert-eq "HVL length, 4 rows of 6 frames" 21168 (tracker-length song 44100))
(assert-eq "HVL F03 the second effect, a speed of 3" 10584
	(tracker-length (module-read (tt-hvl 6 4 '((0 25 1 0 0 15 3)))) 44100))
(each (lambda ((what steps))
		(defq hvl_song (module-read (tt-hvl 6 4 steps)))
		(assert-eq (cat "HVL measured as played, " what) (tt-played hvl_song) (tracker-length hvl_song 44100)))
	'(("a note" ((0 25 1 0 0 0 0))) ("a speed second" ((0 25 1 0 0 15 3))) ("a break" ((1 0 0 13 0 0 0)))))
(defq player (tt-ticks (tracker-player song 44100) 2) chans (. player :find :chans))
(assert-list-eq "HVL the note's voice sounds, the others not" '(:t :nil)
	(list (> (. (first chans) :find :volume_out) 0) (> (. (second chans) :find :volume_out) 0)))
(assert-eq "HVL no ring" -1 (elem-get (. player :find :voices) 14))
;a ring of a sawtooth a fifth up from the track's note, 8 and 0x87
(defq player (tt-ticks (tracker-player (module-read (tt-hvl 6 4 '((0 25 1 0 0 0 0)) '(8 0 0x87 0))) 44100) 2))
(assert-eq "HVL a ring, its buffer after the voices'" 6 (elem-get (. player :find :voices) 14))
(assert-true "HVL the ring's step, a fifth over the voice's, periods 906 and 604"
	(<= (abs (- (* 2 (elem-get (. player :find :voices) 16)) (* 3 (elem-get (. player :find :voices) 2)))) 2))

; --- the Jukebox's playlists ---
(defq m3u (cat "#EXTM3U\r\n"
	"http://ftp.example.org/mirrors/ftp.modland.com/pub/modules/Protracker/Someone/a%20song.mod\r\n"
	"http://ftp.example.org/mirrors/ftp.modland.com/pub/modules/Future%20Composer%201.4/Someone/b.fc\r\n"))
(assert-list-eq "playlist-read, those played here, at Modland"
	'(("a song.mod" "https://modland.com/pub/modules/Protracker/Someone/a%20song.mod"))
	(playlist-read m3u))
(assert-list-eq "playlist-read, a # escaped, it would end the address"
	'(("nordic report #3.mod" "https://modland.com/pub/modules/Protracker/Someone/nordic%20report%20%233.mod"))
	(playlist-read (cat "#EXTM3U\r\n"
		"http://ftp.example.org/mirrors/ftp.modland.com/pub/modules/Protracker/Someone/nordic%20report%20#3.mod\r\n")))

;a module a playlist names that Modland has renamed, found in its folder by
;the same name but for its ending
(defq listing (cat "<a href=\q../\q>Parent</a>\n<a href=\qsupernova.mod\q>supernova.mod</a>\n"
	"<a href=\qnearly%20there.mod\q>nearly there.mod</a>\n<a href=\qsupernova/\q>supernova/</a>\n"))
(assert-eq "entry-renamed, another ending" "supernova.mod" (entry-renamed listing "supernova.ft"))
(assert-eq "entry-renamed, dots before the ending" "nearly%20there.mod" (entry-renamed listing "nearly%20there...mod"))
(assert-eq "entry-renamed, none" :nil (entry-renamed listing "nothing.mod"))

;an XM whose instrument has no samples, and a note on it: the map of such
;an instrument is of no sample for each key, as libxmp has it, where it
;was empty and the note's look in it failed, sweet flavour.xm
(defq xm_bytes (cat "Extended Module: " (str-alloc 20) (char 0x1a) (str-alloc 20) (char 0x104 2)
	;its header: 276 bytes, a song of 1 order, 1 channel, 1 pattern and 1
	;instrument, linear, speed 6, tempo 125, its orders
	(char 276 4) (char 1 2) (char 0 2) (char 1 2) (char 1 2) (char 1 2) (char 1 2) (char 6 2)
	(char 125 2) (str-alloc 256)
	;a pattern of 1 row, its cell note 55 of instrument 1
	(char 9 4) (char 0) (char 1 2) (char 5 2) (char 55) (char 1) (char 0) (char 0) (char 0)
	;the instrument, its 29 bytes, no samples
	(char 29 4) (str-alloc 22) (char 0) (char 0 2)))
(defq xm_song (module-read xm_bytes))
(assert-eq "an XM instrument of no samples, a map of 96 keys" 96
	(length (. (first (. xm_song :find :instruments)) :find :map)))
(assert-true "a note on it, the song measured" (num? (tracker-length xm_song 44100)))
(defq listing (cat "<a href=\qmillenium%20powerup.xm\q>x</a>\n<a href=\qjazzfunk.xm\q>x</a>\n"
	"<a href=\qmill%20power.mod\q>x</a>\n<a href=\qmuddy%20pools.xm\q>x</a>\n<a href=\qmud%20pool.xm\q>x</a>\n"))
(assert-eq "entry-renamed, a DOS name of the name it has now" "millenium%20powerup.xm" (entry-renamed listing "millpwr.xm"))
(assert-eq "entry-renamed, not when two could be it" :nil (entry-renamed listing "mdpool.xm"))
(assert-eq "entry-renamed, not a long name" :nil (entry-renamed listing "jazzfunkier.xm"))
(defq listing (cat "<a href=\q20%20z%200i.xm\q>x</a>\n<a href=\qsid%20nostalgica.xm\q>x</a>\n"
	"<a href=\qnot%20stale.mod\q>x</a>\n"))
(assert-eq "entry-renamed, from a word of the name it has now" "sid%20nostalgica.xm" (entry-renamed listing "nostal.xm"))
(assert-eq "entry-renamed, not from inside a word" :nil (entry-renamed listing "ostal.xm"))
(defq listing (cat "<a href=\qbubble%20toast%20%28win%29.mod\q>x</a>\n<a href=\qeverything%27s%20good.xm\q>x</a>\n"
	"<a href=\qbubble%20toast%20%28title%29.mod\q>x</a>\n"))
(assert-eq "entry-renamed, marks added" "bubble%20toast%20%28win%29.mod" (entry-renamed listing "bubble%20toast%20win.mod"))
(assert-eq "entry-renamed, a mark taken" "everything%27s%20good.xm" (entry-renamed listing "everythings%20good.xm"))
(defq listing (cat "<a href=\qpolish%20xmas%20v2.mod\q>x</a>\n<a href=\qpolishing.mod\q>x</a>\n"
	"<a href=\qduck%20season%20v1.mod\q>x</a>\n<a href=\qduck%20season%20v2.mod\q>x</a>\n"))
(assert-eq "entry-renamed, a word after" "polish%20xmas%20v2.mod" (entry-renamed listing "polish%20xmas.mod"))
(assert-eq "entry-renamed, not when two have words after" :nil (entry-renamed listing "duck%20season.mod"))
(assert-eq "entry-renamed, a word after, not more of one" "polish%20xmas%20v2.mod" (entry-renamed listing "polish.mod"))
(defq listing (cat "<a href=\qrom-theme.mod\q>x</a>\n<a href=\qbrainfood-%232.mod\q>x</a>\n"
	"<a href=\qbrainfood-%233.mod\q>x</a>\n<a href=\qpep.mod\q>x</a>\n"))
(assert-eq "entry-renamed, letters taken out" "rom-theme.mod" (entry-renamed listing "rom-1-theme.mod"))
(assert-eq "entry-renamed, not one of letters of its own" :nil (entry-renamed listing "brainfood-%234.mod"))
(assert-eq "entry-renamed, not when two could be it" :nil (entry-renamed listing "brainfood-%2323.mod"))
(assert-eq "entry-renamed, not a short one of a long name" :nil (entry-renamed listing "pepsi%20party.mod"))
(assert-eq "coop-parent, the artist's own folder" "https://modland.com/pub/modules/Impulsetracker/Siren/"
	(coop-parent "https://modland.com/pub/modules/Impulsetracker/Siren/coop-Sirrus/"))
(assert-eq "coop-parent, none of the artist's own" :nil
	(coop-parent "https://modland.com/pub/modules/Impulsetracker/Siren/"))


;where Modland has a module now, by its list of all it has, allmods.txt: a
;size and a path a line, of trackers played here and not
(defq allmods (cat "100\tProtracker/Mel O'Dee/whales forever.mod\n"
	"100\tProtracker/Virgill/coop-Poldi/hard sun.mod\n100\tProtracker/Marc/funky dog.mod\n"
	"100\tProtracker/Spoon/coop-Jam/funky dog.mod\n100\tProtracker/Cool Col J/majestic.mod\n"
	"100\tProtracker/Projex/majestic.mod\n100\tFuture Composer 1.4/Someone/whales forever.fc\n")
	played (allmods-played allmods) index (allmods-index played))
(assert-eq "allmods-played, the trackers played here" 6 (length (split played "\n")))
(assert-eq "allmods-find, its artist's folder of another case" "Protracker/Mel O'Dee/whales forever.mod"
	(allmods-find index "Protracker/Mel o'Dee/whales forever.mod"))
(assert-eq "allmods-find, moved into a folder with another" "Protracker/Virgill/coop-Poldi/hard sun.mod"
	(allmods-find index "Protracker/Virgill/hard sun.mod"))
(assert-eq "allmods-find, of two the one whose folders share its words" "Protracker/Spoon/coop-Jam/funky dog.mod"
	(allmods-find index "Protracker/Jam & Spoon/funky dog.mod"))
(assert-eq "allmods-find, not when two could be it" :nil (allmods-find index "Protracker/TJM/majestic.mod"))
(assert-eq "allmods-find, none of that name" :nil (allmods-find index "Protracker/Anyone/nothing.mod"))
