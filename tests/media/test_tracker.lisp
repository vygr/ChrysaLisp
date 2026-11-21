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

; --- MOD ---
(defq song (module-read (tt-mod '())))
(assert-eq "MOD read, its title" "test" (. song :find :title))
(assert-eq "MOD read, its channels" 4 (. song :find :channels))
(assert-eq "MOD read, its sample" 32 (. (first (. song :find :samples)) :find :length))
(assert-eq "MOD length, 64 rows of 6 ticks of 882 frames" 338688 (tracker-length song 44100))
(assert-eq "MOD length, F03 speed 3" 169344 (tracker-length (module-read (tt-mod '((0 0 0 0 15 3)))) 44100))
(assert-eq "MOD length, D00 at row 15" 84672 (tracker-length (module-read (tt-mod '((15 0 0 0 13 0)))) 44100))
(assert-eq "MOD A01, a row's 5 slides down"
	59 (. (first (. (tt-ticks (tracker-player (module-read (tt-mod '((0 0 428 1 10 1)))) 44100) 6)
		:find :chans)) :find :volume))

; --- IT ---
(defq notes '((0 0 60 1 0 0) (1 0 62 1 0 0) (2 0 64 1 0 0)))
(setq song (module-read (tt-it notes)))
(assert-eq "IT read, its mode" :it (. song :find :mode))
(assert-eq "IT length, 4 rows of 6 ticks" 21168 (tracker-length song 44100))
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
		(nums 0 0 65536 65536 65536 -1 64 0 9098 8310578 -4125373 0 0 0)))
(assert-list-eq "media-mix filtered, as libxmp"
	'(17 51 102 170 253 352 465 593) (map! (# (code out 2 (* 4 (!)))) '() 0 8))

;the sample it is at, a MOD's, or the nearest, the others', at half speed
;round a loop of four
(defun tt-mix-round (round)
	; (tt-mix-round round) -> (left ...)
	(defq out (media-mix (str-alloc (* 4 8)) (fill (nums) 16 0)
		(list (apply (const cat) (map (# (char %0 2)) '(100 200 300 400))))
		(nums 0 0 32768 65536 65536 0 4 0 0 0 0 0 0 round)))
	(map! (# (code out 2 (* 4 (!)))) '() 0 8))
(assert-list-eq "media-mix the sample it is at" '(100 100 200 200 300 300 400 400) (tt-mix-round 0))
(assert-list-eq "media-mix the nearest sample" '(100 200 200 300 300 400 400 100) (tt-mix-round 0x8000))

; --- the Jukebox's playlists ---
(defq m3u (cat "#EXTM3U\r\n"
	"http://ftp.example.org/mirrors/ftp.modland.com/pub/modules/Protracker/Someone/a%20song.mod\r\n"
	"http://ftp.example.org/mirrors/ftp.modland.com/pub/modules/AHX/Someone/b.ahx\r\n"))
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
