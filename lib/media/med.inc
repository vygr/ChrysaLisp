;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a module of MED and OctaMED, the Amiga's, MMD0 to MMD3 and MMDC, read
; into a song as lib/media/mod.inc has them, which lib/media/tracker.inc
; plays as libxmp 4.7 does, the med- functions. Blocks of their own
; length and up to 64 tracks, a cell of a note, an instrument and one
; command and its number, as MED has them: a cell's volume is +no_volume.
; Its instruments: a sample, a sample of up to seven octaves, each of
; twice the length of the one before, a synth sound of short waveforms
; played by a table, or a hybrid, a sample played by one. Each has the
; subs it plays, (sample transpose volume finetune), and a map of which
; sub each of 121 keys plays and moved how far, (sub transpose). The
; tables of a synth and its speeds, and how long a note holds and how
; fast it decays after, are the instrument's
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/media/mod.inc")

(defq
	;the trackers whose rules a song is played by, as libxmp tells them
	;apart by what the file has
	+med_ver_300 0x0300 +med_ver_320 0x0320 +med_ver_octamed_300 0x3000
	+med_ver_octamed_500 0x5000 +med_ver_ss_1 0x6000 +med_ver_ss_2 0x7000
	;the octaves of a sample of several, by its type, 1 to 6
	+med_octaves ''(5 3 2 4 6 7)
	;which octave's sample each octave of keys plays, and moved how far,
	;by how many octaves the sample has, 2 to 7
	+med_oct_sub ''((1 1 1 0 0 0 0 0 0) (2 2 2 2 2 2 1 1 0) (3 3 3 2 2 2 1 1 0)
		(4 4 4 3 2 2 1 1 0) (5 5 5 5 4 3 2 1 0) (6 6 6 6 5 4 3 2 1))
	+med_oct_xpo ''((12 12 12 0 0 0 0 0 0) (12 12 12 12 12 12 0 0 -12) (12 12 12 0 0 0 -12 -12 -24)
		(24 24 24 12 0 0 -12 -24 -36) (12 12 12 12 0 -12 -24 -36 -48) (12 12 12 12 0 -12 -24 -36 -48)))

(defun med-u32 (data at)
	; (med-u32 data at) -> num
	;four bytes high first, 0 past the file's end
	(if (<= (+ at 4) (length data)) (code-be data 4 at) 0))

(defun med-u16 (data at)
	; (med-u16 data at) -> num
	(if (<= (+ at 2) (length data)) (code-be data 2 at) 0))

(defun med-u8 (data at)
	; (med-u8 data at) -> num
	(if (< at (length data)) (code data 1 at) 0))

(defun med-tempo (tempo bpm_on med_8ch)
	; (med-tempo tempo bpm_on med_8ch) -> bpm
	;a tempo as libxmp has it: of an 8 channel song 1 to 10 by a table,
	;and 1 to 10 when not in BPM mode the Soundtracker's speeds
	(cond
		((<= tempo 0) tempo)
		(med_8ch (elem-get '(179 164 152 141 131 123 116 110 104 99) (dec (min 10 tempo))))
		((and (<= tempo 10) (not bpm_on)) (elem-get '(195 97 65 49 39 32 28 24 22 20) (dec tempo)))
		(tempo)))

(defun med-version (mmd mmdc med_8ch exp_size song_name)
	; (med-version mmd mmdc med_8ch exp_size song_name) -> version
	;the tracker a song is likely of, as libxmp guesses it, exp_size -1 if
	;it has no expansion data
	(cond
		((> exp_size 18) +med_ver_ss_2)
		((>= mmd 3) +med_ver_ss_1)
		((> exp_size 10) +med_ver_ss_1)
		((> exp_size 4) +med_ver_octamed_500)
		((>= mmd 2) +med_ver_octamed_500)
		((> song_name 0) +med_ver_octamed_300)
		((>= mmd 1) +med_ver_octamed_300)
		((> exp_size 2) (if med_8ch 0x2000 +med_ver_320))
		((>= exp_size 0) (if med_8ch 0x1000 +med_ver_300))
		(0x0210)))

(defun med-wave (data at n kind)
	; (med-wave data at n kind) -> str
	;so many frames of a sample's bytes at at, as the mixer has them:
	;8 bit, or 16 bit high byte first, of one channel, or two one after
	;the other made one, kind :8 :16 :8s :16s
	(defq n (max 0 n))
	(case kind
		(:8 (mod-widen data at n +widen_s8 :nil))
		(:16 (apply (const cat) (map! (lambda ()
				(defq p (+ at (* 2 (!))))
				(cat (slice data (inc p) (+ p 2)) (slice data p (inc p))))
			'() 0 n)))
		(:t ;the left and the right of a stereo sample, the two halved
			(defq left (med-wave data at n (if (eql kind :8s) :8 :16))
				right (med-wave data (+ at (if (eql kind :8s) n (* 2 n))) n (if (eql kind :8s) :8 :16)))
			(apply (const cat) (map! (lambda ()
					(defq i (* 2 (!)))
					(char (>> (+ (get-short left i) (get-short right i)) 1) 2))
				'() 0 n)))))

(defun med-sample (data at n loop_start loop_len looped pingpong bare kind)
	; (med-sample data at n loop_start loop_len looped pingpong bare kind) -> sample
	;a sample of so many frames, its loop as libxmp makes it good: past
	;the end cut back, none if it starts past where it ends
	(defq n (mod-frames data at n (* (if (find kind '(:16 :16s)) 2 1) (if (find kind '(:8s :16s)) 2 1)))
		ls (max 0 loop_start) le (min n (+ loop_start loop_len)))
	(if (or (not looped) (>= ls n) (>= ls le)) (setq ls -1 le n))
	(scatter (Emap) :name "" :length n :loop_start ls :loop_end le :pingpong (and (>= ls 0) pingpong)
		:data (if (or bare (= n 0)) "" (med-wave data at n kind))))

(defun med-map (xpo_of)
	; (med-map xpo_of) -> map
	;the 121 keys each of the first sub, moved (xpo_of key)
	(map! (# (list 0 (xpo_of (!)))) '() 0 121))

(defun med-instrument (data at hdr ext ver samples bare)
	; (med-instrument data at hdr exp ver samples bare) -> instrument
	;the instrument at, of the song's sample header hdr, (rep replen svol
	;strans), and of its expansion's, (hold decay finetune pitch flags
	;long_rep long_replen) or :nil each, its samples pushed on samples
	(bind '(rep replen svol strans) hdr)
	(bind '(hold decay fine pitch flags long_rep long_len) ext)
	(defq len (med-u32 data at) stype (sign-extend (med-u16 data (+ at 4)) 16)
		kind (logand stype (lognot 0x38)) fin (<< (sign-extend fine 8) 4)
		;the flags of an OctaMED 5 instrument, a loop or not by its length
		;before
		looped (if (and (>= ver 2) flags) (bits? flags 1) (> replen 1))
		pingpong (and (>= ver 2) flags (bits? flags 8)) enable (not (and (>= ver 2) flags (bits? flags 4)))
		sampletrans (+ 36 strans) synthtrans (+ 12 strans)
		ins (scatter (Emap) :subs (list) :map :nil :hold 0 :decay 0 :synth :nil :vts 0 :wts 0 :nsm 0))
	;a sample of mix mode is two octaves down, not those of several
	(if (or (and (>= ver 3) (= kind 0)) (= kind 7)) (setq sampletrans (- sampletrans 24)))
	(defq lrep (if (and (>= ver 3) long_rep) long_rep (<< rep 1))
		llen (if (and (>= ver 3) long_rep) long_len (<< replen 1))
		bits16 (bits? stype 0x10) stereo (bits? stype 0x20) frames (if bits16 (>> len 1) len)
		skind (cond ((and bits16 stereo) :16s) (bits16 :16) (stereo :8s) (:8))
		;mix mode plays key 12, note 1, as its instrument's default pitch
		def_note (if (and pitch (> pitch 0)) (dec pitch) 53))
	(cond
		((or (= stype -1) (= stype -2))
			;a synth sound, or a hybrid: its tables, and its waveforms
			;its fields from its start, its waveforms by where they are from it
			(defq wforms (med-u16 data (+ at 20)))
			(when (and (/= wforms 0xffff) (<= 1 wforms 64))
				(defq vtlen (min 128 (med-u16 data (+ at 14))) wtlen (min 128 (med-u16 data (+ at 16)))
					offs (map! (# (med-u32 data (+ at 278 (* 4 (!))))) '() 0 wforms))
				(scatter ins :hold -1 :decay -1 :vts (med-u8 data (+ at 18)) :wts (med-u8 data (+ at 19)) :nsm wforms
					:synth (list (slice data (+ at 22) (+ at 22 vtlen)) (slice data (+ at 150) (+ at 150 wtlen))))
				(each (lambda (off)
						(defq w (+ at off))
						(cond
							((and (= (!) 0) (= stype -2))
								;a hybrid's first is a sample
								(push samples (med-sample data (+ w 6) (med-u32 data w) lrep llen looped :nil bare :8))
								(push (. ins :find :subs) (list (dec (length samples)) sampletrans (if enable svol 0) fin)))
							(:t (defq n (* 2 (med-u16 data w)))
								(push samples (med-sample data (+ w 2) n 0 n :t :nil bare :8))
								(push (. ins :find :subs) (list (dec (length samples)) synthtrans (if enable 64 0) fin)))))
					offs)
				(. ins :insert :map (med-map (# (if (and (>= ver 3) (= %0 12)) def_note 0))))))
		((<= 1 stype 6)
			;a sample of several octaves, each twice the one before
			(defq oct (elem-get +med_octaves (dec stype)) size (/ len (dec (<< 1 oct)))
				p (+ at 6) osub (elem-get +med_oct_sub (- oct 2)) oxpo (elem-get +med_oct_xpo (- oct 2)))
			(times oct
				(push samples (med-sample data p size lrep llen looped pingpong bare :8))
				(push (. ins :find :subs) (list (dec (length samples)) (- sampletrans 12) (if enable svol 0) fin))
				(setq p (+ p size) size (<< size 1) lrep (<< lrep 1) llen (<< llen 1)))
			(scatter ins :hold hold :decay decay :nsm oct
				:map (map! (# (list (elem-get osub (min 8 (/ (!) 12))) (elem-get oxpo (min 8 (/ (!) 12))))) '() 0 121)))
		((or (= kind 0) (= kind 7))
			(push samples (med-sample data (+ at 6) frames lrep llen looped pingpong bare skind))
			(push (. ins :find :subs) (list (dec (length samples)) sampletrans (if enable svol 0) fin))
			;before mix mode a sample plays three octaves, five of an
			;ExtSample, those above and below played an octave in
			(defq octaves (if (= kind 7) 5 3))
			(scatter ins :hold hold :decay decay :nsm 1
				:map (med-map (lambda (key)
					(defq j (/ key 12))
					(cond
						((>= ver 3) (if (= key 12) def_note 0))
						((< j 1) (* 12 (- 1 j)))
						((> j octaves) (* -12 (- j octaves)))
						(0)))))))
	ins)

(defun med-cells (data at tracks rows chans mmd mmdc transpose)
	; (med-cells data at tracks rows chans mmd mmdc transpose) -> nums
	;the cells of a block, of so many tracks, as the song's channels, the
	;tracks it has not empty
	(defq size (* tracks rows (if (> mmd 0) 4 3)) cells (nums) src data from at)
	(when mmdc
		;a block of MMDC packed: runs of 0, and bytes as they are
		(defq out (list) j 0 p at)
		(while (and (< j size) (< p (length data)))
			(defq pack (code data 1 p))
			(setq p (inc p))
			(cond
				((bits? pack 0x80) (push out (pad "" (min (- size j) (- 256 pack)) "\x00"))
					(setq j (+ j (- 256 pack))))
				(:t (defq n (min (inc pack) (- size j)))
					(push out (slice data p (min (length data) (+ p n))))
					(setq p (+ p n) j (+ j n)))))
		(setq src (apply (const cat) out) from 0))
	(defq step (if (> mmd 0) 4 3))
	(each! (lambda ()
			(defq row (!))
			(each! (lambda ()
					(defq k (!))
					(if (< k tracks)
						(progn
							(defq b (+ from (* (+ (* row tracks) k) step)) b0 (med-u8 src b) b1 (med-u8 src (inc b))
								note (if (> mmd 0) (logand b0 0x7f) (logand b0 0x3f))
								ins (if (> mmd 0) (logand b1 0x3f) (logior (>> b1 4) (>> (logand b0 0x80) 3) (>> (logand b0 0x40) 1)))
								cmd (if (> mmd 0) (med-u8 src (+ b 2)) (logand b1 15))
								param (med-u8 src (+ b (if (> mmd 0) 3 2))))
							(when (> note 0)
								(setq note (+ note 12 transpose))
								(if (or (< note 0) (>= note 121)) (setq note 0)))
							;FFF a cut of the note
							(and (= cmd 15) (= param 255) (setq note +note_cut cmd 0 param 0))
							(push cells note ins +no_volume cmd param))
						(push cells 0 0 +no_volume 0 0)))
				'() 0 chans))
		'() 0 rows)
	cells)

(defun med-read (data &optional bare)
	; (med-read data [bare]) -> :nil | song
	;the song of a MED module's bytes, :nil if they are not one; bare, its
	;samples' bytes not had, for what only shows it
	(when (and (> (length data) 52) (find (slice data 0 4) '("MMD0" "MMD1" "MMD2" "MMD3" "MMDC")))
		(defq id (elem-get data 3) mmdc (eql id "C") mmd (if mmdc 0 (- (code id) (ascii-code "0")))
			song (med-u32 data 8) blockarr (med-u32 data 16) smplarr (med-u32 data 24) expdata (med-u32 data 32)
			numblocks (med-u16 data (+ song 504)) songlen (med-u16 data (+ song 506))
			deftempo (med-u16 data (+ song 764)) transpose (get-byte data (+ song 766))
			flags (med-u8 data (+ song 767)) flags2 (med-u8 data (+ song 768)) tempo2 (med-u8 data (+ song 769))
			numsamples (med-u8 data (+ song 787))
			med_8ch (bits? flags 0x40) bpm_on (bits? flags2 0x20) bpmlen (inc (logand flags2 0x1f)))
		(when (and (<= numblocks 255) (<= numsamples 63) (< (+ song 788) (length data)))
			;the order the blocks play in: MMD2 and on, the first play
			;sequence of its table
			(defq orders (cond
					((< mmd 2) (map! (# (code data 1 (+ song 508 (!)))) '() 0 (min songlen 256)))
					(:t (defq ps (med-u32 data (med-u32 data (+ song 508))) n (min 255 (med-u16 data (+ ps 40))))
						(map! (# (med-u16 data (+ ps 42 (* 2 (!))))) '() 0 n))))
			;the expansion: the instruments' holds, decays and finetunes, and
			;the song's name
			(defq exp_size -1 exps (list) title "" name_len 0 names (list))
			(when (> expdata 0)
				(defq esmp (med-u32 data (+ expdata 4)) entries (med-u16 data (+ expdata 8))
					exp_size (med-u16 data (+ expdata 10)) name_at (med-u32 data (+ expdata 44))
					name_len (med-u32 data (+ expdata 48)))
				(and (> name_at 0) (> name_len 0) (setq title (mod-text data name_at (min name_len 64))))
				;the instruments' names, 40 bytes each at least
				(defq iinfo (med-u32 data (+ expdata 20)) isize (med-u16 data (+ expdata 26)))
				(and (> iinfo 0) (>= isize 40)
					(setq names (map! (# (mod-text data (+ iinfo (* (!) isize)) 40))
						'() 0 (min numsamples (med-u16 data (+ expdata 24))))))
				(when (> esmp 0)
					(setq exps (map! (lambda ()
							(defq e (+ esmp (* (!) exp_size)))
							(list (if (>= exp_size 2) (med-u8 data e) 0) (if (>= exp_size 2) (med-u8 data (inc e)) 0)
								(if (>= exp_size 4) (med-u8 data (+ e 3)) 0)
								(and (>= mmd 2) (>= exp_size 8) (med-u8 data (+ e 4)))
								(and (>= mmd 2) (>= exp_size 8) (med-u8 data (+ e 5)))
								(and (>= mmd 2) (>= exp_size 18) (med-u32 data (+ e 10)))
								(and (>= mmd 2) (>= exp_size 18) (med-u32 data (+ e 14)))))
						'() 0 (min numsamples entries)))))
			;the blocks: how many tracks the song has is the most of any
			(defq heads (map! (lambda ()
					(defq b (med-u32 data (+ blockarr (* 4 (!)))))
					(cond
						((= b 0) (list 0 0 0))
						((> mmd 0) (list b (med-u16 data b) (inc (med-u16 data (+ b 2)))))
						((list b (med-u8 data b) (inc (med-u8 data (inc b)))))))
				'() 0 numblocks)
				chans (reduce (# (max %0 (second %1))) heads 0))
			(when (<= 1 chans (if (> mmd 1) 64 16))
				(defq version (med-version mmd mmdc med_8ch exp_size name_len) samples (list)
					instruments (map! (lambda ()
							(defq i (!) at (med-u32 data (+ smplarr (* 4 i))) h (+ song (* 8 i))
								hdr (list (med-u16 data h) (med-u16 data (+ h 2)) (med-u8 data (+ h 6)) (get-byte data (+ h 7)))
								ext (if (< i (length exps)) (elem-get exps i) '(0 0 0 :nil :nil :nil :nil)))
							(if (> at 0) (med-instrument data at hdr ext mmd samples bare)
								(scatter (Emap) :subs (list) :map :nil :nsm 0)))
						'() 0 numsamples)
					;an instrument's name is its first sample's, the one shown
					_ (each (lambda (ins)
							(and (< (!) (length names)) (nempty? (defq subs (. ins :find :subs)))
								(. (elem-get samples (first (first subs))) :insert :name (elem-get names (!)))))
						instruments)
					pans (cond
						((< mmd 2) (mod-pans chans 0 255))
						(:t (defq tp (med-u32 data (+ song 524)))
							(map! (# (if (> tp 0) (+ 128 (min 127 (* 8 (get-byte data (+ tp (!)))))) 128)) '() 0 chans)))
					chvols (cond
						((< mmd 2) (map! (# (med-u8 data (+ song 770 (!)))) '() 0 chans))
						(:t (defq tv (med-u32 data (+ song 516)))
							(map! (# (med-u8 data (+ tv (!)))) '() 0 chans))))
				(scatter (Emap) :kind (cat "MMD" (if mmdc "C" (str mmd))) :mode :med :pitch :amiga :title title
					:channels chans :samples samples :instruments instruments :orders orders :restart 0
					:patterns (map (lambda (head)
							(bind '(b tracks rows) head)
							(if (= b 0) (mod-cells 64 chans)
								(med-cells data (+ b (if (> mmd 0) 8 2)) tracks rows chans mmd mmdc transpose)))
						heads)
					:rows (map (# (if (= (first %0) 0) 64 (third %0))) heads)
					:speed (max 1 tempo2) :tempo (max 1 (med-tempo deftempo bpm_on med_8ch)) :global 64
					:pans pans :chvols chvols :version version :slides_all (not (bits? flags 0x20))
					:bpm_on bpm_on :med_8ch med_8ch
					;a tick is 0.66 / bpm of a second, as libxmp has MED's, a
					;whole song of 8 channels 2.5, and in BPM mode by its beat
					:time_factor (cond (med_8ch '(10 1)) (bpm_on (list 40 bpmlen)) ('(264 100))))))))
