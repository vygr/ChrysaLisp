(report-header "Streams: mp4, a film of boxes read as a reel, its samples where the tables say")

(import "lib/media/mp4.inc")

;a box: its size, 4 bytes high first, its name, and what is in it
(defun m4-be (n bytes)
	(apply (const cat) (map (# (char (logand (>> n (* 8 (- bytes 1 %0))) 0xff))) (range 0 bytes))))
(defun m4-box (name &rest parts)
	(defq body (apply (const cat) (cat (list "") parts)))
	(cat (m4-be (+ 8 (length body)) 4) name body))
(defun m4-full (name &rest parts)
	;a full box, version 0 and no flags before what is in it
	(apply (const m4-box) (cat (list name (m4-be 0 4)) parts)))

;the samples, each its own bytes, seven of them
(defq m4_samples (map (# (apply (const cat) (map (lambda (_) (char (+ 65 %0))) (range 0 (+ 10 (* %0 7)))))) (range 0 7)))

(defun m4-moov (chunk_offsets wide &optional ctts)
	;a moov of one video track, 320 by 240, avc1, at 600 a second each
	;sample 20 of those, so 30 a second; samples 1 and 5 are keys; three
	;chunks of 3, 2 and 2 samples, at the offsets given
	(m4-box "moov"
		(m4-full "mvhd" (m4-be 0 8) (m4-be 600 4) (m4-be 140 4) (m4-be 0 80))
		(m4-box "trak"
			(m4-full "tkhd" (m4-be 0 76) (m4-be (<< 320 16) 4) (m4-be (<< 240 16) 4))
			(m4-box "mdia"
				(m4-full "mdhd" (m4-be 0 8) (m4-be 600 4) (m4-be 140 4) (m4-be 0 4))
				(m4-full "hdlr" (m4-be 0 4) "vide" (m4-be 0 12) "video" (char 0))
				(m4-box "minf"
					(m4-box "stbl"
						(m4-full "stsd" (m4-be 1 4)
							(m4-box "avc1" (m4-be 0 6) (m4-be 1 2) (m4-be 0 16) (m4-be 320 2) (m4-be 240 2)
								(m4-be 0x480000 4) (m4-be 0x480000 4) (m4-be 0 4) (m4-be 1 2) (m4-be 0 32)
								(m4-be 24 2) (m4-be 0xffff 2)
								(m4-box "avcC" "setup")))
						(m4-full "stts" (m4-be 1 4) (m4-be 7 4) (m4-be 20 4))
						(m4-full "stss" (m4-be 2 4) (m4-be 1 4) (m4-be 5 4))
						(if ctts ctts "")
						(m4-full "stsc" (m4-be 2 4) (m4-be 1 4) (m4-be 3 4) (m4-be 1 4) (m4-be 2 4) (m4-be 2 4) (m4-be 1 4))
						(m4-full "stsz" (m4-be 0 4) (m4-be 7 4)
							(apply (const cat) (map (# (m4-be (length %0) 4)) m4_samples)))
						(if wide
							(m4-full "co64" (m4-be (length chunk_offsets) 4)
								(apply (const cat) (map (# (m4-be %0 8)) chunk_offsets)))
							(m4-full "stco" (m4-be (length chunk_offsets) 4)
								(apply (const cat) (map (# (m4-be %0 4)) chunk_offsets))))))))))

(defun m4-file (moov_first wide)
	;ftyp, then moov and mdat in one order or the other. The chunks are
	;the samples 0 to 2, 3 and 4, and 5 and 6, in mdat, with a gap of junk
	;between them, so an offset that is out shows
	(defq ftyp (m4-box "ftyp" "isom" (m4-be 0 4) "isom")
		chunk_a (apply (const cat) (slice m4_samples 0 3)) chunk_b (cat (elem-get m4_samples 3) (elem-get m4_samples 4))
		chunk_c (cat (elem-get m4_samples 5) (elem-get m4_samples 6)) junk "#####"
		media (cat chunk_a junk chunk_b junk chunk_c)
		;mdat with a 64 bit size when wide, a size of 1 and 8 bytes more
		mdat_head (if wide (cat (m4-be 1 4) "mdat" (m4-be (+ 16 (length media)) 8)) (cat (m4-be (+ 8 (length media)) 4) "mdat"))
		size_moov (length (m4-moov '(0 0 0) wide))
		mdat_at (if moov_first (+ (length ftyp) size_moov) (length ftyp))
		media_at (+ mdat_at (length mdat_head))
		offsets (list media_at (+ media_at (length chunk_a) 5) (+ media_at (length chunk_a) 5 (length chunk_b) 5))
		moov (m4-moov offsets wide))
	(if moov_first (cat ftyp moov mdat_head media) (cat ftyp mdat_head media moov)))

(each (lambda ((moov_first wide what))
	(defq reel (mp4-reel (string-stream (m4-file moov_first wide))))
	(assert-true (cat "a reel of it, " what) (and reel (= (. reel :find :frames) 7)))
	(when reel
		(assert-list-eq (cat "its size and codec, " what) '(320 240 "avc1")
			(list (. reel :find :width) (. reel :find :height) (. reel :find :codec)))
		(assert-eq (cat "30 a second, from the times, " what) 30 (n2i (. reel :find :rate)))
		(assert-list-eq (cat "every packet its sample, from the chunk the tables say, " what) m4_samples
			(map (# (mp4-packet reel %0)) (range 0 7)))
		(assert-list-eq (cat "the keys, " what) '(0 4) (. reel :find :keys))
		(assert-eq (cat "its setup, the avcC, " what) "avcC" (first (first (. (. reel :find :video) :find :config))))))
	'((:nil :nil "moov after mdat, as a camera writes it") (:t :nil "moov first, as the web has it")
		(:nil :t "64 bit chunk offsets and an mdat with a 64 bit size")))

;B-frames: decoded I P B B P B B, every 20, and shown I B B P B B P. ctts
;says what to add to each time, in runs, version 0 unsigned, version 1
;signed, one less by 20, so some come before their time
(defun m4-ctts (version runs)
	(m4-box "ctts" (m4-be (<< version 24) 4) (m4-be (length runs) 4)
		(apply (const cat) (map (lambda ((count offset)) (cat (m4-be count 4) (m4-be (logand offset 0xffffffff) 4))) runs))))
(each (lambda ((version runs want))
	(defq offsets '(100 200 300) file (cat (m4-box "ftyp" "isom") (m4-moov offsets :nil (m4-ctts version runs)))
		reel (mp4-reel (string-stream file)))
	(assert-list-eq (cat "decoded every 20, ctts version " (str version)) '(0 20 40 60 80 100 120) (. reel :find :times))
	(assert-list-eq (cat "shown in another order, ctts version " (str version)) want (. reel :find :shown)))
	(list (list 0 '((1 20) (1 60) (2 0) (1 60) (2 0)) '(20 80 40 60 140 100 120))
		(list 1 '((1 0) (1 40) (2 -20) (1 40) (2 -20)) '(0 60 20 40 120 80 100))))
(assert-list-eq "with no ctts, shown as decoded" '(0 20 40 60 80 100 120)
	(. (mp4-reel (string-stream (m4-file :nil :nil))) :find :shown))

(assert-eq "a file with no moov is no reel" :nil (mp4-reel (string-stream (m4-box "ftyp" "isom"))))
(assert-true "an mp4 is a kind of reel" (some (# (eql (first %0) ".mp4")) *reel_kinds*))
