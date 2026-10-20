;the sound of films: the tables of an MP4 read, and the packets of its
;sound decoded, a time for each packet, each run from a new decoder.
;Files of the repo, the stream tests' half seconds
(import "lib/media/mp4.inc")
(import "lib/media/aac.inc")
(import "lib/media/ac3.inc")

(defun bm-track (file)
	;the sound track of a file, its tables read, and its packets
	(defq reel (mp4-reel (file-stream file)) stream (. reel :find :stream)
		track (some (# (if (eql (. %0 :find :kind) "soun") %0)) (. reel :find :tracks)))
	(mp4-tables track)
	(list track (map (lambda (at size) (stream-seek stream at 0) (read-blk stream size))
		(. track :find :offsets) (. track :find :sizes))))

(defun bm-tables (file)
	;the tables of a file's sound track, read afresh
	(defq reel (mp4-reel (file-stream file)))
	(each (# (if (eql (. %0 :find :kind) "soun") (mp4-tables %0))) (. reel :find :tracks)))

(bind '(bm_aac bm_aac_packets) (bm-track "tests/streams/data/tone.m4a"))
(bind '(bm_ac3 bm_ac3_packets) (bm-track "tests/streams/data/six_ac3.mp4"))
(bind '(bm_eac3 bm_eac3_packets) (bm-track "tests/streams/data/six_eac3.mp4"))
(bind '(bm_short bm_short_packets) (bm-track "tests/streams/data/short_ac3.mp4"))

(bench "MP4 reel and sound tables of the AC-3 half second, 10 times, a packet" (* 10 (length bm_ac3_packets))
	(times 10 (bm-tables "tests/streams/data/six_ac3.mp4")))
(bench "AAC-LC stereo, a packet of 1024" (length bm_aac_packets)
	(defq d (aac-decoder (aac-track-config bm_aac)))
	(each (# (aac-decode d %0)) bm_aac_packets))
(bench "AC-3 5.1, a frame of 1536" (length bm_ac3_packets)
	(defq d (ac3-decoder (ac3-header (bits-of (first bm_ac3_packets)))))
	(each (# (ac3-frame d %0)) bm_ac3_packets))
(bench "E-AC-3 5.1, a frame of 1536" (length bm_eac3_packets)
	(defq d (ac3-decoder (ac3-header (bits-of (first bm_eac3_packets)))))
	(each (# (ac3-frame d %0)) bm_eac3_packets))
(bench "AC-3 stereo with short blocks, a frame of 1536" (length bm_short_packets)
	(defq d (ac3-decoder (ac3-header (bits-of (first bm_short_packets)))))
	(each (# (ac3-frame d %0)) bm_short_packets))
(bench "AC-3 5.1 made stereo, a frame" (length bm_ac3_packets)
	(defq d (ac3-decoder (ac3-header (bits-of (first bm_ac3_packets)))))
	(each (# (ac3-stereo d (ac3-frame d %0))) bm_ac3_packets))
