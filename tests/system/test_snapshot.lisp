(report-header "Snapshot: the Windows host programs in snapshot.zip are of this host source")
(import "lib/files/files.inc")
(import "lib/crypto/sha256.inc")

;Windows does not build its host programs, a Windows machine has them from
;snapshot.zip, by git or a follower's pull, so a change to src/host goes
;with a new snapshot, made with the Windows hosts by make snapshot. That
;writes the hash of the source they were made from, snapshot_host: a line
;a file of src/host in order, its path and its sha256, and the sha256 of
;all of that. A change to src/host with no new snapshot fails here
(defq sn_files (sort (filter (# (not (starts-with "." (last (split %0 "/"))))) (files-all "src/host")))
	sn_lines (apply (const cat) (map (# (cat %0 " " (to-lower (hex-encode (sha256 (load %0)))) (ascii-char 10))) sn_files))
	sn_now (to-lower (hex-encode (sha256 sn_lines)))
	sn_made (trim (ifn (load "snapshot_host") "") (const (char-class " \t\r\n"))))
(assert-true "there are host sources" (> (length sn_files) 10))
(assert-eq "snapshot.zip was made from src/host as it is, make -f Makefile.mingw windows_all and make snapshot after a host change"
	sn_made sn_now)
(undef (env) 'sn_files 'sn_lines 'sn_now 'sn_made)
