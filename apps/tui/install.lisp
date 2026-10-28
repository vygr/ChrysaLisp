(import "lib/task/pipe.inc")
(print)
(print "ChrysaLisp Installer v0.1")
(print "Building platform native boot image and apps.")
(print "Please wait...")
(print)
(import "lib/boot/id.inc")
;a build that went wrong is said, and leaves obj/install_failed, as the
;host program's exit says nothing: rack.sh looks for it, to put back
;what was there. Wrong is an error said, or no native boot image whose id
;is of the files it says it was made from
(pii-remove "obj/install_failed")
;the apps too, their VP made here and not as each is first run
(defq errors 0)
(each (# (pipe-run %0
		(lambda (line) (prin line) (stream-flush (io-stream "stdout"))
			(if (found? line "Error") (++ errors)))))
	'("make -v 1 all boot | time -s" "make apps | time -s"))
(print)
(defq files (load (boot-files-file (cpu) (abi)))
	built (and files (eql (trim (ifn (load (boot-id-file (cpu) (abi))) ""))
		(boot-id-of (filter (const nempty?) (split files "\n"))))))
(when (or (> errors 0) (not built))
	(save (if built "errors" "no image") "obj/install_failed")
	(print "Install failed: " (if (> errors 0) (cat (str errors) " errors in the build") "no native boot image of this source"))
	(print)
	(pii-exit))
;a machine has a key from the day it is installed, and one it has is left
;as it is. Only machines with the same key join each other, so a new one
;is by itself till it is given the key of the others, the file mesh_key
(unless (pii-fstat "mesh_key")
	(import "service/net/keys.inc")
	(unless (pii-fstat (keys-file "own")) (keys-new "own"))
	(keys-use "own")
	(print "This machine has been given a key of its own, named own, in keys/ and in use.")
	(print "To join it to your other machines take in their key: key -i name file, then key -u name.")
	(print))
;the tree's waypoint noted, if it is exactly a commit, as after a pull:
;so a pull and an install is all it takes. Then what was built is of that
;waypoint: a system at the same one has the same, and @Source asks it
;nothing, service/source/app.inc (source-waypoint). A make that builds
;takes it away
(pipe-run "waypoint" (lambda (line) (prin line) (stream-flush (io-stream "stdout"))))
(import "lib/sync/sync.inc")
(if (defq at (sync-clean "." (sync-kept "."))) (save (first at) "obj/installed"))
(print "Install complete.")
(print)
(print "If on MacOS or Linux you may now run:")
(print "TUI with './run_tui.sh'")
(print "GUI with './run.sh'")
(print "stop the network with './stop.sh'")
(print)
(print "If on Windows you may now run:")
(print "TUI with './run_tui.bat'")
(print "GUI with './run.bat'")
(print "stop the network with './stop.bat'")
(print)
(pii-exit)
