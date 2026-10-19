(import "lib/options/options.inc")
(import "service/net/keys.inc")

(defq usage `(
(("-h" "--help")
"Usage: key [options] [name] [file]

    options:
        -h --help: this help info.
        -n --new: make a key of that name, 32 bytes at random.
        -u --use: use that key from now on: it is put in
            mesh_key, and the Net service ends its links and
            joins the machines that have it.
        -k --keep: keep the key in use by that name, one
            that was put in mesh_key by hand.
        -i --import: a key someone gave you, in a file of the
            host, into the keyring by that name.
        -x --export: say the key of that name, to give it to
            someone. Give it only to those you trust.
        -r --rename: the key of that name by the new name, the
            file given.
        -d --delete: forget the key of that name. If it is the
            one in use it stays in use.

    With no option, the keys this machine holds, each with its
    fingerprint, and the one in use. Two machines with the same
    fingerprint have the same key, and can join each other: say
    the fingerprint, never the key, to check.

    The keys are the files of keys/, the key in use mesh_key,
    the machine's own, never committed nor taken by a sync.
    A key is 64 hex digits, or any other text, a passphrase.")
(("-n" "--new") ,(opt-flag 'opt_n))
(("-u" "--use") ,(opt-flag 'opt_u))
(("-k" "--keep") ,(opt-flag 'opt_k))
(("-i" "--import") ,(opt-flag 'opt_i))
(("-x" "--export") ,(opt-flag 'opt_x))
(("-r" "--rename") ,(opt-flag 'opt_r))
(("-d" "--delete") ,(opt-flag 'opt_d))
))

(defun list-keys ()
	;the keyring, a line a key, and the key in use if it is not of it
	(defq names (keys-names) using (keys-in-use) key (door-key))
	(each (# (print (if (eql %0 using) "  * " "    ") %0 (pad "" (max 1 (- 16 (length %0))))
			(keys-print (door-key (keys-file %0))) (if (eql %0 using) "  in use" "")))
		names)
	(cond
		((and key (not using))
			(print "  * mesh_key" (pad "" 9) (keys-print key) "  in use, in no keyring file: key -k name keeps it"))
		((not key)
			(print "No key is in use. This machine is off the network: key -n name, then key -u name."))
		((empty? names))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_n :nil opt_u :nil opt_k :nil opt_i :nil opt_x :nil opt_r :nil opt_d :nil
				args (options stdio usage)))
		(defq name (if (> (length args) 1) (second args) "") file (if (> (length args) 2) (third args) ""))
		(cond
			((not (or opt_n opt_u opt_k opt_i opt_x opt_r opt_d)) (list-keys))
			((eql name "") (print "Name a key, key lists them."))
			((not (keys-name? name))
				(print "A key's name is letters, digits, - and _, 32 at most."))
			(opt_n
				(if (keys-new name)
					(print "Key " name " made, " (keys-print (door-key (keys-file name))) ". key -u " name " to use it.")
					(print "There is a key " name " already.")))
			(opt_k
				(if (keys-keep name)
					(print "The key in use is kept as " name ".")
					(print (if (pii-fstat +door_key_file) (cat "There is a key " name " already.") "No key is in use."))))
			(opt_i
				(if (keys-take name file)
					(print "Key " name " taken in, " (keys-print (door-key (keys-file name))) ".")
					(print "Not taken: " (if (pii-fstat (keys-file name)) (cat "there is a key " name " already.")
						(cat "no key in " file ".")))))
			((not (pii-fstat (keys-file name)))
				(print "No key " name ", key lists them."))
			(opt_u
				;one in use that is in no keyring file would be lost
				(cond
					((and (door-key) (not (keys-in-use)))
						(print "The key in use is in no keyring file, and would be lost: key -k name keeps it first."))
					((keys-use name)
						(print "Key " name " in use, " (keys-print (door-key)) ". The Net service joins the machines that have it."))
					((print "Key " name " is no key."))))
			(opt_x (prin (load (keys-file name))))
			(opt_r
				(cond
					((not (keys-name? file)) (print "The new name, letters, digits, - and _."))
					((keys-rename name file) (print "Key " name " is " file " now."))
					((print "There is a key " file " already."))))
			(opt_d
				(defq using (eql (keys-in-use) name))
				(keys-remove name)
				(print "Key " name " forgotten." (if using " It is still the one in use, mesh_key." ""))))))
