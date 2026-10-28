;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The keyring of a machine, the mesh keys it holds, each by a name.
;
; A key is a file of the folder keys/ at the root of the tree, its name
; the file's: keys/rig, keys/wan, a café's. The one in use is the file
; mesh_key, which the Net service watches and joins by,
; service/net/door.inc; to use another is to copy it there. keys/ and
; mesh_key are the machine's own, never committed nor taken by a sync.
;
; A key's fingerprint is what two people say to each other to know they
; hold the same one, without saying the key: four groups of four hex
; digits of a hash of it, of the 32 bytes it is, whether it was written
; as 64 hex digits or as a passphrase.
;
; Each takes the root of the tree it is of last, this one's if not given,
; so a test can have a keyring of its own.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./door.inc")

(defq +keys_folder "keys" +keys_mode 384)

(defun keys-file (name &optional root)
	; (keys-file name [root]) -> path
	(cat (ifn root ".") "/" +keys_folder "/" name))

(defun keys-mesh (&optional root)
	; (keys-mesh [root]) -> path
	;the file of the key in use
	(cat (ifn root ".") "/" +door_key_file))

(defun keys-name? (name)
	; (keys-name? name) -> :nil | :t
	;a name a key may have: letters, digits, - and _, as a file of any host
	(and (str? name) (<= 1 (length name) 32)
		(every (# (find %0 "-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz")) name)
		:t))

(defun keys-names (&optional root)
	; (keys-names [root]) -> (name ...)
	;the keys of the keyring, in order
	(sort (reduce (lambda (out (name kind))
			(and (eql kind "f") (keys-name? name) (push out name))
			out)
		(partition (split (ifn (pii-dirlist (cat (ifn root ".") "/" +keys_folder)) "") ",") 2) (list))))

(defun keys-print (key)
	; (keys-print key) -> str
	;the fingerprint of a key, its 32 bytes
	(defq hex (slice (hex-encode (sha256 (cat "ChrysaLisp key fingerprint " key))) 0 16))
	(join (map (# (slice hex %0 (+ %0 4))) (range 0 16 4)) "-"))

(defun keys-in-use (&optional root)
	; (keys-in-use [root]) -> :nil | name
	;the name of the key in use, mesh_key, if it is one of the keyring
	(when (defq key (door-key (keys-mesh root)))
		(some (# (if (eql (door-key (keys-file %0 root)) key) %0)) (keys-names root))))

(defun keys-save (text path)
	; (keys-save text path)
	;a key written, for this user alone to read
	(save text path)
	(pii-chmod path +keys_mode))

(defun keys-new (name &optional root)
	; (keys-new name [root]) -> :nil | :t
	;a new key made, 32 bytes at random, 64 hex digits. :nil if there is
	;one of that name
	(unless (pii-fstat (keys-file name root))
		(keys-save (cat (to-lower (hex-encode (random-bytes 32))) "\n") (keys-file name root))
		:t))

(defun keys-use (name &optional root)
	; (keys-use name [root]) -> :nil | :t
	;that key in use from now on, the Net service ends the links it has
	;and joins those with it. :nil if there is no such key
	(when (and (defq text (load (keys-file name root))) (door-key (keys-file name root)))
		(keys-save text (keys-mesh root))
		:t))

(defun keys-keep (name &optional root)
	; (keys-keep name [root]) -> :nil | :t
	;the key in use kept in the keyring by that name. :nil if there is
	;none in use, or a key of that name already
	(when (and (defq text (load (keys-mesh root))) (not (pii-fstat (keys-file name root))))
		(keys-save text (keys-file name root))
		:t))

(defun keys-take (name file &optional root)
	; (keys-take name file [root]) -> :nil | :t
	;a key someone gave, in a file of the host, into the keyring. :nil if
	;the file is no key, or there is a key of that name
	(when (and (door-key file) (not (pii-fstat (keys-file name root))))
		(keys-save (load file) (keys-file name root))
		:t))

(defun keys-take-text (name text &optional root)
	; (keys-take-text name text [root]) -> :nil | :t
	;a key given as its words, 64 hex digits or a passphrase, into the
	;keyring. :nil if it is no key, or there is a key of that name
	(unless (pii-fstat (keys-file name root))
		(keys-save (cat (trim text (const (char-class " \t\r\n"))) "\n") (keys-file name root))
		(cond
			((door-key (keys-file name root)) :t)
			(:t (pii-remove (keys-file name root)) :nil))))

(defun keys-rename (old new &optional root)
	; (keys-rename old new [root]) -> :nil | :t
	(when (and (defq text (load (keys-file old root))) (not (pii-fstat (keys-file new root))))
		(keys-save text (keys-file new root))
		(pii-remove (keys-file old root))
		:t))

(defun keys-remove (name &optional root)
	; (keys-remove name [root]) -> :nil | :t
	;a key gone from the keyring. If it is the one in use, mesh_key, that
	;is left as it is
	(when (pii-fstat (keys-file name root))
		(pii-remove (keys-file name root))
		:t))
