(report-header "Net: the keyring, mesh keys by name and the one in use")
(import "service/net/keys.inc")

;a tree of its own, so the key of this machine is not touched
(defq ky_root "tests/scratch/keyring")
(each (# (pii-remove (keys-file %0 ky_root))) (keys-names ky_root))
(pii-remove (keys-mesh ky_root))

(assert-list-eq "none at first" '() (keys-names ky_root))
(assert-eq "and none in use" :nil (keys-in-use ky_root))
(assert-true "a new key" (keys-new "rig" ky_root))
(assert-eq "not two of a name" :nil (keys-new "rig" ky_root))
(assert-true "another" (keys-new "wan" ky_root))
(assert-list-eq "both, in order" '("rig" "wan") (keys-names ky_root))
(assert-eq "64 hex digits" 64 (length (trim (load (keys-file "rig" ky_root)) (const (char-class " \t\r\n")))))
(assert-eq "new keys differ" :nil
	(eql (door-key (keys-file "rig" ky_root)) (door-key (keys-file "wan" ky_root))))

;in use
(assert-true "one used" (keys-use "wan" ky_root))
(assert-eq "it is the one in use" "wan" (keys-in-use ky_root))
(assert-eq "mesh_key is it" (door-key (keys-file "wan" ky_root)) (door-key (keys-mesh ky_root)))
(assert-eq "no such key to use" :nil (keys-use "cafe" ky_root))

;fingerprints, of the key's 32 bytes, as written or by a passphrase
(defq ky_print (keys-print (door-key (keys-file "wan" ky_root))))
(assert-eq "four groups of four" 19 (length ky_print))
(assert-eq "the same key, the same print" ky_print (keys-print (door-key (keys-mesh ky_root))))
(save (to-upper (load (keys-file "wan" ky_root))) "tests/scratch/key_given")
(assert-eq "hex in another case is the same key" ky_print (keys-print (door-key "tests/scratch/key_given")))

;one given, kept in use by hand, renamed, forgotten
(assert-true "a key given is taken" (keys-take "given" "tests/scratch/key_given" ky_root))
(save "  " "tests/scratch/key_given")
(assert-eq "a file of nothing is no key" :nil (keys-take "nothing" "tests/scratch/key_given" ky_root))
(pii-remove "tests/scratch/key_given")
(save "a passphrase of our own" (keys-mesh ky_root))
(assert-eq "one put in mesh_key by hand is in no keyring file" :nil (keys-in-use ky_root))
(assert-true "kept" (keys-keep "phrase" ky_root))
(assert-eq "and it is the one in use" "phrase" (keys-in-use ky_root))
(assert-true "renamed" (keys-rename "phrase" "ours" ky_root))
(assert-eq "in use by its new name" "ours" (keys-in-use ky_root))
(assert-eq "not onto one there is" :nil (keys-rename "ours" "rig" ky_root))
(assert-true "forgotten" (keys-remove "ours" ky_root))
(assert-eq "mesh_key is left in use" :t (if (door-key (keys-mesh ky_root)) :t))
(test-cases
	(keys-name? "rig") :t
	(keys-name? "wan-2_b") :t
	(keys-name? "") :nil
	(keys-name? "../x") :nil
	(keys-name? "a b") :nil)

(each (# (pii-remove (keys-file %0 ky_root))) (keys-names ky_root))
(pii-remove (keys-mesh ky_root))
(undef (env) 'ky_root 'ky_print)
