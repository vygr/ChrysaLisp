(report-header "Source: VP another system has, not this node, comes as VP binary and is translated here")

;@Source's own functions, called from here: a manifest kept as if another
;system had sent it says its VP of a function is not what this node has,
;so (source-translate) asks that system's @Source for its VP, "$mode name",
;which is answered with VP binary, and translates it. The other system is
;this one's @Source, so the native must be what the build made
(defq main :nil)
(import "service/source/app_impl.lisp")
(defq *source_env* (env) *source_hashes* (Fmap 101) *source_files* (Fmap 101) *source_kept* (Fmap 101)
	*source_svcs* (Fmap 11) *source_counts* (Fmap 11)
	sb_svc (hex-encode (source-here)) sb_mode (source-mode) sb_hash (pad "" 64 "0")
	sb_name "apps/system/netspeed/vops")

(defun sb-translate (name)
	;the natives of a function, of the manifest kept for it, a line of its
	;name and a hash not this node's
	(. *source_kept* :insert (cat sb_svc " #" sb_mode " " name) (cat name " " sb_hash))
	(pii-remove (source-cache sb_hash))
	(source-translate (cat "Tsb " sb_svc " " sb_mode " " name)))

(assert-eq "fetched as VP binary, translated here, the same as the build's"
	(load (cat (load-path) sb_name)) (sb-translate sb_name))
(if (source-caching?)
	(assert-eq "and kept by the hash the manifest gave it" (load (cat (load-path) sb_name)) (load (source-cache sb_hash)))
	(test-skip "kept by its hash" "this system keeps no translations"))
(assert-eq "one the other system has none of: nothing, not a part" :nil
	(sb-translate "tests/source_bin/none"))

(pii-remove (source-cache sb_hash))
