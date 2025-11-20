(import "lib/asm/asm.inc")

(print "=== WASM Backend Minimal Test ===")
(print "")

; Set up for WASM compilation
(defq *cpu* 'wasm32 *abi* 'WASM32)
(print "CPU set to: " *cpu*)
(print "ABI set to: " *abi*)
(print "")

; Test that the emit file exists and loads
(defq emit_file (cat "lib/trans/" *cpu* ".inc"))
(print "Loading: " emit_file)

(catch
	(progn
		(import emit_file)
		(print "✓ WASM backend loaded successfully!")
		(print ""))
	(lambda (e)
		(print "✗ Failed to load WASM backend: " e)
		(exit 1)))

; Verify critical functions exist
(print "Checking critical functions...")
(defq checks (list
	'emit-native-reg?
	'emit-add-rr
	'emit-sub-rr
	'emit-mul-rr
	'emit-cpy-cr
	'emit-cpy-rr
	'emit-cpy-ir
	'emit-cpy-ri
	'emit-beq-rr
	'emit-call
	'emit-ret
	'emit-push
	'emit-pop
	'emit-leb128-unsigned
	'emit-leb128-signed))

(defq all_ok :t)
(each (lambda (fname)
	(if (def? fname)
		(print "  ✓ " fname " available")
		(progn
			(print "  ✗ " fname " MISSING!")
			(setq all_ok :nil))))
	checks)

(print "")

(if all_ok
	(progn
		(print "=== All Checks Passed! ===")
		(print "")
		(print "WASM backend is properly loaded and ready for use.")
		(print "Next step: Test actual instruction emission"))
	(progn
		(print "=== Some Checks Failed ===")
		(exit 1)))
