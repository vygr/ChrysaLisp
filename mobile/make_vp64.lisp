;the VP64 objects and boot image, all of them, for the emulator a phone or a
;tablet runs. ./run_tui.sh -n 0 -f -s mobile/make_vp64.lisp
(import "lib/asm/asm.inc")
(remake-all-vp)
(pii-exit)
