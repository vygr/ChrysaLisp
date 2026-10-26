(report-header "Bind: a function with no native is translated from its VP by the loader, and kept")

;two functions there is only VP of, the binary bundle of their file in
;obj/vp/<mode>/, one calling the other: a bind of the first has the loader
;find and translate both, closure first, keep their natives where a build
;would, the same to the byte, and run. The bundle is put in the folder of
;each mode, a test can not tell a debug image from a validate one
(bind '(& bt_cpu bt_abi) (map (const sym) (split (load-path) "/")))
(defq bt_lf (ascii-char 10) bt_id (str (pii-time))
	bt_a (cat "tests/bt_" bt_id "_a") bt_b (cat "tests/bt_" bt_id "_b")
	bt_obj (cat "obj/" bt_cpu "/" bt_abi "/") bt_src (cat bt_obj "tests/bt_" bt_id ".vp")
	bt_vpb (map (# (cat "obj/vp/" %0 "/tests/bt_" bt_id ".vpb")) '("release" "debug" "validate"))
	bt_bins (list))

;b adds 3 to :r0, a is called from Lisp with a num and gives a num, of b of it
(save (cat "(include {lib/asm/func.inc})" bt_lf "(include {class/num/class.inc})" bt_lf
	"(include {class/lisp/class.inc})" bt_lf
	"(def-func {" bt_b "})" bt_lf "(vp-add-cr 3 :r0)" bt_lf "(vp-ret)" bt_lf "(def-func-end)" bt_lf
	"(def-func {" bt_a "})" bt_lf "(vp-push :r0)" bt_lf "(list-bind-args :r1 '(:r0) '(:num))" bt_lf
	"(fn-call '" bt_b ")" bt_lf "(call :num :create '(:r0) '(:r1))" bt_lf
	"(vp-pop :r0)" bt_lf "(vp-ret)" bt_lf "(def-func-end)" bt_lf) bt_src)
(defun bt-make ()
	;the VP binary of each, and what a build makes of it
	(bind '(& *cpu* *abi*) (map (const sym) (split (load-path) "/")))
	(within-compile-env (lambda ()
		(each (lambda (entry)
				(defq bin (vpb-encode entry))
				(push bt_bins (list (first entry) (vp-translate-bin bin) bin)))
			(vp-compile bt_src))
		(defq bundle (vpb-bundle (map (const third) bt_bins)))
		(each (# (save bundle %0)) bt_vpb))))
(bt-make)
(pii-remove bt_src)

(assert-true "no native of either before" (every (# (= 0 (age (cat bt_obj %0)))) (list bt_a bt_b)))
(ffi bt_a bt_f)
(assert-eq "bound, and b called from a" 45 (bt_f 42))
(each (lambda ((name native &ignore))
		(assert-eq (cat "the native kept is what a build makes, " name) native (load (cat bt_obj name))))
	bt_bins)

(each (# (pii-remove (cat bt_obj %0))) (list bt_a bt_b))
(each (const pii-remove) bt_vpb)

;the loader's VP is made once for every CPU, so it names no CPU's
;translator: it links lib/trans/vpb, which the maker of an image gives the
;translator of the CPU it makes the image for. The image this runs on holds
;its own CPU's
(import "lib/boot/image.inc")
(defq bt_names (map (lambda ((c a)) (trans-name c a))
		'((arm64 ARM64) (x86_64 AMD64) (x86_64 WIN64) (riscv64 RISCV64) (la64 LA64) (vp64 VP64)))
)
(defun bt-loader ()
	;the loader's VP, compiled here, as text
	(bind '(& *cpu* *abi*) (map (const sym) (split (load-path) "/")))
	(defq out "")
	(within-compile-env (lambda ()
		(each (lambda (entry)
				(if (eql (first entry) "sys/load/load") (setq out (str entry))))
			(vp-compile "sys/load/class.vp"))))
	out)
(defq bt_loader (bt-loader))
(assert-true "the loader's VP links lib/trans/vpb" (found? bt_loader "lib/trans/vpb"))
(assert-eq "and names no CPU's translator" :nil (some (# (if (found? bt_loader %0) %0)) bt_names))
(assert-true "the image this runs on holds its own CPU's translator"
	(found? (load (cat (load-path) "sys/boot_image")) (trans-name bt_cpu bt_abi)))

(undef (env) 'bt_cpu 'bt_abi 'bt_lf 'bt_id 'bt_a 'bt_b 'bt_obj 'bt_src 'bt_vpb 'bt_bins 'bt_f 'bt-make
	'bt_names 'bt_loader 'bt-loader)

