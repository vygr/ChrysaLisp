;docs/vm/vp_vm.md: the registers of the virtual processor
(defq s (dia-scene) blue 0xffd7e6f7 green 0xffdff0d8 gold 0xffffe8a8)
(dia-say s "16 integer registers, 64 bits" 0 12 12 :t 0xff202428)
(each (# (dia-put s :nil (dia-box (if (= %0 15) ":rsp" (cat ":r" (str %0))) 44 30 (if (= %0 15) gold blue) 11) (* %0 46) 20)) (range 0 16))
(dia-say s "the stack pointer" 660 66 11)
(dia-say s "16 float registers, IEEE" 0 96 12 :t 0xff202428)
(each (# (dia-put s :nil (dia-box (cat ":f" (str %0)) 44 30 green 11) (* %0 46) 104)) (range 0 16))
(dia-say s "A load and store machine. Each is given a real register of the processor by its emit functions, lib/trans/." 0 160 11)
(diagram "vp_registers" (dia-scene-doc s))
