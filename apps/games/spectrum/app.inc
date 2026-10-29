;the Spectrum 48K, what its native code and its window both need

;the machine is one str: this state, then the 64KB the Z80 sees. The
;native code holds the Z80 in VP registers for a frame, and reads and
;writes this only as it starts and stops, and for what is seldom used: the
;index registers, the other set, the interrupt state, the keys. af has A
;high and F low. hlx is HL while an index prefix has IX or IY in its place.
;r is the refresh count, its top bit r7 kept apart as the Z80 does. ts is
;the T states left of the frame, 0 or less a new frame to start. keys is
;the matrix, a byte a half row, a bit 0 a key held, kemp the joystick, ear
;what the tape reads, beep the last bit 4 sent to the ULA. trap, not 0,
;stops at the ROM's LD-BYTES for the loader
(structure +zx 0
	(long ts)
	(ushort bc de hl af ix iy sp pc)
	(ushort bc2 de2 hl2 af2 hlx)
	(ubyte i r r7 im iff1 iff2 halted border)
	(ubyte kemp ear trap beep)
	(struct keys 8)
	(uint beep_at)
	(int dc_x dc_y)
	(align))

;why a run stopped: the frame done, the ROM's tape loader reached
(enums +zx_stop 0
	(enum frame tape))

;T states of a frame, where LD-BYTES starts, and where it is past its
;EX AF,AF' and border
(defq +zx_frame 69888 +zx_ld_bytes 0x0556 +zx_ld_bytes2 0x0562 +zx_mem_size 0x10000)

;the beeper, a sample each 112 T states, 624 a frame, 31200 a second. After
;the memory, how long the speaker was high in each sample of the frame,
;then the frame's samples, 16 bit, made of them at its end. beep_at is how
;far into the frame they are counted, dc_x and dc_y the last in and out of
;the filter that takes away what does not change
(defq +zx_t_sample 112 +zx_samples 624 +zx_rate 31200
	+zx_bins 0x10000 +zx_pcm (+ 0x10000 (* 624 2))
	+zx_machine_size (+ +zx_size 0x10000 (* 624 4)))

;the screen and the border round it, as drawn at zoom 1
(defq +zx_edge 32 +zx_width (+ 256 (* 2 32)) +zx_height (+ 192 (* 2 32)))
