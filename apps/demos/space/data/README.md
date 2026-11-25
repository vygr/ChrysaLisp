# The pictures of Space

`pillars.cpm` is the Pillars of Creation, in the Eagle Nebula, M16, as the
Hubble Space Telescope saw them in visible light in 2014, the whole of the
picture made 1024 by 1024, kept as a `.cpm` of 24 bits a pixel, packed: 16
would leave the dark of the nebula in steps.

Credit: NASA, ESA/Hubble and the Hubble Heritage Team. It is ESA/Hubble's
image heic1501a, the large copy, 6780 by 7071, released under the Creative
Commons Attribution 4.0 International license: it may be reproduced provided
it is clearly and visibly credited, so the demo shows the credit in its
window too.

Each of the other files is made by a bake of its own, so remaking one
leaves the others as they are, each run as
`lisp -r (import "apps/demos/space/bake_nebula.lisp")` and the like:

*	`nebula.cpm`, `bake_nebula.lisp`: a map of all of the sky round the eye,
	made from the Pillars, their own nebula carried out past their edges into
	the dark, the galaxy's band round all of it and a nebula of their colours
	on the far side, 32 bits a pixel. The sky shader lays the picture on the
	sky in one direction over it, so the picture has no frame. Made again
	after a change to the picture or to where it is, `sky.inc`.

*	`stars.cpm`, `bake_stars.lisp`: the stars, a map as the nebula's but of
	the size they are seen at, 4096 by 2048, black but for them, the bright
	ones with Hubble's spikes on the Pillars' diagonals. At random, so a bake
	is new stars.

*	`crunch_1.wav` to `crunch_3.wav` and `crunch_far.wav`, `bake_crunch.lisp`:
	the sound of rock hitting rock, near and far off. At random, so a bake is
	new sounds.

*	`crunch_1_60.wav` and the like, `bake_levels.lisp`: the crunches at the
	loudness a hit is heard at, made of the crunches there are, after
	`bake_crunch.lisp`.

*	`thrust.wav`, `bake_thrust.lisp`: the roar of the ship's boost, four
	seconds of it that go round without a seam, its end faded into its
	start, and nothing in it that beats in time, so it is not heard to go
	round: a deep rumble, the hiss of the exhaust and a faint low hum. The
	app writes it to a stream as the boost is held, from a place at random
	each time. At random, so a bake is a new roar.

To make `pillars.cpm`, with the tree's own loader and resize, the picture
being some 190 MB of pixels:

	(import "gui/lisp.inc")
	(import "lib/image/cpm.inc")
	(defq big (canvas-load "heic1501a.jpg" +load_flag_noswap)
		square (Canvas 1024 1024 1))
	(. square :resize big)
	(CPM-save square (file-stream "pillars.cpm" +file_open_write) 24 :nil :t)
