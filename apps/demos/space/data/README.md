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

*	`crunch_1.wav` to `crunch_7.wav` and `crunch_far.wav`: the sound of rock
	hitting rock, a near one picked at random and the far one further off,
	each played as loud as a hit is heard. Cut from "Impacts 01" by aglinder
	(Freesound), via freesound_community on Pixabay, published there on 27
	July 2022, pixabay.com/sound-effects/film-special-effects-impacts-01-73737/,
	under the Pixabay Content License: seven of its hits near, each from just
	before it to the end of its own ring, and its longest, heaviest one far
	off, low passed twice at 900Hz so it is dulled by the distance. Faded in
	over 5ms and out over 10ms so neither end clicks, made mono, 16 bits at
	22050 a second, and each brought to a peak 2dB under full.

*	`thrust.wav`: the roar of the ship's thrust, six seconds of a steady
	rumble of thrusters that go round without a seam, cut from
	"thrusters_loop.wav" by DJT4NN3R (Freesound), via freesound_community
	on Pixabay, published there on 28 January 2022,
	pixabay.com/sound-effects/film-special-effects-thrusters-loopwav-14699/,
	under the Pixabay Content License. Its last half second faded into its
	first, at equal power, so its end runs on into its start; made mono, 16
	bits at 22050 a second, and brought to a peak 2dB under full. The app
	writes it to a stream as the ship thrusts, from a place at random each
	time, rising quickly and dying away as a sound does.

*	`launch.wav` and `blast.wav`: a missile fired, and going off, cut from
	"Missile Explosion" by VoiceBosch, published on Pixabay on 28 September
	2023, pixabay.com/sound-effects/film-special-effects-missile-explosion-168600/,
	under the Pixabay Content License. The launch its whoosh, up to the gap
	after it, faded in over 20ms and out over 30ms; the blast from its bang,
	two seconds of it, faded in over 5ms and out over its last 600ms. Each
	made mono, 16 bits at 22050 a second, and brought to a peak 2dB under
	full; the launch is played from the ship, the blast from its side and the
	quieter the further off.

*	`klaxon.wav`: the warning of a buoy armed to go nuclear, one call of an
	old klaxon horn, the first of three in "jsyd_klaxon2" by jbum
	(Freesound), via freesound_community on Pixabay, published there on 8
	September 2022,
	pixabay.com/sound-effects/film-special-effects-jsyd-klaxon2-108145/,
	under the Pixabay Content License. Cut from just before it sounds to
	just after, faded in over 30ms and out over 200ms so neither end clicks,
	made 16 bits at 22050 a second from its 8000, and brought to a peak 2dB
	under full.

*	`boom.wav`: a buoy going nuclear, a big explosion dying away of itself,
	from "Explosion SFX" by DRAGON-STUDIO, published on Pixabay on 17
	September 2025, pixabay.com/sound-effects/film-special-effects-explosion-sfx-405455/,
	under the Pixabay Content License. All of it, its tail its own, faded in
	over 5ms, so its bang is kept and its first sample is not a click, and
	out over its last 10ms, silent by then; made mono, 16 bits at 22050 a
	second, and brought to a peak 2dB under full.

To make `pillars.cpm`, with the tree's own loader and resize, the picture
being some 190 MB of pixels:

	(import "gui/lisp.inc")
	(import "lib/image/cpm.inc")
	(defq big (canvas-load "heic1501a.jpg" +load_flag_noswap)
		square (Canvas 1024 1024 1))
	(. square :resize big)
	(CPM-save square (file-stream "pillars.cpm" +file_open_write) 24 :nil :t)
