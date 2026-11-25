(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "lib/math/matrix.inc")
(import "lib/gpu/render_farm.inc")
(import "lib/gpu/vp.inc")
(import "lib/math/scene.inc")
(import "./craft.inc")
(import "apps/media/jukebox/music.inc")

;A flight over a land that is made as it goes, drawn by a (Render-tris),
;lib/gpu/render_farm.inc, which has the GPU draw it, or on a host with
;none the nodes of this machine, a strip of the frame each, or this task
;if there is only the one. A frame is two draws, each by its own pair of shaders:
;the sky, at the back, and the land, a grid that is moved under the eye
;and raised to the hills of a noise in its vertex shader, so the land
;rolls by and nothing of it is made again. The flight goes round a figure
;of eight, leaning into each turn, left and then right; or it finds its
;own way along the valleys and over the lakes, low, by the height of the
;land the shaders have, land.shader, which Lisp asks too, (shader-vp-func)

(enums +event +event_user
	(enum path prev music next day))

;the timer is last, what is first in the list is what is read first, and
;a frame can take longer than a tick
(enums +select 0
	(enum main task reply music music_song music_tick timer))

;the frame's two sizes, the min and max buttons
(defq +small ''(640 480) +large ''(1024 768))

(defq +timer_rate (/ 1000000 60)
	;the grid, cells each way and how big a cell is, the GPU's, and the
	;CPU's of cells twice as big over the same ground, for a host with no
	;GPU: a quarter of the vertices, each of which is five samples of the
	;land's height. The rest of the frame is the same, the shaders and all
	;they are given. There is no CPU button: Flight is a show of the GPU,
	;and on the CPU it crawls
	+cells 192 +cell (n2d 6.5) +cells_cpu 96 +cell_cpu (n2d 13)
	;the flight, a figure of eight: how far it reaches across and along,
	;how fast, how high, how it looks down, and how hard it leans into a
	;turn, as a plane does for a pull of so much
	+reach_x (n2d 640) +reach_z (n2d 320) +speed (n2d 24) +altitude (n2d 85) +pull (n2d 9)
	+pitch_eight (n2d 0.18)
	;the flight along the valleys, as a jet low down: how far ahead and how
	;far round it looks at the land, how hard it turns to the lower side,
	;how fast at most it turns, a second, and how slowly it comes to that,
	;how far over the land it keeps, how it looks down, how fast it may
	;climb and come down, a second, and how slowly it comes to that
	+ahead (n2d 120) +aside (n2d 0.45) +steer (n2d 0.004) +turn_most (n2d 0.25) +turn_ease (n2d 0.5)
	+clearance (n2d 24) +pitch_valley (n2d 0.1) +climb (n2d 16) +sink (n2d 8) +rise_ease (n2d 0.8)
	;flown by the pointer: the middle of the picture it does nothing in, how
	;fast at most it turns and climbs or dives for the pointer at an edge,
	;how near the land it may go whatever the pointer says, and how high
	+dead (n2d 0.12) +stick_turn (n2d 0.5) +stick_climb (n2d 24) +floor (n2d 14) +ceiling (n2d 260)
	;how high the frame is at a distance of 1, its width of its shape, and
	;how near and far it sees
	+spread_y (n2d 0.5) +near (n2d 1) +far (n2d 700)
	+land_vertex (cat *app_root* "land_vertex.shader") +land_lit (cat *app_root* "land_lit.shader")
	sky_files (list (cat *app_root* "sky_vertex.shader") (cat *app_root* "sky_lit.shader"))
	moon_files (list (cat *app_root* "moon_vertex.shader") (cat *app_root* "moon_lit.shader"))
	craft_files (list (cat *app_root* "craft_vertex.shader") (cat *app_root* "craft_lit.shader"))
	beam_files (list (cat *app_root* "beam_vertex.shader") (cat *app_root* "beam_lit.shader"))
	cloud_files (list (cat *app_root* "cloud_vertex.shader") (cat *app_root* "cloud_lit.shader"))
	;the meshes, by their numbers, the craft's parts from the last on
	+mesh_land 0 +mesh_sky 1 +mesh_land_cpu 2 +mesh_moon 3 +mesh_beam 30 +mesh_cloud 31 +mesh_craft 4
	;the farm that makes the craft's parts, which of them have come, and
	;the craft, fleet.inc
	jobs :nil parts_had (map (const (lambda (_) :nil)) craft_parts) crafts (list)
	;the moon: the way to it, opposite the sun, (day-state), how far, in
	;front of the sky and behind the land near by, how big, how it leans and
	;how fast it turns, a second
	moon_way (doubles (n2d -0.646) (n2d 0.188) (n2d 0.74)) +moon_far (n2d 680) +moon_size (n2d 52)
	+moon_lean (n2d 0.12) +moon_turn (n2d 0.02) moon_map :nil
	;the sun, and the haze at the horizon and the blue above
	sun (doubles (n2d 0.55) (n2d 0.42) (n2d -0.72)) haze (doubles (n2d 0.74) (n2d 0.80) (n2d 0.86))
	above (doubles (n2d 0.28) (n2d 0.46) (n2d 0.78))
	;the day: where the sun is, the light that lights the land, the sun's by
	;day and the moon's by night, its colour, and how it shines off water;
	;and where in the day it starts, where it was last, (day-state)
	sun_at sun daylight (doubles +double_1 +double_1 +double_1) shine (doubles +double_1 +double_1 +double_1)
	day_start +double_0 day_at +double_0 day_dragged 0 night +double_0
	;how long the day, dusk, night and dawn are, seconds
	+day_long (n2d 300) +dusk_long (n2d 60) +night_long (n2d 240) +dawn_long (n2d 60)
	select :nil id :t render :nil frames 0 since (pii-time) began (pii-time)
	;the size wanted, the size of the canvas drawn on, and the canvas still
	;on show till a frame of a new one is whole
	want +small size +small shown :nil
	;how far round the eight the flight is, and when it was last moved on
	round_at +double_0 moved (pii-time)
	;the flight, the eight or along the valleys: where it is and its way,
	;level, how high, how fast it turns, and the middle of the eight
	path :eight place (doubles +double_0 +double_0) way (doubles +double_0 +double_1)
	height +altitude rising +double_0 turning +double_0 middle (doubles +double_0 +double_0)
	;where the pointer is over the picture, -1 to 1 across and up, or :nil
	;when it is not
	stick :nil
	;the lean and the pitch, as eased to what the way of flying wants, and
	;how long yet the flight is coming onto the eight, and how long it takes
	lean +double_0 pitch +double_0 eight_in +double_0 +eight_onto (n2d 2.5)
	;how fast the lean and the pitch move, and how stiff their spring is
	lean_v +double_0 pitch_v +double_0 +spring_k (n2d 3.5)
	;the lean the way of flying wanted last, what turn it was in
	lean_want +double_0
	;how much less detail it draws, 0 all of it to 4 least, for a GPU too
	;slow for all of it: how many seconds running it has been slow, or fast,
	;what is slow and fast, and when it last went down, not to go up again
	;for a while after
	detail 4 slow 0 fast 0 +rate_least 28 +rate_most 50 detail_down 0
	;how the eight lies, the cosine and sine of its turn on the land
	eight_lie (doubles +double_1 +double_0)
	;the height of the land at a place, of land.shader, the shaders' own
	land_height :nil
	;the music, the user's Jukebox favourites, apps/media/jukebox/music.inc,
	;and whether the user wants it, kept whether or not there is any to play
	music :nil music_on :t
	;the flight, the size, the music, the time of day and the detail, as
	;they were when last closed, (config-save)
	*config* :nil +config_version 1 +config_file (cat (get :home *env*) "flight.tre"))

(defun make-canvas (size)
	; (make-canvas size) -> canvas
	;its pixels are in shared memory if the host has it, the nodes of this
	;machine then draw on them, a strip each
	(bind '(w h) size)
	(ifn (canvas-shared w h 1) (Canvas w h 1)))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Flight" (+sym_close +sym_max +sym_min) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(. (ui-radio-bar *path* ("Eight" "Valleys" "Mouse") (:font :font_body)) :connect +event_path)
		(ui-tool-bar *music* ()
			(ui-buttons (+sym_media_start +sym_media_play +sym_media_end) +event_prev))
		(ui-label *status* (:text "..." :font :font_body)))
	;the time of day, which goes on, and is dragged to another
	(. (ui-slider *day* (:value 0 :maximum 1000 :portion 1 :min_height 12)) :connect +event_day)
	(ui-backdrop *backdrop* (:style :plain :paper +argb_black :min_width (first +small) :min_height (second +small))
		(ui-element *canvas* (make-canvas +small))))

(defun land-verts (cells cell)
	; (land-verts cells cell) -> doubles
	;the grid of cells each way, each cell big, its middle at 0 0, a vertex
	;a place across and along, two triangles a cell
	(defq out (list) half (* (n2d (/ cells 2)) cell))
	(each! (lambda () (defq z (- (* (n2d (!)) cell) half) z1 (+ z cell))
			(each! (lambda () (defq x (- (* (n2d (!)) cell) half) x1 (+ x cell))
					(push out x z x1 z x1 z1 x z x1 z1 x z1)) '() 0 cells)) '() 0 cells)
	(apply (const doubles) out))

(defun sky-verts ()
	; (sky-verts) -> doubles
	;two triangles over all of the frame, by its corners
	(doubles +double_-1 +double_-1 +double_1 +double_-1 +double_1 +double_1
		+double_-1 +double_-1 +double_1 +double_1 +double_-1 +double_1))

(defun globe-verts (segs rings)
	; (globe-verts segs rings) -> doubles
	;a ball of radius 1 as triangles, three vertices each, a vertex its
	;place, 3 numbers, and where on the map that is, 2. North is up, and
	;east goes round to the right as seen from outside, as a map has it.
	;A triangle goes round counter clockwise as seen from outside
	(defq out (list) rows (map! (lambda () (defq r (!)) (defq theta (/ (* +double_pi (n2d r)) (n2d rings)) st (sin theta) ct (cos theta))
				(map! (lambda () (defq phi (/ (* +double_2pi (n2d (!))) (n2d segs)))
						(list (* st (sin phi)) ct (* st (cos phi))
							(/ (n2d (!)) (n2d segs)) (/ (n2d r) (n2d rings)))) '() 0 (inc segs))) '() 0 (inc rings)))
	(each! (lambda () (defq r (!)) (each! (lambda () (defq s (!)) (defq a (elem-get (elem-get rows r) s) b (elem-get (elem-get rows (inc r)) s)
						c (elem-get (elem-get rows (inc r)) (inc s)) d (elem-get (elem-get rows r) (inc s)))
					;at a pole a quad is a triangle, the other has no area
					(if (< r (dec rings)) (each (lambda (v) (each (lambda (n) (push out n)) v)) (list a b c)))
					(if (> r 0) (each (lambda (v) (each (lambda (n) (push out n)) v)) (list a c d)))) '() 0 segs)) '() 0 rings)
	(apply (const doubles) out))

(defun tube-verts (segs)
	; (tube-verts segs) -> doubles
	;an open tube of segs sides, a column, its top of radius 1 at 0 and its
	;foot a little wider at -1, a vertex its place, 3 numbers, three to a
	;triangle
	(defq out (list) flare (n2d 1.2))
	(each! (lambda ()
			(defq a (/ (* +double_2pi (n2d (!))) (n2d segs)) b (/ (* +double_2pi (n2d (inc (!)))) (n2d segs))
				ca (cos a) sa (sin a) cb (cos b) sb (sin b))
			(push out ca +double_0 sa (* ca flare) +double_-1 (* sa flare) (* cb flare) +double_-1 (* sb flare)
				ca +double_0 sa (* cb flare) +double_-1 (* sb flare) cb +double_0 sb))
		'() 0 segs)
	(apply (const doubles) out))

(defun argb (rgb)
	; (argb rgb) -> argb
	(+ 0xff000000 (reduce (lambda (a c) (+ (<< a 8) (n2i (* c (n2d 255))))) rgb 0)))

(defun blend (a b ka kb)
	; (blend a b ka kb) -> doubles
	;a times ka and b times kb, two ways in the world added
	(apply (const doubles) (map (lambda (x y) (+ (* x ka) (* y kb))) a b)))

(defun path-pace (at)
	; (path-pace at) -> double
	;how far the eight goes for a step of at there, so the flight is moved
	;on by its speed, as fast round the ends as through the middle
	(defq dx (* +reach_x (cos at)) dz (* (n2d 2) +reach_z (cos (* (n2d 2) at))))
	(sqrt (+ (* dx dx) (* dz dz))))

(defun eight-lie (x z)
	; (eight-lie x z) -> (x z)
	;a place or a way on the eight, turned as the eight lies, its cosine and
	;sine, no angle
	(bind '(c s) eight_lie)
	(list (- (* c x) (* s z)) (+ (* s x) (* c z))))

(defun eight-point (at)
	; (eight-point at) -> doubles
	;where on the eight, across and along, for its middle at 0 0
	(apply (const doubles) (eight-lie (* +reach_x (sin at)) (* +reach_z (sin (* (n2d 2) at))))))

(defun mix3 (a b k)
	; (mix3 a b k) -> doubles
	;a colour or a way, from a at 0 to b at 1
	(blend a b (- +double_1 k) k))

(defun day-state (seconds)
	; (day-state seconds)
	;the day as it is: by day the sun high, then dusk, its colours and the
	;sun going down, night, the moon's light, and dawn. The light the land
	;and the craft are lit by and cast shadows from goes from the sun to
	;the moon and back across dusk and dawn, so the craft's shadows are of
	;the moon by night, and its colour is warm by day, red at the ends and
	;cool white by night; the sky's sun and the moon's phase are the sun's
	(defq total (+ +day_long +dusk_long +night_long +dawn_long)
		at (+ seconds day_start) at (- at (* total (n2d (n2i (/ at total)))))
		d +double_0 g +double_0 e +double_0)
	(setq day_at at)
	;the slider where the day is, unless a hand has had it in the last second
	(and (> (- (pii-time) day_dragged) 1000000)
		(/= (get :value *day*) (defq v (n2i (/ (* at (n2d 1000)) total))))
		(def (. *day* :dirty) :value v))
	(cond
		((< at +day_long)
			(setq e (+ (n2d 0.12) (* (n2d 0.38) (sin (/ (* +double_pi at) +day_long))))))
		((< at (+ +day_long +dusk_long))
			(defq r (/ (- at +day_long) +dusk_long))
			(setq d (* r r (- (n2d 3) (* (n2d 2) r))) g (* (n2d 4) r (- +double_1 r)) e (- (n2d 0.12) (* (n2d 0.37) r))))
		((< at (+ +day_long +dusk_long +night_long))
			(setq d +double_1 e (n2d -0.25)))
		(:t (defq r (/ (- at +day_long +dusk_long +night_long) +dawn_long))
			(setq d (- +double_1 (* r r (- (n2d 3) (* (n2d 2) r)))) g (* (n2d 4) r (- +double_1 r))
				e (+ (n2d -0.25) (* (n2d 0.37) r)))))
	(defq az (+ (n2d 0.9) (/ (* +double_2pi at) total)) ce (cos e)
		k (* g (n2d 0.85)))
	;the moon opposite the sun, full, up by night as the sun is down; by
	;day low in the sky, not set, the moon by day
	(defq me (max (neg e) (n2d 0.24)) mce (cos me))
	(setq moon_way (doubles (* mce (cos (+ az +double_pi))) (sin me) (* mce (sin (+ az +double_pi))))
		sun_at (doubles (* ce (cos az)) (sin e) (* ce (sin az)))
		sun (blend sun_at moon_way (- +double_1 d) d)
		haze (mix3 (mix3 (doubles (n2d 0.74) (n2d 0.80) (n2d 0.86)) (doubles (n2d 0.05) (n2d 0.07) (n2d 0.13)) d)
			(doubles (n2d 0.95) (n2d 0.60) (n2d 0.38)) k)
		above (mix3 (mix3 (doubles (n2d 0.28) (n2d 0.46) (n2d 0.78)) (doubles (n2d 0.01) (n2d 0.02) (n2d 0.07)) d)
			(doubles (n2d 0.30) (n2d 0.30) (n2d 0.55)) k)
		daylight (mix3 (mix3 (doubles +double_1 +double_1 +double_1) (doubles (n2d 0.24) (n2d 0.29) (n2d 0.44)) d)
			(doubles +double_1 (n2d 0.68) (n2d 0.48)) k)
		shine (mix3 (mix3 (doubles +double_1 (n2d 0.94) (n2d 0.80)) (doubles (n2d 0.80) (n2d 0.90) (n2d 1.15)) d)
			(doubles (n2d 1.25) (n2d 0.60) (n2d 0.30)) k))
	(bind '(sx sy sz) sun)
	(defq n (sqrt (+ (* sx sx) (* sy sy) (* sz sz))))
	(setq sun (doubles (/ sx n) (/ sy n) (/ sz n)) night d))

(defun spring (x v to dt)
	; (spring x v to dt) -> (x v)
	;x moved toward to by a spring that is damped just enough not to go
	;past it, v how fast it moves: a change starts slowly and gathers, no
	;kick, and stops as slowly. A step of no more than a tenth of a second
	(defq dt (min dt (n2d 0.1))
		v (+ v (* (- (* +spring_k +spring_k (- to x)) (* (n2d 2) +spring_k v)) dt)))
	(list (+ x (* v dt)) v))

(defun rise-toward (want dt)
	;the height on by how fast it rises, eased toward how fast it would: a
	;climb and a dive in a curve, not at a corner
	(setq rising (+ rising (* (- want rising) (min +double_1 (* dt +rise_ease))))
		height (+ height (* rising dt))))

(defun ease-height (target dt)
	;toward a height, no faster up or down than it may
	(rise-toward (max (neg +sink) (min +climb (* (- target height) (n2d 0.6)))) dt))

(defun beyond-dead (x)
	; (beyond-dead x) -> double
	;how far past the dead middle the pointer is, -1 to 1
	(cond
		((> x +dead) (/ (- x +dead) (- +double_1 +dead)))
		((< x (neg +dead)) (/ (+ x +dead) (- +double_1 +dead)))
		(+double_0)))

(defun eight-pose (dt)
	; (eight-pose dt) -> (fx fz lean)
	;moved on round the eight by its speed, as fast round the ends as
	;through the middle: its way, level, and its lean into the turn, whose
	;tangent is speed squared times the bend over the pull. Coming onto it,
	;for a while, the flight goes on its own way, turning as it was eased
	;to the eight's bend, and its way drawn more and more to the eight's,
	;the eight moved along with it: it flies onto the eight, no kick
	(setq round_at (+ round_at (/ (* +speed dt) (path-pace round_at))))
	(defq at round_at two (* (n2d 2) at)
		dx (* +reach_x (cos at)) dz (* (n2d 2) +reach_z (cos two))
		ddx (neg (* +reach_x (sin at))) ddz (neg (* (n2d 4) +reach_z (sin two)))
		pace (sqrt (+ (* dx dx) (* dz dz)))
		bend (/ (- (* dx ddz) (* dz ddx)) (* pace pace pace)))
	(bind '(ex ez) (eight-lie (/ dx pace) (/ dz pace)))
	(ease-height +altitude dt)
	(cond
		((> eight_in +double_0)
			(setq eight_in (max +double_0 (- eight_in dt))
				turning (+ turning (* (- (neg (* +speed bend)) turning) (min +double_1 (* dt (n2d 2))))))
			(bind '(fx fz) way)
			(defq t (- +double_1 (/ eight_in +eight_onto)) s (* t t (- (n2d 3) (* (n2d 2) t)))
				a (* turning dt) ca (cos a) sa (sin a)
				nx (+ (* (+ (* fx ca) (* fz sa)) (- +double_1 s)) (* ex s))
				nz (+ (* (- (* fz ca) (* fx sa)) (- +double_1 s)) (* ez s))
				n (sqrt (+ (* nx nx) (* nz nz))) nx (/ nx n) nz (/ nz n))
			(bind '(px pz) place)
			(setq way (doubles nx nz) place (doubles (+ px (* nx +speed dt)) (+ pz (* nz +speed dt)))
				middle (blend place (eight-point at) +double_1 +double_-1))
			(list nx nz (+ (* (neg (/ (* +speed turning) +pull)) (- +double_1 s))
				(* (/ (* +speed +speed bend) +pull) s))))
		(:t (setq place (blend middle (eight-point at) +double_1 +double_1) way (doubles ex ez))
			(list ex ez (/ (* +speed +speed bend) +pull)))))

(defun land-at (x z)
	; (land-at x z) -> double
	;the land's height there, or the water's where it is under
	(max (n2d -4) (land_height (doubles x z))))

(defun valley-pose (dt)
	; (valley-pose dt) -> (fx fz lean)
	;on along its way, turning to the lower land ahead, left or right, by
	;how much lower, no faster than it may, and easing into a turn as a
	;plane does; kept over the highest land under it and ahead, so it
	;climbs before a ridge. Its lean is speed times how fast it turns
	(bind '(px pz) place)
	(bind '(fx fz) way)
	(defq c (cos +aside) s (sin +aside)
		;the ways ahead a little left, and a little right
		lx (+ (* fx c) (* fz s)) lz (- (* fz c) (* fx s))
		rx (- (* fx c) (* fz s)) rz (+ (* fz c) (* fx s))
		;the land a little left and a little right, at three distances, the
		;nearer the more
		left (+ (* +double_2 (land-at (+ px (* lx +ahead)) (+ pz (* lz +ahead))))
			(land-at (+ px (* lx +ahead +double_2)) (+ pz (* lz +ahead +double_2)))
			(* (n2d 0.5) (land-at (+ px (* lx +ahead (n2d 3))) (+ pz (* lz +ahead (n2d 3))))))
		right (+ (* +double_2 (land-at (+ px (* rx +ahead)) (+ pz (* rz +ahead))))
			(land-at (+ px (* rx +ahead +double_2)) (+ pz (* rz +ahead +double_2)))
			(* (n2d 0.5) (land-at (+ px (* rx +ahead (n2d 3))) (+ pz (* rz +ahead (n2d 3))))))
		;to the left is positive, toward the lower
		target (max (neg +turn_most) (min +turn_most (* +steer (- right left)))))
	(setq turning (+ turning (* (- target turning) (min +double_1 (* dt +turn_ease)))))
	(defq a (* turning dt) ca (cos a) sa (sin a)
		nx (+ (* fx ca) (* fz sa)) nz (- (* fz ca) (* fx sa)))
	(setq way (doubles nx nz)
		place (doubles (+ px (* nx +speed dt)) (+ pz (* nz +speed dt))))
	(bind '(px pz) place)
	(ease-height (+ +clearance (max (land-at px pz) (land-at (+ px (* nx (n2d 60))) (+ pz (* nz (n2d 60))))
			(land-at (+ px (* nx (n2d 120))) (+ pz (* nz (n2d 120))))
			(land-at (+ px (* nx (n2d 240))) (+ pz (* nz (n2d 240)))))) dt)
	(list nx nz (neg (/ (* +speed turning) +pull))))

(defun mouse-pose (dt)
	; (mouse-pose dt) -> (fx fz lean)
	;flown by the pointer: across it turns, up and down it climbs and dives,
	;both eased as the valleys' are, straight and level when it is not over
	;the picture. But never nearer the land under it and ahead than the
	;floor, whatever the pointer says: into a hill it pulls up of itself;
	;and never over the ceiling
	(bind '(px pz) place)
	(bind '(fx fz) way)
	(bind '(sx sy) (ifn stick (list +double_0 +double_0) stick))
	(setq turning (+ turning (* (- (* (neg (beyond-dead sx)) +stick_turn) turning) (min +double_1 (* dt +turn_ease (n2d 2))))))
	(defq a (* turning dt) ca (cos a) sa (sin a)
		nx (+ (* fx ca) (* fz sa)) nz (- (* fz ca) (* fx sa)))
	(setq way (doubles nx nz)
		place (doubles (+ px (* nx +speed dt)) (+ pz (* nz +speed dt))))
	(bind '(px pz) place)
	(defq low (+ +floor (max (land-at px pz) (land-at (+ px (* nx (n2d 40))) (+ pz (* nz (n2d 40))))
			(land-at (+ px (* nx (n2d 90))) (+ pz (* nz (n2d 90))))
			(land-at (+ px (* nx (n2d 180))) (+ pz (* nz (n2d 180))))))
		want (* (beyond-dead sy) +stick_climb))
	(cond
		((< height (+ low (n2d 12))) (setq want (max want (* (- (+ low (n2d 12)) height) (n2d 0.9)))))
		((> height +ceiling) (setq want (min want (* (- +ceiling height) (n2d 0.5))))))
	(rise-toward want dt)
	;the floor itself is never gone under, an easing can not be late for it
	(setq height (max height low))
	(list nx nz (neg (/ (* +speed turning) +pull))))

(import "./fleet.inc")

(defun path-set (to)
	;the flight to another way of flying, from where it is and as it is
	;going, so one smooths into the next: the valleys and the pointer go on
	;from here, turning as it was, by the lean the way of flying wanted,
	;not the one sprung to it, which is behind; the eight is taken up where
	;it crosses itself, straight, turned to lie along the flight's way and
	;moved so that it is here, so the flight goes on ahead onto it
	(defq turn (neg (/ (* lean_want +pull) +speed)))
	(when (eql to :eight)
		;where it crosses itself, straight, the one whose first bend is the
		;way the flight turns, and the eight turned to lie along the flight's
		;way there: the first thing is on, straight ahead
		(bind '(wx wz) way)
		(defq at (if (>= turn +double_0) +double_0 +double_pi)
			tx (* +reach_x (cos at)) tz (* (n2d 2) +reach_z (cos (* (n2d 2) at)))
			tn (sqrt (+ (* tx tx) (* tz tz))) tx (/ tx tn) tz (/ tz tn))
		(setq eight_lie (doubles (+ (* wx tx) (* wz tz)) (- (* wz tx) (* wx tz)))
			round_at at middle (blend place (eight-point at) +double_1 +double_-1) eight_in +eight_onto))
	(setq path to turning turn))

(defun draw-frame ()
	;where the flight is, the way it looks, and the frame: the sky, then the
	;land, a grid moved to under the eye by whole cells. Forward is along
	;its way, looking down a little, and it leans into a turn by its lean,
	;the tangent of the angle, made of its cosine and sine, no angle
	(defq now (pii-time) dt (/ (n2d (- now moved)) (n2d 1000000)))
	(setq moved now)
	(bind '(fx fz lean_to) (case path (:eight (eight-pose dt)) (:valleys (valley-pose dt)) (:t (mouse-pose dt))))
	;the lean, and how the nose is, sprung to what the way of flying wants,
	;so a change of it is a roll and a pitch that gathers, not a kick
	(bind '(l lv) (spring lean lean_v lean_to dt))
	(setq lean l lean_v lv lean_want lean_to)
	;the craft about fly, each by its own rules
	(fleet-fly dt)
	;the nose up a little as it climbs and down as it comes down
	(bind '(p pv) (spring pitch pitch_v (- (if (eql path :eight) +pitch_eight +pitch_valley) (* (n2d 0.5) (/ rising +speed))) dt))
	(setq pitch p pitch_v pv)
	(defq pitch_cos (cos pitch) pitch_sin (sin pitch)
		lean_cos (recip (sqrt (+ +double_1 (* lean lean)))) lean_sin (* lean lean_cos)
		level (doubles (neg fz) +double_0 fx)
		forward (doubles (* fx pitch_cos) (neg pitch_sin) (* fz pitch_cos))
		upward (doubles (* fx pitch_sin) pitch_cos (* fz pitch_sin))
		right (blend level upward lean_cos (neg lean_sin)) up (blend upward level lean_cos lean_sin)
		eye (doubles (first place) height (second place))
		;how it looks, its rows the eye's ways, it looks along minus z
		look (cat right (doubles +double_0) up (doubles +double_0)
			(apply (const doubles) (map (const neg) forward)) (doubles +double_0 +double_0 +double_0 +double_0 +double_1))
		;the frame's width at a distance of 1, of the shape of the canvas
		spread_x (/ (* +spread_y (n2d (first size))) (n2d (second size)))
		lens (mat4x4-frustum (neg spread_x) spread_x +spread_y (neg +spread_y) +near +far)
		view (mat4x4-mul lens (mat4x4-mul look (mat4x4-translate (neg (first eye)) (neg (second eye)) (neg (third eye)))))
		seconds (/ (n2d (- moved began)) (n2d 1000000))
		;the GPU's land, or the CPU's of bigger cells, moved under the eye by
		;whole cells of it
		gpu (eql (. render :mode) :gpu) fine (and gpu (= detail 0)) cell (if fine +cell +cell_cpu)
		shift (doubles (* (n2d (n2i (/ (first eye) cell))) cell) (* (n2d (n2i (/ (third eye) cell))) cell)))
	;the day as it is now, the sun, the sky and the light
	(day-state seconds)
	(. render :frame (cat (list
		(list +mesh_sky '()
			(list (list 'forward forward) (list 'right right) (list 'up up)
				(list 'spread (doubles spread_x +spread_y)) (list 'sun sun_at) (list 'haze haze) (list 'above above)
				(list 'moon moon_way) (list 'night night) (list 'time seconds)
				(list 'starry (if (< detail 4) +double_1 +double_0)))
			'() sky_files)
		;the moon, the eye's way to it the same however far the eye goes, as
		;anything in the sky, turning slowly on its leaning axis
		(list +mesh_moon (list (list 'model (mat4x4-mul (mat4x4-translate
						(+ (first eye) (* (first moon_way) +moon_far)) (+ (second eye) (* (second moon_way) +moon_far))
						(+ (third eye) (* (third moon_way) +moon_far)))
					(mat4x4-mul (mat4x4-scale +moon_size +moon_size +moon_size) (defq spin (mat4x4-mul (mat4x4-rotz +moon_lean)
						(mat4x4-roty (* seconds +moon_turn)))))))
				(list 'spin spin) (list 'lens view))
			(list (list 'sun sun_at) (list 'haze haze) (list 'above above) (list 'rise (second moon_way))
				(list 'bright (+ (n2d 1.5) (* (n2d 3.0) night))))
			(list moon_map) moon_files)
		(list (if fine +mesh_land +mesh_land_cpu) (list (list 'lens view) (list 'shift shift) (list 'eye eye)
				(list 'quick (if fine +double_0 +double_1)))
			(cat (list (list 'sun sun) (list 'sun_way sun_at) (list 'moon_way moon_way)
				(list 'haze haze) (list 'above above) (list 'eye eye)
				(list 'daylight daylight) (list 'shine shine) (list 'night night)
				(list 'time seconds) (list 'rough (if (< detail 2) +double_1 +double_0))
				(list 'shadows (elem-get '(12 6 0 0 0) detail))
				(list 'plain (if (< detail 4) +double_0 +double_1)))
				;the craft about, their shadows and their lights on the land
				(fleet-lights seconds))))
		;the craft about, fleet.inc, the clouds over them, seen through, and
		;last the craft's beams, added to all else
		(fleet-draws view eye seconds)
		(if (< detail 3) (list (list +mesh_cloud (list (list 'lens view) (list 'eye eye))
			(list (list 'sun sun) (list 'haze haze) (list 'daylight daylight) (list 'shine shine)
				(list 'eye eye) (list 'time seconds))
			'() cloud_files)) '())
		(fleet-beams view eye seconds)))
	(++ frames))

(defun set-size (wanted)
	;the window the size of the frame wanted, now; the canvas of it is made
	;when the next frame is given, (new-canvas), and the one there is shown
	;the size till then
	(bind '(w h) wanted)
	(def *backdrop* :min_width w :min_height h :want_width w :want_height h)
	(. *window* :fit)
	(. (ifn shown *canvas*) :change 0 0 w h)
	(. *window* :layout)
	(setq want wanted))

(defun new-canvas ()
	;a canvas of the size that is wanted, if that is not the size there is,
	;drawn on from this frame. It is not put on show till a frame of it is
	;whole, (show-canvas): the one on show stays till then, so nothing is
	;seen that is not a picture
	(unless (eql want size)
		(unless shown (setq shown *canvas*))
		(setq *canvas* (make-canvas want) size want)
		(. render :canvas *canvas*)))

(defun show-canvas ()
	;a frame of the new canvas is whole: it goes on show, and the old one,
	;whose pixels may be shared, is let go of
	(.-> shown :sub :free)
	(. *backdrop* :add_child *canvas*)
	(bind '(w h) size)
	(. *canvas* :change 0 0 w h)
	(. *window* :layout)
	(setq shown :nil))

(defun music-show ()
	;the play button a pause while the music is on, to stop it
	(. (second (. *music* :children)) :set_text
		(num-to-utf8 (if (. music :title) +sym_media_pause +sym_media_play))))

(defun config-default ()
	;the first time
	(scatter (Emap) :version +config_version :path :eight :large :nil :music :t :day 60 :detail 4))

(defun config-load ()
	;the defaults with what was kept laid over them,
	;docs/ai_digest/app_configuration.md
	(setq *config* (config-default))
	(and (defq old (with-read-lock +config_file
				(tree-load (file-stream +config_file))))
		(<= (ifn (. old :find :version) 0) +config_version)
		(. old :each (lambda (key val)
			(unless (eql key :version) (. *config* :insert key val))))))

(defun config-save ()
	(scatter *config* :path path :large (eql want +large) :music music_on :day (n2i day_at) :detail detail)
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))

(defun main ()
	(setq select (task-mboxes +select_size)
		render (Render-tris *canvas* +land_vertex +land_lit))
	(def render :clear (argb haze))
	(. render :mesh +mesh_land (land-verts +cells +cell))
	(. render :mesh +mesh_land_cpu (land-verts +cells_cpu +cell_cpu))
	(. render :mesh +mesh_moon (globe-verts 48 24) (first moon_files))
	;the moon's map, its pixels in shared memory if the host has it, so the
	;nodes that draw a strip each all read the one copy of it
	(setq moon_map (. (shader-strip-picture (canvas-load (cat *app_root* "data/moon.cpm") +load_flag_noswap)) :swap +swap_write))
	(. render :mesh +mesh_sky (sky-verts) (first sky_files))
	(. render :mesh +mesh_beam (tube-verts 24) (first beam_files))
	(. render :mesh +mesh_cloud (sky-verts) (first cloud_files))
	;the craft's parts, made by a farm of children on this machine's
	;nodes, off this one, and each put in its craft as it comes
	(setq jobs (Jobs (cat *app_root* "child.lisp") (elem-get select +select_task)
			(elem-get select +select_reply) '(4 2) :t)
		crafts (list (make-scout) (make-orb) (make-blob) (make-triangle) (make-jelly) (make-top)))
	(. jobs :add (map (lambda ((command flat))
			(cat (setf-> (str-alloc +part_size) (+part_num (!)) (+part_flat (if flat 1 0))) command)) craft_parts))
	(config-load)
	;the day goes on from where it was when last closed, seconds into it
	(setq day_start (n2d (. *config* :find :day))
		;the detail it had, the least the first time, to go up from
		detail (. *config* :find :detail))
	(setq land_height (shader-vp-func (shader-load (cat *app_root* "land.shader")) 'ground)
		place (eight-point round_at) way (doubles +double_1 +double_0))
	(path-set (. *config* :find :path))
	(. *path* :set_selected (find path '(:eight :valleys :mouse)))
	;the size it was closed at
	(if (. *config* :find :large) (set-size +large))
	;the user's Jukebox favourites behind it, if it had them on
	(setq music (Music select +select_music) music_on (. *config* :find :music))
	(if music_on (. music :start))
	(music-show)
	(.-> *canvas* (:fill (argb haze)) (:swap +swap_write))
	(. *window* :open)
	(mail-timeout (elem-get select +select_timer) +timer_rate 0)
	(while id
		;the render's mail is read with the app's, before the timer, and
		;dealt with: a child has started, wants a mesh, or has drawn its
		;part of a frame
		(bind '(idx msg) (. render :read select +select_timer))
		(cond
			((not idx))
			;the music's
			((. music :mail idx msg))
			((= idx +select_task)
				;a child of the farm has started, unless the farm has gone
				(if jobs (. jobs :launched msg)))
			((= idx +select_reply)
				;a part made, its mesh given to the render and the part
				;drawn from the next frame; when all have come the farm goes,
				;and what its children send after that is for no one
				(when (and jobs (defq out (. jobs :answered msg)))
					(defq part (getf msg +part_reply_num))
					(. render :mesh (+ +mesh_craft part) (push (doubles) (slice msg +part_reply_data -1))
						(first craft_files))
					(elem-set parts_had part :t)
					(when (= out 0) (. jobs :close) (setq jobs :nil))))
			((= idx +select_main)
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					((= id +event_max) (set-size +large))
					((= id +event_min) (set-size +small))
					((= id +event_path)
						(path-set (elem-get '(:eight :valleys :mouse) (. *path* :get_selected))))
					;the music: the song before, on or off, the next
					((= id +event_prev) (. music :move -1))
					((= id +event_next) (. music :move 1))
					;the day dragged to another time of it
					((= id +event_day)
						(defq total (+ +day_long +dusk_long +night_long +dawn_long))
						(setq day_dragged (pii-time)
							day_start (- (/ (* (n2d (get :value *day*)) total) (n2d 1000))
								(/ (n2d (- (pii-time) began)) (n2d 1000000)))))
					((= id +event_music)
						(setq music_on (not (. music :title)))
						(if music_on (. music :start) (. music :stop))
						(music-show))
					;the pointer over the picture is the stick, and off it, none
					((and (= (getf msg +ev_msg_target_id) (. *canvas* :get_id))
							(= (getf msg +ev_msg_type) +ev_type_mouse))
						(bind '(w h) size)
						(setq stick (list (- (/ (n2d (* 2 (getf msg +ev_msg_mouse_rx))) (n2d w)) +double_1)
							(- +double_1 (/ (n2d (* 2 (getf msg +ev_msg_mouse_ry))) (n2d h)))))
						(. *window* :event msg))
					((and (= (getf msg +ev_msg_target_id) (. *canvas* :get_id))
							(= (getf msg +ev_msg_type) +ev_type_exit))
						(setq stick :nil)
						(. *window* :event msg))
					((. *window* :event msg))))
			(:t ;the timer, the flight goes on and is drawn, if the last frame
				;is not still out with a farm
				(defq start (pii-time))
				;a new canvas goes on show once a frame of it is whole, before the
				;next frame is given: after it the render is busy again
				(and shown (not (. render :busy)) (show-canvas))
				(unless (. render :busy) (new-canvas) (draw-frame))
				;the next frame is a tick on, or four times as long as this
				;one took here if that is more: never more than a quarter of
				;this node, the apps beside it have the rest
				(defq took (- (pii-time) start))
				(mail-timeout (elem-get select +select_timer)
					(- (max +timer_rate (* took 4)) took) 0)
				(. render :tick)
				(when (>= (- (defq now (pii-time)) since) 1000000)
					(defq rate (/ (* frames 1000000) (- now since)))
					;the detail to what the GPU keeps up with: less, two seconds
					;running slow, a coarser land, fewer steps to the sun, no bump,
					;no clouds, plain water and land and no stars; more, three
					;seconds running fast, not for half a minute after it was
					;less; a step at a time, after the first few
					(cond
						((< (- now began) 3000000) (setq slow 0 fast 0))
						((< rate +rate_least)
							(setq fast 0)
							(and (< detail 4) (>= (++ slow) 2)
								(setq detail (inc detail) slow 0 detail_down now)))
						((>= rate +rate_most)
							(setq slow 0)
							(and (> detail 0) (> (- now detail_down) 30000000) (>= (++ fast) 3)
								(setq detail (dec detail) fast 0)))
						((setq slow 0 fast 0)))
					(. *status* :set_text (cat (str rate) " frames a second, "
							(. render :how) (if (> detail 0) (cat ", detail less " (str detail)) "")
							(if (defq song (. music :title)) (cat ", " song) "")))
					(setq frames 0 since now)
					;a child of the farm that has died or is too slow is
					;started again, and its part made again
					(if jobs (. jobs :refresh 30000000))))))
	(config-save)
	(. music :stop)
	(if jobs (. jobs :close))
	(. render :close)
	(. *window* :close))
