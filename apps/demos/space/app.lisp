(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "lib/math/matrix.inc")
(import "lib/gpu/render_farm.inc")
(import "lib/gpu/shapes.inc")
(import "./sky.inc")
(import "./rock.inc")
(import "./field.inc")
(import "apps/media/jukebox/music.inc")
(import "service/audio/app.inc")

;Space, a voyage in deep space, drawn by a (Render-tris),
;lib/gpu/render_farm.inc, which has the GPU draw it, or on a host with
;none the nodes of this machine, a strip of the frame each. A frame is a
;list of draws, each by its own pair of shaders: the rocks of a field of
;them that the ship flies through, each made by a farm of children and
;slotted in as it comes, rock.inc, its face the moon's; and the sky, deep
;space and its stars, the sun, and the Pillars of Creation with their
;nebula round them, data/README.md. The field is a box of rocks that goes
;on the same every way, so the ship never comes to its end, the eye kept
;in the box and the rocks drawn where they are nearest it. The ship goes
;on ahead, turned by the pointer, or for now of itself, slowly. The
;user's Jukebox favourites play behind it

(enums +event +event_user
	(enum mode prev music next))

;the timer is last, what is first in the list is what is read first, and
;a frame can take longer than a tick
(enums +select 0
	(enum main tip task reply music music_song music_tick timer))

;the frame's two sizes, the min and max buttons
(defq +small ''(640 480) +large ''(1024 768))

(defq +timer_rate (/ 1000000 60)
	;how many times what this task took over a frame till the next, so it
	;has a third of its node at most, the apps beside it the rest
	+pace 3
	;how high the frame is at a distance of 1, and how near and far it sees
	+spread_y (n2d 0.5) +near (n2d 1) +far (n2d 2000)
	;how fast the eye turns round of itself, a second, when the pointer is
	;not over the picture; and with it, the middle of the picture it does
	;nothing in, and how fast at most it turns and pitches for the pointer
	;at an edge, a second
	+drift (n2d 0.04) +dead (n2d 0.1) +stick_rate (n2d 0.9)
	;flying: how fast at most it turns, a second, how soon it comes to the
	;turn it wants, how hard a rock in the way turns it, how far it leans
	;into a turn, and how soon it comes upright again; the auto pilot, how
	;far off a swarm it visits, how long it keeps to one, and how hard it
	;turns for it
	+turn_most (n2d 0.45) +turn_ease (n2d 1) +lean (n2d 0.5) +upright (n2d 0.4)
	+visit_far (n2d 480) +visit_long 20000000
	;how long before a big rock visited may be again, and how often a
	;cluster out of sight is put in orbit again, and since when
	+revisit 90000000 +refresh_every 1000000 refreshed 0
	;how hard it turns for the heading it has chosen, how many headings it
	;tries a frame, and how far the pointer at an edge turns the way wanted
	+head_turn (n2d 1.4) +choose_count 7 +stick_reach (n2d 0.6)
	sky_files (list "lib/gpu/shaders/sky_vertex.shader" (cat *app_root* "sky_lit.shader"))
	rock_files (list (cat *app_root* "rock_vertex.shader") (cat *app_root* "rock_lit.shader"))
	grain_files (list (cat *app_root* "grain_vertex.shader") (cat *app_root* "grain_lit.shader"))
	buoy_files (list (cat *app_root* "rock_vertex.shader") (cat *app_root* "buoy_lit.shader"))
	glow_files (list (cat *app_root* "glow_vertex.shader") (cat *app_root* "glow_lit.shader"))
	;the meshes, by their numbers, the rocks' kinds from +mesh_rock on
	+mesh_sky 1 +mesh_glow 6 +mesh_rock 10
	;the marker buoys: their mesh, after the rocks', whether it has come,
	;how big a buoy is, which is looked at next, how often each flashes at
	;least and at most, a second, how long a flash is, seconds, how
	;bright between, how bright its lamp lights the rocks, and how far a
	;rock looks for one to be lit by
	+mesh_buoy 18 buoy_had :nil +buoy_size (n2d 7) buoy_next 0 +flash_least (n2d 0.25) +flash_most (n2d 1.1)
	+flash_long (n2d 0.25) +flash_dim (n2d 0.12) +buoy_power (n2d 6) +buoy_light_far (n2d 170)
	;how hard a hit is at least to throw grains, and how far off at most
	+fx_least (n2d 1.5) +fx_far (n2d 450)
	;the sprays of grains hits throw out, each (at born throw kind), the
	;meshes of them from +mesh_spray on, how many kinds, grains in each, how
	;big a grain, how long a spray lasts, and how many at most
	sprays (list) +mesh_spray 3 +spray_kinds 3 +spray_grains 64 +grain_size (n2d 0.35)
	+spray_life 2500000 +spray_most 10
	;the ship's speed, a second, the farm that makes the rocks and which
	;kinds have come; the field is field.inc
	+speed (n2d 24) jobs :nil kinds_had (map! (lambda () :nil) '() 0 +rock_kinds)
	;the part of the rocks met this frame, and the grid of this round
	part 0 grid :nil
	;the crunches of rocks hitting, near and far, each at the loudness it
	;was made at, bake_levels.lisp, loudest first, as the audio service has
	;no loudness of its own; the hits that are heard, how hard at least, how
	;far off at most, how near for a near one, and at most how many a
	;frame
	crunches :nil crunch_far :nil +near_levels ''(100 60 35) +far_levels ''(50 30 18 10)
	+hit_least (n2d 0.8) +hear_far (n2d 300) +hear_near (n2d 110) +hear_most 2
	moon_map :nil moon_pixels :nil
	;where the eye is in the box of the field
	eye :nil
	;the pictures, the Pillars and the nebula of all of the sky
	pillars_map :nil nebula_map :nil stars_map :nil pillars_pixels :nil nebula_pixels :nil stars_pixels :nil
	select :nil id :t render :nil frames 0 since (pii-time) began (pii-time)
	;when the last frame was given, how long this task took over it, and
	;when the next tick of the timer is due
	given 0 frame_took 0 due 0
	;the size wanted, the size of the canvas drawn on, and the canvas still
	;on show till a frame of a new one is whole
	want +small size +small shown :nil
	;how much less detail it draws, 0 all of it to 4 least, for a machine
	;too slow for all of it: how many seconds running it has been slow, or
	;fast, what is slow and fast, and when it last went down, not to go up
	;again for a while after. Each rock drawn is work of this task's, so
	;the less detail draws them less far: how far of all the way at each,
	;and whether the bump; and how far it draws now, eased to the level's
	;over seconds, its fade with it, so rocks go into the dark sooner, not
	;at once, and the fade and the furthest now
	detail 4 slow 0 fast 0 +rate_least 50 +rate_most 58 detail_down 0
	+view_of (doubles (n2d 1) (n2d 0.85) (n2d 0.85) (n2d 0.7) (n2d 0.55)) +bump_most 1
	+view_ease (n2d 1.5) view (n2d 0.55) fade_near +fade_near fade_far +fade_far rock_far +rock_far
	;the music, the user's Jukebox favourites, apps/media/jukebox/music.inc,
	;and whether the user wants it, kept whether or not there is any to play
	music :nil music_on :t
	;the way the eye looks and its up, turned as it goes, when the eye was
	;last turned, and where the pointer is over the picture, -1 to 1 across
	;and up, or :nil when it is not
	forward :nil up :nil turned 0 stick :nil
	;how fast it is turning across and up, a second, eased; the big rock it
	;is visiting, and since when
	yaw +double_0 pitch +double_0 visit :nil visit_since 0
	;the big rocks visited, by number, and when, so the tour moves on
	visited (Fmap 31)
	;the ship's ping, a short sharp flash of white light now and then: where
	;it is this frame and as bright as it is, its brightness and the dark
	;between, where it is on the ship, up and back from the eye, and how often
	;and how long, microseconds; and each buoy's lamp this frame, where and as
	;bright as it is
	ping_at :nil ping :nil +ping_flash (doubles (n2d 2.5) (n2d 2.5) (n2d 2.7)) +dark (doubles +double_0 +double_0 +double_0)
	+ping_top (n2d 2.5) +ping_back (n2d 4) +ping_round 1100000 +ping_long 50000
	lamps (list) lamps_near (list)
	;the heading chosen through the rocks, and the choice of the next one
	;being made, a few headings a frame, field.inc
	heading :nil thread :nil
	;the way of flying, :auto or :mouse
	mode :auto
	;the size, the music and the detail, as they were when last closed,
	;(config-save)
	*config* :nil +config_version 1 +config_file (cat (get :home *env*) "space.tre"))

(defun make-canvas (size)
	; (make-canvas size) -> canvas
	;its pixels are in shared memory if the host has it, the nodes of this
	;machine then draw on them, a strip each. Every pixel of a frame is
	;drawn, so it is opaque, and what is behind it is not drawn under each
	;frame; black till the first
	(bind '(w h) size)
	(.-> (ifn (canvas-shared w h 1) (Canvas w h 1))
		(:fill +argb_black)
		(:set_flags +view_flag_opaque +view_flag_opaque)
		(:swap +swap_write)))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Space" (+sym_close +sym_max +sym_min) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		;the way of flying, the auto pilot or the pointer
		(. (ui-radio-bar *mode* (+sym_auto +sym_select)) :connect +event_mode)
		(ui-tool-bar *music* ()
			(ui-buttons (+sym_media_start +sym_media_play +sym_media_end) +event_prev))
		(ui-label *status* (:text "..." :font :font_body)))
	(ui-backdrop *backdrop* (:style :plain :paper +argb_black :min_width (first +small) :min_height (second +small))
		(ui-element *canvas* (make-canvas +small)))
	;the picture's credit, its licence has it seen by all who see it
	(ui-label _ (:text "Pillars of Creation: NASA, ESA/Hubble and the Hubble Heritage Team"
		:font :font_small)))

(ui-tool-tips *mode* '("auto pilot" "mouse"))
(ui-tool-tips *music* '("prev" "play" "next"))

(defun picture (file &optional side)
	; (picture file [side]) -> canvas
	;a picture of the app's data, its pixels in shared memory if the host
	;has it, so the nodes that draw a strip each all read the one copy;
	;rescaled to a square of side pixels if that is given, as a GPU likes
	;a picture best
	(defq canvas (canvas-load (cat *app_root* "data/" file) +load_flag_noswap))
	(when side
		(defq square (Canvas side side 1))
		(. square :resize canvas)
		(setq canvas square))
	(. (shader-strip-picture canvas) :swap +swap_write))

(defun beyond-dead (v)
	; (beyond-dead v) -> double
	;how far past the dead middle a stick is, -1 to 1, 0 in it
	(cond
		((> v +dead) (/ (- v +dead) (const (- +double_1 +dead))))
		((< v (neg +dead)) (/ (+ v +dead) (const (- +double_1 +dead))))
		(+double_0)))

(defun visit-want (right)
	; (visit-want right) -> doubles
	;the way the auto pilot wants to go: toward a big rock it visits, to go
	;just past it, through its swarm, the nearest that is ahead, within 60
	;degrees of its way, near enough, and not visited lately, so the tour
	;moves on and does not go round the one swarm; a new one now and then or
	;when it is behind; with none, a slow wander
	(defq now (pii-time))
	(bind '(ex ey ez) eye)
	(when (or (not visit) (> (- now visit_since) +visit_long)
			(<= (rock-ahead (elem-get rocks visit)) +double_0))
		(setq visit :nil visit_since now)
		(each (lambda (rock)
				(defq far (rock-distance rock) been (. visited :find (!)))
				(and (> (third rock) (const (n2d 30))) (> (rock-ahead rock) (max (const (n2d 80)) (* far +double_1/2)))
					(< far +visit_far) (or (not been) (> (- now been) +revisit))
					(or (not visit) (< far (rock-distance (elem-get rocks visit))))
					(setq visit (!))))
			rocks)
		(if visit (. visited :insert visit now)))
	(cond
		(visit
			;toward a place just beside it, out from it across the ship's way,
			;so the ship goes through the swarm round it
			(bind '(at & bulk kind &ignore) (elem-get rocks visit))
			(defq rx (near-way (- (first at) ex)) ry (near-way (- (second at) ey)) rz (near-way (- (third at) ez))
				side (if (>= (+ (* rx (first right)) (* ry (second right)) (* rz (third right))) +double_0) +double_-1 +double_1)
				aim (sky-unit (nums-add (doubles rx ry rz)
					(nums-scale right (* side (+ (* bulk (elem-get kind_reach kind)) (const (n2d 12))))))))
			aim)
		((sky-unit (nums-add forward (nums-add (nums-scale right +drift)
			(nums-scale up (* (const (n2d 0.05)) (sin (/ (n2d now) (const (n2d 9000000))))))))))))

(defun rock-ahead (rock)
	; (rock-ahead rock) -> double
	;how far ahead of the eye a rock is, along its way, where it is nearest
	(bind '(at &ignore) rock)
	(+ (* (near-way (- (first at) (first eye))) (first forward))
		(* (near-way (- (second at) (second eye))) (second forward))
		(* (near-way (- (third at) (third eye))) (third forward))))

(defun rock-distance (rock)
	; (rock-distance rock) -> double
	(bind '(at &ignore) rock)
	(defq rx (near-way (- (first at) (first eye))) ry (near-way (- (second at) (second eye)))
		rz (near-way (- (third at) (third eye))))
	(sqrt (+ (* rx rx) (* ry ry) (* rz rz))))

(defun turn-eye ()
	; (turn-eye) -> dt
	;the eye turned, across about its up and up and down about its across,
	;toward a heading chosen through the rocks, rocks-thread, round the way
	;the pointer or the auto pilot wants, whichever steers. The turn is eased
	;to what is wanted, and no faster than a ship turns. It leans into a
	;turn, and comes upright again of the galaxy's up. Its ways are made
	;square and of length 1 again, so they do not drift. The seconds since
	;last time
	(defq now (pii-time) dt (min (/ (n2d (- now turned)) (const (n2d 1000000))) (const (n2d 0.1)))
		right (sky-cross forward up))
	(setq turned now)
	;a heading chosen through the rocks round the way wanted, the pointer's,
	;or the auto pilot's, which threads the needle: a choice a few frames
	;long, a few headings a frame, the next begun as one is made
	(unless thread
		(setq thread (thread-start eye +speed
			(if (eql mode :mouse)
				(if stick (sky-unit (nums-add forward (nums-add (nums-scale right (* (beyond-dead (first stick)) +stick_reach))
						(nums-scale up (* (beyond-dead (second stick)) +stick_reach)))))
					forward)
				(visit-want right))
			right up (eql mode :auto))))
	(defq step (thread-step thread +choose_count))
	(if (nums? step) (setq heading step thread :nil) (setq thread step))
	(unless heading (setq heading forward))
	(defq want_yaw (* (nums-dot heading right) +head_turn) want_pitch (* (nums-dot heading up) +head_turn))
	(defq k (min +double_1 (* dt +turn_ease)))
	(setq yaw (+ yaw (* k (- (max (neg +turn_most) (min +turn_most want_yaw)) yaw)))
		pitch (+ pitch (* k (- (max (neg +turn_most) (min +turn_most want_pitch)) pitch))))
	(defq a (* yaw dt) b (* pitch dt)
		f (nums-add (nums-scale forward (cos a)) (nums-scale right (sin a))))
	(setq forward (sky-unit (nums-add (nums-scale f (cos b)) (nums-scale up (sin b))))
		up (sky-unit (nums-sub (nums-scale up (cos b)) (nums-scale f (sin b)))))
	;leaning into the turn, and coming upright of the sky's up, the less the
	;nearer straight up or down it goes, where which way is upright swings
	;round with the least turn, and not at all within 30 degrees of it
	(defq level (nums-sub (const (doubles +double_0 +double_1 +double_0)) (nums-scale forward (second forward)))
		level_by (sqrt (nums-dot level level)))
	(when (> level_by +double_1/2)
		(setq level (nums-scale level (/ +double_1 level_by)))
		(defq lean_to (* yaw +lean) across (sky-cross forward level)
			want_up (nums-add (nums-scale level (cos lean_to)) (nums-scale across (sin lean_to))))
		(setq up (nums-add up (nums-scale (nums-sub want_up up)
			(min +double_1 (* dt +upright +double_2 (- level_by +double_1/2)))))))
	(setq up (sky-unit (sky-cross (sky-cross forward up) forward)))
	dt)

(defun view-sides (right up spread_x)
	; (view-sides right up spread_x) -> (side ...)
	;the four sides of what the frame sees, each the way in, square to a
	;plane through the eye, of length 1: a place is seen if it is not out
	;past any of them
	(map (lambda ((s across)) (sky-unit (nums-add (nums-scale forward s) across)))
		(list (list spread_x (nums-scale right +double_-1)) (list spread_x right)
			(list +spread_y (nums-scale up +double_-1)) (list +spread_y up))))

(defun seen? (rx ry rz reach sides)
	; (seen? rx ry rz reach sides) -> :t | :nil
	;a ball so far from the eye, of so much reach, is it in what the frame
	;sees, any of it
	(every (lambda ((sx sy sz)) (> (+ (* rx sx) (* ry sy) (* rz sz)) (neg reach))) sides))

(defun rock-draws (view sides)
	; (rock-draws view sides) -> draws
	;the rocks near enough and in what the frame sees, any of them, each
	;where it is nearest the eye of all the places it is as the box goes on,
	;turned as far as it has tumbled, as big as it is
	;the pixel shader's inputs the one list for all of them, so packed the
	;once; what is each rock's own goes to the vertex shader, and on
	(defq out (list) pictures (list moon_map nebula_map) pvals (list (list 'sun sun_at) (list 'eye eye) (list 'rough (if (<= detail +bump_most) +double_1 +double_0))
			(list 'sun_light sun_light) (list 'pillars_at pillars_at) (list 'pillars_light pillars_light)
			(list 'far_at far_at) (list 'far_light far_light)
			(list 'fade_near fade_near) (list 'fade_far fade_far)
			(list 'ping_at ping_at) (list 'ping ping)
			(list 'surface_pixels moon_pixels) (list 'nebula_pixels nebula_pixels)))
	(bind '(ex ey ez) eye)
	(each (lambda ((at drift bulk kind spin patch))
			(when (elem-get kinds_had kind)
				(bind '(px py pz) at)
				(defq rx (near-way (- px ex)) ry (near-way (- py ey)) rz (near-way (- pz ez)))
				(when (and (< (- (sqrt (+ (* rx rx) (* ry ry) (* rz rz))) bulk) rock_far)
						(seen? rx ry rz (* bulk (elem-get kind_reach kind)) sides))
					(bind '(& & turn_a turn_b) spin)
					(push out (list (+ +mesh_rock kind)
						(cat (list (list 'model (mat4x4-mul (mat4x4-translate (+ ex rx) (+ ey ry) (+ ez rz))
								(mat4x4-mul (mat4x4-mul (mat4x4-roty turn_a) (mat4x4-rotx turn_b))
									(mat4x4-scale bulk bulk bulk))))
							(list 'lens view) (list 'eye eye) (list 'rock_patch patch) (list 'rock_bulk bulk))
							(rock-buoy (+ ex rx) (+ ey ry) (+ ez rz)))
						pvals pictures rock_files)))))
		rocks)
	out)

(defun crunch-levels (name levels)
	; (crunch-levels name levels) -> :nil | (handle ...)
	;a crunch at each of its loudnesses, percent, 100 the crunch as made,
	;loaded into the audio service; :nil with no audio service
	(defq handles (map (lambda (level)
			(audio-add-rpc (cat *app_root* "data/" name (if (= level 100) "" (cat "_" (str level))) ".wav")))
		levels))
	(if (every (const num?) handles) handles))

(defun hear-hits ()
	;the hits of the rocks since last time that are hard enough heard, the
	;loudest few of those near enough: a near crunch close by, the far one
	;further off, as loud as the hit was hard and the less the further, from
	;the side of the eye it is on. Those not heard are let go of
	(defq right (sky-cross forward up) heard (list))
	(bind '(ex ey ez) eye)
	(each (lambda ((x y z speed))
			(when (> speed +hit_least)
				(defq rx (near-way (- x ex)) ry (near-way (- y ey)) rz (near-way (- z ez))
					d (sqrt (+ (* rx rx) (* ry ry) (* rz rz) +double_1)))
				;near enough to see and ahead of the eye, and hard enough, a spray
				;of grains thrown out, as fast as it was hard
				(when (and (< d +fx_far) (> speed +fx_least)
						(> (+ (* rx (first forward)) (* ry (second forward)) (* rz (third forward))) (const (n2d -10))))
					(push sprays (list (doubles x y z) (pii-time) (+ (const (n2d 1.5)) (* +double_2 speed))
						(random +spray_kinds)))
					(if (> (length sprays) +spray_most) (setq sprays (slice sprays 1 -1))))
				(when (< d +hear_far)
					(defq near (- +double_1 (/ d +hear_far))
						loud (* (min +double_1 (/ speed (const (n2d 4)))) near near))
					(push heard (list loud d (/ (+ (* rx (first right)) (* ry (second right)) (* rz (third right))) d))))))
		hits)
	(setq hits (list))
	;none with no audio service on this node, its sounds have no handles
	(when crunch_far
		(each! (lambda ((loud d side))
				;the copy as loud as the hit is heard, the near ones each of a
				;crunch at random
				(defq levels (if (< d +hear_near) (elem-get crunches (random (length crunches))) crunch_far)
					percent (n2i (* loud (const (n2d 100))))
					at (some (lambda (level) (if (<= level (+ percent (/ level 4))) (!))) (if (< d +hear_near) +near_levels +far_levels)))
				(if at (audio-play-rpc (elem-get levels at) (n2i (* side (const (n2d 255)))))))
			(list (sort heard (lambda (a b) (> (first a) (first b))))) 0 (min +hear_most (length heard)))))

(defun spray-verts ()
	; (spray-verts) -> doubles
	;the grains of a spray, each a little square of two triangles, every
	;corner of it its corner and its grain's way and speed, -1 to 1 each
	;way, a few fast and most slower
	(defq out (cap (* +spray_grains 30) (doubles)))
	(times +spray_grains
		(defq way (nums-scale (rnd-way) (+ (const (n2d 0.2)) (* (const (n2d 0.8)) (rnd) (rnd)))))
		(bind '(wx wy wz) way)
		(each (lambda ((cx cy)) (push out cx cy wx wy wz))
			(const `'((,+double_-1 ,+double_-1) (,+double_1 ,+double_-1) (,+double_1 ,+double_1)
				(,+double_-1 ,+double_-1) (,+double_1 ,+double_1) (,+double_-1 ,+double_1)))))
	out)

(defun spray-draws (view right up sides)
	; (spray-draws view right up sides) -> draws
	;the sprays still going, their grains where the GPU has taken them;
	;those gone let go of
	(defq now (pii-time) out (list))
	(setq sprays (filter (lambda ((& born &ignore)) (< (- now born) +spray_life)) sprays))
	(bind '(ex ey ez) eye)
	(each (lambda ((at born throw kind))
			(defq since (/ (n2d (- now born)) (const (n2d 1000000)))
				rx (near-way (- (first at) ex)) ry (near-way (- (second at) ey)) rz (near-way (- (third at) ez)))
			(if (seen? rx ry rz (+ (* throw since) +double_1) sides)
				(push out (list (+ +mesh_spray kind)
					(list (list 'middle (doubles (+ ex rx) (+ ey ry) (+ ez rz)))
						(list 'seconds since) (list 'throw throw) (list 'grain +grain_size)
						(list 'right right) (list 'up up) (list 'lens view))
					(list (list 'age (/ (* since (const (n2d 1000000))) (n2d +spray_life)))
						(list 'sun_light sun_light) (list 'pillars_light pillars_light))
					'() grain_files))))
		sprays)
	out)

(defun buoy-lamps ()
	;each buoy's lamp now: where it is, nearest the eye, on top of the buoy,
	;and as bright as it is, a flash of its colour or the dim glow between,
	;flashing the faster the greater its hazard, each in its own time
	(defq now (/ (n2d (pii-time)) (const (n2d 1000000))))
	(bind '(ex ey ez) eye)
	(setq lamps (map (lambda ((at & hazard colour &ignore))
			(defq rate (+ +flash_least (* hazard (- +flash_most +flash_least)))
				beat (+ (* now rate) (* (n2d (!)) (const (n2d 0.37))))
				on (< (- beat (floor beat)) (* rate +flash_long)))
			(list (doubles (+ ex (near-way (- (first at) ex))) (+ ey (near-way (- (second at) ey)))
					(+ ez (near-way (- (third at) ez))))
				(nums-scale colour (if on +double_1 +flash_dim))))
		buoys)
		;and those near enough to the eye to light a rock it sees
		lamps_near (filter (lambda ((at &ignore))
				(defq dx (- (first at) ex) dy (- (second at) ey) dz (- (third at) ez))
				(< (+ (* dx dx) (* dy dy) (* dz dz)) (* (+ rock_far +buoy_light_far) (+ rock_far +buoy_light_far))))
			lamps)))

(defun rock-buoy (x y z)
	; (rock-buoy x y z) -> ((rock_lamp_at place) (rock_lamp colour))
	;the lamp of the buoy nearest a rock at x y z, near enough to light it,
	;where it is and as bright as it is, or none: of the lamps near
	;enough to the eye to light a rock it sees
	(defq best :nil best_d +buoy_light_far)
	(each (lambda ((at colour))
			(defq dx (- (first at) x) dy (- (second at) y) dz (- (third at) z)
				d (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
			(when (< d best_d) (setq best_d d best (list at colour))))
		lamps_near)
	(if best
		(list (list 'rock_lamp_at (first best)) (list 'rock_lamp (nums-scale (second best) +buoy_power)))
		(list (list 'rock_lamp_at eye) (list 'rock_lamp +dark))))

(defun buoy-draws (view sides)
	; (buoy-draws view sides) -> draws
	;the buoys near enough and in what the frame sees, each upright where
	;its lamp is, turning slowly, its lamp as bright as it is now
	(defq out (list) turn (/ (n2d (pii-time)) (const (n2d 4000000))))
	(when buoy_had
		(each (lambda ((at colour))
				;its body's middle, below its lamp
				(defq body_at (nums-sub at (doubles +double_0 (* +buoy_size (const (n2d 0.72))) +double_0))
					rx (- (first body_at) (first eye)) ry (- (second body_at) (second eye)) rz (- (third body_at) (third eye)))
				(and (< (sqrt (+ (* rx rx) (* ry ry) (* rz rz))) rock_far) (seen? rx ry rz +buoy_size sides)
					(push out (list +mesh_buoy
						(list (list 'model (mat4x4-mul (apply (const mat4x4-translate) (map (# %0) body_at))
								(mat4x4-mul (mat4x4-roty (+ turn (n2d (!)))) (mat4x4-scale +buoy_size +buoy_size +buoy_size))))
							(list 'lens view) (list 'eye eye))
						(list (list 'sun sun_at) (list 'eye eye) (list 'sun_light sun_light) (list 'pillars_at pillars_at)
							(list 'pillars_light pillars_light) (list 'nebula_pixels nebula_pixels) (list 'lamp colour))
						(list nebula_map) buoy_files))))
			lamps))
	out)

(defun glow-draws (view right up sides)
	; (glow-draws view right up sides) -> draws
	;the glow round each buoy's lamp in what the frame sees, the bigger and
	;brighter as it flashes, added to what is behind
	(defq out (list))
	(each (lambda ((at colour))
			(defq rx (- (first at) (first eye)) ry (- (second at) (second eye)) rz (- (third at) (third eye))
				size (* +buoy_size (+ (const (n2d 0.6)) (* (const (n2d 1.4)) (reduce (const max) colour +double_0)))))
			(and (< (sqrt (+ (* rx rx) (* ry ry) (* rz rz))) rock_far) (seen? rx ry rz size sides)
				(push out (list +mesh_glow
					(list (list 'middle at) (list 'size size) (list 'right right) (list 'up up) (list 'lens view))
					(list (list 'lamp colour)) '() glow_files))))
		lamps)
	out)

;the frame log, for tuning, on with +frame_log: each second its rate and
;detail, the milliseconds a frame of each part of it and of the GPU, and
;the rocks drawn, to tests/scratch/space/frames.log. For the Test user, a
;session of a test's, the app quits once settled after its third second
;under 50, to look at the log of it. The parts' times, when the last part
;ended, the rocks drawn, the stream, when it began and the slow seconds
(defq +frame_log :nil frame_parts (list 0 0 0 0 0 0 0) frame_at 0 frame_rocks 0
	frame_stream :nil frame_began 0 frame_slow 0)

(defun frame-mark (part)
	;a part of the frame ended, for the frame log
	(when +frame_log
		(defq now (pii-time))
		(elem-set frame_parts part (+ (elem-get frame_parts part) (- now frame_at)))
		(setq frame_at now)))

(defun frame-log (rate now)
	; (frame-log rate now) -> :t | :nil
	;a second's line of the frame log, the frames of it frames; :t to quit
	(unless frame_stream
		(setq frame_stream (file-stream "tests/scratch/space/frames.log" +file_open_write) frame_began now))
	(defq ms (map (lambda (us) (defq a (/ us (max frames 1))) (cat (str (/ a 1000)) "." (str (/ (% a 1000) 100))))
			(push (cat frame_parts) (get :took render))))
	(write-line frame_stream (cat (str (/ (- now frame_began) 1000000)) "s " (str rate) " fps detail " (str detail)
		" | eye " (elem-get ms 0) " move " (elem-get ms 1) " meet " (elem-get ms 2) " hits " (elem-get ms 3)
		" buoys " (elem-get ms 4) " draws " (elem-get ms 5) " render " (elem-get ms 6)
		" gpu " (elem-get ms 7) " rocks " (str (/ frame_rocks (max frames 1)))))
	(setq frame_parts (list 0 0 0 0 0 0 0) frame_rocks 0)
	(stream-flush frame_stream)
	(when (and (eql (get '*env_node_user*) "Test") (> (- now frame_began) 20000000) (< rate 50) (>= (++ frame_slow) 3))
		(write-line frame_stream "QUIT")
		(stream-flush frame_stream)
		:t))

(defun draw-frame ()
	;the eye turned and gone on, kept in the box of the field, the way it
	;looks, and the frame of it, the rocks and the sky behind them
	(if +frame_log (setq frame_at (pii-time)))
	(defq dt (turn-eye))
	;how far it draws, eased to the level's
	(setq view (+ view (* (- (elem-get +view_of detail) view) (min +double_1 (/ dt +view_ease))))
		fade_near (* +fade_near view) fade_far (* +fade_far view) rock_far (* +rock_far view))
	(frame-mark 0)
	;the rocks on, and a part of them moved by each other, a round of all
	;the parts as many frames, the grid made at the start of each round
	(rocks-move dt)
	(frame-mark 1)
	(if (= part 0) (setq grid (rocks-grid)))
	(rocks-meet (* (min dt (const (n2d 0.05))) (n2d +meet_parts)) grid part)
	(frame-mark 2)
	(setq part (% (inc part) +meet_parts))
	;now and then a cluster out of sight made lively again
	(when (>= (- (pii-time) refreshed) +refresh_every)
		(setq refreshed (pii-time))
		(clusters-refresh eye (+ +rock_far (const (n2d 60)))))
	(hear-hits)
	(frame-mark 3)
	(setq eye (apply (const doubles) (map (lambda (e f)
		(defq v (+ e (* f +speed dt)))
		(cond ((< v +double_0) (+ v +field)) ((>= v +field) (- v +field)) (v))) eye forward)))
	(defq right (sky-cross forward up)
		;the frame's width at a distance of 1, of the shape of the canvas
		spread_x (/ (* +spread_y (n2d (first size))) (n2d (second size)))
		;how it looks, its rows the eye's ways, it looks along minus z
		look (cat right (const (doubles +double_0)) up (const (doubles +double_0))
			(nums-scale forward +double_-1) (const (doubles +double_0 +double_0 +double_0 +double_0 +double_1)))
		lens (mat4x4-frustum (neg spread_x) spread_x +spread_y (neg +spread_y) +near +far)
		view (mat4x4-mul lens (mat4x4-mul look (mat4x4-translate (neg (first eye)) (neg (second eye)) (neg (third eye))))))
	(defq sides (view-sides right up spread_x))
	;the ship's ping where it is now, and as bright as it is
	(setq ping_at (nums-add eye (nums-add (nums-scale up +ping_top) (nums-scale forward (neg +ping_back))))
		ping (if (< (% (pii-time) +ping_round) +ping_long) +ping_flash +dark))
	;the buoys on, one of them looked at, and each one's lamp now
	(buoys-move dt)
	(buoy-look buoy_next)
	(setq buoy_next (% (inc buoy_next) +buoys))
	(buoy-lamps)
	(frame-mark 4)
	(defq rd (rock-draws view sides) bd (buoy-draws view sides))
	(if +frame_log (setq frame_rocks (+ frame_rocks (length rd))))
	(frame-mark 5)
	(. render :frame (cat rd bd (list
		(list +mesh_sky '()
			(list (list 'forward forward) (list 'right right) (list 'up up)
				(list 'spread (doubles spread_x +spread_y)) (list 'sun sun_at)
				(list 'pillars_at pillars_at) (list 'pillars_across pillars_across)
				(list 'pillars_up pillars_up) (list 'pillars_size pillars_size)
				(list 'pillars_pixels pillars_pixels) (list 'nebula_pixels nebula_pixels)
				(list 'stars_pixels stars_pixels))
			(list pillars_map nebula_map stars_map) sky_files))
		;the buoys' lamps' glows and the grains hits throw out, over what is
		;behind
		(glow-draws view right up sides) (spray-draws view right up sides)))
	(frame-mark 6)
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
	;whole, (show-canvas): the one on show stays till then
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
	(scatter (Emap) :version +config_version :mode :auto :large :nil :music :t :detail 4))

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
	(scatter *config* :mode mode :large (eql want +large) :music music_on :detail detail)
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))

(defun next-frame ()
	;the next frame, if the last is whole. A new canvas goes on show once a
	;frame of it is whole, before the next is given. When it was given, and
	;how long this task took over it, are kept
	(unless (. render :busy)
		(defq start (pii-time))
		(if shown (show-canvas))
		(new-canvas)
		(draw-frame)
		(setq given start frame_took (- (pii-time) start))))

(defun detail-fit (rate now)
	;the detail to what the GPU keeps up with, by the rate of the second
	;gone: less, two seconds running slow; more, three seconds running fast,
	;not for half a minute after it was less; a step at a time, after the
	;first few seconds
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
		((setq slow 0 fast 0))))

(defun main ()
	(setq select (task-mboxes +select_size)
		render (Render-tris *canvas* (first sky_files) (second sky_files)))
	(def render :clear +argb_black)
	(. render :mesh +mesh_sky (sky-verts) (first sky_files))
	(. render :mesh +mesh_glow (sky-verts) (first glow_files))
	(each! (lambda () (. render :mesh (+ +mesh_spray (!)) (spray-verts) (first grain_files))) '() 0 +spray_kinds)
	(setq pillars_map (picture "pillars.cpm") nebula_map (picture "nebula.cpm")
		pillars_pixels (apply (const doubles) (map (const n2d) (. pillars_map :pref_size)))
		nebula_pixels (apply (const doubles) (map (const n2d) (. nebula_map :pref_size)))
		stars_map (picture "stars.cpm")
		stars_pixels (apply (const doubles) (map (const n2d) (. stars_map :pref_size)))
		;the moon's map, Flight's, for the rocks' faces
		moon_map (. (shader-strip-picture (canvas-load "apps/demos/flight/data/moon.cpm" +load_flag_noswap)) :swap +swap_write)
		moon_pixels (apply (const doubles) (map (const n2d) (. moon_map :pref_size)))
		rocks (make-rocks) eye (doubles +double_0 +double_0 +double_0)
		;the crunches, the near ones and the far one, each at its loudnesses
		crunches (map (lambda (n) (crunch-levels (cat "crunch_" (str n)) +near_levels)) '(1 2 3))
		crunch_far (crunch-levels "crunch_far" +far_levels))
	;the rocks' kinds, made by a farm of children on this machine's nodes,
	;off this one, and each slotted in as it comes
	(setq jobs (Jobs (cat *app_root* "child.lisp") (elem-get select +select_task)
		(elem-get select +select_reply) '(4 2) :t))
	(. jobs :add (push (map! (lambda ()
			(cat (setf-> (str-alloc +rock_size) (+rock_num (!))) (str (+ 1000 (* (!) 7919)))))
		'() 0 +rock_kinds)
		;and the buoy, the kind after them
		(cat (setf-> (str-alloc +rock_size) (+rock_num +rock_kinds)) "buoy")))
	(setq buoys (make-buoys))
	(config-load)
	(setq detail (min 4 (. *config* :find :detail)) view (elem-get +view_of detail) mode (. *config* :find :mode)
		;the Pillars ahead, level, at first
		forward (sky-unit (doubles (first pillars_at) +double_0 (third pillars_at)))
		up (doubles +double_0 +double_1 +double_0) turned (pii-time))
	(. *mode* :set_selected (find mode '(:auto :mouse)))
	;the size it was closed at
	(if (. *config* :find :large) (set-size +large))
	;the user's Jukebox favourites behind it, if it had them on
	(setq music (Music select +select_music) music_on (. *config* :find :music))
	(if music_on (. music :start))
	(music-show)
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	;the tool tips come to a mailbox of their own
	(def *window* :tip_mbox (elem-get select +select_tip))
	(. *window* :open)
	(setq due (+ (pii-time) +timer_rate))
	(mail-timeout (elem-get select +select_timer) +timer_rate 0)
	(while id
		;the render's mail is read with the app's, before the timer, and
		;dealt with: a child has started, wants a mesh, or has drawn its
		;part of a frame
		(bind '(idx msg) (. render :read select +select_timer))
		(cond
			((not idx)
				;a frame that was out is whole: the next at once if it took a
				;tick or more, and +pace times what this task took over it
				(and (not (. render :busy)) (>= (- (pii-time) given) (max +timer_rate (* frame_took +pace)))
					(next-frame)))
			;the music's
			((. music :mail idx msg))
			((= idx +select_task)
				;a child of the farm has started, unless the farm has gone
				(if jobs (. jobs :launched msg)))
			((= idx +select_reply)
				;a kind of rock made, its mesh given to the render and its rocks
				;drawn from the next frame; when all have come the farm goes
				(when (and jobs (defq out (. jobs :answered msg)))
					(defq kind (getf msg +rock_reply_num))
					(. render :mesh (+ +mesh_rock kind) (push (doubles) (slice msg +rock_reply_data -1))
						(first rock_files))
					(cond
						((= kind +rock_kinds) (setq buoy_had :t))
						(:t (elem-set kinds_had kind :t)
							;its bound, as far as it reaches from its middle as it turns
							(bind '(middle radius) (vector-bounds-sphere (push (doubles) (slice msg +rock_reply_data -1)) 7))
							(elem-set kind_reach kind (+ radius (vector-length middle)))))
					(when (= out 0) (. jobs :close) (setq jobs :nil))))
			;a tip of a button the pointer has stopped over
			((= idx +select_tip) (. *window* :tip msg))
			((= idx +select_main)
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					((= id +event_max) (set-size +large))
					((= id +event_mode)
						(setq mode (elem-get '(:auto :mouse) (. *mode* :get_selected))))
					((= id +event_min) (set-size +small))
					;the music: the song before, on or off, the next
					((= id +event_prev) (. music :move -1))
					((= id +event_next) (. music :move 1))
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
			(:t ;the timer, a frame a tick, if the last frame is not still out
				(defq start (pii-time))
				(next-frame)
				;the next tick is a tick on from when this one was due, or +pace
				;times as long as this one took here if that is more
				(defq took (- (pii-time) start))
				(setq due (max (+ due (max +timer_rate (* took +pace))) (pii-time)))
				(mail-timeout (elem-get select +select_timer) (max 1 (- due (pii-time))) 0)
				(. render :tick)
				(when (>= (- (defq now (pii-time)) since) 1000000)
					(defq rate (/ (* frames 1000000) (- now since)))
					(detail-fit rate now)
					(. *status* :set_text (cat (str rate) " frames a second, "
							(. render :how) (if (> detail 0) (cat ", detail less " (str detail)) "")
							(if (defq song (. music :title)) (cat ", " song) "")))
					(and +frame_log (frame-log rate now) (setq id :nil))
					(setq frames 0 since now)))))
	(config-save)
	(. music :stop)
	(if jobs (. jobs :close))
	(if crunch_far (each (lambda (levels) (each (const audio-remove-rpc) levels)) (push (cat crunches) crunch_far)))
	(. render :close)
	(. *window* :close))
