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
(import "lib/audio/wav.inc")

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
	+spread_y +double_1/2 +near +double_1 +far (n2d 2000)
	;how fast the eye turns round of itself, a second, when the pointer is
	;not over the picture; and with it, the middle of the picture it does
	;nothing in, and how fast at most it turns and pitches for the pointer
	;at an edge, a second
	+drift (n2d 0.04) +dead +double_1/10 +stick_rate (n2d 0.9)
	;flying: how fast at most it turns, a second, how soon it comes to the
	;turn it wants, how hard a rock in the way turns it, how far it leans
	;into a turn, and how soon it comes upright again; the auto pilot, how
	;far off a swarm it visits, how long it keeps to one, and how hard it
	;turns for it
	+turn_most (n2d 0.45) +turn_ease +double_1 +lean +double_1/2 +upright (n2d 0.4)
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
	missile_files (list (cat *app_root* "rock_vertex.shader") (cat *app_root* "missile_lit.shader"))
	glow_files (list (cat *app_root* "glow_vertex.shader") (cat *app_root* "glow_lit.shader"))
	;the meshes, by their numbers, the rocks' kinds from +mesh_rock on
	+mesh_sky 1 +mesh_glow 6 +mesh_rock 10
	;the marker buoys: their mesh, after the rocks', whether it has come,
	;how big a buoy is, which is looked at next, how often each flashes at
	;least and at most, a second, how long a flash is, seconds, how
	;bright between, how bright its lamp lights the rocks, and how far a
	;rock looks for one to be lit by
	+mesh_buoy 18 +mesh_missile 19 missile_had :nil +missile_size +double_3 buoy_had :nil +buoy_size +double_7 buoy_next 0 +flash_least +double_1/4 +flash_most (n2d 1.1)
	+flash_long +double_1/4 +flash_dim (n2d 0.12) +buoy_power +double_6 +buoy_light_far (n2d 170)
	;how hard a hit is at least to throw grains, and how far off at most
	+fx_least (n2d 1.5) +fx_far (n2d 450)
	;the sprays of grains hits throw out, each (at born throw kind grain),
	;how big its grains, a hit's or a shattered rock's debris; the meshes of
	;them from +mesh_spray on, how many kinds, grains in each, how big a
	;hit's grain, how long a spray lasts, and how many at most
	sprays (list) +mesh_spray 3 +spray_kinds 3 +spray_grains 64 +grain_size (n2d 0.35)
	;bombs, a click fires one: each (at way born), where it is, how fast
	;which way, and when it was fired; how fast it goes out from the ship,
	;how long its fuse, how many may be out, and its light; blasts, each
	;(at born front), how far its front has gone, how long its light lasts,
	;its light at its brightest, and its glow's size at its least and most;
	;the lights of both this frame, which light a rock before any buoy's,
	;and their glows; and the mouse buttons as they were
	bombs (list) +bomb_speed (n2d 160) +bomb_fuse 1600000 +bombs_most 3
	+bomb_light (doubles (n2d 0.6) (n2d 0.5) (n2d 0.35))
	;a bomb's glow, as much bigger as it is further, and its trail: how many
	;glows behind it, seconds of its way apart, each the fainter
	+bomb_glow_far (n2d 0.02) +trail_glows 6 +trail_step (n2d 0.012) +trail_fade (n2d 0.62)
	blasts (list) +blast_life 900000 +blast_light (doubles (n2d 2.6) (n2d 1.8) (n2d 1.0))
	+blast_glow_least (n2d 15) +blast_glow_most (n2d 150) +blast_light_far (n2d 260)
	;a buoy gone nuclear: the klaxon it sounds as it is armed, its handle
	;of the audio service, how long it flashes white, armed, before, and how
	;fast; its blast's reach, strength, how long its light lasts, its light
	;at its brightest and its glow's size at its most; how many of the rocks
	;it shatters in view throw out debris, and how big its pieces are at
	;least, and for how far off
	klaxon :nil +nuke_fuse 3000000 +armed_beat 125000 +armed_flash (doubles (n2d 1.6) (n2d 1.6) (n2d 1.6))
	+nuke_reach (n2d 360) +nuke_strength (n2d 1200) +nuke_life 2500000
	+nuke_light (doubles (n2d 7) (n2d 5.5) (n2d 4)) +nuke_glow (n2d 500) +nuke_sprays 14
	+debris_least +double_1/2 +debris_far (n2d 0.005) blast_lamps (list) blast_glows (list) pressed 0
	;the ship's speed eased to a boost's while the right button is held and
	;back to cruising after: whether it is held, the boost's speed, and how
	;fast it eases up and down, a second; and the buttons' bits
	;the boost's roar: its loop's samples, data/thrust.wav, the stream it
	;is played on, where in the loop it is, its rate, how far ahead of what
	;is heard it is kept, how loud at most, how loud now, eased up as the
	;right button is held and dying away as it is let go, how fast up, a
	;second, and how fast it dies, a share a second, as a sound does
	thrust_pcm :nil thrust_stream :nil thrust_at 0 +thrust_rate 22050 +thrust_ahead 1100
	+thrust_loud (n2d 0.8) thrust_gain +double_0 +thrust_up +double_6 +thrust_down (n2d 1.7)
	boosting :nil +boost (n2d 60) +boost_up (n2d 1.2) +boost_down (n2d 0.8)
	+left_button 1 +right_button 4
	+spray_life 2500000 +spray_most 24
	;the ship's cruising speed, a second, and its speed now, the farm that
	;makes the rocks and which kinds have come; the field is field.inc
	+cruise (n2d 24) ship_speed +cruise jobs :nil kinds_had (map! (lambda () :nil) '() 0 +rock_kinds)
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
	;at once, and the furthest now. A big rock is drawn further, as it is
	;more of the frame, rock-view, all the way from +bulk_full across
	detail 4 slow 0 fast 0 +rate_least 50 +rate_most 58 detail_down 0
	+view_of (doubles (n2d 1) (n2d 0.85) (n2d 0.85) (n2d 0.7) (n2d 0.55)) +bump_most 1
	+view_ease (n2d 1.5) draw_share (n2d 0.55) rock_far +rock_far +bulk_full (n2d 40)
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
	+ping_top (n2d 2.5) +ping_back +double_4 +ping_round 1100000 +ping_long 50000
	lamps (list) lamps_near (list)
	;each buoy's way up, from its lamp to its float, kept toward the ship's
	;side: the least it may be toward the ship, the dot of the two, and how
	;fast it turns when it must, a second
	buoy_axes (list) +face_least (n2d 0.3) +face_rate (n2d 1.2)
	;the heading chosen through the rocks, and the choice of the next one
	;being made, a few headings a frame, field.inc
	heading :nil thread :nil
	;the way of flying, :auto or :mouse
	mode :auto
	;the size, the music and the detail, as they were when last closed,
	;(config-save)
	*config* :nil +config_version 1 +config_file (cat (get :home *env*) "space.tre"))

;the numbers of the functions below: a second, of (pii-time); the least
;size of a rock the tour seeks out, the least it is ahead, and how far to
;its side it is passed; the bob of the eye on the tour and how long it
;takes; the longest step of time a frame; the least a crunch throws its
;grains; a crunch's loudness as a percent; the most a sound is to one
;side; a spray grain's least way out and the spread past it; how far
;apart in their flashing the buoys are; the thrust's least loudness that
;is heard; how far past the rocks drawn is out of sight; a shattered
;rock's spread of throw; how far behind a missile its nozzle is, the least
;size of its glow, how its trail's glows shrink, and how bright a blast's
;glow is; the dot of a buoy's way up that is straight down; how long a
;buoy takes to turn round; where its body is along it; the least size of
;its glow and how it grows with its light; the longest step of the rocks'
;meeting; how far behind the eye a crunch is sprayed, how fast a meeting
;is heard as loud as it goes, how fast a blast is heard to crunch, the
;least throw of a shattered rock, how far ahead of the eye a missile is
;fired from, and how bright a missile's glow is
(defq +second (n2d 1000000) +visit_bulk (n2d 30) +visit_ahead (n2d 80) +visit_side (n2d 12)
	+bob +double_1/20 +bob_long (n2d 9000000) +dt_most +double_1/10 +spray_throw_least (n2d 1.5)
	+percent (n2d 100) +pan_most (n2d 255) +grain_least +double_1/5 +grain_spread (n2d 0.8)
	+beat_apart (n2d 0.37) +thrust_quiet (n2d 0.01) +out_of_sight (n2d 60) +shatter_throw (n2d 14)
	+nozzle_back (n2d 0.8) +bomb_glow_least (n2d 1.5) +trail_shrink (n2d 0.85)
	+blast_glow_bright (n2d 1.5) +tilt_flip (n2d -0.999) +buoy_turn_long (n2d 4000000)
	+buoy_body (n2d 0.72) +glow_least (n2d 0.6) +glow_grow (n2d 1.4) +meet_dt_most +double_1/20
	+behind_most +double_-10 +loud_speed +double_4 +blast_crunch +double_8
	+shatter_throw_least +double_8 +missile_ahead +double_4 +bomb_bright +double_5)

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
				(and (> (third rock) +visit_bulk) (> (rock-ahead rock) (max +visit_ahead (* far +double_1/2)))
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
					(nums-scale right (* side (+ (* bulk (elem-get kind_reach kind)) +visit_side))))))
			aim)
		((sky-unit (nums-add forward (nums-add (nums-scale right +drift)
			(nums-scale up (* +bob (sin (/ (n2d now) +bob_long))))))))))

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
	(defq now (pii-time) dt (min (/ (n2d (- now turned)) +second) +dt_most)
		right (sky-cross forward up))
	(setq turned now)
	;a heading chosen through the rocks round the way wanted, the pointer's,
	;or the auto pilot's, which threads the needle: a choice a few frames
	;long, a few headings a frame, the next begun as one is made
	(unless thread
		(setq thread (thread-start eye ship_speed
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

(defun rock-view (bulk)
	; (rock-view bulk) -> double
	;how much of the way a rock of a size is drawn: with less detail,
	;draw_share of it, but a big one more, as it is more of the frame, all
	;of it from +bulk_full across, as the rocks' shader fades them
	(min +double_1 (+ draw_share (* (- +double_1 draw_share) (/ bulk +bulk_full)))))

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
			(list 'fade_near +fade_near) (list 'fade_far +fade_far) (list 'draw_share draw_share)
			(list 'ping_at ping_at) (list 'ping ping)
			(list 'surface_pixels moon_pixels) (list 'nebula_pixels nebula_pixels)))
	(bind '(ex ey ez) eye)
	(each (lambda ((at drift bulk kind spin patch &ignore))
			(when (elem-get kinds_had kind)
				(bind '(px py pz) at)
				(defq rx (near-way (- px ex)) ry (near-way (- py ey)) rz (near-way (- pz ez)))
				(and (< (- (sqrt (+ (* rx rx) (* ry ry) (* rz rz))) bulk) (* +rock_far (rock-view bulk)))
					(seen? rx ry rz (* bulk (elem-get kind_reach kind)) sides)
					(push out (list (+ +mesh_rock kind)
						(cat (list (list 'model (mat4x4-mul (mat4x4-translate (+ ex rx) (+ ey ry) (+ ez rz))
								(mat4x4-mul (rock-turned spin) (mat4x4-scale bulk bulk bulk))))
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
						(> (+ (* rx (first forward)) (* ry (second forward)) (* rz (third forward))) +behind_most))
					(push sprays (list (doubles x y z) (pii-time) (+ +spray_throw_least (* +double_2 speed))
						(random +spray_kinds) +grain_size))
					(if (> (length sprays) +spray_most) (setq sprays (slice sprays 1 -1))))
				(when (< d +hear_far)
					(defq near (- +double_1 (/ d +hear_far))
						loud (* (min +double_1 (/ speed +loud_speed)) near near))
					(push heard (list loud d (/ (+ (* rx (first right)) (* ry (second right)) (* rz (third right))) d))))))
		hits)
	(setq hits (list))
	;none with no audio service on this node, its sounds have no handles
	(when crunch_far
		(each! (lambda ((loud d side))
				;the copy as loud as the hit is heard, the near ones each of a
				;crunch at random
				(defq levels (if (< d +hear_near) (elem-get crunches (random (length crunches))) crunch_far)
					percent (n2i (* loud +percent))
					at (some (lambda (level) (if (<= level (+ percent (/ level 4))) (!))) (if (< d +hear_near) +near_levels +far_levels)))
				(if at (audio-play-rpc (elem-get levels at) (n2i (* side +pan_most)))))
			(list (sort heard (lambda (a b) (> (first a) (first b))))) 0 (min +hear_most (length heard)))))

(defun spray-verts ()
	; (spray-verts) -> doubles
	;the grains of a spray, each a little square of two triangles, every
	;corner of it its corner and its grain's way and speed, -1 to 1 each
	;way, a few fast and most slower
	(defq out (cap (* +spray_grains 30) (doubles)))
	(times +spray_grains
		(defq way (nums-scale (rnd-way) (+ +grain_least (* +grain_spread (rnd) (rnd)))))
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
	(each (lambda ((at born throw kind grain))
			(defq since (/ (n2d (- now born)) +second)
				rx (near-way (- (first at) ex)) ry (near-way (- (second at) ey)) rz (near-way (- (third at) ez)))
			(if (seen? rx ry rz (+ (* throw since) +double_1) sides)
				(push out (list (+ +mesh_spray kind)
					(list (list 'middle (doubles (+ ex rx) (+ ey ry) (+ ez rz)))
						(list 'seconds since) (list 'throw throw) (list 'grain grain)
						(list 'right right) (list 'up up) (list 'lens view))
					(list (list 'age (/ (* since +second) (n2d +spray_life)))
						(list 'sun_light sun_light) (list 'pillars_light pillars_light))
					'() grain_files))))
		sprays)
	out)

(defun buoy-lamps ()
	;each buoy's lamp now: where it is, nearest the eye, on top of the buoy,
	;and as bright as it is, a flash of its colour or the dim glow between,
	;flashing the faster the greater its hazard, each in its own time
	(defq now (/ (n2d (pii-time)) +second))
	(bind '(ex ey ez) eye)
	(setq lamps (map (lambda ((at & hazard colour & & & armed))
			(defq rate (+ +flash_least (* hazard (- +flash_most +flash_least)))
				beat (+ (* now rate) (* (n2d (!)) +beat_apart))
				on (< (- beat (floor beat)) (* rate +flash_long)))
			(list (doubles (+ ex (near-way (- (first at) ex))) (+ ey (near-way (- (second at) ey)))
					(+ ez (near-way (- (third at) ez))))
				;one armed to go nuclear flashes white, fast
				(if (> armed 0)
					(if (< (% (- (pii-time) armed) +armed_beat) (/ +armed_beat 2)) +armed_flash (nums-scale +armed_flash +flash_dim))
					(nums-scale colour (if on +double_1 +flash_dim)))))
		buoys)
		;and those near enough to the eye to light a rock it sees
		lamps_near (filter (lambda ((at &ignore))
				(defq dx (- (first at) ex) dy (- (second at) ey) dz (- (third at) ez))
				(< (+ (* dx dx) (* dy dy) (* dz dz)) (* (+ rock_far +buoy_light_far) (+ rock_far +buoy_light_far))))
			lamps)))

(defun thrust-load ()
	; (thrust-load) -> :nil | doubles
	;the boost's roar, its loop's samples read as numbers, :nil if not
	(when (defq in (file-stream (cat *app_root* "data/thrust.wav")))
		(bind '(rate channels bits bytes) (WAV-info in))
		(and (= rate +thrust_rate) (= channels 1) (= bits 16) (defq data (read-blk in bytes))
			(nums-convert (push (shorts) data) (doubles) :t))))

(defun thrust-feed (dt)
	;the roar as the right button is held, quickly up, and as it is let go
	;dying away as a sound does, the less the quieter, a tail of a second
	;or so, from a place at random in its loop each boost:
	;written to its stream ahead of what is heard, between +thrust_ahead and
	;twice that, none when it has died away. A stream not written to for a
	;while is let go of by the audio service, and is opened again
	(and boosting (< thrust_gain +thrust_quiet) thrust_pcm
		(setq thrust_at (random (length thrust_pcm))))
	(setq thrust_gain (if boosting
		(min +double_1 (+ thrust_gain (* dt +thrust_up)))
		(* thrust_gain (- +double_1 (min +double_1 (* dt +thrust_down))))))
	(defq loud thrust_gain)
	(when (and thrust_pcm (> loud +thrust_quiet))
		(unless thrust_stream (setq thrust_stream (audio-stream-open-rpc +thrust_rate 1)))
		(when thrust_stream
			(cond
				((not (defq left (audio-stream-left-rpc thrust_stream))) (setq thrust_stream :nil))
				((< left +thrust_ahead)
					(defq count (- (* 2 +thrust_ahead) left) long (length thrust_pcm) end (+ thrust_at count)
						part (if (<= end long) (slice thrust_pcm thrust_at end)
							(cat (slice thrust_pcm thrust_at -1) (slice thrust_pcm 0 (- end long))))
						pcm (nums-convert (nums-scale part (* +thrust_loud loud loud)) (shorts))
						bytes (* (length pcm) 2))
					(setq thrust_at (% end long))
					(audio-stream-write-rpc thrust_stream (obj-set (str-alloc bytes) +str_data bytes pcm)))))))

(defun in-view? (at sides)
	; (in-view? at sides) -> :t | :nil
	;is a place near enough to be drawn, and in what the frame sees
	(defq rx (near-way (- (first at) (first eye))) ry (near-way (- (second at) (second eye)))
		rz (near-way (- (third at) (third eye))))
	(and (< (+ (* rx rx) (* ry ry) (* rz rz)) (* rock_far rock_far)) (seen? rx ry rz +buoy_size sides)))

(defun side-of (at)
	; (side-of at) -> num
	;the pan of a sound at a place, -255 full left to 255 full right, by
	;the side of the eye it is on
	(defq right (sky-cross forward up) rx (near-way (- (first at) (first eye))) ry (near-way (- (second at) (second eye)))
		rz (near-way (- (third at) (third eye))) d (sqrt (+ (* rx rx) (* ry ry) (* rz rz) +double_1)))
	(n2i (* +pan_most (/ (+ (* rx (first right)) (* ry (second right)) (* rz (third right))) d))))

(defun buoys-nuke (sides)
	;a buoy too crowded for +nuke_looks looks running armed, if it is in
	;view, near enough to be drawn and in what the frame sees, sides, as
	;only a heap drawn slows the frames and it is a sight to see, the
	;klaxon sounding; and three seconds after, flashing white, gone
	;nuclear: the small rocks round it
	;shattered and sent off out of sight, rocks-nuke, grains thrown out
	;where those near the eye were, and a blast far bigger than a
	;missile's, its light, glow and front, throwing the rest out, heard
	(defq now (pii-time))
	(each (lambda (buoy)
			(bind '(at & & & & & over armed) buoy)
			(cond
				((and (= armed 0) (>= over +nuke_looks) (in-view? at sides))
					(elem-set buoy 7 now)
					;the klaxon, once, from the side it is on, as loud wherever
					(if (num? klaxon) (audio-play-rpc klaxon (side-of at))))
				((and (> armed 0) (> (- now armed) +nuke_fuse))
					(bind '(x y z) at)
					(defq shattered (rocks-nuke x y z eye (+ +rock_far +out_of_sight))
						ahead (filter (# (in-view? %0 sides)) shattered))
					;each shattered in view a shower of debris, its pieces as big
					;as a few pixels however far off
					(each! (lambda (p)
							(defq rx (near-way (- (first p) (first eye))) ry (near-way (- (second p) (second eye)))
								rz (near-way (- (third p) (third eye))))
							(push sprays (list p now (+ +shatter_throw_least (* +shatter_throw (rnd))) (random +spray_kinds)
								(max +debris_least (* (sqrt (+ (* rx rx) (* ry ry) (* rz rz))) +debris_far)))))
						(list ahead) 0 (min (length ahead) +nuke_sprays))
					(if (> (length sprays) +spray_most) (setq sprays (slice sprays (- (length sprays) +spray_most) -1)))
					(push blasts (list (cat at) now +double_0 +nuke_reach +nuke_strength +nuke_life +nuke_light +nuke_glow :t))
					(times 4 (push hits (list x y z +blast_crunch)))
					(elem-set buoy 6 0)
					(elem-set buoy 7 0))))
		buoys))

(defun bomb-fire (sx sy)
	;a bomb fired from the ship, through where the picture was clicked, sx
	;and sy -1 to 1 across and up it, going with the ship as well
	(bind '(w h) size)
	(defq right (sky-cross forward up) spread_x (/ (* +spread_y (n2d w)) (n2d h))
		way (sky-unit (nums-add forward (nums-add (nums-scale right (* sx spread_x)) (nums-scale up (* sy +spread_y))))))
	(if (< (length bombs) +bombs_most)
		(push bombs (list (nums-add eye (nums-scale way +missile_ahead))
			(nums-add (nums-scale way +bomb_speed) (nums-scale forward ship_speed)) (pii-time)))))

(defun bombs-move (dt)
	;each bomb on, kept in the box of the field, gone off at the end of its
	;fuse or in the reach of a rock: a blast, and hits at its middle, so it
	;is heard and throws out grains as a hit does. Each blast's front on,
	;the rocks it goes over thrown out, rocks-blast, till its light is gone
	(defq now (pii-time))
	(setq bombs (filter (lambda ((at way born))
			(each! (lambda (p v)
					(defq n (+ p (* v dt)))
					(elem-set at (!) (cond ((< n +double_0) (+ n +field)) ((>= n +field) (- n +field)) (n))))
				(list at way))
			(cond
				((or (> (- now born) +bomb_fuse)
						(when (defq struck (apply (const rock-at) at))
							;the rock it hits spun by where it is struck
							(bind '(x y z) at)
							(rock-strike struck x y z (sky-unit way))
							:t))
					(push blasts (list at now +double_0 +blast_reach +blast_strength +blast_life +blast_light +blast_glow_most :nil))
					(bind '(x y z) at)
					(times 3 (push hits (list x y z +blast_crunch)))
					:nil)
				(:t)))
		bombs)
		blasts (filter (lambda (blast)
			(bind '(at born from reach strength life & & calm) blast)
			(bind '(x y z) at)
			(defq to (* +blast_front (/ (n2d (- now born)) +second)))
			(if (< from reach) (rocks-blast x y z from to reach strength calm))
			(elem-set blast 2 to)
			(< (- now born) life))
		blasts)))

(defun bomb-lamps ()
	;the lights of the bombs and blasts now, nearest the eye, which light a
	;rock before a buoy's: a bomb's its own, a blast's as bright as it is,
	;fading; and the glows of both, each (at colour size), a blast's the
	;bigger as its front goes out
	(defq now (pii-time) glows (list))
	(bind '(ex ey ez) eye)
	(setq blast_lamps (cat
			(map (lambda ((at way &ignore))
					(defq near (doubles (+ ex (near-way (- (first at) ex))) (+ ey (near-way (- (second at) ey)))
							(+ ez (near-way (- (third at) ez))))
						;its nozzle's glow, as big as a few pixels however far,
						;and a trail behind along its way, fading
						nozzle (nums-sub near (nums-scale (sky-unit way) (* +missile_size +nozzle_back)))
						far (sqrt (nums-dot (nums-sub near eye) (nums-sub near eye)))
						size (+ +bomb_glow_least (* far +bomb_glow_far)) bright (nums-scale +bomb_light +bomb_bright))
					(push glows (list nozzle bright size))
					(each! (lambda ()
							(defq back (n2d (inc (!))))
							(push glows (list (nums-sub nozzle (nums-scale way (* back +trail_step)))
								(nums-scale bright (pow +trail_fade back)) (* size (pow +trail_shrink back)))))
						'() 0 +trail_glows)
					(list near +bomb_light))
				bombs)
			(map (lambda ((at born & & & life light glow_most &ignore))
					(defq near (doubles (+ ex (near-way (- (first at) ex))) (+ ey (near-way (- (second at) ey)))
							(+ ez (near-way (- (third at) ez))))
						left (max +double_0 (- +double_1 (/ (n2d (- now born)) (n2d life)))) colour (nums-scale light (* left left)))
					(push glows (list near (nums-scale colour +blast_glow_bright)
						(+ +blast_glow_least (* (- glow_most +blast_glow_least) (- +double_1 left)))))
					(list near colour))
				blasts))
		blast_glows glows))

(defun rock-buoy (x y z)
	; (rock-buoy x y z) -> ((rock_lamp_at place) (rock_lamp colour))
	;the lamp of the buoy nearest a rock at x y z, near enough to light it,
	;where it is and as bright as it is, or none: of the lamps near
	;enough to the eye to light a rock it sees
	;a bomb's or a blast's before any buoy's
	(defq best :nil best_d +blast_light_far)
	(each (lambda ((at colour))
			(defq dx (- (first at) x) dy (- (second at) y) dz (- (third at) z)
				d (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
			(when (< d best_d) (setq best_d d best (list at colour))))
		blast_lamps)
	(unless best
		(setq best_d +buoy_light_far)
		(each (lambda ((at colour))
				(defq dx (- (first at) x) dy (- (second at) y) dz (- (third at) z)
					d (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
				(when (< d best_d) (setq best_d d best (list at colour))))
			lamps_near))
	(if best
		(list (list 'rock_lamp_at (first best)) (list 'rock_lamp (nums-scale (second best) +buoy_power)))
		(list (list 'rock_lamp_at eye) (list 'rock_lamp +dark))))

(defun buoy-tilt (axis)
	; (buoy-tilt axis) -> mat4x4
	;the least turn that takes up, y, to axis, of length 1: about the way
	;across both, so it goes smoothly as the axis does; turned over if it
	;is all but straight down
	(bind '(ax ay az) axis)
	(if (< ay +tilt_flip)
		(mat4x4-rotx +double_pi)
		(defq k (/ +double_1 (+ +double_1 ay)))
		(doubles (- +double_1 (* k ax ax)) ax (neg (* k ax az)) +double_0
			(neg ax) (- +double_1 (* k (+ (* ax ax) (* az az)))) (neg az) +double_0
			(neg (* k ax az)) az (- +double_1 (* k az az)) +double_0
			+double_0 +double_0 +double_0 +double_1)))

(defun buoys-face (dt)
	;each buoy's lamp kept toward the ship's side of it: a buoy whose way up
	;is turned from the ship past +face_least turns to it, slowly, dt
	;seconds of it, so each turns when it must and not all as one
	(each (lambda ((at &ignore) axis)
			(defq to (sky-unit (nums-sub eye at)))
			(when (< (nums-dot axis to) +face_least)
				(defq turned (sky-unit (nums-add axis (nums-scale to (min +double_1 (* dt +face_rate))))))
				(each! (lambda (v) (elem-set axis (!) v)) (list turned))))
		lamps buoy_axes))

(defun missile-draws (view sides)
	; (missile-draws view sides) -> draws
	;each bomb in flight in what the frame sees, a missile pointed the way
	;it goes, its middle ahead of its nozzle's glow
	(defq out (list))
	(bind '(ex ey ez) eye)
	(when missile_had
		(each (lambda ((at way &ignore))
				(defq near (doubles (+ ex (near-way (- (first at) ex))) (+ ey (near-way (- (second at) ey)))
						(+ ez (near-way (- (third at) ez))))
					rx (- (first near) ex) ry (- (second near) ey) rz (- (third near) ez))
				(and (< (sqrt (+ (* rx rx) (* ry ry) (* rz rz))) rock_far) (seen? rx ry rz +missile_size sides)
					(push out (list +mesh_missile
						(list (list 'model (mat4x4-mul (apply (const mat4x4-translate) (map (# %0) near))
								(mat4x4-mul (buoy-tilt (sky-unit way)) (mat4x4-scale +missile_size +missile_size +missile_size))))
							(list 'lens view) (list 'eye eye))
						(list (list 'sun sun_at) (list 'eye eye) (list 'sun_light sun_light) (list 'pillars_at pillars_at)
							(list 'pillars_light pillars_light) (list 'nebula_pixels nebula_pixels))
						(list nebula_map) missile_files))))
			bombs))
	out)

(defun buoy-draws (view sides)
	; (buoy-draws view sides) -> draws
	;the buoys near enough and in what the frame sees, each hung from where
	;its lamp is along its way up, turning slowly about it, its lamp as
	;bright as it is now
	(defq out (list) turn (/ (n2d (pii-time)) +buoy_turn_long))
	(when buoy_had
		(each (lambda ((at colour) axis)
				;its body's middle, along its way up from its lamp
				(defq body_at (nums-sub at (nums-scale axis (* +buoy_size +buoy_body)))
					rx (- (first body_at) (first eye)) ry (- (second body_at) (second eye)) rz (- (third body_at) (third eye)))
				(and (< (sqrt (+ (* rx rx) (* ry ry) (* rz rz))) rock_far) (seen? rx ry rz +buoy_size sides)
					(push out (list +mesh_buoy
						(list (list 'model (mat4x4-mul (apply (const mat4x4-translate) (map (# %0) body_at))
								(mat4x4-mul (buoy-tilt axis)
									(mat4x4-mul (mat4x4-roty (+ turn (n2d (!)))) (mat4x4-scale +buoy_size +buoy_size +buoy_size)))))
							(list 'lens view) (list 'eye eye))
						(list (list 'sun sun_at) (list 'eye eye) (list 'sun_light sun_light)
							(list 'pillars_light pillars_light) (list 'pillars_at pillars_at)
							(list 'pillars_across pillars_across) (list 'pillars_up pillars_up)
							(list 'pillars_size pillars_size) (list 'pillars_pixels pillars_pixels)
							(list 'nebula_pixels nebula_pixels) (list 'stars_pixels stars_pixels) (list 'lamp colour))
						(list pillars_map nebula_map stars_map) buoy_files))))
			lamps buoy_axes))
	out)

(defun bomb-glow-draws (view right up sides)
	; (bomb-glow-draws view right up sides) -> draws
	;the glow of each bomb and blast in what the frame sees, added to what
	;is behind
	(defq out (list))
	(each (lambda ((at colour size))
			(defq rx (- (first at) (first eye)) ry (- (second at) (second eye)) rz (- (third at) (third eye)))
			(and (< (sqrt (+ (* rx rx) (* ry ry) (* rz rz))) rock_far) (seen? rx ry rz size sides)
				(push out (list +mesh_glow
					(list (list 'middle at) (list 'size size) (list 'right right) (list 'up up) (list 'lens view))
					(list (list 'lamp colour)) '() glow_files))))
		blast_glows)
	out)

(defun glow-draws (view right up sides)
	; (glow-draws view right up sides) -> draws
	;the glow round each buoy's lamp in what the frame sees, the bigger and
	;brighter as it flashes, added to what is behind
	(defq out (list))
	(each (lambda ((at colour))
			(defq rx (- (first at) (first eye)) ry (- (second at) (second eye)) rz (- (third at) (third eye))
				size (* +buoy_size (+ +glow_least (* +glow_grow (reduce (const max) colour +double_0)))))
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
	;the speed eased to a boost's while the right button is held, back to
	;cruising after
	(setq ship_speed (+ ship_speed (* (- (if boosting +boost +cruise) ship_speed)
		(min +double_1 (* dt (if boosting +boost_up +boost_down))))))
	(thrust-feed dt)
	;how far it draws, eased to the level's
	(setq draw_share (+ draw_share (* (- (elem-get +view_of detail) draw_share) (min +double_1 (/ dt +view_ease))))
		rock_far (* +rock_far draw_share))
	(frame-mark 0)
	;the rocks on, and a part of them moved by each other, a round of all
	;the parts as many frames, the grid made at the start of each round
	(rocks-move dt)
	(frame-mark 1)
	(if (= part 0) (setq grid (rocks-grid)))
	(rocks-meet (* (min dt +meet_dt_most) (n2d +meet_parts)) grid part)
	(frame-mark 2)
	(setq part (% (inc part) +meet_parts))
	;now and then a cluster out of sight made lively again
	(when (>= (- (pii-time) refreshed) +refresh_every)
		(setq refreshed (pii-time))
		(clusters-refresh eye (+ +rock_far +out_of_sight))
		(buoys-restation eye (+ +rock_far +out_of_sight)))
	;the bombs on and the blasts, heard with the hits
	(bombs-move dt)
	(hear-hits)
	(frame-mark 3)
	(setq eye (apply (const doubles) (map (lambda (e f)
		(defq v (+ e (* f ship_speed dt)))
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
	(buoys-nuke sides)
	(buoy-lamps)
	(buoys-face dt)
	(bomb-lamps)
	(frame-mark 4)
	(defq rd (rock-draws view sides) bd (cat (buoy-draws view sides) (missile-draws view sides)))
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
		(glow-draws view right up sides) (bomb-glow-draws view right up sides) (spray-draws view right up sides)))
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
		crunch_far (crunch-levels "crunch_far" +far_levels) thrust_pcm (thrust-load)
		klaxon (if crunch_far (audio-add-rpc (cat *app_root* "data/klaxon.wav"))))
	;the rocks' kinds, made by a farm of children on this machine's nodes,
	;off this one, and each slotted in as it comes
	(setq jobs (Jobs (cat *app_root* "child.lisp") (elem-get select +select_task)
		(elem-get select +select_reply) '(4 2) :t))
	(. jobs :add (push (map! (lambda ()
			(cat (setf-> (str-alloc +rock_size) (+rock_num (!))) (str (+ 1000 (* (!) 7919)))))
		'() 0 +rock_kinds)
		;and the buoy, the kind after them
		(cat (setf-> (str-alloc +rock_size) (+rock_num +rock_kinds)) "buoy")
		;and the missile, the kind after that
		(cat (setf-> (str-alloc +rock_size) (+rock_num (inc +rock_kinds))) "missile")))
	(setq buoys (make-buoys) buoy_axes (map (lambda (&ignore) (doubles +double_0 +double_1 +double_0)) buoys))
	(config-load)
	(setq detail (min 4 (. *config* :find :detail)) draw_share (elem-get +view_of detail) mode (. *config* :find :mode)
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
						((> kind +rock_kinds) (setq missile_had :t))
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
						;a click of the left button fires a bomb through where it
						;is, the right held is a boost. A first press says which
						;button by its number, 3 the right, and a move with them
						;down says them as bits, as the window takes them
						(defq buttons (getf msg +ev_msg_mouse_buttons))
						(and (= pressed 0) (/= buttons 0)
							(setq buttons (case buttons (2 2) (3 +right_button) (:t +left_button))))
						(and (bits? buttons +left_button) (not (bits? pressed +left_button)) (apply (const bomb-fire) stick))
						(setq pressed buttons boosting (bits? buttons +right_button))
						(. *window* :event msg))
					((and (= (getf msg +ev_msg_target_id) (. *canvas* :get_id))
							(= (getf msg +ev_msg_type) +ev_type_exit))
						(setq stick :nil pressed 0 boosting :nil)
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
	(if thrust_stream (audio-stream-close-rpc thrust_stream))
	(if (num? klaxon) (audio-remove-rpc klaxon))
	(if crunch_far (each (lambda (levels) (each (const audio-remove-rpc) levels)) (push (cat crunches) crunch_far)))
	(. render :close)
	(. *window* :close))
