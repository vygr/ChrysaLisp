(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "lib/math/matrix.inc")
(import "lib/gpu/render_farm.inc")
(import "lib/gpu/shapes.inc")
(import "apps/media/jukebox/music.inc")
(import "service/audio/app.inc")
(import "lib/audio/wav.inc")
(import "./chunk.inc")
(import "./route.inc")
(import "./fish.inc")
(import "lib/nav/nav.inc")
(import "lib/debug/frame_log.inc")
(import "lib/physics/move.inc")
(import "lib/physics/collide.inc")

;Water, a dive through a reef that goes on every way, drawn by a
;(Render-tris), lib/gpu/render_farm.inc, which has the GPU draw it. The
;reef is a field of how much water there is at each place, field.shader,
;the seabed, outcrops of rock on it, and caves under it, a lattice of
;junctions joined by tunnels, and shallows that break the surface. It is
;made a chunk at a time as the diver comes near, by a farm of children,
;child.lisp, each chunk's surface marched by its cubes, chunk.inc, and
;let go of behind. Everything of a place is of the place, so it is the
;same every time. The auto pilot goes from junction to junction, along
;the tunnels and out over the reef; or the pointer steers. The diver is
;always under the water. The user's Jukebox favourites play behind it
;
;The sub's sonar: its ping, data/ping.wav, from "Submarine Sonar" by
;DRAGON-STUDIO,
;pixabay.com/sound-effects/film-special-effects-submarine-sonar-386154/;
;its echo, data/echo.wav, from "Sonar" by VoiceBosch,
;pixabay.com/sound-effects/film-special-effects-sonar-183436/.
;The scooter's whine, data/prop.wav, from "submarine prop whooshing" by
;rexen_devar (Freesound), via freesound_community on Pixabay,
;pixabay.com/sound-effects/film-special-effects-submarine-prop-whooshing-45861/.
;A bump into the rock, data/thud.wav, from "Dramatic Thud Stinger" by
;DRAGON-STUDIO,
;pixabay.com/sound-effects/film-special-effects-dramatic-thud-stinger-631487/.
;All under the Pixabay Content License, data/README.md.

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
	;how high the frame is at a distance of 1, and how near it sees
	+spread_y +double_1/2 +near +double_1/4
	reef_files (list (cat *app_root* "reef_vertex.shader") (cat *app_root* "reef_lit.shader"))
	water_files (list "lib/gpu/shaders/sky_vertex.shader" (cat *app_root* "water_lit.shader"))
	stone_files (list (cat *app_root* "stone_vertex.shader") (cat *app_root* "stone_lit.shader"))
	weed_files (list (cat *app_root* "weed_vertex.shader") (cat *app_root* "weed_lit.shader"))
	;the kinds of stone and weed, all their meshes
	+lying_kinds (+ +stone_kinds +weed_kinds)
	;the fish: their shaders, their meshes from +mesh_fish on, which the
	;render has, and the schools swimming round
	fish_files (list (cat *app_root* "fish_vertex.shader") (cat *app_root* "fish_lit.shader"))
	+mesh_fish 80 fish_had (map! (lambda () :nil) '() 0 +fish_kinds) schools (list) fish_drawn 0
	;how the fish move, lib/physics: each a body
	mover (Mover)
	;the meshes, the water round and over, the stones from +mesh_stone on,
	;each kind near to and further off, and the chunks from +mesh_chunk on;
	;which stones' meshes the render has
	+mesh_water 1 +mesh_stone 10 +mesh_chunk 100 mesh_next +mesh_chunk
	stones_had (map! (lambda () :nil) '() 0 (* +lying_kinds +stone_fines))
	;how far off the stones are drawn, of how far the frame sees, where
	;the water hides them all, whatever their size, so none is seen to come;
	;and how near the finest mesh of one is drawn, and the next, for size 1
	+stone_hidden (n2d 0.8) +stone_fine (n2d 20) +stone_coarse (n2d 45) stones_drawn 0
	;how far the biggest stone reaches out of its chunk, its bulk 7 and its
	;mesh 1.2 of that
	+stone_over (n2d 9)
	;the world: how far round it goes, how big a chunk is, how many go
	;round, the levels of chunks there are, and how big a cell of the
	;lattice is and how far apart its levels are, field.shader
	+world (n2d 18496) +chunk (n2d +chunk_side) +chunks_round 1088 +chunk_low -6 +chunk_high 2
	+cell (n2d 68) +level (n2d 36) +surface (n2d 30) +under_surface (n2d 5)
	;the chunks known, by their place, each (state id cx cy cz cubes asked
	;stones):
	;:out as a child makes it, :none when it has no surface, :mesh when its
	;mesh is the render's, how many cubes it was made of, and when it was
	;asked for, and the stones on its floors; the places of those out, by their mesh's number; how many
	;may be out at once, as many as the farm has
	;children: the farm gives the last job it was given first, so a queue
	;of them would leave the first, the nearest, till last
	chunks (Fmap 101) outs (Fmap 31) outs_most 8
	;how much less detail it draws, 0 all of it to 4 least: how far it
	;sees at each, all of it is made that far and the water hides it all by
	;then; and how many cubes a chunk is each way, all of them alike, so
	;two side by side are worked out at the very same places of their face
	;and meet with no gap
	detail 0 slow 0 fast 0 +rate_least 50 +rate_most 58 detail_down 0
	+view_of (doubles (n2d 90) (n2d 80) (n2d 70) (n2d 60) (n2d 50)) +cubes_of ''(26 26 22 17 13)
	view (n2d 90) murk (n2d 0.05) +murk_gone (n2d 4.6)
	;the diver: where the eye is, the way it looks and its up, the pointer,
	;how fast it turns at most and how fast it swims
	eye :nil forward :nil up :nil turned 0 stick :nil
	+turn_most (n2d 0.3) +swim +double_2
	;the auto pilot, a Nav, lib/nav: the stiffness of its turn over the
	;reef and in the caves, a spring, and how much it is damped, more than
	;just never past it, as water damps a turn
	;the stones lying on the seabed, each a ball as far as it reaches, of
	;lib/physics, its chunk's, round the world as it wraps across, its
	;cells so big
	stone_world :nil +stone_cell (n2d 16)
	pilot :nil +auto_settle (n2d 1.2) +cave_settle (n2d 2.5) +pilot_zeta (n2d 1.2)
	;how far from straight up or down, across, the auto pilot's up is still
	;all its own, and over how much more it comes to be the world's
	+upright_from (n2d 0.2) +upright_over (n2d 0.3)
	;its speed now, eased up to a sprint while the right button is held and
	;down again after, how fast either way, a second, and the buttons; its
	;body of the mover, how fast its going turns to where it faces, a share a
	;second, and how fast it is shoved off the rock
	speed +double_2 +sprint +double_6 +sprint_up +double_1 +sprint_down (n2d 0.7)
	sub :nil +sub_align (n2d 8) +sub_shove (n2d 30)
	sprinting :nil pressed 0 +right_button 4 +lamp (n2d 2.4)
	;the scooter's whine as the right button is held: its loop's samples,
	;loaded at the start, data/prop.wav, the stream it is played on, where
	;in the loop it is, its rate, how far ahead of what is heard it is kept,
	;how loud at most and now, eased up as the button is held and dying
	;away after, how fast up, a second, and how fast it dies, a share a
	;second; how loud the loop is made as it loads, and the loudness of the
	;sounds loaded, the thud's and the sonar's
	prop_pcm :nil prop_stream :nil prop_at 0 +prop_rate 22050 +prop_ahead 1100
	+prop_loud (n2d 0.7) prop_gain +double_0 +prop_up +double_4 +prop_down (n2d 1.5) +prop_quiet (n2d 0.01)
	+prop_peak (n2d 21000) +prop_scale (n2d 14000)
	;a bump into the rock: its thud's samples, loaded at the start,
	;data/thud.wav, the stream it is played on, how hard a bump is softest,
	;its speed into what it hit, and loudest; what it touched lately, the
	;rock and the stones, each when last and the way it faced, how long off
	;a thing it is touching it anew, and how far round the rock must face
	;to be anew, the cosine
	thud_pcm :nil thud_stream :nil +thud_least (n2d 0.2) +thud_most +double_4
	touches (list) +thud_again 300000 +thud_turn (n2d 0.7)
	;how loud the softest bump is, of the loudest, and the least step of time
	+thud_soft (n2d 0.3) +dt_least (n2d 0.001)
	;the sub's sonar, in stereo: its ping's samples, its echo's in the left
	;and in the right, silence to place the echoes in, loaded at the start,
	;data/README.md, the stream it is played on, when the last went out,
	;how often, the first how soon; how loud the ping is, of the scooter's;
	;how far it hears the rock, how many samples later an echo is for a
	;unit further, how loud the nearest is, the ways across the front it
	;looks, how far round the widest is, how near the rock the search must
	;come and its least step; and the loudest a sample of it may be
	ping_pcm :nil echo_left :nil echo_right :nil ping_quiet :nil sonar_stream :nil ping_at 0 echoes 0
	+ping_every 4000000 +ping_first 2000000 +ping_loud (n2d 0.3)
	+ping_reach (n2d 80) +ping_unit (n2d 300) +echo_most +double_1/2 +sonar_most (n2d 30000)
	+ping_ways ''(-1 -0.5 0 0.5 1) +ping_wide (n2d 0.6) +ping_near +double_1/2 +ping_step +double_1/2
	;the stones as the diver bumps into them: each kind a rounded box, how
	;far each way, of its size, how far up its middle, and how square across,
	;2 round to more square; how far round a stone to look, of its size, and
	;how clear of one the eye keeps
	+stone_shapes ''((1.0 0.36 0.95 0 2.8) (1.0 0.36 0.95 0 2.8) (1.0 0.36 0.95 0 2.8)
		(0.85 0.55 0.75 0 3) (0.85 0.55 0.75 0 3) (0.85 0.55 0.75 0 3)
		(0.8 0.6 0.8 0 2) (0.8 0.6 0.8 0 2) (0.8 0.6 0.8 0 2) (0.8 0.6 0.8 0 2) (0.8 0.6 0.8 0 2)
		(0.72 0.95 0.72 0 5) (0.72 0.95 0.72 0 5) (0.72 0.95 0.72 0 5))
	+stone_reach (n2d 1.5) +stone_clear (n2d 0.5)
	;how clear of a stone the auto pilot aims
	+aim_clear (n2d 3)
	;how much of the water's light reaches the eye, eased, the least, and
	;how often it is looked at again
	open +double_1 +open_least (n2d 0.04) +open_ease +double_2 open_at 0
	;the pointer: how far its edge turns the way wanted, its dead middle, how
	;fast at most it turns the diver, a second, and the share of the way to
	;what it asks the turn goes, a second
	+stick_reach (n2d 0.3) +dead (n2d 0.08) +stick_turn (n2d 0.25) +point_rate +double_2
	;how near the rock it may come, and how hard it is pushed off
	+clear (n2d 1.3) +push (n2d 3)
	;the auto pilot: the way ahead, legs of it, each (id places centred
	;cave), the rough curve of a tunnel till a child sends the line down
	;its middle, route.inc, and those asked for by their numbers; the junction
	;it is going to, and the one it came from, and how far ahead of the eye
	;it aims along the way; the way out of the caves it is following, and
	;how many junctions it looks at for it at most; where it was a while
	;ago, and when, so it
	;gives up a way it makes no headway along, and how much headway that is
	legs (list) leg_outs (Fmap 11) going :nil came :nil escape (list) trail (list) +search_most 600 +lead (n2d 5)
	+sight_step (n2d 1.5)
	;how many places on along a leg it looks for the nearest, and how many
	;on from that it aims
	+look_on 12 +aim_on 5
	;where it was a while ago and when, how long it may make no headway on
	;its line before it asks for a new one from where it is, and how much
	;headway that is
	was :nil was_at 0 +stuck_long 8000000 +headway (n2d 2)
	;the native functions of the field, field.shader
	field_water :nil field_junction :nil field_link :nil field_bend :nil field_warp :nil field_lit :nil field_bed :nil
	;how far under the seabed the caves are counted from
	+under_bed +double_2
	select :nil id :t render :nil frames 0 since (pii-time) began (pii-time)
	given 0 frame_took 0 due 0 jobs :nil
	want +small size +small shown :nil
	music :nil music_on :t
	;the way of flying, :auto or :mouse
	mode :auto
	*config* :nil +config_version 1 +config_file (cat (get :home *env*) "water.tre"))

;the numbers of the functions below: a second, of (pii-time); the longest
;step of time a frame; the ways of the lattice from a junction, across,
;along and the diagonals, k 3 to 6, and up, 7, field.shader; how far round
;a chunk's middle it reaches; how far past the view a chunk is let go of
(defq +second (n2d 1000000) +dt_most +double_1/10
	+ways ''((3 1 0 0) (4 0 0 1) (5 1 0 1) (6 1 0 -1)) +chunk_reach (n2d 15) +let_go (n2d 34)
	+half_chunk (n2d 8.5)
	;how far from the rock a chunk's middle is, either way, for it to have
	;no surface: the field changes by no more than about 2 a unit moved,
	;and a chunk reaches 14.7 from its middle; and how many frames apart
	;the chunks wanted are looked for
	+no_surface (n2d 32) +want_every 6
	;the auto pilot's choice of way: how much chance there is in it, a
	;thousandth of a random number, and how hard a shaft down from the reef
	;and a way up from the caves pull
	+choose_chance (n2d 0.0008) +dive_pull +double_4 +seek_pull (n2d 0.7)
	;how long it cruises over the reef after a cave before the next, and
	;since when
	+cave_rest 120000000 cave_left 0
	;a launch: of how many ways round it faces one, how clear of the rock
	;its junction is, of how many ways it takes the clearest, and how far it
	;looks along each
	+launch_turns (n2d 3600)
	;a stone's own number of where it is, a hash of the shaders' own
	+seed_x (/ (n2d 129898) (n2d 10000)) +seed_y (/ (n2d 78233) (n2d 1000)) +seed_z (/ (n2d 37719) (n2d 1000))
	+seed_k (/ (n2d 437585453) (n2d 10000)) +launch_clear (n2d 8) +launch_ways 16 +launch_look (n2d 40)
	;its outings over the open water, of a hundred, along a formation and
	;over the seabed, the rest on to the next junction; how far over the
	;seabed it skims; how far round a junction it looks for a formation, in
	;steps of the grid, and how big they are; how far under the surface it
	;goes along one, how clear it keeps of it, how far a step along it, how
	;many steps and how few will do, and how far ahead a formation must be; how far ahead of the eye it takes over, at most, and
	;how near a junction too near to go to
	+circle_chance 35 +skim_chance 30 +skim_over +double_2 +pinnacle_reach 6 +pinnacle_step (n2d 8)
	+ring_under +double_2 +edge_clear (n2d 6) +edge_step (n2d 5) +edge_steps 14 +edge_least 5
	+ahead_least (n2d 0.2) +ahead_most (n2d 20) +ahead_near (n2d 15)
	;how far into the open water a junction must be for it to go to it
	+junction_clear +double_1
	;the auto pilot on its line: how fast it turns on a leg of the caves or
	;a shaft, the way it is aiming, how slow it may go for a sharp turn, how
	;slow on a leg of the caves not yet lined, and further than anywhere
	+cave_turn (n2d 1.1) aim_way :nil +least_speed (n2d 0.4) +unlined_speed (n2d 0.4) +far_clear (n2d 100)
	;how far over the rock the auto pilot keeps over the reef
	+over_rock +double_4)

(defun make-canvas (size)
	; (make-canvas size) -> canvas
	;its pixels are in shared memory if the host has it. Every pixel of a
	;frame is drawn, so it is opaque; black till the first
	(bind '(w h) size)
	(.-> (ifn (canvas-shared w h 1) (Canvas w h 1))
		(:fill +argb_black)
		(:set_flags +view_flag_opaque +view_flag_opaque)
		(:swap +swap_write)))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Water" (+sym_close +sym_max +sym_min) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		;the way of swimming, the auto pilot or the pointer
		(. (ui-radio-bar *mode* (+sym_auto +sym_select)) :connect +event_mode)
		(ui-tool-bar *music* (:paper :paper_toolbar2)
			(ui-buttons (+sym_media_start +sym_media_play +sym_media_end) +event_prev))
		(ui-label *status* (:text "..." :font :font_body)))
	(ui-backdrop *backdrop* (:style :plain :paper +argb_black :min_width (first +small) :min_height (second +small))
		(ui-element *canvas* (make-canvas +small))))

(ui-tool-tips *mode* '("auto pilot" "mouse"))
(ui-tool-tips *music* '("prev" "play" "next"))

(defun unit (v)
	; (unit v) -> doubles
	(nums-scale v (recip (sqrt (nums-dot v v)))))

(defun cross (a b)
	; (cross a b) -> doubles
	(bind '(ax ay az) a)
	(bind '(bx by bz) b)
	(doubles (- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx))))

(defun at-cell (c)
	; (at-cell c) -> doubles
	;a cell of the lattice, (x level z), as the field takes it
	(apply (const doubles) (map (const n2d) c)))

(defun junction-of (c)
	; (junction-of c) -> doubles
	(field_junction (at-cell c)))

(defun open-junction? (c)
	; (open-junction? c) -> :t | :nil
	;is the junction of a cell in the open water, not in the rock of an
	;island that comes up through the surface
	(> (field_water (junction-of c)) +junction_clear))

(defun open-way? (here option)
	; (open-way? here option) -> :t | :nil
	;can the auto pilot go this way from a junction: to one in the open
	;water, and over the reef not blocked by rock that comes up to the
	;surface, as no line over it gets by
	(bind '(to pair k) option)
	(and (open-junction? to)
		(or (< (second here) 0) (< (second to) 0)
			(not (route-blocked? field_water (list (defq a (junction-of here))
				(nums-add (nums-scale (nums-add a (defq b (junction-of to))) +double_1/2) (field_bend (at-cell pair) (n2d k)))
				b))))))

(defun links-of (c)
	; (links-of c) -> ((cell pair k) ...)
	;the junctions a junction has tunnels to: each, the cell of the pair
	;and its way, the pair being of the cell it starts from, field.shader
	(bind '(x l z) c)
	(defq out (list))
	(each (lambda ((k dx dy dz))
			(defq fk (n2d k))
			(if (> (field_link (at-cell c) fk) +double_0)
				(push out (list (list (+ x dx) l (+ z dz)) c k)))
			(defq back (list (- x dx) l (- z dz)))
			(if (> (field_link (at-cell back) fk) +double_0)
				(push out (list back back k))))
		+ways)
	(and (< l 0) (> (field_link (at-cell c) +double_7) +double_0)
		(push out (list (list x (inc l) z) c 7)))
	(when (> l -2)
		(defq below (list x (dec l) z))
		(if (> (field_link (at-cell below) +double_7) +double_0)
			(push out (list below below 7))))
	out)

(defun clear-to (p)
	; (clear-to p) -> :t | :nil
	;is there clear water all the way from the eye to p
	(defq d (nums-sub p eye) n (inc (n2i (/ (sqrt (nums-dot d d)) +sight_step))))
	(every (# (> (field_water (nums-add eye (nums-scale d (/ (n2d %0) (n2d n))))) +double_0))
		(map! (# (inc (!))) '() 0 n)))

(defun in-caves? ()
	; (in-caves?) -> :t | :nil
	;is the eye under the seabed, in the caves
	(< (second eye) (- (field_bed (doubles (first eye) (third eye))) +under_bed)))

(defun has-shaft (c)
	; (has-shaft c) -> :t | :nil
	;has the junction of a cell of the open water a shaft down under it
	(bind '(x l z) c)
	(> (field_link (at-cell (list x (dec l) z)) +double_7) +double_0))

(defun launch-cell ()
	; (launch-cell) -> cell
	;a junction over the reef anywhere round the world, by chance, with
	;clear water well round it, not by the rock of an island nor over the
	;mouth of a shaft down to the caves
	(defq c :nil)
	(until (and c (> (field_water (junction-of c)) +launch_clear) (not (has-shaft c)))
		(setq c (list (random (const (n2i (/ +world +cell)))) 0 (random (const (n2i (/ +world +cell)))))))
	c)

(defun launch-way (at)
	; (launch-way at) -> doubles
	;the way across, level, of a few round, from a place by chance, with the
	;most clear water ahead of it
	(defq best :nil best_far +double_-1 a0 (* +double_2pi (/ (n2d (random 3600)) +launch_turns)))
	(each! (lambda ()
			(defq a (+ a0 (/ (* +double_2pi (n2d (!))) (n2d +launch_ways)))
				way (doubles (sin a) +double_0 (cos a)) far +double_0)
			(while (and (< far +launch_look) (> (field_water (nums-add at (nums-scale way far))) +double_2))
				(setq far (+ far +sight_step)))
			(if (> far best_far) (setq best way best_far far)))
		'() 0 +launch_ways)
	best)

(defun nearest-junction ()
	; (nearest-junction) -> cell
	;the nearest junction there is clear water to from the eye, of the cells
	;round it, so the one of the very tunnels the eye is in, not one through
	;the rock; the nearest if none is clear. Over the seabed only those of
	;the open water, the mouth of a shaft is not the caves
	(defq cx (n2i (floor (/ (first eye) +cell))) cz (n2i (floor (/ (third eye) +cell))) all (list)
		low (if (in-caves?) 0 2))
	(each! (lambda ()
			(defq l (- (!) 2))
			(each! (lambda ()
					(defq z (!))
					(each! (lambda ()
							(defq c (list (!) l z) j (junction-of c) at (if (< l 0) (nums-sub j (field_warp j)) j)
								d (nums-sub at eye))
							(push all (list (nums-dot d d) c at)))
						'() (dec cx) (+ cx 2)))
				'() (dec cz) (+ cz 2)))
		'() low 3)
	(sort all (lambda ((a &ignore) (b &ignore)) (if (< a b) -1 (if (> a b) 1 0))))
	(second (ifn (some (lambda (one) (if (clear-to (third one)) one)) all) (first all))))

(defun way-out (start &optional avoid)
	; (way-out start [avoid]) -> cells
	;the way out of the caves from a junction, breadth first along their
	;tunnels and shafts to the nearest junction of the open water, the
	;junctions after start; none if there is none near enough. Not by the
	;junction avoid first, the way it came in, so a dive into the caves is
	;a tour of them, in by one shaft and out by another. A link is only a
	;hash, so it looks at many
	(defq parents (Fmap 101) queue (list start) at 0 found :nil)
	(if avoid (. parents :insert (str avoid) :start))
	(. parents :insert (str start) :start)
	(while (and (not found) (< at (length queue)) (< at +search_most))
		(defq here (elem-get queue at))
		(++ at)
		(if (= (second here) 0)
			(setq found here)
			(each (lambda ((to &ignore))
					(unless (. parents :find (defq key (str to)))
						(. parents :insert key here)
						(push queue to)))
				(links-of here))))
	(defq path (list))
	(when found
		(while (nql (defq back (. parents :find (str found))) :start)
			(push path found)
			(setq found back))
		(reverse path)))

(defun leg-ask (a mid b cave)
	; (leg-ask a mid b cave)
	;a leg of the way ahead, the rough curve of it till a child sends the
	;line down its middle; the jobs of the farm are given the last first, so
	;it is made before the chunks asked for with it
	(defq id mesh_next rough (nav-path-resample (list a mid b) +double_1)
		rough (if cave (map (# (nums-sub %0 (field_warp %0))) rough) rough)
		leg (list id rough :nil cave 0))
	(setq mesh_next (inc mesh_next))
	(push legs leg)
	(. leg_outs :insert id leg)
	(. jobs :add (list (cat (setf-> (str-alloc +chunk_size) (+chunk_id id) (+chunk_cubes 0) (+chunk_x (if cave 1 0)))
		(shader-verts-str (cat a mid b))))))

(defun leg-came (leg data)
	; (leg-came leg data)
	;the line down the middle of a leg, from a child, in place of its curve,
	;the place along it the eye is at found again, the nearest of it
	(defq pts (partition (push (doubles) data) 3) at 0 near +far_clear)
	(each (lambda (p) (when (< (defq d (far-from p)) near) (setq near d at (!)))) pts)
	(elem-set leg 1 pts)
	(elem-set leg 2 :t)
	(elem-set leg 4 at))

(defun choose-next ()
	; (choose-next)
	;the next junction of the auto pilot, a tunnel from the one it is going
	;to. It travels: the tunnel most nearly the way it is going wins, with a
	;little chance in it, and never back the way it came unless there is no
	;other. Over the reef a shaft down pulls hard, so it goes caving now and
	;then; in the caves it follows the way out, (way-out). The way to it put
	;on the route
	(defq here going came_key (str came) all (filter (lambda (option) (open-way? here option)) (links-of here)))
	(if (empty? all) (setq all (links-of here)))
	(defq options (filter (lambda ((to &ignore)) (nql (str to) came_key)) all))
	(if (empty? options) (setq options all))
	;in the caves it is always on its way out of them, not back the way it
	;came in if there is another; and if the search goes too far, back the
	;way it came through them, the junctions it has passed
	(cond
		((< (second here) 0) (push trail here))
		(:t (if (nempty? trail) (setq cave_left (pii-time))) (setq trail (list))))
	(when (and (< (second here) 0) (empty? escape))
		(setq escape (way-out here came))
		(if (empty? escape) (setq escape (way-out here)))
		(if (empty? escape) (setq escape (slice trail -2 0))))
	;on the way out of the caves, the next junction of it, if it is a way on
	(when (nempty? escape)
		(defq next_key (str (first escape)) on (filter (lambda ((to &ignore)) (eql (str to) next_key)) options))
		(setq escape (if (nempty? on) (progn (setq options on) (slice escape 1 -1)) (list))))
	;over the reef, keen to go caving once it has been out a while
	(defq a (junction-of here) best :nil best_score +double_0 level (second here)
		keen (> (- (pii-time) cave_left) +cave_rest))
	(each (lambda (option)
			(bind '((& l &ignore) &ignore) option)
			(defq way (nums-sub (junction-of (first option)) a) score (+ (nums-dot (unit way) forward)
				(* (n2d (random 1000)) +choose_chance)
				(cond
					((not keen) +double_0)
					;a shaft down from here, or a junction with one under it
					((and (= level 0) (< l level)) +dive_pull)
					((and (= level 0) (has-shaft (first option))) +seek_pull)
					(+double_0))))
			(when (or (not best) (> score best_score)) (setq best option best_score score)))
		options)
	(bind '(to pair k) best)
	(defq b (junction-of to) roll (random 100)
		mid (nums-add (nums-scale (nums-add a b) +double_1/2) (field_bend (at-cell pair) (n2d k))))
	;over the open water an outing on the way to it: along a formation that
	;comes up to the surface, round it if small, if there is one ahead, or down over the seabed, or
	;just on to it
	(cond
		((and (= level 0) (= (second to) 0) (< roll +circle_chance) (defq c (pinnacle-near a))
			(edge-legs a c b)))
		((and (= level 0) (= (second to) 0) (< roll (+ +circle_chance +skim_chance)))
			(elem-set mid 1 (+ (field_bed (doubles (first mid) (third mid))) +skim_over))
			(leg-ask a mid b :nil))
		((leg-ask a mid b (or (< level 0) (< (second to) 0)))))
	(setq came here going to))

(defun pinnacle-near (a)
	; (pinnacle-near a) -> :nil | doubles
	;a formation near a junction of the open water that comes up near the
	;surface, the place of it most nearly ahead, of a grid round it at the
	;depth the auto pilot rounds it at; none if none ahead
	(defq best :nil best_score +ahead_least y (- +surface +under_surface +ring_under))
	(each! (lambda ()
			(defq j (!))
			(each! (lambda ()
					(defq p (doubles (+ (first a) (* (n2d (!)) +pinnacle_step)) y (+ (third a) (* (n2d j) +pinnacle_step))))
					(when (< (field_water p) +double_1)
						(defq d (nums-sub p a))
						(elem-set d 1 +double_0)
						(when (> (defq dd (nums-dot d d)) +double_1)
							(defq score (+ (nums-dot (nums-scale d (recip (sqrt dd))) forward) (* (n2d (random 100)) +choose_chance)))
							(when (> score best_score) (setq best p best_score score)))))
				'() (neg +pinnacle_reach) (inc +pinnacle_reach)))
		'() (neg +pinnacle_reach) (inc +pinnacle_reach))
	best)

(defun flat-slope (p)
	; (flat-slope p) -> doubles
	;the slope of the field at p across, the way out of the rock, of length 1
	(defq e +double_1/2 g (doubles (- (field_water (nums-add p (doubles e +double_0 +double_0)))
				(field_water (nums-sub p (doubles e +double_0 +double_0))))
			+double_0 (- (field_water (nums-add p (doubles +double_0 +double_0 e)))
				(field_water (nums-sub p (doubles +double_0 +double_0 e)))))
		gg (nums-dot g g))
	(if (> gg +double_0) (nums-scale g (recip (sqrt gg))) g))

(defun edge-legs (a c b)
	; (edge-legs a c b) -> :t | :nil
	;the legs from a in toward the formation at c, then along its side as
	;far from it as it keeps, round it if it is small, along its wall if it
	;is big, the way that turns least, then on to b; none if no edge is met
	;at the depth it was found at
	(defq p (cat a))
	(elem-set p 1 (second c))
	(defq d (nums-sub c p) far (sqrt (nums-dot d d)) step (nums-scale d (/ +double_2 far)) n 0)
	;in toward it till the water is as near the rock as it keeps
	(while (and (< (* (n2d n) +double_2) far) (> (field_water p) +edge_clear))
		(setq p (nums-add p step) n (inc n)))
	(when (< (* (n2d n) +double_2) far)
		(defq pts (list p) way :nil)
		(times +edge_steps
			(defq g (flat-slope p) t (doubles (neg (third g)) +double_0 (first g)))
			(when (> (nums-dot g g) +double_0)
				;along the side, the way it set off
				(if (< (nums-dot t (ifn way forward)) +double_0) (setq t (nums-scale t +double_-1)))
				(setq way t p (nums-add p (nums-scale t +edge_step)))
				;and back to as near the rock as it keeps
				(times 3 (setq p (nums-add p (nums-scale (flat-slope p)
					(max +double_-2 (min +double_2 (- +edge_clear (field_water p))))))))
				(push pts p)))
		(when (> (length pts) +edge_least)
			(leg-ask a (nums-scale (nums-add a (first pts)) +double_1/2) (first pts) :nil)
			(each! (lambda ()
					(defq i (* (!) 2))
					(leg-ask (elem-get pts i) (elem-get pts (inc i)) (elem-get pts (+ i 2)) :nil))
				'() 0 (/ (dec (length pts)) 2))
			(defq end (elem-get pts (* (/ (dec (length pts)) 2) 2)))
			(leg-ask end (nums-scale (nums-add end b) +double_1/2) b :nil)
			:t)))

(defun junction-ahead ()
	; (junction-ahead) -> cell
	;the junction of the open water round the eye most nearly the way it is
	;looking, not too near, and none with rock up to the surface on the way
	(defq cx (n2i (floor (/ (first eye) +cell))) cz (n2i (floor (/ (third eye) +cell))) best :nil best_score +double_0)
	(each! (lambda ()
			(defq z (!))
			(each! (lambda ()
					(defq c (list (!) 0 z) d (nums-sub (junction-of c) eye) dd (sqrt (nums-dot d d)))
					(when (and (> dd +ahead_near) (open-junction? c) (not (route-blocked? field_water (list eye (junction-of c)))))
						(defq score (nums-dot (nums-scale d (recip dd)) forward))
						(when (or (not best) (> score best_score)) (setq best c best_score score))))
				'() (dec cx) (+ cx 2)))
		'() (dec cz) (+ cz 2))
	best)

(defun head-out ()
	; (head-out)
	;the auto pilot taken on from where the eye is: in the caves from the
	;junction nearest, the way out, which it follows before it chooses for
	;itself again; over the reef to the junction most nearly ahead, its
	;first leg from the eye on the way it is looking, so it never turns
	;about when it takes over
	(setq came :nil escape (list) was :nil legs (list))
	(if pilot (def pilot :spin (doubles +double_0 +double_0 +double_0)))
	(cond
		((in-caves?)
			(setq going (nearest-junction)
				legs (list (list 0 (list (nums-sub (defq j (junction-of going)) (field_warp j))) :t (< (second going) 0) 0)))
			(if (< (second going) 0) (setq escape (way-out going))))
		(:t (setq going (junction-ahead))
			(defq j (junction-of going))
			(leg-ask (cat eye) (nums-add eye (nums-scale forward (min +ahead_most (* (far-from j) +double_1/2)))) j :nil))))

(defun pilot-aim (nav dt)
	; (pilot-aim nav dt) -> doubles
	;the place the auto pilot aims at: its first two legs laid end to end
	;as the Nav's path, and nav-aim-follow's aim along it, from where the
	;eye will be when its turn has caught up, sliding on as it goes, never a
	;place at a time; its place on its legs only goes on, into the next
	;leg, which is then the first. More legs as they are used. Over the reef a leg not
	;yet lined is lifted over the rock here, as the ways of the open water
	;carve nothing
	(while (< (length legs) 2) (choose-next))
	(defq leg (first legs) pts (second leg) n (length pts))
	(while (and (>= (elem-get leg 4) (- n 2)) (< (far-from (last pts)) +lead))
		(setq legs (slice legs 1 -1))
		(if (< (length legs) 2) (choose-next))
		(setq leg (first legs) pts (second leg) n (length pts)))
	(def nav :path (cat pts (second (second legs))) :cursor (elem-get leg 4)
		:look (n2i (* (n2d +look_on) (max +double_1 (/ speed +swim)))))
	(defq p (nav-aim-follow nav dt) cursor (get :cursor nav))
	;its place gone on into the next leg: this one done, the next from there
	(cond
		((>= cursor n)
			(setq legs (slice legs 1 -1))
			(elem-set (first legs) 4 (- cursor n))
			(if (< (length legs) 2) (choose-next)))
		((elem-set leg 4 cursor)))
	(unless (or (third leg) (elem-get leg 3))
		(times 8 (if (< (field_water p) +over_rock) (elem-set p 1 (+ (second p) +double_2))))
		(elem-set p 1 (min (second p) (- +surface +under_surface))))
	p)

(defun pilot-avoid (nav aim)
	; (pilot-avoid nav aim) -> doubles
	;the lines know nothing of the stones: the first place on the way to the
	;aim that is near one is moved clear of it, and aimed at, so the sub
	;steers round or over it
	(defq to (nums-sub aim eye))
	(ifn (some (lambda (along)
			(bind '(q moved) (clear-of-stones (nums-add eye (nums-scale to (n2d along))) +aim_clear))
			(if (> moved +double_0) q))
		'(0.35 0.7 1)) aim))

(defun far-from (p)
	; (far-from p) -> double
	;how far a place is from the eye
	(sqrt (nums-dot (defq d (nums-sub p eye)) d)))

(defun auto-turn (dt)
	; (auto-turn dt)
	;the auto pilot turned toward its line, a Nav's turn: the way it faces
	;and its up a quaternion, how fast it turns sprung toward the turn that
	;looks at its aim upright, damped more than just enough never to go
	;past it, as water damps a turn, stiffer in the caves, and no faster
	;than it can, both the more the faster it swims. Its up the world's, but near
	;straight up or down its own kept, the more the nearer, so it is the
	;same down a shaft as level, and never left upside down. The eye is
	;where the Nav is, it swims as it did, swim
	(defq aim (pilot-avoid pilot (pilot-aim pilot dt)) cave (elem-get (first legs) 3))
	(setq aim_way (unit (nums-sub aim eye)))
	;as nimble the faster it goes, so it follows a curve as well at speed:
	;stiffer and no less fast to turn, by how much faster than it swims
	(defq faster (max +double_1 (/ speed +swim)))
	(def pilot :k (* faster (if cave +cave_settle +auto_settle))
		:turn_most (* faster (if cave +cave_turn +turn_most)))
	(nav-turn-to pilot (quat-frame aim_way (nav-up-level pilot aim_way dt)) dt)
	(setq forward (get :way pilot) up (get :up pilot)))

(defun turn-eye ()
	; (turn-eye) -> dt
	;the eye turned toward where it is going, the auto pilot's aim or the
	;pointer's, eased, and no faster than it can; upright again of the
	;world's up, the less the nearer straight up or down it looks. Its ways
	;made square and of length 1 again. The seconds since last time
	(defq now (pii-time) dt (min (/ (n2d (- now turned)) +second) +dt_most))
	(setq turned now)
	;the pilot, a Nav, where the eye is
	(unless pilot
		(setq pilot (Nav eye forward up))
		(def pilot :zeta +pilot_zeta :lead (/ (n2d +aim_on) +swim) :lead_least (n2d +aim_on)
			:upright_from +upright_from :upright_over +upright_over :aim_fn pilot-aim :avoid_fn pilot-avoid
			:stick_reach +stick_reach :stick_dead +dead :point_rate +point_rate :point_most +stick_turn
			:lean +double_0))
	(def pilot :at eye :speed speed :way forward :up up :turn (quat-frame forward up))
	;the auto pilot's, or toward the pointer, a player's turn, still as it is
	;let go
	(cond
		((eql mode :auto) (auto-turn dt))
		(:t (def pilot :stick stick)
			(nav-turn-pointer pilot dt)
			(setq forward (get :way pilot) up (get :up pilot))))
	dt)

(defun clear-of-stones (p clear)
	; (clear-of-stones p clear) -> (p moved)
	;p moved out of the stones near it, and as far clear of them as asked,
	;each a rounded box of its kind, as it lies, turned and sized, and how
	;far it was moved: those near it as the world of them, lib/physics, has
	;them, a ball as far as each reaches; weed it swims through, it is not
	;in the world
	(defq moved +double_0)
	(each (lambda ((id &ignore))
			(bind '(& & & at bulk & kind (a up b)) (. stone_world :shape id))
			(defq d (nums-sub p at))
			(bind '(rx ry rz cy sq) (elem-get +stone_shapes kind))
			(defq k (/ clear bulk) rx (+ (n2d rx) k) ry (+ (n2d ry) k) rz (+ (n2d rz) k) cy (n2d cy)
				sq (n2d sq) qx (/ (nums-dot d a) bulk) qy (- (/ (nums-dot d up) bulk) cy) qz (/ (nums-dot d b) bulk)
				;how far out across, as square as the stone is, and up
				across (pow (+ (pow (abs (/ qx rx)) sq) (pow (abs (/ qz rz)) sq)) (recip sq))
				high (abs (/ qy ry)))
			(when (< (max across high) +double_1)
				;out the nearer way, through its top or bottom, or its side
				(defq to (if (> high across)
						(nums-add at (nums-scale (nums-add (nums-add (nums-scale a qx)
							(nums-scale up (+ cy (if (< qy +double_0) (neg ry) ry)))) (nums-scale b qz)) bulk))
						(nums-add at (nums-scale (nums-add (nums-add (nums-scale a (/ qx (max across +dt_least)))
							(nums-scale up (+ cy qy))) (nums-scale b (/ qz (max across +dt_least)))) bulk))))
				(setq moved (+ moved (sqrt (nums-dot (defq m (nums-sub to p)) m))) p to)))
		(. stone_world :near p clear))
	(list p moved))

(defun clear-of-rock (p)
	; (clear-of-rock p) -> doubles
	;p moved out of the rock it is in or too near, along the slope of the
	;field, a few steps, so never left in it
	(times 4
		(when (< (defq h (field_water p)) +clear)
			(defq e +double_1/5 g (doubles
					(- (field_water (nums-add p (doubles e +double_0 +double_0))) h)
					(- (field_water (nums-add p (doubles +double_0 e +double_0))) h)
					(- (field_water (nums-add p (doubles +double_0 +double_0 e))) h))
				gg (nums-dot g g))
			(if (> gg +double_0)
				(setq p (nums-add p (nums-scale g (/ (* (- +clear h) e) gg)))))))
	p)

(defun rock-way (p)
	; (rock-way p) -> doubles
	;the way the rock faces at p, out of it, of length 1
	(defq h (field_water p) e +double_1/5)
	(nums-scale (defq g (doubles
			(- (field_water (nums-add p (doubles e +double_0 +double_0))) h)
			(- (field_water (nums-add p (doubles +double_0 e +double_0))) h)
			(- (field_water (nums-add p (doubles +double_0 +double_0 e))) h)))
		(recip (max (sqrt (nums-dot g g)) +dt_least))))

(defun touch-new? (key way now)
	; (touch-new? key way now) -> :t | :nil
	;is a touch of a thing, the rock :rock or the stones :stones, anew: not
	;touched lately, or the rock facing well round from the way it did, way
	;:nil for the stones, which push every way as they lie together; it is
	;remembered as touched now, facing this way
	(cond
		((defq t (some (# (if (eql (first %0) key) %0)) touches))
			(defq fresh (or (> (- now (second t)) +thud_again) (and way (< (nums-dot way (third t)) +thud_turn))))
			(elem-set t 1 now)
			(elem-set t 2 way)
			fresh)
		((push touches (list key now way)) :t)))

(defun thud-play (hard)
	; (thud-play hard)
	;a thud as loud as the bump was hard, the softest the least loud
	(when thud_pcm
		(unless thud_stream (setq thud_stream (audio-stream-open-rpc +prop_rate 1)))
		(when thud_stream
			(defq loud (max +double_0 (min +double_1 (/ (- hard +thud_least) (- +thud_most +thud_least))))
				pcm (nums-convert (nums-scale thud_pcm (+ +thud_soft (* (- +double_1 +thud_soft) loud))) (shorts)) bytes (* (length pcm) 2))
			(ifn (audio-stream-left-rpc thud_stream) (setq thud_stream :nil)
				(audio-stream-write-rpc thud_stream (obj-set (str-alloc bytes) +str_data bytes pcm))))))

(defun sound-load (file peak)
	; (sound-load file peak) -> :nil | doubles
	;a sound of the app's data, 16 bit mono at +prop_rate, its samples as
	;numbers made as loud as peak at the most; :nil if it is not there or
	;not that
	(when (defq in (file-stream (cat *app_root* "data/" file)))
		(bind '(rate channels bits bytes) (WAV-info in))
		(when (and (= rate +prop_rate) (= channels 1) (= bits 16) (defq data (read-blk in bytes)))
			(defq pcm (nums-convert (push (shorts) data) (doubles) :t)
				most (reduce (lambda (m v) (max m (abs v))) pcm +double_1))
			(nums-scale pcm (/ peak most)))))

(defun stereo (l r)
	; (stereo l r) -> doubles
	;two channels as a stream takes them, a frame each, left first
	(apply (const doubles) (reduce! (lambda (out a b) (push out a b)) (list l r) (list))))

(defun ping-make ()
	; (ping-make) -> :nil | (ping echo_left echo_right quiet)
	;the sonar's ping, data/ping.wav, both sides alike, as loud as
	;+ping_loud of the scooter; its echo, data/echo.wav, higher and short,
	;on the left and on the right, so an echo is placed between by how much
	;of each; and silence as long as all of them. :nil with none of them
	(when (and (defq ping (sound-load "ping.wav" (* +prop_scale +ping_loud)))
			(defq echo (sound-load "echo.wav" +prop_scale)))
		(defq none (nums-scale echo +double_0)
			frames (+ (length ping) (length echo) (n2i (* +ping_reach +ping_unit))))
		(list (stereo ping ping) (stereo echo none) (stereo none echo)
			(nums-scale (stereo (defq all (apply (const doubles) (map! (# +double_0) '() 0 frames))) all) +double_0))))

(defun ping-range (way)
	; (ping-range way) -> :nil | double
	;how far the rock is from the sub the way given, if it is in reach:
	;stepped along by how far the water says the rock is at least; or a
	;stone, if one is nearer
	(defq d +double_1 hit :nil)
	(while (and (not hit) (< d +ping_reach))
		(if (< (defq h (field_water (nums-add eye (nums-scale way d)))) +ping_near)
			(setq hit d)
			(setq d (+ d (max +ping_step h)))))
	;and a stone in the way first, the world of them's ray
	(if (defq stone (. stone_world :cast eye way (ifn hit +ping_reach)))
		(second stone) hit))

(defun sonar-feed ()
	; (sonar-feed)
	;the sonar's ping now and then, and the echoes off the rock across the
	;front of the sub, each later and fainter the further its rock is, and
	;from the side its rock is on; none where no rock is in reach, the ping
	;ringing away into the open water
	(defq now (pii-time))
	(when (and ping_pcm (> (- now ping_at) +ping_every))
		(setq ping_at now)
		(unless sonar_stream (setq sonar_stream (audio-stream-open-rpc +prop_rate 2)))
		(when sonar_stream
			(setq echoes 0)
			(defq all (length ping_quiet) echo_n (length echo_left) right (cross forward up)
				out (cat ping_pcm (slice ping_quiet 0 (- all (length ping_pcm)))))
			(each (lambda (side)
					(defq a (* (n2d side) +ping_wide)
						way (nums-add (nums-scale forward (cos a)) (nums-scale right (sin a))))
					(when (defq d (ping-range way))
						(++ echoes)
						(defq at (* 2 (n2i (* d +ping_unit))) near (- +double_1 (/ d +ping_reach))
							loud (* +echo_most near near (cos a))
							pan (* (+ (n2d side) +double_1) (const (/ +double_pi +double_4))))
						(setq out (nums-add out (cat (slice ping_quiet 0 at)
							(nums-add (nums-scale echo_left (* loud (cos pan))) (nums-scale echo_right (* loud (sin pan))))
							(slice ping_quiet 0 (- all at echo_n)))))))
				+ping_ways)
			;never past what a sample holds, all of it made quieter if the
			;echoes together would be, not wrapped round to a crackle
			(defq most (reduce (lambda (m v) (max m (abs v))) out +double_1))
			(if (> most +sonar_most) (setq out (nums-scale out (/ +sonar_most most))))
			(defq pcm (nums-convert out (shorts)) bytes (* (length pcm) 2))
			(ifn (audio-stream-left-rpc sonar_stream) (setq sonar_stream :nil)
				(audio-stream-write-rpc sonar_stream (obj-set (str-alloc bytes) +str_data bytes pcm))))))

(defun prop-feed (dt)
	; (prop-feed dt)
	;the scooter's whine as the right button is held, quickly up, and dying
	;away as it is let go: written to its stream ahead of what is heard,
	;none when it has died away, as Space's boost
	(setq prop_gain (if sprinting
		(min +double_1 (+ prop_gain (* dt +prop_up)))
		(* prop_gain (- +double_1 (min +double_1 (* dt +prop_down))))))
	(when (and prop_pcm (> prop_gain +prop_quiet))
		(unless prop_stream (setq prop_stream (audio-stream-open-rpc +prop_rate 1)))
		(when prop_stream
			(cond
				((not (defq left (audio-stream-left-rpc prop_stream))) (setq prop_stream :nil))
				((< left +prop_ahead)
					(defq count (- (* 2 +prop_ahead) left) long (length prop_pcm) end (+ prop_at count)
						part (if (<= end long) (slice prop_pcm prop_at end)
							(cat (slice prop_pcm prop_at -1) (slice prop_pcm 0 (- end long))))
						pcm (nums-convert (nums-scale part (* +prop_loud prop_gain prop_gain)) (shorts))
						bytes (* (length pcm) 2))
					(setq prop_at (% end long))
					(audio-stream-write-rpc prop_stream (obj-set (str-alloc bytes) +str_data bytes pcm)))))))

(defun swim (dt)
	; (swim dt)
	;the eye on along its way, kept off the rock, and under the surface, by
	;its body of the mover: the speed it would go its flow, eased to, up to a
	;sprint and down again as fast as each, its going turned to where it
	;faces. The auto pilot slows for a sharp turn, and on a leg of the caves
	;till its line comes; if it makes no headway for a long while it asks for
	;a new line from where it is
	(defq want (if sprinting +sprint (if (and (eql mode :auto) aim_way)
			(* +swim (max +least_speed (nums-dot forward aim_way))
				(if (and (elem-get (first legs) 3) (not (elem-get (first legs) 2))) +unlined_speed +double_1))
			+swim)))
	(. mover :face sub forward)
	(. mover :set sub :flow (nums-scale forward want) :drag (if sprinting +sprint_up +sprint_down))
	(setq speed (sqrt (nums-dot (defq v (. mover :velocity sub)) v)))
	;touching the rock, shoved off it, and out of the stones: a thud as it
	;touches the rock or the stones anew, as loud as it hit, its speed into
	;what it hit; none for what it goes on touching, a while after, or
	;facing much the same
	(defq hard +double_0 fresh :nil touch_at (pii-time))
	(when (< (field_water eye) +clear)
		(if frame_log (. frame_log :add :touches))
		(setq hard (max +double_0 (neg (nums-dot v (defq way (rock-way eye)))))
			fresh (touch-new? :rock way touch_at)))
	(bind '(p moved) (clear-of-stones (defq q (clear-of-rock eye)) +stone_clear))
	(. mover :shove sub (nums-sub p eye) :t)
	(when (> moved +double_0)
		(if frame_log (. frame_log :add :touches))
		(defq way (nums-scale (defq d (nums-sub p q)) (recip (max (sqrt (nums-dot d d)) +dt_least))))
		(setq hard (max hard (neg (nums-dot v way))))
		(if (touch-new? :stones :nil touch_at) (setq fresh :t)))
	(setq touches (filter (# (<= (- touch_at (second %0)) +thud_again)) touches))
	(when fresh
		(if frame_log (. frame_log :add :thuds))
		(thud-play hard))
	(defq now (pii-time))
	(cond
		((or (not was) (nql mode :auto)) (setq was (cat eye) was_at now))
		((> (sqrt (nums-dot (defq d (nums-sub eye was)) d)) +headway) (setq was (cat eye) was_at now))
		((> (- now was_at) +stuck_long)
			;stuck a long while: a new line from where it is
			(++ stucks)
			(setq was (cat eye) was_at now)
			(head-out))))

(defun chunk-key (cx cy cz)
	; (chunk-key cx cy cz) -> num
	;the place of a chunk, the same wherever round the world it is met
	(+ (% (+ (% cx +chunks_round) +chunks_round) +chunks_round)
		(* +chunks_round (+ (% (+ (% cz +chunks_round) +chunks_round) +chunks_round)
			(* +chunks_round (- cy +chunk_low))))))

(defun chunk-centre (cx cy cz)
	; (chunk-centre cx cy cz) -> doubles
	(doubles (+ (* (n2d cx) +chunk) +half_chunk) (+ (* (n2d cy) +chunk) +half_chunk) (+ (* (n2d cz) +chunk) +half_chunk)))

(defun chunk-ask (key cx cy cz cubes)
	; (chunk-ask key cx cy cz cubes) -> num
	;a chunk asked of the farm, a new mesh number for it
	(defq mid mesh_next)
	(setq mesh_next (inc mesh_next))
	(. outs :insert mid key)
	(. jobs :add (list (setf-> (str-alloc +chunk_size)
		(+chunk_id mid) (+chunk_cubes cubes) (+chunk_x cx) (+chunk_y cy) (+chunk_z cz))))
	mid)

(defun chunks-want ()
	; (chunks-want)
	;the chunks round the eye that are not known asked of the farm, the
	;nearest first, as many as may be out; and those gone far behind let go
	;of
	(bind '(ex ey ez) eye)
	(setq outs_most (max 2 (. jobs :size)))
	(defq reach (+ view +chunk_reach) r (inc (n2i (/ reach +chunk))) ecx (n2i (floor (/ ex +chunk)))
		ecy (n2i (floor (/ ey +chunk))) ecz (n2i (floor (/ ez +chunk))) wanted (list)
		cubes (elem-get +cubes_of detail))
	(when (< (. jobs :out) outs_most)
		(each! (lambda ()
				(defq cy (!))
				(each! (lambda ()
						(defq cz (!))
						(each! (lambda ()
								(defq cx (!) d (nums-sub (chunk-centre cx cy cz) eye) far (sqrt (nums-dot d d))
									known (. chunks :find (chunk-key cx cy cz)))
								;ahead first: what is behind counts as further
								(cond
									((>= far reach))
									((and (not known) (> (abs (field_water (chunk-centre cx cy cz))) +no_surface))
										;surely all rock or all water, none of the farm's time
										(. chunks :insert (chunk-key cx cy cz) (list :none 0 cx cy cz cubes 0 '())))
									((not known)
										(push wanted (list (- far (* +double_1/2 (nums-dot d forward))) cx cy cz)))))
							'() (- ecx r) (+ ecx r 1)))
					'() (- ecz r) (+ ecz r 1)))
			'() (max +chunk_low (- ecy r)) (inc (min +chunk_high (+ ecy r))))
		(sort wanted (lambda ((a &ignore) (b &ignore)) (if (< a b) -1 (if (> a b) 1 0))))
		(each (lambda ((& cx cy cz))
				(when (< (. jobs :out) outs_most)
					(defq key (chunk-key cx cy cz))
					(. chunks :insert key (list :out (chunk-ask key cx cy cz cubes) cx cy cz cubes (pii-time) '()))))
			wanted))
	;those far behind let go of, the render's mesh with them
	(defq gone (list) out_of (+ view +let_go))
	(. chunks :each (lambda (key (state mid cx cy cz &ignore))
			(defq d (nums-sub (chunk-centre cx cy cz) eye))
			(when (> (nums-dot d d) (* out_of out_of))
				(push gone key)
				(when (eql state :mesh)
					(. render :unmesh mid)
					(. stone_world :detach key)))))
	(each (# (. chunks :erase %0)) gone))

(defun stones-ask ()
	; (stones-ask)
	;the meshes of the kinds of stone asked of the farm, each two ways
	(each! (lambda ()
			(defq kind (/ (!) +stone_fines) fine (% (!) +stone_fines))
			(. jobs :add (list (setf-> (str-alloc +chunk_size)
				(+chunk_id (+ +mesh_stone (!))) (+chunk_cubes 1) (+chunk_x kind) (+chunk_y fine)
				(+chunk_z 0)))))
		'() 0 (* +lying_kinds +stone_fines)))

(defun fishes-ask ()
	; (fishes-ask)
	;the meshes of the kinds of fish asked of the farm
	(each! (lambda ()
			(. jobs :add (list (setf-> (str-alloc +chunk_size)
				(+chunk_id (+ +mesh_fish (!))) (+chunk_cubes 2) (+chunk_x (!)) (+chunk_y 0) (+chunk_z 0)))))
		'() 0 +fish_kinds))

(defun stones-of (data n)
	; (stones-of data n) -> ((mesh model stone at bulk weed kind axes) ...)
	;the stones of a chunk, each its mesh's first number, its matrix, laid
	;on its floor, turned and sized, what of it the shaders want, and for
	;the diver to bump into it, its kind and its own three ways
	(map! (lambda ()
			(defq i (!))
			(bind '(x y z ux uy uz yaw bulk kind sun sky basalt floor)
				(slice data (* i +stone_size) (* (inc i) +stone_size)))
			(defq up (doubles ux uy uz) a (doubles (cos yaw) +double_0 (sin yaw))
				a (unit (nums-sub a (nums-scale up (nums-dot a up)))) b (cross a up))
			(bind '(ax ay az) (nums-scale a bulk))
			(bind '(vx vy vz) (nums-scale up bulk))
			(bind '(bx by bz) (nums-scale b bulk))
			(list (+ +mesh_stone (* (n2i kind) +stone_fines))
				(doubles ax vx bx x ay vy by y az vz bz z +double_0 +double_0 +double_0 +double_1)
				(doubles sun sky basalt floor) (doubles x y z) bulk (>= (n2i kind) +stone_kinds) (n2i kind) (list a up b)))
		'() 0 n))

(defun chunk-came (msg)
	; (chunk-came msg)
	;a chunk made: its mesh given to the render, drawn from the next frame,
	;unless it has been let go of meanwhile, and its stones kept; or the
	;mesh of a kind of stone
	(defq mid (getf msg +chunk_reply_id) data (slice msg +chunk_reply_data -1))
	(when (< -1 (- mid +mesh_fish) +fish_kinds)
		(. render :mesh mid (push (doubles) data) (first fish_files))
		(elem-set fish_had (- mid +mesh_fish) :t))
	(when (< -1 (- mid +mesh_stone) (const (* +lying_kinds +stone_fines)))
		(. render :mesh mid (push (doubles) data)
			(first (if (< (/ (- mid +mesh_stone) +stone_fines) +stone_kinds) stone_files weed_files)))
		(elem-set stones_had (- mid +mesh_stone) :t))
	(when (defq leg (. leg_outs :find mid))
		(. leg_outs :erase mid)
		(leg-came leg data))
	(when (defq key (. outs :find mid))
		(. outs :erase mid)
		(when (defq known (. chunks :find key))
			(defq n (getf msg +chunk_reply_stones) at (* n +stone_size 8))
			(cond
				((= (length data) at) (elem-set known 0 :none))
				(:t (. render :mesh mid (push (doubles) (slice data at -1)) (first reef_files))
					(elem-set known 0 :mesh)
					(elem-set known 7 (defq lying (stones-of (push (doubles) (slice data 0 at)) n)))
					;its stones in the world of them, weed swum through
					(each (lambda (stone)
							(bind '(& & & at bulk weed &ignore) stone)
							(unless weed
								(. stone_world :narrow
									(. stone_world :attach key at :nil (list :sphere (* bulk +stone_reach))) :nil stone)))
						lying))))))

(defun view-sides (right up spread_x)
	; (view-sides right up spread_x) -> (side ...)
	;the four sides of what the frame sees, each the way in
	(map (lambda ((s across)) (unit (nums-add (nums-scale forward s) across)))
		(list (list spread_x (nums-scale right +double_-1)) (list spread_x right)
			(list +spread_y (nums-scale up +double_-1)) (list +spread_y up))))

(defun stone-seed (at)
	; (stone-seed at) -> double
	;a stone's own number, 0 to 1, of where it is, in doubles, so the same
	;each frame wherever it is round the world
	(bind '(x y z) at)
	(defq s (* (sin (+ (* x +seed_x) (* y +seed_y) (* z +seed_z))) +seed_k))
	(- s (floor s)))

(defun eye-model (m)
	; (eye-model m) -> mat4x4
	;a model's matrix moved from the world's middle to the eye's
	(defq m (cat m))
	(elem-set m 3 (- (elem-get m 3) (first eye)))
	(elem-set m 7 (- (elem-get m 7) (second eye)))
	(elem-set m 11 (- (elem-get m 11) (third eye)))
	m)

(defun chunk-draws (view_mat sides pvals)
	; (chunk-draws view_mat sides pvals) -> draws
	;the chunks with a surface in what the frame sees, each where it is,
	;and then the stones on them near enough, the bigger the further, the
	;finer mesh near to; what hides most first. The pixel shader's inputs
	;the one list for all, packed the once
	(defq out (list) stones (list) vvals_eye (list 'eye eye) lens (list 'lens view_mat))
	(. chunks :each (lambda (key (state mid cx cy cz & & lying))
			(when (eql state :mesh)
				(bind '(rx ry rz) (nums-sub (chunk-centre cx cy cz) eye))
				;its own surface if it is in view, its stones if it is near
				;enough that one of them, reaching out over the next, may be
				(defq inside (reduce (lambda (most (sx sy sz)) (min most (+ (* rx sx) (* ry sy) (* rz sz)))) sides +far_clear))
				(if (> inside (neg +chunk_reach))
					(push out (list mid (list (list 'model (mat4x4-translate (- (* (n2d cx) +chunk) (first eye))
							(- (* (n2d cy) +chunk) (second eye)) (- (* (n2d cz) +chunk) (third eye)))) lens vvals_eye) pvals)))
				(when (> inside (neg (+ +chunk_reach +stone_over)))
					(each (lambda ((mesh model stone at bulk weed &ignore))
							(bind '(sx sy sz) (defq d (nums-sub at eye)))
							(defq far (sqrt (nums-dot d d)) near_by (* far (recip (+ +double_1 bulk)))
								fine (cond ((< near_by +stone_fine) 0) ((< near_by +stone_coarse) 1) (2))
								reach (* view +stone_hidden))
							(and (< far reach)
								(elem-get stones_had (+ (- mesh +mesh_stone) fine))
								(every (lambda ((wx wy wz)) (> (+ (* sx wx) (* sy wy) (* sz wz)) (neg (* bulk +double_2)))) sides)
								;its matrix from the eye, (eye-model)
								(defq model (eye-model model))
								(push stones (if weed
									(list (+ mesh fine) (list (list 'model model) lens vvals_eye (list 'stone stone)
											(list 'stone_reach reach) (list 'time seconds))
										pvals '() weed_files)
									(list (+ mesh fine) (list (list 'model model) lens vvals_eye (list 'stone stone)
											(list 'stone_reach reach) (list 'seed (stone-seed at)))
										pvals '() stone_files)))))
						lying)))))
	(setq stones_drawn (length stones))
	(cat out stones))

;the frame log, for tuning, on with +frame_log, lib/debug/frame_log.inc:
;each second its rate and detail, the milliseconds a frame of each part of
;it and of the GPU, and what was drawn, how the view turned and where the
;eye is, to tests/scratch/water/frames.log. For the Test user, a session
;of a test's, the app quits after a minute, to look at the log of it. The
;log, and the stucks so far
(defq +frame_log :nil frame_log (if +frame_log
		(FrameLog "tests/scratch/water/frames.log" '("eye" "fish" "chunks" "draws" "render")))
	stucks 0)

(defun frame-mark (part)
	; (frame-mark part)
	;a part of the frame ended, for the frame log
	(if frame_log (. frame_log :mark part)))

(defun frame-log (rate now)
	; (frame-log rate now) -> :t | :nil
	;a second's line of the frame log; :t to quit
	(defq meshed 0 out 0 log frame_log)
	(. chunks :each (lambda (key (state &ignore)) (case state (:mesh (++ meshed)) (:out (++ out)))))
	(and (> (. log :line now rate (list (list "detail" detail))
				(list (list "gpu" (. log :ms (* (ifn (get :took render) 0) (max (get :frames log) 1)))) "|"
					(list "chunks" meshed) (list "out" out) (list "stones" stones_drawn) (list "fish" fish_drawn)
					(list "upsides" (. log :got :upsides)) (list "caves" (. log :got :caves)) (list "nods" (. log :got :nods))
					(list "stucks" stucks) (list "echoes" echoes) (list "legs" (length legs)) (list "junction" going)
					(list "eye" (map (const n2i) eye)) (list "touches" (. log :got :touches)) (list "thuds" (. log :got :thuds))))
			60)
		(eql (get '*env_node_user*) "Test")))

(defun draw-frame ()
	;the eye turned and on, the chunks it wants asked for, and the frame of
	;it, the reef and the water behind it
	(if frame_log (. frame_log :frame))
	(defq dt (turn-eye))
	(swim dt)
	(prop-feed dt)
	(sonar-feed)
	(when frame_log
		(. frame_log :turn forward up)
		(if (in-caves?) (. frame_log :add :caves)))
	(frame-mark 0)
	(setq schools (schools-move schools field_water eye forward view dt +surface prop_gain mover
		(lambda (p r) (map (lambda ((id &ignore)) (list (. stone_world :centre id) (second (. stone_world :bound id))))
			(. stone_world :near p r)))))
	(. mover :step dt)
	;the eye under the surface
	(if (> (second eye) (const (- +surface +under_surface))) (elem-set eye 1 (const (- +surface +under_surface))))
	(frame-mark 1)
	(setq view (+ view (* (- (elem-get +view_of detail) view) (min +double_1 dt)))
		murk (/ +murk_gone view)
		;as open as the water straight up from the eye, eased, so it goes
		;dark in the caves and light again out of them
		open (+ open (* (- (+ +open_least (* (- +double_1 +open_least)
			(field_lit eye (const (doubles +double_0 +double_1 +double_0)) +double_2))) open) (min +double_1 (* dt +open_ease)))))
	(if (= (% frames +want_every) 0) (chunks-want))
	(frame-mark 2)
	(defq right (cross forward up) seconds (/ (n2d (- (pii-time) began)) +second)
		spread_x (/ (* +spread_y (n2d (first size))) (n2d (second size)))
		look (cat right (const (doubles +double_0)) up (const (doubles +double_0))
			(nums-scale forward +double_-1) (const (doubles +double_0 +double_0 +double_0 +double_0 +double_1)))
		;the frustum's sides are at the near plane, the frame's at a distance of 1
		lens (mat4x4-frustum (neg (* spread_x +near)) (* spread_x +near) (const (* +spread_y +near))
			(const (neg (* +spread_y +near))) +near (+ view +chunk_reach))
		;the view's turn only: what is drawn is placed from the eye, in doubles,
		;so the GPU's singles have small numbers anywhere round the world, the
		;depth of a stone and the sand under it kept apart
		view_mat (mat4x4-mul lens look)
		pvals (list (list 'eye eye) (list 'forward forward) (list 'time seconds) (list 'lamp +lamp) (list 'murk murk)
			(list 'open open)))
	(defq fish (fish-draws schools fish_had +mesh_fish (list 'lens view_mat) (list 'eye eye) pvals fish_files
		seconds eye forward view))
	(setq fish_drawn (length fish))
	(defq draws (cat (chunk-draws view_mat (view-sides right up spread_x) pvals) fish
		;the water, behind all the reef, after it
		(list (list +mesh_water '()
			(list (list 'forward forward) (list 'right right) (list 'up up)
				(list 'spread (doubles spread_x +spread_y)) (list 'eye eye) (list 'time seconds) (list 'murk murk)
				(list 'open open) (list 'lamp +lamp))
			'() water_files))))
	(frame-mark 3)
	(. render :frame draws)
	(frame-mark 4)
	(++ frames))

(defun set-size (wanted)
	;the window the size of the frame wanted, now; the canvas of it is made
	;when the next frame is given, (new-canvas)
	(bind '(w h) wanted)
	(def *backdrop* :min_width w :min_height h :want_width w :want_height h)
	(. *window* :fit)
	(. (ifn shown *canvas*) :change 0 0 w h)
	(. *window* :layout)
	(setq want wanted))

(defun new-canvas ()
	;a canvas of the size that is wanted, if that is not the size there is,
	;not put on show till a frame of it is whole, (show-canvas)
	(unless (eql want size)
		(unless shown (setq shown *canvas*))
		(setq *canvas* (make-canvas want) size want)
		(. render :canvas *canvas*)))

(defun show-canvas ()
	;a frame of the new canvas is whole: it goes on show, the old let go of
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
	(scatter (Emap) :version +config_version :mode :auto :large :nil :music :t :detail 2))

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
	;the next frame, if the last is whole
	(unless (. render :busy)
		(defq start (pii-time))
		(if shown (show-canvas))
		(new-canvas)
		(draw-frame)
		(setq given start frame_took (- (pii-time) start))))

(defun detail-fit (rate now)
	;the detail to what the GPU keeps up with, by the rate of the second
	;gone: less, two seconds running slow; more, three seconds running fast,
	;not for half a minute after it was less; a step at a time
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
		render (Render-tris *canvas* (first reef_files) (second reef_files)))
	(def render :clear +argb_black)
	(. render :mesh +mesh_water (sky-verts) (first water_files))
	;the native functions of the field the diver and the auto pilot ask of
	(defq field (shader-load (cat *app_root* "field.shader")))
	(setq field_water (shader-vp-func field 'water) field_junction (shader-vp-func field 'junction)
		field_link (shader-vp-func field 'link_open) field_bend (shader-vp-func field 'bend)
		field_warp (shader-vp-func field 'warp) field_lit (shader-vp-func field 'lit_by)
		field_bed (shader-vp-func field 'bed))
	;the chunks, made by a farm of children on this machine's nodes, off
	;this one, each slotted in as it comes
	(setq stone_world (Collide +stone_cell (doubles +world +double_0 +world)))
	(setq jobs (Jobs (cat *app_root* "child.lisp") (elem-get select +select_task)
		(elem-get select +select_reply) (list (max 1 (dec (length (lisp-nodes :t))))) :t))
	(stones-ask)
	(fishes-ask)
	(setq prop_pcm (sound-load "prop.wav" +prop_peak) thud_pcm (sound-load "thud.wav" +prop_scale) ping_at (- (pii-time) (- +ping_every +ping_first)))
	(when (defq sonar (ping-make))
		(bind '(ping echo_l echo_r quiet) sonar)
		(setq ping_pcm ping echo_left echo_l echo_right echo_r ping_quiet quiet))
	(config-load)
	(setq detail (min 4 (. *config* :find :detail)) view (elem-get +view_of detail) mode (. *config* :find :mode)
		;at a junction over the reef somewhere by chance, to go on from,
		;facing any way
		going (launch-cell) eye (clear-of-rock (junction-of going))
		forward (launch-way eye) up (doubles +double_0 +double_1 +double_0)
		turned (pii-time) cave_left (pii-time))
	;the sub a body of the mover, the eye its place, going where it faces
	(setq sub (. mover :add eye (nums-scale forward speed)))
	(. mover :set sub :align +sub_align :rate +sub_shove)
	(. *mode* :set_selected (find mode '(:auto :mouse)))
	(head-out)
	(if (. *config* :find :large) (set-size +large))
	;the user's Jukebox favourites behind it, if it had them on
	(setq music (Music select +select_music) music_on (. *config* :find :music))
	(if music_on (. music :start))
	(music-show)
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(def *window* :tip_mbox (elem-get select +select_tip))
	(. *window* :open)
	(setq due (+ (pii-time) +timer_rate))
	(mail-timeout (elem-get select +select_timer) +timer_rate 0)
	(while id
		(bind '(idx msg) (. render :read select +select_timer))
		(cond
			((not idx)
				(and (not (. render :busy)) (>= (- (pii-time) given) (max +timer_rate (* frame_took +pace)))
					(next-frame)))
			((. music :mail idx msg))
			((= idx +select_task) (. jobs :launched msg))
			((= idx +select_reply)
				(when (. jobs :answered msg) (chunk-came msg)))
			((= idx +select_tip) (. *window* :tip msg))
			((= idx +select_main)
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					((= id +event_max) (set-size +large))
					((= id +event_mode)
						(setq mode (elem-get '(:auto :mouse) (. *mode* :get_selected)))
						;the auto pilot taken on finds its way from where the eye is,
						;out of the caves if it is in them
						(if (eql mode :auto) (head-out)))
					((= id +event_min) (set-size +small))
					((= id +event_prev) (. music :move -1))
					((= id +event_next) (. music :move 1))
					((= id +event_music)
						(setq music_on (not (. music :title)))
						(if music_on (. music :start) (. music :stop))
						(music-show))
					((and (= (getf msg +ev_msg_target_id) (. *canvas* :get_id))
							(= (getf msg +ev_msg_type) +ev_type_mouse))
						(bind '(w h) size)
						(setq stick (list (- (/ (n2d (* 2 (getf msg +ev_msg_mouse_rx))) (n2d w)) +double_1)
							(- +double_1 (/ (n2d (* 2 (getf msg +ev_msg_mouse_ry))) (n2d h)))))
						;the right button held is a sprint. A first press says which
						;button by its number, 3 the right, and a move with them down
						;says them as bits, as the window takes them
						(defq buttons (getf msg +ev_msg_mouse_buttons))
						(and (= pressed 0) (/= buttons 0)
							(setq buttons (case buttons (3 +right_button) (:t buttons))))
						(setq pressed buttons sprinting (bits? buttons +right_button))
						(. *window* :event msg))
					((and (= (getf msg +ev_msg_target_id) (. *canvas* :get_id))
							(= (getf msg +ev_msg_type) +ev_type_exit))
						(setq stick :nil pressed 0 sprinting :nil)
						(. *window* :event msg))
					((. *window* :event msg))))
			(:t ;the timer, a frame a tick, if the last frame is not still out
				(defq start (pii-time))
				(next-frame)
				(defq took (- (pii-time) start))
				(setq due (max (+ due (max +timer_rate (* took +pace))) (pii-time)))
				(mail-timeout (elem-get select +select_timer) (max 1 (- due (pii-time))) 0)
				(. render :tick)
				(. jobs :refresh 20000000)
				(when (>= (- (defq now (pii-time)) since) 1000000)
					(defq rate (/ (* frames 1000000) (- now since)))
					(detail-fit rate now)
					(. *status* :set_text (cat (str rate) " frames a second, "
							(. render :how) (if (> detail 0) (cat ", detail less " (str detail)) "")
							", depth " (str (n2i (- +surface (second eye))))
							(if (and (eql mode :auto) (nempty? escape)) ", heading out" "")
							(if (defq song (. music :title)) (cat ", " song) "")))
					(and +frame_log (frame-log rate now) (setq id :nil))
					(setq frames 0 since now)))))
	(config-save)
	(. music :stop)
	(. jobs :close)
	(. render :close)
	(. *window* :close))
