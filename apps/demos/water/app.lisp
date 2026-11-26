(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "lib/math/matrix.inc")
(import "lib/gpu/render_farm.inc")
(import "lib/gpu/shapes.inc")
(import "apps/media/jukebox/music.inc")
(import "service/audio/app.inc")
(import "./chunk.inc")
(import "./route.inc")
(import "./fish.inc")
(import "lib/math/quaternion.inc")

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
	;the diver: where the eye is, the way it looks and its up, how fast it
	;turns at most, how soon it comes to the turn it wants, how soon it
	;comes upright, how fast it swims, its lamp, and the pointer
	eye :nil forward :nil up :nil turned 0 stick :nil yaw +double_0 pitch +double_0
	+turn_most (n2d 0.3) +turn_ease +double_1 +upright (n2d 0.6) +swim +double_2
	;the share of the way to its turn the auto pilot turns a second, as a
	;rate, over the reef and in the caves: past it by none, water damps a turn
	+auto_settle (n2d 1.2) +cave_settle (n2d 2.5)
	;its speed now, eased up to a sprint while the right button is held and
	;down again after, how fast either way, a second, and the buttons
	speed +double_2 +sprint +double_6 +sprint_up +double_1 +sprint_down (n2d 0.7)
	sprinting :nil pressed 0 +right_button 4 +lamp (n2d 2.4)
	;the scooter's whine as the right button is held: its loop's samples,
	;made at the start, the stream it is played on, where in the loop it
	;is, its rate, how far ahead of what is heard it is kept, how loud at
	;most and now, eased up as the button is held and dying away after,
	;how fast up, a second, and how fast it dies, a share a second; and
	;the loop: the motor's hum and its overtones, the chop of the blades,
	;and how muffled the churn of the water
	prop_pcm :nil prop_stream :nil prop_at 0 +prop_rate 22050 +prop_ahead 1100
	+prop_loud (n2d 0.7) prop_gain +double_0 +prop_up +double_4 +prop_down (n2d 1.5) +prop_quiet (n2d 0.01)
	+prop_hum (n2d 120) +prop_chop (n2d 24) +prop_muffle (n2d 0.06) +prop_scale (n2d 14000)
	;the hum's overtones, each how many times the hum, how loud and its
	;phase; how much the chop beats the hum and the churn; the noise's range
	+prop_tones ''((1 0.45 0) (2 0.22 0.7) (3 0.1 1.3)) +beat_least (n2d 0.6) +beat_more (n2d 0.4)
	+churn_least (n2d 0.7) +churn_more (n2d 0.3) +prop_noise 65536 +prop_noise_half (n2d 32768)
	;how long the noise's start is faded from what follows its end
	+prop_fade 2205
	;a bump into the rock: its thud's samples, made at the start, the stream
	;it is played on, whether the diver is against the rock or a stone now,
	;how hard a bump is softest, units a second into it, and loudest;
	;the thud, how long, how deep, how fast it dies, and how much knock
	thud_pcm :nil thud_stream :nil touching :nil +thud_least (n2d 1.5) +thud_most (n2d 5)
	+thud_long 8820 +thud_deep (n2d 70) +thud_die (n2d 14) +thud_knock (n2d 0.5) +thud_knock_die (n2d 90)
	;how loud the softest bump is, of the loudest, and the least step of time
	+thud_soft (n2d 0.3) +dt_least (n2d 0.001)
	;the sub's sonar, in stereo: its ping's samples, its echo's in the left
	;and in the right, silence to place the echoes in, made at the start,
	;the stream it is played on, when the last went out, how often, the
	;first how soon; the ping's note, how fast it sounds and dies, how long,
	;and how loud, of the scooter's; the echo's, higher and short; how far
	;it hears the rock, how many samples later an echo is for a unit
	;further, how loud the nearest is, the ways across the front it looks,
	;how far round the widest is, how near the rock the search must come
	;and its least step
	ping_pcm :nil echo_left :nil echo_right :nil ping_quiet :nil sonar_stream :nil ping_at 0 echoes 0
	+ping_every 4000000 +ping_first 2000000 +ping_note (n2d 1800) +ping_rise (n2d 300) +ping_die (n2d 4.4)
	+ping_long 16538 +ping_loud (n2d 0.3) +echo_note (n2d 2700) +echo_rise (n2d 400) +echo_die (n2d 22)
	+echo_long 4410 +ping_reach (n2d 80) +ping_unit (n2d 300) +echo_most (n2d 0.7)
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
	+stick_reach (n2d 0.3) +dead (n2d 0.15) +stick_turn (n2d 0.25)
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
	+cave_rest 40000000 cave_left 0
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
	(defq id mesh_next rough (resample (list a mid b))
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
	(cond
		((in-caves?)
			(setq going (nearest-junction)
				legs (list (list 0 (list (nums-sub (defq j (junction-of going)) (field_warp j))) :t (< (second going) 0) 0)))
			(if (< (second going) 0) (setq escape (way-out going))))
		(:t (setq going (junction-ahead))
			(defq j (junction-of going))
			(leg-ask (cat eye) (nums-add eye (nums-scale forward (min +ahead_most (* (far-from j) +double_1/2)))) j :nil))))

(defun route-ahead ()
	; (route-ahead) -> doubles
	;the place the auto pilot aims at: a few places on along its leg from
	;where the eye is along it, which only goes on, the nearest of the next
	;few places to the eye; the next leg once one is done, more legs as they
	;are used. Over the reef a leg not yet lined is lifted over the rock
	;here, as the ways of the open water carve nothing
	(while (< (length legs) 2) (choose-next))
	(defq leg (first legs))
	(while (progn
			(bind '(& pts & & at) leg)
			(defq n (length pts) near (far-from (elem-get pts at)))
			(each! (lambda (p) (when (< (defq d (far-from p)) near) (setq near d at (!))))
				(list pts) (inc at) (min n (+ at (n2i (* (n2d +look_on) (max +double_1 (/ speed +swim)))))))
			(elem-set leg 4 at)
			(and (>= at (- n 2)) (< (far-from (last pts)) +lead)))
		(setq legs (slice legs 1 -1))
		(if (< (length legs) 2) (choose-next))
		(setq leg (first legs)))
	;as far on as it would swim in the same time, faster or slower
	(defq p (cat (elem-get pts (min (dec n) (+ at (n2i (* (n2d +aim_on) (max +double_1 (/ speed +swim)))))))))
	(unless (or (third leg) (elem-get leg 3))
		(times 8 (if (< (field_water p) +over_rock) (elem-set p 1 (+ (second p) +double_2))))
		(elem-set p 1 (min (second p) (- +surface +under_surface))))
	;the lines know nothing of the stones: the first place on the way to the
	;aim that is near one is moved clear of it, and aimed at, so the sub
	;steers round or over it
	(defq to (nums-sub p eye) aim p)
	(some (lambda (along)
			(bind '(q moved) (clear-of-stones (nums-add eye (nums-scale to (n2d along))) +aim_clear))
			(if (> moved +double_0) (setq aim q)))
		'(0.35 0.7 1))
	aim)

(defun far-from (p)
	; (far-from p) -> double
	;how far a place is from the eye
	(sqrt (nums-dot (defq d (nums-sub p eye)) d)))

(defun beyond-dead (v)
	; (beyond-dead v) -> double
	(cond
		((> v +dead) (/ (- v +dead) (const (- +double_1 +dead))))
		((< v (neg +dead)) (/ (+ v +dead) (const (- +double_1 +dead))))
		(+double_0)))

(defun auto-turn (dt)
	; (auto-turn dt)
	;the auto pilot turned toward the line, a place a little on along it:
	;the way it faces and its up a turn, quaternions, turned a share of the
	;way to the turn that looks at the place, its up eased to the world's,
	;the share the more the longer the step, and no more than it can turn,
	;the faster it swims the more, nimbly in the caves. It nears what it
	;wants and never swings past it, as water damps a turn, and is the same
	;looking up or down a shaft as level
	(setq aim_way (unit (nums-sub (route-ahead) eye)) yaw +double_0 pitch +double_0)
	(defq most (* dt (max +double_1 (/ speed +swim)) (if (elem-get (first legs) 3) +cave_turn +turn_most))
		lean (min +double_1 (* dt +upright))
		level (unit (nums-add (nums-scale up (- +double_1 lean)) (nums-scale (const (doubles +double_0 +double_1 +double_0)) lean)))
		now_turn (quat-frame forward up) want_turn (quat-frame aim_way level)
		share (- +double_1 (exp (neg (* dt (if (elem-get (first legs) 3) +cave_settle +auto_settle)))))
		turn (quat-toward now_turn want_turn (min most (* share (quat-angle now_turn want_turn)))))
	(setq forward (quat-vec3-rot turn (const (doubles +double_0 +double_0 +double_-1)))
		up (quat-vec3-rot turn (const (doubles +double_0 +double_1 +double_0)))))

(defun pointer-turn (dt right)
	; (pointer-turn dt right)
	;the eye turned toward the pointer, eased, a lean, no faster than it
	;can; upright again of the world's up, the less the nearer straight up or
	;down it looks
	(defq heading (if stick (unit (nums-add forward (nums-add (nums-scale right (* (beyond-dead (first stick)) +stick_reach))
				(nums-scale up (* (beyond-dead (second stick)) +stick_reach)))))
			forward)
		want_yaw (* (nums-dot heading right) +double_2) want_pitch (* (nums-dot heading up) +double_2)
		k (min +double_1 (* dt +turn_ease))
		most +stick_turn)
	(setq yaw (+ yaw (* k (- (max (neg most) (min most want_yaw)) yaw)))
		pitch (+ pitch (* k (- (max (neg most) (min most want_pitch)) pitch))))
	(defq a (* yaw dt) b (* pitch dt)
		f (nums-add (nums-scale forward (cos a)) (nums-scale right (sin a))))
	(setq forward (unit (nums-add (nums-scale f (cos b)) (nums-scale up (sin b))))
		up (unit (nums-sub (nums-scale up (cos b)) (nums-scale f (sin b)))))
	(defq level (nums-sub (const (doubles +double_0 +double_1 +double_0)) (nums-scale forward (second forward)))
		level_by (sqrt (nums-dot level level)))
	(when (> level_by +double_1/2)
		(setq up (nums-add up (nums-scale (nums-sub (nums-scale level (recip level_by)) up)
			(min +double_1 (* dt +upright +double_2 (- level_by +double_1/2)))))))
	(setq up (unit (cross (cross forward up) forward))))

(defun turn-eye ()
	; (turn-eye) -> dt
	;the eye turned toward where it is going, the auto pilot's aim or the
	;pointer's, eased, and no faster than it can; upright again of the
	;world's up, the less the nearer straight up or down it looks. Its ways
	;made square and of length 1 again. The seconds since last time
	(defq now (pii-time) dt (min (/ (n2d (- now turned)) +second) +dt_most) right (cross forward up))
	(setq turned now)
	(if (eql mode :auto) (auto-turn dt) (pointer-turn dt right))
	dt)

(defun clear-of-stones (p clear)
	; (clear-of-stones p clear) -> (p moved)
	;p moved out of the stones near it, and as far clear of them as asked,
	;each a rounded box of its kind, as it lies, turned and sized, and how
	;far it was moved; weed it swims through
	(defq moved +double_0 cx (n2i (floor (/ (first p) +chunk))) cy (n2i (floor (/ (second p) +chunk)))
		cz (n2i (floor (/ (third p) +chunk))))
	(each! (lambda ()
			(defq dz (!))
			(each! (lambda ()
					(defq dy (!))
					(each! (lambda ()
							(and (defq known (. chunks :find (chunk-key (+ cx (!)) (+ cy dy) (+ cz dz))))
								(eql (first known) :mesh)
								(each (lambda ((& & & at bulk weed kind (a up b)))
										(defq d (nums-sub p at) reach (+ (* bulk +stone_reach) clear))
										(and (not weed) (< (nums-dot d d) (* reach reach))
											(bind '(rx ry rz cy sq) (elem-get +stone_shapes kind))
											(defq k (/ clear bulk) rx (+ (n2d rx) k) ry (+ (n2d ry) k) rz (+ (n2d rz) k) cy (n2d cy)
												sq (n2d sq) qx (/ (nums-dot d a) bulk) qy (- (/ (nums-dot d up) bulk) cy) qz (/ (nums-dot d b) bulk)
												;how far out across, as square as the stone is, and up
												across (pow (+ (pow (abs (/ qx rx)) sq) (pow (abs (/ qz rz)) sq)) (recip sq))
												high (abs (/ qy ry)))
											(< (max across high) +double_1)
											;out the nearer way, through its top or bottom, or its side
											(defq to (if (> high across)
													(nums-add at (nums-scale (nums-add (nums-add (nums-scale a qx)
														(nums-scale up (+ cy (if (< qy +double_0) (neg ry) ry)))) (nums-scale b qz)) bulk))
													(nums-add at (nums-scale (nums-add (nums-add (nums-scale a (/ qx (max across +dt_least)))
														(nums-scale up (+ cy qy))) (nums-scale b (/ qz (max across +dt_least)))) bulk))))
											(setq moved (+ moved (sqrt (nums-dot (defq m (nums-sub to p)) m))) p to)))
									(elem-get known 7))))
						'() -1 2))
				'() -1 2))
		'() -1 2)
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

(defun prop-make ()
	; (prop-make) -> doubles
	;a second of the scooter, a loop: the motor's hum, 120 a second and
	;its overtones, beating with the chop of the blades, and the churn of
	;the water, noise muffled round the loop so it joins with no click,
	;all of whole cycles a second so the loop does too
	(defq n +prop_rate m +prop_fade y +double_0 k (/ +double_2pi (n2d n))
		tones (map (lambda (tone) (map (const n2d) tone)) +prop_tones)
		;the noise muffled, twice over, a little longer than the loop
		long (map! (lambda ()
				(setq y (+ y (* +prop_muffle (- (- (/ (n2d (random +prop_noise)) +prop_noise_half) +double_1) y)))))
			'() 0 (+ n m))
		long (progn (setq y +double_0) (map (lambda (x) (setq y (+ y (* +prop_muffle (- x y))))) long))
		;and its start faded from what came after its end, so the end runs
		;into the start with no step
		churn (map! (lambda ()
				(defq i (!) x (elem-get long i))
				(if (< i m)
					(+ (* x (/ (n2d i) (n2d m))) (* (elem-get long (+ n i)) (- +double_1 (/ (n2d i) (n2d m)))))
					x))
			'() 0 n))
	(apply (const doubles) (map (lambda (c)
			(defq t (* k (n2d (!))) beat (+ +beat_least (* +beat_more (sin (* t +prop_chop)))))
			(* +prop_scale (+ (* beat (reduce (lambda (sum (over loud phase)) (+ sum (* loud (sin (+ (* t +prop_hum over) phase)))))
					tones +double_0))
				(* +double_3 c (+ +churn_least (* +churn_more beat))))))
		churn)))

(defun thud-make ()
	; (thud-make) -> doubles
	;a bump into the rock, muffled by the water: a low thump falling away,
	;a little lower as it goes, and a knock of muffled noise at the start
	(defq y +double_0 k (/ +double_2pi (n2d +prop_rate)))
	(apply (const doubles) (map! (lambda ()
			(defq t (/ (n2d (!)) (n2d +prop_rate))
				x (- (/ (n2d (random +prop_noise)) +prop_noise_half) +double_1))
			(setq y (+ y (* +prop_muffle (- x y))))
			(* +prop_scale (+ (* (exp (* (neg +thud_die) t)) (sin (* k (n2d (!)) +thud_deep (- +double_1 (* t +double_1/2)))))
				(* +thud_knock (exp (* (neg +thud_knock_die) t)) y +double_3))))
		'() 0 +thud_long)))

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

(defun ring (note rise die long loud)
	; (ring note rise die long loud) -> doubles
	;a note rung, quickly up, ringing away
	(defq k (* +double_2pi note (recip (n2d +prop_rate))))
	(apply (const doubles) (map! (lambda ()
			(defq t (/ (n2d (!)) (n2d +prop_rate)))
			(* +prop_scale loud (- +double_1 (exp (* (neg rise) t))) (exp (* (neg die) t)) (sin (* k (n2d (!))))))
		'() 0 long)))

(defun stereo (l r)
	; (stereo l r) -> doubles
	;two channels as a stream takes them, a frame each, left first
	(apply (const doubles) (reduce! (lambda (out a b) (push out a b)) (list l r) (list))))

(defun ping-make ()
	; (ping-make) -> (ping echo_left echo_right quiet)
	;the sonar's ping, long and high, both sides alike; its echo, higher
	;and short, on the left and on the right, so an echo is placed between
	;by how much of each; and silence as long as all of them
	(defq ping (ring +ping_note +ping_rise +ping_die +ping_long +ping_loud)
		echo (ring +echo_note +echo_rise +echo_die +echo_long +double_1)
		none (nums-scale echo +double_0)
		frames (+ +ping_long +echo_long (n2i (* +ping_reach +ping_unit))))
	(list (stereo ping ping) (stereo echo none) (stereo none echo)
		(nums-scale (stereo (defq all (apply (const doubles) (map! (# +double_0) '() 0 frames))) all) +double_0)))

(defun ping-range (way)
	; (ping-range way) -> :nil | double
	;how far the rock is from the sub the way given, if it is in reach:
	;stepped along by how far the water says the rock is at least
	(defq d +double_1 hit :nil)
	(while (and (not hit) (< d +ping_reach))
		(if (< (defq h (field_water (nums-add eye (nums-scale way d)))) +ping_near)
			(setq hit d)
			(setq d (+ d (max +ping_step h)))))
	hit)

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
	;the eye on along its way, kept off the rock, and under the surface.
	;The auto pilot slows for a sharp turn, and on a leg of the caves till
	;its line comes; if it makes no headway for a long while it asks for a
	;new line from where it is
	(setq speed (+ speed (* (- (if sprinting +sprint (if (and (eql mode :auto) aim_way)
				(* +swim (max +least_speed (nums-dot forward aim_way))
					(if (and (elem-get (first legs) 3) (not (elem-get (first legs) 2))) +unlined_speed +double_1))
				+swim)) speed)
			(min +double_1 (* dt (if sprinting +sprint_up +sprint_down)))))
		eye (nums-add eye (nums-scale forward (* speed dt))))
	;touching the rock, pushed off it, and out of the stones: a thud the
	;once as it hits, as loud as it hit, none again till it is clear
	(defq hit +double_0)
	(when (< (defq h (field_water eye)) +clear)
		(++ touches)
		(setq hit (- +clear h)))
	(setq eye (clear-of-rock eye))
	(bind '(p moved) (clear-of-stones eye +stone_clear))
	(setq eye p)
	(when (> moved +double_0)
		(++ touches)
		(setq hit (+ hit moved)))
	(cond
		((= hit +double_0) (setq touching :nil))
		((not touching)
			(setq touching :t)
			(thud-play (/ hit (max dt +dt_least)))))
	(defq top (- +surface +under_surface))
	(if (> (second eye) top) (elem-set eye 1 top))
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
				(if (eql state :mesh) (. render :unmesh mid)))))
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
					(elem-set known 7 (stones-of (push (doubles) (slice data 0 at)) n)))))))

(defun view-sides (right up spread_x)
	; (view-sides right up spread_x) -> (side ...)
	;the four sides of what the frame sees, each the way in
	(map (lambda ((s across)) (unit (nums-add (nums-scale forward s) across)))
		(list (list spread_x (nums-scale right +double_-1)) (list spread_x right)
			(list +spread_y (nums-scale up +double_-1)) (list +spread_y up))))

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
					(push out (list mid (list (list 'model (mat4x4-translate (* (n2d cx) +chunk) (* (n2d cy) +chunk)
							(* (n2d cz) +chunk))) lens vvals_eye) pvals)))
				(when (> inside (neg (+ +chunk_reach +stone_over)))
					(each (lambda ((mesh model stone at bulk weed &ignore))
							(bind '(sx sy sz) (defq d (nums-sub at eye)))
							(defq far (sqrt (nums-dot d d)) near_by (* far (recip (+ +double_1 bulk)))
								fine (cond ((< near_by +stone_fine) 0) ((< near_by +stone_coarse) 1) (2))
								reach (* view +stone_hidden))
							(and (< far reach)
								(elem-get stones_had (+ (- mesh +mesh_stone) fine))
								(every (lambda ((wx wy wz)) (> (+ (* sx wx) (* sy wy) (* sz wz)) (neg (* bulk +double_2)))) sides)
								(push stones (if weed
									(list (+ mesh fine) (list (list 'model model) lens vvals_eye (list 'stone stone)
											(list 'stone_reach reach) (list 'time seconds))
										pvals '() weed_files)
									(list (+ mesh fine) (list (list 'model model) lens vvals_eye (list 'stone stone)
											(list 'stone_reach reach))
										pvals '() stone_files)))))
						lying)))))
	(setq stones_drawn (length stones))
	(cat out stones))

;the frame log, for tuning, on with +frame_log: each second its rate and
;detail, the milliseconds a frame of each part of it and of the GPU, and
;what was drawn and where the eye is, to tests/scratch/water/frames.log.
;For the Test user, a session of a test's, the app quits after a minute,
;to look at the log of it. The parts' times, when the last part ended, the
;frames of this second, the stream and when it began
(defq +frame_log :nil frame_parts (list 0 0 0 0 0) frame_at 0 frame_count 0 frame_stream :nil frame_began 0
	touches 0 stucks 0 nods 0 nod_was +double_0 nod_way +double_0 +nod_least (n2d 0.01))

(defun frame-nod ()
	; (frame-nod)
	;for the frame log, a nod of the view each time it turns from pitching
	;up to pitching down or back, more than a little
	(when +frame_log
		(defq d (- (second forward) nod_was))
		(when (> (abs d) +nod_least)
			(if (< (* d nod_way) +double_0) (++ nods))
			(setq nod_way d nod_was (second forward)))))

(defun frame-mark (part)
	; (frame-mark part)
	;a part of the frame ended, for the frame log
	(when +frame_log
		(defq now (pii-time))
		(elem-set frame_parts part (+ (elem-get frame_parts part) (- now frame_at)))
		(setq frame_at now)))

(defun frame-log (rate now)
	; (frame-log rate now) -> :t | :nil
	;a second's line of the frame log; :t to quit
	(unless frame_stream
		(setq frame_stream (file-stream "tests/scratch/water/frames.log" +file_open_write) frame_began now))
	(defq ms (map (lambda (us) (defq a (/ us (max frame_count 1))) (cat (str (/ a 1000)) "." (str (/ (% a 1000) 100))))
			(push (cat frame_parts) (* (ifn (get :took render) 0) (max frame_count 1))))
		meshed 0 out 0)
	(. chunks :each (lambda (key (state &ignore)) (case state (:mesh (++ meshed)) (:out (++ out)))))
	(write-line frame_stream (cat (str (/ (- now frame_began) 1000000)) "s " (str rate) " fps detail " (str detail)
		" | eye " (elem-get ms 0) " fish " (elem-get ms 1) " chunks " (elem-get ms 2) " draws " (elem-get ms 3)
		" render " (elem-get ms 4) " gpu " (elem-get ms 5)
		" | chunks " (str meshed) " out " (str out) " stones " (str stones_drawn) " fish " (str fish_drawn)
		" nods " (str nods) " stucks " (str stucks) " echoes " (str echoes) " legs " (str (length legs)) " junction " (str going) " eye " (str (map (const n2i) eye))
		" touches " (str touches)))
	(setq frame_parts (list 0 0 0 0 0) frame_count 0 touches 0 nods 0)
	(stream-flush frame_stream)
	(and (eql (get '*env_node_user*) "Test") (> (- now frame_began) 60000000)))

(defun draw-frame ()
	;the eye turned and on, the chunks it wants asked for, and the frame of
	;it, the reef and the water behind it
	(if +frame_log (setq frame_at (pii-time)))
	(defq dt (turn-eye))
	(swim dt)
	(prop-feed dt)
	(sonar-feed)
	(frame-nod)
	(frame-mark 0)
	(setq schools (schools-move schools field_water eye forward view dt +surface prop_gain))
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
		view_mat (mat4x4-mul lens (mat4x4-mul look (mat4x4-translate (neg (first eye)) (neg (second eye)) (neg (third eye)))))
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
	(++ frames)
	(++ frame_count))

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
	(setq jobs (Jobs (cat *app_root* "child.lisp") (elem-get select +select_task)
		(elem-get select +select_reply) (list (max 1 (dec (length (lisp-nodes :t))))) :t))
	(stones-ask)
	(fishes-ask)
	(setq prop_pcm (prop-make) thud_pcm (thud-make) ping_at (- (pii-time) (- +ping_every +ping_first)))
	(bind '(ping echo_l echo_r quiet) (ping-make))
	(setq ping_pcm ping echo_left echo_l echo_right echo_r ping_quiet quiet)
	(config-load)
	(setq detail (min 4 (. *config* :find :detail)) view (elem-get +view_of detail) mode (. *config* :find :mode)
		;at a junction over the reef, to go on from
		going (list 10 0 10) eye (clear-of-rock (nums-sub (defq j (junction-of going)) (field_warp j)))
		forward (doubles +double_0 +double_0 +double_1) up (doubles +double_0 +double_1 +double_0)
		turned (pii-time) cave_left (pii-time))
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
