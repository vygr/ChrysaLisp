---
name: onslaught
display-name: ChrysaLisp Onslaught
description: Use when writing, reviewing, or modifying the Onslaught 2D entity-component engine (apps/games/onslaught/) — sprites, components, collisions, and cinematic sequencing.
---

# ChrysaLisp Onslaught 2D Entity-Component Engine

## Domain & Scope

*	**Target Application:** `apps/games/onslaught/`

*	**Primary Files:** `sprite.inc`, `addons.inc`, `collisions.inc`,
	`fanatic.inc`, `enemy.inc`, `title.inc`, `widgets.inc`, `map.inc`,
	`sky.inc`, `utils.inc`, `assets.inc`, `enums.inc`, `app_impl.lisp`

*	**Architectural Heritage:** Directly based on the original 1989 Commodore
	Amiga / Atari ST game *Onslaught* by Chris Hinsley. The architectural
	insights developed during the creation of this engine—treating sprites as
	autonomous, composable, intelligent entities communicating through event
	hooks and ordered component pipelines—served as the original conceptual
	inspiration for the Taos Operating System, its Virtual Processor (VP),
	and ultimately ChrysaLisp itself.

---

## 1. Core Engineering Philosophy

The Onslaught engine is built around four unifying principles:

1.	**Sprites Are Autonomous Actors:**
	Each sprite is an independent entity hosting its own coordinate state,
	visual atlas, and an ordered component execution pipeline.

2.	**Strict Component Isolation (The `this` / `that` Model):**
	Components are encapsulated instances created via `(env 1)`. They hold
	their private state in `that`, while global entity state resides in
	`this` (`Sprite`). No component variables are flattened onto the sprite.

3.	**Chained Lifecycle Sequencing:**
	Cinematic sequences and state transitions do not rely on global step
	counters. They advance naturally through death callbacks (`:sp_death`)
	and terminal animation frames (`-1`), where one entity's completion
	automatically triggers the next phase.

4.	**Zero Display-Scale Pollution:**
	The simulation logic operates exclusively within an unscaled 1x coordinate
	space (320x240 screen, 16x16 tiles, 32x32 player hull). Only the GUI
	rendering bridge applies `:zoom`.

---

## 2. The Entity-Component Model (`Sprite`)

### 2.1 The Host Object

Defined in `apps/games/onslaught/sprite.inc`, `Sprite` subclasses ChrysaLisp's
core scene-graph node `View`:

```vdu
(defclass Sprite (canvas_l canvas_r width height tag &optional death) (View)
	; (Sprite canvas_l canvas_r width height tag [death]) -> sprite
	(def this :color 0
		:sp_x 0 :sp_y 0 :sp_w width :sp_h height
		:sp_frame_w width :sp_frame_h height
		:sp_canvas_l canvas_l :sp_canvas_r canvas_r :sp_canvas_tag tag
		:sp_frame 0 :sp_dir 1 :sp_oy 0 :sp_components (list)
		:sp_death (ifn death :nil death) :sp_type 0 :sp_flags 0 :sp_dead :nil
		:sp_hitpnt 0 :sp_addon :nil)
	(.-> this (:sp_set_pos 0 0) (:sp_set_size width height)))
```

Key state properties on `this`:

*	`:sp_x`, `:sp_y`, `:sp_w`, `:sp_h`: Unscaled 1x logical coordinates and
	collision dimensions.

*	`:sp_frame_w`, `:sp_frame_h`: Fixed native tile dimensions in the sprite
	sheet (e.g., 32x32 for Fanatic, 16x16 for addons).

*	`:sp_canvas_l`, `:sp_canvas_r`: Preloaded directional atlas sheets.
	Facing direction (`:sp_dir`, `-1` for left, `1` for right) selects which
	canvas is sampled.

*	`:sp_canvas_tag`: Metadata symbol (`:fanatic`, `:16x16_lr`, `:32c32`,
	etc.) used by `update-sprite-canvas` during zoom changes.

*	`:sp_type`: Bitmask identifying the entity type (`+ftp_*`). Always a
	power-of-two bitmask defined via `(bits +ftp 0 ...)`, never an index.

*	`:sp_flags`: Bitmask of runtime entity flags (`+fsp_*`), including
	`+fsp_collide` and `+fsp_action` defined via `(bits +fsp 0 ...)`.

*	`:sp_frame`: Current atlas frame index. Setting this to `-1` triggers
	immediate destruction via `:sp_kill`.

*	`:sp_components`: List of component environments executed sequentially.

*	`:sp_death`: Optional callback closure invoked when the sprite dies.

*	`:sp_dead`: Guard flag set upon death to prevent re-entrant callbacks.

*	`:sp_hitpnt`: Hit point counter or remaining item usage count.

*	`:sp_addon`: Optional weapon or status addon child attached to this sprite.

---

### 2.2 The `(this that)` Invocation Contract

Every component is an `(env 1)` environment holding its own local variables.
During each frame tick, `update-sprite-layers` invokes `(. sprite :sp_update)`
across all active sprites in each layer.

The update loop evaluates each component sequentially in the order it was
added:

```vdu
(defmethod :sp_update ()
	; (. sprite :sp_update) -> sprite
	(if (or (= (get :sp_frame this) -1) (get :sp_dead this))
		(. this :sp_kill)
		(progn
			(each (lambda (comp)
				(unless (or (= (get :sp_frame this) -1) (get :sp_dead this))
					(if (env? comp)
						((get :update comp) this comp)
						(comp this :nil))))
				(get :sp_components this))
			(when (or (= (get :sp_frame this) -1) (get :sp_dead this))
				(. this :sp_kill))))
	this)
```

Parameters passed to component update procedures:

*	`this`: The `Sprite` entity (`View`). Accesses position, bounds, frame,
	direction, and destruction methods.

*	`that`: The `Component` instance (`env 1`). Accesses private component
	state (velocities, animation tables, collision masks).

If any component sets `:sp_frame` to `-1` or marks `:sp_dead`, subsequent
components in the pipeline are skipped immediately for the rest of that frame.

---

### 2.3 Known Component Index Access

Components reside **strictly** within the `:sp_components` list. No component
instances are stored as redundant properties on `Sprite`.

When an entity needs to reference a specific component (e.g.,
`player-man-update` manipulating the player's animation table `AT`), it indexes
directly into `:sp_components` using a named compile-time constant:

```vdu
(defq +man_comp_at 0)

; access at component at index 0
(defq man_at (elem-get (get :sp_components this) +man_comp_at))
```

This mirrors the fixed struct member offset of the original C++ engine.

---

### 2.4 Destruction & Death Hook Contract

Setting `:sp_frame` to `-1` or calling `(. sprite :sp_kill)` unlinks the sprite
from its view layer and executes the death callback:

```vdu
(defmethod :sp_kill ()
	; (. sprite :sp_kill) -> sprite
	(unless (get :sp_dead this)
		(def this :sp_dead :t :sp_frame -1)
		(when (defq death (get :sp_death this))
			(def this :sp_death :nil)
			(death this))
		(. this :sub))
	this)

(defmethod :sp_set_frame (f)
	; (. sprite :sp_set_frame f) -> sprite
	(if (= f -1)
		(. this :sp_kill)
		(when (/= f (get :sp_frame this))
			(def this :sp_frame f)))
	this)
```

`:sp_dead` guarantees that `:sp_death` and `(:sub)` run exactly once, even if
`:sp_kill` is invoked multiple times.

---

## 3. The Standard Component Suite (`sprite.inc`)

All standard components are defined in `apps/games/onslaught/sprite.inc`.
Their constructors instantiate a private single-bucket environment `(env 1)`
and explicitly return `that`.

### 3.1 Procedural Component Wrapper (`CP`)

Wraps an arbitrary procedural function into a pipeline component:

```vdu
(defun cp-update (this that)
	(when (defq proc (get :proc that))
		(proc this that))
	that)

(defun CP (proc)
	; (CP proc) -> component
	(def (defq that (env 1)) :update (const cp-update) :proc proc)
	that)
```

---

### 3.2 Kinematic Vector Movement (`MV`)

Applies continuous velocity, acceleration, and terminal limits:

*	**Constructor:** `(MV [vx vy ax ay max_vx max_vy])`

*	**Local State on `that`:** `:vx`, `:vy`, `:ax`, `:ay`, `:max_vx`,
	`:max_vy`.

*	**Update:** Applies acceleration, clamps to limits, updates `:sp_pos` on
	`this`.

```vdu
(defun mv-update (this that)
	(bind '(x y) (. this :sp_get_pos))
	(defq ax (get :ax that) ay (get :ay that)
		max_vx (get :max_vx that) max_vy (get :max_vy that)
		vx (max (neg max_vx) (min max_vx (+ (get :vx that) ax)))
		vy (max (neg max_vy) (min max_vy (+ (get :vy that) ay))))
	(set that :vx vx :vy vy)
	(. this :sp_set_pos (+ x vx) (+ y vy))
	that)

(defun MV (&optional vx vy ax ay max_vx max_vy)
	; (MV [vx vy ax ay max_vx max_vy]) -> component
	(def (defq that (env 1)) :update (const mv-update)
		:vx (or vx 0) :vy (or vy 0)
		:ax (or ax 0) :ay (or ay 0)
		:max_vx (or max_vx 100) :max_vy (or max_vy 100))
	that)
```

---

### 3.3 Discrete Table Movement (`MT`)

Steps an entity through a scripted series of relative offsets
`((dx dy) ...)`:

*	**Constructor:** `(MT speed table &optional track)`

*	**Local State on `that`:** `:speed`, `:table`, `:count`, `:index`,
	`:track`.

*	**Tracking Mode:** If `:track` is non-nil, offsets are added relative to the
	tracked sprite's position; otherwise, they are relative to `this`.

```vdu
(defun mt-update (this that)
	(when (defq table (get :table that))
		(when (> (defq speed (get :speed that)) 0)
			(when (>= (defq count (inc (get :count that))) speed)
				(defq count 0 idx (get :index that))
				(bind '(dx dy) (elem-get table idx))
				(if (defq trk (get :track that))
					(bind '(tx ty) (. trk :sp_get_pos))
					(bind '(tx ty) (. this :sp_get_pos)))
				(. this :sp_set_pos (+ tx dx) (+ ty dy))
				(set that :index (if (= (inc idx) (length table)) 0 (inc idx))))
			(set that :count count)))
	that)

(defun MT (speed table &optional track)
	; (MT speed table [track]) -> component
	(def (defq that (env 1)) :update (const mt-update)
		:speed (or speed 1) :table table :count 0 :index 0
		:track (ifn track :nil track))
	that)
```

---

### 3.4 Linear Bresenham Interpolation (`ML`)

Interpolates an entity along a straight-line vector to exact target
coordinates:

*	**Constructor:** `(ML [speed])`

*	**Path Initialization:** `(init-ml this that x y x1 y1 [speed])`

*	**Local State on `that`:** `:target_x`, `:target_y`, `:speed`, `:count`,
	`:slope`, `:dx`, `:dy`, `:d1x`, `:d2x`, `:d1y`, `:d2y`.

```vdu
(defun ml-update (this that)
	(defq speed (get :speed that))
	(when (> speed 0)
		(defq slope (get :slope that) count (get :count that)
			dx (get :dx that) dy (get :dy that)
			d1x (get :d1x that) d2x (get :d2x that)
			d1y (get :d1y that) d2y (get :d2y that))
		(bind '(x y) (. this :sp_get_pos))
		(while (and (> speed 0) (> count 0))
			(-- count)
			(setq slope (- slope dy))
			(if (< slope 0)
				(setq slope (+ slope dx) x (+ x d1x) y (+ y d1y))
				(setq x (+ x d2x) y (+ y d2y)))
			(-- speed))
		(when (<= count 0)
			(setq count -1 x (get :target_x that) y (get :target_y that)))
		(set that :slope slope :count count)
		(. this :sp_set_pos x y))
	that)

(defun init-ml (this that x y x1 y1 &optional speed)
	; (init-ml sprite ml x y x1 y1 [speed]) -> ml
	(when speed (set that :speed speed))
	(. this :sp_set_pos x y)
	(defq dx (- x1 x) dy (- y1 y)
		d1x (cond ((< dx 0) -1) ((> dx 0) 1) (:t 0)) d2x d1x
		d1y (cond ((< dy 0) -1) ((> dy 0) 1) (:t 0)) d2y 0
		abs_dx (abs dx) abs_dy (abs dy) tmpi 0)
	(when (< abs_dx abs_dy)
		(setq d2y d1y d2x 0 tmpi abs_dx abs_dx abs_dy abs_dy tmpi))
	(set that :target_x x1 :target_y y1
		:dx abs_dx :dy abs_dy :d1x d1x :d2x d2x :d1y d1y :d2y d2y
		:count abs_dx :slope (>> abs_dx 1))
	that)

(defun ML (&optional speed)
	; (ML [speed]) -> component
	(def (defq that (env 1)) :update (const ml-update)
		:speed (or speed 0) :count -1 :target_x 0 :target_y 0
		:slope 0 :dx 0 :dy 0 :d1x 0 :d2x 0 :d1y 0 :d2y 0)
	that)
```

---

### 3.5 Table Animation (`AT`)

Drives multi-frame sprite atlas animations:

*	**Constructor:** `(AT speed table &optional loop)`

*	**Local State on `that`:** `:speed`, `:table`, `:count`, `:index`, `:loop`.

*	**Termination Contract:**
	*	Terminal `-1`: Automatically invokes `(. this :sp_kill)`.
	*	Non-looping (`:loop :nil`): When the last frame is displayed, sets
		`:speed 0 :table :nil`, allowing one-shot action controllers to detect
		completion.
	*	Looping (`:loop :t`): Automatically wraps index to 0.

```vdu
(defun at-update (this that)
	(when (defq table (get :table that))
		(when (> (defq speed (get :speed that)) 0)
			(when (>= (defq count (inc (get :count that))) speed)
				(defq count 0 idx (get :index that) f (elem-get table idx))
				(cond
					((= f -1)
						(. this :sp_kill))
					((and (ifn (get :loop that) :t) (= (inc idx) (length table)))
						; non-looping animation reached end
						(. this :sp_set_frame f)
						(set that :speed 0 :table :nil :count 0 :index 0))
					(:t
						(. this :sp_set_frame f)
						(set that :index (if (= (inc idx) (length table)) 0 (inc idx))))))
			(set that :count count)))
	that)

(defun AT (speed table &optional loop)
	; (AT speed table [loop]) -> component
	(def (defq that (env 1)) :update (const at-update)
		:speed (or speed 1) :table table :count 0 :index 0
		:loop (ifn (nil? loop) loop :t))
	that)
```

---

### 3.6 Collision Component (`CL`)

Executes collision checks at a specific point in the component pipeline:

*	**Constructor:** `(CL layer type table)`

*	**Local State on `that`:** `:layer`, `:type`, `:table`.

*	**Collision Primitives:**
	*	`(sprite-collide layer x y w h type)`: Scans `layer` for the first live
		sprite overlapping bounding box `(x y w h)` matching `type` mask.
		No `ignore_sp` argument is needed because collisions in Onslaught are
		strictly bipartite across separate layers.

*	**Dispatch Logic:**
	*	If `:table` is a list, indexes by target type bit:
		`(log2 (get :sp_type hit))`.
	*	If `:table` is a function, calls `(table this hit)` directly.

```vdu
(defun sprite-collide (layer x y w h type)
	; (sprite-collide layer x y w h type) -> sprite | :nil
	(defq xw (+ x w) yh (+ y h))
	(some (lambda (sp)
		(when (and (Sprite? sp)
				   (/= (get :sp_frame sp) -1)
				   (ifn (get :sp_dead sp) :t)
				   (or (= type -1) (/= 0 (logand (or (get :sp_type sp) 0) type))))
			(bind '(tx ty tw th) (. sp :sp_get_bounds))
			(if (and (< x (+ tx tw)) (< tx xw)
					 (< y (+ ty th)) (< ty yh))
				sp)))
		(. layer :children)))

(defun cl-update (this that)
	(when (and (defq lyr (get :layer that))
			   (/= (get :sp_frame this) -1)
			   (ifn (get :sp_dead this) :t))
		(bind '(x y w h) (. this :sp_get_bounds))
		(when (defq hit (sprite-collide lyr x y w h (get :type that)))
			(defq tbl (get :table that))
			(cond
				((list? tbl)
					(when (defq idx (log2 (or (get :sp_type hit) 0)))
						(when (< -1 idx (length tbl))
							(when (defq handler (elem-get tbl idx))
								(handler this hit)))))
				(tbl
					(tbl this hit)))))
	that)

(defun CL (layer type table)
	; (CL layer type table) -> component
	(def (defq that (env 1)) :update (const cl-update)
		:layer layer :type (ifn type -1 type) :table table)
	that)
```

---

### 3.7 Viewport Boundary Culling (`offscreen-update`)

Procedural component checking visibility relative to `*world_scroll*`. Invokes
`(. this :sp_kill)` when completely outside view bounds:

```vdu
(defun offscreen-update (this &optional that)
	; offscreen killing component
	(bind '(x y w h) (. *world_scroll* :get_relative this))
	(bind '(ww wh) (. *world_scroll* :get_size))
	(when (or (< x (neg w)) (< y (neg h)) (>= x ww) (>= y wh))
		(. this :sp_kill))
	this)
```

---

## 4. Collision & Combat Subsystem (`collisions.inc`)

### 4.1 Architecture

Collision logic is decoupled from entities and centralized in
`apps/games/onslaught/collisions.inc`:

1.	**Player State:** Defines `*man_power*` and `*man_strength*`.

2.	**Collision Handlers:** Functions with signature `(this hit)` (e.g.,
	`cl-man-hits-enemy`, `cl-man-hits-dragging-enemy`, `cl-man-hits-monk`,
	`cl-man-hits`, `cl-man-hits-banner`, `cl-man-missile-hits`).

3.	**Dispatch Tables:** `*man_collision_table*` is a list of 21 function
	references indexed directly by `(log2 type)` (matching C++
	`man_collision_handlers[]`).

---

### 4.2 Pipeline Sequencing in `Fanatic`

Player collisions are not checked in a monolithic batch; they are inserted
into the component list in exact C++ sequence:

```vdu
(defclass Fanatic () (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic)
	(def this :sp_type +ftp_man :sp_dir 1 :man_xv 0 :man_yv 0 :man_walk_tick 0
		:action_playing :nil)
	(.-> this
		; 0: at (man weapon attack body animations)
		(:sp_add_component (AT 0 :nil :nil))
		; 1: cp (input, movement, jumping, and falling physics)
		(:sp_add_component (CP (const player-man-update)))
		; 2: cp (ducking height and y bounds adjustment)
		(:sp_add_component (CP (const cp-manduck)))
		; 3..7: cl1..cl5 (ordered collision checks)
		(:sp_add_component (CL *layer_missiles* -1 *man_collision_table*))
		(:sp_add_component (CL *layer_enemies*
			(logior +ftp_tower +ftp_horse +ftp_boarrider +ftp_carpet +ftp_skeleton_horse)
			*man_collision_table*))
		(:sp_add_component (CL *layer_enemies*
			(logior +ftp_knight +ftp_skeleton +ftp_monk)
			*man_collision_table*))
		(:sp_add_component (CL *layer_enemies* +ftp_footman *man_collision_table*))
		(:sp_add_component (CL *layer_bans* -1 *man_collision_table*))
		(:sp_set_frame +frm_fanatic_l_stance)))
```

---

## 5. Addon Weapon & Item Subsystem (`addons.inc`)

### 5.1 Reusable `Addon` Sprite Class

Weapons and items that attach to the player are modeled by `Addon`:

```vdu
(defclass Addon (man mt_table at_speed at_table)
	(Sprite *img_frm_16x16_l* *img_frm_16x16_r* 16 16 :16x16_lr)
	; (Addon man mt_table at_speed at_table) -> addon sprite
	(def this :sp_dir (get :sp_dir man))
	; set initial position immediately to eliminate 1-frame spawn lag
	(bind '(mx my) (. man :sp_get_pos))
	(bind '(dx dy) (first mt_table))
	(. this :sp_set_pos (+ mx dx) (+ my dy))
	(defq at_comp (AT at_speed at_table :nil)
		mt_comp (MT 1 mt_table man))
	(set at_comp :index 1 :count 0)
	(set mt_comp :index 1 :count 0)
	(.-> this
		(:sp_add_component mt_comp)
		(:sp_add_component at_comp)
		(:sp_set_frame (first at_table))))
```

Key synchronization rules:

*	**Immediate Initial Positioning:** `Addon` calculates its position from
	`man`'s current position and `(first mt_table)` inside its constructor,
	preventing 1-frame spawn lag at `(0, 0)`.

*	**Layer Ordering via `:add_back`:** Addons are added to `*layer_player*`
	using `(:add_back addon)`. This ensures `man` updates his movement and
	bounds *before* the addon's `MT` component reads `man`'s position.

*	**Lockstep Timing (`:index 1 :count 0`):** Because frame 0 and offset 0
	are applied immediately at spawn (tick 0), both `AT` and `MT` are initialized
	with `:index 1 :count 0` so subsequent steps advance in mathematical
	lockstep.

---

### 5.2 Prebinder Literal List Rule for Tables

All constant animation tables (`+at_*`) and movement tables (`+mt_*`) must be
declared using double-quoted literal lists `''(...)`:

```vdu
; correct: double-quoted literal list for raw literals
(defq +at_upper_mase ''(1 2 3 -1))
(defq +mt_upper_mase_l ''((32 0) (32 0) (8 0) (8 0) (-16 0) (-16 0)))

; correct: quasiquoted list when referencing constant symbols (+ftp_*, +frm_*)
(defq +army_tables
	`'((,+ftp_spearman ,+ftp_spearman ,+ftp_beserk ...)
	   (,+ftp_wizard ,+ftp_carpet ...)))
```

**Prebinder Constant Evaluation & AST Substitution:**
In the prebind stage of the REPL, any symbol starting with `+` (`+xxxxx`) is evaluated in the current environment, and **what it evaluates to is directly substituted into the AST**:
*	An atom/integer (e.g. `+ftp_spearman`) evaluates to its integer bitmask and is substituted directly as an integer literal.
*	If a constant list is defined using `(defq +table (list ...))` or `'(...)`, `+table` evaluates to the raw list. Substituting that raw list directly into the AST produces an unquoted list form `(first_elem second_elem ...)`. At runtime, the evaluator treats `first_elem` as a function call, failing with `not_a_function ! Obj: ...`.
*	Using `''(...)` (double quote) or quasiquote `` `'(,...) `` ensures that `+table` evaluates to `'(...)` (i.e. `(quote (...))`). The prebinder substitutes the `(quote ...)` form into the AST, which evaluates at runtime to the literal data list.

---

## 6. Player Physics, Ducking & Camera (`fanatic.inc`, `utils.inc`)

### 6.1 Ducking Bounds & Texture Sampling (`cp-manduck`)

*	**Frame Threshold:** Frames `< +frm_fanatic_l_duck` (29) are standing
	(height 32); frames `>= +frm_fanatic_l_duck` are ducking / lower mase hits
	(height 16).

*	**Bounding Box Shift:** When ducking, `h` drops to 16 and `y` increases
	by 16:

	```vdu
	(defun cp-manduck (this that)
		(defq h (get :sp_h this) f (get :sp_frame this))
		(if (< f +frm_fanatic_l_duck)
			(when (/= h 32)
				(. this :sp_set_bounds (get :sp_x this) (- (get :sp_y this) 16) 32 32))
			(when (= h 32)
				(. this :sp_set_bounds (get :sp_x this) (+ (get :sp_y this) 16) 32 16)))
		this)
	```

*	**Texture Sampling in `:draw`:**
	The crouching sprite occupies the **top** 16 pixels of the 32x32 tile in
	`fanatic_l.cpm`. `Sprite :draw` must sample `sy` directly as `(% raw_y th)`
	without subtracting height differences, allowing `y += 16` to position
	the feet at floor level.

---

### 6.2 Ground-Anchored Camera Tracking (`update-camera`)

In `apps/games/onslaught/utils.inc`, vertical camera tracking anchors to the
player's **feet** (the ground plane) minus 16 units:

```vdu
(defun update-camera (sprite)
	; center camera on sprite using feet baseline anchor (+ y h -16)
	(bind '(x y w h) (. sprite :sp_get_bounds))
	(defq cam_x (- (+ x (/ w 2)) (const (/ +window_width 2)))
		cam_y (- (+ y h -16) (const (/ +window_height 2)))
		max_cam_x (- (* +map_width +tile_width) +window_width)
		max_cam_y (- (* +map_height +tile_height) +window_height))
	; scroll all world layers by negative camera offset
	(set-world-layers-pos
		(* *zoom* (neg (max 0 (min max_cam_x cam_x))))
		(* *zoom* (neg (max 0 (min max_cam_y cam_y))))))
```

Because `(+ y h)` is identical whether standing (`y + 32`) or ducking
(`(y + 16) + 16`), the anchor `(+ y h -16)` never moves when ducking,
eliminating camera jitter during jump wind-up and crouching.

---

### 6.3 Parabolic Jump Arc

Jumping executes an 11-step committed trajectory table (`+jump_offsets`):

*	Steps 0–1: Crouch wind-up (`FRM_DUCK`, `jv = 0`).

*	Steps 2–9: Upward impulse decaying from 7 to 1 (`FRM_JUMP`, `jv: 7..1`).

*	Step 10: Apex transition (`FRM_WALK + 1`, `jv = 1`).

Horizontal velocity `xv` is preserved throughout the jump (`x += xv`),
producing a parabolic arc. When the table ends, control falls through to gravity
physics.

---

## 7. Task Isolation & Rendering

The GUI compositor runs in its own cooperative task (`host_gui`). Game globals
such as `*zoom*` do not exist in the GUI rendering task.

Inside `Sprite :draw`, zoom is accessed via dynamic scene-graph property
inheritance:

```vdu
(defmethod :draw ()
	; (. sprite :draw) -> sprite
	(when (/= (defq f (get :sp_frame this)) -1)
		(when (defq c (. this :sp_get_canvas))
			(when (defq texture (getf c +canvas_texture 0))
				(bind '(tid tw th) (texture-metrics texture))
				(bind '(w h) (. this :get_size))
				(when (and (> w 0) (> h 0))
					(defq zoom (get :zoom this)
						fw (get :sp_frame_w this)
						fh (get :sp_frame_h this)
						scaled_fh (* zoom fh)
						scaled_fw (* zoom fw)
						raw_y (+ (* f scaled_fh) (* zoom (get :sp_oy this)))
						col (/ raw_y th)
						sy (% raw_y th)
						sx (* col scaled_fw)
						dir (get :sp_dir this))
					(when (and (>= dir 0) (get :sp_canvas_r this))
						(setq sx (- tw (+ sx scaled_fw))))
					(. this :ctx_blit tid +argb_white 0 0 w h sx sy)))))
	this)
```

Because `Sprite` is a `View` (and thus an `hmap`), `(get :zoom this)` walks
up the scene graph parent chain to `*window*` (where `:zoom` is defined on the
top-level `Window`), safely operating across task boundaries without accessing
task-thread global variables.

---

## 8. Prescriptions for Agents Modifying Onslaught

When extending or maintaining this engine, follow these strict disciplines:

1.	**Follow the `(this that)` Component Model:**
	Never store component-specific variables on `Sprite`. Component state
	belongs in `that` (`(env 1)`), while `this` is reserved for the `Sprite`.

2.	**No Component Handles on `Sprite`:**
	Do not add properties like `:man_at` to `Sprite`. Index into
	`:sp_components` using named constants (`+man_comp_at`).

3.	**Use `''(...)` for Constant Tables:**
	Always define `+at_*`, `+mt_*`, and `+jump_offsets` using double-quoted
	lists `''(...)` to prevent prebinder code inlining bugs.

4.	**Order Addons with `:add_back`:**
	Always attach addons to layers using `(:add_back addon)` so the parent
	sprite updates position before the addon's `MT` component samples it.

5.	**Anchor Camera to Feet:**
	Keep camera vertical tracking anchored to `(+ y h -16)`. Never center
	on `(/ h 2)`.

6.	**Use `(get :zoom this)` in `:draw`:**
	Never reference `*zoom*` inside `:draw` methods. Access `:zoom` via
	dynamic inheritance from the top-level `Window` using `(get :zoom this)`.

7.	**No Defensive Property Checks `(or (get ...))`:**
	Properties must be defined with default values at the appropriate level
	in the class hierarchy (base `Sprite` for `:sp_*`, subclasses for
	entity-specific properties like `:item_prev_y`, `:spear_timer`,
	`:skull_timer`). Callers should use direct `(get :prop this)` without
	defensive fallback wrappers.

8.	**Types and Flags Are Bitmasks, Not Indices:**
	`:sp_type` uses `+ftp_*` bit constants from `enums.inc` (`(bits +ftp 0 ...)`).
	`:sp_flags` uses `+fsp_*` bit constants (`(bits +fsp 0 ...)`), testing
	via `(bits? flags +fsp_...)`, setting with `logior`, and clearing with
	`(logand ... (lognot ...))`. Never use raw index numbers or artificial
	properties like `bftp_type`.

9.	**Do Not Copy `(:children)` with `cat`:**
	`(. view :children)` returns a fresh Lisp list of child views from the
	scene-graph linked list. Never wrap it in `(cat (. view :children))` when
	simply iterating over child views, as the list is not mutated.

11.	**GUI Apps Cannot Run in Headless / TUI Mode:**
	Do not attempt to launch GUI applications (like Onslaught) from the TUI /
	headless boot image via terminal commands or subagents. The user must run
	the game in their native ChrysaLisp GUI environment. To facilitate testing,
	configure `battle.inc` to pick a fixed army index in `battle-state-init`
	(e.g. `*enemy_army* 1` for Necromantic) and ask the user to test and report
	back.

12.	**No `return` in Control Flow:**
	ChrysaLisp has no early `return` keyword. Structure branches cleanly
	with `cond` and `ifn`.

13.	**Adhere to ChrysaLisp Style Guidelines:**

	*	Indent with 4-space tab characters.

	*	Start source comments with lowercase letters.

	*	Wrap documentation at 80 columns.

	*	Maintain blank lines between all markdown elements.

---

## 9. Testing & Verification Workflow

Because Onslaught is an interactive GUI application running on top of the
ChrysaLisp compositor:

1.	**Static Code Verification (Before User Testing):**
	Always run static syntax and reference checks on `apps/games/onslaught/` before
	asking the user to test in the GUI:

	*	**Bracket Matching Check (`brackets`):**
		Verify that all parentheses, square brackets, and braces are balanced and
		syntax-clean across all onslaught files:

		```sh
		echo "files apps/games/onslaught/ | brackets -q" | ./run.sh -f
		```

		Must output nothing (zero syntax or bracket mismatches). For a full breakdown
		with bracket counts and nesting depths:

		```sh
		echo "files apps/games/onslaught/ | brackets -v" | ./run.sh -f
		```

	*	**Forward Reference Check (`forward`):**
		Verify zero forward references to functions or macros:

		```sh
		echo "files apps/games/onslaught/ | forward" | ./run.sh -f
		```

		Must output nothing (zero forward references).

2.	**User-Driven Testing:**
	The agent cannot run or interact with the game window directly. All gameplay
	verification must be performed by the user launching the game in their
	active GUI session.

3.	**Targeted Army Configuration in `battle.inc`:**
	To verify specific entity interactions, AI logic, missile collisions, or
	rendering, set `*enemy_army*` in `battle-state-init` (`battle.inc`) to the
	specific army index matching the scenario under test:

	```lisp
	; in apps/games/onslaught/battle.inc (battle-state-init):
	; *enemy_army* (random (length +army_tables))
	*enemy_army* 1 ; fixed army for user testing
	```

4.	**Army Index Quick Reference:**

	*	`0`: **HILLMEN** — Spearmen, Berserkers
	*	`1`: **NECROMANTIC** — Wizards, Carpets
	*	`2`: **ROBBER** — Footmen, Cannons
	*	`3`: **BOARRIDER** — Boarriders, Monks, Spearmen
	*	`4`: **MERCENARY** — Knights, Towers, Spearmen
	*	`5`: **KNIGHTLY** — Horses, Knights, Boarriders, Spearmen
	*	`6`: **BERSERKER** — Berserkers, Spearmen
	*	`7`: **MYSTIC** — Carpets, Towers, Spearmen
	*	`8`: **BALISTIC** — Balistas, Cannons, Footmen
	*	`9`: **POWDER** — Cannons, Footmen
	*	`10`: **CAULDRON** — Oil, Footmen, Horses, Knights
	*	`11`: **MONASTIC** — Monks, Berserkers
	*	`12`: **JUGGERNAUT** — Towers, Balistas, Oil, Spearmen
	*	`13`: **PLAGUE** — Skeletons, Skeleton Horses, Skeleton Monks

5.	**Feedback Loop:**
	Always inform the user which army index was configured and specify the
	exact visual or gameplay behavior they should observe and report back.
