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
	`sky.inc`, `utils.inc`, `assets.inc`, `enums.inc`, `battle.inc`,
	`menu.inc`, `mind.inc`, `campaign.inc`, `app.inc`, `remote.inc`,
	`app_impl.lisp`, `cmd/onslaught.lisp`

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

2.	**Dynamic Component Scoping via `env-push`:**
	Components are encapsulated instances created via `(env 1)`. During
	entity updates, `(env-push comp)` temporarily grafts the component
	into the dynamic environment chain immediately beneath the handler's
	argument frame. Handlers read and mutate component state directly via
	`setq` and `++` as native local variables, avoiding property lookup
	overhead while keeping component state isolated from the `Sprite`.

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
	(def this :color 0 :zoom *zoom*
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

### 2.2 The `env-push` Invocation Contract

Every component is an `(env 1)` environment holding its state variables
without colon prefixes (`vx`, `vy`, `speed`, `count`, etc.). During each
frame tick, `update-sprite-layers` invokes `(. sprite :sp_update)` across all
active sprites in each layer.

The `:sp_update` pipeline wraps component execution in `env-push` / `env-pop`:

```vdu
(defmethod :sp_update ()
	; (. sprite :sp_update) -> sprite
	(if (or (= (get :sp_frame this) -1) (get :sp_dead this))
		(. this :sp_kill)
		(progn
			(each (lambda (comp)
				(unless (or (= (get :sp_frame this) -1) (get :sp_dead this))
					(if (env? comp)
						(progn
							(env-push comp)
							((get :update comp) this comp)
							(env-pop))
						(comp this :nil))))
				(get :sp_components this))
			(when (or (= (get :sp_frame this) -1) (get :sp_dead this))
				(. this :sp_kill))))
	this)
```

The `env-push` execution lifecycle:

1.	**Push:** `(env-push comp)` pushes `comp` onto `+lisp_environment` and
	links `comp->+hmap_parent` to the caller's environment.

2.	**Invocation:** `((get :update comp) this comp)` is called. The
	interpreter's `repl_apply` creates the handler's argument frame
	`(this that)` on top of `comp`:

	```
	[Handler Frame: this, that, local defqs]
	                  │
	                  ▼
	       [Component State: comp]
	                  │
	                  ▼
	          [Caller Frame]
	                  │
	                  ▼
	             [*root_env*]
	```

3.	**Scoping & Shadowing:**
	*	Parameters (`this`, `that`) and local `defq` variables sit at the top
		of the scope. Scratch variables (`(defq f ...)`, `(bind '(x y) ...)`)
		remain purely in the ephemeral handler frame and never pollute `comp`.
	*	Component variables (`vx`, `speed`, `count`) are read directly as outer
		variables. Mutating them via `(setq vx ...)`, `(++ count)`, or
		`(-- spd)` traverses to `comp` and modifies component state in-place.
	*	**Never use `defq` on a component variable inside a handler.** Doing
		`(defq count 0)` creates a shadowed local variable in the handler
		frame, leaving `comp`'s state unmutated. Use `(setq count 0)`.

4.	**Pop:** `(env-pop)` takes no arguments, restores `+lisp_environment`
	to the caller's frame and clears `comp->+hmap_parent` to `0`, preventing
	stack frame memory leaks across frames.

---

### 2.3 External Access to Component Variables

When entity logic, collision hooks, or dismount callbacks need to inspect or
modify a component's variables from outside its pushed handler, **always use
quoted bare symbols** (`'vx`, `'vy`, `'table`, `'speed`, `'index`, `'count`),
never colon keywords:

```vdu
; correct: using quoted bare symbols on unpushed components
(set mv 'vx 0)
(set mv 'vx (* (get 'max_vx mv) dir))
(set at_comp 'table body_anim 'speed 2 'count 0 'index 1 'loop :nil)
(when (nil? (get 'table at_comp)) ...)
(when (and mv (= (get 'ay mv) 0)) ...)

; wrong: using colon keywords returns :nil because properties lack colons
(set mv :vx 0)                    ; fails: :vx not bound in mv!
(set mv :vx (* (get :max_vx mv) dir)) ; fails: (get :max_vx mv) is :nil -> (* :nil dir) crashes!
```

---

### 2.4 Known Component Index Access

Components reside **strictly** within the `:sp_components` list. No component
instances are stored as redundant properties on `Sprite`.

When an entity needs to reference a specific component, it indexes directly into
`:sp_components` using a named compile-time constant:

```vdu
(defq +man_comp_at 0)
(defq +en_comp_mv 0 +en_comp_at 1)

; access at component at index 0
(defq man_at (elem-get (get :sp_components this) +man_comp_at))
(defq mv (elem-get (get :sp_components enemy) +en_comp_mv))
```

This mirrors the fixed struct member offset of the original C++ engine.

---

### 2.5 Destruction & Death Hook Contract

Death works exactly as in the C++ engine (`engine.cpp`), where `killsprite`
is just `SPRITE_FRAME = -1` and `sprite_proc_cb` does the death processing
when it next visits that sprite.

`(. sprite :sp_kill)`, or setting `:sp_frame` to `-1`, only marks the
sprite. The death hook and removal happen on the sprite's own turn in
`:sp_update`:

```vdu
(defmethod :sp_kill ()
	; (. sprite :sp_kill) -> sprite
	(def this :sp_dead :t :sp_frame -1)
	this)

(defmethod :sp_reap ()
	; (. sprite :sp_reap) -> sprite
	(. this :sub)
	(when (defq addon (get :sp_addon this))
		(def this :sp_addon :nil)
		(. addon :sp_kill))
	(when (defq death (get :sp_death this))
		(def this :sp_death :nil)
		(death this))
	this)

(defmethod :sp_update ()
	; (. sprite :sp_update) -> sprite
	(each (lambda (comp)
		(unless (= (get :sp_frame this) -1)
			(env-push comp)
			((get :update comp) this comp)
			(env-pop)))
		(get :sp_components this))
	(when (= (get :sp_frame this) -1)
		(. this :sp_kill)
		(. this :sp_reap))
	this)
```

*	A sprite that kills itself during its own update (an animation's `-1`,
	going offscreen, a timeout) dies at the end of that same update.

*	A sprite killed by another dies when the update loop reaches it: later
	the same frame if it comes after its killer in the layer order, else
	next frame. Until then it stays in its layer, is skipped by
	`sprite-collide` and is not drawn.

*	A death hook therefore always runs from the top of the update loop,
	never from inside a collision handler or another death hook. A chain of
	mines or monks ripples through the loop instead of nesting on the stack.

*	Code may change `:sp_death` after calling `:sp_kill`, as the C++ does
	(`killsprite(hit); hit->SPRITE_DEATH = dt_exp_fx;`).

*	`clear-sprite-layers` removes sprites directly, without death hooks.

---

## 3. The Standard Component Suite (`sprite.inc`)

All standard components are defined in `apps/games/onslaught/sprite.inc`.
Their constructors instantiate a private single-bucket environment `(env 1)`
with un-colonized quoted keys (`'vx`, `'table`, etc.) and return `that`.
Method dispatch keys retain the colon (`:update`).

### 3.1 Procedural Component Wrapper (`CP`)

Wraps an arbitrary procedural function into a pipeline component:

```vdu
(defun cp-update (this that)
	; this = sprite, that = component instance
	(when proc
		(proc this that))
	that)

(defun CP (proc)
	; (CP proc) -> component
	(def (defq that (env 1)) :update (const cp-update)
		'proc proc)
	that)
```

---

### 3.2 Kinematic Vector Movement (`MV`)

Applies continuous velocity, acceleration, and terminal limits:

*	**Constructor:** `(MV [vx vy ax ay max_vx max_vy])`

*	**Local Variables:** `vx`, `vy`, `ax`, `ay`, `max_vx`, `max_vy`.

*	**Update:** Applies acceleration, clamps to limits, updates `:sp_pos` on
	`this`.

```vdu
(defun mv-update (this that)
	; this = sprite, that = component instance
	(bind '(x y) (. this :sp_get_pos))
	(setq vx (max (neg max_vx) (min max_vx (+ vx ax)))
		vy (max (neg max_vy) (min max_vy (+ vy ay))))
	(. this :sp_set_pos (+ x vx) (+ y vy))
	that)

(defun MV (&optional vx vy ax ay max_vx max_vy)
	; (MV [vx vy ax ay max_vx max_vy]) -> component
	(def (defq that (env 1)) :update (const mv-update)
		'vx (or vx 0) 'vy (or vy 0)
		'ax (or ax 0) 'ay (or ay 0)
		'max_vx (or max_vx 100) 'max_vy (or max_vy 100))
	that)
```

---

### 3.3 Discrete Table Movement (`MT`)

Steps an entity through a scripted series of relative offsets
`((dx dy) ...)`:

*	**Constructor:** `(MT speed table &optional track)`

*	**Local Variables:** `speed`, `table`, `count`, `index`, `track`.

*	**Tracking Mode:** If `track` is non-nil, offsets are added relative to the
	tracked sprite's position; otherwise, they are relative to `this`.

```vdu
(defun mt-update (this that)
	; this = sprite, that = component instance
	(if (and track (or (get :sp_dead track) (= (get :sp_frame track) -1)))
		(. this :sp_kill)
		(when table
			(when (> speed 0)
				(when (>= (++ count) speed)
					(setq count 0)
					(bind '(dx dy) (elem-get table index))
					(bind '(tx ty) (. (or track this) :sp_get_pos))
					(. this :sp_set_pos (+ tx dx) (+ ty dy))
					(setq index (if (= (inc index) (length table)) 0 (inc index)))))))
	that)

(defun MT (speed table &optional track)
	; (MT speed table [track]) -> component
	(def (defq that (env 1)) :update (const mt-update)
		'speed (or speed 1) 'table table 'count 0 'index 0
		'track (ifn track :nil track))
	that)
```

---

### 3.4 Linear Bresenham Interpolation (`ML`)

Interpolates an entity along a straight-line vector to exact target
coordinates:

*	**Constructor:** `(ML [speed])`

*	**Path Initialization:** `(init-ml this that x y x1 y1 [speed])`

*	**Local Variables:** `target_x`, `target_y`, `speed`, `count`, `slope`,
	`dx`, `dy`, `d1x`, `d2x`, `d1y`, `d2y`.

*	**Local Loop Speed Rule:** In `ml-update`, never decrement `speed`
	directly with `(-- speed)`; copy it to a local loop variable `(defq spd speed)`
	so the component's step velocity is preserved across frames.

```vdu
(defun ml-update (this that)
	; this = sprite, that = component instance
	(when (> speed 0)
		(defq spd speed)
		(bind '(x y) (. this :sp_get_pos))
		(while (and (> spd 0) (> count 0))
			(-- count)
			(setq slope (- slope dy))
			(if (< slope 0)
				(setq slope (+ slope dx) x (+ x d1x) y (+ y d1y))
				(setq x (+ x d2x) y (+ y d2y)))
			(-- spd))
		(when (<= count 0)
			(setq count -1 x target_x y target_y))
		(. this :sp_set_pos x y))
	that)

(defun init-ml (this that x y x1 y1 &optional speed)
	; (init-ml sprite ml x y x1 y1 [speed]) -> ml
	(when speed (set that 'speed speed))
	(. this :sp_set_pos x y)
	(defq dx (- x1 x) dy (- y1 y)
		d1x (cond ((< dx 0) -1) ((> dx 0) 1) (:t 0)) d2x d1x
		d1y (cond ((< dy 0) -1) ((> dy 0) 1) (:t 0)) d2y 0
		abs_dx (abs dx) abs_dy (abs dy) tmpi 0)
	(when (< abs_dx abs_dy)
		(setq d2y d1y d2x 0 tmpi abs_dx abs_dx abs_dy abs_dy tmpi))
	(set that 'target_x x1 'target_y y1
		'dx abs_dx 'dy abs_dy 'd1x d1x 'd2x d2x 'd1y d1y 'd2y d2y
		'count abs_dx 'slope (>> abs_dx 1))
	that)

(defun ML (&optional speed)
	; (ML [speed]) -> component
	(def (defq that (env 1)) :update (const ml-update)
		'speed (or speed 0) 'table :nil 'count -1 'index 0
		'target_x 0 'target_y 0 'slope 0 'dx 0 'dy 0
		'd1x 0 'd2x 0 'd1y 0 'd2y 0)
	that)
```

---

### 3.5 Table Animation (`AT`)

Drives multi-frame sprite atlas animations:

*	**Constructor:** `(AT speed table &optional loop)`

*	**Local Variables:** `speed`, `table`, `count`, `index`, `loop`.

*	**Termination Contract:**
	*	Terminal `-1`: Automatically invokes `(. this :sp_kill)`.
	*	Non-looping (`loop` is `:nil`): When the last frame is reached,
		sets `speed` to `0` and `table` to `:nil`, allowing controllers to
		detect completion via `(nil? (get 'table at_comp))`.
	*	Looping (`loop` is `:t`): Automatically wraps `index` to 0.

```vdu
(defun at-update (this that)
	; this = sprite, that = component instance
	(when table
		(when (> speed 0)
			(when (>= (++ count) speed)
				(setq count 0)
				(defq f (elem-get table index))
				(cond
					((= f -1)
						(. this :sp_kill))
					((and (not loop) (= (inc index) (length table)))
						; non-looping animation reached end
						(. this :sp_set_frame f)
						(setq speed 0 table :nil count 0 index 0))
					(:t
						(. this :sp_set_frame f)
						(setq index (if (= (inc index) (length table)) 0 (inc index))))))))
	that)

(defun AT (speed table &rest opt)
	; (AT speed table [loop]) -> component
	(defq loop (if (empty? opt) :t (first opt)))
	(def (defq that (env 1)) :update (const at-update)
		'speed (or speed 1) 'table table 'count 0 'index 0
		'loop (if loop :t :nil))
	that)
```

---

### 3.6 Collision Component (`CL`)

Executes collision checks at a specific point in the component pipeline:

*	**Constructor:** `(CL layer type table)`

*	**Local Variables:** `layer`, `type`, `table`.

*	**Collision Primitives:**
	*	`(sprite-collide layer x y w h type &optional ignore)`: Scans `layer`
		for the first live sprite overlapping bounding box `(x y w h)` matching
		`type` mask.

*	**Dispatch Logic:**
	*	If `table` is a list, indexes by target type bit:
		`(log2 (get :sp_type hit))`.
	*	If `table` is a function, calls `(table this hit)` directly.

```vdu
(defun sprite-collide (layer x y w h type &optional ignore)
	; (sprite-collide layer x y w h type [ignore]) -> sprite | :nil
	(defq xw (+ x w) yh (+ y h))
	(some (lambda (sp)
		(when (and (Sprite? sp)
				   (nql sp ignore)
				   (/= (get :sp_frame sp) -1)
				   (not (get :sp_dead sp))
				   (or (= type -1) (/= 0 (logand (get :sp_type sp) type))))
			(bind '(tx ty tw th) (. sp :sp_get_bounds))
			(if (and (< x (+ tx tw)) (< tx xw)
					 (< y (+ ty th)) (< ty yh))
				sp)))
		(. layer :children)))

(defun cl-update (this that)
	; this = sprite, that = component instance
	(when (and layer
			   (/= (get :sp_frame this) -1)
			   (not (get :sp_dead this)))
		(bind '(x y w h) (. this :sp_get_bounds))
		(when (defq hit (sprite-collide layer x y w h type this))
			(cond
				((or (func? table) (lambda-func? table))
					(table this hit))
				((list? table)
					(when (defq idx (log2 (get :sp_type hit)))
						(when (< -1 idx (length table))
							(when (defq handler (elem-get table idx))
								(handler this hit))))))))
	that)

(defun CL (layer type table)
	; (CL layer type table) -> component
	(def (defq that (env 1)) :update (const cl-update)
		'layer layer 'table table 'type (ifn type -1 type))
	that)
```

---

### 3.7 Viewport Boundary Culling (`offscreen-update`)

Procedural components checking visibility relative to `*world_scroll*`:

*	`offscreen-update`: Preserves `:sp_death` callback when killed.

*	`offscreen-nodeath-update`: Clears `:sp_death` before killing so offscreen
	culling does not trigger explosion FX or item drops.

```vdu
(defun offscreen-nodeath-update (this &optional that)
	; offscreen update that clears death hook before killing sprite
	(bind '(x y w h) (. *world_scroll* :get_relative this))
	(bind '(ww wh) (. *world_scroll* :get_size))
	(when (or (< x (neg w)) (< y (neg h)) (>= x ww) (>= y wh))
		(def this :sp_death :nil)
		(. this :sp_kill))
	this)

(defun offscreen-update (this &optional that)
	; offscreen update that preserves death hook on kill
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
(defclass Fanatic () (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic dt-kill-man)
	(def this :sp_type +ftp_man :sp_dir 1 :man_xv 0 :man_yv 0 :man_walk_tick 0
		:man_jump :nil)
	(.-> this
		; at (index 0 / +man_comp_at): player weapon body animations
		(:sp_add_component (AT 0 :nil :nil))
		; cp (index 1): player input, movement, jumping, and falling physics
		(:sp_add_component (CP player-man-update))
		; cp (index 2): ducking height and y bounds adjustment component
		(:sp_add_component (CP cp-manduck))
		; cl1..cl6 (indices 3..8): collision checks
		(:sp_add_component (CL *layer_items* +ftp_item *man_collision_table*))
		(:sp_add_component (CL *layer_missiles* -1 *man_collision_table*))
		(:sp_add_component (CL *layer_enemies*
			(logior +ftp_tower +ftp_horse +ftp_boarrider +ftp_carpet +ftp_skeleton_horse)
			*man_collision_table*))
		(:sp_add_component (CL *layer_enemies*
			(logior +ftp_knight +ftp_skeleton +ftp_monk +ftp_skeleton_monk)
			*man_collision_table*))
		(:sp_add_component (CL *layer_enemies* +ftp_footman *man_collision_table*))
		(:sp_add_component (CL *layer_bans* -1 *man_collision_table*))
		(:sp_set_frame +frm_fanatic_l_stance)))
```

---

## 5. Addon Weapon & Item Subsystem (`addons.inc`)

### 5.1 Reusable `Addon` Sprite Class

Weapons and items that attach to parent sprites are modeled by `Addon`:

```vdu
(defun dt-addon (this)
	; clear addon link on parent sprite when addon finishes or dies
	(when (defq parent (get :sp_parent_sprite this))
		(when (eql (get :sp_addon parent) this)
			(def parent :sp_addon :nil))))

(defclass Addon (man mt_table at_speed at_table) (Sprite *img_frm_16x16_l* *img_frm_16x16_r* 16 16 :16x16_lr dt-addon)
	; (Addon man mt_table at_speed at_table) -> addon sprite
	(def this :sp_dir (get :sp_dir man) :sp_parent_sprite man)
	(def man :sp_addon this)
	; set initial position immediately to eliminate 1-frame spawn lag
	(bind '(mx my) (. man :sp_get_pos))
	(bind '(dx dy) (first mt_table))
	(. this :sp_set_pos (+ mx dx) (+ my dy))
	(defq at_comp (AT at_speed at_table :nil)
		mt_comp (MT 1 mt_table man))
	; start at index 1 since step 0 was applied at spawn
	(set at_comp 'index 1 'count 0)
	(set mt_comp 'index 1 'count 0)
	(.-> this
		(:sp_add_component mt_comp)
		(:sp_add_component at_comp)
		(:sp_set_frame (first at_table))))
```

Key synchronization rules:

*	**Immediate Initial Positioning:** `Addon` calculates its position from
	`man`'s current position and `(first mt_table)` inside its constructor,
	preventing 1-frame spawn lag at `(0, 0)`.

*	**Layer Ordering via `:add_before`:** Addons are added using
	`(. man :add_before wep)` or `(:add_front wep)`. This ensures `man` updates
	movement and bounds *before* the addon's `MT` component reads `man`'s
	position.

*	**Lockstep Timing (`'index 1 'count 0`):** Because frame 0 and offset 0
	are applied immediately at spawn (tick 0), both `AT` and `MT` are
	initialized with `'index 1 'count 0` using quoted symbols so subsequent steps
	advance in mathematical lockstep.

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
In the prebind stage of the REPL, any symbol starting with `+` (`+xxxxx`) is
evaluated in the current environment, and **what it evaluates to is directly
substituted into the AST**:

*	An atom/integer (e.g. `+ftp_spearman`) evaluates to its integer bitmask and
	is substituted directly as an integer literal.

*	If a constant list is defined using `(defq +table (list ...))` or `'(...)`,
	`+table` evaluates to the raw list. Substituting that raw list directly into
	the AST produces an unquoted list form `(first_elem second_elem ...)`. At
	runtime, the evaluator treats `first_elem` as a function call, failing with
	`not_a_function ! Obj: ...`.

*	Using `''(...)` (double quote) or quasiquote `` `'(,...) `` ensures that
	`+table` evaluates to `'(...)` (i.e. `(quote (...))`). The prebinder
	substitutes the `(quote ...)` form into the AST, which evaluates at runtime
	to the literal data list.

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
	`fanatic_l.cpm`. `Sprite :draw` samples `sy` directly as `(% raw_y th)`
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
					(ifn (= (get :sp_h this) 48)
						(. this :ctx_blit tid +argb_white 0 0 w h sx sy)
						; banner head (top 16x16)
						(. this :ctx_blit tid +argb_white 0 0 scaled_fw scaled_fh sx sy)
						; pole 1 (middle 16x16)
						(. this :ctx_blit tid +argb_white 0 scaled_fh scaled_fw scaled_fh 0 0)
						; pole 2 (bottom 16x16)
						(. this :ctx_blit tid +argb_white 0 (* scaled_fh 2) scaled_fw scaled_fh 0 0))))))
	this)
```

Because `Sprite` is a `View` (and thus an `hmap`), `(get :zoom this)` walks
up the scene graph parent chain to `*window*` (where `:zoom` is defined on the
top-level `Window`), safely operating across task boundaries without accessing
task-thread global variables.

---

## 8. Campaign Map (`campaign.inc`)

`campaign.inc` is the port of the C++ `game_frame_map` family. It owns the
map, mind and oracle game states, and is imported last, after `menu.inc`.

### 8.1 Campaign State

*	`*campaign_map*`: a list of 256 tile types (`+frm_campain_*`), a 16x16
	grid loaded from `data/campainmap.dat`. Index is `x + y * 16`.

*	`*location_x*` / `*location_y*`: the current location.
	`*location_last_x*` / `*location_last_y*`: the last player owned location,
	which events never overwrite. `*location_ox*` / `*location_oy*`: when
	`*location_ox*` is not `-1` the only legal move is back to that location.

*	`*campaign_active*` (`battle.inc`): `:nil` means a new campaign.
	`map-state-init` then calls `game-generate-player-info`. The menu clears
	it on START GAME, and death or losing the last territory clears it.

*	**Save and load.** `campaign-snapshot` copies the campaign in progress
	into `*campaign_state*`, an `Emap` that `config-save` writes as
	`:campaign`. It is taken on every return to the map and at exit, and
	cleared when the campaign ends. The menu's LOAD GAME sets
	`*campaign_active*` to `:load`, and `map-state-init` then calls
	`campaign-restore`.

*	**Power and strength carry over** from battle to battle, as in the C++.
	`game-generate-player-info` sets them, power at half, at the start of a
	campaign. `battle-state-init` must not reset them.

### 8.2 Game Flow

*	Menu START GAME goes straight to `+game_state_map`.

*	Fire on the map enters the location: enemy, plague, crusade and
	rebellion tiles start a battle, the oracle shows a hint, temples go to
	the mind state. `b` uses a selected map talisman (`+fkey_keyb`).

*	`game-generate-enemy-info` derives everything about the enemy at the
	current location, `*enemy_army*`, `*enemy_banner*`, `*enemy_max*`,
	`*mine_max*`, `*enemy_wizshot*` and `*game_level*`, by temporarily seeding
	`game-random` with the location index. It is called every map frame, so
	`battle-state-init` must not set these itself.

*	Battle outcomes: field win -> seige, seige win -> mind. Field loss ->
	defend, seige loss -> field, defend win -> field, defend loss -> mind.

*	The mind state runs the mind combat (`mind.inc`). After 64 frames in
	the won or lost state, `mind-won` / `mind-lost` apply the outcome: a
	seige mind win takes the location, a defend mind loss loses the last
	player location, a temple mind loss loses all talismans. Power is
	restored to its value before the combat.

*	Any talisman used in battle destroys all missiles, scoring them, and
	all mines, and costs one of its three uses (`use-talisman`).

*	The hall of glory asks for initials when the glory beats the lowest
	entry and the mode is not auto. Left and right spin a 3 by 11 grid of
	letters under a fixed sight, fire picks the letter, `[` is backspace
	and `]` enters. The entry is the generated name plus the initials.

*	Every `+campaign_event_rate` (200) map frames `campaign-events` creates,
	destroys and grows the plagues, crusades and rebellions.

### 8.3 Mind Combat (`mind.inc`)

*	`Hand`: the player, runs around the inside edge of the screen in 8
	pixel steps and fires up to `+mind_fire_max` `HandFire` shots at the mind.

*	`Mind`: the wizardlord face. Homes on a random point, fires a `MindFire`
	every 10 frames, and loses an arm segment every 8 hits. It dies when
	`:mind_segs` reaches 0. A `MindFire` hit costs the player 200 power, and
	power 0 loses the combat.

*	Every 96 frames Mr Smith drops a `Talisman` item on the edge: a blue
	spell (power) in a battle mind combat, or at a temple its terrain
	talisman followed by the `+mind_items` table.

*	**Multi blit drawing uses an overlay View, not segment sprites.** The
	C++ drew the four arms from inside the mind's draw callback. A `Sprite`
	can only draw inside its own bounds, so the arms are drawn by
	`MindArms`, a full screen transparent `View` behind the mind. `cp-mind`
	copies the mind's position, segment count, arm phase and frame count
	into its properties each frame, and its `:draw` loops over the `+mind_arms`
	table blitting segments. `MindMap`, the two plane tiled background, works
	the same way. Use this pattern for any effect that needs many blits
	outside one sprite's bounds.

### 8.4 Drawing

`CampaignView` is a full screen overlay `View`, like `MenuView`. Its `:draw`
runs in the GUI task, so it reads only properties: `:map` (the tile list,
or `:nil` for no map), `:canvas` (`*img_campain*`), `:tid` (ascii texture)
and `:texts`, a list of `(txt x y col)` built each frame by
`campaign-sync` in the game task. The banner, stance, wizardlord and sight
are plain `Sprite` instances added to `*layer_fx*` in front of the view.

---

### 8.5 Stack Depth

The task stack is small, around 6KB is classed as big. Hard mode against
the Monk army, with its cascading explosions, is the deepest case, and
peaks at about 5.3KB (it was over 7KB when death hooks ran nested inside
collision handlers). Keep it that way:

*	Never run a death hook from inside a collision handler or another death
	hook. Call `:sp_kill` and let the sprite die on its own turn (section
	2.5).

*	Check the peak with the remote state's `:max_stack` (section 8.6) while
	the bot plays, on a `make it validate` build so a stack overrun is
	reported rather than crashing.

### 8.6 Remote Play Service (`app.inc`, `remote.inc`)

A running game declares the `@Onslaught` service on its `+select_service`
mailbox. The main loop passes anything arriving there to `remote-request`,
so any task, on any node, can play the game by mail:

*	`app.inc` is the client side, an include for any program: the
	`+ons_rpc` message structure and `onslaught-keys-rpc`,
	`onslaught-state-rpc` and `onslaught-quit-rpc`.

*	`(onslaught-keys-rpc keys)` sets the held control keys, a mask of the
	`+fkey_*` bits. Remote keys act exactly as the user's keys do. Menu moves
	and fire trigger on release, so tap by sending the key, then `0`.

*	`(onslaught-state-rpc)` returns an `Lmap`: `:state` (`:menu`, `:map`,
	`:battle`, `:mind` ...), `:level`, `:score`, `:power`, `:strength`,
	`:inventory`, `:location`, `:back`, `:territory`, `:map` (the 256 tiles,
	only on the map), `:army`, `:stack`, and for each sprite layer, `:player`,
	`:enemies`, `:missiles`, `:items`, `:bans`, a list of
	`(class frame x y)`, and `:man`, the player's `(x y w h dir)` in battle.

*	`(onslaught-land-rpc)` returns the battle map's `+fmap_*` tile flags, one
	char per tile, row by row. The bot fetches it on entering a battle and
	path finds to the enemy banner over it. Also `:max_stack`, the peak task stack use on the
	game's node from `(kernel-stats)`, and `:mem_used`.

*	`remote.inc` is the server side. `remote-request` checks the message is
	at least `+ons_rpc_size` long and otherwise ignores it, the service
	mailbox is a public boundary. The state reply is `(str (remote-state))`,
	read back by the client with `read`.

*	`cmd/onslaught.lisp` is the `onslaught` command: a one line summary,
	`-s` full state, `-k num` set keys, `-b secs` a simple bot that plays
	(menu, map, attack, battle, mind duel), `-q` quit.

---

## 9. Prescriptions for Agents Modifying Onslaught

When extending or maintaining this engine, follow these strict disciplines:

1.	**Follow the `env-push` Dynamic Scoping Model:**
	Wrap component execution in `(:sp_update)` with `(env-push comp)` before
	invoking the handler and `(env-pop)` immediately after. Component handlers
	access component state as bare local variables.

2.	**No Colon Prefixes on Component State Variables:**
	Component properties (`vx`, `vy`, `ax`, `ay`, `speed`, `table`, `count`,
	`index`, `track`, `proc`, `layer`, `type`) MUST NOT have a `:` prefix.
	Reserve `:` exclusively for method hooks (like `:update`).

3.	**Quote Component Keys in Constructors:**
	When instantiating components using `(def that ...)`, always quote bare
	symbol names: `'vx`, `'vy`, `'speed`, `'table`, `'count`. Unquoted symbols
	in argument position evaluate as variable lookups and will throw
	`symbol_not_bound` or `not_a_symbol`.

4.	**Mutate Component State via `setq`, `++`, or `--`:**
	Handlers modify component variables directly using `(setq vx ...)`,
	`(++ count)`, etc. **Never use `defq` on a component variable inside a
	handler**, as that defines a shadowed local variable in the function frame
	instead of mutating component state.

5.	**Use `defq` and `bind` Exclusively for Ephemeral Scratch Variables:**
	Any temporary variables (`x`, `y`, `f`, `dir`, `spd`) must use `defq` or
	`bind` inside the handler so they live strictly in the function frame and
	do not pollute the component environment.

6.	**Preserve Persistent Step Velocities in `ML`:**
	In `ml-update`, never decrement `speed` directly using `(-- speed)`; copy it
	to a local loop variable `(defq spd speed)` so the component's step velocity
	persists across frames.

7.	**External Access to Component State Uses Quoted Symbols:**
	When code outside a component handler inspects or mutates component state,
	always pass quoted symbols: `(get 'max_vx mv)`, `(set mv 'vx 0)`,
	`(get 'table at_comp)`. Passing colon keywords (like `:max_vx`) searches for
	a non-existent property and returns `:nil`.

8.	**No Component Handles on `Sprite`:**
	Do not add properties like `:man_at` to `Sprite`. Index into
	`:sp_components` using named constants: `+man_comp_at`, `+en_comp_mv`,
	`+en_comp_at`, `+comp_move` (the MV, MT or ML that is first on missiles,
	items, addons and fx) and `+manbits_comp_mv`. Never a raw `0` or `1`.

9.	**Use `''(...)` for Constant Tables:**
	Always define `+at_*`, `+mt_*`, and `+jump_offsets` using double-quoted
	lists `''(...)` to prevent prebinder code inlining bugs.

10.	**Order Addons with `:add_before` or `:add_front`:**
	Addons must be attached so the parent sprite updates position before the
	addon's `MT` component samples it.

11.	**Anchor Camera to Feet:**
	Keep camera vertical tracking anchored to `(+ y h -16)`. Never center
	on `(/ h 2)`.

12.	**Use `(get :zoom this)` in `:draw`:**
	Never reference `*zoom*` inside `:draw` methods. Access `:zoom` via
	dynamic inheritance from the top-level `Window` using `(get :zoom this)`.

13.	**No Defensive Property Checks `(or (get ...))`:**
	Properties must be defined with default values at the appropriate level
	in the class hierarchy. Callers should use direct `(get :prop this)` without
	defensive fallback wrappers.

14.	**Types and Flags Are Bitmasks, Not Indices:**
	`:sp_type` uses `+ftp_*` bit constants from `enums.inc` (`(bits +ftp 0 ...)`).
	`:sp_flags` uses `+fsp_*` bit constants (`(bits +fsp 0 ...)`), testing
	via `(bits? flags +fsp_...)`, setting with `logior`, and clearing with
	`(logand ... (lognot ...))`. Never use raw index numbers or artificial
	properties like `bftp_type`.

15.	**Do Not Copy `(:children)` with `cat`:**
	`(. view :children)` returns a fresh Lisp list of child views from the
	scene-graph linked list. Never wrap it in `(cat (. view :children))` when
	simply iterating over child views, as the list is not mutated.

16.	**Use the Frame Enums, Not Numbers:**
	Animation tables and `:sp_set_frame` calls use the `+frm_*` names from
	`enums.inc` (`+frm_16x16_shield1`, `+frm_16x16_l_blood`), never magic
	numbers or `(+ +frm_16x16_shield 1)`.

17.	**Play Sounds with `play-sfx`:**
	Use `(play-sfx sfx sprite)` (`utils.inc`), not `audio-play-rpc`, so the
	effect is panned to where the sprite is on the screen. Leave the sprite
	off only for sounds with no position, such as the menu's.

18.	**GUI Apps Cannot Run in Headless / TUI Mode:**
	Do not attempt to launch GUI applications (like Onslaught) from the TUI
	boot image. Game logic and state code can be exercised by an agent with a
	probe script under the GUI boot image (see the testing section). Gameplay
	itself must be tested by the user in their ChrysaLisp GUI environment.

19.	**No `return` in Control Flow:**
	ChrysaLisp has no early `return` keyword. Structure branches cleanly
	with `cond` and `ifn`.

20.	**Adhere to ChrysaLisp Style Guidelines:**

	*	Indent with 4-space tab characters.

	*	Start source comments with lowercase letters.

	*	Wrap documentation at 80 columns.

	*	Maintain blank lines between all markdown elements.

---

## 10. Testing & Verification Workflow

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

2.	**Agent Probe Scripts (Before User Testing):**
	State and logic code can be run by an agent without playing the game.
	Write a script in `tests/scratch/` that imports the same files as
	`app_impl.lisp`, up to but not including `(defun main`, with `*app_root*`
	set to `"apps/games/onslaught/"`. Do not import `app_impl.lisp` itself,
	its `main` clashes with the `lisp` command's own. Then call the state
	functions directly and `print` the globals:

	```lisp
	(def *window* :zoom *zoom*)
	(load-cpm-assets *zoom*)
	(game-seed 12345)
	(setq *campaign_active* :nil *game_controls* 0)
	(map-state-init)
	(setq *game_controls* +fkey_right) (map-state-update)
	(print *location_x* " " *location_y*)
	```

	```sh
	echo 'lisp -r (import {tests/scratch/probe.lisp})' | perl -e 'alarm 60; exec @ARGV' ./run.sh -n 1 -f; ./stop.sh
	```

	Keep probe loops flat, use `while` rather than nested `each` and
	`lambda`, as the probe runs the game code several call levels deeper
	than the real main loop does and can overflow the task stack by itself.
	Call `(task-slice)` each frame so the host GUI stays responsive. To get
	stack check errors rather than a VM crash, build with `make it validate`
	first, and restore with `make it` afterwards. `(last (kernel-stats))`
	gives the peak stack use.

	To exercise the `:draw` methods too, call `(config-load)` and
	`(window-resize)`, add `*window*` with `gui-add-front-rpc`, run frames
	with `(update-frame)` and `(task-sleep 50000)` for a few seconds, then
	`gui-sub-rpc`. Tell the user a window will appear.

	**Keep probes off the user's config.** `map-state-init` saves the
	campaign with `config-save`, and a finished battle can save a demo. Add
	`(setq *env_home* "tests/scratch/")` straight after the probe's
	`(import "usr/env.inc")`, before `config.inc` is imported, so
	`+config_file` points at a scratch file.

3.	**Remote Play (Preferred for Whole Game Checks):**
	Launch the real game and drive it through its `@Onslaught` service.
	This runs the true main loop at its true stack depth, the user can
	watch, and state comes back as data:

	```sh
	echo 'lisp -r (import {lib/task/pipe.inc}) (open-child {service/audio/app.lisp} +kn_call_pin) (task-sleep 1000000) (open-child {apps/games/onslaught/app.lisp} +kn_call_pin) (task-sleep 3000000) (pipe-run {onslaught -b 45}) (pipe-run {onslaught -s}) (pipe-run {onslaught -q})' | perl -e 'alarm 120; exec @ARGV' ./run.sh -n 1 -f; ./stop.sh
	```

	The audio service is normally started by the login app, so start it
	first as above, or the game finds no `@Audio` service and plays
	silently. This runs the real game on the user's real config file: its
	campaign is saved there, so clear `:campaign` afterwards if the bot's
	game should not be left for LOAD GAME.

	Use `onslaught -k num` and `onslaught -s` for scripted steps, or write a
	script using `apps/games/onslaught/app.inc` directly. Tell the user the
	game window will appear.

4.	**User-Driven Testing:**
	Gameplay verification must be performed by the user launching the game
	in their active GUI session.

5.	**Targeted Army Configuration:**
	The enemy army is chosen by `game-generate-enemy-info` (`campaign.inc`)
	from the map location. To verify specific entity interactions, AI logic,
	missile collisions, or rendering, fix `*enemy_army*` there to the army
	index matching the scenario under test:

	```lisp
	; in apps/games/onslaught/campaign.inc (game-generate-enemy-info):
	*enemy_army* 1 ; fixed army for user testing
	```

6.	**Army Index Quick Reference:**

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

7.	**Feedback Loop:**
	Always inform the user which army index was configured and specify the
	exact visual or gameplay behavior they should observe and report back.
