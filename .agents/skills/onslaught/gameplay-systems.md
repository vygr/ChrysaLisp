# Onslaught: Collisions, Addons and Player Physics

Part of the `onslaught` skill. Read this before changing combat, weapons and
items, or how the player moves and the camera follows.

## Contents

*	[4. Collision & Combat Subsystem (`collisions.inc`)](#4-collision--combat-subsystem-collisionsinc)

*	[5. Addon Weapon & Item Subsystem (`addons.inc`)](#5-addon-weapon--item-subsystem-addonsinc)

*	[6. Player Physics, Ducking & Camera (`fanatic.inc`, `utils.inc`)](#6-player-physics-ducking--camera-fanaticinc-utilsinc)

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
