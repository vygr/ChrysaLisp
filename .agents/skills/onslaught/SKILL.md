# Agent Skill: ChrysaLisp Onslaught 2D Entity-Component Engine

## Domain & Scope

*	**Target Application:** `apps/games/onslaught/`

*	**Primary Files:** `sprite.inc`, `fanatic.inc`, `enemy.inc`, `title.inc`,
	`field.inc`, `widgets.inc`, `map.inc`, `sky.inc`, `utils.inc`

*	**Architectural Heritage:** Directly based on the original 1989 Commodore
	Amiga / Atari ST game *Onslaught* by Chris Hinsley. The architectural
	insights developed during the creation of this engine—treating sprites as
	autonomous, composable, intelligent entities communicating through event
	hooks—served as the original conceptual inspiration for the Taos Operating
	System, its Virtual Processor (VP), and ultimately ChrysaLisp itself.

---

## 1. Core Engineering Philosophy

The Onslaught engine is built around a single unifying principle:

> **"Configure the entities and their hooks, and the game plays itself."**

Traditional game loops require monolithic managers tracking global arrays of
monsters, projectiles, and particle lifetimes. In this engine:

*	**Sprites Are Autonomous Actors:** Each sprite is an independent entity
	hosting its own coordinate state, visual atlas, and attached behavior
	components.

*	**Chained Lifecycle Sequencing:** State transitions and cinematic sequences
	do not rely on global step counters. They advance naturally through death
	callbacks (`:sp_death`) and terminal animation frames (`-1`), where one
	entity's completion automatically triggers the next phase.

*	**Zero Display-Scale Pollution:** The entire simulation operates within an
	isolated, immutable 1x coordinate space (320x240 screen, 16x16 tiles,
	32x32 player hull). The simulation logic never computes zoom factors;
	the rendering bridge handles all display scaling transparently.

---

## 2. The Entity-Component Model (`Sprite`)

### 2.1 The Host Object

Defined in `apps/games/onslaught/sprite.inc`, `Sprite` subclasses ChrysaLisp's
core scene-graph node `View`:

```lisp
(defclass Sprite (canvas_l canvas_r width height tag &optional death) (View)
	; (Sprite canvas_l canvas_r width height [death]) -> sprite
	(def this :color 0
		:sp_x 0 :sp_y 0 :sp_w 0 :sp_h 0
		:sp_canvas_l canvas_l :sp_canvas_r canvas_r :sp_canvas_tag tag
		:sp_frame 0 :sp_dir -1 :sp_components (list) :sp_death (ifn death :nil))
	(.-> this (:sp_set_pos 0 0) (:sp_set_size width height)))
```

Key state properties:

*	`:sp_x`, `:sp_y`, `:sp_w`, `:sp_h`: Unscaled 1x logical coordinates.

*	`:sp_canvas_l`, `:sp_canvas_r`: Preloaded directional atlas sheets.
	Facing direction (`:sp_dir`, `-1` for left, `1` for right) determines
	which canvas is sampled during `:draw`.

*	`:sp_frame`: Current atlas frame index. Setting this to `-1` kills the
	sprite.

*	`:sp_components`: List of component update functions executed every frame.

*	`:sp_death`: Optional callback closure invoked when the sprite dies.

### 2.2 Component Registration and Update

A component is simply a function that accepts `this` as its argument. It may
optionally supply an initializer:

```lisp
(defmethod :add_component (update &optional init)
	; (. sprite :add_component update [init]) -> sprite
	(when init (init this))
	(push (get :sp_components this) update)
	this)
```

During each frame tick, the layer manager invokes `(. sprite :update)`, which
iterates through `:sp_components`. If any component sets `:sp_frame` to `-1`,
the entity immediately terminates execution of remaining components and
triggers `:kill`:

```lisp
(defmethod :update ()
	; (. sprite :update) -> sprite
	(if (= (get :sp_frame this) -1)
		(. this :kill)
		(progn
			(each (lambda (fn)
				(unless (= (get :sp_frame this) -1)
					(fn this)))
				(get :sp_components this))
			(when (= (get :sp_frame this) -1)
				(. this :kill))))
	this)
```

---

## 3. The Standard Component Toolkit

When assembling entities, prefer composing existing components from
`apps/games/onslaught/sprite.inc` before writing custom behavior:

### 3.1 Kinematic Vector Movement (`mv-*`)

Applies continuous velocity, acceleration, and terminal velocity clamping:

*	**Initialization:** `(mv-init this)` defines `:mv_vx`, `:mv_vy`, `:mv_ax`,
	`:mv_ay`, `:mv_max_vx`, `:mv_max_vy`.

*	**Update:** `(mv-update this)` updates velocities by acceleration, clamps to
	max limits, and advances `:sp_pos`.

*	**Usage Pattern:**

	```lisp
	(.-> this
		(:add_component (const mv-update) (const mv-init)))
	(set this :mv_ay 1 :mv_max_vy 10) ; gravity
	```

### 3.2 Discrete Table Movement (`mt-*`)

Guides an entity through a scripted series of relative displacements:

*	**Initialization:** `(mt-init this)` defines `:mt_table`, `:mt_speed`,
	`:mt_count`, `:mt_index`.

*	**Update:** `(mt-update this)` steps through `(dx dy)` tuples, advancing
	position every `:mt_speed` ticks and looping cleanly at the end.

### 3.3 Linear Bresenham Interpolation (`ml-*`)

Moves an entity along a straight-line vector to exact target coordinates:

*	**Initialization:** `(init-ml this x y x1 y1 [speed])` computes
	differential stepping variables (`d1x`, `d1y`, `d2x`, `d2y`, `slope`,
	`count`).

*	**Update:** `(ml-update this)` steps up to `:ml_speed` units per tick along
	the major axis. Once `:ml_count <= 0`, it snaps to target `(x1, y1)` and
	stops.

### 3.4 Table Animation with Auto-Kill (`at-*`)

Drives multi-frame sprite atlas animations:

*	**Initialization:** `(at-init this)` defines `:at_table`, `:at_speed`,
	`:at_count`, `:at_index`.

*	**Update:** `(at-update this)` steps through atlas indices.

*	**Auto-Kill Contract:** Placing `-1` as the final entry in `:at_table`
	causes the sprite to automatically invoke `(. this :kill)` upon animation
	completion.

*	**Usage Pattern (Transient Particle):**

	```lisp
	(.-> this
		(:add_component (const at-update) (const at-init))
		(:set_frame +frm_32c32_bigexp))
	(set this
		:at_speed 2
		:at_table (list +frm_32c32_bigexp
			(+ +frm_32c32_bigexp 1)
			(+ +frm_32c32_bigexp 2)
			-1)) ; auto-kill at end
	```

### 3.5 Viewport Boundary Culling (`offscreen-update`)

Monitors entity visibility relative to `*world_scroll*`. As soon as the sprite's
bounding box moves completely out of the viewport window, it invokes
`(. this :kill)`. Use this on bullets, arrows, and blood drops.

---

## 4. Autonomous Sequence Chaining & Death Hooks

A key architectural paradigm of the engine is that complex cinematic or game
sequences are constructed by chaining entities together via death callbacks
(`:sp_death`) and arrival hooks.

### 4.1 Case Study: The Intro Title Sequence (`title.inc`)

The famous title sequence requires zero central orchestrator code. Each stage
spawns the next when it dies or reaches its target:

1.	**Letter Flight:** Each letter `TitleLetter` uses `ml-update` to fly toward
	its designated header slot.

2.	**Letter Arrival:** Its arrival callback `cp-letter` triggers an explosion
	sound, spawns a transient `ExplosionSprite` cluster (which self-kills via
	`at-table`), and launches the next letter.

3.	**Sword Plunge:** After letter 9 arrives, a 16-tick pause initiates
	`TitleSword`, which drops through the "A" glyph via `ml-update`.

4.	**Blood Drip:** When the sword hits `y=52`, its callback triggers
	`*sfx_clash2*`, spawns sparks (`FizzSprite`), and creates a falling blood
	droplet (`TitleBloodDrip`).

5.	**Autonomous State Transition:** The blood drop uses `mv-update` with
	gravity and `offscreen-update`. Its death callback is wired to `dt-drip`:

	```lisp
	(defun dt-drip (this)
		; death hook for drip
		(audio-play-rpc *sfx_drip*)
		(setq *title_state* :done))
	```

When the blood droplet falls off the screen, `offscreen-update` kills it,
triggering `dt-drip`, which plays the drip sound and sets `*title_state*` to
`:done`. The main game loop detects this and launches the field battle:

```lisp
(case *game_state*
	(:title
		(title-sequence-update)
		(when (eql *title_state* :done)
			(setq *game_state* :field)
			(field-sequence-start)))
	(:field
		(field-sequence-update)))
```

---

## 5. Coordinate Space Isolation & Resolution Independence

A core discipline of the Onslaught engine is strict isolation of game
coordinates from display coordinates:

*	**Logical 1x Space:** The game world is strictly defined in unscaled units:

	*	Virtual screen: `320 x 240` (`+screen_width`, `+screen_height`).

	*	Tilemap dimensions: `128 x 16` tiles (`+map_width`, `+map_height`).

	*	Tile grid size: `16 x 16` pixels (`+tile_width`, `+tile_height`).

	*	Status panel HUD: Height `72` pixels (`+panel_height`).

*	**The Scaling Bridge:** Only the view boundary wrappers apply `*zoom*`:

	```lisp
	(defmethod :sp_set_pos (x y)
		; (. sprite :sp_set_pos x y) -> sprite
		(def this :sp_x x :sp_y y)
		(. this :set_pos (* *zoom* x) (* *zoom* y)))

	(defmethod :sp_set_size (w h)
		; (. sprite :sp_set_size w h) -> sprite
		(def this :sp_w w :sp_h h)
		(. this :set_size (* *zoom* w) (* *zoom* h)))
	```

*	**Asset Rescaling:** When the user toggles zoom level (`window-resize` in
	`app_impl.lisp`), the engine calls `(load-cpm-assets *zoom*)` to reload and
	scale canvases using `(Canvas:resize)` and regenerates flipped right-facing
	sheets with `(Canvas:flip_x)`. Active sprites simply update their internal
	canvas references via `(rescale-active-sprites)`. **Entity positions,
	velocities, bounding boxes, and collision math remain completely
	untouched.**

---

## 6. Environment & Collision Engine (`FieldMap`)

Terrain interaction is handled by the `FieldMap` view (`map.inc`).

### 6.1 Binary Zero-Allocation Indexing

Map terrain is stored in flat binary byte strings (`fieldmapX.dat`,
`mapflags1.dat`). Rather than using multidimensional lists or allocations,
tiles are indexed in O(1) time using the ChrysaLisp `code` primitive:

```lisp
(defmethod :get_tile_flags (x y)
	; (. map :get_tile_flags x y) -> flags
	(if (and (< -1 x (const (* +map_width +tile_width)))
			(< -1 y (const (* +map_height +tile_height))))
		(if (and (defq md (get :map_data this)) (defq fd (get :flags_data this)))
			(code fd 1 (code md 1 (+ (/ x +tile_width) (* (/ y +tile_height) +map_width))))
			+fmap_stand)
		+fmap_stand))
```

*	`+fmap_stand` (`0x02`): Solid walkable surface.

*	`+fmap_climb` (`0x04`): Ladder rung traversable vertically.

*	`+fmap_masked` (`0x01`): Foreground overlay masking.

### 6.2 Dual-Probe Ground Sampling

Physics controllers (such as `player-man-update` in `fanatic.inc`) sample the
terrain using two horizontal probes under the entity's footprint (e.g.,
offsets `x + 8` and `x + 23` for a 32x32 entity).

*	If neither probe detects `+fmap_stand` or `+fmap_climb`, gravity increments
	downward velocity `yv` up to terminal speed (`10`).

*	When a surface is detected, the entity snaps cleanly to the top of the tile
	grid:

	```lisp
	(setq y (- (* (/ y1 tile_h) tile_h) h) yv 0 xv 0)
	```

*	When climbing ladders (`+fkey_up` / `+fkey_down`), detecting `+fmap_climb`
	locks the character's horizontal coordinate directly to the ladder tile
	edge, aligning the climbing animation with the rungs.

---

## 7. Scene Graph Layering & Depth Hierarchy

Entities reside within dedicated sibling layers inside `*world_layers*`
(`widgets.inc`):

```
+world_layers (Viewport container, scrolls via update-camera)
  ├── *layer_sky*       : Sky gradient background
  ├── *layer_land*      : FieldMap tile renderer
  ├── *layer_bans*      : Static poles and level markers
  ├── *layer_items*     : Collectible powerups, gold, spells
  ├── *layer_enemies*   : Enemy cavalry, soldiers, monsters
  ├── *layer_player*    : Player hero (Fanatic)
  ├── *layer_bodies*    : Slain combatants and corpses
  ├── *layer_missiles*  : Arrows, cannonballs, magic missiles
  └── *layer_fx*        : Explosions, blood, sparks, flying letters
```

*	**Z-Ordering:** Visual priority is determined entirely by child order in
	`*world_layers*`. Sprites do not need Z-index sorting.

*	**HUD Isolation:** `*panel_layers*` sits outside `*world_scroll*`. The HUD
	is drawn in fixed screen coordinates while the world scrolls beneath it.

---

## 8. Prescriptions for Agents Modifying Onslaught

When extending or maintaining this engine, follow these strict disciplines:

1.	**Never Pollute Entity Math with `*zoom*`:**
	Keep all motion, velocity, collision boxes, and distances in 1x space. Use
	`:sp_set_pos`, `:sp_get_pos`, and `:sp_set_bounds`—these methods handle
	scaling internally.

2.	**Implement Behavior via Components, Not Monolithic Updates:**
	When adding a new monster, weapon, or effect, compose it from `mv-*`,
	`at-*`, `mt-*`, or write a focused component closure. Attach it with
	`(. this :add_component (const my-comp-update) (const my-comp-init))`.

3.	**Kill Entities via Frame `-1`:**
	To remove a sprite, set its frame to `-1` or ensure its animation table
	terminates with `-1`. Let the engine trigger death hooks and unlink the
	view.

4.	**Use Death Callbacks for Sequencing:**
	Chain cinematics and multi-stage behaviors using the `:sp_death` closure
	argument rather than tracking frame timers in global state variables.

5.	**Use Binary `code` Access for Level Data:**
	Always read tile maps and collision flags via single-byte string indexing
	`(code data 1 offset)`. Never allocate nested lists or maps for terrain
	lookups.

6.	**Adhere to ChrysaLisp Style Guidelines:**

	*	Indent with 4-space tab characters.

	*	Start source comments with lowercase letters.

	*	Wrap documentation at 80 columns.

	*	Maintain blank lines between all markdown elements.
