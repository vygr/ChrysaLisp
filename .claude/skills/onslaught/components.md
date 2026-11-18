# Onslaught: The Standard Component Suite

Part of the `onslaught` skill. Read this before using or changing a movement,
animation or collision component.

## Contents

*	[3.1 Procedural Component Wrapper (`CP`)](#31-procedural-component-wrapper-cp)

*	[3.2 Kinematic Vector Movement (`MV`)](#32-kinematic-vector-movement-mv)

*	[3.3 Discrete Table Movement (`MT`)](#33-discrete-table-movement-mt)

*	[3.4 Linear Bresenham Interpolation (`ML`)](#34-linear-bresenham-interpolation-ml)

*	[3.5 Table Animation (`AT`)](#35-table-animation-at)

*	[3.6 Collision Component (`CL`)](#36-collision-component-cl)

*	[3.7 Viewport Boundary Culling (`offscreen-update`)](#37-viewport-boundary-culling-offscreen-update)

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
