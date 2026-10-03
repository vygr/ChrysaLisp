# Onslaught: The Sprite Entity-Component Model

Part of the `onslaught` skill. Read this before touching `sprite.inc`, writing
a component, or changing how sprites are killed.

## Contents

*	[2.1 The Host Object](#21-the-host-object)

*	[2.2 The `env-push` Invocation Contract](#22-the-env-push-invocation-contract)

*	[2.3 External Access to Component Variables](#23-external-access-to-component-variables)

*	[2.4 Known Component Index Access](#24-known-component-index-access)

*	[2.5 Destruction & Death Hook Contract](#25-destruction--death-hook-contract)

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
