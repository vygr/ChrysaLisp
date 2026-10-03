---
name: onslaught
display-name: ChrysaLisp Onslaught
description: Use when writing, reviewing, or modifying the Onslaught 2D entity-component engine (apps/games/onslaught/) — sprites, components, collisions, and cinematic sequencing.
---

# ChrysaLisp Onslaught 2D Entity-Component Engine

## Contents

Find the task below and read that section, or that file, in full before acting,
rather than skimming. Sections marked mandatory apply to every task. The
section numbers are kept across the files, so "section 8.6" is in
`campaign.md`.

Reference files, in this folder, read on demand:

*	**[sprite-model.md](sprite-model.md)**
	Read this before touching `sprite.inc`, writing a component, or changing
	how sprites are killed. Sections 2.

*	**[components.md](components.md)**
	Read this before using or changing a movement, animation or collision
	component. Sections 3.

*	**[gameplay-systems.md](gameplay-systems.md)**
	Read this before changing combat, weapons and items, or how the player
	moves and the camera follows. Sections 4, 5, 6.

*	**[campaign.md](campaign.md)**
	Read this before changing the campaign map, the mind duel, save and load,
	or the remote play service and its bot. Sections 8.

Sections of this file:

*	**[Domain & Scope](#domain--scope)**
	Which files make up the game, and what "as the C++" refers to.

*	**[1. Core Engineering Philosophy](#1-core-engineering-philosophy)**
	The four principles the engine is built on.

*	**[2. The Entity-Component Model (`Sprite`)](#2-the-entity-component-model-sprite)**
	In `sprite-model.md`. The `Sprite` host object, the `env-push` invocation
	contract, reaching component variables, and the kill and death hook
	contract.

*	**[3. The Standard Component Suite (`sprite.inc`)](#3-the-standard-component-suite-spriteinc)**
	In `components.md`. Each component: `CP`, `MV`, `MT`, `ML`, `AT`, `CL`, and
	offscreen culling.

*	**[4. Collision & Combat Subsystem (`collisions.inc`)](#4-collision--combat-subsystem-collisionsinc)**
	In `gameplay-systems.md`. How collisions are centralised and the order they
	run in.

*	**[5. Addon Weapon & Item Subsystem (`addons.inc`)](#5-addon-weapon--item-subsystem-addonsinc)**
	In `gameplay-systems.md`. The `Addon` class and the literal list rule for
	tables.

*	**[6. Player Physics, Ducking & Camera (`fanatic.inc`, `utils.inc`)](#6-player-physics-ducking--camera-fanaticinc-utilsinc)**
	In `gameplay-systems.md`. Ducking bounds, camera tracking, and the jump
	arc.

*	**[7. Task Isolation & Rendering](#7-task-isolation--rendering)**
	What the GUI task can and cannot see when it draws a sprite.

*	**[8. Campaign Map (`campaign.inc`)](#8-campaign-map-campaigninc)**
	In `campaign.md`. Campaign state, game flow, mind combat, drawing, stack
	depth, and the remote play service and bot.

*	**[9. Prescriptions for Agents Modifying Onslaught](#9-prescriptions-for-agents-modifying-onslaught)**
	Mandatory. The numbered rules to follow for any change to the game.

*	**[10. Testing & Verification Workflow](#10-testing--verification-workflow)**
	How to check a change statically, then run the game and the bot.

## Domain & Scope

*	**Target Application:** `apps/games/onslaught/`

*	**Primary Files:** `sprite.inc`, `addons.inc`, `collisions.inc`,
	`fanatic.inc`, `enemy.inc`, `title.inc`, `widgets.inc`, `map.inc`,
	`sky.inc`, `utils.inc`, `assets.inc`, `enums.inc`, `battle.inc`,
	`menu.inc`, `mind.inc`, `campaign.inc`, `app.inc`, `remote.inc`,
	`app_impl.lisp`, `cmd/onslaught.lisp`

*	**The C++ Version:** The game is a complete port of an earlier C++
	version (`onslaught.cpp`, with its sprite engine in `engine.cpp`). Where
	this skill says "as the C++" it means that version. Its source is not in
	this repository.

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

This section is in [sprite-model.md](sprite-model.md). Read it there in full
before working on this part of the game. It covers:

*	2.1 The Host Object

*	2.2 The `env-push` Invocation Contract

*	2.3 External Access to Component Variables

*	2.4 Known Component Index Access

*	2.5 Destruction & Death Hook Contract

## 3. The Standard Component Suite (`sprite.inc`)

This section is in [components.md](components.md). Read it there in full before
working on this part of the game. It covers:

*	3.1 Procedural Component Wrapper (`CP`)

*	3.2 Kinematic Vector Movement (`MV`)

*	3.3 Discrete Table Movement (`MT`)

*	3.4 Linear Bresenham Interpolation (`ML`)

*	3.5 Table Animation (`AT`)

*	3.6 Collision Component (`CL`)

*	3.7 Viewport Boundary Culling (`offscreen-update`)

## 4. Collision & Combat Subsystem (`collisions.inc`)

This section is in [gameplay-systems.md](gameplay-systems.md). Read it there in
full before working on this part of the game. It covers:

*	4.1 Architecture

*	4.2 Pipeline Sequencing in `Fanatic`

## 5. Addon Weapon & Item Subsystem (`addons.inc`)

This section is in [gameplay-systems.md](gameplay-systems.md). Read it there in
full before working on this part of the game. It covers:

*	5.1 Reusable `Addon` Sprite Class

*	5.2 Prebinder Literal List Rule for Tables

## 6. Player Physics, Ducking & Camera (`fanatic.inc`, `utils.inc`)

This section is in [gameplay-systems.md](gameplay-systems.md). Read it there in
full before working on this part of the game. It covers:

*	6.1 Ducking Bounds & Texture Sampling (`cp-manduck`)

*	6.2 Ground-Anchored Camera Tracking (`update-camera`)

*	6.3 Parabolic Jump Arc

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

This section is in [campaign.md](campaign.md). Read it there in full before
working on this part of the game. It covers:

*	8.1 Campaign State

*	8.2 Game Flow

*	8.3 Mind Combat (`mind.inc`)

*	8.4 Drawing

*	8.5 Stack Depth

*	8.6 Remote Play Service (`app.inc`, `remote.inc`)

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
