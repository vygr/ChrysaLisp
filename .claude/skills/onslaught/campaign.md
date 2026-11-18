# Onslaught: Campaign Map, Mind Combat and Remote Play

Part of the `onslaught` skill. Read this before changing the campaign map, the
mind duel, save and load, or the remote play service and its bot.

## Contents

*	[8.1 Campaign State](#81-campaign-state)

*	[8.2 Game Flow](#82-game-flow)

*	[8.3 Mind Combat (`mind.inc`)](#83-mind-combat-mindinc)

*	[8.4 Drawing](#84-drawing)

*	[8.5 Stack Depth](#85-stack-depth)

*	[8.6 Remote Play Service (`app.inc`, `remote.inc`)](#86-remote-play-service-appinc-remoteinc)

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

*	`@Onslaught` is a system wide name, so only finds a game on the same
	machine. A mailbox is good from anywhere though. `onslaught -i` prints
	the mailbox id of the game's service, and `onslaught -m id` plays the
	game with that id, by setting `*onslaught_mbox*` in `app.inc`, which
	`(onslaught-service)` uses in place of the lookup. So a game on another
	machine of a cluster can be played from here once its id is known.
	`tests/net/remote_onslaught.lisp` does it: it opens the game on another
	machine's GUI node, has the id mailed back, and runs the bot here.
