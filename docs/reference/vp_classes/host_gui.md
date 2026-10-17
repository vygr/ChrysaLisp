# :host_gui

```image
docs/reference/vp_classes/host_gui.cwb
```

## Lisp Bindings

### (gui-deinit) -> :nil

### (gui-event) -> :nil | str

### (gui-info) -> (mx my sw sh)

### (gui-init screen) -> screen

### (gui-read-screen x y w h) -> :nil | str

### (gui-update mx my flags) -> :nil

## VP methods

### :begin_composite -> :nil

### :blit -> :nil

### :box -> :nil

### :clip_free -> :nil

### :clip_get -> :nil

### :clip_put -> :nil

### :composite -> service/gui/composite

```code
inputs
:r0 = root view object (ptr)
outputs
:r7 = x (pixels)
:r8 = y (pixels)
:r9 = width (pixels)
:r10 = height (pixels)
trashes
:r0-:r14, :f0-:f15
info
the dirty region bounds is returned for use by the :flush call !
```

### :create_texture -> :nil

### :deinit -> :nil

### :destroy_texture -> :nil

### :end_composite -> :nil

### :filled_box -> :nil

### :flush -> :nil

### :frame_free -> service/gui/frame_free

```code
trashes
:r0-:r5, :f0-:f15
info
the dirty regions of the frame freed, its views let go of, and its
list, here, not on the host, as a view let go of may be the last of it
```

### :frame_view -> service/gui/frame_view

### :init -> :nil

### :mesh_create -> :nil

### :mesh_destroy -> :nil

### :pair_create -> :nil

### :poll_event -> :nil

### :read_screen -> :nil

### :read_texture -> :nil

### :replay -> service/gui/replay

```code
inputs
:r0 = the frame's dirty bounds (ptr)
trashes
:r0-:r4, :r7-:r14, :f0-:f15
info
on the host's stack: each view of the frame, for each rect of its
dirty region, clipped to it, its list of draws played where it is
```

### :resize -> :nil

### :set_clip -> :nil

### :set_color -> :nil

### :set_texture_color -> :nil

### :shader_create -> :nil

### :shader_destroy -> :nil

### :shader_draw -> :nil

### :shader_format -> :nil

### :shader_texture -> :nil

### :tris_draw -> :nil

### :update -> service/gui/update

```code
inputs
:r0 = mouse x
:r1 = mouse y
:r2 = update flags
trashes
:r0-:r14, :f0-:f15
info
the whole frame is one callback, on the host's stack, so no task can
change a view while it is worked out: made or sized on the host, the
views to draw found, those that must draw again drawing into their
lists, Lisp or VP, the others' kept lists used, then the frame put on
the back buffer and the screen
```

