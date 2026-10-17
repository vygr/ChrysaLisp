# Palette

```image
docs/reference/classes/Palette.cwb
```

## Panel

```code
(Palette board x y [kind id]) -> palette

about a point of the document, for the pointer of that kind and id,
default the mouse. It is not on the stage till it is put there,
(palette-open) does both
```

### :cell_items

```code
(. palette :cell_items cell state) -> shapes

its back, or its ink, its icon, and what is current for its pointer
marked, or on a floating one a square of each device that holds it
```

### :devices

```code
(. palette :devices) -> ((color what slot) ...)

the dots of the devices, (. bindings :devices), each in its place
```

### :dismiss

```code
(. palette :dismiss event) -> :t | :nil

a pointer has gone down on the board, off this. It shuts if the
pointer is the one it belongs to: of its kind, and for a pen the
same pen. Did it
```

### :draw_state

```code
(. palette :draw_state) -> (mode color width pen snap pens devices)

what is current for its pointer, the inks of the pens as they are
now, and for a floating one the squares of the devices
```

### :get_cells

```code
(. palette :get_cells) -> cells

made again if what the board offers, swatches or the window, has
changed since they were made
```

### :layout

```code
(. palette :layout) -> (cells w h)

the tools, the pens of a board that has them else its colours, the
widths, and the things to do: the swatches if what shows the board
has them, :on_swatches, and the window if the board is shown with
nothing round it, :on_window
```

### :pick

```code
(. palette :pick what [val event]) -> palette

do what a tap on a cell does, see (:where). What it picks is the
board's to do, (. board :pick), for what the menu is for: a floating
menu, given the event of the tap, what tapped it, and it stays; one
opened for a pen, that pen; one opened by the mouse or a finger, the
board's own. One not floating shuts on a tool
```

### :tap

```code
(. palette :tap cell event) -> palette
```

### :tip

```code
(. palette :tip cell) -> :nil | str
```

### :value

```code
(. palette :value key) -> val

:mode, :color or :width, as the pointer it belongs to has it, a
pen that is bound to its own, or the board's
```

