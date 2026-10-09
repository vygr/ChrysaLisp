# Board

```code
(Board [doc]) -> board
```

### :add

```code
(. board :add item) -> id

as (:commit), and the id of it, for a script
```

### :align

```code
(. board :align how) -> board

line up what is selected, each with the box round all of it. how is
:left :right :top :bottom, or :hcenter or :vcenter for the middles
```

### :clear

```code
(. board :clear) -> board

take out everything
```

### :commit

```code
(. board :commit item) -> item

put an item on the layer that is drawn on, as a step that can be undone
```

### :delete

```code
(. board :delete) -> board

take out what is selected
```

### :dirty?

```code
(. board :dirty? flags) -> :t | :nil

has that changed, and it is taken as drawn
```

### :draw

```code
(. board :draw canvas [m clip]) -> count

the document, on a canvas, by the matrix it is seen by
```

### :draw_overlay

```code
(. board :draw_overlay canvas [m]) -> board

what is over the document and not of it: what is being drawn,
the box being dragged out, and the handles of what is selected
```

### :drop_temp

### :duplicate

```code
(. board :duplicate [dx dy]) -> board

a copy of what is selected, moved a little, default 16 each way,
and the copy is what is selected
```

### :float

```code
(. board :float ids) -> board

these items are being moved about. They are drawn over the
document, not in it, so that only they are drawn as they move
```

### :forget

```code
(. board :forget) -> board

nothing was done after all, the last (:snapshot) is dropped
```

### :get_bindings

### :get_doc

### :get_selected

### :get_stage

### :group

```code
(. board :group) -> :nil | id

what is selected becomes one group, where the top one of them was
```

### :layer_index

```code
(. board :layer_index) -> index

the layer that is drawn on, the top one unless one was chosen
```

### :new_shape

```code
(. board :new_shape event tool) -> shape

a shape for a pointer to draw, with the colour and width that
pointer is bound to, or the board's. It is drawn over the
document till it is kept
```

### :order

```code
(. board :order front) -> board

what is selected goes to the front of its layer, or the back
```

### :pointers

```code
(. board :pointers events) -> board

a batch of pointer events, each in the space of the document
```

### :put_state

### :redo

```code
(. board :redo) -> :t | :nil
```

### :remove

```code
(. board :remove ids) -> board

take items out, no step is made, see (:delete)
```

### :select

```code
(. board :select ids) -> board
```

### :select_all

```code
(. board :select_all) -> board
```

### :selected_items

```code
(. board :selected_items) -> items

the items that are selected, each one of a layer's own, in the
order they are in, the back one first
```

### :set_doc

```code
(. board :set_doc doc) -> board

another document, nothing of the last is kept
```

### :snap_x

### :snap_y

### :snapshot

```code
(. board :snapshot) -> board

what is about to be done can be undone
```

### :state

```code
(. board :state) -> state

the layers as they are, to go back to
```

### :style

```code
(. board :style [key val] ...) -> board

set the colour, fill, width and so on of every shape that is
selected, those in groups too
```

### :take_appended

```code
(. board :take_appended) -> items

the items put on top since this was last asked
```

### :touch

```code
(. board :touch flags) -> board

something has changed that is drawn
```

### :transform

```code
(. board :transform m) -> board

move what is selected by a matrix
```

### :undo

```code
(. board :undo) -> :t | :nil
```

### :ungroup

```code
(. board :ungroup) -> board

each group that is selected becomes its items, where it was, each
where it was on the board
```

