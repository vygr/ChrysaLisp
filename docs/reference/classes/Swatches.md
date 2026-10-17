# Swatches

```image
docs/reference/classes/Swatches.cwb
```

## Panel

```code
(Swatches board x y) -> swatches

about a point of the document
```

### :cell_items

```code
(. swatches :cell_items cell state) -> shapes
```

### :layout

```code
(. swatches :layout) -> (cells w h)

the inks, row by row, then how see through, two cells each, and none
at the end of that row
```

### :tap

```code
(. swatches :tap cell event) -> swatches
```

### :tip

```code
(. swatches :tip cell) -> :nil | str
```

