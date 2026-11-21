# Items

```image
docs/reference/classes/Items.cwb
```

## View

```code
(Items) -> items

a list of items one under another, as a flow down of all of them, of
which only those seen are views: put in a Scroll, it is the size of
the whole list, and as it is moved it has the views of the items in
the part of it its parent shows, asked for of :item_fn as they come
into it and let go of as they leave. Each item is the height of the
first, :item_h, and as wide as the list
```

### :constraint

```code
(. items :constraint) -> (width height)

as wide as asked, or as its first item, and as high as all its
items
```

### :draw

```code
(. items :draw) -> items

its paper behind the items, if it has its own
```

### :layout

```code
(. items :layout) -> items

opaque only of a paper of its own, which it draws
```

### :refresh

```code
(. items :refresh) -> items

the items it shows asked for again, the list the same length
```

### :set_bounds

```code
(. items :set_bounds x y width height) -> items

moved by what it is in, a Scroll scrolled: what is seen of it
shown
```

### :set_count

```code
(. items :set_count count) -> items

how many items there are, the views it holds let go of, and
the height of an item, if not given, that of the first. Its
owner lays out what it is in, (.-> scroll :layout :dirty_all)
```

### :show

```code
(. items :show) -> items

the views of the items its parent shows, those it has kept, the
others asked for, and those it has that are out of sight let go
```

