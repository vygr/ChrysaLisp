# Collections, Maps, Sets and Trees

The collections classes are held in the `lib/collections/` folder. They consist
of various maps and sets. Each subclass implements a common API, but the
individual types are specialized for particular uses and performance
characteristics.

At the VP level, maps and sets are provided as lightweight, highly optimized VP
classes. In addition to the `:hmap` (used for Lisp environments), ChrysaLisp
features native `:pset` (Property Set) and `:pmap` (Property Map) classes that
provide fast, O(1)-cached lookups on flat sequence buffers.

## Sets

All set classes inherit from the `Set` base class, `lib/collections/set.inc`.

```file
lib/collections/set.inc
```

### Fset

The `Fset` is a set class built with multiple `(pset)` buckets. It uses the
`(hash)` function on the key to find the appropriate bucket, and the highly
optimized `(pfind)` FFI function to search that bucket for the key.

When you are dealing with a large number of keys, this type of set reduces the
maximum number of keys that need to be tested during `:find` operations.

### Xset

The `Xset` is a set class built with multiple buckets holding the keys. By
default, it uses the `(hash)` function on the key to find the bucket and the
`(eql)` function to search that bucket for key matches.

However, this set is extendable by providing your own hash and equality testing
functions on construction. This allows you to create arbitrary sets where the
keys can be anything your hash and equal function can work with.

### Lset

The `Lset` is a set class built with a single, native `(pset)` holding the keys.
It uses the fast, O(1)-cached `(pfind)` FFI function to search the key set for
`:find` operations.

When you are dealing with a relatively small number of keys, this type of set
has excellent cache line characteristics and minimal memory overhead.

## Maps

All map classes inherit from the `Map` base class, `lib/collections/map.inc`.

```image
docs/diagrams/map_kinds.cwb
```

```file
lib/collections/map.inc
```

### Emap

The `Emap` is a map class built on the VP level `:hmap` class. As such, the keys
can only be symbol objects.

It implements multiple buckets and has excellent performance characteristics due
to this and its implementation in VP machine code.

### Lmap

The `Lmap` is a map class built on a single, native `(pmap)` (Property Map)
structure. It uses the fast `(pfind)` FFI function to search the flat map for
`:find` operations.

When you are dealing with a relatively small number of keys and values, this
type of map has excellent cache line characteristics and avoids any list
partitioning overhead.

### Fmap

The `Fmap` is a map class built with multiple `(pmap)` buckets. It uses the
`(hash)` function on the key to find the appropriate bucket, and the fast
`(pfind)` FFI function to search that bucket for the key.

When you are dealing with a large number of keys and values, this type of map
reduces the maximum number of keys that need to be tested during `:find`
operations.

### Xmap

The `Xmap` is a map class built with multiple buckets, each holding a list of
keys and a corresponding list of values. By default, it uses the `(hash)`
function on the key to find the bucket and the `(eql)` function to search that
bucket for key matches.

However, this map is extendable by providing your own hash and equality testing
functions on construction. This allows you to create arbitrary maps where the
keys can be anything your hash and equal function can work with.

## Emptying a part at a time

`(. set :empty)` lets go of all of a set or a map at once. With a count,
`(. set :empty count)` lets go of about that many items and gives `:t`
while there are more, `:nil` when there are none, so a big one can be let
go of a part at a time, with other work between, and is as good as ever
meanwhile. Those of many buckets let go of a bucket at a time in turn,
from where the last call stopped; an `Lset` or `Lmap` lets go of its last
items. An `Emap`'s env is let go of as one, whatever the count.

```lisp
(while (. big_set :empty 1024) (task-slice))
```

## Trees

Trees are an arbitrary mix of maps, sets, lists, and arrays that can be loaded
and saved to and from streams.

Many applications adopt this format to save their state information, such as the
Editor and Terminal applications.

### Loading

Loading a tree can be performed by use of the `(tree-load stream) -> form`
function.

For example, here is the `Editor` load state routine.

```file
apps/tools/edit/state.inc "state-load" ""
```

### Saving

Saving a tree can be performed by use of the `(tree-save stream form)` function.

Here again is the `Editor` save state routine.

```file
apps/tools/edit/state.inc "state-save" ""
```

## Tree nodes

`Tree-node`, `lib/collections/tree_node.inc`, is a node of a tree of objects,
not a form to save. A node has its children, in the order they were added, and
a weak link to its parent. The tree is owned from the top: letting go of the top
lets go of all of it, and a parent must outlive its children, a weak link to one
that has gone is not safe to follow.

A class that is a tree of things is made of it and adds what it is. `Scene-node`
of `lib/math/scene.inc` adds where an object is and its matrix, `Figure` of
`lib/motion/figure.inc` how it looks and moves. Its keys begin `:node_`, so a
class made of it can keep more in the same place.

```lisp
(import "lib/collections/tree_node.inc")
(defq top (Tree-node "top") a (Tree-node "a") b (Tree-node "b"))
(.-> top (:add_node a) (:add_node b))
(. a :add_node (Tree-node "a2"))
(map (# (. %0 :get_name)) (. top :find_nodes "a"))
```

* `:add_node` adds a child last, out of the parent it had.
* `:sub_node` takes a node out of its parent.
* `:set_children` sets all of its children, each given it as its parent.
* `:root` is the top of the tree a node is in.
* `:each_node` calls a function of a node and of every one under it, a parent
  before its children, they in order. What is still to come is a stack, the
  depth of the tree costs nothing.
* `:walk_nodes` calls one function going in to a node and another coming out of
  it, so a walk can keep a state per level, a matrix say. The first giving
  anything but `:nil` does not step into that node's children.
* `:find_nodes` is those whose names begin so.

```file
lib/collections/tree_node.inc
```

## Scatter and gather functions

As you can see in the two examples above, functions are provided that enable you
to easily find values in a map, and replace values in a map. These are known as
gather and scatter operations respectively.

Writing values can be done as follows:

```lisp
(defq my_map (Fmap) my_list (list))
(scatter my_map :x 45 :y "bert" :z (list 1 2 3))
(. my_map :each (# (push my_list (list %0 %1))))
my_list
```

Reading values can be done as follows:

```lisp
(defq my_map (Fmap))
(scatter my_map :x 23 :y "alf" :z '(a b c))
;read from the map
(bind '(x y z) (gather my_map :x :y :z))
(list x y z)
```
