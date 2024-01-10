# Collections, Maps, Sets and Trees

The collections classes are held in the `lib/collections/` folder. They consist of various maps and sets. Each subclass implements a common API. But the individual types are specialized for particular uses and performance characteristics.

At the VP level maps and sets are provided as VP classes. The environment
system is built on a single map class, the `hmap`. A Lisp level API interface
to this class is provided by the `Emap` subclass.

## Sets

All set classes inherit from the `Set` base class, `lib/collections/set.inc`.

```file
lib/collections/set.inc
```

### Fset

The `Fset` is a set class built with multiple lists, buckets, holding the keys.
It uses the `(hash)` function on the key to find the bucket and the `(find)`
function to search that bucket for the key.

When you are dealing with a large number of keys. This type of map reduces the
maximum number of keys that need to be tested during `find` operations.

### Xset

The `Xset` is a set class built with multiple lists, buckets, holding the keys.
By default, it uses the `(hash)` function on the key to find the bucket and the
`(eql)` function to search that bucket for key matches.

However, this set is extendable by providing your own hash and equality testing
functions on construction. This allows you to create arbitrary sets where the
keys can be anything your hash and equal function can work with.

## Maps

All map classes inherit from the `Map` base class, `lib/collections/map.inc`.

```file
lib/collections/map.inc
```

### Emap

The `Emap` is a map class built on the VP level `hmap` class. As such the keys
can only be symbol objects.

It implements multiple buckets and has excellent performance characteristics
due to this and its implementation in VP machine code.

### Lmap

The `Lmap` is a map class built with a single list holding the keys and a
single list holding the values. It uses the `(find)` function to search the key
list for `:find` operations.

When you are dealing with a relatively small number of keys and values. This
type of map has excellent cache line characteristics.

### Fmap

The `Fmap` is a map class built with multiple lists, buckets, holding the keys
and corresponding lists holding the values. It uses the `(hash)` function on
the key to find the bucket and the `(find)` function to search that bucket for
the key.

When you are dealing with a large number of keys and values. This type of map
reduces the maximum number of keys that need to be tested during `:find`
operations.

### Xmap

The `Xmap` is a map class built with multiple lists, buckets, holding the keys
and corresponding lists holding the values. By default, it uses the `(hash)`
function on the key to find the bucket and the `(eql)` function to search that
bucket for key matches.

However, this map is extendable by providing your own hash and equality testing
functions on construction. This allows you to create arbitrary maps where the
keys can be anything your hash and equal function can work with.

## Trees

Trees are an arbitrary mix of maps, sets, lists, and arrays, that can be loaded
and saved too and from streams.

Many applications adopt this format to save their state information. Such as
the Editor and Terminal applications.

### Loading

Loading a tree can be performed by use of the `(tree-load stream) -> form`
function.

For example, here is the `Editor` load state routine.

```file
apps/edit/state.inc "state-load" ""
```

### Saveing

Saving a tree can be performed by use of the `(tree-save stream form)`
function.

Here again is the `Editor` save state routine.

```file
apps/edit/state.inc "state-save" ""
```

## Scatter and gather functions

As you can see in the two examples above, functions are provided that enable
you to easily find values in a map, and replace values in a map. These are
known as gather and scatter operations respectively.

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
