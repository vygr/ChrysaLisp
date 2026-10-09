# The Symbol Font

`fonts/Symbols.ctf` is the font the toolbars are drawn with, a symbol for
each thing a button does. It is ChrysaLisp's own, drawn for what its apps
need, and the system makes it: the symbols are data in the repo and a
command turns them into the font.

```code
symbols        make the fonts and the names
symbols -l     list the symbols and their codes
```

## A symbol by name

Every symbol has a name, `+sym_undo`, `+sym_save_all`, `+sym_find_global`,
in `lib/consts/symbols.inc`, and a toolbar is written with names:

```vdu
(ui-tool-bar *main_toolbar* ()
	(ui-buttons (+sym_undo +sym_redo +sym_cut +sym_copy +sym_paste) +event_undo))
```

`(ui-buttons)`, `(ui-radio-bar)`, `(ui-toggle-bar)` and `(ui-title-bar)`
take a name, a code, or a str that is shown as it is. Elsewhere it is
`(num-to-utf8 +sym_warning)`.

Each meaning has one symbol and each symbol one meaning. A name that ends
`_all` is its symbol with three dots, for all of them, `save_all`. One that
ends `_global` is its symbol with a ring, for everywhere, `find_global`.

## How a symbol is drawn

On a grid of 24, with strokes that are all one weight, and a few shapes
that are filled. A symbol is a list of those, made with a small kit,
`lib/font/symbols.inc`:

| | |
|---|---|
| `(s-line x y x y ...)` | a stroke from point to point |
| `(s-loop x y x y ...)` | a stroke that comes back to its start |
| `(s-arc cx cy r a0 a1)` | part of a circle, in degrees |
| `(s-ring cx cy r)` | a circle, its line |
| `(s-rrect x y x1 y1 [r])` | a rectangle with round corners, its line |
| `(s-disc cx cy r)` `(s-box x y x1 y1)` `(s-fill x y ...)` | filled |
| `(s-arrow x y x1 y1)` `(s-arc-arrow ...)` `(s-head ...)` | arrows |
| `(s-corners)` | the four corners of a frame |
| `(s-mirror)` `(s-flip)` `(s-turn)` `(s-small)` | a thing changed |
| `(s-all)` `(s-global)` | a thing with a badge |

They give lists, put together with `cat`. The symbols are
`lib/font/symbol_set.inc`, and the parts more than one of them use, the
document, the folder, the lens, are named at its top:

```vdu
(list "new" (cat s_doc (s-line 9 15 15 15) (s-line 12 12 12 18)))
(list "zoom_in" (cat s_lens (s-line 8 11 14 11) (s-line 11 8 11 14)))
(list "save_all" (s-all s_floppy))
```

That they look like one family is from the rules, not from the drawing:
one grid, one weight, round ends and corners, the same arrow head and the
same document everywhere.

## To change or add one

Change it in `lib/font/symbol_set.inc` and run `symbols`. The fonts and
the names are written again, and an app shows it the next time it is
started. A new symbol goes on the end of the list. The code of a symbol
is where it is in the list, so one put in the middle, or taken out, moves
every code after it. That is safe for apps, they use the names, and not
for anything that kept a code.

`tests/system/test_symbols.lisp` fails if the fonts or the names in the
repo are not what the symbols make, so the two can not drift apart. Run
`symbols` and commit what it writes.

## Themes

A theme is the same symbols with another weight of stroke, and other ends
and corners. They are a list in `lib/font/symbol_set.inc`, and `symbols`
makes a font for each:

| font | stroke | ends and corners |
|---|---|---|
| `fonts/Symbols.ctf` | 2.3 of 24 | round |
| `fonts/Symbols-Light.ctf` | 1.6 | round |
| `fonts/Symbols-Bold.ctf` | 3.0 | round |
| `fonts/Symbols-Sharp.ctf` | 2.3 | square, mitred |

There is one set, and a small arrow is one of it at a small size. The
arrows of a spinner, the toggle of a folder in the files widget and of a
category in the launcher are `prev`, `next`, `up` and `down`, from the
theme's own font, `*env_tiny_symbol_font*` is it at 10 pixels.

Which theme a desktop has is chosen in the Themes app, and is kept for
the user, `usr/<user>/theme`. A change is seen at once: the GUI sends
every window an event, `+ev_type_theme`, and `(. window :event)` swaps the
window's symbol fonts for the new theme's, lays it out and draws it. An
app does nothing for this but pass on the events it does not know, as
they all do. `lib/theme/theme.inc`.

## How the font is made

Each stroke is given to the stroker of the path library,
`(path-stroke-polyline)`, the same one the Canvas draws lines with, and it
gives the stroke's outline. The outlines of a symbol's strokes are its
glyph, as they come. Where two strokes cross, the two outlines overlap,
and nothing is done about it: a glyph is filled by the non zero rule, as
a TrueType outline is meant to be, so what overlaps is filled once. A ring
has an outer and an inner outline that go opposite ways round, and the
inner one is a hole.

That holds only for outlines that go the same way round. Two that go
opposite ways cut a hole where they overlap, by this rule as by the other.
A stroke's outline always goes the one way, a disc from `(path-gen-arc)`
went the other, and the dot of `comment` had a bite out of it where its
tail left. So the outlines of each item are turned, `(sym-wind)`, all of
them together, till the biggest goes the way a stroke's does. A ring's
inner outline is still the opposite of its outer, and still a hole.

The font class drew glyphs by the odd even rule before, by which an
overlap is a hole. Every glyph of every font in `fonts/`, 723 of them, is
the same picture by either rule, so nothing else changed.

It is all fixed point, and the same bytes come out on an M4, an x86_64
Mac, a Raspberry Pi and the emulator. Four fonts of 112 symbols are made
in well under a second.

## Where it came from

The toolbars used Entypo, a general set of 335 symbols of which 90 were
in use, chosen as the nearest fit. What each was for was read from the
apps: a toolbar lists its symbols and its tooltips in the same order. 22
symbols were doing more than one job, the play triangle was also replace,
and three things were drawn two ways. Entypo is still in `fonts/`.
