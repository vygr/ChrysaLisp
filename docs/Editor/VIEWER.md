# Viewer

The `Viewer` application is a single buffer programers file viewer tailored for
the ChrysaLisp environment, language and file types present within the
ChrysaLisp file tree.

If you hover the mouse over the embedded UI below you can see the kind of
features available. There are more features available through the key bindings
which can be found in the `KEYS.md` documentation.

The viewer can only open a single file at a time, and you can't change any file
contents. But you can copy content to the clipboard and search for things same
as in the full `Editor` application.

You can have multiple instances of the viewer open at the same time, as there
is no user persistent state stored.

## UI

```widget
apps/viewer/widgets.inc *window* 0 512
```
