# Editor

The `Editor` application is a multi buffer programers editor tailored for the
ChrysaLisp environment, language and file types present within the ChrysaLisp
file tree.

If you hover the mouse over the embedded UI below you can see the kind of
features available. There are more features available through the key bindings
which can be found in the `KEYS.md` documentation.

The editor can open multiple files at a time, but you can only have a single
instance of the editor open at once, as there is user persistent state stored.
This state is saved and loaded to maintain a consistent project work
environment between sessions.

## UI

```widget
apps/edit/widgets.inc *window* 0 512
```
