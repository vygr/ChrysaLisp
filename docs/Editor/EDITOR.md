# Editor

The ChrysaLisp Editor application is a programers Editor tailored for the
ChrysaLisp environment, language and file types present within the ChrysaLisp
file tree.

If you hover the mouse over the Embedded UI below you can see the kind of
features available. There are more features available through the key bindings
which can be found in the `KEYS.md` documentation.

The Editor application can open multiple files at a time, but you can only have
a single instance of the Editor open at once, as there is user persistent state
stored. This state is saved and loaded to maintain a consistent project work
environment between sessions.

## UI

```widget
apps/edit/widgets.inc *window* 0 512
```
