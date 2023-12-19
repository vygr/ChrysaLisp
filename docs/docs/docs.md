# Docs

The `Docs` application is the Chrysalisp documentation viewer. It is used to
display tutorials as well as reference material auto generated from the source
code files.

Section handlers are loaded dynamically as required and given responsibility
for the embeding of content. Content ranges from wrapped text to imagesd and
live Lisp code snippets, including embeding the entire UI of applications. The
mechanisation for the section handlers is explained in the `event_dispatch.md`
document.

The Terminal command app `make docs` is used to scan and create the reference
documentation files.

If you hover the mouse over the embedded UI below you can see the kind of
features available. There are more features available through the key bindings
which can be found in the `keys.md` documentation.

## UI

```widget
apps/docs/widgets.inc *window* 512 512
```
