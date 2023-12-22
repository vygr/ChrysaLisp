# TODO

```image
apps/images/data/sticker.svg
```

In no particular order but to just to ensure I get thoughts down. I'll keep
adding to this as I go along. If anybody would like to try helping out, then
just get in touch.

* Recode quasi quote non recursive?

* optimize write after write vp opts.

* canvas to support stretched or aspect correct drawing, centered, and aligned
flags.

* ui-load-icon .... for button images.

* gen-ellipse needs better eps scaling.

* WYSIWYG UI tree Editor.

* Extend `(split)` to allow splitting of any sequence by any sequence. Any
element that `(find)` returns true for, within the splitter, counts as a
splitting element.

* Update File picker app to use a Tree widget for the folder list. With
collapse and expand buttons.

* Break out the Terminal tab completion into a separate library. Add this to
the Editor new file Textfield. Also add this to the File picker apps file
Textfields.

* Fix the auto addition of whole words only mode in the `query` function.
Either by changing the `Regexp` class or the wrapping code.

* Extend `(bfind)` to full `(find)` set of options. Test with Dictionary class
to speed up typing completions.

* Dynamic doc reference pages for the translators ! Plugin for VP -> native
codes etc.

* Add cycle protection to the `(debug-sanitize)` call.

* Integrate the HOST clipboard and the CL Clipboard service.

* Add shift key selection to the Terminal app.

* Change Regexp compiler to produce VP code and hold a cache of translated
expressions.

* Gui remote desktop service that runs over CLB.

* Move `vp-min, vp-max, vp-abs` into the VP VM proper. ARM64 and x64 have
native operations `cmov` that could implement these.

* Environment browser GUI app to aid debugging and housekeeping.

* `env.inc` for Guest should always run prior to the user `env.inc` file to
ensure defaults present for all env vars.

* Forth that outputs words compiled to VP ops !!! I will do this just to honor
Chuck !

* Separate the VP instruction for div and rem into individual operations. The
x64 isn't really the way things should be done anymore.

* Add a faster way to test for trashed registers from `emit` ops. Maybe
redesign the format to make it easier to test. This will speed up most of the
optimization stages of the compiler and assembler.

* Add support for `(vp-simd)` instructions at the translator level. MMX, SSX
etc.

* SDL2 Audio output, plus the implementation of a ChrysaLisp multi channel
mixer service.

* Add support for FPU to the VM. Scalar float/double support first of all maybe
eventually create a FPU accelerated version of the vector math DSL.

* Create a Virtualbox boot image of the system. I've been looking at bare metal
OS and talking to the author for a couple of months. He's got plans to
implement a frame buffer device for bare metal. At that point I think I would
have everything I need to do a platform isolation interface targeting bare
metal as the bootstrap and take over the UI work directly rather than going
through SDL. At this point I would start writing device drivers for the virtual
device interfaces provided by the VM environment.

* Create or port an existing TCP/IP stack. Provide this as a standard service
on the network. Implement another link driver type that uses TCP/IP in order to
experiment with bridging multiple computers together into a single addressable
network. I'm very tempted to create a link driver based on the Nanomsg library,
this could prove interesting but would only work on hosted systems, still worth
doing.

* Create a vp-code binary encoding. Heterogeneous applications work currently,
just using the separated name spaces in the obj/ directory. But I'd quite like
to get to the point where there is a single virtual binary package for an
application for distribution. At this point I would implement the ability for
applications to have native coded versions of functions available and those
functions would be bound to in preference to the VP versions when a task finds
itself on such an architecture.

* Investigate getting LLVM/GCC to produce functions in ChrysaLisp format. One
of the advantages of the simple function format using a local Vtable for all
calls and jumps is that there is no code patching needed. The loader does not
need to concern itself with patching a variety of native binary formats, it
only concerns itself with looking up function names and writing to Vtable
entries.

* Webassembly backend in order to experiment with using browsers and web socket
links to create a distributed network across the Internet.

* Create a more comprehensive command line suite of tools. I don't think this
needs to be especially large or even go as far as Busybox. But some of the more
useful text processing and helpful commands would go a long way.

* Other targets and platform configs. Bare metal PI3/Riscv64 and other
development board support along with GPIO/SPI/I2C and so forth for the maker
community. EGLFS version of GUI driver. L4 micro kernel hosted version could
prove interesting.

* Network wide DHT based object store, provided as a service. I've been
planning a network wide object store that presents itself as a standard service
and uses a distributed hash table technique to store objects throughout the
network with redundancy.
