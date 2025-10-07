# TODO

```image
apps/images/data/sticker.svg
```

In no particular order but to just to ensure I get thoughts down. I'll keep
adding to this as I go along. If anybody would like to try helping out, then
just get in touch.

* there should be a clean separation between the various buffers for the
  Terminal app. User input, pipe output, history, prompt etc.

* add a `select_form` and `cut_form` actions to the Editor. This should select
the current atom or matched brackets.

* canvas to support stretched or aspect correct drawing, centered, and aligned
flags.

* add default parameters for `(slice)` ?.

* ui-load-icon .... for button images.

* WYSIWYG UI tree Editor.

* Fix the auto addition of whole words only mode in the `query` function.
Either by changing the `Regexp` class or the wrapping code.

* Dynamic doc reference pages for the translators ! Plugin for VP -> native
codes etc.

* Add cycle protection to the `(debug-sanitize)` call.

* Integrate the HOST clipboard and the CL Clipboard service.

* Change Regexp compiler to produce VP code and hold a cache of translated
expressions. This is less relevant since the `charclass` VP functions, but
still a very good exercise in Jit tech.

* Gui remote desktop service that runs over CLB.

* Move `vp-min, vp-max, vp-abs` into the VP VM proper. ARM64 and x64 have
native operations `cmov` that could implement these.

* Environment browser GUI app to aid debugging and housekeeping.

* Forth that outputs words compiled to VP ops !!! I will do this just to honor
Chuck !

* Separate the VP instruction for div and rem into individual operations. The
x64 isn't really the way things should be done anymore.

* Add support for `(vp-simd)` instructions at the translator level. MMX, SSX
etc.

* Audio service at the VP level should share and reference count the handles.

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
on the network.

* Create a vp-code binary encoding. Heterogeneous applications work currently,
just using the separated name spaces in the obj/ directory. But I'd quite like
to get to the point where there is a single virtual binary package for an
application for distribution. At this point I would implement the ability for
applications to have native coded versions of functions available and those
functions would be bound to in preference to the VP versions when a task finds
itself on such an architecture.

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
