# TODO

In no particular order but to just to ensure I get thoughts down. I'll keep
adding to this as I go along. If anybody would like to try helping out, then
just get in touch.

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

* Create a byte coded version of the VP instructions and implement a runtime
translator. The runtime translator could be as simple as expanding the byte
code into the existing emit buffer format and using the existing emit buffer
compilation technique. Heterogeneous applications should work currently, just
using the separated name spaces in the obj/ directory. But I'd quite like to
get to the point where there is a single virtual binary package for an
application for distribution. At this point I would implement the ability for
applications to have native coded versions of functions available and those
functions would be bound to in preference to the VP versions when a process
finds itself on such an architecture.

* Live hot patching of functions. This should be relatively easy because all
calls and jumps between functions go via a local Vtable at the end of the
function or via a class Vtable. Therefore all that needs to happen is a new sys
load function that doesn't check the existing function list but just loads the
new function and walks the existing function list patching all the Vtable
addresses. Re-claiming any existing space used by the old function is probably
not worth it at the moment. Although eventually the function list and function
blocks could be garbage collected, or functions could be compacted or moved
around using exactly the same hot patching mechanism.

* Live tracing and runtime statistics gathering. Like the live hot patching of
functions it should be possible to walk the function list and patch all the
Vtables to call via a debugging tracer or statistics gathering app.

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

* Other targets and platform configs. RISC V and MIPS targets. Bare metal PI3
and other Aarch64 development board support along with GPIO/SPI/I2C and so
forth for the maker community. SDL over frame buffer version for PI3, maybe
just needs testing, but needs doing. L4 micro kernel hosted version could prove
interesting.

* Better cooking of key modifiers !

* Network wide DHT based object store, provided as a service. I've been
planning a network wide object store that presents itself as a standard service
and uses a distributed hash table technique to store objects throughout the
network with redundancy.
