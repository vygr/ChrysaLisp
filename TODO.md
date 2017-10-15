# ChrysaLisp

In no particular order but to just to ensure I get thoughts down. I'll keep
adding to this as I go along. If anybody would like to try helping out, then
just get in touch.

* Tidy up the GUI structure. I want to restructure the GUI objects so that
there is a minimal amount of methods and all of the properties of the View
objects are presented through their hash_map inheritance. To the Lisp I want a
GUI application to look like an environment tree where leaves of that tree
represent the UI elements, branching points likely layout containers. Lisp apps
should be able to interact with properties using the set and def functions. UI
elements inherit properties of parents such that a property like colour can be
set on the root of the UI tree and all of the elements in that tree, if they
don't override the colour property themselves, will have that colour.

* Enhance the signals and slots system so that as well as providing the
callback interface to multiple listeners there is a standard mechanism for
signalling on property changes and affecting properties of listening objects.
This may just end up being a set of standard callbacks that are provided as a
toolbox for users. Along with this should come a way to represent a UI tree in
standard Lisp syntax that can be read and inflated automatically, complete with
all the signals and slots prewired, before the application begins to interact
with it.

* Make the Lisp a first class citizen in terms of message passing ability. At
the moment the Lisp lives a little on the side of the rest of the system in
that it has no access to sending and receiving messages. The process launching
functionality for tasks should extend to a standard Lisp task and there should
be a standard way for such a Lisp process to read and send messages. This also
means there needs to be sensible way for the Lisp to handle formatting and
reading of mail message data. I've not yet decided how to approach this and
initially I may just map the mail message data to a string and take a string
when sending a message. Marshalling data in and out of mail messages is
something that I don't want to get too complicated but I can see eventually
that some higher-level data marshalling protocol would be handy.

* Implement a much better native call interface for the Lisp. At the moment
there is a call function that allows a restricted set of objects and methods to
be accessed. What's needed is a much more encompassing system that allows the
Lisp access to all objects and methods without needing to create bindings for
every single instance. If this is done in an efficient manner large sections of
the current Lisp functions could be replaced by such bindings to the existing
class library, as well as easing access to future additions to the class
library.

* Create a Virtualbox boot image of the system. I've been looking at bare metal
OS and talking to the author for a couple of months. He's got plans to
implement a frame buffer device for bare metal. At that point I think I would
have everything I need to do a platform isolation interface targeting bare
metal as the bootstrap and take over the UI work directly rather than going
through SDL. At this point I would start writing device drivers for the virtual
device interfaces provided by the VM environment.

* Create a standard services registration system. This is a mechanism to allow
processes to register themselves as providing a particular service to the rest
of the network. All this amounts to is a standard mechanism for attaching a
name globally throughout the system to a mailbox ID. Initially there will
probably be a message to kernel zero to register a mailbox ID. Although I may
cache these locally on retrieval in order to distribute the global names
throughout the network. I've also been planning a network wide object store
that presents itself as a standard service and uses a distributed hash table
technique to store objects throughout the network with redundancy.

* Create or port an existing TCP/IP stack. Provide this as a standard service
on the network. Implement another link driver type that uses TCP/IP in order to
experiment with bridging multiple computers together into a single addressable
network.

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
blocks could be garbage collected.

* Investigate getting LLVM/GCC to produce functions in my format. One of the
advantages of the simple function format using a local Vtable for all calls and
jumps is that there is no code patching needed. The loader does not need to
concern itself with patching a variety of native binary formats, it only
concerns itself with looking up function names and writing to Vtable entries.

* Webassembly backend in order to experiment with using browsers and web socket
links to create a distributed network across the Internet.

* Create a more comprehensive command line suite of tools. I don't think this
needs to be especially large or even go as far as Busybox. But some of the more
useful text processing and helpful commands would go a long way.

* Other targets and platform configs. RISC V and MIPS targets. Bare metal PI3
and other Aarch64 development board support along with GPIO/SPI/I2C and so
forth for the maker community. SDL over frame buffer version for PI3, maybe
just needs testing, but needs doing.
