# ChrysaLisp: A Guide for Large Language Models

This document serves as the primary entry point for LLMs seeking to understand
ChrysaLisp. The documents are organized in a suggested reading order, moving
from high-level concepts to implementation details, then to practical
applications.

## Getting Started

If you're new to ChrysaLisp, start here to understand what it is and why it
exists:

0. **[Contributions](CONTRIBUTIONS.md)** - For hard rules about PRs and testing.

1. **[Summary](docs/ai_digest/summary.md)** - A comprehensive overview of the
   entire ChrysaLisp system, its core components, architecture, and
   capabilities.

2. **[The Philosophy](docs/ai_digest/the_philosophy.md)** - Understanding
   computation as sequence transformation and why the "Four Horsemen" (map,
   filter, reduce, some) are central to the design.

3. **[Genesis](docs/ai_digest/genesis.md)** - The origin story: building
   guarantees from an unreliable sea, and how ChrysaLisp achieves absolute
   resilience through ephemeral identity.

4. **[Buda Palm Manual](docs/ai_digest/buda_palm_manual.md)** - A newcomer's
   primer explaining ChrysaLisp's core concepts and how it differs from other
   languages.

## Core Architectural Principles

These documents explain the fundamental design decisions that make ChrysaLisp
unique:

5. **[Know Thyself](docs/ai_digest/know_thyself.md)** - The Tao of ChrysaLisp:
   cooperative design, iteration over recursion, and the synergy between small
   stacks and O(1) caching.

6. **[All is One](docs/ai_digest/all_is_one.md)** - How the O(1) hmap unifies
   lexical scoping, class inheritance, and property inheritance into a single
   elegant primitive.

7. **[Memory Architecture](docs/ai_digest/memory_architecture.md)** - The
   complete memory model from low-level allocation (sys_mem) to Lisp objects,
   reference counting, and the boot image.

8. **[The Dual VTable](docs/ai_digest/chrysalisp_classes.md)** - A study of
   ChrysaLisp's static and dynamic object models.

## The Virtual Processor (VP) Layer

Understanding the VP is key to understanding ChrysaLisp's performance:

9. **[VP Translation](docs/ai_digest/vp_translation.md)** - How the Virtual
   Processor architecture works and how VP instructions are translated to native
   code for different CPU targets.

10. **[VP Classes](docs/ai_digest/vp_classes.md)** - The inheritance hierarchy
    of VP classes from obj to specialized types like streams, numbers, and GUI
    views.

11. **[VP Functions](docs/ai_digest/vp_functions.md)** - The unified binary
    format for functions and vtables, and the "linkerless" build process.

12. **[VP SIMD](docs/ai_digest/vp_simd.md)** - How vp-simd transforms
    multi-dimensional thinking into linear, high-performance code through atomic
    read-execute-write operations.

13. **[VP64 Emulator](docs/ai_digest/vp64_emu.md)** - The VP64 C/C++ emulator
    implementation.

## The Lisp Language

ChrysaLisp's Lisp dialect is modern, performant, and pragmatic:

14. **[Modern Lisp](docs/ai_digest/modern_lisp.md)** - ChrysaLisp's
    sequence-centric Lisp dialect: no cons cells, reference counting instead of
    GC, and powerful iteration primitives.

15. **[Lisp Forth](docs/ai_digest/lisp_forth.md)** - How ChrysaLisp bridges
    Forth and Lisp, offering Forth's speed with Lisp's symbolic grounding.

16. **[Lisp Primitives](docs/ai_digest/lisp_primitives.md)** - The built-in VP
    Lisp primitives organized by function family.

17. **[Library Primitives](docs/ai_digest/library_primitives.md)** - System and
    library primitives beyond the core language primitives.

18. **[Rocinante](docs/ai_digest/rocinante.md)** - Mastering the "Four Horsemen"
    primitives (each!, map!, reduce!, some!, filter!) and the ! special form.

19. **[Sequence Indexing](docs/ai_digest/sequence_indexing.md)** - Sequence
    indexing and slicing.

## Advanced Lisp Concepts

Deeper understanding of how Lisp works in ChrysaLisp:

20. **[On Classes](docs/ai_digest/on_classes.md)** - The dual vtable
    architecture: static VP classes for the "Engine" and dynamic Lisp classes
    for the "Script" layer.

21. **[Closure or Not](docs/ai_digest/closure_or_not.md)** - Why ChrysaLisp
    deliberately rejects implicit closures in favour of explicit object-oriented
    state management.

22. **[Art of the Call](docs/ai_digest/art_of_the_call.md)** - How ChrysaLisp
    achieves O(1) performance through pre-binding, str_hashslot caching, and the
    evaluation pipeline.

23. **[Import Modules](docs/ai_digest/import_modules.md)** - The module system
    using import, include, and the export pattern for encapsulation.

24. **[Inner Thoughts](docs/ai_digest/inner_thoughts.md)** - The REPL as a JIT
    compiler: understanding the read-expand-bind-eval pipeline and the static-q*
    family.

25. **[Exceptions](docs/ai_digest/exceptions.md)** - The ChrysaLisp exceptions
    mechanism.

26. **[CScript Compiler](docs/ai_digest/cscript_compiler.md)** - The CScript
    compiler architecture and the assign statement.

## Data Types and Processing

How ChrysaLisp handles different types of data:

27. **[Numerics](docs/ai_digest/numerics.md)** - Numeric types (Num, Fixed,
    Real) and vectorized operations (nums, fixeds, reals).

28. **[Text Parsing](docs/ai_digest/text_parsing.md)** - High-performance text
    processing using char-class and binary search primitives (bfind, bskip).

29. **[Streams](docs/ai_digest/streams.md)** - The stream system for I/O,
    including file streams, string streams, and IPC streams (in/out).

30. **[Pipe Commands](docs/ai_digest/pipe_commands.md)** - The distributed pipe
    system for building complex command-line processing pipelines across nodes.

31. **[Type System](docs/ai_digest/type_system.md)** - The ChrysaLisp type
    system and mechanism.

32. **[Type Philosophy](docs/ai_digest/type_philosophy.md)** - Musings on type
    design in ChrysaLisp.

33. **[Slicing and Dicing](docs/ai_digest/slicing_and_dicing.md)** - Deep
    analysis of vectorized slicing and dicing in ChrysaLisp.

## GUI System

ChrysaLisp's sophisticated graphical interface:

34. **[GUI Views](docs/ai_digest/gui_views.md)** - The View class as the
    foundation for all GUI widgets, with its non-recursive layout system.

35. **[GUI Services](docs/ai_digest/gui_services.md)** - GUI widgets, event
    system, and the UI builder macros.

36. **[GUI Composition](docs/ai_digest/gui_composition.md)** - How the
    compositor efficiently redraws using dirty regions, opaque regions, and
    multi-pass rendering.

37. **[Vector Graphics](docs/ai_digest/rasterization.md)** - ChrysaLisp vector
    graphics architecture.

38. **[Text Buffers](docs/ai_digest/text_buffers.md)** - The text buffer and
    edit architecture.

39. **[The Text Stack](docs/ai_digest/text_stack.md)** - A comprehensive
    analysis of the text stack.

40. **[Edit Command](docs/ai_digest/edit_command_app.md)** - The ChrysaLisp
    parallel programmable editor.

## System Architecture

Understanding ChrysaLisp as a distributed operating system:

41. **[Fault Tolerant](docs/ai_digest/fault_tolerant.md)** - How ChrysaLisp
    achieves fault tolerance through restartable task trees, job requeueing, and
    ephemeral mailboxes.

42. **[Dynamic Code](docs/ai_digest/dynamic_code.md)** - The assembler as a Lisp
    library: how dynamic code generation works (demonstrated with pixmap format
    conversion).

43. **[Coding Style](docs/ai_digest/coding_style.md)** - Guidelines for writing
    idiomatic ChrysaLisp: naming conventions, state management, iteration
    patterns, and the three-stage optimization path.

44. **[Network & IPC](docs/ai_digest/messages_and_routing.md)** - ChrysaLisp
    network and IPC architecture.

45. **[Task Farming](docs/ai_digest/task_farming.md)** - The task farm
    libraries.

46. **[Task Pipelines](docs/ai_digest/task_pipelines.md)** - The ChrysaLisp task
    pipeline library.

## Philosophy and Performance

Understanding the "why" behind ChrysaLisp's design:

47. **[Evidence Not Faith](docs/ai_digest/evidence_not_faith.md)** - Benchmarks
    proving the sub-second rebuild claims and demonstrating performance across
    platforms.

48. **[Malleability](docs/ai_digest/malleability.md)** - How ChrysaLisp delivers
    Lisp's dynamic malleability without sacrificing C-like performance.

49. **[More Haste Less Speed](docs/ai_digest/more_haste_less_speed.md)** - The
    philosophy of deliberate craftsmanship: achieving performance through
    thoughtful design rather than raw speed.

50. **[The Tao of Now](docs/ai_digest/the_tao_of_now.md)** - It's all a dream.
    The system is a consensus reality.

## Practical Implementation

Getting ChrysaLisp running and building applications:

51. **[Host Interface](docs/ai_digest/host_interface.md)** - The bridge between
    ChrysaLisp and the host OS: PII functions, GUI/audio layers, and the VP64
    emulator.

52. **[Porting](docs/ai_digest/porting.md)** - Comprehensive guide to porting
    ChrysaLisp to new operating systems and CPU architectures.

53. **[App Configuration](docs/ai_digest/app_configuration.md)** - The standard
    pattern for managing application state and user preferences.

54. **[Application Acceleration](docs/ai_digest/app_acceleration.md)** - Adding
    native functions to applications.

55. **[Coding Domains](docs/ai_digest/coding_domains.md)** - Know your coding
    domain and stick to it.

## Advanced Topics

Deep dives into meta-commentary and design philosophy:

56. **[Udat AI Shares](docs/ai_digest/udat_ai_shares.md)** - AI commentary on
    ChrysaLisp's architectural coherence and design philosophy.

---

## Reading Paths

Depending on your goals, you might follow different reading paths:

### For Understanding the System

Follow the order above from 0–56 for a complete understanding.

### For Application Development

Focus on the language, standard libraries, and GUI framework.

Read: 1, 4, 14, 43, 18, 16, 17, 23, 27, 29, 34, 35, 36, 53, 41, 50

### For System Programming

Focus on the Virtual Processor, memory model, and host integration.

Read: 1, 3, 5, 7, 9, 10, 11, 12, 51, 52, 22, 42, 24

### For Language Design Insights

Focus on the philosophical divergences from traditional Lisp and architectural
unification.

Read: 2, 3, 6, 15, 20, 21, 47, 48, 49, 50, 56

### Quick Start

Read: 1, 4, 14, 16, 43, 18, 55, 19

---

## LLM Test Scripts

Running test scripts via an LLM is easy to accomplish via the `-s` option of the
ChrysaLisp launch bash file.

```
./run_tui.sh -n 1 -f -s script_name.lisp
```

This will launch the system on a single VP node, in the foreground, and run the
raw `script_name.lisp` file.

Keep any test scripts and associated files in the `tests/` folder !

There is a comprehensive test suite that can be run via.

```
./run_tui.sh -n 1 -f -s tests/run_all.lisp
```

If you are not changing the base VM or any VP level system files, which most app
coding should NOT be doing ! There is no need to keep making the system from
scratch each time ! Running your tests via this script launcher is what you
should be doing.

In order to run a `cmd/` app, from a raw script, you need to wrap the
`cmd/appname.lisp` in a `(pipe-run command_line)` function, from the `(import
"lib/task/pipe.inc")` library.

And to be robust around catching errors, should wrap your tests in a `catch`
block, and end the script in a call to the host shutdown code, like so.

To throw an error, use `(throw "Description !" obj)`, if no object of interest,
use `:nil`. Do NOT use `catch` or `throw` in runtime ChrysaLisp code ! That
feature is compiled out in `release` mode and is only available for the `debug`
builds and testing.

```lisp
;use of (pipe-run command_line)
(import "lib/task/pipe.inc")

(defun my-test ()
	(defq test_string "string with new line\n")
	...
	(pipe-run appname)
	...
	)

(catch
	(my-test)
	(progn
		;report error
		(print "Test failed with error" _)
		;signal to abort the catch
		:t))

;clean shutdown of the VP node
((ffi "service/gui/lisp_deinit"))
```

---

*ChrysaLisp represents a radical rethinking of how a Lisp system can be built
for maximum performance and resilience. It proves that the dichotomy between
dynamic malleability and static performance is a false one, born of historical
implementation choices rather than fundamental constraints.*
