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

19. **[Flow Through](docs/ai_digest/flow_through.md)** - Understanding
    conditional flow and the "flow-through of the current value" architecture.

20. **[Sequence Indexing](docs/ai_digest/sequence_indexing.md)** - Sequence
    indexing and slicing.

## Advanced Lisp Concepts

Deeper understanding of how Lisp works in ChrysaLisp:

21. **[On Classes](docs/ai_digest/on_classes.md)** - The dual vtable
    architecture: static VP classes for the "Engine" and dynamic Lisp classes
    for the "Script" layer.

22. **[Closure or Not](docs/ai_digest/closure_or_not.md)** - Why ChrysaLisp
    deliberately rejects implicit closures in favour of explicit object-oriented
    state management.

23. **[Art of the Call](docs/ai_digest/art_of_the_call.md)** - How ChrysaLisp
    achieves O(1) performance through pre-binding, str_hashslot caching, and the
    evaluation pipeline.

24. **[Import Modules](docs/ai_digest/import_modules.md)** - The module system
    using import, include, and the export pattern for encapsulation.

25. **[Inner Thoughts](docs/ai_digest/inner_thoughts.md)** - The REPL as a JIT
    compiler: understanding the read-expand-bind-eval pipeline and the static-q*
    family.

26. **[Exceptions](docs/ai_digest/exceptions.md)** - The ChrysaLisp exceptions
    mechanism.

27. **[CScript Compiler](docs/ai_digest/cscript_compiler.md)** - The CScript
    compiler architecture and the assign statement.

28. **[CScript Skills](docs/ai_digest/cscript_skills.md)** - Mixing CScript and
    VP assembly: register mapping, stack management, and load/drain patterns.

## Data Types and Processing

How ChrysaLisp handles different types of data:

29. **[Numerics](docs/ai_digest/numerics.md)** - Numeric types (Num, Fixed,
    Real) and vectorized operations (nums, fixeds, reals).

30. **[Text Parsing](docs/ai_digest/text_parsing.md)** - High-performance text
    processing using char-class and binary search primitives (bfind, bskip).

31. **[Regexp System](docs/ai_digest/regexp_system.md)** - The NFA-based Regular
    Expression engine, greedy and lazy quantifiers, and optimized replacement via
    splice.

32. **[Streams](docs/ai_digest/streams.md)** - The stream system for I/O,
    including file streams, string streams, and IPC streams (in/out).

33. **[Pipe Commands](docs/ai_digest/pipe_commands.md)** - The distributed pipe
    system for building complex command-line processing pipelines across nodes.

34. **[Type System](docs/ai_digest/type_system.md)** - The ChrysaLisp type
    system and mechanism.

35. **[Type Philosophy](docs/ai_digest/type_philosophy.md)** - Musings on type
    design in ChrysaLisp.

36. **[Slicing and Dicing](docs/ai_digest/slicing_and_dicing.md)** - Deep
    analysis of vectorized slicing and dicing in ChrysaLisp.

## GUI System

ChrysaLisp's sophisticated graphical interface:

37. **[GUI Views](docs/ai_digest/gui_views.md)** - The View class as the
    foundation for all GUI widgets, with its non-recursive layout system.

38. **[GUI Services](docs/ai_digest/gui_services.md)** - GUI widgets, event
    system, and the UI builder macros.

39. **[GUI Composition](docs/ai_digest/gui_composition.md)** - How the
    compositor efficiently redraws using dirty regions, opaque regions, and
    multi-pass rendering.

40. **[Vector Graphics](docs/ai_digest/rasterization.md)** - ChrysaLisp vector
    graphics architecture.

41. **[Text Buffers](docs/ai_digest/text_buffers.md)** - The text buffer and
    edit architecture.

42. **[The Text Stack](docs/ai_digest/text_stack.md)** - A comprehensive
    analysis of the text stack.

43. **[Edit Command](docs/ai_digest/edit_command_app.md)** - The ChrysaLisp
    parallel programmable editor.

44. **[Docs Architecture](docs/ai_digest/docs_rendering.md)** - Internal
    architecture of the Docs application: word-as-widget rendering, dynamic
    section handlers, and distributed search.

## System Architecture

Understanding ChrysaLisp as a distributed operating system:

45. **[Fault Tolerant](docs/ai_digest/fault_tolerant.md)** - How ChrysaLisp
    achieves fault tolerance through restartable task trees, job requeueing, and
    ephemeral mailboxes.

46. **[Dynamic Code](docs/ai_digest/dynamic_code.md)** - The assembler as a Lisp
    library: how dynamic code generation works (demonstrated with pixmap format
    conversion).

47. **[Coding Style](docs/ai_digest/coding_style.md)** - Guidelines for writing
    idiomatic ChrysaLisp: naming conventions, state management, iteration
    patterns, and the three-stage optimization path.

48. **[Network & IPC](docs/ai_digest/messages_and_routing.md)** - ChrysaLisp
    network and IPC architecture.

49. **[Task Farming](docs/ai_digest/task_farming.md)** - The task farm
    libraries.

50. **[Task Pipelines](docs/ai_digest/task_pipelines.md)** - The ChrysaLisp task
    pipeline library.

## Philosophy and Performance

Understanding the "why" behind ChrysaLisp's design:

51. **[Evidence Not Faith](docs/ai_digest/evidence_not_faith.md)** - Benchmarks
    proving the sub-second rebuild claims and demonstrating performance across
    platforms.

52. **[Malleability](docs/ai_digest/malleability.md)** - How ChrysaLisp delivers
    Lisp's dynamic malleability without sacrificing C-like performance.

53. **[Gemini Masterclass](docs/ai_digest/gemini_masterclass.md)** - Advanced
    data-driven dispatch using `some`, `#` anaphoric lambdas, and `const`
    compile-time resolution.

54. **[More Haste Less Speed](docs/ai_digest/more_haste_less_speed.md)** - The
    philosophy of deliberate craftsmanship: achieving performance through
    thoughtful design rather than raw speed.

55. **[The Tao of Now](docs/ai_digest/the_tao_of_now.md)** - It's all a dream.
    The system is a consensus reality.

## Practical Implementation

Getting ChrysaLisp running and building applications:

56. **[Host Interface](docs/ai_digest/host_interface.md)** - The bridge between
    ChrysaLisp and the host OS: PII functions, GUI/audio layers, and the VP64
    emulator.

57. **[Porting](docs/ai_digest/porting.md)** - Comprehensive guide to porting
    ChrysaLisp to new operating systems and CPU architectures.

58. **[App Configuration](docs/ai_digest/app_configuration.md)** - The standard
    pattern for managing application state and user preferences.

59. **[Application Acceleration](docs/ai_digest/app_acceleration.md)** - Adding
    native functions to applications.

60. **[Coding Domains](docs/ai_digest/coding_domains.md)** - Know your coding
    domain and stick to it.

## Advanced Topics

Deep dives into meta-commentary and design philosophy:

61. **[Udat AI Shares](docs/ai_digest/udat_ai_shares.md)** - AI commentary on
    ChrysaLisp's architectural coherence and design philosophy.

62. **[Turbo Charging](docs/ai_digest/turbo_charging.md)** - Translator Turbo
    charging of environment maps.

63. **[Source Database](docs/ai_digest/source_database.md)** - VP source database.

64. **[Trace Analysis](docs/ai_digest/trace_command.md)** - VP register dataflow
    tracing.

65. **[Plist and Case](docs/ai_digest/case_for_plist.md)** - VP `:plist` class
    and it's uses.

---

## Reading Paths

Depending on your goals, you might follow different reading paths:

### For Understanding the System

Follow the order above from 0–61 for a complete understanding.

### For Application Development

Focus on the language, standard libraries, and GUI framework.

Read: 1, 4, 14, 47, 18, 19, 16, 17, 24, 29, 31, 32, 37, 38, 39, 44, 58, 45, 55, 53

### For System Programming

Focus on the Virtual Processor, memory model, and host integration.

Read: 1, 3, 5, 7, 9, 10, 11, 12, 56, 57, 23, 46, 25, 28

### For Language Design Insights

Focus on the philosophical divergences from traditional Lisp and architectural
unification.

Read: 2, 3, 6, 15, 19, 21, 22, 51, 52, 54, 55, 61

### Quick Start

Read: 1, 4, 14, 16, 47, 18, 60, 20

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
