# ChrysaLisp: A Guide for Large Language Models

This document serves as the primary entry point for LLMs seeking to understand ChrysaLisp. The documents are organized in a suggested reading order, moving from high-level concepts to implementation details, then to practical applications.

## Getting Started

If you're new to ChrysaLisp, start here to understand what it is and why it exists:

1. **[Summary](docs/ai_digest/summary.md)** - A comprehensive overview of the entire ChrysaLisp system, its core components, architecture, and capabilities.

2. **[The Philosophy](docs/ai_digest/the_philosophy.md)** - Understanding computation as sequence transformation and why the "Four Horsemen" (map, filter, reduce, some) are central to the design.

3. **[Genesis](docs/ai_digest/genesis.md)** - The origin story: building guarantees from an unreliable sea, and how ChrysaLisp achieves absolute resilience through ephemeral identity.

4. **[Buda Palm Manual](docs/ai_digest/buda_palm_manual.md)** - A newcomer's primer explaining ChrysaLisp's core concepts and how it differs from other languages.

## Core Architectural Principles

These documents explain the fundamental design decisions that make ChrysaLisp unique:

5. **[Know Thyself](docs/ai_digest/know_thyself.md)** - The Tao of ChrysaLisp: cooperative design, iteration over recursion, and the synergy between small stacks and O(1) caching.

6. **[All is One](docs/ai_digest/all_is_one.md)** - How the O(1) hmap unifies lexical scoping, class inheritance, and property inheritance into a single elegant primitive.

7. **[Memory Architecture](docs/ai_digest/memory_architecture.md)** - The complete memory model from low-level allocation (sys_mem) to Lisp objects, reference counting, and the boot image.

## The Virtual Processor (VP) Layer

Understanding the VP is key to understanding ChrysaLisp's performance:

8. **[VP Translation](docs/ai_digest/vp_translation.md)** - How the Virtual Processor architecture works and how VP instructions are translated to native code for different CPU targets.

9. **[VP Classes](docs/ai_digest/vp_classes.md)** - The inheritance hierarchy of VP classes from obj to specialized types like streams, numbers, and GUI views.

10. **[VP Functions](docs/ai_digest/vp_functions.md)** - The unified binary format for functions and vtables, and the "linkerless" build process.

11. **[VP SIMD](docs/ai_digest/vp_simd.md)** - How vp-simd transforms multi-dimensional thinking into linear, high-performance code through atomic read-execute-write operations.

## The Lisp Language

ChrysaLisp's Lisp dialect is modern, performant, and pragmatic:

12. **[Modern Lisp](docs/ai_digest/modern_lisp.md)** - ChrysaLisp's sequence-centric Lisp dialect: no cons cells, reference counting instead of GC, and powerful iteration primitives.

13. **[Lisp Forth](docs/ai_digest/lisp_forth.md)** - How ChrysaLisp bridges Forth and Lisp, offering Forth's speed with Lisp's symbolic grounding.

14. **[Lisp Primitives](docs/ai_digest/lisp_primitives.md)** - The built-in VP Lisp primitives organized by function family.

15. **[Library Primitives](docs/ai_digest/library_primitives.md)** - System and library primitives beyond the core language primitives.

## Advanced Lisp Concepts

Deeper understanding of how Lisp works in ChrysaLisp:

16. **[On Classes](docs/ai_digest/on_classes.md)** - The dual vtable architecture: static VP classes for the "Engine" and dynamic Lisp classes for the "Script" layer.

17. **[Closure or Not](docs/ai_digest/closure_or_not.md)** - Why ChrysaLisp deliberately rejects implicit closures in favor of explicit object-oriented state management.

18. **[Art of the Call](docs/ai_digest/art_of_the_call.md)** - How ChrysaLisp achieves O(1) performance through pre-binding, str_hashslot caching, and the evaluation pipeline.

19. **[Import Modules](docs/ai_digest/import_modules.md)** - The module system using import, include, and the export pattern for encapsulation.

## Data Types and Processing

How ChrysaLisp handles different types of data:

20. **[Numerics](docs/ai_digest/numerics.md)** - Numeric types (Num, Fixed, Real) and vectorized operations (nums, fixeds, reals).

21. **[Text Parsing](docs/ai_digest/text_parsing.md)** - High-performance text processing using char-class and binary search primitives (bfind, bskip).

22. **[Streams](docs/ai_digest/streams.md)** - The stream system for I/O, including file streams, string streams, and IPC streams (in/out).

23. **[Pipe Commands](docs/ai_digest/pipe_commands.md)** - The distributed pipe system for building complex command-line processing pipelines across nodes.

## GUI System

ChrysaLisp's sophisticated graphical interface:

24. **[GUI Views](docs/ai_digest/gui_views.md)** - The View class as the foundation for all GUI widgets, with its non-recursive layout system.

25. **[GUI Services](docs/ai_digest/gui_services.md)** - GUI widgets, event system, and the UI builder macros.

26. **[GUI Composition](docs/ai_digest/gui_composition.md)** - How the compositor efficiently redraws using dirty regions, opaque regions, and multi-pass rendering.

## System Architecture

Understanding ChrysaLisp as a distributed operating system:

27. **[Fault Tolerant](docs/ai_digest/fault_tolerant.md)** - How ChrysaLisp achieves fault tolerance through restartable task trees, job requeueing, and ephemeral mailboxes.

28. **[Dynamic Code](docs/ai_digest/dynamic_code.md)** - The assembler as a Lisp library: how dynamic code generation works (demonstrated with pixmap format conversion).

29. **[Coding Style](docs/ai_digest/coding_style.md)** - Guidelines for writing idiomatic ChrysaLisp: naming conventions, state management, iteration patterns, and the three-stage optimization path.

## Philosophy and Performance

Understanding the "why" behind ChrysaLisp's design:

30. **[Evidence Not Faith](docs/ai_digest/evidence_not_faith.md)** - Benchmarks proving the sub-second rebuild claims and demonstrating performance across platforms.

31. **[Malleability](docs/ai_digest/malleability.md)** - How ChrysaLisp delivers Lisp's dynamic malleability without sacrificing C-like performance.

32. **[More Haste Less Speed](docs/ai_digest/more_haste_less_speed.md)** - The philosophy of deliberate craftsmanship: achieving performance through thoughtful design rather than raw speed.

## Practical Implementation

Getting ChrysaLisp running and building applications:

33. **[Host Interface](docs/ai_digest/host_interface.md)** - The bridge between ChrysaLisp and the host OS: PII functions, GUI/audio layers, and the VP64 emulator.

34. **[Porting](docs/ai_digest/porting.md)** - Comprehensive guide to porting ChrysaLisp to new operating systems and CPU architectures.

35. **[App Configuration](docs/ai_digest/app_configuration.md)** - The standard pattern for managing application state and user preferences.

## Advanced Topics

Deep dives into specialized areas:

36. **[Inner Thoughts](docs/ai_digest/inner_thoughts.md)** - The REPL as a JIT compiler: understanding the read-expand-bind-eval pipeline and the static-q* family.

37. **[Rocinante](docs/ai_digest/rocinante.md)** - Mastering the "Four Horsemen" primitives (each!, map!, reduce!, some!, filter!) and the ! special form.

38. **[Udat AI Shares](docs/ai_digest/udat_ai_shares.md)** - AI commentary on ChrysaLisp's architectural coherence and design philosophy.

---

## Reading Paths

Depending on your goals, you might follow different reading paths:

### For Understanding the System
Follow the order above from 1-38 for a complete understanding.

### For Application Development
Read: 1-7, 12-15, 20-26, 29, 35

### For System Programming
Read: 1-11, 16-19, 27-29, 33-34

### For Language Design Insights
Read: 1-7, 12-19, 30-32, 36-38

### Quick Start
Read: 1, 4, 12, 14, 29, 37

---

*ChrysaLisp represents a radical rethinking of how a Lisp system can be built for maximum performance and resilience. It proves that the dichotomy between dynamic malleability and static performance is a false one, born of historical implementation choices rather than fundamental constraints.*
