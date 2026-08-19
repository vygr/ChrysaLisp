# ChrysaLisp Docs Application Architecture

The `apps/desktop/docs` application in ChrysaLisp is a markdown document viewer
that rejects the traditional monolithic "Rich Text" widget approach. Instead of
rendering a single massive text buffer, it leverages ChrysaLisp's graphical
composition system, dynamic module loading, and functional primitives to build
documents out of independent UI components.

This document details the internal architecture of the application, focusing on
its text layout engine, its dynamic code-block delegation system, and its
multi-tiered search capabilities.

## Text Rendering and Flow Layout

The text rendering engine (primarily located in
`apps/desktop/docs/handlers/text.inc`) treats a markdown document not as a
string to be painted, but as a hierarchical UI tree.

* **The Word-as-Widget Philosophy**

    * Every single word in a standard markdown paragraph is instantiated as its
      own independent `Text` widget.

    * Formatting is maintained via a state machine. As the parser encounters
      markup tags, it flips bits in a `state` integer (e.g., `+state_bold`,
      `+state_code`). When a word is instantiated, it checks this bitmask and
      dynamically pulls the correct font or color property from its parent
      container via ChrysaLisp's O(1) property inheritance (`:hmap :search`).

* **Prefix Isolation and Collision Avoidance**

    * Prior to processing markdown formatting, the parser extracts leading
      indentation and list/bullet prefixes (`* `, `- `, `1. `, etc.) from the
      first line of a block.

    * The prefix is stripped from the text buffer before quote and style
      transformations begin. This completely prevents bullet characters (such as
      `*`) from colliding with subsequent markdown emphasis patterns (such as
      `*italic*` or `**bold**`).

* **Single-Pass Quote Masking and Vector Buffer Reuse**

    * The block is processed through a unified `parse-text` pipeline. Code spans
      (enclosed in backticks or double quotes) are extracted into a `quoted`
      list, and replaced in-stream with structured `<q>` placeholder tags.

    * The parser allocates an interleaved index vector `idxs` (`nums`) once per
      text block. This exact same buffer is recycled in-place using O(1) `(clear
      idxs)` across the quote masking pass and all subsequent rungs of the style
      ladder, eliminating heap allocation thrashing.

* **The Multi-Sequence `reduce!` Ladder with Static Tag Templates**

    * Inline markdown styling is parsed without nested loops or dynamic string
      concatenations. Instead, it uses ChrysaLisp's multi-sequence `reduce!`
      primitive stepping through parallel sequences of regex patterns, static
      replacement tag strings, and delimiter strip counts.

    * The ladder applies strikethrough (`~~`), highlight (`==`), bold-italic
      (`***`), bold (`**`), and italic (`*`) tags sequentially using
      pre-allocated tag templates (e.g., `" <b> </b> "`, `" <i> </i> "`),
      leaving paragraph line breaks and table delimiter markers intact.

* **Atomic Scatter-Copy Formatting via `splice`**

    * As regex matches are found, byte offsets for source text segments and
      injected markup tags are pushed into the recycled `idxs` vector.

    * Once a match pass completes, a single call to ChrysaLisp's native `splice`
      primitive executes a hardware-accelerated scatter-copy, instantly
      stitching preserved text and markup tags together in a single operation.

* **Zero-Overhead Grid Table Generation and Column Alignment**

	* Table parsing is gated by a fast substring test: `(find "|" tagged_text)`. If
	  no pipe delimiter exists, the text block immediately bypasses table checks
	  and proceeds directly to paragraph rendering.

	* When pipe markers are present, lines and cells are split in a single pass.
	  Rows are categorized into headers and data rows, separator lines
	  (`|---|---|`) are detected, and column alignment rules (`:---`, `:---:`,
	  `---:`) are parsed into alignment flow flags (`+flow_down`,
	  `+flow_flag_align_hcenter`, `+flow_flag_align_hright`).

	* Tables are rendered into an enclosing `Flow` widget populated in
	  bottom-to-top order via `reach` and `:add_front`. Each row is constructed as
	  a `Grid` with columns populated right-to-left `(for num_cols 0)` via
	  `:add_front`. Immediate containment attachment preserves the `:parent` chain
	  for O(1) font property inheritance (`:font_bold`, `:font_term`).

	* Because the rows and columns are evaluated in reverse, `restore-quotes` is
	  reused directly for table cells, popping quotes from the back of the original
	  `quoted` list in place with zero allocation overhead or list reversals.

* **Unified In-Place Quote Restoration**

	* Both standard paragraphs and grid tables share the exact same in-place quote
	  restoration mechanism.

	* By executing traversal in reverse order of quote discovery, `restore-quotes`
	  pops replacement words directly from the back of the unmodified `quoted`
	  list, eliminating all intermediate quote-stack allocations.

* **Flow-Based Word Wrapping**

    * Word wrapping is completely offloaded to the emergent behavior of
      ChrysaLisp's standard `Flow` GUI components.

    * To build a paragraph, the system creates a horizontal `Flow` widget to
      represent a single line.

    * It loops through the parsed words, spawning a `Text` widget for each. It
      queries the preferred width of the widget via `(. text :pref_size)` and
      accumulates it into a running width counter.

    * When the counter exceeds the container's width bounds, the line wraps: a
      new horizontal `Flow` is created, the width counter resets, any hanging
      indents or bullet icons (`0xe979`) are inserted, and word placement
      continues.

## Dynamic Section Handlers

The document parser does not hardcode the logic for every possible markdown
block. Instead, it utilizes a powerful Delegation Pattern driven by Markdown's
fenced code blocks ("```tag").

* **Lazy Module Loading**

    * When the parser encounters a fenced block, it reads the tag (e.g.,
      "```lisp", "```image").

    * It passes this tag to the `handler-func` router. The router checks an
      environment map (`handlers`) for an existing function.

    * If no handler is found, the system dynamically generates a file path
      (`handlers/tag.inc`), reads the file from disk, and compiles/evaluates it
      live into the running application using `repl`. The new handler is cached
      and immediately used to process the block's text.

* **Built-in Handlers**

    * **`vdu` / `code`:** Renders standard code blocks. Instead of using `Text`
      widgets, it spawns a highly efficient `Vdu` (Video Display Unit) widget,
      complete with syntax highlighting driven by `lib/text/syntax.inc`.

    * **`image`:** Reads an image file path from the block and spawns a
      hardware-accelerated `Canvas` widget directly inline with the text,
      supporting `.cpm`, `.tga`, and fully vector-parsed `.svg` and `.cwb`
      files.

    * **`file`:** Opens an external text or code file and embeds its contents.
      It supports start and end regex markers, allowing documentation to
      dynamically pull in specific functions directly from the OS source code,
      ensuring docs never go out of sync with the codebase.

    * **`lispq` / `lisp`:** Evaluates the contents of the code block as live
      ChrysaLisp code within a sandbox. If the code returns a string, it prints
      the string. Astoundingly, if the code returns a GUI `View` object, *it
      embeds that live, interactive widget directly into the document*.

    * **`widget`:** Directly imports an application widget module (e.g., a
      calculator or a game) and embeds it natively within the document flow,
      proving that ChrysaLisp documents are actually interactive UI containers.

    * **`info`:** Queries runtime reflection interfaces (such as root functions,
      macros, and lambdas) and formats them dynamically into paragraphs.

## The Search System

The application features a dual-tiered search system that covers both the
currently open document and the entire file system.

* **Polymorphic Search Engines**

    * Searching utilizes two distinct classes: `Substr` (for simple text
      matching) and `Regexp` (for complex pattern matching).

    * Both classes share the exact same method signatures (`:compile`,
      `:search`, `:match?`). The `query` function looks at the user's UI toggles
      (Regex, Whole Words) and returns the correct engine instance. The rest of
      the application remains entirely unaware of which engine is doing the
      work.

    * Search patterns are compiled and memoized via the `memoize` macro, meaning
      repeating a search is an O(1) cache lookup rather than a recompilation of
      the regex state machine.

* **Local Widget Highlighting**

    * Because the document is built from individual `Text` widgets, local search
      does not require complex string index tracking to highlight text.

    * When a document is loaded, every word widget is pushed into a flat
      `*search_widgets*` array.

    * When the user searches, the app filters this array using the search
      engine. It simply changes the background color property (`:color
      *env_highlight_col*`) of the matching `Text` widgets and commands the
      parent `Scroll` view to ensure that specific widget is visible on screen.

* **Global Distributed Search**

    * The global search feature (searching across the whole `docs/` directory)
      is not processed sequentially.

    * It utilizes ChrysaLisp's `pipe-farm` actor-model system. The app spawns a
      distributed fleet of `grep` tasks, one for every CPU core available in the
      entire node cluster.

    * Each core processes a subset of the markdown files independently, using
      lock-free message passing to return matching file paths back to the main
      GUI thread, where they are displayed in the application's file tree
      sidebar.

## Supported Text Styles

The text rendering engine dynamically translates inline markdown into visual
state bitmasks, allowing multiple properties to be applied to individual word
widgets on the fly. Here is a summary of supported formatting styles:

* **Headers:** Level 1 through 6 headers (`# `, `## `, `### `, `#### `, `#####
  `, `###### `) with scaled typography and dynamic underlines.

* **Emphasis:** **Bold** (`**bold**`), *Italic* (`*italic*`), and
  ***Bold-Italic*** (`***bold-italic***`).

* **Code:** `Inline Code` (`` `inline code` `` or `\qcode\q`) with monospace
  font mapping.

* **Markup Annotations:** ==Highlighted== (`==highlighted==`) and
  ~~Strikethrough~~ (`~~strikethrough~~`) text.

* **Lists:** Numbered lists (`1. `) and bullet lists (`* `, `- `) with automatic
  hanging indents and custom bullet glyphs (`0xe979`).

* **Tables:** Markdown grid tables with column alignment syntax (`:---`,
  `:---:`, `---:`), multi-word cell wrapping, header underlines, and embedded
  inline code/styles.

* **Dividers:** Horizontal thematic break rules (`---`).
