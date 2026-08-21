# ChrysaLisp Docs Application Architecture

The `apps/desktop/docs` application in ChrysaLisp is a modular markdown document
viewer that rejects the traditional monolithic "Rich Text" buffer approach.
Instead of treating text as a monolithic string canvas, it leverages
ChrysaLisp's graphical composition tree, dynamic module loading, and functional
primitives to build documents out of independent, interactive UI components.

Historically, the Docs application implemented its own bespoke markdown parser
and layout logic directly within its text handlers. In the modern architecture,
this functionality is fully encapsulated in the reusable `Md` GUI widget
(`gui/md/lisp.inc`). The Docs application delegates all text section rendering
to `Md` instances, dynamically composing them alongside code editors, live
canvases, and interactive application widgets.

This document details the internal architecture of the application, focusing on
the `Md` widget layout engine, the dynamic section handler delegation system,
and the multi-tiered distributed search architecture.

## The `Md` Component and Text Rendering Engine

All standard markdown text processing, typography scaling, table formatting, and
word-wrapping layout are implemented in the `Md` class (`gui/md/lisp.inc`),
which inherits from `Flow`.

* **Encapsulated Markdown Widget (`Md`)**

    * The `Md` component takes a sequence of markdown text lines and renders
      them into an internal hierarchy of container flows and individual words.

    * When rendered via `(. md :render)`, the widget builds an internal list of
      all instantiated word widgets (`:search_widgets`), exposing them via the
      getter method `(. md :get_search_widgets)` for external search indexing.

* **The Word-as-Widget Philosophy**

    * Every single word in a paragraph or table cell is instantiated as an
      independent `Text` widget.

    * Formatting is maintained via a compact bitmask state (`+state_bold`,
      `+state_italic`, `+state_code`, `+state_highlight`, `+state_strike`).
      Plain words inherit fonts and colors dynamically from the parent `Flow`
      container via ChrysaLisp's O(1) containment hierarchy (`:parent`
      traversal), while styled words explicitly link to specialized fonts.

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

    * Table parsing is gated by a fast substring test: `(find "|" tagged_text)`.
      If no pipe delimiter exists, the text block immediately bypasses table
      checks and proceeds directly to paragraph rendering.

    * When pipe markers are present, lines and cells are split in a single pass.
      Rows are categorized into headers and data rows, separator lines
      (`|---|---|`) are detected, and column alignment rules (`:---`, `:---:`,
      `---:`) are parsed into alignment flow flags (`+flow_down`,
      `+flow_flag_align_hcenter`, `+flow_flag_align_hright`).

    * Tables are rendered in natural forward reading order. Each row is
      instantiated as a `Grid` widget containing vertical `Flow` column cells
      attached directly to the container, establishing the `:parent` hierarchy
      for O(1) font property inheritance.

    * Quoted code tokens within table cells are restored sequentially from a
      temporary `q_stack` (populated via `(reverse quoted)`), naturally
      preserving word order and ensuring search widgets are registered in
      forward document reading order.

* **In-Place Quote Restoration for Flow Paragraphs**

    * For standard paragraphs and headers, `restore-quotes` walks the word token
      stream in reverse using `reach`, popping replacement words directly from
      the back of the `quoted` list in place.

    * This eliminates the need to allocate and reverse separate quote stacks for
      non-table text blocks.

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

The document parser in `apps/desktop/docs/app.lisp` does not hardcode the logic
for every possible markdown block. Instead, it utilizes a Delegation Pattern
driven by Markdown's fenced code blocks ("```tag").

* **Lazy Module Loading**

    * When the parser encounters a fenced block, it reads the tag (e.g.,
      "```vdu", "```image").

    * It passes this tag to the `handler-func` router. The router checks an
      environment map (`handlers`) for an existing function.

    * If no handler is found, the system dynamically generates a file path
      (`handlers/tag.inc`), reads the file from disk, and compiles/evaluates it
      live into the running application using `repl`. The new handler is cached
      and immediately used to process the block's text.

* **Text Handler and `Md` Integration (`handlers/text.inc`)**

    * Prose text outside of fenced blocks is gathered line by line in
      `handlers/text.inc`.

    * When a transition occurs (such as encountering a fenced block or reaching
      the end of the document), `flush-md` instantiates an `(Md (cat lines))`
      widget, configures its zoom, page width, and font properties, renders it,
      and attaches it as a child to the main `page` flow.

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

* **Aggregate Search Widget Collection**

    * Because text sections are rendered across multiple `Md` instances
      separated by code blocks or images, the Docs application gathers all
      searchable tokens across the whole document upon load.

    * In `populate-page`, the application filters the page's children for `Md?`
      instances and concatenates their individual `:search_widgets` lists into a
      single, unified `*search_widgets*` list using `reduce`:

      ```vdu
      (setq *search_widgets*
          (reduce (# (cat %0 (. %1 :get_search_widgets)))
              (filter (const Md?) (. page :children)) (list)))
      ```

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

* **Identity-Based Widget Highlighting and Navigation**

    * Because all `Text` widgets representing the same plain text word have
      identical `hmap` contents, standard `find` (which calls `:obj :eql`)
      matches the content of the first occurrence in the list.

    * Navigation in `search.inc` therefore locates the active widget using its
      unique integer view ID via `(. %0 :get_id)`:

      ```vdu
      (defq idx (some (# (if (= (. %0 :get_id) (. *last_widget* :get_id)) (!))) found))
      ```

    * This guarantees accurate forward and backward cycling across duplicate
      words throughout all `Md` sections in the document.

    * The active match is highlighted by setting `:color *env_highlight_col*`
      and scrolled into view via `(. *page_scroll* :visible *last_widget*)`.

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

The `Md` widget dynamically translates inline markdown into visual state
bitmasks, allowing multiple properties to be applied to individual word widgets
on the fly:

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