# Text Handler Master Test Suite

This document tests all typographic and layout features parsed and formatted
directly by the `text` handler.

---

# Heading Level 1 (34pt)

## Heading Level 2 (30pt)

### Heading Level 3 (26pt)

#### Heading Level 4 (22pt)

##### Heading Level 5 (18pt)

###### Heading Level 6 (14pt)

Standard body paragraph text (18pt) demonstrating proportional font rendering,
natural line breaks, and automatic word wrapping across the view width.

Test that embedded pipe symbols in quotes work correctly "this is an embedded |
symbol" section of quoted text.

---

### Inline Styles and Character Formatting

This section tests every supported inline character formatting rule:

* Standard body text followed by **bold text**, *italic text*, and
  ***bold-italic text***.

* Technical highlights using ==highlighted background== and ~~strikethrough
  text~~.

* Inline code blocks such as `(defq a 10 b 20)` and `(mail-send mbox msg)`.

* Alternate curly-brace literal syntax `{literal text string}` parsed as a code
  span.

* Quoted literals like "double-quoted string text" within normal prose.

* Punctuation immediately adjacent to formatted spans: `(first list)`,
  `*root_env*`, **bolded!**, and *italicized?*.

* Multiple formatted spans within a single line: **Alpha**, *Beta*, `Gamma`,
  ==Delta==, and ~~Epsilon~~.

---

### Bulleted and Numbered Lists

* Asterisk-prefixed bullet item with standard icon formatting.

* Secondary bullet item containing **bold text** and `(code-tokens)`.

* Multi-line bullet item containing enough text to verify that wrapped lines
  automatically indent cleanly underneath the first line of the bullet.

- Dash-prefixed list item verifying alternate bullet syntax.

- Second dash-prefixed item with *italic emphasis* and ==highlight==.

1. First sequential numbered step in a procedural list.

2. Second sequential numbered step with `inline-code` expressions.

3. Third sequential numbered step demonstrating multi-item alignment.

10. Multi-digit numbered item verifying correct prefix alignment.

1.1. Nested outline numbering test item.

1.2. Secondary nested outline numbering item.

1.2.1. Deeply nested triple-level outline numbering item.

---

### Standard Table with Inline Styles

| Component | Status | Description |
| --- | --- | --- |
| `Grid` | **Active** | Supports `:grid_width 3` and `:grid_height 0` |
| `Flow` | *Tested* | Wraps words into `+flow_right` line flows |
| `Text` | ***Verified*** | Fully supports ==highlight== and ~~strikethrough~~ styles |

---

### Multi-Word Wrapping Inside Grid Cells

| Column 1 (Left) | Column 2 (Center) | Column 3 (Right) |
| --- | --- | --- |
| This cell contains several words that should automatically wrap across multiple lines when exceeding the allotted cell width. | Short entry. | Here is another longer cell designed to test boundary calculations and line flow generation. |
| Second row, first column text. | `sys/task/class.vp` | Second row, third column text with **bold** emphasis. |

---

### Ragged Rows and Column Alignment

| Primary Key | Register | Data Type | Notes |
| --- | --- | --- | --- |
| Entry 1 | `:r0` | `ptr` | Base pointer reference |
| Entry 2 | `:r1` | `long` | |
| Entry 3 | `:r2` | | |

---

### High Column Count Table (5 Columns)

| Reg | Type | Width | Offset | Usage |
| --- | --- | --- | --- | --- |
| `:r0` | `ptr` | 8 bytes | `+0` | `this` object context |
| `:r1` | `ptr` | 8 bytes | `+8` | `args` argument list |
| `:r2` | `long` | 8 bytes | `+16` | General integer / scratch |
| `:r3` | `long` | 8 bytes | `+24` | Counter / accumulator |
| `:r4` | `uint` | 4 bytes | `+32` | Flags bitmask |

---

### Multiple Code Tokens in Table Cells

| Primitive | Arguments | Returns | Example |
| --- | --- | --- | --- |
| `pinsert` | `props` `key` `val` | `props` | `(pinsert p :font font)` |
| `pfind` | `props` `key` | `val` | `(pfind p :color)` |
| `perase` | `props` `key` | `props` | `(perase p :state)` |

---

### Quoted Formatting Invariance in Prose and Lists

This section verifies that formatting syntax within double quotes is treated
strictly as literal text and is not acted on by the style parser:

* Literal quote containing asterisks: "This text has **no bold**, *no italic*,
  and ***no bold-italic*** formatting."

* Literal quote containing highlights and strikes: "This text has ==no
  highlight== and ~~no strikethrough~~."

* Literal quote containing code markers: "This text has `no backtick code`."

* Literal quote containing pipe delimiters: "First | Second | Third column
  markers that must remain verbatim."

---

### Quoted Styles and Embedded Pipes in Table Columns

| Target Style | Quoted Input (No Action) | Expected Interpretation |
| --- | --- | --- |
| Bold & Italic | "**literal bold** and *literal italic*" | Literal asterisks rendered inside quotes |
| Highlight & Strike | "==literal highlight== and ~~literal strike~~" | Literal equals and tildes inside quotes |
| Triple Asterisks | "***literal bold-italic***" | Literal three asterisks |
| Embedded Column Pipes | "Left Cell | Right Cell | Extra |" | Kept intact within single column cell |

---

### Headerless Two-Column Layout

Alpha Channel | Premultiplied 32-bit ARGB representation
Color Palette | Indexed 16-color lookup table with transparency
Vector Font | Cubic and quadratic Bezier curves