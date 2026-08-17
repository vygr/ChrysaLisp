# Text Handler Master Test Suite

This document tests all typographic and layout features parsed and formatted
directly by the `text` handler.

---

# Heading Level 1 (34pt)

## Heading Level 2 (30pt)

### Heading Level 3 (26pt)

#### Heading Level 4 (22pt)

Standard body paragraph text (18pt) demonstrating proportional font rendering,
natural line breaks, and automatic word wrapping across the view width.

---

### Inline Styles and Character Formatting

This section tests every supported inline character formatting rule:

* Standard body text followed by **bold text**, *italic text*, and
  ***bold-italic text***.

* Technical highlights using ==highlighted background== and ~~strikethrough
  text~~.

* Inline code blocks such as `(defq a 10 b 20)` and `(mail-send mbox msg)`.

* Quoted literals like "double-quoted string text" within normal prose.

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

1.1. Nested outline numbering test item.

1.2. Secondary nested outline numbering item.

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

### Headerless Two-Column Layout

Alpha Channel | Premultiplied 32-bit ARGB representation
Color Palette | Indexed 16-color lookup table with transparency
Vector Font | Cubic and quadratic Bezier curves