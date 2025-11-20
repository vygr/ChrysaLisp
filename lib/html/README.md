# ChrysaLisp HTML Browser with CSS

A complete HTML parser, CSS styling engine, and graphical renderer for ChrysaLisp, ported from KDE's KHTML engine.

## Overview

This library provides full HTML parsing, CSS styling, and graphical rendering capabilities for ChrysaLisp. It includes both text-based and graphical (canvas-based) rendering, CSS parsing and styling, and support for all major HTML elements including tables, forms, lists, and more.

## Features

✅ **HTML Parsing** - Full HTML5-compatible parser
✅ **CSS Styling** - CSS parser with selector matching and style application
✅ **Graphical Rendering** - Canvas-based rendering with fonts, colors, and layout
✅ **Text Rendering** - Terminal-friendly text output
✅ **All HTML Elements** - Tables, forms, lists, headings, links, images, etc.
✅ **DOM Manipulation** - Complete DOM API with queries and traversal
✅ **Unit Tests** - Comprehensive test suite ported from KHTML

## Components

### 1. Unit Testing Framework (`lib/test/unittest.inc`)

A comprehensive unit testing framework for ChrysaLisp, providing:
- Test suite definition with `deftest-suite`
- Individual test cases with `deftest`
- Assertions: `assert-true`, `assert-false`, `assert-eq`, `assert-neq`, `assert-nil`, `assert-not-nil`
- Automatic test reporting with pass/fail counts

### 2. Character Encoding (`lib/html/encoding.inc`)

Character encoding detection and conversion, supporting:
- UTF-8 encoding/decoding
- ISO-8859-1 (Latin-1) support
- Invalid character detection
- Buffered decoding for automatic encoding detection
- Encoding sources: user-specified, HTTP headers, auto-detection

Ported from: `khtml/src/kencodingdetector.h/cpp`

### 3. DOM Structure (`lib/html/dom.inc`)

Document Object Model implementation with:
- Node types: Element, Text, Comment, Document
- DOM tree construction and manipulation
- Attribute management
- Element lookup by ID and tag name
- Tree traversal and serialization

Ported from: `khtml/src/dom/`

### 4. HTML Tokenizer (`lib/html/tokenizer.inc`)

HTML tokenization with support for:
- Start tags and end tags
- Attributes (quoted and unquoted)
- Self-closing tags (void elements)
- Comments
- DOCTYPE declarations
- Text content

### 5. HTML Parser (`lib/html/parser.inc`)

HTML parsing engine that:
- Tokenizes HTML input
- Builds a DOM tree
- Handles nested elements
- Manages void elements (br, img, etc.)
- Processes attributes
- Handles malformed HTML gracefully

Ported from: `khtml/src/html/`

### 6. HTML Part & View (`lib/html/part.inc`)

KHTML-style part/view architecture:
- `html-part`: Manages document parsing and content
- `html-view`: Represents visual display
- Begin/write/end API for incremental content
- Bidirectional part-view linking

Ported from: `khtml/src/khtml_part.h/cpp`, `khtml/src/khtmlview.h/cpp`

### 7. CSS Parser and Styling (`lib/html/css.inc`)

CSS parsing and styling engine with:
- CSS rule parsing (selectors and properties)
- Selector matching (tag, class, ID, universal)
- Style computation and cascading
- Color parsing (hex, rgb, named colors)
- Size parsing (px, em, %)
- Font weight and size styling

### 8. Text Renderer (`lib/html/renderer.inc`)

Text-based HTML renderer with:
- Line wrapping at configurable width
- Indentation for nested elements
- Special handling for headings, paragraphs, lists
- Link rendering with URLs
- Bold and italic text markers
- Preformatted text support

### 9. Canvas Renderer (`lib/html/canvas_renderer.inc`)

Graphical HTML renderer using ChrysaLisp's Canvas:
- Full graphical rendering with fonts and colors
- CSS styling application (colors, fonts, sizes)
- All HTML elements rendered graphically:
  - Text with wrapping and styling
  - Headings with size scaling
  - Tables with borders
  - Lists (ordered and unordered)
  - Links with colored text
  - Forms and input elements
  - Images (placeholder)
  - Horizontal rules
- Layout engine with positioning

### 10. Browser Widget (`lib/html/browser.inc`)

HTML browser GUI widget:
- Scrollable HTML view
- Integration with Canvas system
- HTML content loading
- CSS stylesheet support

## Unit Tests

### Ported from KHTML autotests:

1. **Encoding Detector Tests** (`test/html/test_encoding.lisp`)
   - Ported from: `khtml/autotests/kencodingdetectortest.cpp`
   - Tests encoding detection, decoding, buffering

2. **HTML Part Tests** (`test/html/test_part.lisp`)
   - Ported from: `khtml/autotests/khtmlparttest.cpp`
   - Tests part/view construction and relationships

3. **Parser Tests** (`test/html/test_parser.lisp`)
   - Tests HTML parsing, DOM construction, rendering
   - Tests element lookup, attributes, nesting

## Usage Examples

### Basic HTML Parsing

```lisp
(import "lib/html/parser.inc")

(defq html "<html><body><h1>Hello World</h1><p>Welcome to ChrysaLisp HTML!</p></body></html>")
(defq doc (parse-html html))

; Find elements
(defq headings (. doc :get-elements-by-tag-name "h1"))
(defq elem (. doc :get-element-by-id "myid"))
```

### CSS Parsing and Styling

```lisp
(import "lib/html/parser.inc")
(import "lib/html/css.inc")

; Parse HTML
(defq html "<h1 id=\"title\">Styled Heading</h1><p class=\"intro\">Text</p>")
(defq doc (parse-html html))

; Parse CSS
(defq css "#title { color: #ff0000; font-size: 24px; } .intro { color: #0000ff; }")
(defq stylesheet (parse-css css))

; Get styles for an element
(defq title (. doc :get-element-by-id "title"))
(defq styles (. stylesheet :compute-styles title))
; styles will contain: {"color" "#ff0000", "font-size" "24px"}
```

### Graphical Rendering with Canvas

```lisp
(import "gui/canvas/lisp.inc")
(import "lib/html/parser.inc")
(import "lib/html/css.inc")
(import "lib/html/canvas_renderer.inc")

; Parse HTML and CSS
(defq html "<h1>Hello</h1><p class=\"red\">Red text</p>")
(defq css "h1 { color: #0000ff; } .red { color: #ff0000; }")
(defq doc (parse-html html))
(defq stylesheet (parse-css css))

; Create canvas
(defq canvas (Canvas))
(.-> canvas
	(:set_size 800 600)
	(:canvas_alloc 0 800 600 0xffffffff 1))

; Render HTML with CSS
(defq renderer (html-canvas-renderer :init doc canvas 800 stylesheet))
(. renderer :render)

; Save to file
(. canvas :save "output.png" "png")
```

### Text Rendering

```lisp
(import "lib/html/parser.inc")
(import "lib/html/renderer.inc")

(defq html "<h1>Title</h1><p>This is a paragraph.</p>")
(defq doc (parse-html html))
(defq text (render-html-to-text doc 80))  ; 80 character line width
(print text)
```

### Using HTML Part (KHTML-style)

```lisp
(import "lib/html/part.inc")

(defq part (html-part :init))
(. part :begin)
(. part :write "<html><body>")
(. part :write "<h1>Dynamic Content</h1>")
(. part :write "</body></html>")
(. part :end)

; Get the HTML
(defq content (. part :get-html))
```

### DOM Manipulation

```lisp
(import "lib/html/dom.inc")

(defq doc (html-document :init))
(defq div (. doc :create-element "div"))
(. div :set-attribute "id" "container")

(defq para (. doc :create-element "p"))
(defq text (. doc :create-text-node "Hello!"))
(. para :append-child text)
(. div :append-child para)
(. doc :append-child div)

(print (. doc :to-string))
```

## Running Tests

Run all tests:
```bash
./run_lisp.sh test/html/run_all_tests.lisp
```

Run individual test suites:
```bash
./run_lisp.sh test/html/test_encoding.lisp
./run_lisp.sh test/html/test_part.lisp
./run_lisp.sh test/html/test_parser.lisp
```

## Demo Application

See the HTML parser and renderer in action:
```bash
./run_lisp.sh cmd/htmldemo.lisp
```

## What's Not Included (Yet)

Since ChrysaLisp doesn't have a TCP/IP stack, the following KHTML features are not included:
- Network loading (HTTP/HTTPS)
- Image loading and rendering
- CSS styling and layout
- JavaScript execution
- Form handling
- Cookie management
- Graphical rendering (only text rendering implemented)

## Future Enhancements

Potential additions:
1. CSS parser and basic styling
2. Graphical renderer using ChrysaLisp's GUI system
3. More sophisticated layout engine
4. Table rendering
5. Form element representation
6. SVG support (KHTML has SVG support)
7. Integration with ChrysaLisp's networking when available

## Source Attribution

This implementation is ported from KDE's KHTML:
- Original source: https://github.com/KDE/khtml (kf5 branch)
- License: LGPL v2
- Copyright: KDE contributors

The ChrysaLisp port maintains compatibility with KHTML's architecture while adapting to ChrysaLisp's Lisp-based object system and idioms.

## Architecture Notes

### Differences from KHTML

1. **Object System**: Uses ChrysaLisp's class/method system instead of C++
2. **Memory Management**: Relies on ChrysaLisp's reference counting instead of Qt's QObject
3. **Strings**: Uses ChrysaLisp strings instead of QString
4. **Collections**: Uses ChrysaLisp lists and environments instead of Qt containers
5. **Rendering**: Text-based rendering instead of QPainter graphics

### Design Principles

- **Simplicity**: Focus on core HTML parsing and rendering
- **Testability**: Comprehensive unit tests ported from KHTML
- **Modularity**: Clean separation of tokenizer, parser, DOM, and renderer
- **ChrysaLisp Idioms**: Uses iteration (`each!`, `map!`, etc.) over recursion where appropriate

## Contributing

When contributing to this HTML library:
1. Follow ChrysaLisp coding style (see `docs/ai_digest/coding_style.md`)
2. Add unit tests for new features
3. Ensure all existing tests pass
4. Update documentation

## References

- [ChrysaLisp Documentation](../../docs/ai_digest/)
- [KHTML Documentation](https://api.kde.org/frameworks/khtml/html/)
- [HTML5 Specification](https://html.spec.whatwg.org/)
