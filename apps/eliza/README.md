# ELIZA - Classic Pattern Matching Chatbot

A ChrysaLisp implementation of the classic ELIZA chatbot, demonstrating pattern matching and symbolic processing.

## About ELIZA

ELIZA was created by Joseph Weizenbaum at MIT in 1966. It simulates a Rogerian psychotherapist using pattern matching and transformation rules. ELIZA is one of the earliest examples of natural language processing and demonstrates how simple pattern matching can create the illusion of understanding.

## How It Works

### Pattern Matching

ELIZA uses a keyword-based pattern matching system with wildcards:

- **Keywords**: Words like "i", "you", "my", "remember", "dream" etc.
- **Patterns**: Templates with wildcards (`*`) that match user input
- **Rank**: Priority for patterns (higher rank patterns are preferred)

Example pattern:
```lisp
("remember" 5
  (("* remember *")
   ("Do you often think of %1?"
    "Does thinking of %1 bring anything else to mind?")))
```

### Reflection

ELIZA transforms pronouns and possessives to create natural responses:

- "I" → "you"
- "my" → "your"
- "am" → "are"
- "me" → "you"

Example:
- User: "I am feeling sad"
- Pattern match: "* i am * sad *"
- Reflection: "I" → "you", "am" → "are"
- Response: "I'm sure it's not pleasant to be feeling sad."

### Symbolic Processing

The implementation demonstrates:

1. **Text normalization**: Converting to lowercase, removing punctuation
2. **Tokenization**: Splitting input into words
3. **Pattern matching**: Finding matching patterns with wildcards
4. **Substitution**: Replacing matched wildcards with reflected text
5. **Response generation**: Creating natural-sounding responses

## Features

- Over 20 keyword patterns with multiple response templates
- Reflection system for pronoun transformation
- Wildcard pattern matching
- VDU-based chat interface
- Word wrapping for display
- Clear conversation option

## Architecture

```
patterns.inc  - Pattern rules, reflections, and response templates
engine.inc    - Pattern matching engine and symbolic processing
app.lisp      - GUI interface and event handling
```

## Usage

Launch ELIZA from the ChrysaLisp launcher or run:
```
apps/eliza/app.lisp
```

Type messages and press Enter to chat. Type "quit", "exit", "bye", or "goodbye" to end the conversation.

## Classic ELIZA Responses

Try these classic inputs:

- "I am feeling sad"
- "I remember my childhood"
- "I have a dream"
- "My mother doesn't understand me"
- "I can't do anything right"
- "You don't like me"
- "Are you a computer?"

## Implementation Notes

This implementation stays true to the original ELIZA concept while using modern ChrysaLisp idioms:

- Pattern matching using list processing
- Functional programming with `map`, `filter`, `reduce`
- Symbolic list manipulation
- No external dependencies beyond ChrysaLisp core

## Historical Significance

ELIZA was groundbreaking in demonstrating:

1. **Simple rules can create illusion of intelligence**
2. **Pattern matching for language processing**
3. **Human tendency to anthropomorphize computers**
4. **Power of reflection and open-ended questions**

Many users became emotionally attached to ELIZA, revealing insights about human-computer interaction that remain relevant today.

## References

- Weizenbaum, Joseph (1966). "ELIZA—A Computer Program For the Study of Natural Language Communication Between Man And Machine". *Communications of the ACM*. 9 (1): 36–45.
