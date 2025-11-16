# Designer State Toggles

Inspired by Paul Hammant's AngularJS StoryNavigator showcase and blog post "The Importance of Design Mode for Client-Side MVC" (2012).

## The Problem

When designing UIs, you often have **conditional elements** that appear based on application state:

- Error messages (shown when `error_state = true`)
- Login vs Logout buttons (based on `logged_in`)
- Debug panels (shown when `debug_mode = true`)
- Empty state messages (shown when `empty_data = true`)
- Loading spinners, tooltips, modals, etc.

**Traditional approach**: Run the app and manually trigger each state to see what it looks like.

**Problem**: Slow, requires working code, hard to compare states.

## The Solution: Design-Time State Toggles

Add **toggle buttons** to the designer's ribbon/palette that let you flip between states **without running the app**.

```
╔════════════════════════════════════════════════════╗
║  Design-Time State Toggles                        ║
╠════════════════════════════════════════════════════╣
║  [error_state = false]  [logged_in = true]  ...   ║
║     Error messages         User logged in          ║
╚════════════════════════════════════════════════════╝
```

Click a toggle:
- `[error_state = false]` → `[error_state = true]`
- Preview updates instantly to show error messages
- No need to run the app or trigger the error

## How It Works

### 1. Register Toggles

Define design-time state variables:

```lisp
(designer-register-toggle "error_state" :nil "Show error messages")
(designer-register-toggle "logged_in" :t "User logged in")
(designer-register-toggle "debug_mode" :nil "Show debug info")
```

### 2. Mark Conditional Elements

Annotate which UI elements depend on which toggles:

```lisp
;Error label - only shown when error_state = true
(defq error-label (make-element "ui-label" "error_msg"))
(designer-set-property error-label :text "ERROR!")
(designer-set-property error-label :color "+argb_red")
(designer-mark-conditional error-label "error_state" "Shown when error occurs")

;Login button - only shown when logged_in = false
(defq login-btn (make-element "ui-button" "login_btn"))
(designer-set-property login-btn :text "Login")
(designer-mark-conditional login-btn "!logged_in" "Shown when not logged in")

;Logout button - only shown when logged_in = true
(defq logout-btn (make-element "ui-button" "logout_btn"))
(designer-set-property logout-btn :text "Logout")
(designer-mark-conditional logout-btn "logged_in" "Shown when logged in")
```

### 3. Toggle in Designer

Designer UI shows toggle buttons:

```
[error_state = false]  ← Click to flip
[logged_in = true]     ← Click to flip
[debug_mode = false]   ← Click to flip
```

When clicked:
1. Toggle state flips (`false` → `true`)
2. Button updates: `[error_state = true]` (now green)
3. Preview filters the UI tree based on new state
4. Conditional elements show/hide automatically

### 4. Filter and Preview

The designer filters the UI tree based on current toggles:

```lisp
(defun designer-filter-by-state (tree)
  ;Check each element's condition
  (when (get :conditional elem)
    (defq condition (get :condition elem))  ;e.g., "error_state"
    (unless (designer-evaluate-condition condition)
      ;Hide this element
      (set-hidden elem))))
```

Result: Preview shows exactly what the UI looks like in that state.

## AngularJS Showcase vs Design Tool

### AngularJS StoryNavigator (Original)

Shows **both states simultaneously** for comparison:

```
Status: [✓ Passed] [✗ Failed]  ← Both visible at once
Sort:   [↑ Asc] [↓ Desc] [- None]  ← All three visible
```

Good for:
- Documentation
- Showing all possibilities
- Static showcase

### Design Tool (This Implementation)

Shows **one state at a time** via toggles:

```
Click: [error_state = false]
  ↓
Shows: [error_state = true]
Preview: Error message now visible
```

Good for:
- Interactive design
- Testing layouts in different states
- Realistic preview (only one state active at a time)

## Example: Calculator States

Imagine designing the calculator app with state toggles:

### Toggle 1: Error State

```lisp
(designer-register-toggle "error_state" :nil "Division by zero error")
```

UI has two elements:
- Normal display: `"42"` (when `!error_state`)
- Error display: `"Error"` (when `error_state`)

Toggle to `true` → see how error message looks.

### Toggle 2: Memory Indicator

```lisp
(designer-register-toggle "has_memory" :nil "Memory value stored")
```

UI element:
- `"M"` indicator (when `has_memory`)

Toggle to `true` → see memory indicator appear.

### Toggle 3: Base Mode

```lisp
(designer-register-toggle "hex_mode" :nil "Hexadecimal mode")
```

UI elements:
- Buttons A-F enabled (when `hex_mode`)
- Buttons A-F disabled (when `!hex_mode`)

Toggle to see which buttons are available.

## Designer Comments

Add inline comments that appear only in the designer:

```lisp
(designer-add-comment error-label "This appears on divide-by-zero")
(designer-add-comment memory-indicator "Shows when M+ or M- used")
```

When serialized, these become actual code comments:

```lisp
;Designer: This appears on divide-by-zero
;Condition: error_state - Shown when error occurs
(ui-label error_msg (:text "Error" :color +argb_red))

;Designer: Shows when M+ or M- used
;Condition: has_memory - Memory value stored
(ui-label memory_indicator (:text "M"))
```

## Benefits

### 1. Design All States

See every possible UI state without running code:
- Error states
- Empty states
- Loading states
- Different user roles
- Feature flags

### 2. Fast Iteration

No need to:
- Compile and run
- Navigate to specific screen
- Trigger specific condition
- Set up test data

Just click the toggle!

### 3. Visual Comparison

Quickly toggle back and forth:
```
[logged_in = true]  → Shows: Logout button
[logged_in = false] → Shows: Login button
```

Compare visually, adjust styling, iterate.

### 4. Documentation

The toggles document the app's states:
```
Toggles:
  [error_state] - Show error messages
  [logged_in] - User logged in
  [debug_mode] - Debug info
```

Anyone opening the designer sees all possible states.

### 5. Designer-Developer Communication

Designers can:
- See all conditional states
- Design for edge cases
- Communicate state requirements

Developers get:
- Clear state definitions
- UI for each state
- Documented conditions

## Implementation Details

### Toggle Storage

```lisp
*designer-state-toggles* = {
  "error_state": {:value false :description "Show errors"}
  "logged_in": {:value true :description "User logged in"}
}
```

### Condition Evaluation

```lisp
(designer-evaluate-condition "error_state")   → false
(designer-evaluate-condition "!error_state")  → true
(designer-evaluate-condition "logged_in")     → true
```

### Tree Filtering

```lisp
(designer-filter-by-state tree)
  → Returns new tree with hidden elements marked
  → Preview renders only visible elements
```

### Serialization

Conditional metadata is preserved:

```lisp
Element:
  :conditional true
  :condition "error_state"
  :condition_desc "Shown when error occurs"
  :designer_comment "Error message label"

Serializes to:
  ;Designer: Error message label
  ;Condition: error_state - Shown when error occurs
  (ui-label error_msg ...)
```

## Usage Example

```bash
# Run the state toggle demo
./run.sh "(import \"apps/designer/designer_with_state_toggles.lisp\") (main)"
```

You'll see:
1. **Toggle ribbon** at top with 5 toggle buttons
2. **Preview area** showing conditional elements
3. **History panel** showing toggle changes

Try clicking toggles:
- Click `[error_state = false]` → becomes `[error_state = true]`
- Preview shows error message
- History shows: `error_state: false → true`

## Future Enhancements

### Multiple Values

Not just boolean, but multi-state:

```
[theme = light] → [theme = dark] → [theme = high-contrast]
```

### Grouped Toggles

Mutually exclusive states:

```
User Role:
  ( ) Guest
  (•) User
  ( ) Admin
```

### State Presets

Save and recall state combinations:

```
"Error Flow": error_state=true, logged_in=true
"Empty State": empty_data=true, logged_in=false
"Debug View": debug_mode=true, logged_in=true
```

### Animation Preview

Toggle between states with animation:

```
[error_state = false] → [error_state = true]
  ↓
Preview shows fade-in animation of error message
```

## Conclusion

State toggles bring the power of **design mode** to ChrysaLisp:

- **See all UI states** without running code
- **Fast iteration** on conditional elements
- **Better communication** between designers and developers
- **Documentation** of app states built into the designer

This was revolutionary in AngularJS (2012), and it's still revolutionary today!

## References

- [Paul Hammant's Blog: The Importance of Design Mode](https://paulhammant.com/2012/03/12/the-importance-of-design-mode-for-client-side-mvc/)
- [AngularJS StoryNavigator Showcase](https://paul-hammant-fork.github.io/StoryNavigator/navigator.html)
- Original NeXT InterfaceBuilder (design mode concept)

---

*"The designer should show all possible states of the UI, not just the default state."* - The Design Mode Philosophy
