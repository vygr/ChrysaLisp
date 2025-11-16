# Visual Drag-and-Drop Implementation

## The Model-then-View Pattern

The key insight: **Don't move widgets directly. Update the tree model, then regenerate the view.**

```
User drags → Determine intention → Update MODEL → Regenerate VIEW
```

## Why This Works

### Traditional Approach (DOM/Widget Manipulation)
```javascript
// Move widget in UI
widget.parent.removeChild(widget);
newParent.insertBefore(widget, target);

// Problem: Model and view are now out of sync!
// Serialization won't match what user sees
```

### Model-First Approach (This Implementation)
```lisp
; Update tree model
(designer-execute-move tree source target :before)

; Regenerate view from tree
(render-tree-to-preview tree)

; Serialization always correct (from model)
(designer-serialize-tree tree)
```

## Implementation Overview

### 1. Capture Mouse Events

```lisp
(defun handle-preview-mouse-down (event)
  ; Find what element was clicked
  (defq x (getf event +ev_msg_mouse_x)
        y (getf event +ev_msg_mouse_y))
  (defq element (designer-find-element-at tree x y preview-view))

  ; Select it and record position
  (designer-select-element element)
  (. *drag-state* :insert :drag-start-x x)
  (. *drag-state* :insert :drag-start-y y))
```

### 2. Track Dragging

```lisp
(defun handle-preview-mouse-move (event)
  ; Update current position
  (defq x (getf event +ev_msg_mouse_x)
        y (getf event +ev_msg_mouse_y))

  ; Check if drag threshold exceeded (5 pixels)
  (when (> (drag-distance start-x start-y x y) 5)
    ; Start dragging
    (. *drag-state* :insert :dragging :t))

  ; Find drop target
  (defq target (designer-find-element-at tree x y preview-view))
  (defq position (designer-determine-drop-position target x y))
  (. *drag-state* :insert :drop-target target)
  (. *drag-state* :insert :drop-position position))
```

### 3. Execute on Release

```lisp
(defun handle-preview-mouse-up (event)
  (when (get :dragging *drag-state*)
    ; Get drag info
    (defq source (get :selected-element *drag-state*)
          target (get :drop-target *drag-state*)
          position (get :drop-position *drag-state*))

    ; UPDATE THE MODEL
    (designer-execute-move tree source target position)

    ; REGENERATE THE VIEW
    (regenerate-preview tree)

    ; Clear drag state
    (. *drag-state* :insert :dragging :nil)))
```

## Tree Manipulation Functions

### insertBefore Equivalent

```lisp
(defun designer-insert-child-before (parent new-child before-child)
  ; Rebuild children list with insertion
  (defq children (get :children parent))
  (defq new-children (list))

  (each (lambda (child)
    ; Insert before target
    (when (eql child before-child)
      (push new-children new-child))
    (push new-children child))
    children)

  ; Update parent
  (. parent :insert :children new-children))
```

### insertAfter Equivalent

```lisp
(defun designer-insert-child-after (parent new-child after-child)
  (defq children (get :children parent))
  (defq new-children (list))

  (each (lambda (child)
    (push new-children child)
    ; Insert after target
    (when (eql child after-child)
      (push new-children new-child)))
    children)

  (. parent :insert :children new-children))
```

### appendChild (Inside Container)

```lisp
(defun designer-add-child (parent child)
  ; Already exists in runtime.inc
  (push (get :children parent) child))
```

### Move Up/Down in Sibling Order

```lisp
(defun designer-move-child-up (parent child)
  ; Swap with previous sibling
  (defq children (get :children parent))
  (defq index (find-index child children))

  (when (> index 0)
    ; Swap positions
    (swap-elements children index (dec index))
    (. parent :insert :children children)))
```

## Drop Position Determination

### Simple Heuristic

```lisp
(defun designer-determine-drop-position (target x y)
  (defq target-type (get :type target))

  ; Containers: drop inside
  (if (or (eql target-type "ui-flow")
          (eql target-type "ui-grid")
          (eql target-type "ui-window"))
    :inside

    ; Non-containers: drop before/after
    ; Use mouse position to determine which
    :after))
```

### Advanced: Position-Based

```lisp
(defun designer-determine-drop-position (target x y)
  ; Get target's bounds
  (bind '(tx ty tw th) (get-element-bounds target))

  ; If mouse is in top half, insert before
  (if (< y (+ ty (/ th 2)))
    :before
    :after))
```

## Complete Drag Operation

```lisp
(defun designer-execute-move (tree source target position)
  ; 1. Find source's current parent
  (defq source-parent (designer-find-parent tree source))

  ; 2. Remove from current location
  (designer-remove-child source-parent source)

  ; 3. Insert at new location
  (case position
    (:before
      (defq target-parent (designer-find-parent tree target))
      (designer-insert-child-before target-parent source target))

    (:after
      (defq target-parent (designer-find-parent tree target))
      (designer-insert-child-after target-parent source target))

    (:inside
      (designer-add-child target source)))

  ; Return modified tree
  tree)
```

## View Regeneration

### Option 1: Full Rebuild

```lisp
(defun regenerate-preview (tree)
  ; Clear old preview
  (clear-preview-area)

  ; Serialize tree to code
  (defq code (designer-serialize-tree tree))

  ; Execute code to create new widgets
  (eval (read code))

  ; Display in preview area
  (display-in-preview *window*))
```

### Option 2: Incremental Update

```lisp
(defun update-preview-for-move (source target position)
  ; Remove source widget from view
  (. (get :view source) :sub)

  ; Insert at new position in view
  (case position
    (:before
      (. (get :parent-view target) :add_before
        (get :view source) (get :view target)))

    (:after
      (. (get :parent-view target) :add_after
        (get :view source) (get :view target)))

    (:inside
      (. (get :view target) :add_child (get :view source))))

  ; Relayout
  (.-> preview-area :constrain :t :dirty_all))
```

## Example: Drag Button Between Containers

### Initial State

```lisp
Tree:
  window
    toolbar (flow)
      new_btn
      open_btn
    content (flow)
      title
      submit_btn

UI:
  [New] [Open]
  My App
  [Submit]
```

### User Action

1. Click on `submit_btn` → selects it
2. Drag mouse toward `toolbar`
3. Release mouse over `open_btn`

### System Response

```lisp
; Detected:
; source = submit_btn
; target = open_btn
; position = :after

; Execute move:
(designer-execute-move tree submit_btn open_btn :after)

; Tree now:
  window
    toolbar
      new_btn
      open_btn
      submit_btn  ← Moved here!
    content
      title  ← submit_btn removed

; Regenerate view:
(regenerate-preview tree)

; UI updates:
  [New] [Open] [Submit]  ← Button moved!
  My App
```

### Serialized Code

```lisp
(ui-window *window* ()
  (ui-flow toolbar ()
    (ui-button new_btn (:text "New"))
    (ui-button open_btn (:text "Open"))
    (ui-button submit_btn (:text "Submit")))  ← Here now!
  (ui-flow content ()
    (ui-label title (:text "My App"))))
```

## Visual Feedback

### During Drag

```lisp
(defun show-drag-feedback ()
  ; Ghost image of dragged element
  (draw-ghost-at current-x current-y)

  ; Drop target highlight
  (highlight-element drop-target drop-position)

  ; Drop indicator
  (case drop-position
    (:before (draw-line-above drop-target))
    (:after (draw-line-below drop-target))
    (:inside (draw-border-around drop-target))))
```

### Selection Highlight

```lisp
(defun highlight-selected (element)
  ; Draw selection border
  (when (defq view (get :view element))
    (bind '(x y w h) (. view :get_bounds))
    (draw-selection-border x y w h)))
```

## Integration with Designer App

### Add Mouse Handlers to Preview

```lisp
(ui-window *window* ()
  ; ... toolbar, palette ...

  ; Preview area with mouse handling
  (ui-backdrop preview (:min_width 400 :min_height 400)
    ; Preview content rendered here

    ; Mouse handlers
    (defmethod :mouse_down (event)
      (handle-preview-mouse-down event))

    (defmethod :mouse_move (event)
      (handle-preview-mouse-move event))

    (defmethod :mouse_up (event)
      (handle-preview-mouse-up event))))
```

### In Main Event Loop

```lisp
(defun main ()
  ; ... setup ...

  (while running
    (defq msg (mail-read ...))

    ; Tree might be modified by drag operations
    (when (defq new-tree (handle-event msg tree))
      ; Tree was updated
      (setq tree new-tree)

      ; Regenerate preview
      (regenerate-preview tree)

      ; Mark as modified
      (setq modified :t))))
```

## Benefits of Model-First Approach

### 1. Always Consistent

```
Model (tree) = Source of Truth
View = Rendered from tree
Serialization = From tree

All three always match!
```

### 2. Undo/Redo Simple

```lisp
; Undo = restore previous tree state
(setq tree (pop undo-stack))
(regenerate-preview tree)

; Redo = restore next tree state
(setq tree (pop redo-stack))
(regenerate-preview tree)
```

### 3. Multi-View Updates

```lisp
; Update tree once
(designer-execute-move tree source target :before)

; All views update automatically
(regenerate-preview tree)
(update-tree-view tree)
(update-property-panel tree)
```

### 4. Save Always Works

```lisp
; Save just serializes the model
(save-file (designer-serialize-tree tree))

; No need to sync view back to model
; No risk of inconsistency
```

## Performance Considerations

### For Simple UIs

Full rebuild is fine:
```lisp
(defun regenerate-preview (tree)
  (clear-preview)
  (render-tree tree))
```

### For Complex UIs

Incremental updates:
```lisp
(defun update-after-drag (source target position)
  ; Only move the affected widgets
  ; Don't rebuild entire tree
  (move-widget-in-view source target position)
  (relayout-affected-containers))
```

### Hybrid Approach

```lisp
(defun regenerate-preview (tree)
  ; If tree is small, full rebuild
  (if (< (count-elements tree) 50)
    (full-rebuild tree)
    (incremental-update tree)))
```

## Demo

Run the drag-drop demo:

```bash
./run.sh "(import \"apps/designer/demo_drag_drop.lisp\")"
```

Shows:
1. Initial tree structure
2. Drag button before another (reorder)
3. Drag element between containers (move)
4. Drag element into container (nest)
5. Final serialized code

Each operation updates the tree model, then shows the result.

## Next Steps

1. **Wire to Mouse Events** - Connect to actual preview widget
2. **Visual Feedback** - Show ghost image, drop indicators
3. **Hit Testing** - Accurately find elements at mouse position
4. **Constraints** - Prevent invalid moves (e.g., can't drag window)
5. **Keyboard Shortcuts** - Arrow keys to move, Del to remove

## Conclusion

The **model-first approach** makes drag-and-drop clean and reliable:

- ✅ Update tree model (the truth)
- ✅ Regenerate view (derived from truth)
- ✅ Serialization always correct
- ✅ Undo/redo trivial
- ✅ No view-model sync issues

This is the **right way** to implement visual editing! 🎯
