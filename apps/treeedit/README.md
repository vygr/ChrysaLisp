# WYSIWYG Tree Editor

A graphical tree editor for ChrysaLisp that allows you to create, edit, and manage hierarchical tree structures.

## Features

- **Visual Tree Display**: See your tree structure with expandable/collapsible folders
- **Add Folders**: Create new folder nodes in the hierarchy
- **Add Items**: Create leaf nodes (items) in the tree
- **Delete Nodes**: Remove selected nodes from the tree
- **Rename Nodes**: Edit the names of existing nodes
- **Save/Load**: Persist tree structures to files (.tree format)

## Usage

### Running the Application

From the ChrysaLisp REPL or shell:
```lisp
(import "apps/treeedit/app.lisp")
(main)
```

### Toolbar Buttons

The toolbar contains six action buttons (left to right):
1. **Add Folder** (📁): Creates a new folder node
2. **Add Item** (📄): Creates a new leaf item
3. **Delete** (🗑️): Removes the selected node
4. **Rename** (✏️): Renames the selected node to the text in the name field
5. **Save** (💾): Saves the tree structure to a file
6. **Load** (📂): Loads a tree structure from a file

### Workflow

1. **Select a Node**: Click on any node in the tree to select it
2. **Add New Nodes**:
   - Enter a name in the "Node Name" field
   - Click "Add Folder" to create a folder under the selected node
   - Click "Add Item" to create a leaf item under the selected node
   - Press Enter in the name field to quickly add an item
3. **Rename a Node**:
   - Click to select the node you want to rename
   - Edit the name in the "Node Name" field
   - Click the "Rename" button
4. **Delete a Node**:
   - Click to select the node you want to delete
   - Click the "Delete" button
5. **Save Your Work**:
   - Click the "Save" button
   - Choose a filename in the file picker
   - The tree structure will be saved with a .tree extension
6. **Load a Tree**:
   - Click the "Load" button
   - Select a .tree file from the file picker
   - The tree will be loaded and displayed

## File Format

Tree files (.tree) are simple text files with one route per line:
```
Root/.
Root/Folder1/.
Root/Folder1/Item1
Root/Folder1/Item2
Root/Folder2/.
Root/Item3
```

- Lines ending with `/` followed by `.` represent folders
- Other lines represent leaf items
- Paths use `/` as a separator

## Implementation Details

The tree editor is built using ChrysaLisp's GUI system:
- **Tree Widget**: The core hierarchical display (`gui/tree/lisp.inc`)
- **Files Widget**: Wraps the tree with scrolling and title bar
- **Event System**: Button clicks and node selection trigger actions
- **File I/O**: Uses file-stream for saving and loading tree data

## Files

- `app.lisp` - Parent process launcher
- `child.lisp` - Main application implementation
- `widgets.inc` - UI layout definition
- `actions.inc` - Event-to-action mapping
- `ui.inc` - Action implementations

## Sample Data

The editor loads with sample data to demonstrate its capabilities:
- Root folder with two subfolders
- Nested items and folders
- Various hierarchy levels

Feel free to delete this sample data and create your own tree structures!
