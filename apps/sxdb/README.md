# SXDb - S-expression Database

A natural persistence format for ChrysaLisp data structures, built on top of `tree-load` and `tree-save`.

## Features

- **Natural Lisp Format**: Data is stored as human-readable S-expressions
- **Fast Serialization**: Built on ChrysaLisp's native `tree-save` and `tree-load`
- **Indexing**: Create indexes on any field for O(1) lookups
- **Querying**: Query records with predicate functions
- **Multi-Client**: Message-passing architecture supports multiple concurrent clients
- **CRUD Operations**: Full Create, Read, Update, Delete support
- **Collections**: Organize data into named collections (like tables)
- **Auto-generated IDs**: Each record gets a unique numeric ID

## Architecture

### Service Model

SXDb runs as a ChrysaLisp service with message-passing for multi-client access:
- Single service instance per node
- Clients communicate via mailboxes
- Asynchronous request/response pattern

### Data Structure

```lisp
(Emap
  :name "database_name"
  :path "/path/to/db.sxdb"
  :collections (Emap
    "collection_name" (Emap
      :data (Emap id -> record)
      :next_id num
      :indexes (Emap field_name -> (Emap field_value -> (list ids...)))
    )
  )
)
```

### Storage Format

Data is persisted to `.sxdb` files using ChrysaLisp's tree format. Files are human-readable and editable, containing pure S-expressions.

## Usage

### Starting the Service

```lisp
(import "apps/sxdb/app.lisp")
```

The service will start automatically if not already running.

### Using the Client Library

```lisp
(import "apps/sxdb/client.inc")

;connect to service
(defq db (sxdb-connect))

;open database
(sxdb-open db "./mydata.sxdb")

;create an index
(sxdb-create-index db "./mydata.sxdb" "users" "email")

;insert a record
(defq user (scatter (Emap) :name "Alice" :email "alice@example.com" :age 30))
(defq id (sxdb-insert db "./mydata.sxdb" "users" user))

;find by id
(sxdb-find db "./mydata.sxdb" "users" id)

;find by indexed field
(sxdb-find-by db "./mydata.sxdb" "users" "email" "alice@example.com")

;query with predicate
(sxdb-query db "./mydata.sxdb" "users"
  (lambda (record) (> (. record :find "age") 25)))

;update a record
(sxdb-update db "./mydata.sxdb" "users" id
  (scatter (Emap) :age 31))

;delete a record
(sxdb-delete db "./mydata.sxdb" "users" id)

;close database
(sxdb-close db "./mydata.sxdb")
```

## API Reference

### Connection

- `(sxdb-connect)` - Connect to the SXDb service

### Database Operations

- `(sxdb-open db_mbox path)` - Open or create a database
- `(sxdb-close db_mbox path)` - Close and save a database
- `(sxdb-list-dbs db_mbox)` - List all open databases

### Collection Operations

- `(sxdb-insert db_mbox path coll record)` - Insert a record
- `(sxdb-find db_mbox path coll id)` - Find by ID
- `(sxdb-find-by db_mbox path coll field value)` - Find by indexed field
- `(sxdb-update db_mbox path coll id updates)` - Update a record
- `(sxdb-delete db_mbox path coll id)` - Delete a record
- `(sxdb-all db_mbox path coll)` - Get all records
- `(sxdb-query db_mbox path coll pred)` - Query with predicate
- `(sxdb-list-collections db_mbox path)` - List collections
- `(sxdb-stats db_mbox path coll)` - Get collection statistics

### Index Operations

- `(sxdb-create-index db_mbox path coll field)` - Create index
- `(sxdb-drop-index db_mbox path coll field)` - Drop index
- `(sxdb-list-indexes db_mbox path coll)` - List indexes

## Example

See `example.lisp` for a comprehensive demonstration:

```bash
;start the service
(import "apps/sxdb/app.lisp")

;in another terminal or REPL instance
(import "apps/sxdb/example.lisp")
(main)
```

## Testing

A comprehensive test suite is provided in `test.lisp` that validates all functionality:

### Running the Tests

```lisp
;start the service
(import "apps/sxdb/app.lisp")

;in another terminal or REPL instance
(import "apps/sxdb/test.lisp")
(main)
```

### Test Coverage

The test suite covers:
- Database open/close operations
- Index creation and management
- Insert/find/update/delete operations
- Indexed field lookups
- Query predicates
- Collection management
- Statistics and metadata
- Persistence and reload
- Multiple databases
- Error handling

The tests automatically clean up after themselves, removing test database files.

### Test Output

The test suite provides clear pass/fail indicators:
- ✓ marks passing tests
- ✗ marks failing tests
- Summary shows total/passed/failed counts
- Returns exit code 0 on success, 1 on failure

## Design Philosophy

SXDb embraces ChrysaLisp's philosophy:

1. **Natural Format**: S-expressions are the native format for Lisp data
2. **Leverage Existing Primitives**: Built on `tree-load` and `tree-save`
3. **Message-Passing**: Uses ChrysaLisp's mailbox system for concurrency
4. **Simple and Direct**: No complex query language - use predicates
5. **Human-Readable**: Database files can be inspected and edited
6. **Fast**: O(1) indexed lookups, efficient serialization

## Implementation Details

### Indexing

Indexes are implemented as nested maps:
```
field_name -> field_value -> [list of record IDs]
```

When a field is indexed, all insert/update/delete operations automatically maintain the index.

### ID Generation

Each collection maintains a `next_id` counter. IDs are simple incrementing integers stored in the record under the `_id` key.

### Persistence

Databases are saved to disk on `close` operations. The entire database structure is serialized using `tree-save`, which handles:
- Nested collections
- Various data types (strings, numbers, symbols)
- Proper encoding of special characters

## Advantages Over Other Databases

1. **Native Lisp**: No impedance mismatch - store Lisp data directly
2. **No Schema**: Records can have any fields (schemaless)
3. **Inspectable**: Open the .sxdb file in any text editor
4. **Embeddable**: No external dependencies or processes
5. **Type-Safe**: Uses ChrysaLisp's native types
6. **Fast**: In-memory with efficient disk format
7. **Simple**: No SQL, no complex setup - just Lisp

## Use Cases

- **Configuration**: Store application settings
- **User Data**: Manage user profiles and preferences
- **Logging**: Structured log storage
- **Caching**: Persistent cache with indexing
- **Development**: Easy data inspection and modification
- **Small to Medium Data**: Perfect for datasets that fit in memory

## Limitations

- **In-Memory**: Entire database loads into memory
- **Single-Writer**: One service instance per node
- **No Transactions**: Operations are not atomic across multiple records
- **No Foreign Keys**: No built-in relationship management
- **Queries are Linear**: Without indexes, queries scan all records

## Future Enhancements

Possible extensions:
- Write-ahead logging for durability
- Snapshot/checkpoint system
- Compound indexes
- Background persistence
- Query optimizer
- Replication support
- Compression

## License

Part of ChrysaLisp - see main repository license.
