# ExFat

## Fs

```code
Initialize handle management
```

### :alloc_cluster

### :close_file

```code
Close file handle - remove from handles map
```

### :cluster_to_offset

### :create_dir

```code
Create a directory at the given path
```

### :create_entry

### :create_file

```code
Create a new file at the given path

Returns the new file's cluster number or :nil
```

### :create_sub_dir

### :delete_dir

```code
Delete a directory at the given path

Reuses delete_file logic as delete_entry handles both
```

### :delete_entry

### :delete_file

```code
Delete a file at the given path or return :nil
```

### :find_bitmap

### :find_bitmap

### :find_entry

### :format

### :free_chain

### :free_cluster

### :get_fat_entry

### :list_dir

### :mount

### :open_file

```code
Open a file and return handle ID (or :nil if invalid)

mode: :read, :write, :append
```

### :read_cluster

### :read_dir_cluster

### :read_file

```code
Read data from file at current position
```

### :resolve_path

### :seek_file

```code
Seek to position in file

whence: 0=SEEK_SET, 1=SEEK_CUR, 2=SEEK_END
```

### :set_fat_entry

### :stat

```code
Gets file/directory metadata

Returns list: (name size is_dir attr first_cluster) or :nil
```

### :unmount

### :update_entry_size

### :write_cluster

### :write_file

```code
Write data to file at current position
```

