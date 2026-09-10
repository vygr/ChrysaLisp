# :host_net

## Lisp Bindings

### (net-accept handle)

### (net-close handle)

### (net-connect host port)

### (net-deinit)

### (net-init)

### (net-listen port)

### (net-poll handle)

### (net-recv handle max_len)

### (net-send handle str)

## VP methods

### :accept -> :nil

### :close -> :nil

### :conn -> service/net/conn

```code
started for each network connection
trashes
:r0-:r14, :f0-:f15
```

### :connect -> :nil

### :deinit -> :nil

### :in -> service/net/in

```code
trashes
:r0-:r14, :f0-:f15
```

### :init -> :nil

### :link -> service/net/link

```code
started by kernel for each network link
trashes
:r0-:r14, :f0-:f15
```

### :listen -> :nil

### :out -> service/net/out

```code
trashes
:r0-:r14, :f0-:f15
```

### :poll -> :nil

### :recv -> :nil

### :recv_all -> service/net/recv_all

```code
inputs
:r0 = handle
:r1 = buffer ptr
:r2 = remaining bytes
:r3 = lk_node ptr
outputs
:r0 = 0 if ok, -1 if error
trashes
:r0-:r14, :f0-:f15
```

### :send -> :nil

### :send_all -> service/net/send_all

```code
inputs
:r0 = handle
:r1 = buffer ptr
:r2 = remaining bytes
outputs
:r0 = 0 if ok, -1 if error
```

