# :sys_mail

## Lisp Bindings

### (mail-declare mbox name info) -> key

### (mail-nodes) -> nodeids

### (mail-enquire prefix) -> netids

### (mail-forget key)

### (mail-mbox) -> netid

### (mail-poll mboxs) -> :nil | idx

### (mail-read mbox) -> :nil | msg

### (mail-select mboxs) -> idx

### (mail-send mbox obj)

### (mail-timeout mbox ns id) -> mbox

### (mail-validate mbox) -> :t | :nil

## VP methods

### :alloc -> sys/mail/alloc

```code
inputs
:r0 = mail size (bytes)
outputs
:r0 = mail message (ptr)
:r1 = string data (pubyte)
trashes
:r0-:r5, :f0-:f15
```

### :alloc_mbox -> sys/mail/alloc_mbox

```code
outputs
:r0 = mailbox id (uint)
:r1 = mailbox address (ptr)
trashes
:r0-:r5, :f0-:f15
```

### :alloc_obj -> sys/mail/alloc_obj

```code
inputs
:r0 = object (ptr)
:r1 = data (pubyte)
:r2 = data length (bytes)
outputs
:r0 = mail message (ptr)
trashes
:r0-:r5, :f0-:f15
```

### :declare -> sys/mail/declare

```code
inputs
:r0 = ID str object (net_id)
:r1 = service name str object (ptr)
:r2 = service info str object (ptr)
outputs
:r0 = service key str object (ptr)
trashes
:r0-:r14, :f0-:f15
```

### :devices -> sys/mail/devices

```code
outputs
:r0 = known network nodes list object (ptr)
trashes
:r0-:r14, :f0-:f15
```

### :enquire -> sys/mail/enquire

```code
inputs
:r0 = service prefix str object (ptr)
outputs
:r0 = matching service entries list object (ptr)
trashes
:r0-:r14, :f0-:f15
```

### :forget -> sys/mail/forget

```code
inputs
:r0 = service key str object (ptr)
trashes
:r0-:r14, :f0-:f15
```

### :free -> sys/mail/free

```code
inputs
:r0 = mail message (ptr)
trashes
:r0-:r14, :f0-:f15
info
only ever derefs a :str !
trashes
:r0-:r3
```

### :free_mbox -> sys/mail/free_mbox

```code
inputs
:r0 = mailbox id (uint)
trashes
:r0-:r4
```

### :free_obj -> sys/mail/free_obj

```code
inputs
:r0 = mail message (ptr)
outputs
:r0 = 0 if msg was 0, else object (ptr)
:r1 = data (pubyte)
:r2 = data length (bytes)
trashes
:r0-:r5
```

### :in -> sys/mail/in

```code
inputs
:r0 = link input buffer (ptr)
:r1 = link input ring buffer (ptr)
trashes
:r0-:r14, :f0-:f15
```

### :junk_mail -> sys/mail/junk_mail

```code
inputs
:r3 = mail list pointer (ptr)
trashes
:r0-:r2, :r4
```

### :mymail -> sys/mail/mymail

```code
outputs
:r0 = mail address (ptr)
:r1 = string data (pubyte)
trashes
:r0-:r2, :rsp
```

### :out -> sys/mail/out

```code
info
parcels going off chip or junk mail task
trashes
none
info
never breaks out
```

### :ping -> sys/mail/ping

```code
trashes
:r0-:r14, :f0-:f15
info
ping services out to network
```

### :poll -> sys/mail/poll

```code
inputs
:r0 = mailbox list object (ptr)
outputs
:r0 = -1, else mailbox index (uint)
:r4 = mailbox list begin iter (pptr)
:r5 = mailbox list end iter (pptr)
trashes
:r0-:r6
```

### :read -> sys/mail/read

```code
inputs
:r0 = mailbox address (ptr)
outputs
:r0 = mail address (ptr)
:r1 = string data (pubyte)
trashes
:r0-:r2, :rsp
info
in *build_mode* > 0
a signal to a Lisp task can return 0 here !
:lisp_read method is aware and returns :nil.
a VP level task should never see this happen !
```

### :ready -> sys/mail/ready

```code
inputs
:r0-:r1 = peer node id (node_id)
:r2 = key node object (ptr)
outputs
:r0 = 0 if none, else msg (ptr)
trashes
:r0-:r14, :f0-:f15
```

### :select -> sys/mail/select

```code
inputs
:r0 = mailbox id array object (ptr)
outputs
:r0 = mailbox index (uint)
trashes
:r0-:r8
```

### :send -> sys/mail/send

```code
inputs
:r0 = mail message (ptr)
trashes
:r0-:r14, :f0-:f15
info
only ever derefs a :str !
trashes
:r0-:r4
```

### :service -> sys/mail/service

```code
inputs
:r0 = service name str object (ptr)
:r1 = mailbox id str object (ptr)
:r2 = service info str object (ptr)
outputs
:r0 = service entry str object (ptr)
trashes
:r0-:r14, :f0-:f15
```

### :statics_init -> sys/mail/statics_init

```code
info
init the mailbox system, heap, buckets and id
trashes
:r0-:r3
```

### :statics_init1 -> sys/mail/statics_init1

```code
info
init the mail system
trashes
:r0-:r7, :r14, :f0-:f15
```

### :validate -> sys/mail/validate

```code
inputs
:r0 = mailbox id (uint)
outputs
:r0 = 0, else mailbox address (ptr)
trashes
:r0-:r3
```

