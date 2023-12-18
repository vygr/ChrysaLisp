# sys_mail

## Lisp Bindings

### (mail-alloc-mbox)

### (mail-declare mbox name info)

### (mail-nodes)

### (mail-enquire prefix)

### (mail-forget key)

### (mail-free-mbox mbox)

### (mail-poll mboxs)

### (mail-read mbox)

### (mail-select mboxs)

### (mail-send mbox obj)

### (mail-timeout mbox ns id)

### (mail-validate mbox)

## VP methods

### :alloc -> sys/mail/alloc

```code
inputs
:r0 = mail size (bytes)
outputs
:r0 = mail message (ptr)
:r1 = string data (pubyte)
trashes
:r0-:r6
```

### :alloc_mbox -> sys/mail/alloc_mbox

```code
outputs
:r0 = mailbox id (uint)
:r1 = mailbox address (ptr)
trashes
:r0-:r5
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
:r0-:r5
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
:r0-:r14
```

### :devices -> sys/mail/devices

```code
outputs
:r0 = known network nodes list object (ptr)
trashes
:r0-:r14
```

### :enquire -> sys/mail/enquire

```code
inputs
:r0 = service prefix str object (ptr)
outputs
:r0 = matching service entries list object (ptr)
trashes
:r0-:r14
```

### :forget -> sys/mail/forget

```code
inputs
:r0 = service key str object (ptr)
trashes
:r0-:r14
```

### :free -> sys/mail/free

```code
inputs
:r0 = mail message (ptr)
trashes
:r0-:r14
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
:r0 = link input msg buffer (ptr)
trashes
:r0-:r14
```

### :junk_mail -> sys/mail/junk_mail

```code
inputs
:r3 = mail list pointer (ptr)
trashes
:r0-:r4
```

### :mymail -> sys/mail/mymail

```code
outputs
:r0 = mail address (ptr)
:r1 = string data (pubyte)
trashes
:r0-:r2
```

### :out -> sys/mail/out

```code
info
parcels going off chip or junk mail task
```

### :ping -> sys/mail/ping

```code
trashes
:r0-:r14
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
:r0-:r2
```

### :ready -> sys/mail/ready

```code
inputs
:r0-:r1 = peer node id (node_id)
:r2 = key node object (ptr)
outputs
:r0 = 0 if none, else msg (ptr)
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
:r0-:r14
```

### :statics_init -> sys/mail/statics_init

```code
info
init the mailbox system, heap, buckets and id
```

### :statics_init1 -> sys/mail/statics_init1

```code
info
init the mail system
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

