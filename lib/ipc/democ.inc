;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; democ - Demonstration client that extends
; client-ipc for pinging and shutdown
; Wrapper to generalize client ipc functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/ipc/client_ipc.inc")

(defq _ipc_service nil)

(defun ipc-test (node)
  ; (ipc-test) -> mailbox id | nil
  ; Retrieves the service mailbox for local ipc testing
  ; If not found in service registry, start
  ; the service. Otherwise return the mailbox id
  (cond
    ((nil? _ipc_service)
      (defq res (mail-enquire "IPC"))
      (setq _ipc_service
        (if (nempty? res)
            (to-net-id (second (split (first res) ",")))
            (open-remote
              "lib/ipc/demos.lisp"
              node
              +kn_call_open))))
    (t _ipc_service)))

(defclass democ (client server) (client-ipc client server)
  (defmethod :shutdown (this)
    ; (. democ :shutdown) -> nil
    ; This is for demo only
    (bind '(client cmd msg)
      (.
        (. this :send :shutdown (list))
        :recieve))
    (print ":shutdown response cmd " cmd " data " msg)
    this)

  (defmethod :ping (this)
    ; (. democ :ping) -> list
    (print "Client ping sending")
    (bind '(client cmd msg)
      (.
        (. this :send :ping)
        :recieve))
    (print ":ping cmd " cmd " data " msg)
    this)
  )

; For client side REPL Testing start Terminal with at least 2 CPU
; ./run_tui.sh 2


; To test co-resident (optimized) client/server
; >lisp lib/ipc/democ.inc
; (defq cipc (democ (mail-alloc-mbox) (ipc-test (local-node))))
; Threaded messages - Double entries are to see effect
; of registering something already registered, etc.
; Shutdown is for demo to tell the server to stop
; (.-> cipc :ping :register :register :deregister :deregister :shutdown)
; Clear this for retesting
; (setq _ipc_service nil)

; To test different CPU (serializing data) client/server
; >lisp lib/ipc/_ipc.inc
; (defq cipc (democ (mail-alloc-mbox) (ipc-test (remote-node))))
; Threaded messages
; (.-> cipc :ping :register :register :deregister :deregister :shutdown)
