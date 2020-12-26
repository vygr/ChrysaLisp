;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; client_ipc - Client IPC library
; Wrapper to generalize client ipc functions
; for registering and deregistering itself
; from server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/ipc/ipc.inc")

; client_ipc derives from ipc to provide
; registering itself as a ipc source to ipc sink

(defclass client-ipc (client server) (ipc client server)
  (def this
    :registered  nil)

  (defmethod :register (this)
    ; (. client_ipc :register) -> t | exception
    (bind '(client cmd msg)
      (.
        (. this :send ipc_event_register)
        :recieve))
    (print ":register response cmd " cmd " data " msg)
    (if (= cmd ipc_event_success)
      (set this :registered t)
      (throw "Unable to register client " msg))
    this)

  (defmethod :deregister (this)
    ; (. client_ipc :deregister) -> t | exception
    (bind '(client cmd msg)
      (.
        (. this :send ipc_event_deregister)
        :recieve))
    (print ":deregister response cmd " cmd " data " msg)
    (if (= cmd ipc_event_success)
      (set this :registered nil)
      (throw "Unable to deregister client " msg))
    this)
  )

(class-pred client-ipc)
