;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; client_ipc - Client IPC library
; Wrapper to generalize client ipc functions
; for registering and deregistering itself
; from server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/ipc/ipc.inc")

; client_ipc derives from ipc to provide
; registering itself as a ipc source to ipc sink

(defclass client-ipc (&optional client server) (ipc client server)
  (def this
    :registered       nil
    :registered_data  nil)

  (defmethod :register (this &optional msg)
    ; (. client_ipc :register) -> t | exception
    (bind '(client cmd msg)
      (.
        (. this :send :register msg)
        :recieve))
    (cond
      ((eql cmd :success)
        (set this :registered t)
        (set this :registered_data msg))
      (t
        (throw (str "Unable to register client " cmd )msg)))
    this)

  (defmethod :deregister (this)
    ; (. client_ipc :deregister) -> t | exception
    (bind '(client cmd msg)
      (.
        (. this :send :deregister)
        :recieve))
    (cond
      ((eql cmd :success)
        (set this :registered nil)
        (set this :registered_data nil))
      (t
        (throw (str "Unable to deregister client " cmd) msg)))
    this)
  )

