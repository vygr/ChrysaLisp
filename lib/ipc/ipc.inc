;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ipc - interprocess communication library
; Wrapper to generalize cross process and
; service communications
;
; Optimizes messaging depending on residence
; between source and sinks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "lib/ipc/ipc_defs.inc")
(import "lib/xchange/msg-data.inc")

(defun _default_msg_handler (&rest data)
  ; (_default_msg_handler client command data) -> list
  ; Default handler just returns data tuple
  data)

; ipc is the baseline class that performs
; the heavy lifting of communication between
; two endpoints:
; source - considered the client mailbox for receiving messages
; sink - considered the server mailbox to send message to

(defclass ipc (&optional source sink) nil
  (def this
       :source      source
       :sink        sink
       :optimized   (node= source sink))

  (defmethod :get_source (this)
    ; (. ipc :get_source) -> mailbox
    (get :source this))

  (defmethod :set_source (this source)
    (set this
      :source     source
      :optimized  (node= source (get :sink this))))

  (defmethod :get_sink (this)
    ; (. ipc :get_sink) -> mailbox
    (get :sink this))

  (defmethod :set_sink (this source)
    (set this
      :sink       sink
      :optimized  (node= (get :source this) sink)))

  (defmethod :optimized? (this)
    ; (. ipc :optimized) -> boolean
    (get :optimized this))

  (defmethod :send (this cmd &optional msg)
    ; (. ipc :send cmd [message-block]) -> ipc
    (when (or (nil? (get :source this)) (nil? (get :sink this)))
      (throw "IPC not configured with endpoints" (tolist this)))
    (defq em (xmap-kv
      :command cmd
      :client (to-service-id (. this :get_source))
      :data   msg))
    (mail-send
      (. this :get_sink)
      (cond
        ((. this :optimized?)
          (data-xlist em))
        (t
          (data-xliststr em))))
    this)

  (defmethod :recieve (this &optional handler_fn)
    ; (. ipc :receive [receipt handler]) -> result
    (when (or (nil? (get :source this)) (nil? (get :sink this)))
      (throw "IPC not configured with endpoints" (tolist this)))
    (setd handler_fn _default_msg_handler)
    (defq
      fmsg (mail-read (. this :get_source)))
    (defq
      res  (if (list? fmsg)
                (first (xlist-data fmsg))
                (first (xliststr-data fmsg))))
    (handler_fn
      (gets res :client)
      (gets res :command)
      (gets res :data)))
  )
