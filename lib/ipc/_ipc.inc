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
(import "lib/ipc/ipcdefs.inc")

(defq
  _ipc_service nil)

(print *stream_name*)

(defun get-ipc-mb ()
  ; (get-logsrvc-mb) -> mailbox id | nil
  ; Retrieves the service mailbox for logging
  ; If not found in service registry, start
  ; the service. Otherwise return the mailbox id
  (cond
    ((nil? _ipc_service)
      (defq res (mail-enquire "IPC"))
      (setq _ipc_service
        (if (nempty? res)
            (to-net-id (second (split (first res) ",")))
            (open-child "lib/ipc/ipctest.lisp" kn_call_child))))
    (t _ipc_service)))

(defun _default_msg_handler (cmd msg)
  '(cmd msg))

(defclass _ipc (source sink) nil
  (def this
       :source    source
       :sink      sink
       :optimized optimized)

  (set this :optimized (node=? source sink))

  (defmethod :get_source (this)
    (get :source this))

  (defmethod :get_sink (this)
    (get :sink this))

  (defmethod :ping (this)
    (. :send ipc_event_ping))

  (defmethod :register (this)
    (. :send ipc_event_register (. )))

  (defmethod :optimized? (this)
    (get :optimized this))

  (defmethod :send (this cmd msg)
    (mail-send
      (. this :get_sink)
      (cond
        ((. this :optimized?)
          (insert msg
            '(char cmd long_size)))
      (t
        (cat
          (char cmd long_size)
          (str (data-xser msgs)))))))

  (defmethod :recieve (this &optional handler_fn)
    (setd handler_fn _default_msg_handler)
    (cond
      ((. this :optimized?)
        (defq fmsg (mail-read (. this :get_source)))
        (handler_fn (first fmsg) (rest fmsg)))
      (t
        )))
  )

(defun shutdown ()
  (mail-send
    (get-ipc-mb)
    (cat
      (char ipc_event_shutdown long_size)
      (task-mailbox))))
