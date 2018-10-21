;import system settings
(include 'sys/lisp.inc)

;open farm of children, create multicast msg
(defq ids (open-farm "tests/global_child" (mul (kernel-total) 3) kn_call_open)
	msg (apply array (map (lambda (_) (mul _ long_size)) (range 0 (div (mul lk_data_size 10) long_size)))))

(while (defq mbox (pop ids))
	(if (ne mbox 0) (mail-send msg mbox)))
