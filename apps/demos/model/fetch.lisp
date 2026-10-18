(import "lib/math/obj.inc")
(import "lib/net/http.inc")

;The fetching of a model, a task of its own, so the window goes on being
;drawn while it comes over the internet. Asked for an address, and the
;address of a picture for a model that names none, it fetches each file
;the model will ask for: the .obj, the .mtl files it names and the
;pictures they name, or the .zip alone, that has the rest in it. All are
;sent back in one message, each its address, its length and its bytes,
;or if the model itself could not be had, ! and why

;why the last fetch had nothing
(defq *why* "")

(defun fetch (url)
	; (fetch url) -> :nil | str
	(defq resp (http-get url))
	(cond
		((not resp) (setq *why* (ifn (tls-why) "no connection")) :nil)
		((/= (pfind resp :status) 200)
			(setq *why* (cat (str (pfind resp :status)) " " (ifn (pfind resp :reason) ""))) :nil)
		((http-body-str resp))))

(defun main ()
	(defq msg (mail-read (task-mbox)) reply (slice msg 0 +net_id_size)
		ask (split (slice msg +net_id_size -1) (ascii-char 10))
		path (first ask) picture (if (> (length ask) 1) (second ask) "") got (list)
		;each address the once
		take (lambda (url)
			(unless (some (# (eql (first %0) url)) got)
				(if (defq data (fetch url)) (push got (list url data)))
				data)))
	(when (defq text (take path))
		(unless (ends-with ".zip" path)
			;(rfind) gives the place after the slash, the folder is up to it
			(defq folder (slice path 0 (ifn (rfind "/" path) 0)))
			(lines! (lambda (line)
					(when (eql (first (defq words (split (trim line) (const (char-class " \t\r"))))) "mtllib")
						(each! (lambda (library)
								(when (defq mtl (take (cat folder library)))
									(. (mtl-read (string-stream mtl)) :each (lambda (k v)
										(when (defq file (. v :find :map))
											;a picture's name may have a folder of another
											;machine in it
											(take (cat folder (last (split file (const (char-class "/\\")))))))))))
							(list words) 1 -1))
					:nil)
				(string-stream text)))
		(if (nempty? picture) (take picture)))
	(mail-send reply (if (empty? got) (cat "!" *why*)
		(apply (const cat) (map (lambda ((url data))
			(cat url (ascii-char 10) (str (length data)) (ascii-char 10) data)) got)))))
