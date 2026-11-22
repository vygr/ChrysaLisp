(import "lib/math/obj.inc")
(import "lib/net/http.inc")

;The fetching of a model, a task of its own, so the window goes on being
;drawn while it comes over the internet. Asked for an address, and the
;address of a picture for a model that names none, it fetches each file
;the model will ask for: the .obj, the .mtl files it names and the
;pictures they name, or the .zip alone, that has the rest in it. All are
;sent back in one message, each its address, its length and its bytes,
;or if the model itself could not be had, ! and why. Each file is kept
;in downloads/models/, by its address, and taken from there after

;why the last fetch had nothing
(defq *why* "")

(defun fetch (url)
	; (fetch url) -> :nil | str
	;kept from a fetch before, or fetched and kept
	(defq path (url-kept-path "downloads/models" url))
	(cond
		((defq data (load path)) data)
		((defq why (http-why (defq resp (http-get url)))) (setq *why* why) :nil)
		((defq data (http-body-str resp)) (save data path) data)))

(defun child-run (said)
	(defq reply (slice said 0 +net_id_size)
		ask (split (slice said +net_id_size -1) "\n")
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
						(cat url "\n" (str (length data)) "\n" data)) got)))))

(defun main ()
	;who started it may be gone before it says what is wanted
	(if (defq said (mail-read-timeout (task-mbox) 10000000)) (child-run said)))
