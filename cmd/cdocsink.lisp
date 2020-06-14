;imports
(import 'class/lisp.inc)

(defq target "docs/COMMANDS.md")

(defun read-instream ()
  (defq
    siin (file-stream 'stdin)
    stf  (list))
  (while (defq c (read-line siin))
         (push stf c))
  (join stf (ascii-char 10)))

(defun wrap-block (cmd)
  (str "## " cmd
       (pad (ascii-char 10) 2) "```" (ascii-char 10)
       (read-instream)
       (ascii-char 10) "```"
       (ascii-char 10)))

(defun new-doc (cmd)
  (if (= (age target) 0)
      (save (wrap-block cmd) target)))

(defun append-doc (cmd)
  (if (/= (age target) 0)
      (let ((ostrm (file-stream target))
            (sstrm (string-stream (cat ""))))
        (while (defq ln (read-line ostrm))
               (write-line sstrm ln))
        (write sstrm (wrap-block cmd))
        (save (str sstrm) target))))

(defun make-command-doc (argv)
  (case (sym (second argv))
    (:new (new-doc (first argv)))
    (:extend (append-doc (first argv)))
    (t (throw
         "Directive not found. Expected :new or :append, found "
         sym))))


(defun main ()
  ;initialize pipe details and command args, abort on error
  (when (defq stdio (create-stdio))
    (make-command-doc (rest (stdio-get-args stdio)))))

