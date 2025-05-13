;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Word, Line, and Paragraph Counter for ChrysaLisp                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/options/options.inc")
(import "lib/text/charclass.inc") ; For +char_class_white_space

; --- Option Handlers & Usage ---

(defq
  *do_word_count* :nil
  *do_line_count* :nil
  *do_paragraph_count* :nil
)

(defq usage_text
"Usage: wordcount [options] [path]

  Counts words, lines, and/or paragraphs in a file or from stdin.
  If no count option is specified, all counts are performed.

  options:
    -h, --help:       This help information.
    -wc, --wordcount: Perform word count.
    -lc, --linecount: Perform line count.
    -pc, --paracount: Perform paragraph count."
)

(defq option_definitions `(
  (("-h" "--help") ,usage_text)
  (("-wc" "--wordcount")
    ,(lambda (remaining_args opt_str) (setq *do_word_count* :t) remaining_args)
  )
  (("-lc" "--linecount")
    ,(lambda (remaining_args opt_str) (setq *do_line_count* :t) remaining_args)
  )
  (("-pc" "--paracount")
    ,(lambda (remaining_args opt_str) (setq *do_paragraph_count* :t) remaining_args)
  )
))

; --- Core Logic ---

(defun process-stream (input_s)
  (defq
    word_c 0
    line_c 0
    paragraph_c 0
    in_para :nil
  )

  (each-line (lambda (current_l)
    (task-slice)
    (setq line_c (inc line_c))

    (if (empty? (trim current_l))
      (when in_para
        (setq paragraph_c (inc paragraph_c))
        (setq in_para :nil))
      (setq in_para :t))

    (when *do_word_count*
      (setq word_c (+ word_c (length (filter-array nempty? (split current_l))))))
    )
    input_s)

  (when in_para
    (setq paragraph_c (inc paragraph_c)))

  ; --- Output Results ---
  (defq output_str_parts (list))
  ; Push in the desired final print order - 'push' appends in ChrysaLisp.
  (when *do_line_count*      (push output_str_parts (str line_c " lines")))
  (when *do_word_count*      (push output_str_parts (str word_c " words")))
  (when *do_paragraph_count* (push output_str_parts (str paragraph_c " paragraphs")))
  ; Now output_str_parts is, e.g., ("L lines" "W words" "P paragraphs") if all flags are true.

  (if (nempty? output_str_parts)
    ; (join output_str_parts '(", ")) produces a flat list of strings:
    ;   ("L lines" ", " "W words" ", " "P paragraphs")
    ; (apply (const cat) ...) concatenates these into a single string.
    (print (apply (const cat) (join output_str_parts '(", "))))
    (print "No counts performed (select an option or provide input)."))
)

; --- Main Application Entry ---

(defun main ()
  (when (and
          (defq stdio_obj (create-stdio))
          (defq cmd_args (options stdio_obj option_definitions)))

    (unless (or *do_word_count* *do_line_count* *do_paragraph_count*)
      (setq *do_word_count* :t *do_line_count* :t *do_paragraph_count* :t))

    (if (empty? (rest cmd_args))
      (process-stream (io-stream 'stdin))
      (when (defq file_s (file-stream (first (rest cmd_args))))
        (process-stream file_s)
        )
    )
  )
)