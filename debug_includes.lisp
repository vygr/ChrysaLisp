(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/files/files.inc")

(defq +split_class (char-class " ()'	\q{@}<>")
	+implicit_file "lib/asm/func.inc"
	+class_prefixes ''(
		(":reals" "+reals_") (":real" "+real_")
		(":fixeds" "+fixeds_") (":fixed" "+fixed_")
		(":nums" "+nums_") (":num" "+num_")
		(":out" "+stream_mail_" "+out_") (":stream" "+stream_")
		(":lisp" "+lisp_") (":str" "+str_") (":obj" "+obj_")
		(":seq" "+seq_") (":array" "+array_") (":list" "+list_")
		(":dim" "+dim_") (":sys_math" "+math_")
		(":hset" "+hset_") (":hmap" "+hmap_") (":fstream" "+fstream_")
		(":sstream" "+sstream_") (":mstream" "+mstream_") (":in" "+in_")
		(":netid" "+netid_") (":nodeid" "+nodeid_") (":stdio" "+stdio_")
		(":error" "+error_") (":func" "+func_")
		(":sys_mail" "+net_id_" "+node_id_" "+mbox_" "+msg_")
		(":sys_list" "+ln_" "+lh_") (":sys_task" "+tk_") (":sys_kernel" "+kn_")
		(":host_os" "+stat_" "+file_open_" "+mmap_")
		(":host_audio" "+audio_") (":host_clipboard" "+clip_")
		(":view" "+view_") (":path" "+path_") (":canvas" "+canvas_")
		(":pixmap" "+pixmap_") (":vdu" "+vdu_") (":texture" "+texture_")
		(":font" "+font_") (":sys_heap" "+hp_") (":sys_mem" "+mem_")
		(":region" "rect_") (":sys_link" "lk_") (":sys_load" "ld_")
		(":sym" "static_sym_") (":host_gui" "+ev_")))

(defq defs_map (Fmap 11))
(each (lambda (file)
		(lines! (# (defq input (split %0 +split_class))
				(when (eql (first input) "def-class")
					(. defs_map :insert (second input) file)))
			(file-stream file)))
	(files-all "." '("class.inc") 2))

(print "File for :sys_task: " (. defs_map :find ":sys_task"))

(defq token "+tk_node_priority" classes (list))
(some (lambda (item)
	(if (some (# (starts-with %0 token)) (rest item))
		(progn (merge classes (list (first item))) :t))) +class_prefixes)

(print "Classes for " token ": " classes)
(defq cls (first classes))
(print "File for " cls ": " (. defs_map :find cls))

((ffi "service/gui/lisp_deinit"))
