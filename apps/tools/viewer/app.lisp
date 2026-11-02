(defq +app_name "Viewer" +state_filename "viewer.tre" +hex_width :nil
	+text_types ''(".md" ".txt")
	+file_types ''(".lisp" ".inc" ".vp" ".md" ".txt" ".tre" ".cwb"))

(import "./app_impl.lisp")
