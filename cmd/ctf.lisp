;;;;;;;;;;;;;;;;;
; cmd/ctf.lisp
; the idea here is to have a common internal font_db database in .tre style.
; we load the entire font file into a buffer via (load) and use the (get-uint) etc
; functions to scan and read the info into the common font_db database.
; we can then output .ctf (via -c option) to upgrade old .ctf format files
; or convert existing .otf/.ttf files to .ctf format.
; CTF files store the glyph data as MoveTo/LineTo/CurveTo commands, with the numeric format
; scaled to fit 8.24 signed fixed point format.
;;;;;;;;;;;;;;;;;

(import "lib/options/options.inc")
(import "lib/debug/frames.inc")
(import "lib/collections/collections.inc")

(defq usage `(
(("-h" "--help")
"Usage: ctf [options] [file] ...

    options:
        -h --help: this help info.
        -v --verbosity num: verbosity level, default 0.
        -c --ctf: convert/upgrade font file to latest .ctf spec (planned).

    Inspects and outputs information about ChrysaLisp Vector Font (.ctf)
    or OpenType/TrueType (.otf/.ttf) files. If no files are specified on the
    command line, file paths are read from stdin.")
(("-c" "--ctf") ,(opt-flag 'opt_c))
(("-v" "--verbosity") ,(opt-num 'opt_v))
))

(defun format-fixed-24 (val)
	(defq sign "" abs_val val)
	(if (< val 0)
		(setq sign "-" abs_val (neg val)))
	(defq int_part (>> abs_val 24)
		frac_part (logand abs_val 0xffffff)
		frac_dec (/ (* frac_part 10000) 16777216))
	(cat sign (str int_part) "." (pad frac_dec 4 "0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Big-Endian Memory Getters (OTF)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-uint16-be (buf idx)
	(defq b0 (get-ubyte buf idx)
		b1 (get-ubyte buf (inc idx)))
	(+ (<< b0 8) b1))

(defun get-int16-be (buf idx)
	(defq val (get-uint16-be buf idx))
	(if (>= val 32768)
		(- val 65536)
		val))

(defun get-uint32-be (buf idx)
	(defq b0 (get-ubyte buf idx)
		b1 (get-ubyte buf (+ idx 1))
		b2 (get-ubyte buf (+ idx 2))
		b3 (get-ubyte buf (+ idx 3)))
	(+ (<< b0 24) (<< b1 16) (<< b2 8) b3))

(defun get-int32-be (buf idx)
	(defq val (get-uint32-be buf idx))
	(if (>= val 0x80000000)
		(- val 0x100000000)
		val))

(defun get-otf-name (buf tables name_id)
	(bind '(tbl_offset tbl_len) (. tables :find "name"))
	(if (and tbl_offset (> tbl_len 0))
		(progn
			(defq count (get-uint16-be buf (+ tbl_offset 2))
				string_offset (+ tbl_offset (get-uint16-be buf (+ tbl_offset 4)))
				record_offset (+ tbl_offset 6)
				found_str :nil)
			(times count
				(unless found_str
					(defq platform_id (get-uint16-be buf record_offset)
						encoding_id (get-uint16-be buf (+ record_offset 2))
						language_id (get-uint16-be buf (+ record_offset 4))
						curr_name_id (get-uint16-be buf (+ record_offset 6))
						length (get-uint16-be buf (+ record_offset 8))
						offset (get-uint16-be buf (+ record_offset 10)))
					(if (= curr_name_id name_id)
						(progn
							(defq str_ptr (+ string_offset offset))
							; Platforms 0 (Unicode) and 3 (Windows) use UTF-16BE
							(if (or (= platform_id 0) (= platform_id 3))
								(progn
									; Decode UTF-16BE to ASCII simply by skipping the first byte of each pair (since ASCII fits in the lower byte)
									(defq decoded (str-alloc (/ length 2))
										di 0)
									(times (/ length 2)
										(set-byte decoded di (get-ubyte buf (+ str_ptr (* di 2) 1)))
										(++ di))
									(setq found_str decoded))
								; Platform 1 (Mac) uses single-byte encodings (usually ASCII compatible)
								(if (= platform_id 1)
									(setq found_str (get-str buf str_ptr length))))))
					(setq record_offset (+ record_offset 12))))
			found_str)
		:nil))

(defun get-otf-glyph-index (buf tables char_code)
	(bind '(tbl_offset tbl_len) (. tables :find "cmap"))
	(if (and tbl_offset (> tbl_len 0))
		(progn
			(defq num_tables (get-uint16-be buf (+ tbl_offset 2))
				subtable_offset :nil
				record_offset (+ tbl_offset 4))
			; Find Platform 3, Encoding 1 (Windows Unicode BMP), Encoding 10 (UCS-4), or Platform 0 (Unicode)
			(times num_tables
				(unless subtable_offset
					(defq platform_id (get-uint16-be buf record_offset)
						encoding_id (get-uint16-be buf (+ record_offset 2))
						offset (get-uint32-be buf (+ record_offset 4)))
					(if (or (and (= platform_id 3) (or (= encoding_id 1) (= encoding_id 10)))
							(= platform_id 0))
						(setq subtable_offset (+ tbl_offset offset)))
					(setq record_offset (+ record_offset 8))))
			(if subtable_offset
				(progn
					(defq format (get-uint16-be buf subtable_offset))
					(cond
						((= format 4)
							(defq seg_count (/ (get-uint16-be buf (+ subtable_offset 6)) 2)
								end_count_offset (+ subtable_offset 14)
								start_count_offset (+ end_count_offset (* seg_count 2) 2)
								id_delta_offset (+ start_count_offset (* seg_count 2))
								id_range_offset (+ id_delta_offset (* seg_count 2))
								seg_idx 0
								found_g_idx 0)
							(times seg_count
								(unless (/= found_g_idx 0)
									(defq end_code (get-uint16-be buf (+ end_count_offset (* seg_idx 2))))
									(if (>= end_code char_code)
										(progn
											(defq start_code (get-uint16-be buf (+ start_count_offset (* seg_idx 2))))
											(if (<= start_code char_code)
												(progn
													(defq id_range (get-uint16-be buf (+ id_range_offset (* seg_idx 2))))
													(if (= id_range 0)
														(setq found_g_idx (logand (+ char_code (get-int16-be buf (+ id_delta_offset (* seg_idx 2)))) 0xffff))
														(progn
															(defq glyph_addr (+ id_range_offset (* seg_idx 2) id_range (* (- char_code start_code) 2)))
															(setq found_g_idx (get-uint16-be buf glyph_addr))
															(if (/= found_g_idx 0)
																(setq found_g_idx (logand (+ found_g_idx (get-int16-be buf (+ id_delta_offset (* seg_idx 2)))) 0xffff)))))))
											(setq found_g_idx (or found_g_idx -1)))))
								(setq seg_idx (inc seg_idx)))
							(if (= found_g_idx -1) 0 found_g_idx))
						(0)))))
		0))

(defun get-otf-advance (buf tables num_h_metrics g_index)
	(bind '(offset len) (. tables :find "hmtx"))
	(if (and offset (> len 0))
		(progn
			(if (< g_index num_h_metrics)
				(get-uint16-be buf (+ offset (* g_index 4)))
				(get-uint16-be buf (+ offset (* (dec num_h_metrics) 4)))))
		0))

(defun get-ttf-glyph-offset-and-len (buf tables index_to_loc_format g_index)
	(bind '(loca_offset loca_len) (. tables :find "loca"))
	(bind '(glyf_offset glyf_len) (. tables :find "glyf"))
	(if (and loca_offset glyf_offset)
		(progn
			(if (= index_to_loc_format 0)
				(defq offset (* (get-uint16-be buf (+ loca_offset (* g_index 2))) 2)
					next_offset (* (get-uint16-be buf (+ loca_offset (* (inc g_index) 2))) 2))
				(defq offset (get-uint32-be buf (+ loca_offset (* g_index 4)))
					next_offset (get-uint32-be buf (+ loca_offset (* (inc g_index) 4)))))
			(if (= offset next_offset)
				(list 0 0)
				(list (+ glyf_offset offset) (- next_offset offset))))
		(list 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Little-Endian Memory Getters (CTF)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-uint16-le (buf idx)
	(defq b0 (get-ubyte buf idx)
		b1 (get-ubyte buf (inc idx)))
	(+ b0 (<< b1 8)))

(defun get-uint32-le (buf idx)
	(defq b0 (get-ubyte buf idx)
		b1 (get-ubyte buf (+ idx 1))
		b2 (get-ubyte buf (+ idx 2))
		b3 (get-ubyte buf (+ idx 3)))
	(+ b0 (<< b1 8) (<< b2 16) (<< b3 24)))

(defun get-int32-le (buf idx)
	(defq val (get-uint32-le buf idx))
	(if (>= val 0x80000000)
		(- val 0x100000000)
		val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; OpenType Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-otf-tables (buf)
	(defq version (get-uint32-be buf 0)
		num_tables (get-uint16-be buf 4)
		tables (Lmap)
		offset 12)
	(times num_tables
		(defq tag (get-str buf offset 4)
			checksum (get-uint32-be buf (+ offset 4))
			tbl_offset (get-uint32-be buf (+ offset 8))
			len (get-uint32-be buf (+ offset 12)))
		(. tables :insert tag (list tbl_offset len))
		(setq offset (+ offset 16)))
	(scatter (Lmap) :version version :tables tables))

(defun get-otf-head (buf tables)
	(bind '(offset len) (. tables :find "head"))
	(get-uint16-be buf (+ offset 18)))

(defun get-otf-hhea (buf tables)
	(bind '(offset len) (. tables :find "hhea"))
	(defq ascender (get-int16-be buf (+ offset 4))
		descender (get-int16-be buf (+ offset 6))
		num_h_metrics (get-uint16-be buf (+ offset 34)))
	(list ascender descender num_h_metrics))

(defun get-otf-maxp (buf tables)
	(bind '(offset len) (. tables :find "maxp"))
	(get-uint16-be buf (+ offset 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TrueType Outlines Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-ttf-glyph (buf glyf_offset len scale_factor)
	(defq number_of_contours (get-int16-be buf glyf_offset)
		min_x (get-int16-be buf (+ glyf_offset 2))
		min_y (get-int16-be buf (+ glyf_offset 4))
		max_x (get-int16-be buf (+ glyf_offset 6))
		max_y (get-int16-be buf (+ glyf_offset 8))
		neg_scale_factor (neg scale_factor)
		commands (list))
	(if (> number_of_contours 0)
		(progn
			; Read endPtsOfContours
			(defq end_pts (list)
				idx (+ glyf_offset 10))
			(times number_of_contours
				(push end_pts (get-uint16-be buf idx))
				(setq idx (+ idx 2)))
			(defq num_points (inc (last end_pts))
				instr_len (get-uint16-be buf idx))
			(setq idx (+ idx 2 instr_len))
			
			; Read flags
			(defq flags (str-alloc num_points)
				fi 0)
			(while (< fi num_points)
				(defq f (get-ubyte buf idx))
				(setq idx (inc idx))
				(set-byte flags fi f)
				(++ fi)
				(if (/= (logand f 8) 0)
					(progn
						(defq repeat_count (get-ubyte buf idx))
						(setq idx (inc idx))
						(times repeat_count
							(set-byte flags fi f)
							(++ fi)))))
			
			; Read X coordinates
			(defq coords_x (nums)
				curr_x 0
				fi 0)
			(times num_points
				(defq f (get-ubyte flags fi))
				(cond
					((/= (logand f 2) 0)
						(defq dx (get-ubyte buf idx))
						(setq idx (inc idx))
						(if (= (logand f 16) 0) (setq dx (neg dx)))
						(setq curr_x (+ curr_x dx)))
					((= (logand f 16) 0)
						(defq dx (get-int16-be buf idx))
						(setq idx (+ idx 2))
						(setq curr_x (+ curr_x dx))))
				(push coords_x curr_x)
				(++ fi))
			
			; Read Y coordinates
			(defq coords_y (nums)
				curr_y 0
				fi 0)
			(times num_points
				(defq f (get-ubyte flags fi))
				(cond
					((/= (logand f 4) 0)
						(defq dy (get-ubyte buf idx))
						(setq idx (inc idx))
						(if (= (logand f 32) 0) (setq dy (neg dy)))
						(setq curr_y (+ curr_y dy)))
					((= (logand f 32) 0)
						(defq dy (get-int16-be buf idx))
						(setq idx (+ idx 2))
						(setq curr_y (+ curr_y dy))))
				(push coords_y curr_y)
				(++ fi))
			
			; Convert TTF contours to CTF commands
			(defq start_idx 0
				contour_idx 0)
			(times number_of_contours
				(defq end_idx (elem-get end_pts contour_idx)
					c_num_points (inc (- end_idx start_idx))
					c_points (list)
					pi start_idx)
				(times c_num_points
					(push c_points (list (elem-get coords_x pi) (elem-get coords_y pi) (/= (logand (get-ubyte flags pi) 1) 0)))
					(++ pi))
				
				; Resolve starting point
				(defq resolved_points (list))
				(if (third (first c_points))
					(setq resolved_points c_points)
					(progn
						; Rotate or adjust so we start with an on-curve point
						(defq first_on_idx (some! (# (if (third %0) (!))) c_points))
						(if first_on_idx
							(setq resolved_points (cat (slice c_points first_on_idx -1) (slice c_points 0 first_on_idx)))
							(progn
								; All points are off-curve
								(defq p0 (last c_points)
									pi 0)
								(times c_num_points
									(defq p1 (elem-get c_points pi)
										mx (/ (+ (first p0) (first p1)) 2)
										my (/ (+ (second p0) (second p1)) 2))
									(push resolved_points (list mx my :t) p1)
									(setq p0 p1)
									(++ pi))))))
				
				; Now process the resolved points
				(when (nempty? resolved_points)
					; Add closing point
					(push resolved_points (first resolved_points))
					(defq p0 (first resolved_points)
						p1x (* (first p0) scale_factor)
						p1y (* (second p0) neg_scale_factor))
					(push commands (list 0 p1x p1y))
					
					(defq r_len (length resolved_points)
						ri 1)
					(while (< ri r_len)
						(defq p1 (elem-get resolved_points ri))
						(if (third p1)
							; Next is on-curve -> LineTo
							(progn
								(push commands (list 1 (* (first p1) scale_factor) (* (second p1) neg_scale_factor)))
								(setq p0 p1)
								(++ ri))
							; Next is off-curve (Control point for Quadratic spline)
							(progn
								(defq p2 (elem-get resolved_points (inc ri)))
								(if (third p2)
									(setq ri (+ ri 2)))
								(unless (third p2)
									(setq p2 (list (/ (+ (first p1) (first p2)) 2) (/ (+ (second p1) (second p2)) 2) :t))
									(setq ri (inc ri)))
								; Convert Quadratic (p0, p1, p2) to Cubic (p0, q1, q2, p2)
								(defq p0x (* (first p0) scale_factor)
									p0y (* (second p0) neg_scale_factor)
									p1x (* (first p1) scale_factor)
									p1y (* (second p1) neg_scale_factor)
									p2x (* (first p2) scale_factor)
									p2y (* (second p2) neg_scale_factor)
									q1x (+ p0x (/ (* (- p1x p0x) 2) 3))
									q1y (+ p0y (/ (* (- p1y p0y) 2) 3))
									q2x (+ p2x (/ (* (- p1x p2x) 2) 3))
									q2y (+ p2y (/ (* (- p1y p2y) 2) 3)))
								(if (and (= p0x q1x) (= p0y q1y) (= q2x p2x) (= q2y p2y))
									(push commands (list 1 p2x p2y))
									(push commands (list 2 q1x q1y q2x q2y p2x p2y)))
								(setq p0 p2)))))
				(setq start_idx (inc end_idx)
					contour_idx (inc contour_idx)))
			(defq temp_min_y min_y)
			(setq min_x (* min_x scale_factor)
				max_x (* max_x scale_factor)
				min_y (* max_y neg_scale_factor)
				max_y (* temp_min_y neg_scale_factor))))
	(scatter (Lmap)
		:advance 0
		:min_x min_x
		:max_x max_x
		:min_y min_y
		:max_y max_y
		:commands commands))

;;;;;;;;;;;;;;;;;;;;;;
; CTF Parser
;;;;;;;;;;;;;;;;;;;;;;

(defun load-ctf-buf (buf)
	(defq ascent (get-uint32-le buf 0)
		descent (get-uint32-le buf 4)
		pages (list)
		pages_info (list)
		offset 8
		running :t)
	(defq font_db (scatter (Lmap)
		:file :nil
		:type "CTF"
		:ascent ascent
		:descent descent))
	(while running
		(defq pend (get-uint32-le buf offset))
		(setq offset (+ offset 4))
		(if (or (= pend 0) (= pend -1))
			(setq running :nil)
			(progn
				(defq pstart (get-uint32-le buf offset)
					count (inc (- pend pstart))
					offsets (list))
				(setq offset (+ offset 4))
				(times count
					(push offsets (get-uint32-le buf offset))
					(setq offset (+ offset 4)))
				(push pages_info (list pstart pend offsets)))))
	(each (lambda ((start end offsets))
		(defq glyphs (list)
			page_db (scatter (Lmap)
				:start start
				:end end)
			c start)
		(each (lambda (glyph_offset)
			(defq width (get-uint32-le buf glyph_offset)
				len (get-uint32-le buf (+ glyph_offset 4))
				min_x 0 max_x 0 min_y 0 max_y 0
				commands (list)
				g_offset (+ glyph_offset 8))
			(if (> len 0)
				(progn
					(defq end_g_offset (+ g_offset len)
						coords_x (list) coords_y (list))
					(while (< g_offset end_g_offset)
						(defq type (get-int32-le buf g_offset))
						(setq g_offset (+ g_offset 4))
						(cond
							((or (= type 0) (= type 1))
								(defq rx (get-int32-le buf g_offset)
									ry (get-int32-le buf (+ g_offset 4)))
								(setq g_offset (+ g_offset 8))
								(push coords_x rx)
								(push coords_y ry)
								(push commands (list type rx ry)))
							((= type 2)
								(defq rx1 (get-int32-le buf g_offset)
									ry1 (get-int32-le buf (+ g_offset 4))
									rx2 (get-int32-le buf (+ g_offset 8))
									ry2 (get-int32-le buf (+ g_offset 12))
									rx (get-int32-le buf (+ g_offset 16))
									ry (get-int32-le buf (+ g_offset 20)))
								(setq g_offset (+ g_offset 24))
								(push coords_x rx1 rx2 rx)
								(push coords_y ry1 ry2 ry)
								(push commands (list type rx1 ry1 rx2 ry2 rx ry)))))
					(setq min_x (reduce min coords_x (first coords_x))
						max_x (reduce max coords_x (first coords_x))
						min_y (reduce min coords_y (first coords_y))
						max_y (reduce max coords_y (first coords_y)))))
			(push glyphs (scatter (Lmap)
				:char_code c
				:offset glyph_offset
				:advance width
				:min_x min_x
				:max_x max_x
				:min_y min_y
				:max_y max_y
				:commands commands))
			(++ c))
			offsets)
		(push pages (scatter page_db :glyphs glyphs)))
		pages_info)
	(scatter font_db :pages pages))

(defun load-ctf (file)
	(if (defq buf (load file))
		(scatter (load-ctf-buf buf) :file file)
		:nil))

;;;;;;;;;;;;;;;;;;;;;;
; OTF/TTF Compiler
;;;;;;;;;;;;;;;;;;;;;;

(defun get-cff-offset (buf idx size)
	(cond
		((= size 1) (get-ubyte buf idx))
		((= size 2) (get-uint16-be buf idx))
		((= size 3) (+ (<< (get-ubyte buf idx) 16) (get-uint16-be buf (inc idx))))
		((get-uint32-be buf idx))))

(defun get-cff-index-end (buf idx)
	(defq count (get-uint16-be buf idx))
	(if (= count 0) (+ idx 2)
		(progn
			(defq off_size (get-ubyte buf (+ idx 2))
				last_off_addr (+ idx 3 (* count off_size))
				last_off (get-cff-offset buf last_off_addr off_size))
			(+ last_off_addr off_size last_off -1))))

(defun get-cff-index-item (buf idx i)
	(defq count (get-uint16-be buf idx))
	(if (and (> count 0) (< i count))
		(progn
			(defq off_size (get-ubyte buf (+ idx 2))
				off_addr (+ idx 3 (* i off_size))
				o1 (get-cff-offset buf off_addr off_size)
				o2 (get-cff-offset buf (+ off_addr off_size) off_size)
				data_start (+ idx 3 (* (inc count) off_size) -1))
			(list (+ data_start o1) (- o2 o1)))
		(list 0 0)))

(defun get-cff-charstrings-offset (buf dict_addr dict_len)
	(defq idx dict_addr end (+ dict_addr dict_len) stack (list) res 0)
	(while (< idx end)
		(defq b (get-ubyte buf idx))
		(cond
			((<= 32 b 246)
				(push stack (- b 139))
				(++ idx))
			((<= 247 b 250)
				(push stack (+ (* (- b 247) 256) (get-ubyte buf (inc idx)) 108))
				(setq idx (+ idx 2)))
			((<= 251 b 254)
				(push stack (- (neg (* (- b 251) 256)) (get-ubyte buf (inc idx)) 108))
				(setq idx (+ idx 2)))
			((= b 28)
				(push stack (get-int16-be buf (inc idx)))
				(setq idx (+ idx 3)))
			((= b 29)
				(push stack (get-int32-be buf (inc idx)))
				(setq idx (+ idx 5)))
			((= b 30)
				(setq idx (inc idx))
				(while (and (< idx end) (/= (logand (get-ubyte buf idx) 0xf) 0xf))
					(setq idx (inc idx)))
				(setq idx (inc idx)))
			((= b 12)
				(setq idx (+ idx 2) stack (clear stack)))
			(:t
				(if (= b 17)
					(setq res (pop stack) idx end)
					(setq stack (clear stack)))
				(setq idx (inc idx)))))
	res)

(defun parse-cff-glyph (buf g_offset len scale_factor)
	(defq idx g_offset end (+ g_offset len)
		cx (n2r 0) cy (n2r 0) commands (list) stack (list)
		r_scale (n2r scale_factor) num_stems 0)
	(defq push-curveto (lambda (x1 y1 x2 y2 x3 y3)
		(if (and (= cx x1) (= cy y1) (= x2 x3) (= y2 y3))
			(push commands (list 1 (n2i (* x3 r_scale)) (n2i (* y3 r_scale))))
			(push commands (list 2 (n2i (* x1 r_scale)) (n2i (* y1 r_scale))
				(n2i (* x2 r_scale)) (n2i (* y2 r_scale))
				(n2i (* x3 r_scale)) (n2i (* y3 r_scale)))))))
	(while (< idx end)
		(defq b (get-ubyte buf idx))
		(cond
			((<= 32 b 246)
				(push stack (n2r (- b 139)))
				(setq idx (inc idx)))
			((<= 247 b 250)
				(push stack (n2r (+ (* (- b 247) 256) (get-ubyte buf (inc idx)) 108)))
				(setq idx (+ idx 2)))
			((<= 251 b 254)
				(push stack (n2r (- (neg (* (- b 251) 256)) (get-ubyte buf (inc idx)) 108)))
				(setq idx (+ idx 2)))
			((= b 28)
				(push stack (n2r (get-int16-be buf (inc idx))))
				(setq idx (+ idx 3)))
			((= b 255)
				(push stack (/ (n2r (get-int32-be buf (inc idx))) (n2r 65536)))
				(setq idx (+ idx 5)))
			((= b 12)
				(setq idx (+ idx 2)))
			(:t
				(setq idx (inc idx))
				(cond
					((find b '(1 3 18 23))
						(++ num_stems (/ (length stack) 2))
						(setq stack (clear stack)))
					((find b '(19 20))
						(++ num_stems (/ (length stack) 2))
						(defq mask_bytes (/ (+ num_stems 7) 8))
						(setq idx (+ idx mask_bytes) stack (clear stack)))
					((find b '(4 21 22))
						(if (odd? (length stack)) (setq stack (rest stack)))
						(cond
							((= b 4) (bind '(dy) stack) (-- cy dy))
							((= b 22) (bind '(dx) stack) (++ cx dx))
							((bind '(dx dy) stack) (++ cx dx) (-- cy dy)))
						(push commands (list 0 (n2i (* cx r_scale)) (n2i (* cy r_scale))))
						(setq stack (clear stack)))
					((= b 5)
						(each (lambda ((dx dy))
							(++ cx dx) (-- cy dy)
							(push commands (list 1 (n2i (* cx r_scale)) (n2i (* cy r_scale)))))
							(partition stack 2))
						(setq stack (clear stack)))
					((find b '(6 7))
						(defq horiz (= b 6))
						(each (lambda (val)
							(if horiz (++ cx val) (-- cy val))
							(push commands (list 1 (n2i (* cx r_scale)) (n2i (* cy r_scale))))
							(setq horiz (not horiz))) stack)
						(setq stack (clear stack)))
					((= b 8)
						(each (lambda ((dxa dya dxb dyb dxc dyc))
							(defq x1 (+ cx dxa) y1 (- cy dya)
								x2 (+ x1 dxb) y2 (- y1 dyb)
								x3 (+ x2 dxc) y3 (- y2 dyc))
							(push-curveto x1 y1 x2 y2 x3 y3)
							(setq cx x3 cy y3)) (partition stack 6))
						(setq stack (clear stack)))
					((find b '(26 27))
						(defq curves (partition (if (odd? (length stack)) (rest stack) stack) 4)
							first_curve (first curves))
						(if (odd? (length stack))
							(progn
								(if (= b 26)
									(setq first_curve (cat (list (first stack)) first_curve))
									(setq first_curve (cat (slice first_curve 0 2) (list (first stack)) (slice first_curve 2 -1))))
								(setq curves (cat (list first_curve) (rest curves)))))
						(each (lambda (c)
							(if (= b 26)
								(bind '(dxa dya dyb dyc &optional dxb) c)
								(bind '(dxa dya dxb dyc &optional dyc_ignored) c))
							(setd dxb (n2r 0))
							(defq x1 (+ cx dxa) y1 (- cy dya)
								x2 (+ x1 dxb) y2 (- y1 dyb)
								x3 (+ x2 (if (= b 26) (n2r 0) dyc)) y3 (- y2 (if (= b 26) dyc (n2r 0))))
							(push-curveto x1 y1 x2 y2 x3 y3)
							(setq cx x3 cy y3)) curves)
						(setq stack (clear stack)))
					((find b '(30 31))
						(defq h (eql b 31) i 0 sp (length stack))
						(while (< (+ i 3) sp)
							(defq x1 (n2r 0) y1 (n2r 0) x2 (n2r 0) y2 (n2r 0) x3 (n2r 0) y3 (n2r 0)
								extra (if (= (- sp i) 5) (elem-get stack (+ i 4)) (n2r 0)))
							(if h
								(progn
									(setq x1 (+ cx (elem-get stack i))
										y1 cy
										x2 (+ x1 (elem-get stack (+ i 1)))
										y2 (- y1 (elem-get stack (+ i 2)))
										y3 (- y2 (elem-get stack (+ i 3)))
										x3 (+ x2 extra)))
								(progn
									(setq x1 cx
										y1 (- cy (elem-get stack i))
										x2 (+ x1 (elem-get stack (+ i 1)))
										y2 (- y1 (elem-get stack (+ i 2)))
										x3 (+ x2 (elem-get stack (+ i 3)))
										y3 (- y2 extra))))
							(push-curveto x1 y1 x2 y2 x3 y3)
							(setq cx x3 cy y3
								h (not h)
								i (+ i (if (= (- sp i) 5) 5 4))))
						(setq stack (clear stack)))
					((= b 24)
						(defq len (length stack) num_lines (- len 6) i 0)
						(while (< i num_lines)
							(++ cx (elem-get stack i))
							(-- cy (elem-get stack (+ i 1)))
							(push commands (list 1 (n2i (* cx r_scale)) (n2i (* cy r_scale))))
							(setq i (+ i 2)))
						(defq x1 (+ cx (elem-get stack i))
							y1 (- cy (elem-get stack (+ i 1)))
							x2 (+ x1 (elem-get stack (+ i 2)))
							y2 (- y1 (elem-get stack (+ i 3)))
							x3 (+ x2 (elem-get stack (+ i 4)))
							y3 (- y2 (elem-get stack (+ i 5))))
						(push-curveto x1 y1 x2 y2 x3 y3)
						(setq cx x3 cy y3
							stack (clear stack)))
					((= b 25)
						(defq num_curves (- (length stack) 2) i 0)
						(while (< i num_curves)
							(defq x1 (+ cx (elem-get stack i))
								y1 (- cy (elem-get stack (+ i 1)))
								x2 (+ x1 (elem-get stack (+ i 2)))
								y2 (- y1 (elem-get stack (+ i 3)))
								x3 (+ x2 (elem-get stack (+ i 4)))
								y3 (- y2 (elem-get stack (+ i 5))))
							(push-curveto x1 y1 x2 y2 x3 y3)
							(setq cx x3 cy y3
								i (+ i 6)))
						(defq dxa (elem-get stack i) dyb (elem-get stack (inc i)))
						(++ cx dxa) (-- cy dyb)
						(push commands (list 1 (n2i (* cx r_scale)) (n2i (* cy r_scale))))
						(setq stack (clear stack)))
					((= b 26)
						(defq i 0 sp (length stack))
						(vpif (odd? sp))
							(++ cx (elem-get stack 0))
							(setq i 1)
						(endif)
						(while (< i sp)
							(defq dy1 (elem-get stack i)
								dx2 (elem-get stack (+ i 1))
								dy2 (elem-get stack (+ i 2))
								dy3 (elem-get stack (+ i 3))
								c1x cx
								c1y (- cy dy1)
								c2x (+ c1x dx2)
								c2y (- c1y dy2)
								x3 c2x
								y3 (- c2y dy3))
							(push-curveto c1x c1y c2x c2y x3 y3)
							(setq cx x3 cy y3
								i (+ i 4)))
						(setq stack (clear stack)))
					((= b 27)
						(defq i 0 sp (length stack))
						(vpif (odd? sp))
							(-- cy (elem-get stack 0))
							(setq i 1)
						(endif)
						(while (< i sp)
							(defq dx1 (elem-get stack i)
								dx2 (elem-get stack (+ i 1))
								dy2 (elem-get stack (+ i 2))
								dx3 (elem-get stack (+ i 3))
								c1x (+ cx dx1)
								c1y cy
								c2x (+ c1x dx2)
								c2y (- c1y dy2)
								x3 (+ c2x dx3)
								y3 c2y)
							(push-curveto c1x c1y c2x c2y x3 y3)
							(setq cx x3 cy y3
								i (+ i 4)))
						(setq stack (clear stack)))
					((= b 14)
						(setq idx end))
					(:t (setq stack (clear stack)))))))
	(defq min_x 0 max_x 0 min_y 0 max_y 0)
	(when (nempty? commands)
		(defq coords_x (map (const second) commands)
			coords_y (map (const third) commands)
			min_x (reduce min coords_x)
			max_x (reduce max coords_x)
			min_y (reduce min coords_y)
			max_y (reduce max coords_y)))
	(scatter (Lmap)
		:advance 0
		:min_x min_x
		:max_x max_x
		:min_y min_y
		:max_y max_y
		:commands commands))

(defun load-otf-ttf-buf (buf file)
	(defq otf_db (parse-otf-tables buf)
		version (. otf_db :find :version)
		tables (. otf_db :find :tables))
	; Read basic metrics
	(defq units_per_em (get-otf-head buf tables))
	(bind '(ascender descender num_h_metrics) (get-otf-hhea buf tables))
	(defq num_glyphs (get-otf-maxp buf tables)
		scale_factor (/ 16777216 units_per_em)
		ascent (* ascender scale_factor)
		descent (neg (* descender scale_factor))
		is_cff (eql version 0x4f54544f)
		index_to_loc_format (if is_cff 0 (get-uint16-be buf (+ (first (. tables :find "head")) 50))))
	
	; If CFF, locate the CharStrings index
	(defq charstrings_idx :nil)
	(when is_cff
		(bind '(cff_offset cff_len) (. tables :find "CFF "))
		(defq hdr_size (get-ubyte buf (+ cff_offset 2))
			name_idx (+ cff_offset hdr_size)
			top_dict_idx (get-cff-index-end buf name_idx))
		(bind '(top_dict_addr top_dict_len) (get-cff-index-item buf top_dict_idx 0))
		(setq charstrings_idx (+ cff_offset (get-cff-charstrings-offset buf top_dict_addr top_dict_len))))
	
	(defq active_chars (list))
	(bind '(tbl_offset tbl_len) (. tables :find "cmap"))
	(if (and tbl_offset (> tbl_len 0))
		(progn
			(defq num_tables (get-uint16-be buf (+ tbl_offset 2))
				subtable_offset :nil
				record_offset (+ tbl_offset 4))
			; Find Platform 3, Encoding 1 (Windows Unicode BMP), Encoding 10 (UCS-4), or Platform 0 (Unicode)
			(times num_tables
				(unless subtable_offset
					(defq platform_id (get-uint16-be buf record_offset)
						encoding_id (get-uint16-be buf (+ record_offset 2))
						offset (get-uint32-be buf (+ record_offset 4)))
					(if (or (and (= platform_id 3) (or (= encoding_id 1) (= encoding_id 10)))
							(= platform_id 0))
						(setq subtable_offset (+ tbl_offset offset)))
					(setq record_offset (+ record_offset 8))))
			(if subtable_offset
				(progn
					(defq format (get-uint16-be buf subtable_offset))
					(if (= format 4)
						(progn
							(defq seg_count (/ (get-uint16-be buf (+ subtable_offset 6)) 2)
								end_count_offset (+ subtable_offset 14)
								start_count_offset (+ end_count_offset (* seg_count 2) 2)
								seg_idx 0)
							(times (dec seg_count)
								(defq start (get-uint16-be buf (+ start_count_offset (* seg_idx 2))))
								(defq end (get-uint16-be buf (+ end_count_offset (* seg_idx 2))))
								(when (and (< start 0xf900) (<= start end))
									(defq end (min 0xf8ff end)
										c start)
									(while (<= c end)
										(if (> (get-otf-glyph-index buf tables c) 0)
											(push active_chars c))
										(++ c)))
								(setq seg_idx (inc seg_idx)))))))))
	
	(defq build_page (lambda (pstart pend)
		(defq glyphs (list) c pstart)
		(while (<= c pend)
			(defq g_index (get-otf-glyph-index buf tables c))
			(if (> g_index 0)
				(progn
					(defq glyph_db :nil)
					(if is_cff
						(progn
							(bind '(g_offset g_len) (get-cff-index-item buf charstrings_idx g_index))
							(if (and (> g_offset 0) (> g_len 0))
								(setq glyph_db (parse-cff-glyph buf g_offset g_len scale_factor))))
						(progn
							(bind '(g_offset g_len) (get-ttf-glyph-offset-and-len buf tables index_to_loc_format g_index))
							(if (and (> g_offset 0) (> g_len 0))
								(setq glyph_db (parse-ttf-glyph buf g_offset g_len scale_factor)))))
					(unless glyph_db
						(setq glyph_db (scatter (Lmap)
							:min_x 0 :max_x 0 :min_y 0 :max_y 0
							:commands (list))))
					(. glyph_db :insert :char_code c)
					(. glyph_db :insert :advance (- (. glyph_db :find :max_x) (. glyph_db :find :min_x)))
					(push glyphs glyph_db))
				(push glyphs (scatter (Lmap) :char_code c :offset 0 :advance 0 :min_x 0 :max_x 0 :min_y 0 :max_y 0 :commands (list))))
			(++ c))
		(scatter (Lmap) :start pstart :end pend :glyphs glyphs)))
		
	(defq pages (list))
	(when (nempty? active_chars)
		(defq pstart (first active_chars)
			pend pstart
			index 1)
		(while (< index (length active_chars))
			(defq c (elem-get active_chars index))
			(cond
				((> (- c pend) 32)
					(push pages (build_page pstart pend))
					(setq pstart c pend c))
				(:t (setq pend c)))
			(setq index (inc index)))
		(push pages (build_page pstart pend)))
		
	; Pre-calculate compiled byte offsets for print-font and serialization consistency
	(defq current_offset 8)
	(each (lambda (page_db)
		(defq start (. page_db :find :start)
			end (. page_db :find :end)
			count (inc (- end start)))
		(setq current_offset (+ current_offset 8 (* count 4))))
		pages)
	(setq current_offset (+ current_offset 4))
	
	(each (lambda (page_db)
		(defq glyphs (. page_db :find :glyphs))
		(each (lambda (glyph_db)
			(defq commands (. glyph_db :find :commands)
				len 0)
			(each (lambda (cmd)
				(defq type (first cmd))
				(if (= type 2)
					(setq len (+ len 28))
					(setq len (+ len 12))))
				commands)
			(. glyph_db :insert :offset current_offset)
			(setq current_offset (+ current_offset 8 len)))
			glyphs))
		pages)
		
	(scatter (Lmap)
		:file file
		:type "CTF"
		:ascent ascent
		:descent descent
		:pages pages))

(defun load-otf-ttf (file)
	(if (defq buf (load file))
		(scatter (load-otf-ttf-buf buf file) :file file)
		:nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Writer and Printer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-ctf (font_db file)
	(if (and font_db (defq stream (file-stream file +file_open_write)))
		(progn
			(defq ascent (. font_db :find :ascent)
				descent (. font_db :find :descent)
				pages (. font_db :find :pages))
			; Write header
			(write-char stream ascent +int_size)
			(write-char stream descent +int_size)
			
			; Calculate glyph offsets (Header is 8 bytes)
			(defq current_offset 8)
			(each (lambda (page_db)
				(defq start (. page_db :find :start)
					end (. page_db :find :end)
					count (inc (- end start)))
				(setq current_offset (+ current_offset 8 (* count 4))))
				pages)
			; Add 4 bytes for the sentinel (0)
			(setq current_offset (+ current_offset 4))
			
			; Write the page tables and populate the offsets
			(each (lambda (page_db)
				(defq start (. page_db :find :start)
					end (. page_db :find :end)
					glyphs (. page_db :find :glyphs))
				(write-char stream end +int_size)
				(write-char stream start +int_size)
				(each (lambda (glyph_db)
					(defq commands (. glyph_db :find :commands)
						len 0)
					(each (lambda (cmd)
						(defq type (first cmd))
						(if (= type 2)
							(setq len (+ len 28))
							(setq len (+ len 12))))
						commands)
					(. glyph_db :insert :offset current_offset)
					(write-char stream current_offset +int_size)
					(setq current_offset (+ current_offset 8 len)))
					glyphs))
				pages)
			
			; Write sentinel
			(write-char stream 0 +int_size)
			
			; Write the glyph data
			(each (lambda (page_db)
				(defq glyphs (. page_db :find :glyphs))
				(each (lambda (glyph_db)
					(defq width (. glyph_db :find :advance)
						commands (. glyph_db :find :commands)
						len 0)
					(each (lambda (cmd)
						(defq type (first cmd))
						(if (= type 2)
							(setq len (+ len 28))
							(setq len (+ len 12))))
						commands)
					(write-char stream width +int_size)
					(write-char stream len +int_size)
					(each (lambda (cmd)
						(defq type (first cmd))
						(write-char stream type +int_size)
						(if (= type 2)
							(progn
								(write-char stream (second cmd) +int_size)
								(write-char stream (third cmd) +int_size)
								(write-char stream (elem-get cmd 3) +int_size)
								(write-char stream (elem-get cmd 4) +int_size)
								(write-char stream (elem-get cmd 5) +int_size)
								(write-char stream (elem-get cmd 6) +int_size))
							(progn
								(write-char stream (second cmd) +int_size)
								(write-char stream (third cmd) +int_size))))
						commands))
					glyphs))
				pages)
			(stream-flush stream)
			:t)
		:nil))

(defun print-font (font_db verbosity)
	(when font_db
		(print "File: " (. font_db :find :file))
		(print "\tType: " (. font_db :find :type) " (ChrysaLisp Vector Font)")
		(print "\tAscent: " (format-fixed-24 (. font_db :find :ascent)))
		(print "\tDescent: " (format-fixed-24 (. font_db :find :descent)))
		(defq pages (. font_db :find :pages) total_glyphs 0)
		(each (lambda (page_db)
			(defq glyphs (. page_db :find :glyphs))
			(setq total_glyphs (+ total_glyphs (length glyphs))))
			pages)
		(print "\tTotal Pages: " (length pages))
		(each (lambda (page_db)
			(defq start (. page_db :find :start)
				end (. page_db :find :end)
				glyphs (. page_db :find :glyphs)
				count (length glyphs))
			(print "\tPage Range: " start " - " end " (Glyphs: " count ")")
			(when (> verbosity 0)
				(each (lambda (glyph_db)
					(defq c (. glyph_db :find :char_code)
						offset (. glyph_db :find :offset)
						width (. glyph_db :find :advance)
						min_x (. glyph_db :find :min_x)
						min_y (. glyph_db :find :min_y)
						max_x (. glyph_db :find :max_x)
						max_y (. glyph_db :find :max_y)
						commands (. glyph_db :find :commands))
					(defq g_width (- max_x min_x)
						g_height (- max_y min_y)
						char_str (if (<= 32 c 126) (cat "'" (char c) "'") "?"))
					(print "\t\tGlyph " char_str " (" c "): Offset: " offset
						"\n\t\t\tAdvance: " (format-fixed-24 width)
						"\n\t\t\tBounds: [" (format-fixed-24 min_x) ", " (format-fixed-24 min_y) "]"
						" to [" (format-fixed-24 max_x) ", " (format-fixed-24 max_y) "]"
						"\n\t\t\tSize: " (format-fixed-24 g_width) " x " (format-fixed-24 g_height))
					(when (and (> verbosity 1) (nempty? commands))
						(print "\t\t\tCommands:")
						(each (lambda (cmd)
							(defq type (first cmd))
							(cond
								((= type 0)
									(print "\t\t\t\tMoveto: [" (format-fixed-24 (second cmd)) ", " (format-fixed-24 (third cmd)) "]"))
								((= type 1)
									(print "\t\t\t\tLineto: [" (format-fixed-24 (second cmd)) ", " (format-fixed-24 (third cmd)) "]"))
								((= type 2)
									(print "\t\t\t\tCurveto: [" (format-fixed-24 (second cmd)) ", " (format-fixed-24 (third cmd)) "]"
										" (Control: [" (format-fixed-24 (elem-get cmd 3)) ", " (format-fixed-24 (elem-get cmd 4)) "],"
										" [" (format-fixed-24 (elem-get cmd 5)) ", " (format-fixed-24 (elem-get cmd 6)) "])"))))
							commands)))
					glyphs)))
			pages)
		(print "\tTotal Glyphs: " total_glyphs)
		(print)))

(defun process-otf-ttf (file type verbosity)
	(if (defq buf (load file))
		(progn
			(defq otf_db (parse-otf-tables buf)
				version (. otf_db :find :version)
				tables (. otf_db :find :tables))
			(print "File: " file)
			(print "\tType: " type " (OpenType/TrueType Font)")
			(print "\tVersion: " (long-to-hex-str version))
			
			; Read and print naming information
			(defq family_name (get-otf-name buf tables 1)
				subfamily_name (get-otf-name buf tables 2)
				full_name (get-otf-name buf tables 4)
				ps_name (get-otf-name buf tables 6))
			(if family_name (print "\tFamily: " family_name))
			(if subfamily_name (print "\tSubfamily: " subfamily_name))
			(if full_name (print "\tFull Name: " full_name))
			(if ps_name (print "\tPostScript Name: " ps_name))

			; Read and print metrics
			(defq units_per_em (get-otf-head buf tables))
			(bind '(ascender descender num_h_metrics) (get-otf-hhea buf tables))
			(defq num_glyphs (get-otf-maxp buf tables))
			(print "\tUnitsPerEm: " units_per_em)
			(print "\tAscender: " ascender " (" (format-fixed-24 (/ (* ascender 16777216) units_per_em)) ")")
			(print "\tDescender: " descender " (" (format-fixed-24 (/ (* descender 16777216) units_per_em)) ")")
			(print "\tNumGlyphs: " num_glyphs)
			(print "\tNumberOfHMetrics: " num_h_metrics)
			
			(when (> verbosity 0)
				(print "\tTables:")
				(. tables :each (lambda (tag info)
					(bind '(offset len) info)
					(print "\t\t" tag " (Offset: " offset ", Length: " len ")"))))
			(print))
		(print "Error: Cannot open font file " file)))

(defun process-file (file verbosity)
	(cond
		((ends-with ".ctf" file)
			(if (defq font_db (load-ctf file))
				(print-font font_db verbosity)
				(print "Error: Cannot open font file " file)))
		((or (ends-with ".otf" file) (ends-with ".ttf" file))
			(process-otf-ttf file (if (ends-with ".otf" file) "OTF" "TTF") verbosity)
			(when (> verbosity 0)
				(print-font (load-otf-ttf file) verbosity)))
		(:t
			(print "Error: Unsupported font file format " file)
			(print))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_c :nil opt_v 0 args (options stdio usage)))
		(defq files (rest args))
		(if (empty? files)
			(progn
				(defq temp_list (list))
				(lines! (# (push temp_list %0)) (io-stream 'stdin))
				(setq files temp_list)))
		(if opt_c
			(each (lambda (file)
				(cond
					((ends-with ".ctf" file)
						(if (defq font_db (load-ctf file))
							(if (write-ctf font_db (cat file ".new"))
								(print "Wrote binary identical font: " (cat file ".new"))
								(print "Error: Failed to write " (cat file ".new")))
							(print "Error: Cannot open font file " file)))
					((or (ends-with ".otf" file) (ends-with ".ttf" file))
						(if (defq font_db (load-otf-ttf file))
							(if (write-ctf font_db (cat (slice file 0 (dec (rfind "." file))) ".new"))
								(print "Compiled and wrote font: " (cat (slice file 0 (dec (rfind "." file))) ".new"))
								(print "Error: Failed to write " (cat (slice file 0 (dec (rfind "." file))) ".new")))
							(print "Error: Cannot open font file " file)))
					(:t (print "Error: Unsupported font file format " file))))
				files)
			(each (# (process-file %0 opt_v)) files))))