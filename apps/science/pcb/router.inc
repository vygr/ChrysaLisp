(import "./app.inc")
(import "./layer.inc")

;native, apps/science/pcb/lisp.vp
(ffi (cat *app_root* "flood_near") flood-near)
; (flood-near ctx node flood_vecs via_vecs radius gap via out_n out_v) -> out_n
(ffi (cat *app_root* "flood_cost") flood-cost)
(ffi (cat *app_root* "flood_wave") flood-wave)
; (flood-wave ctx starts ends flood_vecs via_vecs radius gap via vias_cost mode) -> :t
; (flood-cost ctx node flood_vecs via_vecs radius gap via out_n out_v out_nd out_vd) -> out_n

;module
(env-push)

(defq +via_vectors (push `(,quote)
	(list (list (nums 0 0 -1) (nums 0 0 1)) (list (nums 0 0 -1) (nums 0 0 1))))
	+spacial_hash_res 1.0 +path_scale 1 +node_sets ''()
	;rounds of the negotiating router at most, and how many with no fewer
	;nets in conflict than its best before it stops; how much dearer a
	;short with a routed track is each round
	+negotiate_rounds 100 +negotiate_stall 10 +pf_start 0.5 +pf_rise 1.5 +pf_most 10000.0
	;times the nets left, and the nets in their way, are negotiated again
	;on their own, the rest of the board kept
	+local_rounds 4)

(defun alloc-node-set ()
	(unless (defq ns (pop +node_sets))
		(defq ns (Fset 11))) ns)

(defun free-node-set (ns)
	(push +node_sets (. ns :empty)))

(defun gen-vectors (vec_range x_range y_range)
	;generate range of routing vectors
	(defq out (list) vec_range (n2f vec_range) y (neg y_range))
	(while (<= y y_range)
		(defq x (neg x_range))
		(while (<= x x_range)
			(defq l (vector-length (Vec2-f (n2f x) (n2f y))))
			(and (> l 0.1) (<= l vec_range)
				(push out (nums x y 0)))
			(++ x))
		(++ y))
	out)

(defun aabb-pads (pads)
	;aabb of pads
	(defq minxyz (Vec2-f 1000000.0 1000000.0)
		maxxyz (Vec2-f -1000000.0 -1000000.0))
	(each (lambda (p)
		(vector-min minxyz (setq p (. p :get_pos)) minxyz)
		(vector-max maxxyz p maxxyz)) pads)
	(bind '(minx miny) minxyz)
	(bind '(maxx maxy) maxxyz)
	(list (* (- maxx minx) (- maxy miny))
		(list minx miny maxx maxy)))

(defun delay-node (delays at node)
	;a node to join the flood at a distance to come
	(unless (defq ns (. delays :find at))
		(. delays :insert at (defq ns (alloc-node-set))))
	(. ns :insert node))

(defun sort-netlist (netlist)
	(sort netlist (# (- (. %0 :get_area) (. %1 :get_area)))))

(defclass Pad (radius gap pos shape) :nil
	; (Pad radius gap pos shape) -> pad
	(def this :radius radius :gap gap :pos pos :shape shape)
	(defgetmethod :radius)
	(defsetmethod :radius)
	(defgetmethod :gap)
	(defsetmethod :gap)
	(defgetmethod :pos)
	(defsetmethod :pos)
	(defgetmethod :shape)
	(defsetmethod :shape))

(defclass Track (id track_radius via_radius gap pads wires) :nil
	; (Track id track_radius via_radius gap pads wires) -> track
	(def this :id id :track_radius track_radius :via_radius via_radius :gap gap
		:pads pads :wires wires)
	(defgetmethod :id)
	(defgetmethod :track_radius)
	(defgetmethod :via_radius)
	(defgetmethod :gap)
	(defgetmethod :pads)
	(defgetmethod :wires))

(defclass Net (track pcb) :nil
	; (Net track pcb) -> net
	(defq grid_res (n2f (. pcb :get_grid_res)))
	;the pcb by a weak ref, claimed where it is used: the pcb holds its
	;nets, a net that held it by a count would hold it round
	(def this
		:pcb (weak-ref pcb) :id (. track :get_id)
		:radius (* (. track :get_track_radius) grid_res)
		:via (defq via (* (. track :get_via_radius) grid_res))
		:gap (* (. track :get_gap) grid_res)
		:pads (defq pads (. track :get_pads))
		:wires (defq wires (. track :get_wires))
		:paths (defq paths (list)) :raw_paths (list) :routed :nil
		:paths_lines (list)
		:pad_lines (defq pad_lines (list))
		:pad_end_nodes (defq pad_end_nodes (list))
		:wire_lines (defq wire_lines (list))
		:wire_nodes (defq wire_nodes (alloc-node-set)))

	;scale pads and wires for resolution of grid
	(each (lambda (p)
		(. p :set_radius (* (. p :get_radius) grid_res))
		(. p :set_gap (* (. p :get_gap) grid_res))
		(bind '(x y z) (. p :get_pos))
		(. p :set_pos (Vec3-f (* x grid_res) (* y grid_res) z))
		(each (lambda (p)
			(vector-scale p grid_res p)) (. p :get_shape))) pads)
	(each (lambda (wire)
		(each (lambda (p)
			(bind '(x y z) p)
			(elem-set wire (!) (Vec3-f (* x grid_res) (* y grid_res) z))) wire)) wires)

	;sort pads
	(sort pads (lambda (p1 p2)
		(bind '(x1 y1 z1) (. p1 :get_pos))
		(bind '(x2 y2 z2) (. p2 :get_pos))
		(if (= (defq s (- x1 x2)) 0)
			(if (= (defq s (- y1 y2)) 0)
				(defq s (- z1 z2)))) s))

	;build pad collision lines and endpoint nodes
	(defq i 0)
	(while (< i (length pads))
		(defq p (elem-get pads i)
			pos (. p :get_pos)
			radius (. p :get_radius)
			gap (. p :get_gap)
			shape (. p :get_shape)
			xy (slice pos +vec3_x +vec3_z))
		(bind '(x y z1) pos)
		(defq z2 z1)
		;traverse pad stack
		(while (and (< (++ i) (length pads))
					(eql xy (slice (defq pos (. (defq p (elem-get pads i)) :get_pos)) +vec3_x +vec3_z))
					(= radius (. p :get_radius))
					(= gap (. p :get_gap))
					(every (const eql) shape (. p :get_shape)))
			(setq z2 (elem-get pos +vec3_z)))
		;collision lines
		(cond
			((= (length shape) 0)
				(push pad_lines (Layers-line x y z1 x y z2 radius gap)))
			(:t (defq z z1)
				(while (<= z z2)
					(bind '(xs ys) (first shape))
					(defq cx x cy y cz z
						p0x :nil p0y :nil p0z :nil
						p1x (+ x xs) p1y (+ y ys) p1z cz)
					(each! (lambda ((xs ys))
						;add pad entries to via only spacial cache
						(.-> pcb :get_via_layers (:add_layers_line (Layers-line cx cy cz p1x p1y p1z radius gap)))
						;pad lines
						(setq p0x p1x p0y p1y p0z p1z p1x (+ x xs) p1y (+ y ys) p1z cz)
						(push pad_lines (Layers-line p0x p0y p0z p1x p1y p1z radius gap))) (list shape) 1)
					(setq z (+ z 1.0)))))
		;ends and deformations
		(push pad_end_nodes (defq ends (list)))
		(defq z z1)
		(while (<= z z2)
			(push ends (. pcb :pad_point_to_node (Vec3-f x y z)))
			(setq z (+ z 1.0))))

	;build wires collision lines and visited
	(each (lambda (wire)
		(defq p0 :nil p1 (first wire))
		(each! (lambda (p)
			(setq p0 p1 p1 p)
			(cond
				((/= (elem-get p0 +vec3_z) (elem-get p1 +vec3_z))
					;via wire
					(bind '(x0 y0 z0) p0)
					(defq depth (n2f (dec (. pcb :get_depth))) z 0.0)
					(push wire_lines (Layers-line x0 y0 0.0 x0 y0 depth via gap))
					(while (<= z depth)
						(. wire_nodes :insert (. pcb :pad_point_to_node (Vec3-f x0 y0 z)))
						(setq z (+ z 1.0))))
				(:t ;layer wire
					(bind '(x0 y0 z0) p0)
					(bind '(x1 y1 z1) p1)
					(push wire_lines (Layers-line x0 y0 z0 x1 y1 z1 radius gap))
					(defq p (Vec2-f x0 y0) v (vector-sub p (Vec2-f x1 y1))
						l (vector-length v) norm (vector-scale v (recip l))
						i 0.0)
					(while (< i l)
						(bind '(x y) (vector-add p (vector-scale norm i)))
						(. wire_nodes :insert (. pcb :pad_point_to_node (Vec3-f x y z0)))
						(setq i (+ i 0.25)))
					(. wire_nodes :insert (. pcb :pad_point_to_node p1))))) (list wire) 1)) wires)

	;add pad entries to via only spacial cache
	(each (# (.-> pcb :get_via_layers (:add_layers_line %0))) pad_lines)

	;bounds
	(bind '(area bbox) (aabb-pads pads))
	(def this :area area :bbox bbox)

	(defgetmethod :radius)
	(defgetmethod :pads)
	(defgetmethod :area)
	(defgetmethod :id)

	(defmethod :add_pad_lines ()
		; (. net :add_pad_lines) -> net
		;add pad entries to spacial cache
		(defq pcb (obj-ref (get :pcb this)))
		(each (# (.-> pcb :get_layers (:add_layers_line %0))) (get :pad_lines this))
		this)

	(defmethod :sub_pad_lines ()
		; (. net :sub_pad_lines) -> net
		;remove pad entries from spacial cache
		(defq pcb (obj-ref (get :pcb this)))
		(each (# (.-> pcb :get_layers (:sub_layers_line %0))) (get :pad_lines this))
		this)

	(defmethod :add_wire_lines ()
		; (. net :add_wire_lines) -> net
		;add wire entries to spacial cache
		(defq pcb (obj-ref (get :pcb this)))
		(each (# (.-> pcb :get_layers (:add_layers_line %0))) (get :wire_lines this))
		this)

	(defmethod :sub_wire_lines ()
		; (. net :sub_wire_lines) -> net
		;remove wire entries from spacial cache
		(defq pcb (obj-ref (get :pcb this)))
		(each (# (.-> pcb :get_layers (:sub_layers_line %0))) (get :wire_lines this))
		this)

	(defmethod :create_paths_lines ()
		; (. net :create_paths_lines) -> lines
		;create paths collision lines
		(defq pcb (obj-ref (get :pcb this))) (raise :radius :via :gap)
		(defq depth (n2f (dec (. pcb :get_depth))))
		(reduce (lambda (lines pth)
			(defq p0 :nil p1 (. pcb :node_to_pad_point (first pth)))
			(each! (lambda (p)
				(setq p0 p1 p1 (. pcb :node_to_pad_point p))
				(bind '(x0 y0 z0) p0)
				(bind '(x1 y1 z1) p1)
				(if (/= z0 z1)
					(push lines (Layers-line x0 y0 0.0 x0 y0 depth via gap))
					(push lines (Layers-line x0 y0 z0 x1 y1 z1 radius gap)))) (list pth) 1)
			lines) (get :paths this) (list)))

	(defmethod :add_paths_lines ()
		; (. net :add_paths_lines) -> net
		;add paths entries to spacial cache
		(defq pcb (obj-ref (get :pcb this)))
		(defq paths_lines (. this :create_paths_lines))
		(lower :paths_lines)
		(each (# (.-> pcb :path_layers (:add_layers_line %0))) paths_lines)
		this)

	(defmethod :sub_paths_lines ()
		; (. net :sub_paths_lines) -> net
		;remove paths entries from spacial cache
		(defq pcb (obj-ref (get :pcb this))) (raise :paths_lines)
		(each (# (.-> pcb :path_layers (:sub_layers_line %0))) paths_lines)
		(clear paths_lines)
		this)

	(defmethod :remove ()
		; (. net :remove) -> net
		;remove net entries from spacial grid
		(.-> this
			:sub_paths_lines
			:sub_wire_lines
			:sub_pad_lines
			:add_pad_lines
			:add_wire_lines)
		;and it is not routed, till it is again
		(clear (get :paths this) (get :raw_paths this))
		(set this :routed :nil)
		this)

	(defmethod :optimise_paths (paths)
		; (. net :optimise_paths paths) -> opt_paths
		;remove redundant points from paths
		(defq pcb (obj-ref (get :pcb this)))
		(reduce (lambda (opt_paths pth)
			(defq opt_path (list)
				d0 (Vec3-f 0.0 0.0 0.0) d1 :nil
				p0 :nil p1 (. pcb :node_to_pad_point (first pth)))
			(each! (lambda (p)
				(setq p0 p1 p1 (. pcb :node_to_pad_point p)
					d1 (vector-norm (vector-sub p1 p0)))
				(unless (eql d0 d1)
					(push opt_path (elem-get pth (dec (!))))
					(setq d0 d1))) (list pth) 1)
			(push opt_path (last pth))
			(push opt_paths opt_path)) paths (list)))

	(defmethod :backtrack_path (visited end_node radius via gap)
		; (. net :backtrack_path this visited end_node radius via gap) -> nodes
		;backtrack path from ends to starts
		(defq pcb (obj-ref (get :pcb this))) (raise (path_vectors (. pcb :get_path_vectors)
			nearer_nodes '() pth (list) path_node end_node exit :nil))
		(until exit
			(task-slice)
			(push pth path_node)
			(clear nearer_nodes)
			(each (# (push nearer_nodes %0))
				(. pcb :all_not_shorting
					(. pcb :all_marked path_vectors path_node)
					path_node radius gap))
			(each (# (push nearer_nodes %0))
				(. pcb :all_not_shorting_via
					(. pcb :all_marked +via_vectors path_node)
					path_node via gap))
			(cond
				((= (length nearer_nodes) 0)
					;gone empty
					(setq exit (list)))
				((defq n (some (# (. visited :find %0)) nearer_nodes))
					;found existing track
					(setq exit (push pth n)))
				((get :negotiating pcb)
					;the step the flood took here: the least of a mark and
					;what the step from it cost, its delay, and a via's cost
					(setq path_node (. this :back_step path_node nearer_nodes radius via gap)))
				(:t ;sort nodes and take the first
					(setq path_node (second (elem-get
						(sort
							(map (# (list (vector-squared-euclidean-distance path_node %0) %0))
							(filter (# (= (. pcb :get_node %0) (. pcb :get_node (first nearer_nodes))))
							(sort nearer_nodes (# (- (. pcb :get_node %0) (. pcb :get_node %1))))))
							(# (- (first %0) (first %1)))) 0))))))
		exit)

	(defmethod :back_step (node nears radius via gap)
		; (. net :back_step node nears radius via gap) -> node
		;of the marked nodes round a node, the one the flood came from: the
		;least of its mark, the step, the delay of the step as the flood had
		;it, (. pcb :step_delay), and the cost of a via, the nearest of
		;those as much
		(defq pcb (obj-ref (get :pcb this)) soft (get :soft_layers pcb)
			depth (n2f (dec (get :depth pcb))) np (. pcb :node_to_pad_point node)
			best :nil best_cost 0 best_dist 0)
		(each (lambda (n)
				(defq nnp (. pcb :node_to_pad_point n)
					cost (+ (. pcb :get_node n) 1
						(if (= (elem-get n +vec3_z) (elem-get node +vec3_z))
							(. pcb :step_delay node (. soft :count_line nnp np radius gap))
							(+ (get :vias_cost pcb) (. pcb :step_delay node
								(. soft :count_line (push (slice nnp +vec3_x +vec3_z) depth)
									(push (slice np +vec3_x +vec3_z) 0.0) via gap)))))
					dist (vector-squared-euclidean-distance node n))
				(when (or (not best) (< cost best_cost) (and (= cost best_cost) (< dist best_dist)))
					(setq best n best_cost cost best_dist dist)))
			nears)
		best)

	(defmethod :route ()
		; (. net :route) -> :t | :nil
		;attempt to route this net on the current boards state
		(defq pcb (obj-ref (get :pcb this))) (raise :paths :radius :gap :via :pad_end_nodes)
		(cond
			((= radius 0.0) :t)
			(:t (clear paths) (clear (get :raw_paths this))
					(.-> this
					:sub_pad_lines
					:sub_wire_lines)
				(defq visited (. (get :wire_nodes this) :copy)
					failed (some! (lambda (ends)
						(each (# (. visited :insert %0)) (elem-get pad_end_nodes (dec (!))))
						(unless (some (# (. visited :find %0)) ends)
							(. pcb :mark_distances visited ends radius gap via)
							(sort ends (# (- (. pcb :get_node %0) (. pcb :get_node %1))))
							(defq nodes (. this :backtrack_path visited (first ends) radius via gap))
							(. pcb :unmark_distances)
							(cond
								((/= (length nodes) 0)
									(each (# (. visited :insert %0)) nodes)
									(push paths nodes) (push (get :raw_paths this) nodes) :nil)
								(:t)))) (list pad_end_nodes) :nil 1))
				(cond
					(failed
						(. this :remove) :nil)
					(:t (setq paths (. this :optimise_paths paths))
						(lower :paths)
						(.-> this
							:add_paths_lines
							:add_pad_lines
							:add_wire_lines)
						:t)))))

	(defmethod :print_net (stream)
		; (. net :print_net stream) -> net
		;output net, pads, wires and paths, for viewer app
		(defq pcb (obj-ref (get :pcb this))) (raise :pads :wires :paths :id :radius :via :gap
			(scale (recip (n2f (. pcb :get_grid_res))) verbosity (. pcb :get_verbosity)))
		(if (>= verbosity 2) (write-line stream ";NET"))
		(write-line stream "(")
		(if (>= verbosity 2) (write-line stream " ;id, radius, via, gap"))
		(write-line stream (str " " id " " (* radius scale) " " (* via scale) " " (* gap scale)))
		(if (>= verbosity 2) (write-line stream " ;PADS"))
		(write-line stream " (")
		(each (lambda (p)
				(if (>= verbosity 2) (write-line stream "  ;PAD"))
				(write-line stream "  (")
				(if (>= verbosity 2) (write-line stream "   ;radius, gap"))
				(write-line stream (str "   "
					(* (. p :get_radius) scale) " "
					(* (. p :get_gap) scale)))
				(if (>= verbosity 2) (write-line stream "   ;pos"))
				(bind '(x y z) (. p :get_pos))
				(write-line stream (str "   ("
					(* x scale) " " (* y scale) " " z ")"))
				(if (>= verbosity 2) (write-line stream "   ;shape"))
				(write-blk stream "   (")
				(each (lambda (p)
						(write-blk stream (str (vector-scale p scale))))
					(. p :get_shape))
				(write-line stream ")")
				(write-line stream "  )"))
			pads)
		(write-line stream " )")
		(if (>= verbosity 2) (write-line stream " ;WIRES"))
		(write-line stream " (")
		(each (lambda (wire)
				(if (>= verbosity 2) (write-line stream "  ;WIRE"))
				(write-blk stream "  (")
				(each (lambda (p)
						(bind '(x y z) p)
						(write-blk stream (str "(" (* x scale) " " (* y scale) " " z ")")))
					wire)
				(write-line stream ")"))
			wires)
		(write-line stream " )")
		(if (>= verbosity 2) (write-line stream " ;PATHS"))
		(write-line stream " (")
		(each (lambda (pth)
				(if (>= verbosity 2) (write-line stream "  ;PATH"))
				(write-blk stream "  (")
				(each (lambda (p)
						(bind '(x y z) (. pcb :node_to_pad_point p))
						(write-blk stream (str "(" (* x scale) " " (* y scale) " " z ")")))
					pth)
				(write-blk stream ")"))
			paths)
		(write-line stream " )")
		(write-line stream ")")
		this)
	)

(defclass Pcb (width height depth grid_res verb vias_cost fr fr_even fr_odd) :nil
	; (Pcb width height depth grid_res verb vias_cost fr fr_even fr_odd) -> pcb
	(def this
		:nodes_dim (dim (nums (* width grid_res) (* height grid_res) depth)
			(defq nodes (cap (* width height depth grid_res grid_res) (nums))))
		:width (* width grid_res) :height (* height grid_res) :depth depth
		:deform (Fmap 101) :grid_res grid_res
		;the points of :deform as the native flood reads them: of each node
		;an index into deform_pts, 0 if it has none, and the points
		:deform_idx (defq deform_idx (cap (* width height depth grid_res grid_res) (nums)))
		:deform_pts (fixeds) :ctx :nil
		;the negotiating router: the routed tracks are soft, in :soft_layers,
		;and a move that shorts with them costs, more where it has before,
		;:hist, and more each round, :pf
		:negotiating :nil :hist (defq hist (cap (* width height depth grid_res grid_res) (nums)))
		:pf (fixeds 0.5) :log (list) :trace :nil
		:flood_vectors (list
			(gen-vectors fr fr_even fr)
			(gen-vectors fr fr fr_odd))
		:path_vectors (list
			(gen-vectors (* fr +path_scale) (* fr_even +path_scale) (* fr +path_scale))
			(gen-vectors (* fr +path_scale) (* fr +path_scale) (* fr_odd +path_scale)))
		:netlist (list) :verbosity verb
		:vias_cost (* vias_cost grid_res)
		:layers (Layers (n2i (* (n2f width) +spacial_hash_res))
						(n2i (* (n2f height) +spacial_hash_res))
						depth (/ +spacial_hash_res (n2f grid_res)))
		:via_layers (Layers (n2i (* (n2f width) +spacial_hash_res))
						(n2i (* (n2f height) +spacial_hash_res))
						depth (/ +spacial_hash_res (n2f grid_res)))
		:nodes nodes)
	(times (* width height depth grid_res grid_res) (push nodes 0) (push deform_idx 0) (push hist 0))
	(def this :soft_layers (Layers (n2i (* (n2f width) +spacial_hash_res))
		(n2i (* (n2f height) +spacial_hash_res))
		depth (/ +spacial_hash_res (n2f grid_res))))

	(defgetmethod :depth)
	(defgetmethod :grid_res)
	(defgetmethod :layers)
	(defgetmethod :via_layers)
	(defgetmethod :verbosity)
	(defgetmethod :path_vectors)

	(defmethod :get_node (node)
		; (. pcb :get_node node) -> val
		;get grid node value
		(dim-get (get :nodes_dim this) node))

	(defmethod :set_node (node val)
		; (. pcb :set_node node val) -> pcb
		;set grid node to value
		(dim-set (get :nodes_dim this) node val)
		this)

	(defmethod :node_to_point ((x y z))
		; (. pcb :node_to_point node) -> point
		;convert node to point
		(Vec3-f (n2f x) (n2f y) (n2f z)))

	(defmethod :point_to_node ((x y z))
		; (. pcb :point_to_node point) -> node
		;convert point to node
		(nums (n2i (+ x 0.5)) (n2i (+ y 0.5)) (n2i z)))

	(defmethod :node_to_pad_point (node)
		; (. pcb :node_to_pad_point node) -> point
		;convert node to pad point
		(if (defq p (. (get :deform this) :find node))
			p (. this :node_to_point node)))

	(defmethod :pad_point_to_node (point)
		; (. pcb :pad_point_to_node point) -> node
		;convert pad point to node
		(. (get :deform this) :insert (defq n (. this :point_to_node point)) point)
		(bind '(x y z) n)
		(raise :deform_idx :deform_pts (i (+ x (* (get :width this) (+ y (* (get :height this) z))))))
		(cond
			((= (defq k (elem-get deform_idx i)) 0)
				(each (# (push deform_pts %0)) point)
				(elem-set deform_idx i (/ (length deform_pts) 3)))
			(:t (each (# (elem-set deform_pts (+ (* (dec k) 3) (!)) %0)) point)))
		n)

	(defmethod :flood_ctx ()
		; (. pcb :flood_ctx) -> ctx
		;what the native flood is given of the board, made once
		(unless (defq ctx (get :ctx this))
			(raise :nodes :deform_idx :deform_pts :width :height :depth)
			(defq layer (first (get :layers (get :layers this))))
			(set this :ctx (setq ctx (list nodes deform_idx deform_pts
				(map (# (get :buckets %0)) (get :layers (get :layers this)))
				(map (# (get :buckets %0)) (get :layers (get :via_layers this)))
				width height depth (get :width layer) (get :height layer) (get :scale layer) (nums 0)
				(map (# (get :buckets %0)) (get :layers (get :soft_layers this)))
				(get :hist this) (get :pf this)
				(defq stamp (cap (length nodes) (nums))) (nums 0) (nums) (nums) (nums))))
			(times (length nodes) (push stamp 0)))
		ctx)

	(defmethod :node_index ((x y z))
		; (. pcb :node_index node) -> num
		;the index of a node on the board
		(+ x (* (get :width this) (+ y (* (get :height this) z)))))

	(defmethod :path_layers ()
		; (. pcb :path_layers) -> layers
		;where routed tracks go: soft while negotiating
		(get (if (get :negotiating this) :soft_layers :layers) this))

	(defmethod :step_delay (node k)
		; (. pcb :step_delay node k) -> num
		;how many steps more than one a move to the node costs, shorting with
		;k soft lines: (1 + its history) * (1 + pf * k)
		(bind '(x y z) node)
		(defq h (elem-get (get :hist this) (+ x (* (get :width this) (+ y (* (get :height this) z))))))
		(dec (n2i (* (+ 1.0 (* (n2f k) (first (get :pf this)))) (n2f (inc h))))))

	(defmethod :flood_cost_lisp (node radius gap via out_n out_v out_nd out_vd)
		; (. pcb :flood_cost_lisp node radius gap via out_n out_v out_nd out_vd) -> out_n
		;the Lisp of what (flood-cost) does, kept to hold it to
		(raise :flood_vectors :soft_layers (depth (n2f (dec (get :depth this)))
			np (. this :node_to_pad_point node)))
		(each (lambda (n)
				(push out_n n)
				(push out_nd (. this :step_delay n
					(. soft_layers :count_line np (. this :node_to_pad_point n) radius gap))))
			(. this :all_not_shorting (. this :all_not_marked flood_vectors node) node radius gap))
		(each (lambda (n)
				(push out_v n)
				(push out_vd (. this :step_delay n
					(. soft_layers :count_line (push (slice np +vec3_x +vec3_z) depth)
						(push (slice (. this :node_to_pad_point n) +vec3_x +vec3_z) 0.0) via gap))))
			(. this :all_not_shorting_via (. this :all_not_marked +via_vectors node) node via gap))
		out_n)

	(defmethod :flood_near_lisp (node radius gap via out_n out_v)
		; (. pcb :flood_near_lisp node radius gap via out_n out_v) -> out_n
		;the Lisp of what (flood-near) does, kept to hold it to
		(raise :flood_vectors)
		(each (# (push out_n %0)) (. this :all_not_shorting
			(. this :all_not_marked flood_vectors node) node radius gap))
		(each (# (push out_v %0)) (. this :all_not_shorting_via
			(. this :all_not_marked +via_vectors node) node via gap))
		out_n)

	(defmethod :all_not_marked (vec node)
		; (. pcb :all_not_marked vec node) -> nodes
		;generate all grid points surrounding node, that are value 0
		(defq out (clear '())
			vecm (nums (dec (get :width this)) (dec (get :height this)) (dec (get :depth this))))
		(each (lambda (v)
			(and (eql (vector-min vecm (vector-max (defq n (vector-add node v)) +nums_zero3 +nums_tmp3) +nums_tmp3) n)
					(= (. this :get_node n) 0)
				(push out n))) (elem-get vec (% (elem-get node +vec3_z) 2)))
		out)

	(defmethod :all_marked (vec node)
		; (. pcb :all_marked vec node) -> nodes
		;generate all grid points surrounding node, that are not value 0, but less than node
		(defq out (clear '()) d (. this :get_node node)
			vecm (nums (dec (get :width this)) (dec (get :height this)) (dec (get :depth this))))
		(each (lambda (v)
			(and (eql (vector-min vecm (vector-max (defq n (vector-add node v)) +nums_zero3 +nums_tmp3) +nums_tmp3) n)
					(< 0 (. this :get_node n) d)
				(push out n))) (elem-get vec (% (elem-get node +vec3_z) 2)))
		out)

	(defmethod :all_not_shorting (gather node radius gap)
		; (. pcb :all_not_shorting gather node radius gap) -> nodes
		;generate all grid points surrounding node that are not shorting with an existing track
		(raise :layers (out (clear '()) np (. this :node_to_pad_point node)))
		(each (lambda (new_node)
			(unless (. layers :hit_line np (. this :node_to_pad_point new_node) radius gap)
				(push out new_node))) gather)
		out)

	(defmethod :all_not_shorting_via (gather node radius gap)
		; (. pcb :all_not_shorting_via gather node radius gap) -> nodes
		;generate all grid points surrounding node that are not shorting with an existing track
		(raise :layers :via_layers (out (clear '())
			np (push (slice (. this :node_to_pad_point node) +vec3_x +vec3_z) (n2f (dec (get :depth this))))))
		(each (lambda (new_node)
			(defq nnp (push (slice (. this :node_to_pad_point new_node) +vec3_x +vec3_z) 0.0))
			(unless (. via_layers :hit_line np nnp radius gap)
				(unless (. layers :hit_line np nnp radius gap)
					(push out new_node)))) gather)
		out)

	(defmethod :mark_distances (starts ends radius gap via)
		; (. pcb :mark_distances starts ends radius gap via) -> pcb
		;the flood, (flood-wave), all of it in one call
		(defq from (nums) to (nums))
		(. starts :each (# (push from (. this :node_index %0))))
		(each (# (push to (. this :node_index %0))) ends)
		(flood-wave (. this :flood_ctx) from to (get :flood_vectors this) +via_vectors
			radius gap via (get :vias_cost this) (if (get :negotiating this) 1 0))
		this)

	(defmethod :mark_distances_lisp (starts ends radius gap via)
		; (. pcb :mark_distances_lisp starts ends radius gap via) -> pcb
		;the Lisp of what (flood-wave) does, kept to hold it to: a front at
		;a time, a move that waits in vias_map for the distance it is due
		(raise :flood_vectors :vias_cost (distance 1 vias_map (const (Fmap 11))
			frontier (. starts :copy) exit :t ctx (. this :flood_ctx) out_n (list) out_v (list)
			out_nd (nums) out_vd (nums) negotiating (get :negotiating this)))
		(while (and exit (or (not (. vias_map :empty?)) (not (. frontier :empty?))))
			(task-slice)
			(. frontier :each (# (. this :set_node %0 distance)))
			(cond
				((every (# (/= (. this :get_node %0) 0)) ends)
					(setq exit :nil))
				(:t (defq ns (alloc-node-set) vias_start (+ distance vias_cost))
					(. frontier :each (lambda (node)
						(cond
							(negotiating
								;a move that shorts with routed tracks waits
								;its delay, as a via waits its cost
								(flood-cost ctx node flood_vectors +via_vectors radius gap via
									(clear out_n) (clear out_v) (clear out_nd) (clear out_vd))
								(each (lambda (n d)
										(if (= d 0) (. ns :insert n) (delay-node vias_map (+ distance d) n)))
									out_n out_nd)
								(each (lambda (n d) (delay-node vias_map (+ vias_start d) n)) out_v out_vd))
							(:t (flood-near ctx node flood_vectors +via_vectors radius gap via
									(clear out_n) (clear out_v))
								(each (# (. ns :insert %0)) out_n)
								(each (# (delay-node vias_map vias_start %0)) out_v)))))
					(when (defq delay_nodes (. vias_map :find distance))
						(. delay_nodes :each (# (if (= (. this :get_node %0) 0) (. ns :insert %0))))
						(free-node-set delay_nodes)
						(. vias_map :erase distance))
					(free-node-set frontier)
					(setq frontier ns distance (inc distance))
					;nothing at the front, on to the next distance a node
					;waits for, not through every one before it
					(and (. frontier :empty?) (not (. vias_map :empty?))
						(defq next_d (reduce (lambda (d (k &ignore)) (min d k)) (. vias_map :tolist) (first (first (. vias_map :tolist)))))
						(> next_d distance)
						(setq distance next_d)))))
		(.-> vias_map (:each (lambda (k v) (free-node-set v))) :empty)
		this)

	(defmethod :unmark_distances ()
		; (. pcb :unmark_distances) -> pcb
		;set all grid values back to 0
		(raise :nodes)
		(vector-sub nodes nodes nodes)
		this)

	(defmethod :remove_netlist ()
		; (. pcb :remove_netlist) -> pcb
		;remove netlist from board
		(each (# (. %0 :remove)) (get :netlist this))
		this)

	(defmethod :route (select reply_mbox prog_mbox)
		; (. pcb :route select reply_mbox prog_mbox) -> :t | :nil
		;the negotiating router over the board, its nets that still short
		;with others routed on the board as it is, (. pcb :finish), and if
		;that does not settle them, they and the nets in their way
		;negotiated again on their own. :t if every net is routed
		(.-> this :remove_netlist :unmark_distances)
		(sort-netlist (get :netlist this))
		(defq nets (get :netlist this) local 0
			conflicts (. this :negotiate select reply_mbox prog_mbox nets :t)
			blockers (map (# (list %0 (. this :blockers_of %0))) conflicts))
		(. this :settle conflicts nets)
		;the nets left, those in their way, negotiated again on their own,
		;kept only if fewer are left after, so stopping one part way loses
		;nothing the board had
		(defq done (. this :finish select reply_mbox prog_mbox conflicts blockers))
		(while (and (not done) (< local +local_rounds) (not (mail-poll select)))
			(defq left (filter (# (not (get :routed %0))) nets) group (cat left))
			(each (lambda (net)
					(each (# (unless (find %0 group) (push group %0)))
						(cat (ifn (some (lambda ((n b)) (if (eql n net) b)) blockers) (list))
							(. this :in_box net))))
				left)
			(if (get :trace this) (print "local " local " left " (length left) " group " (length group)))
			(defq saved (map (# (list %0 (cat (get :paths %0)) (cat (get :raw_paths %0)) (get :routed %0))) group))
			(each (# (. %0 :remove)) group)
			(setq conflicts (. this :negotiate select reply_mbox prog_mbox group :nil)
				blockers (map (# (list %0 (. this :blockers_of %0))) conflicts))
			(. this :settle conflicts group)
			(setq done (. this :finish select reply_mbox prog_mbox conflicts blockers))
			(when (> (length (filter (# (not (get :routed %0))) nets)) (length left))
				(. this :restore saved)
				(setq done :nil)
				(if (get :trace this) (print "local " local " no better, put back")))
			(++ local))
		;every net routed, or those left are left unrouted
		(defq done (length (filter (# (get :routed %0)) nets)))
		(. this :report reply_mbox prog_mbox done
			(if (= done (length nets)) +pcb_state_routed +pcb_state_short))
		(= done (length nets)))

	(defmethod :restore (saved)
		; (. pcb :restore saved) -> pcb
		;nets put back as they were, (net paths raw_paths routed) each
		(each (lambda ((net paths raw routed))
				(. net :remove)
				(when routed
					(each (# (push (get :paths net) %0)) paths)
					(each (# (push (get :raw_paths net) %0)) raw)
					(. net :add_paths_lines))
				(set net :routed routed))
			saved)
		this)

	(defmethod :blockers_of (net)
		; (. pcb :blockers_of net) -> nets
		;the other nets whose soft tracks the net's soft tracks short with
		(defq mine (get :paths_lines net))
		(filter (lambda (other)
				(and (not (eql other net))
					(some (lambda (theirs)
							(bind '(x1 y1 z1 x2 y2 z2 r g) theirs)
							(defq rec (Record 0 (Layer-line x1 y1 x2 y2 r g)) lo (min z1 z2) hi (max z1 z2))
							(some (lambda ((a b c d e f rr gg))
									(and (<= (min c f) hi) (<= lo (max c f))
										(hit-line? rec (Layer-line a b d e rr gg))))
								mine))
						(get :paths_lines other))))
			(get :netlist this)))

	(defmethod :in_box (net)
		; (. pcb :in_box net) -> nets
		;the other nets with track inside the box of the net's pads
		(bind '(x1 y1 x2 y2) (get :bbox net))
		(filter (lambda (other)
				(and (not (eql other net))
					(some (# (some (lambda (node)
							(defq x (n2f (first node)) y (n2f (second node)))
							(and (<= x1 x x2) (<= y1 y y2))) %0))
						(get :paths other))))
			(get :netlist this)))

	(defmethod :finish (select reply_mbox prog_mbox queue blockers)
		; (. pcb :finish select reply_mbox prog_mbox queue blockers) -> :t | :nil
		;the nets left in conflict routed on the hard board as it is, once
		;each, nothing ripped up for them: a net that does not route is
		;left. :t if all are routed
		(each (lambda (net)
				(unless (mail-poll select)
					(task-slice)
					(. net :remove)
					(set net :routed (. net :route))))
			queue)
		(defq left (filter (# (not (get :routed %0))) queue))
		(push (get :log this) (list :finish (length queue) (length left)))
		(if (get :trace this) (print "finish of " (length queue) " left " (length left)))
		(. this :report reply_mbox prog_mbox (length (filter (# (get :routed %0)) (get :netlist this))))
		(empty? left))

	(defmethod :conflicts (nets)
		; (. pcb :conflicts nets) -> nets
		;the nets that did not route, or whose tracks short with another's
		(raise :soft_layers)
		(filter (lambda (net)
				(cond
					((not (get :routed net)))
					(:t (. net :sub_paths_lines)
						(defq hit (some (# (. soft_layers :hit_layers_line %0)) (. net :create_paths_lines)))
						(. net :add_paths_lines)
						hit)))
			nets))

	(defmethod :add_history (nets)
		; (. pcb :add_history nets) -> pcb
		;every node the tracks of the nets pass, dearer from now on
		(raise :hist :width :height)
		(each (lambda (net)
				(each (lambda (pth)
						(each (lambda ((x y z))
								(defq i (+ x (* width (+ y (* height z)))))
								(elem-set hist i (inc (elem-get hist i))))
							pth))
					(get :raw_paths net)))
			nets)
		this)

	(defmethod :negotiate (select reply_mbox prog_mbox nets fresh)
		; (. pcb :negotiate select reply_mbox prog_mbox nets fresh) -> nets
		;the negotiating router: every net routed, its tracks soft, shorting
		;with others at a cost; then round by round those that short with
		;others are routed again, the cost rising, and where they shorted
		;dearer each time after. The nets that still short at the end. Of
		;those nets, the others of the board hard; fresh, the history and
		;rise of cost begun again
		(raise :netlist :hist :pf)
		(set this :negotiating :t)
		(when fresh
			(vector-sub hist hist hist)
			(clear (get :log this)))
		(elem-set pf 0 +pf_start)
		(defq conflicts nets round 0 best (inc (length nets)) best_round 0)
		(while (and (nempty? conflicts) (< round +negotiate_rounds) (< (- round best_round) +negotiate_stall)
				(not (mail-poll select)))
			;every net again, as PathFinder does, so the nets that are not in
			;conflict move out of the way of those that are as it gets dear
			(each (lambda (net)
					(unless (mail-poll select)
						(task-slice)
						(. net :remove)
						(set net :routed (. net :route))))
				nets)
			(setq conflicts (. this :conflicts nets))
			(push (get :log this) (length conflicts))
			(if (< (length conflicts) best) (setq best (length conflicts) best_round round))
			(if (get :trace this) (print "round " round " of " (length nets) " in conflict " (length conflicts)))
			(. this :add_history conflicts)
			(elem-set pf 0 (min +pf_most (* (first pf) +pf_rise)))
			(++ round)
			(. this :report reply_mbox prog_mbox (- (length netlist) (length conflicts))))
		conflicts)

	(defmethod :report (reply_mbox prog_mbox done &optional state)
		; (. pcb :report reply_mbox prog_mbox done [state]) -> pcb
		;to the app, how many nets are done of them all, whether it is still
		;routing or how it ended, and the board
		(when (>= (. this :get_verbosity) 1)
			(mail-send prog_mbox
				(setf-> (str-alloc +pcb_progress_size)
					(+pcb_progress_total (length (get :netlist this)))
					(+pcb_progress_current done)
					(+pcb_progress_state (ifn state +pcb_state_routing))))
			(. this :print_pcb (defq ss (string-stream (cat ""))))
			(mail-send reply_mbox (str ss)))
		this)

	(defmethod :settle (conflicts nets)
		; (. pcb :settle conflicts nets) -> num
		;the negotiated tracks of the nets made hard: those of the nets not
		;in conflict kept, the others ripped up. How many are kept
		(each (# (. %0 :sub_paths_lines)) nets)
		(set this :negotiating :nil)
		(defq done 0)
		(each (lambda (net)
				(cond
					((find net conflicts)
						(clear (get :paths net) (get :raw_paths net))
						(set net :routed :nil))
					(:t (. net :add_paths_lines)
						(++ done))))
			nets)
		done)

	(defmethod :add_track (track)
		; (. pcb :add_track track) -> pcb
		(raise :netlist)
		(push netlist (Net track this))
		this)

	(defmethod :print_pcb (stream)
		; (. pcb :print_pcb stream) -> pcb
		;output board state for viewer app
		(raise :width :height :depth :grid_res :verbosity)
		(if (>= verbosity 2) (write-line stream ";PCB"))
		(write-line stream "(")
		(if (>= verbosity 2) (write-line stream " ;width, height, depth"))
		(write-line stream (str " (" (/ width grid_res) " " (/ height grid_res) " " depth ")"))
		(each (# (. %0 :print_net stream)) (get :netlist this))
		(write-line stream ")")
		this)
	)

;module
(export-symbols '(Node))
(export-classes '(Pcb Track Net Pad))
(env-pop)
