(import "./layer.inc")

;module
(env-push)

(defmacro Node (x y z)
	`(nums ,x ,y ,z))

(defmacro mid-node (n1 n2)
	`(vec-div (vec-add ,n1 ,n2) (const (Node 2 2 2))))

(defq +via_vectors (const-quoted
	'(list (list (Node 0 0 -1) (Node 0 0 1)) (list (Node 0 0 -1) (Node 0 0 1))))
	+spacial_hash_res 1.0 +path_scale 1 +node_sets ''())

(defun alloc-node-set ()
	(unless (defq ns (pop +node_sets))
		(defq ns (fset 11))) ns)

(defun free-node-set (ns)
	(push +node_sets (. ns :empty)))

(defun gen-vectors (vec_range x_range y_range)
	;generate range of routing vectors
	(defq out (list) vec_range (n2f vec_range) y (neg y_range))
	(while (<= y y_range)
		(defq x (neg x_range))
		(while (<= x x_range)
			(defq l (vec-length (Vec2-f (n2f x) (n2f y))))
			(and (> l 0.1) (<= l vec_range)
				(push out (Node x y 0)))
			(setq x (inc x)))
		(setq y (inc y)))
	out)

(defun aabb_pads (pads quant)
	;aabb of pads
	(defq minxyz (Vec3-f 1000000.0 1000000.0 0.0) maxxyz (Vec3-f -1000000.0 -1000000.0 0.0)
		quant (n2f quant))
	(each (lambda (p)
		(setq p (. p :get_pos) minxyz (vec-min minxyz p) maxxyz (vec-max maxxyz p))) pads)
	(bind '(minx miny _) minxyz)
	(bind '(maxx maxy _) maxxyz)
	(list (n2f (n2i (/ (* (- maxx minx) (- maxy miny)) quant)))
		(list minx miny maxx maxy)))

(defun sort-netlist (netlist)
	(sort (# (- (. %0 :get_area) (. %1 :get_area))) netlist))

(defclass Pad (radius gap pos shape) nil
	; (Pad radius gap pos shape) -> pad
	(def this :radius radius :gap gap :pos pos :shape shape)
	(defgetmethod radius)
	(defsetmethod radius)
	(defgetmethod gap)
	(defsetmethod gap)
	(defgetmethod pos)
	(defsetmethod pos)
	(defgetmethod shape)
	(defsetmethod shape))

(defclass Track (id track_radius via_radius gap pads wires) nil
	; (Track id track_radius via_radius gap pads wires) -> track
	(def this :id id :track_radius track_radius :via_radius via_radius :gap gap
		:pads pads :wires wires)
	(defgetmethod id)
	(defgetmethod track_radius)
	(defgetmethod via_radius)
	(defgetmethod gap)
	(defgetmethod pads)
	(defgetmethod wires))

(defclass Net (track pcb) nil
	; (Net track pcb) -> net
	(defq grid_res (n2f (. pcb :get_grid_res)))
	(def this
		:pcb pcb :id (. track :get_id)
		:radius (* (. track :get_track_radius) grid_res)
		:via (defq via (* (. track :get_via_radius) grid_res))
		:gap (* (. track :get_gap) grid_res)
		:pads (defq pads (. track :get_pads))
		:wires (defq wires (. track :get_wires))
		:paths (defq paths (list))
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
			(vec-scale p grid_res p)) (. p :get_shape))) pads)
	(each (lambda (wire)
		(each (lambda (p)
			(bind '(x y z) p)
			(elem-set _ wire (Vec3-f (* x grid_res) (* y grid_res) z))) wire)) wires)

	;sort pads
	(sort (lambda (p1 p2)
		(bind '(x1 y1 z1) (. p1 :get_pos))
		(bind '(x2 y2 z2) (. p2 :get_pos))
		(if (= (defq s (- x1 x2)) 0)
			(if (= (defq s (- y1 y2)) 0)
				(defq s (- z1 z2)))) s) pads)

	;build pad collision lines and endpoint nodes
	(defq i 0)
	(while (< i (length pads))
		(defq p (elem-get i pads)
			pos (. p :get_pos)
			radius (. p :get_radius)
			gap (. p :get_gap)
			shape (. p :get_shape)
			xy (slice +vec3_x +vec3_z pos))
		(bind '(x y z1) pos)
		(defq z2 z1)
		;traverse pad stack
		(while (and (< (setq i (inc i)) (length pads))
					(eql xy (slice +vec3_x +vec3_z (defq pos (. (defq p (elem-get i pads)) :get_pos))))
					(= radius (. p :get_radius))
					(= gap (. p :get_gap))
					(every (const eql) shape (. p :get_shape)))
			(setq z2 (elem-get +vec3_z pos)))
		;collision lines
		(cond
			((= (length shape) 0)
				(push pad_lines (Layers-line x y z1 x y z2 radius gap)))
			(t  (defq z z1)
				(while (<= z z2)
					(bind '(xs ys) (elem-get 0 shape))
					(defq cx x cy y cz z
						p0x nil p0y nil p0z nil
						p1x (+ x xs) p1y (+ y ys) p1z cz)
					(each! 1 -1 (lambda ((xs ys))
						;add pad entries to via only spacial cache
						(.-> pcb :get_via_layers (:add_layers_line (Layers-line cx cy cz p1x p1y p1z radius gap)))
						;pad lines
						(setq p0x p1x p0y p1y p0z p1z p1x (+ x xs) p1y (+ y ys) p1z cz)
						(push pad_lines (Layers-line p0x p0y p0z p1x p1y p1z radius gap))) (list shape))
					(setq z (+ z 1.0)))))
		;ends and deformations
		(push pad_end_nodes (defq ends (list)))
		(defq z z1)
		(while (<= z z2)
			(push ends (. pcb :pad_point_to_node (Vec3-f x y z)))
			(setq z (+ z 1.0))))

	;build wires collision lines and visited
	(each (lambda (wire)
		(defq p0 nil p1 (elem-get 0 wire))
		(each! 1 -1 (lambda (p)
			(setq p0 p1 p1 p)
			(cond
				((/= (elem-get +vec3_z p0) (elem-get +vec3_z p1))
					;via wire
					(bind '(x0 y0 z0) p0)
					(defq depth (n2f (dec (. pcb :get_depth))) z 0.0)
					(push wire_lines (Layers-line x0 y0 0.0 x0 y0 depth via gap))
					(while (<= z depth)
						(. wire_nodes :insert (. pcb :pad_point_to_node (Vec3-f x0 y0 z)))
						(setq z (+ z 1.0))))
				(t  ;layer wire
					(bind '(x0 y0 z0) p0)
					(bind '(x1 y1 z1) p1)
					(push wire_lines (Layers-line x0 y0 z0 x1 y1 z1 radius gap))
					(defq p (Vec2-f x0 y0) v (vec-sub p (Vec2-f x1 y1))
						l (vec-length v) norm (vec-scale v (recip l))
						i 0.0)
					(while (< i l)
						(bind '(x y) (vec-add p (vec-scale norm i)))
						(. wire_nodes :insert (. pcb :pad_point_to_node (Vec3-f x y z0)))
						(setq i (+ i 0.25)))
					(. wire_nodes :insert (. pcb :pad_point_to_node p1))))) (list wire))) wires)

	;add pad entries to via only spacial cache
	(each (# (.-> pcb :get_via_layers (:add_layers_line %0))) pad_lines)

	;bounds
	(bind '(area bbox) (aabb_pads pads (. pcb :get_quant)))
	(def this :area area :bbox bbox)

	(defgetmethod radius)
	(defgetmethod pads)
	(defgetmethod area)
	(defgetmethod id)
	(defsetmethod area)
	(defsetmethod bbox)
	(defsetmethod pcb)

	(defmethod :shuffle_topology (this)
		; (. net :shuffle_topology) -> net
		;randomize order of pads
		(shuffle (get :pad_end_nodes this))
		this)

	(defmethod :add_pad_lines (this)
		; (. net :add_pad_lines) -> net
		;add pad entries to spacial cache
		(raise :pcb)
		(each (# (.-> pcb :get_layers (:add_layers_line %0))) (get :pad_lines this))
		this)

	(defmethod :sub_pad_lines (this)
		; (. net :sub_pad_lines) -> net
		;remove pad entries from spacial cache
		(raise :pcb)
		(each (# (.-> pcb :get_layers (:sub_layers_line %0))) (get :pad_lines this))
		this)

	(defmethod :add_wire_lines (this)
		; (. net :add_wire_lines) -> net
		;add wire entries to spacial cache
		(raise :pcb)
		(each (# (.-> pcb :get_layers (:add_layers_line %0))) (get :wire_lines this))
		this)

	(defmethod :sub_wire_lines (this)
		; (. net :sub_wire_lines) -> net
		;remove wire entries from spacial cache
		(raise :pcb)
		(each (# (.-> pcb :get_layers (:sub_layers_line %0))) (get :wire_lines this))
		this)

	(defmethod :create_paths_lines (this)
		; (. net :create_paths_lines) -> lines
		;create paths collision lines
		(raise :pcb :radius :via :gap)
		(defq depth (n2f (dec (. pcb :get_depth))))
		(reduce (lambda (lines pth)
			(defq p0 nil p1 (. pcb :node_to_pad_point (elem-get 0 pth)))
			(each! 1 -1 (lambda (p)
				(setq p0 p1 p1 (. pcb :node_to_pad_point p))
				(bind '(x0 y0 z0) p0)
				(bind '(x1 y1 z1) p1)
				(if (/= z0 z1)
					(push lines (Layers-line x0 y0 0.0 x0 y0 depth via gap))
					(push lines (Layers-line x0 y0 z0 x1 y1 z1 radius gap)))) (list pth))
			lines) (get :paths this) (list)))

	(defmethod :add_paths_lines (this)
		; (. net :add_paths_lines) -> net
		;add paths entries to spacial cache
		(raise :pcb)
		(defq paths_lines (. this :create_paths_lines))
		(lower :paths_lines)
		(each (# (.-> pcb :get_layers (:add_layers_line %0))) paths_lines)
		this)

	(defmethod :sub_paths_lines (this)
		; (. net :sub_paths_lines) -> net
		;remove paths entries from spacial cache
		(raise :pcb :paths_lines)
		(each (# (.-> pcb :get_layers (:sub_layers_line %0))) paths_lines)
		(clear paths_lines)
		this)

	(defmethod :remove (this)
		; (. net :remove) -> net
		;remove net entries from spacial grid
		(.-> this
			:sub_paths_lines
			:sub_wire_lines
			:sub_pad_lines
			:add_pad_lines
			:add_wire_lines)
		(clear (get :paths this))
		this)

	(defmethod :optimise_paths (this paths)
		; (. net :optimise_paths paths) -> opt_paths
		;remove redundant points from paths
		(raise :pcb)
		(reduce (lambda (opt_paths pth)
			(defq opt_path (list)
				d0 (Vec3-f 0.0 0.0 0.0) d1 nil
				p0 nil p1 (. pcb :node_to_pad_point (elem-get 0 pth)))
			(each! 1 -1 (lambda (p)
				(setq p0 p1 p1 (. pcb :node_to_pad_point p)
					d1 (vec-norm (vec-sub p1 p0)))
				(unless (eql d0 d1)
					(push opt_path (elem-get (dec _) pth))
					(setq d0 d1))) (list pth))
			(push opt_path (elem-get -2 pth))
			(push opt_paths opt_path)) paths (list)))

	(defmethod :backtrack_path (this visited end_node radius via gap)
		; (. net :backtrack_path this visited end_node radius via gap) -> nodes
		;backtrack path from ends to starts
		(raise :pcb)
		(defq path_vectors (. pcb :get_path_vectors)
			nearer_nodes '() pth (list) path_node end_node exit nil)
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
				(t  ;sort nodes and take the first
					(setq path_node (elem-get 1 (elem-get 0
						(sort (# (- (elem-get 0 %0) (elem-get 0 %1)))
						(map (# (list (vec-squared-euclidean-distance path_node %0) %0))
						(filter (# (= (. pcb :get_node %0) (. pcb :get_node (elem-get 0 nearer_nodes))))
						(sort (# (- (. pcb :get_node %0) (. pcb :get_node %1))) nearer_nodes))))))))))
		exit)

	(defmethod :route (this)
		; (. net :route) -> t | nil
		;attempt to route this net on the current boards state
		(raise :pcb :radius :via :gap :paths :pad_end_nodes)
		(cond
			((= radius 0.0) t)
			(t  (clear paths)
				(.-> this
					:sub_pad_lines
					:sub_wire_lines)
				(defq visited (. (get :wire_nodes this) :copy)
					failed (some! 1 -1 nil (lambda (ends)
						(each (# (. visited :insert %0)) (elem-get (dec _) pad_end_nodes))
						(unless (some (# (. visited :find %0)) ends)
							(. pcb :mark_distances radius via gap visited ends)
							(sort (# (- (. pcb :get_node %0) (. pcb :get_node %1))) ends)
							(defq nodes (. this :backtrack_path visited (elem-get 0 ends) radius via gap))
							(. pcb :unmark_distances)
							(cond
								((/= (length nodes) 0)
									(each (# (. visited :insert %0)) nodes)
									(push paths nodes) nil)
								(t)))) (list pad_end_nodes)))
				(cond
					(failed
						(. this :remove) nil)
					(t  (setq paths (. this :optimise_paths paths))
						(lower :paths)
						(.-> this
							:add_paths_lines
							:add_pad_lines
							:add_wire_lines)
						t)))))

	(defmethod :print_net (this stream)
		; (. net :print_net stream) -> net
		;output net, pads, wires and paths, for viewer app
		(raise :pcb :pads :wires :paths :id :radius :via :gap)
		(defq scale (recip (n2f (. pcb :get_grid_res))) verbosity (. pcb :get_verbosity))
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
				(write stream "   (")
				(each (lambda (p)
						(write stream (str (vec-scale p scale))))
					(. p :get_shape))
				(write-line stream ")")
				(write-line stream "  )"))
			pads)
		(write-line stream " )")
		(if (>= verbosity 2) (write-line stream " ;WIRES"))
		(write-line stream " (")
		(each (lambda (wire)
				(if (>= verbosity 2) (write-line stream "  ;WIRE"))
				(write stream "  (")
				(each (lambda (p)
						(bind '(x y z) p)
						(write stream (str "(" (* x scale) " " (* y scale) " " z ")")))
					wire)
				(write-line stream ")"))
			wires)
		(write-line stream " )")
		(if (>= verbosity 2) (write-line stream " ;PATHS"))
		(write-line stream " (")
		(each (lambda (pth)
				(if (>= verbosity 2) (write-line stream "  ;PATH"))
				(write stream "  (")
				(each (lambda (p)
						(bind '(x y z) (. pcb :node_to_pad_point p))
						(write stream (str "(" (* x scale) " " (* y scale) " " z ")")))
					pth)
				(write stream ")"))
			paths)
		(write-line stream " )")
		(write-line stream ")")
		this)
	)

(defclass Pcb (width height depth grid_res verb quant vias_cost fr fr_even fr_odd) nil
	; (Pcb width height depth grid_res verb quant vias_cost fr fr_even fr_odd) -> pcb
	(def this
		:nodes_dim (dim (nums (* width grid_res) (* height grid_res) depth)
			(defq nodes (cap (* width height depth grid_res grid_res) (nums))))
		:width (* width grid_res) :height (* height grid_res) :depth depth
		:deform (fmap 101) :grid_res grid_res
		:flood_vectors (list
			(gen-vectors fr fr_even fr)
			(gen-vectors fr fr fr_odd))
		:path_vectors (list
			(gen-vectors (* fr +path_scale) (* fr_even +path_scale) (* fr +path_scale))
			(gen-vectors (* fr +path_scale) (* fr +path_scale) (* fr_odd +path_scale)))
		:netlist (list) :verbosity verb
		:quant quant :vias_cost (* vias_cost grid_res)
		:layers (Layers (n2i (* (n2f width) +spacial_hash_res))
						(n2i (* (n2f height) +spacial_hash_res))
						depth (/ +spacial_hash_res (n2f grid_res)))
		:via_layers (Layers (n2i (* (n2f width) +spacial_hash_res))
						(n2i (* (n2f height) +spacial_hash_res))
						depth (/ +spacial_hash_res (n2f grid_res)))
		:nodes nodes)
	(times (* width height depth grid_res grid_res) (push nodes 0))

	(defgetmethod depth)
	(defgetmethod grid_res)
	(defgetmethod quant)
	(defgetmethod layers)
	(defgetmethod via_layers)
	(defgetmethod verbosity)
	(defgetmethod path_vectors)

	(defmethod :get_node (this node)
		; (. pcb :get_node node) -> val
		;get grid node value
		(dim-get node (get :nodes_dim this)))

	(defmethod :set_node (this node val)
		; (. pcb :set_node node val) -> pcb
		;set grid node to value
		(dim-set node (get :nodes_dim this) val)
		this)

	(defmethod :node_to_point (this (x y z))
		; (. pcb :node_to_point node) -> point
		;convert node to point
		(Vec3-f (n2f x) (n2f y) (n2f z)))

	(defmethod :point_to_node (this (x y z))
		; (. pcb :point_to_node point) -> node
		;convert point to node
		(Node (n2i (+ x 0.5)) (n2i (+ y 0.5)) (n2i z)))

	(defmethod :node_to_pad_point (this node)
		; (. pcb :node_to_pad_point node) -> point
		;convert node to pad point
		(if (defq p (. (get :deform this) :find node))
			p (. this :node_to_point node)))

	(defmethod :pad_point_to_node (this point)
		; (. pcb :pad_point_to_node point) -> node
		;convert pad point to node
		(. (get :deform this) :insert (defq n (. this :point_to_node point)) point)
		n)

	(defmethod :all_not_marked (this vec node)
		; (. pcb :all_not_marked vec node) -> nodes
		;generate all grid points surrounding node, that are value 0
		(defq out (clear '())
			vecm (Node (dec (get :width this)) (dec (get :height this)) (dec (get :depth this))))
		(each (lambda (v)
			(and (eql (vec-min vecm (vec-max (defq n (vec-add node v)) +nums_zero3 +nums_tmp3) +nums_tmp3) n)
					(= (. this :get_node n) 0)
				(push out n))) (elem-get (% (elem-get +vec3_z node) 2) vec))
		out)

	(defmethod :all_marked (this vec node)
		; (. pcb :all_marked vec node) -> sort_nodes
		;generate all grid points surrounding node, that are not value 0, but less than node
		(defq out (clear '()) d (. this :get_node node)
			vecm (Node (dec (get :width this)) (dec (get :height this)) (dec (get :depth this))))
		(each (lambda (v)
			(and (eql (vec-min vecm (vec-max (defq n (vec-add node v)) +nums_zero3 +nums_tmp3) +nums_tmp3) n)
					(< 0 (. this :get_node n) d)
				(push out n))) (elem-get (% (elem-get +vec3_z node) 2) vec))
		out)

	(defmethod :all_not_shorting (this gather node radius gap)
		; (. pcb :all_not_shorting gather node radius gap) -> nodes
		;generate all grid points surrounding node that are not shorting with an existing track
		(raise :layers)
		(defq out (clear '()) np (. this :node_to_pad_point node))
		(each (lambda (new_node)
			(unless (. layers :hit_line np (. this :node_to_pad_point new_node) radius gap)
				(push out new_node))) gather)
		out)

	(defmethod :all_not_shorting_via (this gather node radius gap)
		; (. pcb :all_not_shorting_via gather node radius gap) -> nodes
		;generate all grid points surrounding node that are not shorting with an existing track
		(raise :layers :via_layers)
		(defq out (clear '()) np (push (slice +vec3_x +vec3_z (. this :node_to_pad_point node))
			(n2f (dec (get :depth this)))))
		(each (lambda (new_node)
			(defq nnp (push (slice +vec3_x +vec3_z (. this :node_to_pad_point new_node)) 0.0))
			(unless (. via_layers :hit_line np nnp radius gap)
				(unless (. layers :hit_line np nnp radius gap)
					(push out new_node)))) gather)
		out)

	(defmethod :mark_distances (this radius via gap starts ends)
		; (. pcb :mark_distances radius via gap starts ends) -> pcb
		;generate all grid points surrounding node that are not shorting with an existing track
		(raise :flood_vectors :vias_cost)
		(defq distance 1 vias_map (const-quoted (fmap 11))
			frontier (. starts :copy) exit t)
		(while (and exit (or (not (. vias_map :empty?)) (not (. frontier :empty?))))
			(task-slice)
			(. frontier :each (# (. this :set_node %0 distance)))
			(cond
				((every (# (/= (. this :get_node %0) 0)) ends)
					(setq exit nil))
				(t  (defq ns (alloc-node-set) vias_start (+ distance vias_cost))
					(. frontier :each (lambda (node)
						(each (# (. ns :insert %0))
							(. this :all_not_shorting
								(. this :all_not_marked flood_vectors node) node radius gap))
						(when (/= 0 (length (defq nv (. this :all_not_shorting_via
									(. this :all_not_marked +via_vectors node) node via gap))))
							(unless (defq vns (. vias_map :find vias_start))
								(. vias_map :insert vias_start (defq vns (alloc-node-set))))
							(each (# (. vns :insert %0)) nv))))
					(when (defq delay_nodes (. vias_map :find distance))
						(. delay_nodes :each (# (if (= (. this :get_node %0) 0) (. ns :insert %0))))
						(free-node-set delay_nodes)
						(. vias_map :erase distance))
					(free-node-set frontier)
					(setq frontier ns distance (inc distance)))))
		(.-> vias_map (:each (lambda (k v) (free-node-set v))) :empty)
		this)

	(defmethod :unmark_distances (this)
		; (. pcb :unmark_distances) -> pcb
		;set all grid values back to 0
		(raise :nodes)
		(vec-sub nodes nodes nodes)
		this)

	(defmethod :reset_areas (this)
		; (. pcb :reset_areas) -> pcb
		;reset areas
		(raise :quant)
		(each (lambda (net)
			(bind '(area bbox) (aabb_pads (. net :get_pads) quant))
			(.-> net (:set_area area) (:set_bbox bbox))) (get :netlist this))
		this)

	(defmethod :shuffle_netlist (this)
		; (. pcb :shuffle_netlist) -> pcb
		;shuffle order of netlist
		(raise :netlist)
		(shuffle netlist)
		(each (# (. %0 :shuffle_topology)) netlist)
		this)

	(defmethod :hoist_net (this index)
		; (. pcb :hoist_net index) -> (netlist new_index)
		;move net to top of area group
		(raise :netlist)
		(defq area (. (elem-get index netlist) :get_area)
			new_index (some! index 0 nil (# (if (< (. %0 :get_area) area) (inc _))) (list netlist)))
		(unless new_index (setq new_index 0))
		(when (/= index new_index)
			(defq net (slice index (inc index) netlist)
				netlist (erase netlist index (inc index))
				netlist (insert netlist new_index net))
			(lower :netlist))
		(list netlist new_index))

	(defmethod :remove_netlist (this)
		; (. pcb :remove_netlist) -> pcb
		;remove netlist from board
		(each (# (. %0 :remove)) (get :netlist this))
		this)

	(defmethod :route (this select mbox)
		; (. pcb :route select mbox) -> t | nil
		(raise :netlist)
		(.-> this :remove_netlist :unmark_distances :reset_areas :shuffle_netlist)
		(sort-netlist netlist)
		(defq hoisted_nets (fset) index 0 exit t retval t)
		(while (and exit (not (mail-poll select)) (< index (length netlist)))
			(task-slice)
			(defq net (elem-get index netlist))
			(cond
				((. net :route)
					(setq index (inc index)))
				(t  (cond
						((= index 0)
							(.-> this :reset_areas :shuffle_netlist)
							(sort-netlist netlist))
						(t  (bind '(netlist pos) (. this :hoist_net index))
							(. (elem-get index netlist) :shuffle_topology)
							(cond
								((or (= pos index) (. hoisted_nets :find net))
									(. net :set_area (. (elem-get (dec index) netlist) :get_area))
									(bind '(netlist pos) (. this :hoist_net index))
									(. hoisted_nets :erase net))
								(t  (. hoisted_nets :insert net)))
							(while (> index pos)
								(. (elem-get index netlist) :remove)
								(setq index (dec index)))))))
			(when (>= (. this :get_verbosity) 1)
				(. this :print_pcb (defq ss (string-stream (cat ""))))
				(mail-send mbox (str ss))))
		retval)

	(defmethod :add_track (this track)
		; (. pcb :add_track track) -> pcb
		(raise :netlist)
		(push netlist (Net track this))
		this)

	(defmethod :close (this)
		; (. pcb :close) -> pcb
		;drop all nets and circular refs to pcb !
		(raise :netlist)
		(each (# (. %0 :set_pcb nil)) (get :netlist this))
		this)

	(defmethod :print_pcb (this stream)
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
(export-symbols
	Node)
(export-classes
	Pcb Track Net Pad)
(env-pop)
