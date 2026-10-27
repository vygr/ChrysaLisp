(report-header "Loops: what holds itself round is never let go of, and nothing should")

(import "lib/debug/loops.inc")
(import "lib/task/global.inc")
(import "lib/task/farm.inc")
(import "lib/task/local.inc")
(import "lib/task/jobs.inc")
(import "lib/task/pipe.inc")
(import "gui/lisp.inc")
(import "lib/cwb/board.inc")
(import "lib/cwb/palette.inc")
(import "lib/cwb/stripes.inc")
(defq *app_root* "apps/science/pcb/")
(import "apps/science/pcb/reader.inc")
(import "apps/science/pcb/router.inc")

;Ownership is a tree. Letting go of the last count of a thing lets go of all
;it holds, with nothing to call: a link back to an owner is a weak ref,
;claimed where it is used. Chris: "We should be able to just close down a ref
;to a tree or lib structure and it never leaks". (loops obj) finds what holds
;a thing above it; this module makes each of these and lets go of them with
;no :close, and tests -e fails it if that leaves anything held on the node.
;:close is a courtesy to the children of a farm, they go on their own

(defq lp_mbox (mail-mbox))
(defun lp-create (key now)
	(open-task "(task-sleep 1)" key +kn_call_pin 0 lp_mbox)
	(def (defq val (env 1)) :timestamp now)
	val)
(defun lp-destroy (key val))
(defun lp-fcreate (key val nodes) (open-task "(task-sleep 1)" (first nodes) +kn_call_pin key lp_mbox))
(defun lp-say (name obj) (assert-eq name "()" (str (map (lambda ((h k)) k) (loops obj)))))
(defq lp_g (Global lp-create lp-destroy)) (. lp_g :refresh) (lp-say "Global" lp_g)
(defq lp_f (Farm lp-fcreate lp-destroy 2)) (lp-say "Farm" lp_f)
(defq lp_l (Local lp-fcreate lp-destroy 4 2 1)) (lp-say "Local" lp_l)
(defq lp_j (Jobs "tests/system/data/jobs_child.lisp" (mail-mbox) (mail-mbox) 2)) (lp-say "Jobs" lp_j)
(defq lp_p (Pipe "echo hi")) (lp-say "Pipe" lp_p)
(defq lp_b (Board (cwb-doc 200 100))) (lp-say "Board" lp_b)
(defq lp_s (Stripes lp_b 2)) (lp-say "Stripes" lp_s)
(defq lp_pal (Palette lp_b 100 100)) (lp-say "Palette" lp_pal)

;the pcb router: a pcb holds its nets, a net held the pcb by a count
(defq pcb_list (pcb-read (load "apps/science/pcb/data/test1.pcb")))
(bind '(width height depth) (first pcb_list))
(defq pcb (Pcb width height depth 1 0 0 2 1 1))
(each! (lambda ((id track_radius via_radius track_gap pads wires &optional paths))
	(setq wires '() pads (map (lambda ((radius gap pos shape)) (Pad radius gap pos shape)) pads))
	(. pcb :add_track (Track id track_radius via_radius track_gap pads wires))) (list pcb_list) 1)
(assert-eq "a pcb and its nets hold nothing round" "()" (str (map (lambda ((h k)) k) (loops pcb))))
(assert-true "and it has nets" (nempty? (get :netlist pcb)))
(defq pcb_net (first (get :netlist pcb)))
(. pcb_net :sub_pad_lines)
(assert-true "a net routed alone uses the pcb it has by its weak ref" (. pcb_net :route))
