;message structures

(structure +pcb_job 0
	(uint grid_res vias_cost)
	(uint flood_range even_range odd_range)
	(netid reply prog)
	(offset data))

;state: routing, every net routed, ended with some not routed, or with a
;fault the design rule check found
(structure +pcb_progress 0
	(uint total current state))

(enums +pcb_state 0
	(enum routing routed short drc))
