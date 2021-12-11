;message structures

(structure +job 0
	(uint grid_res vias_cost quant)
	(uint flood_range even_range odd_range)
	(netid reply prog)
	(offset data))

(structure +progress 0
	(uint total current))
