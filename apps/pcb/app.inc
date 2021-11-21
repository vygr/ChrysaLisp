;message structures

(structure +job 0
	(int grid_res vias_cost quant)
	(int flood_range even_range odd_range)
	(netid reply prog)
	(offset data))

(structure +progress 0
	(int total current))
