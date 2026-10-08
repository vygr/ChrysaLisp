;message structure

;what a node says of itself, then each of its links, as (net-links) has
;them, lib/net/links.inc, one after another
(structure +reply 0
	(nodeid node)
	(nodeid system)
	(uint task_count num_links)
	(offset links))
