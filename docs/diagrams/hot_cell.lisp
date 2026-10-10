;docs/ai_digest/keeping_it_hot.md: an environment and its bindings are one cell
(defq s (dia-scene) blue 0xffd7e6f7 green 0xffdff0d8 gold 0xffffe8a8 grey 0xfff2f4f6)
(dia-under s (dia-box "" 660 86 grey) 0 0)
(dia-say s "a single heap cell" 12 18 12 :t 0xff202428)
(dia-put s :nil (dia-box '("the :hmap, its header" "vtable, count, parent, capacity, length") 290 46 blue 11 :first) 12 28)
(dia-put s :nil (dia-box '("its storage, in line" "key 0, value 0, key 1, value 1 ...") 346 46 green 11 :first) 302 28)
(dia-say s "No second piece of memory for the bindings, and no pointer to follow to reach them." 0 110 12 :nil 0xff202428)
(diagram "hot_cell" (dia-scene-doc s))
