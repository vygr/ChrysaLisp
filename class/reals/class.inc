(include 'class/fixeds/class.inc)

(def-class 'reals 'fixeds)
(dec-method :vtable 'class/reals/vtable)
(dec-method :create 'class/reals/create)

(dec-method :vcreate 'class/reals/create :override)
(dec-method :velement 'class/real/create :override)

(dec-method :add 'class/reals/add :override)
(dec-method :sub 'class/reals/sub :override)
(dec-method :mul 'class/reals/mul :override)
(dec-method :div 'class/reals/div :override)
(dec-method :mod 'class/reals/mod :override)
(dec-method :abs 'class/reals/abs :override)
(dec-method :sum 'class/reals/sum :override)
(dec-method :scale 'class/reals/scale :override)
(dec-method :frac 'class/reals/frac :override)
(dec-method :floor 'class/reals/floor :override)

(def-struct 'reals 'fixeds)
(def-struct-end)
