;user defaults handling
(defq *env_user* (load "apps/login/current"))
(unless (eql *env_user* "Guest")
	(import (cat "apps/login/Guest/env.inc")))
(import (cat "apps/login/" *env_user* "/env.inc"))
