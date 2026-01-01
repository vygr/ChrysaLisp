;user defaults handling
(defq *login_root* (slice (first (repl-info)) 0 (rfind "/" (first (repl-info))))
	*env_user* (load (cat *login_root* "current")))
(unless (eql *env_user* "Guest")
	(import (cat *login_root* "Guest/env.inc")))
(import (cat *login_root* *env_user* "/env.inc"))
