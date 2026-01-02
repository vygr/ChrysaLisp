;user defaults handling
(defq *login_root* (path-to-file)
	*env_user* (load (cat *login_root* "current")))
(unless (eql *env_user* "Guest") (import "usr/Guest/env.inc"))
(import (cat *login_root* *env_user* "/env.inc"))
