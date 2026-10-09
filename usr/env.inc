;user defaults handling
(defq *login_root* (path-to-file)
	;the user of a session that was started as one, run.sh -u name, every
	;node of it is told as it starts, (node-start). Else the user the
	;machine has, who last signed on
	*env_user* (ifn (get '*env_node_user*) (load (cat *login_root* "current"))))
(unless (eql *env_user* "Guest") (import "usr/Guest/env.inc"))
(import (cat *login_root* *env_user* "/env.inc"))
