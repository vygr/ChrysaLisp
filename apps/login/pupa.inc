;user defaults handling
(defq *env_user* (load "apps/login/current"))
(import (cat "apps/login/" *env_user* "/pupa.inc"))
