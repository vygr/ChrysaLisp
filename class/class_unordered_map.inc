%ifndef CLASS_UNORDERED_MAP_1234
%define CLASS_UNORDERED_MAP_1234

(import 'class/class_unordered_set.inc)

(def-class 'unordered_map 'unordered_set)
(def-method 'create 'class/unordered_map/create)
(def-method 'find 'class/unordered_map/find)
(def-method 'copy 'class/unordered_map/copy)
(def-method 'insert 'class/unordered_map/insert 'static '(r0, r1, r2), '(r1, r2))

(def-method 'slice 'class/unordered_map/slice 'override)

(def-struct 'unordered_map 'unordered_set)
(def-struct-end)

%endif
