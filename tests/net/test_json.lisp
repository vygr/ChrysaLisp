(import "lib/net/json.inc")

(report-header "Network & Data: JSON Codec")

; --- 1. Parsing Scalars ---
(assert-eq "json-parse null" :nil (json-parse "null"))
(assert-eq "json-parse true" :t (json-parse "true"))
(assert-eq "json-parse false" :nil (json-parse "false"))
(assert-eq "json-parse integer" 42 (json-parse "42"))
(assert-eq "json-parse negative int" -10 (json-parse "-10"))
(assert-eq "json-parse string" "hello world" (json-parse "\qhello world\q"))

; --- 2. Parsing Arrays ---
(defq arr_res (json-parse "[1, 2, 3]"))
(assert-true "json-parse array is list" (list? arr_res))
(assert-list-eq "json-parse array elements" '(1 2 3) arr_res)

; --- 3. Parsing Objects ---
(defq obj_res (json-parse "{\qname\q: \qChrysaLisp\q, \qversion\q: 1}"))
(assert-true "json-parse object is pmap" (pmap? obj_res))
(assert-eq "json-parse object field name" "ChrysaLisp" (pfind obj_res :name))
(assert-eq "json-parse object field version" 1 (pfind obj_res :version))

; --- 4. Nested Structures ---
(defq nested_json "{\qitems\q: [10, 20], \qmeta\q: {\qactive\q: true}}")
(defq parsed_nested (json-parse nested_json))
(assert-list-eq "nested array in object" '(10 20) (pfind parsed_nested :items))
(assert-eq "nested object in object" :t (pfind (pfind parsed_nested :meta) :active))

; --- 5. Stringification (json-stringify) ---
(assert-eq "json-stringify null" "null" (json-stringify :nil))
(assert-eq "json-stringify true" "true" (json-stringify :t))
(assert-eq "json-stringify number" "123" (json-stringify 123))
(assert-eq "json-stringify string" "\qtest\q" (json-stringify "test"))
(assert-eq "json-stringify list/array" "[1,2,3]" (json-stringify (list 1 2 3)))

(defq sample_pmap (pmap :x 10 :y 20))
(defq stringified_pmap (json-stringify sample_pmap))
(defq roundtrip_pmap (json-parse stringified_pmap))
(assert-eq "stringify/parse roundtrip pmap x" 10 (pfind roundtrip_pmap :x))
(assert-eq "stringify/parse roundtrip pmap y" 20 (pfind roundtrip_pmap :y))

; --- 6. Stream Support ---
(defq ms (string-stream "{\qstream\q: 99}"))
(defq stream_res (json-parse ms))
(assert-eq "json-parse from stream" 99 (pfind stream_res :stream))

; --- 7. .tre Interoperability (json-to-tre / json-from-tre) ---
(defq tre_data (json-to-tre "{\qconfig\q: [\qa\q, \qb\q]}"))
(assert-true "json-to-tre produces pmap" (pmap? tre_data))
(assert-list-eq "json-to-tre array" '("a" "b") (pfind tre_data :config))

(defq back_to_json (json-from-tre tre_data))
(defq tre_roundtrip (json-to-tre back_to_json))
(assert-list-eq "json-from-tre roundtrip" '("a" "b") (pfind tre_roundtrip :config))