;docs/ai_digest/async_pipelines.md: a filter is a stream in and a stream out
(defq s (dia-scene) blue 0xffd7e6f7 green 0xffdff0d8 gold 0xffffe8a8 grey 0xfff2f4f6)
(dia-put s 'in (dia-box '("a stream in" "a file, a string, an :in from another task") 260 44 grey 11 :first) 0 0)
(dia-put s 'filter (dia-box '("a filter" "lz4, rle, huffman ...") 170 44 gold 11 :first) 310 0)
(dia-put s 'out (dia-box '("a stream out" "a file, a string, an :out to another task") 260 44 grey 11 :first) 530 0)
(dia-join s 'in 'filter "")
(dia-join s 'filter 'out "")
(diagram "stream_filter" (dia-scene-doc s))
