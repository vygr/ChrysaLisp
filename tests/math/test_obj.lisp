(report-header "Math: Wavefront .obj and .mtl read, the corners of each material, and flat for a shader")

(import "lib/math/obj.inc")
(import "lib/math/mesh.inc")

(defq to_obj (obj-read (string-stream "mtllib cube.mtl other.mtl
# a square of two materials
v 0 0 0
v 1 0 0
v 1 1 0
v 0 1 0
vt 0 0
vt 1 0
vt 1 1
vt 0 1
vn 0 0 1
usemtl red
f 1/1/1 2/2/1 3/3/1 4/4/1
usemtl blue
f -4//1 -3//1 -2//1
f 1 2 3
usemtl nothing
")))
(assert-list-eq "the .mtl files named" '("cube.mtl" "other.mtl") (. to_obj :find :libraries))
(assert-list-eq "the materials in the order used, one no face is of left out" '("red" "blue")
	(map (const first) (. to_obj :find :groups)))
(assert-eq "a face of four is two triangles, a corner three numbers" 18 (length (second (first (. to_obj :find :groups)))))
(assert-eq "a/b/c is the position, the place and the normal, from 0"
	"(0 0 0 1 1 0 2 2 0 0 0 0 2 2 0 3 3 0)" (str (second (first (. to_obj :find :groups)))))
(assert-eq "a//c has no place, and counted from the end" "(0 -1 0 1 -1 0 2 -1 0)"
	(str (slice (second (second (. to_obj :find :groups))) 0 9)))
(assert-eq "a has neither" "(0 -1 -1 1 -1 -1 2 -1 -1)" (str (slice (second (second (. to_obj :find :groups))) 9 18)))

;reals as whole hundredths, to compare
(defun to-h (rs) (str (map (# (n2i (* %0 (n2r 100)))) rs)))
(defq to_flat (obj-flat to_obj (second (first (. to_obj :find :groups)))))
(assert-eq "flat, eight numbers a corner" 48 (length to_flat))
(assert-eq "a place goes down from the top, 1 less the .obj's" "(0 100)" (to-h (slice to_flat 3 5)))
(assert-eq "its normal is the one given" "(0 0 100)" (to-h (slice to_flat 5 8)))
(defq to_plain (obj-flat to_obj (second (second (. to_obj :find :groups)))))
(assert-eq "a triangle with no normals has that of its face" "(0 0 100)" (to-h (slice to_plain 29 32)))

(defq to_mtl (mtl-read (string-stream "newmtl red
Kd 1 0 0
map_Kd -s 1 1 1 tex/red.png
newmtl blue
Kd 0 0 1
d 0.5
newmtl clear
Tr 0.25
")))
(assert-eq "a material's picture, the last word" "tex/red.png" (. (. to_mtl :find "red") :find :map))
(assert-eq "its colour" "(100 0 0)" (to-h (. (. to_mtl :find "red") :find :colour)))
(assert-eq "how much is seen, d" "(50)" (to-h (list (. (. to_mtl :find "blue") :find :seen))))
(assert-eq "or Tr, how much is not" "(75)" (to-h (list (. (. to_mtl :find "clear") :find :seen))))
(assert-eq "one with no picture has none" :nil (. (. to_mtl :find "blue") :find :map))

;the Mesh demo's teapot, made by Mesh-obj on this, as it was before
(defq to_tea (Mesh-obj (file-stream "apps/science/mesh/data/teapot.obj")))
(assert-eq "the teapot, so many vertices, normals and triangles" "2116 2199 4096"
	(str (length (. to_tea :get_verts)) " " (length (. to_tea :get_norms)) " " (length (. to_tea :get_tris))))
