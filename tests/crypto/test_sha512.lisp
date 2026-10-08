(report-header "Crypto: SHA-512")
(import "lib/crypto/sha512.inc")

(defun s5-hex (bytes)
	(to-lower (apply (const cat) (map (# (byte-to-hex-str (code %0))) bytes))))

;the answers of the standard, from Python's hashlib
(assert-eq "nothing" "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"
	(s5-hex (sha512 "")))
(assert-eq "abc" "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"
	(s5-hex (sha512 "abc")))
(assert-eq "the long example" "8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909"
	(s5-hex (sha512 "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu")))
(assert-eq "a million of the letter a" "e718483d0ce769644e2e42c7bc15b4638e1f98b13b2044285632a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b"
	(s5-hex (sha512 (apply (const cat) (map (lambda (_) (const (apply (const cat) (map (lambda (_) "aaaaaaaaaa") (range 0 100))))) (range 0 1000))))))

;every length about the edges of a block, where the padding changes. Byte i is (i*7+3)&255
(defun s5-data (n) (apply (const cat) (cat (list "") (map (# (char (logand (+ (* %0 7) 3) 255))) (range 0 n)))))
(each (lambda ((n want))
	(assert-eq (cat "a str of " (str n) " bytes") want (s5-hex (sha512 (s5-data n)))))
	'((1 "e45bf5817ddf94aa2f7a407071f0eedc6beb98f768b4cd33d1176d44d1563a45a5d7212290eb7670c6786b13591aedac86478993895e8b24e612014abaa6ba04")
	(55 "14fd424b1fcadee624da946ab03f7e1def7c0d6e00f689594319881a26ff30b875ba4c622ac13100c8cc784c9c2eb23159aecbb4a02e3999062f551193e2b256")
	(56 "480fa85be41ef55a41208ca28ffc8743c91cf7d24758defe6f95bfb16de614fc86b701034896b047dd571de4318853d80e0809df162f1752cb26da6ddb94a0dd")
	(63 "ecd42a703a4e93e163d60d55e3785b1a763838b0351bc2e6f7c94b4bfb24f9aa15da5d744ebcebe11f0fc4315d45ba3a047b6e60e07448357f2795bf34b73502")
	(64 "8f3cc30b3fb5bf963688a46488249248ac2c67f0f85a145233c6c1e3c16dcd1df634c07d1d31da02576f65b9cf64e1c3fdb318b689b8a14e2e9552bcf30fb133")
	(111 "68cffa6d0d76f309c9ce0d35280939f8e25990c43b7b086ccdf709be35b07d4ddba599541ff2b1c19d34ea49aeafb9659adb7ac3c0b078bb30a22d57fc6687ef")
	(112 "d0865c524d1dddf7c23b799c413f5adcd7caefd3f66a9b49750ec81066012c25a8bcf94ddea6dc525691673097ca40e0101e897fc97218cfdb0704084e2bef4b")
	(113 "606314353bd419f9f7720e8297f2af8a9ed5f0bb0bed29b716bf18e34577623ae7effa46f496bd4282134e7e895284a4760a910d2ad8ec11e9dd75863058cd9c")
	(119 "236bdd7f38a611b5014b239245c381ae5d20a96f1e5b3178227c00056b7fa8c44ef54880085d82b7e20cd65f2bfda1326696c3f94a6a5bad0cb5ce289aa46167")
	(120 "4bd16fefaa35cc2df9fbb8ff379ec04a4070ffd5d4992574af239fa175534df87cbedceeb96be08e15090a78b83a328a2900c5411683ab47bab914591048548f")
	(127 "e0b6a20f1c0c88970a9340152cd5a1c1ecf3d3b8de55102741879438079473540133b812706e5dbec322c8c9523b6fc8c6d16ee626e87ad5fe3d2916afedc369")
	(128 "99b16f17aa0b969a5b8f08f367719d516e330ccd2660b6f0688ec031dbc783de50a1cd185a2568dba75070a2403d17d4741d163578515dfd2ff756ddfe4d47b1")
	(129 "a1556e29185778aa5991e34b8884c840d589f0fbb4b8ed590e51e9ac4eb03a008125000db2671f8fe7f485b59a77b518670078ecb41a54b4cd02a7f1d2ca4c6d")
	(239 "18ee83f30261c3c645d52aee6a209105b25bba39d33845ef48984cc238e4f21661fb7bd7dd4336f71c40fe87d95e5115d6c7be52e0d3e7e7877d24500b5b58df")
	(240 "9d60ee60d29ec4fa0b9690c04c1c29413bbe3ed345639182d9d53dcc05926b77b04f4fec1562fb85182954c96b7cbb5d5e4410251ff4f352d09a2da90419fb13")
	(255 "c2e3bb67012f9eb526202efa59997933f7d3e75e7ded738818bc27d94977f4573afddb1b2793745701e62affa3b7a1c8262c992a321f488a6b1942a4795bab98")
	(256 "e49c208e41556e859d1a52d14784a061c2d5ae2c8690a5360e9f9344f60861c1362a9ec05a9f08a4167b3da41bdd122a387413dd06976470e4beff5053f2ac71")
	(257 "a8f13f5f1f09e5c21370bd3e0f3a60ce9987663b95068a2cc9a2bc5cbfdc4c34611325699468e00350bd2e06a62c6cad8710001d407971f5e4e3d1dc6b484fdd")
	(1000 "00e36fccf193e59697a92b5ab24666ce6326d7fa16bf10832d0991ddc591112e9dfa6a636950ed9c4d67344a760654c2ff7785e1d60094d651038735b5dccabd")))

;added a part at a time, in parts of several sizes, is the hash of the whole
(defq s5_whole (s5-data 1000) s5_want (sha512 s5_whole))
(each (lambda (size)
	(defq ctx (sha512-start) at 0)
	(while (< at 1000)
		(sha512-add ctx (slice s5_whole at (min 1000 (+ at size))))
		(setq at (+ at size)))
	(assert-eq (cat "added " (str size) " bytes at a time") s5_want (sha512-end ctx)))
	'(1 7 64 127 128 129 500 1000))

;what the native code refuses
(defq s5_state (first (sha512-start)))
(assert-error "a state that is not 64 bytes" (sha512-blocks "short" (s5-data 128) 0 1))
(assert-error "blocks that are not all in the data" (sha512-blocks (cat s5_state) (s5-data 128) 1 1))
(assert-error "a count below 0" (sha512-blocks (cat s5_state) (s5-data 128) 0 -1))
(assert-eq "no blocks is no change" s5_state (sha512-blocks (cat s5_state) (s5-data 128) 0 0))

(undef (env) 's5_whole 's5_want 's5_state)
