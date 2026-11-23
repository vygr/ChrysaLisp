;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a module of AHX, Abyss' Highest eXperience, the Amiga's synth tracker,
; read and played as HivelyTracker's replayer plays one, by Pete Gordon,
; BSD licensed. No samples: each of its four voices plays a waveform it
; makes, a triangle, a sawtooth, a square of a width that moves, or white
; noise, of 4 to 128 bytes, each filtered to one of 63 by a filter that
; moves, played by its instrument's list of what to do each frame, under
; an envelope. A voice's waveform is repeated into a buffer of 640 bytes
; the mixer plays round and round, made again each frame it changes.
; And HivelyTracker's own, HVL: 4 to 16 voices, two effects a step, and a
; ring, a voice times a triangle or a sawtooth of a note of its own, the
; ring's buffer the mixer's too.
;
; Its song is as a tracker's of lib/media/mod.inc to be shown, an order
; a position of its tracks, and as the replayer has it to be played: its
; :positions, (tracks transposes) each, its :tracks of a step a row,
; (note instrument effect number effect number), and its :instruments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./mod.inc")

(defq +ahx_buffer 640 +ahx_block 6520
	;the period of each note, 0 none, 1 C-1 to 60 B-5
	+ahx_periods ''(
		0 3424 3232 3048 2880 2712 2560 2416 2280 2152 2032 1920 1812 1712 1616 1524
		1440 1356 1280 1208 1140 1076 1016 960 906 856 808 762 720 678 640 604
		570 538 508 480 453 428 404 381 360 339 320 302 285 269 254 240
		226 214 202 190 180 170 160 151 143 135 127 120 113)
	;the vibrato's sine, a whole wave of 64
	+ahx_vib ''(0 24 49 74 97 120 141 161 180 197 212 224 235 244 250 253
		255 253 250 244 235 224 212 197 180 161 141 120 97 74 49 24
		0 -24 -49 -74 -97 -120 -141 -161 -180 -197 -212 -224 -235 -244 -250 -253
		-255 -253 -250 -244 -235 -224 -212 -197 -180 -161 -141 -120 -97 -74 -49 -24)
	;the length less one of each wave of a block: 6 triangles, 6 sawtooths,
	;32 squares and the white noise
	+ahx_lentab ''(3 7 15 31 63 127 3 7 15 31 63 127
		127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127
		127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 1919)
	;the state each wave of a filter starts from, its middle then its low,
	;45 each of the 31 filters, HivelyTracker's filter_thing
	+ahx_filter ''(
		-1161 -4413 -7161 -13094 635 13255 2189 6401 9041 16130 13460 5360 6349 12699 19049 25398 30464 32512 32512 32515
		31625 29756 27158 24060 20667 17156 13970 11375 9263 7543 6142 5002 4074 3318 2702 2178 1755 1415 1141 909
		716 563 444 331 -665 -2082 -6170 -9235 -13622 12545 9617 3951 8345 11246 18486 6917 3848 8635 17271 25907
		32163 32512 32455 30734 27424 23137 18397 13869 10429 7843 5897 4435 3335 2507 1885 1389 1023 720 530 353
		260 173 96 32 -18 -55 -79 -92 -95 -838 -3229 -7298 -12386 -7107 13946 6501 5970 9133 14947 16881
		6081 3048 10921 21843 31371 32512 32068 28864 23686 17672 12233 8469 5862 4058 2809 1944 1346 900 601 371
		223 137 64 7 -34 -58 -69 -70 -63 -52 -39 -26 -14 -5 4984 -4476 -8102 -14892 2894 12723
		4883 8010 9750 17887 11790 5099 2520 13207 26415 32512 32457 28690 22093 14665 9312 5913 3754 2384 1513 911
		548 330 143 3 -86 -130 -139 -125 -97 -65 -35 -11 6 15 19 19 16 12 8 6877
		-5755 -9129 -15709 9705 10893 4157 9882 10897 19236 8153 4285 2149 15493 30618 32512 30220 22942 14203 8241 4781
		2774 1609 933 501 220 81 35 2 -18 -26 -25 -20 -13 -7 -1 2 4 4 3 2
		1 0 0 -1 2431 -6956 -10698 -14594 12720 8980 3714 10892 12622 19554 6915 3745 1872 17779 32512 32622
		26286 16302 8605 4542 2397 1265 599 283 45 -92 -141 -131 -93 -49 -14 8 18 18 14 8
		3 0 -2 -3 -2 -2 -1 0 0 -3654 -8008 -12743 -11088 13625 7342 3330 11330 14859 18769 6484
		3319 1660 20065 32512 30699 21108 10616 5075 2425 1159 477 196 1 -93 -109 -82 -44 -12 7 14
		13 9 4 0 -2 -2 -1 -1 0 0 0 0 0 0 -7765 -8867 -14957 -5862 13550 6139
		2988 11284 17054 16602 6017 2979 1489 22351 32512 28083 15576 6708 2888 1243 535 188 32 -47 -64 -47
		-22 -3 7 8 5 3 0 -1 -1 -1 0 0 0 0 0 0 0 0 0 -9079
		-9532 -16960 -335 13001 5333 2704 11192 18742 13697 5457 2703 1351 24637 32512 24556 10851 4185 1614 622 184
		15 -57 -59 -34 -9 5 8 6 2 0 -1 -1 0 0 0 0 0 0 0 0
		0 0 0 0 -8576 -10043 -18551 4372 12190 4809 2472 11230 19803 11170 4953 2473 1236 26923 32512 20567
		7430 2550 875 212 51 -30 -43 -25 -6 3 5 3 1 0 -1 0 0 0 0 0
		0 0 0 0 0 0 0 0 0 -6960 -10485 -19740 7864 11223 4449 2279 11623 20380 9488 4553
		2280 1140 29209 31829 16235 4924 1493 452 86 -7 -32 -20 -5 2 3 2 0 0 0 0
		0 0 0 0 0 0 0 0 0 0 0 0 0 0 -4739 -10974 -19831 10240 10190 4169
		2114 12524 20649 8531 4226 2114 1057 31495 29672 11916 3168 841 121 17 -22 -18 -5 2 2 1
		0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -2333
		-11641 -19288 11765 9175 3923 1971 13889 20646 8007 3942 1971 985 32512 27426 8446 1949 449 45 -11 -16
		-5 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		0 0 0 0 29 -12616 -17971 12690 8247 3693 1846 15662 20271 7658 3692 1846 923 32512 25132 6284
		1245 246 -71 -78 -17 8 7 1 -1 0 0 0 0 0 0 0 0 0 0 0
		0 0 0 0 0 0 0 0 0 2232 -14001 -15234 13198 7447 3478 1736 17409 19411 7332 3472
		1736 868 32512 22545 4352 731 18 -117 -40 8 9 2 -1 -1 0 0 0 0 0 0
		0 0 0 0 0 0 0 0 0 0 0 0 0 0 4197 -15836 -11480 13408 6791 3281
		1639 19224 18074 6978 3276 1639 819 32512 19657 2706 380 -148 -86 2 13 3 -2 0 0 0
		0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 5863
		-17878 -9460 13389 6270 3104 1551 20996 16431 6616 3102 1551 776 32512 16633 1921 221 -95 -39 5 5
		0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		0 0 0 0 7180 -20270 -6194 13181 5866 2946 1473 22548 14746 6273 2946 1473 737 32512 13621 1263
		116 -53 -15 4 2 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		0 0 0 0 0 0 0 0 0 8117 -21129 -2795 12809 5550 2804 1402 23717 13326 5962 2804
		1402 701 32512 10687 776 -56 -56 4 4 0 0 0 0 0 0 0 0 0 0 0
		0 0 0 0 0 0 0 0 0 0 0 0 0 0 8560 -19953 508 12299 5295 2675
		1337 25109 12263 5684 2675 1338 669 32512 7905 433 -36 -22 3 1 0 0 0 0 0 0
		0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8488
		-18731 3672 11679 5080 2558 1279 26855 11480 5434 2557 1279 639 32512 5357 212 -95 0 4 -1 0
		0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		0 0 0 0 7977 -24055 6537 10986 4883 2450 1225 28611 10918 5206 2450 1225 612 32512 3131 83
		-35 2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		0 0 0 0 0 0 0 0 0 7088 -30584 9054 10265 4696 2351 1176 28707 10494 4996 2351
		1175 588 32512 1920 -155 -13 4 -1 0 0 0 0 0 0 0 0 0 0 0 0
		0 0 0 0 0 0 0 0 0 0 0 0 0 0 5952 -32627 11249 9564 4519 2260
		1130 28678 10113 4803 2260 1130 565 32512 1059 -73 -1 0 0 0 0 0 0 0 0 0
		0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4629
		-32753 13199 8934 4351 2175 1088 28446 9775 4623 2175 1087 544 32512 434 -22 1 0 0 0 0
		0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		0 0 0 0 3132 -32768 15225 8430 4194 2097 1049 30732 9439 4456 2097 1049 524 32512 75 -6
		0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		0 0 0 0 0 0 0 0 0 1345 -32768 16765 8107 4048 2025 1012 32512 9112 4302 2025
		1012 506 32385 392 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		0 0 0 0 0 0 0 0 0 0 0 0 0 0 -706 -32768 17879 8005 3913 1956
		978 32512 8843 4157 1957 978 489 31184 1671 122 10 0 0 0 0 0 0 0 0 0
		0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -3050
		-32768 18923 8163 3799 1893 946 32512 8613 4022 1893 945 473 29903 3074 316 52 11 3 0 0
		0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		0 0 0 0 -5812 -32768 19851 8626 3739 1833 917 32512 7982 3889 1833 916 459 28541 4567 731
		206 66 23 8 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		0 0 0 0 0 0 0 0 0 -9235 -32768 20587 9408 3841 1784 889 32512 6486 3688 1776
		889 447 27099 6112 1379 313 135 65 33 17 7 4 2 2 2 2 2 2 2 2
		2 2 2 2 2 2 2 2 2 2 2 2 2 2 -12713 1188 1318 -1178 -4304 -26320
		-14931 -1716 -1486 2494 3611 22275 27450 -31839 -29668 -26258 -21608 -15880 -9560 -3211 3138 9369 15281 20717 25571 29774
		32512 32512 32512 32512 32512 32512 32512 32512 32512 32512 32512 32512 32512 32748 32600 32750 32566 32659 32730 8886
		1762 506 -1665 -12112 -24641 -8513 -2224 247 3288 9926 25787 28909 -31048 -27034 -20726 -12532 -3896 4733 13043 20568
		27010 32215 32512 32512 32512 32512 32512 32512 32512 32762 32696 32647 32512 32665 32512 32587 32638 32669 32681 32679
		32667 32648 32624 32598 6183 2141 -630 -2674 -21856 -18306 -5711 -2161 2207 4247 17616 26475 29719 -30017 -23596 -13741
		-2819 8029 18049 26470 32512 32512 32512 32512 32512 32512 32512 32738 32663 32612 32756 32549 32602 32629 32636 32628
		32610 32588 32564 32542 32524 32510 32500 32494 32492 3604 2248 -1495 -5612 -26800 -13545 -4745 -1390 3443 6973 23495
		27724 30246 -28745 -19355 -6335 6861 19001 28690 32512 32512 32512 32512 32512 32512 32512 32512 32667 32743 32757 32730
		32681 32624 32572 32529 32500 32482 32476 32477 32482 32489 32497 32504 32509 32513 7977 1975 -1861 -9752 -25893 -10150
		-4241 86 4190 10643 25235 28481 30618 -27231 -14398 1096 15982 27872 32512 32512 32512 32512 32512 32734 32631 32767
		32531 32553 32557 32551 32539 32527 32516 32509 32505 32504 32505 32506 32508 32510 32511 32512 32512 32512 32511 14529
		1389 -2028 -14813 -22765 -7845 -3774 1986 4706 14562 25541 29019 30894 -25476 -9294 8516 23979 32512 32512 32512 32512
		32512 32512 32708 32762 32727 32654 32579 32522 32490 32478 32480 32488 32498 32507 32512 32515 32515 32514 32513 32512
		32510 32510 32510 32510 17663 557 -2504 -19988 -19501 -6436 -3340 4135 5461 18788 26016 29448 31107 -23481 -4160 15347
		30045 32512 32512 32512 32512 32512 32674 32700 32654 32586 32531 32498 32486 32488 32496 32504 32510 32513 32514 32513
		32512 32511 32510 32510 32510 32510 32510 32510 32510 16286 -402 -3522 -23951 -16641 -5631 -2983 6251 6837 22781 26712
		29788 31277 -21244 1108 21806 32512 32512 32512 32512 32695 32576 32622 32600 32557 32520 32501 32496 32500 32505 32509
		32512 32512 32512 32511 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 13436 -1351 -4793 -25948 -14224 -5151
		-2702 7687 8805 25705 27348 30064 31415 -18766 5872 26652 32512 32512 32512 32747 32581 32620 32586 32540 32508 32497
		32499 32505 32510 32512 32512 32512 32511 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 10427
		-2162 -7136 -26147 -12195 -4810 -2474 8723 11098 27251 27832 30293 31530 -16047 10877 30990 32512 32512 32512 32512 32584
		32571 32536 32511 32502 32503 32507 32510 32512 32512 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511
		32511 32511 32511 32511 7797 -2748 -10188 -25174 -10519 -4515 -2281 9397 13473 27937 28213 30487 31627 -13087 15816 32512
		32512 32512 32715 32550 32560 32534 32512 32505 32506 32508 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511
		32511 32511 32511 32511 32511 32511 32511 32511 32511 5840 -3084 -13327 -23617 -9177 -4231 -2116 9892 15843 28292 28538
		30652 31710 -9886 20235 32512 32512 32512 32512 32550 32534 32514 32507 32507 32510 32511 32511 32511 32511 32511 32511
		32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 4592 -3215 -15898 -21856 -8141 -3958
		-1972 10401 18229 28612 28824 30796 31781 -7103 24037 32512 32512 32745 32535 32534 32517 32508 32508 32509 32510 32510
		32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 3964
		-3262 -18721 -20087 -7368 -3705 -1847 11014 20634 28996 29075 30920 31843 -4732 27243 32512 32512 32648 32627 32530 32495
		32500 32510 32512 32512 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511
		32511 32511 32511 32511 3858 -3404 -21965 -18398 -6801 -3479 -1738 12009 22960 29429 29294 31030 31898 -2281 30194 32512
		32512 32699 32569 32496 32496 32509 32513 32512 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510
		32510 32510 32510 32510 32510 32510 32510 32510 32510 4177 -3869 -24180 -16820 -6380 -3280 -1640 13235 25035 29863 29490
		31128 31947 251 32758 32512 32749 32652 32508 32490 32507 32513 32512 32511 32511 32511 32511 32511 32511 32511 32511
		32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 4837 -4913 -26436 -15364 -6056 -3103
		-1553 14759 26704 30256 29664 31215 31991 2863 32512 32512 32657 32580 32503 32501 32510 32512 32510 32510 32510 32510
		32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 5755
		-6290 -27702 -14036 -5788 -2947 -1474 16549 27912 30602 29821 31294 32030 5555 32512 32512 32592 32541 32505 32507 32511
		32511 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510
		32510 32510 32510 32510 6898 -8911 -27788 -12841 -5550 -2805 -1403 18509 28687 30906 29963 31364 32066 8328 32512 32512
		32623 32511 32502 32510 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511
		32511 32511 32511 32511 32511 32511 32511 32511 32511 8107 -11465 -27077 -11789 -5325 -2676 -1339 19833 29213 31179 30092
		31429 32098 11181 32512 32512 32561 32508 32508 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511
		32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 9247 -13203 -25808 -10886 -5109 -2559
		-1280 21060 29636 31428 30209 31488 32127 14114 32512 32681 32529 32502 32511 32511 32511 32511 32511 32511 32511 32511
		32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 10252
		-16863 -24251 -10137 -4902 -2451 -1226 21937 30022 31656 30317 31542 32154 17128 32512 32581 32514 32508 32511 32511 32511
		32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511
		32511 32511 32511 32511 11032 -22427 -22598 -9535 -4705 -2353 -1177 20999 30406 31867 30415 31591 32179 20222 32512 32591
		32501 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511
		32511 32511 32511 32511 32511 32511 32511 32511 32511 11539 -19778 -20962 -9060 -4522 -2261 -1131 19486 30789 32061 30507
		31637 32201 23396 32512 32535 32508 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511
		32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 11803 -12759 -19353 -8690 -4353 -2177
		-1089 18499 31165 32240 30591 31678 32222 26651 32512 32514 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511
		32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 11826
		-7586 -17510 -8384 -4196 -2099 -1050 26861 31521 32406 30669 31718 32241 29986 32585 32510 32511 32511 32511 32511 32511
		32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511 32511
		32511 32511 32511 32511 11599 -2848 -15807 -8097 -4051 -2025 -1014 30693 31850 32561 30743 31755 32261 32512 32524 32510
		32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510
		32510 32510 32510 32510 32510 32510 32510 32510 32510 11037 -5302 -14051 -7770 -3913 -1958 -980 28033 32165 32705 30810
		31789 32278 32512 32729 32536 32513 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510
		32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 10114 -7837 -12293 -7348 -3782 -1894
		-948 24926 32473 32512 30873 31819 32294 32512 32512 32580 32527 32515 32512 32510 32510 32510 32510 32510 32510 32510
		32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 8759
		-10456 -10591 -6766 -3638 -1835 -917 24058 32600 32512 30934 31850 32309 32512 32512 32729 32591 32537 32520 32514 32510
		32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510 32510
		32510 32510 32510 32510 6811 -13156 -9045 -5965 -3421 -1776 -890 31582 32246 32512 30988 31878 32324 32512 32512 32512
		32628 32573 32541 32526 32518 32514 32513 32512 32512 32512 32512 32512 32512 32512 32512 32512 32512 32512 32512 32512
		32512 32512 32512 32512 32512 32512 32512 32512 32512 4835)
	;the place of a voice left and right, by a module's stereo of 0 to 4:
	;an AHX's the fifth, hard left and right as the Amiga's
	+ahx_stereo_left ''(128 96 64 32 0) +ahx_stereo_right ''(128 160 193 225 255)
	;where a waveform of each length starts in a block of them
	+ahx_offsets ''(0 4 12 28 60 124))

(defun ahx-s8 (v)
	; (ahx-s8 v) -> num
	;the low byte of a number, -128 to 127
	(sign-extend (logand v 0xff) 8))

(defun ahx-s32 (v)
	; (ahx-s32 v) -> num
	;the low 32 bits of a number, signed
	(sign-extend (logand v 0xffffffff) 32))

(defun ahx-text (data at)
	; (ahx-text data at) -> (str next)
	;a name to its 0, of 128 at most, and where the one after it starts:
	;past the file's end there is none, and none after it
	(cond
		((>= at (length data)) (list "" at))
		(:t (defq span (slice data at (min (length data) (+ at 128))) end (ifn (find "\x00" span) (length span)))
			(list (mod-text data at end) (+ at end 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the waveforms, made once, and the pan's tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ahx-clip (v)
	; (ahx-clip v) -> num
	;a filter's value, 8.16, held to a byte's range
	(defq top (>>> v 16))
	(cond ((> top 127) (const (<< 127 16))) ((< top -128) (const (neg (<< 128 16)))) (v)))

(defun ahx-filter-block (wave freq mids lows)
	; (ahx-filter-block wave freq mids lows) -> (low high)
	;a block of the waveforms filtered by one of the filters, low and high
	;passed, each wave from where its filter was left, (mids lows)
	(defq low_out (list) high_out (list) at 0)
	(each (lambda (len mid low)
			(setq mid (<< mid 8) low (<< low 8))
			(each! (lambda ()
					(defq in (<< (elem-get wave (+ at (!))) 16)
						high (ahx-clip (- in mid low)))
					(setq mid (ahx-clip (+ mid (* (>>> high 8) freq)))
						low (ahx-clip (+ low (* (>>> mid 8) freq))))
					(push high_out (>>> high 16))
					(push low_out (>>> low 16)))
				'() 0 (inc len))
			(setq at (+ at len 1)))
		+ahx_lentab mids lows)
	(list low_out high_out))

(defun ahx-wave-make ()
	; (ahx-wave-make) -> str
	;the 63 blocks of waveforms, the 31 low passed, the plain, the 31 high
	;passed: in a block the triangles and sawtooths of 4 to 128, the 32
	;squares of 128 each wider, and 1920 of white noise
	(defq wave (list))
	;triangles
	(each (lambda (len)
			(defq d5 (>> len 2) d1 (/ 128 d5) v 0 one (list))
			(times d5 (push one v) (setq v (+ v d1)))
			(push one 127)
			(when (/= d5 1)
				(setq v 128)
				(times (dec d5) (setq v (- v d1)) (push one (ahx-s8 v))))
			(defq half (slice one (- (length one) (>> len 1)) -1))
			(each (# (push one (if (= %0 127) -128 (ahx-s8 (neg %0))))) half)
			(setq wave (cat wave (map (const ahx-s8) one))))
		'(4 8 16 32 64 128))
	;sawtooths
	(each (lambda (len)
			(defq add (/ 256 (dec len)))
			(setq wave (cat wave (map! (# (ahx-s8 (+ -128 (* add (!))))) '() 0 len))))
		'(4 8 16 32 64 128))
	;squares
	(each! (lambda ()
			(defq i (inc (!)))
			(setq wave (cat wave (map! (# (if (< (!) (* 2 (- 64 i))) -128 127)) '() 0 128))))
		'() 0 32)
	;white noise
	(defq ays 0x41595321)
	(times 1920
		(defq s (ahx-s8 ays))
		(when (bits? ays 0x100) (setq s (if (bits? ays 0x8000) -128 127)))
		(push wave s)
		(setq ays (logand (logior (>> ays 5) (<< ays 27)) 0xffffffff)
			ays (logior (logand ays 0xffffff00) (logxor (logand ays 0xff) 0x9a)))
		(defq bx (logand ays 0xffff))
		(setq ays (logand (logior (<< ays 2) (>> ays 30)) 0xffffffff))
		(defq ax (logand ays 0xffff))
		(setq bx (logand (+ bx ax) 0xffff) ax (logxor ax bx)
			ays (logior (logand ays 0xffff0000) ax)
			ays (logand (logior (>> ays 3) (<< ays 29)) 0xffffffff)))
	;the filtered, by 31 filters each of a frequency 9 more than the last
	(defq lows (list) highs (list) freq 25)
	(each! (lambda ()
			(defq f (!) mids (slice +ahx_filter (* f 45) (* (inc f) 45))
				lo (slice +ahx_filter (+ 1395 (* f 45)) (+ 1395 (* (inc f) 45))))
			(bind '(low high) (ahx-filter-block wave freq mids lo))
			(push lows low)
			(push highs high)
			(setq freq (+ freq 9)))
		'() 0 31)
	(defq out (string-stream (cat "")))
	(each (# (write-char out %0 1)) lows)
	(write-char out wave 1)
	(each (# (write-char out %0 1)) highs)
	(str out))

(defun ahx-waves ()
	; (ahx-waves) -> str
	;the waveforms, made the first time they are needed by this task
	(ifn *ahx_waves* (setq *ahx_waves* (ahx-wave-make))))

(defq *ahx_waves* :nil)

(defun ahx-pans ()
	; (ahx-pans) -> (left right)
	;the 256 of each side's share of a pan, a quarter of a sine, as the
	;replayer works them out
	(defq step (/ (/ (const (n2d 314159265)) (const (n2d 100000000))) (const (n2d 512)))
		aa (/ (n2d (n2f (/ (const (n2d 314159265)) (const (n2d 100000000))))) (const (n2d 2)))
		ab (const (n2d 0)) left (list) right (list))
	(times 256
		(push left (n2i (* (sin aa) (const (n2d 255)))))
		(push right (n2i (* (sin ab) (const (n2d 255)))))
		(setq aa (+ aa step) ab (+ ab step)))
	(elem-set left 255 0)
	(elem-set right 0 0)
	(list left right))

;;;;;;;;;;;;;
; the module
;;;;;;;;;;;;;

(defun ahx-read (data &optional bare)
	; (ahx-read data [bare]) -> :nil | song
	;the song of an AHX or a HivelyTracker module's bytes, :nil if they are
	;not one. HivelyTracker's, HVL, has 4 to 16 voices, a second effect a
	;step, a step of none a byte, each voice's place left to right, a gain,
	;and lines of 5 bytes in an instrument's list, of more commands
	(defq magic (if (> (length data) 16) (slice data 0 3) "") hvl (eql magic "HVL"))
	(when (and (or hvl (eql magic "THX")) (<= (code data 1 3) 1))
		(defq b6 (code data 1 6) b8 (code data 1 8) posn (logior (<< (logand b6 0x0f) 8) (code data 1 7))
			restart (if hvl (logior (<< (logand b8 3) 8) (code data 1 9)) (code-be data 2 8))
			channels (if hvl (+ 4 (>> b8 2)) 4) mult (inc (logand (>> b6 5) 3))
			trkl (code data 1 10) trkn (code data 1 11) insn (code data 1 12) ssn (code data 1 13)
			version (code data 1 3) at (+ (if hvl 16 14) (* 2 ssn))
			stereo (if hvl (min 4 (code data 1 15)) 4)
			gain (if hvl (/ (<< (code data 1 14) 8) 100) 256))
		(when (and (<= 1 posn 1000) (<= 1 trkl 64) (<= insn 64) (<= channels 16))
			(if (>= restart posn) (setq restart (dec posn)))
			(bind '(title names) (ahx-text data (code-be data 2 4)))
			;the positions, a track and a transpose of each voice
			(defq positions (map! (lambda ()
					(defq p (+ at (* 2 channels (!))))
					(list (map! (# (mod-u8 data (+ p (* 2 (!))))) '() 0 channels)
						(map! (# (mod-s8 data (+ p (* 2 (!)) 1))) '() 0 channels)))
				'() 0 posn))
			(setq at (+ at (* 2 channels posn)))
			;the tracks, a step a row of note, instrument and two effects and
			;their numbers, the first of none if so
			(defq tracks (map! (lambda ()
					(cond
						((and (bits? b6 0x80) (= (!) 0)) (fill (nums) (* 6 trkl) 0))
						(:t (defq t (nums))
							(times trkl
								(defq b0 (mod-u8 data at) b1 (mod-u8 data (inc at)) b2 (mod-u8 data (+ at 2)))
								(cond
									((not hvl)
										(push t (logand (>> b0 2) 0x3f) (logior (<< (logand b0 3) 4) (>> b1 4))
											(logand b1 15) b2 0 0)
										(setq at (+ at 3)))
									((= b0 0x3f) (push t 0 0 0 0 0 0) (setq at (inc at)))
									(:t (push t b0 b1 (>> b2 4) (mod-u8 data (+ at 3)) (logand b2 15) (mod-u8 data (+ at 4)))
										(setq at (+ at 5)))))
							t)))
				'() 0 (inc trkn)))
			;the instruments, from 1, each 22 bytes and its list of 4 a line,
			;or of HivelyTracker's 5
			(defq name_at names instruments (list (scatter (Emap) :name "" :volume 0)) line (if hvl 5 4))
			(times insn
				(bind '(name next) (ahx-text data name_at))
				(setq name_at next)
				(defq b (map! (# (mod-u8 data (+ at (!)))) '() 0 22) len (elem-get b 21) plist (list))
				(setq at (+ at 22))
				(times len
					(defq e (map! (# (mod-u8 data (+ at (!)))) '() 0 line))
					(defq e0 (first e) e1 (second e) e2 (third e) e3 (elem-get e 3))
					(cond
						(hvl (push plist (list (logand e2 0x3f) (logand e1 7) (logand (>> e2 6) 1)
							(list (logand e0 15) (logand (>> e1 3) 15)) (list e3 (elem-get e 4)))))
						(:t (defq k (elem-get '(0 1 2 3 4 5 12 15) (logand (>> e0 5) 7))
								l (elem-get '(0 1 2 3 4 5 12 15) (logand (>> e0 2) 7)))
							;a filter's toggle in a list of the first AHX, before its filters
							(and (= version 0) (= l 4) (/= (logand e2 0xf0) 0) (setq e2 (logand e2 0x0f)))
							(and (= version 0) (= k 4) (/= (logand e3 0xf0) 0) (setq e3 (logand e3 0x0f)))
							(push plist (list (logand e1 0x3f) (logior (logand (<< e0 1) 6) (>> e1 7)) (logand (>> e1 6) 1)
								(list l k) (list e2 e3)))))
					(setq at (+ at line)))
				(push instruments (scatter (Emap) :name name :volume (elem-get b 0)
					:filter_speed (logior (logand (>> (elem-get b 1) 3) 0x1f) (logand (>> (elem-get b 12) 2) 0x20))
					:wave_length (logand (elem-get b 1) 7)
					:env (slice b 2 9) :filter_lower (logand (elem-get b 12) 0x7f) :vib_delay (elem-get b 13)
					:hard_cut_frames (logand (>> (elem-get b 14) 4) 7) :hard_cut_release (bits? (elem-get b 14) 0x80)
					:vib_depth (logand (elem-get b 14) 15) :vib_speed (elem-get b 15)
					:square_lower (elem-get b 16) :square_upper (elem-get b 17) :square_speed (elem-get b 18)
					:filter_upper (logand (elem-get b 19) 0x3f) :plist_speed (elem-get b 20) :plist plist)))
			;to be shown as a tracker's: an order of each position, its
			;tracks' steps, a note of C-1 13, the first effect of a step that
			;has one
			(defq none (fill (nums) (* 6 trkl) 0)
				left (elem-get +ahx_stereo_left stereo) right (elem-get +ahx_stereo_right stereo))
			(scatter (Emap) :kind (if hvl "HivelyTracker" "AHX") :mode :ahx :pitch :amiga :title title
				:channels channels
				:samples (map! (# (scatter (Emap) :name (. %0 :find :name) :data "" :length 0)) (list instruments) 1 -1)
				:instruments instruments :positions positions :tracks tracks :restart restart :mult mult
				:track_len trkl :version version :mixgain gain
				:orders (map! (# (!)) '() 0 posn) :rows (map! (# trkl) '() 0 posn)
				:patterns (map (lambda (pos)
						(defq cells (nums) tr (first pos))
						(each! (lambda ()
								(defq c (* 6 (!)))
								(each (lambda (t)
										(defq s (if (< t (length tracks)) (elem-get tracks t) none) note (elem-get s c)
											fx (elem-get s (+ c 2)) param (elem-get s (+ c 3)))
										(and (= fx 0) (= param 0) (setq fx (elem-get s (+ c 4)) param (elem-get s (+ c 5))))
										(push cells (if (> note 0) (+ note 12) 0) (elem-get s (inc c)) +no_volume fx param))
									tr))
							'() 0 trkl)
						cells)
					positions)
				;the place of each voice, left right right left, as the Amiga's
				:pans (mod-pans channels left right)
				:speed 6 :tempo 125 :global 64))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the replayer: a step a row, a frame of each voice
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ahx-voice (index pan)
	; (ahx-voice index pan) -> voice
	(scatter (Emap) :index index :next_track 0 :transpose 0 :override 1000
		:adsr_volume 0 :adsr (nums 0 0 0 0 0 0 0) :ins :nil :delta 1
		:instr_period 0 :track_period 0 :vib_period 0 :wave_length 0 :max_volume 0 :perf_sub 0
		:new_wave :nil :waveform 0 :plant_period :nil :voice_volume 0 :plant_square :nil :ignore_square :nil
		:fixed :nil :slide_up 0 :slide_down 0 :hard_cut 0 :hard_cut_release :nil :hard_cut_release_f 0
		:ps_on :nil :ps_speed 0 :ps_period 0 :ps_limit 0 :ps_with_limit :nil
		:pps_speed 0 :pps_period 0 :pps_on :nil
		:vib_delay 0 :vib_speed 0 :vib_current 0 :vib_depth 0
		:sq_on :nil :sq_init :nil :sq_wait 0 :sq_lower 0 :sq_upper 0 :sq_pos 0 :sq_sign 0 :sq_sliding :nil
		:f_on :nil :f_init :nil :f_wait 0 :f_speed 0 :f_upper 0 :f_lower 0 :f_pos 0 :f_sign 0 :f_sliding :nil :f_ignore 0
		:perf_current 0 :perf_speed 0 :perf_wait 0 :plist :nil :source :nil :square :nil
		:delay_on :nil :cut_on :nil :delay_wait 0 :cut_wait 0 :audio_period 0 :audio_volume 0
		:ins_at 0 :wn_random 0x280 :master 64 :pan pan :set_pan pan :buffer_key :nil
		:volume_out 0 :period_out 0 :pan_out pan
		;its ring, HivelyTracker's: on in the mixer, its waveform made again,
		;its step, from the start, where in the waves, its note, fixed or of
		;the track's, triangle or sawtooth, its period, step and buffer
		:ring_mix :nil :ring_new :nil :ring_plant :nil :ring_restart :nil :ring_source :nil
		:ring_base 0 :ring_fixed :nil :ring_wave 0 :ring_period 0 :ring_delta 1 :ring_key :nil))

(defun ahx-step-at (player voice row)
	; (ahx-step-at player voice row) -> (note ins fx param fxb paramb)
	;a voice's step of its track at a row
	(defq song (. player :find :song) tracks (. song :find :tracks)
		t (elem-get (first (elem-get (. song :find :positions) (. player :find :order))) (. voice :find :index))
		s (if (< t (length tracks)) (elem-get tracks t)) c (* 6 row))
	(if s (slice s c (+ c 6)) '(0 0 0 0 0 0)))

(defun ahx-fx1 (player voice fx param)
	; (ahx-fx1 player voice fx param)
	;a step's effects of the song and the volume, the first of three passes
	(case fx
		(0 (if (<= 1 (logand param 15) 9) (. player :insert :pos_jump (logand param 15))))
		((5 10) (scatter voice :slide_down (logand param 15) :slide_up (>> param 4)))
		(7 (defq p (+ (ahx-s8 param) 128))
			(scatter voice :pan p :set_pan p))
		(11 (defq to (+ (* 100 (. player :find :pos_jump)) (logand param 15) (* 10 (>> param 4))))
			(scatter player :pos_jump to :pattern_break :t)
			(if (<= to (. player :find :order)) (. player :insert :end_reached :t)))
		(13 (defq to (+ (logand param 15) (* 10 (>> param 4))))
			(scatter player :pos_jump (inc (. player :find :order)) :pattern_break :t
				:pos_jump_note (if (> to (. (. player :find :song) :find :track_len)) 0 to)))
		(14 (when (and (= (>> param 4) 12) (< (logand param 15) (. player :find :speed)))
			(. voice :insert :cut_wait (logand param 15))
			(if (> (logand param 15) 0) (scatter voice :cut_on :t :hard_cut_release :nil))))
		(15 (. player :insert :speed param)
			(if (= param 0) (. player :insert :end_reached :t)))))

(defun ahx-fx2 (voice fx param note)
	; (ahx-fx2 voice fx param note) -> note
	;a step's slides to a note, the second pass, and the note left to play
	(case fx
		(9 (scatter voice :sq_pos (>> param (- 5 (. voice :find :wave_length))) :ignore_square :t) note)
		((3 5) (and (= fx 3) (/= param 0) (. voice :insert :ps_speed param))
			(when (> note 0)
				(defq diff (- (elem-get +ahx_periods (. voice :find :track_period)) (elem-get +ahx_periods note)))
				(if (/= (+ diff (. voice :find :ps_period)) 0) (. voice :insert :ps_limit (neg diff))))
			(scatter voice :ps_on :t :ps_with_limit :t)
			0)
		(:t note)))

(defun ahx-fx3 (player voice fx param)
	; (ahx-fx3 player voice fx param)
	;a step's other effects, the third pass
	(case fx
		(1 (scatter voice :ps_speed (neg param) :ps_on :t :ps_with_limit :nil))
		(2 (scatter voice :ps_speed param :ps_on :t :ps_with_limit :nil))
		(4 (cond
			((or (= param 0) (= param 0x40)))
			((< param 0x40) (. voice :insert :f_ignore param))
			((<= param 0x7f) (. voice :insert :f_pos (- param 0x40)))))
		(12 (cond
			((<= param 0x40) (. voice :insert :max_volume param))
			((< (setq param (- param 0x50)) 0))
			((<= param 0x40) (each (# (. %0 :insert :master param)) (. player :find :chans)))
			((< (setq param (- param 0x50)) 0))
			((<= param 0x40) (. voice :insert :master param))))
		(14 (defq y (logand param 15))
			(case (>> param 4)
				(1 (scatter voice :ps_period (- (. voice :find :ps_period) y) :plant_period :t))
				(2 (scatter voice :ps_period (+ (. voice :find :ps_period) y) :plant_period :t))
				(4 (. voice :insert :vib_depth y))
				(10 (. voice :insert :max_volume (min 64 (+ (. voice :find :max_volume) y))))
				(11 (. voice :insert :max_volume (max 0 (- (. voice :find :max_volume) y))))
				(15 (and (>= (. (. player :find :song) :find :version) 1) (= y 1)
					(. voice :insert :override (. voice :find :transpose))))))))

(defun ahx-instrument (voice ins)
	; (ahx-instrument voice ins)
	;a voice set going by an instrument: its envelope, waveform, vibrato,
	;square and filter from it, and its list from the start
	(defq wl (. ins :find :wave_length) env (. ins :find :env))
	(bind '(af av df dv sf rf rv) env)
	(scatter voice :pan (. voice :find :set_pan) :ps_speed 0 :ps_period 0 :ps_limit 0 :perf_sub 64
		:adsr_volume 0 :ins ins :restart :t
		:adsr (nums af (if (> af 0) (/ (* av 256) af) (* av 256))
			df (if (> df 0) (/ (* (- dv av) 256) df) (* dv 256)) sf
			rf (if (> rf 0) (/ (* (- rv dv) 256) rf) (* rv 256)))
		:wave_length wl :max_volume (. ins :find :volume) :vib_current 0 :vib_delay (. ins :find :vib_delay)
		:vib_depth (. ins :find :vib_depth) :vib_speed (. ins :find :vib_speed) :vib_period 0
		:hard_cut_release (. ins :find :hard_cut_release) :hard_cut (. ins :find :hard_cut_frames)
		:ignore_square :nil :sq_sliding :nil :sq_wait 0 :sq_on :nil)
	(defq lower (>> (. ins :find :square_lower) (- 5 wl)) upper (>> (. ins :find :square_upper) (- 5 wl))
		d3 (. ins :find :filter_lower) d4 (. ins :find :filter_upper))
	(scatter voice :sq_upper (max lower upper) :sq_lower (min lower upper)
		:f_ignore 0 :f_wait 0 :f_on :nil :f_sliding :nil :f_speed (. ins :find :filter_speed)
		:f_upper (max d3 d4) :f_lower (min d3 d4) :f_pos 32
		:perf_wait 0 :perf_current 0 :perf_speed (. ins :find :plist_speed) :plist (. ins :find :plist)
		;no ring, till its list says, and from its start when it does
		:ring_mix :nil :ring_restart :t :ring_plant :nil :ring_new :nil))

(defun ahx-note-delay (player voice fx param)
	; (ahx-note-delay player voice fx param) -> :go | :held | :done
	;an effect's note delay, EDx: :held, the step left to a later frame of
	;the row, :done, come to again now its wait is over, else :go
	(cond
		((not (and (= fx 14) (= (logand param 0xf0) 0xd0))) :go)
		((. voice :find :delay_on) (. voice :insert :delay_on :nil) :done)
		((< 0 (logand param 15) (. player :find :speed))
			(scatter voice :delay_wait (logand param 15) :delay_on :t)
			:held)
		((< (logand param 15) (. player :find :speed)) (. voice :insert :delay_wait 0) :go)
		(:go)))

(defun ahx-step (player voice)
	; (ahx-step player voice)
	;a voice's step of its track this row, hvl_process_step: a note delay
	;held back to a later frame of the row, by either effect, and each pass
	;of the effects of the first then the second
	(scatter voice :slide_up 0 :slide_down 0)
	(bind '(note ins fx param fxb paramb) (ahx-step-at player voice (. player :find :row)))
	;:go, :held to a later frame, or :done, a delay's wait over: a delay
	;of the second effect only if the first had none
	(defq go (ahx-note-delay player voice fx param))
	(if (eql go :go) (setq go (ahx-note-delay player voice fxb paramb)))
	(unless (eql go :held)
		(if (> note 0) (. voice :insert :override 1000))
		(ahx-fx1 player voice fx param)
		(ahx-fx1 player voice fxb paramb)
		(defq instruments (. (. player :find :song) :find :instruments))
		(when (and (> ins 0) (< ins (length instruments)))
			(ahx-instrument voice (elem-get instruments ins))
			(. voice :insert :ins_at ins))
		(. voice :insert :ps_on :nil)
		(when (> (setq note (ahx-fx2 voice fxb paramb (ahx-fx2 voice fx param note))) 0)
			(scatter voice :track_period note :plant_period :t))
		(ahx-fx3 player voice fx param)
		(ahx-fx3 player voice fxb paramb)))

(defun ahx-plist-fx (voice fx param)
	; (ahx-plist-fx voice fx param)
	;a command of an instrument's list
	(case fx
		(0 (when (< 0 param 0x40)
			(cond
				((/= (. voice :find :f_ignore) 0)
					(scatter voice :f_pos (. voice :find :f_ignore) :f_ignore 0))
				((. voice :insert :f_pos param)))
			(. voice :insert :new_wave :t)))
		(1 (scatter voice :pps_speed param :pps_on :t))
		(2 (scatter voice :pps_speed (neg param) :pps_on :t))
		(3 (if (. voice :find :ignore_square) (. voice :insert :ignore_square :nil)
			(. voice :insert :sq_pos (>> param (- 5 (. voice :find :wave_length))))))
		(4 (cond
			((= param 0)
				(defq on (not (. voice :find :sq_on)))
				(scatter voice :sq_on on :sq_init on :sq_sign 1))
			(:t (when (/= (logand param 15) 0)
					(defq on (not (. voice :find :sq_on)))
					(scatter voice :sq_on on :sq_init on :sq_sign (if (= (logand param 15) 15) -1 1)))
				(when (/= (logand param 0xf0) 0)
					(defq on (not (. voice :find :f_on)))
					(scatter voice :f_on on :f_init on :f_sign (if (= (logand param 0xf0) 0xf0) -1 1))))))
		(5 (. voice :insert :perf_current param))
		;a ring of a triangle or a sawtooth, HivelyTracker's: of a note of
		;its own, or so many up from the track's, or off
		((7 8) (cond
			((<= 1 param 0x3c) (scatter voice :ring_base param :ring_fixed :t)
				(scatter voice :ring_wave (- fx 7) :ring_new :t :ring_plant :t))
			((<= 0x81 param 0xbc) (scatter voice :ring_base (- param 0x80) :ring_fixed :nil)
				(scatter voice :ring_wave (- fx 7) :ring_new :t :ring_plant :t))
			((scatter voice :ring_base 0 :ring_fixed :nil :ring_new :nil :ring_source :nil :ring_mix :nil))))
		;its place, till the next instrument
		(9 (. voice :insert :pan (+ (ahx-s8 param) 128)))
		(12 (cond
			((<= param 0x40) (. voice :insert :max_volume param))
			((< (setq param (- param 0x50)) 0))
			((<= param 0x40) (. voice :insert :perf_sub param))
			((< (setq param (- param 0x50)) 0))
			((<= param 0x40) (. voice :insert :master param))))
		(15 (scatter voice :perf_speed param :perf_wait param))))

(defun ahx-period (voice note fixed)
	; (ahx-period voice note fixed) -> period
	;the period a voice plays a note of its own at: on the track's note and
	;its transpose, or fixed, not, and with the slides and the vibrato
	(unless fixed
		(setq note (+ note (if (/= (. voice :find :override) 1000) (. voice :find :override) (. voice :find :transpose))
			(. voice :find :track_period) -1)))
	(defq period (elem-get +ahx_periods (max 0 (min 60 note))))
	(unless fixed (setq period (+ period (. voice :find :ps_period))))
	(max 0x71 (min 0xd60 (+ period (. voice :find :pps_period) (. voice :find :vib_period)))))

(defun ahx-delayed (player voice)
	; (ahx-delayed player voice)
	;a frame of a note delay: its step read once its wait is over
	(when (. voice :find :delay_on)
		(if (<= (. voice :find :delay_wait) 0) (ahx-step player voice)
			(. voice :insert :delay_wait (dec (. voice :find :delay_wait))))))

(defun ahx-frame (player voice)
	; (ahx-frame player voice)
	;a frame of a voice, hvl_process_frame: a delayed note, a cut, its
	;envelope, slides, vibrato, its instrument's list, the square and the
	;filter moved, and the waveform and the period it plays at
	(defq song (. player :find :song) speed (. player :find :speed))
	(ahx-delayed player voice)
	;a hard cut, if the next row has an instrument
	(when (/= (. voice :find :hard_cut) 0)
		(defq row (inc (. player :find :row)) next_ins (cond
				((< row (. song :find :track_len)) (second (ahx-step-at player voice row)))
				((defq t (. voice :find :next_track) tracks (. song :find :tracks))
					(if (< t (length tracks)) (elem-get (elem-get tracks t) 1) 0))))
		(when (> next_ins 0)
			(defq d1 (max 0 (- speed (. voice :find :hard_cut))))
			(cond
				((not (. voice :find :cut_on))
					(scatter voice :cut_on :t :cut_wait d1 :hard_cut_release_f (- speed d1)))
				((. voice :insert :hard_cut 0)))))
	(when (. voice :find :cut_on)
		(cond
			((<= (. voice :find :cut_wait) 0)
				(. voice :insert :cut_on :nil)
				(cond
					((. voice :find :hard_cut_release)
						(defq adsr (. voice :find :adsr) rf (. voice :find :hard_cut_release_f))
						(elem-set adsr 5 rf)
						(elem-set adsr 6 (if (> rf 0)
							(/ (neg (- (. voice :find :adsr_volume) (<< (elem-get (. (. voice :find :ins) :find :env) 6) 8))) rf) 0))
						(elem-set adsr 0 0) (elem-set adsr 2 0) (elem-set adsr 4 0))
					((. voice :insert :max_volume 0))))
			((. voice :insert :cut_wait (dec (. voice :find :cut_wait))))))
	;the envelope: attack, decay, sustain and release
	(defq adsr (. voice :find :adsr) av (. voice :find :adsr_volume) ins (. voice :find :ins))
	(cond
		((/= (elem-get adsr 0) 0)
			(setq av (+ av (elem-get adsr 1)))
			(elem-set adsr 0 (dec (elem-get adsr 0)))
			(if (<= (elem-get adsr 0) 0) (setq av (<< (elem-get (. ins :find :env) 1) 8))))
		((/= (elem-get adsr 2) 0)
			(setq av (+ av (elem-get adsr 3)))
			(elem-set adsr 2 (dec (elem-get adsr 2)))
			(if (<= (elem-get adsr 2) 0) (setq av (<< (elem-get (. ins :find :env) 3) 8))))
		((/= (elem-get adsr 4) 0) (elem-set adsr 4 (dec (elem-get adsr 4))))
		((/= (elem-get adsr 5) 0)
			(setq av (+ av (elem-get adsr 6)))
			(elem-set adsr 5 (dec (elem-get adsr 5)))
			(if (<= (elem-get adsr 5) 0) (setq av (<< (elem-get (. ins :find :env) 6) 8)))))
	(. voice :insert :adsr_volume av)
	(. voice :insert :max_volume (max 0 (min 64 (+ (. voice :find :max_volume) (. voice :find :slide_up) (neg (. voice :find :slide_down))))))
	;a slide of the period, to a note or on
	(when (. voice :find :ps_on)
		(cond
			((. voice :find :ps_with_limit)
				(defq d0 (- (. voice :find :ps_period) (. voice :find :ps_limit)) d2 (. voice :find :ps_speed))
				(if (> d0 0) (setq d2 (neg d2)))
				(when (/= d0 0)
					(scatter voice :ps_period (if (>= (logxor (+ d0 d2) d0) 0) (+ (. voice :find :ps_period) d2) (. voice :find :ps_limit))
						:plant_period :t)))
			(:t (scatter voice :ps_period (+ (. voice :find :ps_period) (. voice :find :ps_speed)) :plant_period :t))))
	;vibrato
	(when (/= (. voice :find :vib_depth) 0)
		(cond
			((<= (. voice :find :vib_delay) 0)
				(defq cur (. voice :find :vib_current))
				(scatter voice :vib_period (>>> (* (elem-get +ahx_vib cur) (. voice :find :vib_depth)) 7) :plant_period :t
					:vib_current (logand (+ cur (. voice :find :vib_speed)) 0x3f)))
			((. voice :insert :vib_delay (dec (. voice :find :vib_delay))))))
	;the instrument's list, a line each so many frames
	(when (defq plist (. voice :find :plist))
		(cond
			((and ins (< (. voice :find :perf_current) (length plist)))
				(defq wait (. voice :find :perf_wait) over (= wait 128))
				(. voice :insert :perf_wait (setq wait (dec wait)))
				(when (or over (<= (ahx-s8 wait) 0))
					(defq cur (. voice :find :perf_current))
					(scatter voice :perf_current (inc cur) :perf_wait (. voice :find :perf_speed))
					(bind '(pnote pwave pfixed pfx pparam) (elem-get plist cur))
					(when (> pwave 0)
						(scatter voice :waveform (dec pwave) :new_wave :t :pps_speed 0 :pps_period 0))
					(. voice :insert :pps_on :nil)
					(ahx-plist-fx voice (first pfx) (first pparam))
					(ahx-plist-fx voice (second pfx) (second pparam))
					(when (> pnote 0)
						(scatter voice :instr_period pnote :plant_period :t :fixed (= pfixed 1)))))
			((/= (. voice :find :perf_wait) 0) (. voice :insert :perf_wait (dec (. voice :find :perf_wait))))
			((. voice :insert :pps_speed 0))))
	(when (. voice :find :pps_on)
		(. voice :insert :pps_period (- (. voice :find :pps_period) (. voice :find :pps_speed)))
		(if (/= (. voice :find :pps_period) 0) (. voice :insert :plant_period :t)))
	;the square's width moved
	(when (and (= (. voice :find :waveform) 2) (. voice :find :sq_on))
		(. voice :insert :sq_wait (dec (. voice :find :sq_wait)))
		(when (<= (. voice :find :sq_wait) 0)
			(defq d1 (. voice :find :sq_lower) d2 (. voice :find :sq_upper) d3 (. voice :find :sq_pos))
			(when (. voice :find :sq_init)
				(. voice :insert :sq_init :nil)
				(cond
					((<= d3 d1) (scatter voice :sq_sliding :t :sq_sign 1))
					((>= d3 d2) (scatter voice :sq_sliding :t :sq_sign -1))))
			(when (or (= d1 d3) (= d2 d3))
				(if (. voice :find :sq_sliding) (. voice :insert :sq_sliding :nil)
					(. voice :insert :sq_sign (neg (. voice :find :sq_sign)))))
			(scatter voice :sq_pos (+ d3 (. voice :find :sq_sign)) :plant_square :t
				:sq_wait (. ins :find :square_speed))))
	;the filter moved
	(when (. voice :find :f_on)
		(. voice :insert :f_wait (dec (. voice :find :f_wait)))
		(when (<= (. voice :find :f_wait) 0)
			(defq d1 (. voice :find :f_lower) d2 (. voice :find :f_upper) d3 (. voice :find :f_pos)
				fs (. voice :find :f_speed))
			(when (. voice :find :f_init)
				(. voice :insert :f_init :nil)
				(cond
					((<= d3 d1) (scatter voice :f_sliding :t :f_sign 1))
					((>= d3 d2) (scatter voice :f_sliding :t :f_sign -1))))
			(times (if (< fs 4) (- 5 fs) 1)
				(when (or (= d1 d3) (= d2 d3))
					(if (. voice :find :f_sliding) (. voice :insert :f_sliding :nil)
						(. voice :insert :f_sign (neg (. voice :find :f_sign)))))
				(setq d3 (+ d3 (. voice :find :f_sign))))
			(scatter voice :f_pos (max 1 (min 63 d3)) :new_wave :t :f_wait (max 1 (- fs 3)))))
	;a square of its width, of its filter's block, picked from the 128 of it
	(defq waves (ahx-waves) wl (. voice :find :wave_length))
	(when (or (= (. voice :find :waveform) 2) (. voice :find :plant_square))
		(defq at (+ (* (dec (. voice :find :f_pos)) +ahx_block) 504) x (<< (. voice :find :sq_pos) (- 5 wl)))
		(if (> x 32) (setq x (- 64 x)))
		(if (> x 0) (setq at (+ at (<< (dec x) 7))))
		(defq delta (>> 32 wl))
		(scatter voice :square (apply (const cat) (map! (# (slice waves (+ at (* (!) delta)) (+ at (* (!) delta) 1)))
				'() 0 (* 4 (<< 1 wl))))
			:new_wave :t :waveform 2 :plant_square :nil))
	(if (= (. voice :find :waveform) 3) (. voice :insert :new_wave :t))
	;a ring's waveform, of no filter, of the voice's length
	(if (. voice :find :ring_new)
		(. voice :insert :ring_source (+ (const (* 31 +ahx_block)) (* 252 (min 1 (. voice :find :ring_wave)))
			(elem-get +ahx_offsets wl))))
	;where in the waves the waveform is, or the square made
	(when (. voice :find :new_wave)
		(defq wf (. voice :find :waveform))
		(. voice :insert :source (cond
			((= wf 2) :square)
			((= wf 3)
				(defq r (. voice :find :wn_random) at (+ (* (dec (. voice :find :f_pos)) +ahx_block) 4600
					(logand (logand r 0x4ff) (lognot 1))))
				(setq r (ahx-s32 (+ r 2239384))
					r (ahx-s32 (- (logxor (ahx-s32 (+ (logior (logand (>>> r 8) 0xffffffff) (<< r 24)) 782323)) 75) 6735)))
				(. voice :insert :wn_random r)
				at)
			((+ (* (dec (. voice :find :f_pos)) +ahx_block) (* wf 252) (elem-get +ahx_offsets wl))))))
	;a ring's period, as the voice's of its own note, then the voice's
	(if (. voice :find :ring_source)
		(. voice :insert :ring_period (ahx-period voice (. voice :find :ring_base) (. voice :find :ring_fixed))))
	(scatter voice :audio_period (ahx-period voice (. voice :find :instr_period) (. voice :find :fixed))
		:audio_volume (>>> (* (>>> (* (>>> (* (>>> (. voice :find :adsr_volume) 8) (. voice :find :max_volume)) 6)
			(. voice :find :perf_sub)) 6) (. voice :find :master)) 6)))

(defun ahx-delta (player period)
	; (ahx-delta player period) -> num
	;the step a frame, 16.16, round a buffer of 640, of an Amiga's period
	(defq delta (n2i (/ (n2d (/ (const (n2f 232449720320)) (n2f period))) (n2d (. player :find :rate)))))
	(if (> delta (const (<< +ahx_buffer 16))) (setq delta (- delta (const (<< +ahx_buffer 16)))))
	(max 1 delta))

(defun ahx-repeat (bytes)
	; (ahx-repeat bytes) -> str
	;a waveform repeated to 640, the mixer's 16 bits
	(mod-widen (if (= (length bytes) +ahx_buffer) bytes
		(apply (const cat) (map! (# bytes) '() 0 (/ +ahx_buffer (length bytes))))) 0 +ahx_buffer +widen_s8 :nil))

(defun ahx-audio (player voice)
	; (ahx-audio player voice)
	;what a voice gives the mixer, hvl_set_audio: its volume, its step if its
	;period changed, and its buffer if its waveform did, the waveform
	;repeated to 640, and so its ring's
	(scatter voice :voice_volume (. voice :find :audio_volume))
	(when (. voice :find :plant_period)
		(scatter voice :plant_period :nil :delta (ahx-delta player (. voice :find :audio_period))))
	(when (. voice :find :new_wave)
		(defq src (. voice :find :source) wl (. voice :find :wave_length)
			key (if (eql src :square) (cat "s" (. voice :find :square)) src))
		;a buffer made again only if it is of another waveform
		(unless (eql key (. voice :find :buffer_key))
			(. voice :insert :buffer_key key)
			(defq waves (ahx-waves)
				bytes (cond
					((eql src :square) (. voice :find :square))
					((= (. voice :find :waveform) 3) (slice waves src (+ src +ahx_buffer)))
					((slice waves src (+ src (* 4 (<< 1 wl)))))))
			(elem-set (. player :find :samples) (. voice :find :index) (ahx-repeat bytes))))
	(when (. voice :find :ring_plant)
		(scatter voice :ring_plant :nil :ring_delta (ahx-delta player (. voice :find :ring_period))))
	;a ring's buffer, after the voices' in the mixer's samples
	(when (. voice :find :ring_new)
		(defq src (. voice :find :ring_source) len (* 4 (<< 1 (. voice :find :wave_length))) key (+ (* src 1024) len))
		(. voice :insert :ring_mix :t)
		(unless (eql key (. voice :find :ring_key))
			(. voice :insert :ring_key key)
			(elem-set (. player :find :samples) (+ (length (. player :find :chans)) (. voice :find :index))
				(ahx-repeat (slice (ahx-waves) src (+ src len)))))))

(defun ahx-tick (player)
	; (ahx-tick player)
	;one frame of the replayer, hvl_play_irq: a row's steps at its first,
	;every voice's frame, on to the next row at the end of its last, and
	;what each voice gives the mixer, (ahx-voices) of lib/media/tracker.inc
	;puts it there. For a length only, the steps and the delayed ones
	;alone, no frames, no mixer
	(defq song (. player :find :song) chans (. player :find :chans) measure (. player :find :measure))
	(when (= (. player :find :wait) 0)
		(when (. player :find :get_new)
			(defq positions (. song :find :positions) order (. player :find :order)
				pos (elem-get positions order)
				next (elem-get positions (if (= (inc order) (length positions)) 0 (inc order))))
			(each (lambda (voice)
					(defq i (!))
					(scatter voice :transpose (elem-get (second pos) i) :next_track (elem-get (first next) i)))
				chans)
			(. player :insert :get_new :nil))
		(each (# (ahx-step player %0)) chans)
		(. player :insert :wait (. player :find :speed)))
	(if measure
		(each (# (ahx-delayed player %0)) chans)
		(each (# (ahx-frame player %0)) chans))
	(. player :insert :frames_done (inc (. player :find :frames_done)))
	(. player :insert :wait (dec (. player :find :wait)))
	(when (<= (. player :find :wait) 0)
		(unless (. player :find :pattern_break)
			(. player :insert :row (inc (. player :find :row)))
			(when (>= (. player :find :row) (. song :find :track_len))
				(scatter player :pos_jump (inc (. player :find :order)) :pos_jump_note 0 :pattern_break :t)))
		(when (. player :find :pattern_break)
			(scatter player :pattern_break :nil :order (. player :find :pos_jump) :row (. player :find :pos_jump_note))
			(when (>= (. player :find :order) (length (. song :find :positions)))
				(scatter player :end_reached :t :order (. song :find :restart)))
			(scatter player :pos_jump_note 0 :pos_jump 0 :get_new :t)))
	;the song's end is had at the end of a whole frame of so many of these
	(and (. player :find :end_reached) (= (% (. player :find :frames_done) (. song :find :mult)) 0)
		(. player :insert :ended :t))
	(unless measure (each (# (ahx-audio player %0)) chans)))

(defun ahx-frames (player)
	; (ahx-frames player) -> frames
	;how long a frame of the replayer is, 50 a second times the song's
	;speed, whole frames of the rate
	(/ (/ (. player :find :rate) 50) (. (. player :find :song) :find :mult)))
