; Onslaught enums, bits, and game constants

(import "lib/class/struct.inc")

;;;;;;;;;;;;;;;;;;;;;;;
; Screen and dimensions
;;;;;;;;;;;;;;;;;;;;;;;

(defq
	+screen_width 320
	+screen_height 240
	+map_width 128
	+map_height 16
	+tile_width 16
	+tile_height 16
	+panel_height 72
	+game_width +screen_width
	+game_height +screen_height
	+window_width +screen_width
	+window_height (- +screen_height +panel_height) ; 168
	+max_strength 2048
	+max_power (/ +max_strength 2)                  ; 1024
	+frame_rate 30
	+rate (/ 1000000 +frame_rate)
	+dt (/ 1.0 30.0))

;;;;;;;;;;;;;
; Game states
;;;;;;;;;;;;;

(enums +game_state 0
	(enum title menu map scores hiscore battle battle_won battle_lost mind mind_won mind_lost credits oracle demo))

;;;;;;;;;;;;;;;;;;;;
; Game battle levels
;;;;;;;;;;;;;;;;;;;;

(enums +game_level 0
	(enum field seige defend mind))

;;;;;;;;;;;;;
; Menu items
;;;;;;;;;;;;;

(enums +menu 0
	(enum start define difficulty mode sound credits))

;;;;;;;;;
; Sounds
;;;;;;;;;

(enums +sound 0
	(enum explode drip horse spell twang1 twang2 death1 death2 clash1 clash2 hooves roar throw out boar))

;;;;;;;;;;;;;;;
; Sprite flags
;;;;;;;;;;;;;;;

(enums +bfsp 0
	(enum collide action))

(bits +fsp 0
	(bit collide action))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sprite types and bit masks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enums +bftp 0
	(enum horse spearman wizard footman knight balista cannon oil boarrider carpet
		  tower monk beserk skeleton_monk skeleton_horse skeleton enemy_fire
		  item man man_banner enemy_banner))

(bits +ftp 0
	(bit horse spearman wizard footman knight balista cannon oil boarrider carpet
		 tower monk beserk skeleton_monk skeleton_horse skeleton enemy_fire
		 item man man_banner enemy_banner))

;;;;;;;;;;;;;;;;;;
; Map tile flags
;;;;;;;;;;;;;;;;;;

(enums +bfmap 0
	(enum masked stand climb))

(bits +fmap 0
	(bit masked stand climb))

;;;;;;;;;;;;;;;;;;;;;
; Control key bits
;;;;;;;;;;;;;;;;;;;;;

(enums +bfkey 0
	(enum up down left right keya keyb keyc keyd))

(bits +fkey 0
	(bit up down left right keya keyb keyc keyd))

;;;;;;;;;;;;;
; Game flags
;;;;;;;;;;;;;

(enums +bfflag 0
	(enum use select enemy mine title iselect))

(bits +fflag 0
	(bit use select enemy mine title iselect))

;;;;;;;;;;;;;
; Item types
;;;;;;;;;;;;;

(enums +item 0
	(enum gold mase big_bow small_bow naptha helper super_mase super_helper
		  yellow_spell black_spell green_spell white_spell red_spell blue_spell talisman))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sprite frames by image strip
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; frm_64x64_l (horse, skeleton horse)
(enums +frm_64x64_l 0
	(enum horse horse1 horse2 horse3 horse4 horse5
		  skeleton_horse skel_horse1 skel_horse2 skel_horse3 skel_horse4 skel_horse5))

(defq
	+frm_horse +frm_64x64_l_horse
	+frm_skeleton_horse +frm_64x64_l_skeleton_horse)

; frm_64x64 (tower)
(enums +frm_64x64 0
	(enum tower))

(defq
	+frm_tower +frm_64x64_tower)

; frm_64x32_l (boarrider, carpet)
(enums +frm_64x32_l 0
	(enum boarrider boar1 boar2 boar3 boar4 boar5
		  carpet carpet1 carpet2 carpet3 carpet4 carpet5))

(defq
	+frm_boarrider +frm_64x32_l_boarrider
	+frm_carpet +frm_64x32_l_carpet)

; fanatic_l (player man 32x32 and skeletons)
(enums +frm_fanatic 0
	(enum walk w1 w2 w3 w4 w5 w6 w7
		  jump
		  fall
		  climb cl1 cl2 cl3
		  masehit mh1 mh2
		  handbow hb1 hb2
		  crossbow cb1 cb2 cb3
		  naptha na1 na2
		  stance st1
		  duck
		  lmasehit lmh1 lmh2
		  skeleton sk1 sk2 sk3 sk4 sk5 sk6 sk7 sk8 sk9 sk10 sk11 sk12 sk13 sk14
		  skeleton_monk))

(defq
	+frm_walk +frm_fanatic_walk
	+frm_jump +frm_fanatic_jump
	+frm_fall +frm_fanatic_fall
	+frm_climb +frm_fanatic_climb
	+frm_masehit +frm_fanatic_masehit
	+frm_handbow +frm_fanatic_handbow
	+frm_crossbow +frm_fanatic_crossbow
	+frm_naptha +frm_fanatic_naptha
	+frm_stance +frm_fanatic_stance
	+frm_duck +frm_fanatic_duck
	+frm_lmasehit +frm_fanatic_lmasehit
	+frm_skeleton +frm_fanatic_skeleton
	+frm_skeleton_monk +frm_fanatic_skeleton_monk)

; frm_32x32_l (spearman, wizard, footman, knight)
(enums +frm_32x32_l 0
	(enum spearman sm1 sm2 sm3 sm4 sm5 sm6 sm7 sm8 sm9 sm10 sm11
		  wizard wiz1 wiz2 wiz3 wiz4 wiz5 wiz6 wiz7 wiz8 wiz9 wiz10 wiz11
		  footman fm1 fm2 fm3 fm4 fm5 fm6 fm7 fm8 fm9 fm10 fm11 fm12 fm13 fm14 fm15 fm16
		  knight kn1 kn2 kn3 kn4 kn5 kn6 kn7 kn8 kn9 kn10 kn11 kn12 kn13 kn14 kn15 kn16))

(defq
	+frm_spearman +frm_32x32_l_spearman
	+frm_wizard +frm_32x32_l_wizard
	+frm_footman +frm_32x32_l_footman
	+frm_knight +frm_32x32_l_knight)

; frm_32c32_l (balista, cannon, oil, beserk, monk)
(enums +frm_32c32_l 0
	(enum balista bal1 bal2 bal3
		  cannon can1 can2 can3
		  oil oil1 oil2 oil3
		  beserk ber1 ber2 ber3 ber4 ber5
		  monk monk1 monk2 monk3 monk4 monk5))

(defq
	+frm_balista +frm_32c32_l_balista
	+frm_cannon +frm_32c32_l_cannon
	+frm_oil +frm_32c32_l_oil
	+frm_beserk +frm_32c32_l_beserk
	+frm_monk +frm_32c32_l_monk)

; frm_32c32 (big explosion, elemental, faces, title letters, mr smith, flag, power, strength)
(enums +frm_32c32 0
	(enum bigexp bigexp1 bigexp2 bigexp3 bigexp4
		  elemental elemental1 elemental2 elemental3
		  face0 face1 face2 face3 face4 face5 face6 face7
		  letter_o letter_n letter_s letter_l letter_a letter_u letter_g letter_h letter_t
		  smith0 smith1 smith2
		  flag
		  power
		  strength))

(defq
	+frm_bigexp +frm_32c32_bigexp
	+frm_elemental +frm_32c32_elemental
	+frm_faces +frm_32c32_face0
	+frm_letters +frm_32c32_letter_o
	+frm_smith +frm_32c32_smith0
	+frm_flag +frm_32c32_flag
	+frm_power +frm_32c32_power
	+frm_strength +frm_32c32_strength)

; frm_32x16_l (barrow, spear)
(enums +frm_32x16_l 0
	(enum barrow spear))

(defq
	+frm_barrow +frm_32x16_l_barrow
	+frm_spear +frm_32x16_l_spear)

; frm_16x32 (oilshot)
(enums +frm_16x32 0
	(enum oilshot))

(defq
	+frm_oilshot +frm_16x32_oilshot)

; frm_16x64 (sword)
(enums +frm_16x64 0
	(enum sword))

(defq
	+frm_sword +frm_16x64_sword)

; frm_16x16_l (arrow, mase, lmase, hbow, cbow, napth, footadd, knightadd, bkaddon, flame, blood, skeladd)
(enums +frm_16x16_l 0
	(enum arrow
		  mase mase1 mase2
		  lmase lmase1 lmase2
		  hbow hbow1 hbow2
		  cbow cbow1 cbow2 cbow3
		  napth napth1
		  footadd fa1 fa2 fa3 fa4
		  knightadd ka1 ka2 ka3 ka4 ka5
		  bkaddon bka1 bka2 bka3 bka4 bka5
		  flame flame1
		  blood bl1 bl2 bl3
		  skeladd))

(defq
	+frm_arrow +frm_16x16_l_arrow
	+frm_mase +frm_16x16_l_mase
	+frm_lmase +frm_16x16_l_lmase
	+frm_hbow +frm_16x16_l_hbow
	+frm_cbow +frm_16x16_l_cbow
	+frm_napth +frm_16x16_l_napth
	+frm_footadd +frm_16x16_l_footadd
	+frm_knightadd +frm_16x16_l_knightadd
	+frm_bkaddon +frm_16x16_l_bkaddon
	+frm_flame +frm_16x16_l_flame
	+frm_blood +frm_16x16_l_blood
	+frm_skeladd +frm_16x16_l_skeladd)

; frm_16x16 (pole, banners, cball, mine, wizshot, nbomb, demon, fires, sight, gold, shield/weapons, spells, diamond, segments, tends, manbits, blooddrip, fizz, manmind, pshot, mshot, bones)
(enums +frm_16x16 0
	(enum pole
		  banners ban1 ban2 ban3 ban4 ban5 ban6 ban7 ban8 ban9 ban10 ban11 ban12 ban13 ban14 ban15
		  cball
		  mine mine1
		  wizshot ws1 ws2 ws3
		  nbomb nb1 nb2 nb3
		  demon dem1 dem2 dem3
		  fires fire1
		  sight
		  gold
		  shield sh1 sh2 sh3 sh4 sh5 sh6
		  spells sp1 sp2 sp3 sp4 sp5
		  diamond dm1 dm2 dm3 dm4 dm5 dm6 dm7 dm8 dm9
		  segments seg1
		  tends tend1 tend2 tend3
		  manbits mb1 mb2 mb3 mb4
		  blooddrip
		  fizz fiz1 fiz2
		  manmind mm1 mm2 mm3
		  pshot ps1
		  mshot ms1
		  bones))

(defq
	+frm_pole +frm_16x16_pole
	+frm_banners +frm_16x16_banners
	+frm_cball +frm_16x16_cball
	+frm_mine +frm_16x16_mine
	+frm_wizshot +frm_16x16_wizshot
	+frm_nbomb +frm_16x16_nbomb
	+frm_demon +frm_16x16_demon
	+frm_fires +frm_16x16_fires
	+frm_sight +frm_16x16_sight
	+frm_gold +frm_16x16_gold
	+frm_shield +frm_16x16_shield
	+frm_spells +frm_16x16_spells
	+frm_diamond +frm_16x16_diamond
	+frm_segments +frm_16x16_segments
	+frm_tends +frm_16x16_tends
	+frm_manbits +frm_16x16_manbits
	+frm_blooddrip +frm_16x16_blooddrip
	+frm_fizz +frm_16x16_fizz
	+frm_manmind +frm_16x16_manmind
	+frm_pshot +frm_16x16_pshot
	+frm_mshot +frm_16x16_mshot
	+frm_bones +frm_16x16_bones)

; campain 8x8
(enums +frm_campain 0
	(enum oracle twater tswamp tforest tmountain water swamp forest mountain plague crusade rebellion enemy player))

(defq
	+frm_oracle +frm_campain_oracle
	+frm_twater +frm_campain_twater
	+frm_tswamp +frm_campain_tswamp
	+frm_tforest +frm_campain_tforest
	+frm_tmountain +frm_campain_tmountain
	+frm_water +frm_campain_water
	+frm_swamp +frm_campain_swamp
	+frm_forest +frm_campain_forest
	+frm_mountain +frm_campain_mountain
	+frm_plague +frm_campain_plague
	+frm_crusade +frm_campain_crusade
	+frm_rebellion +frm_campain_rebellion
	+frm_enemy +frm_campain_enemy
	+frm_player +frm_campain_player)

;;;;;;;;;;;;;;;;;;;;;
; Item frame mapping
;;;;;;;;;;;;;;;;;;;;;

(defq
	+item_frm_gold +frm_gold
	+item_frm_mase +frm_shield
	+item_frm_big_bow (+ +frm_shield 1)
	+item_frm_small_bow (+ +frm_shield 2)
	+item_frm_naptha (+ +frm_shield 3)
	+item_frm_helper (+ +frm_shield 4)
	+item_frm_super_mase (+ +frm_shield 5)
	+item_frm_super_helper (+ +frm_shield 6)
	+item_frm_yellow_spell +frm_spells
	+item_frm_black_spell (+ +frm_spells 1)
	+item_frm_green_spell (+ +frm_spells 2)
	+item_frm_white_spell (+ +frm_spells 3)
	+item_frm_red_spell (+ +frm_spells 4)
	+item_frm_blue_spell (+ +frm_spells 5))

;;;;;;;;;
; Colors
;;;;;;;;;

(defq
	+rgb_black 0x000000
	+rgb_white 0xffffff
	+rgb_red 0xff0000
	+rgb_green 0x00ff00
	+rgb_blue 0x0000ff
	+rgb_yellow 0xffff00
	+rgb_magenta 0xff00ff
	+rgb_cyan 0x00ffff
	+rgb_dwhite 0x7f7f7f
	+rgb_dred 0x7f0000
	+rgb_dgreen 0x007f00
	+rgb_dblue 0x00007f
	+rgb_dyellow 0x7f7f00
	+rgb_dmagenta 0x7f007f
	+rgb_dcyan 0x007f7f
	+rgb_pwhite 0xffffff
	+rgb_pred 0xff8080
	+rgb_pgreen 0x80ff80
	+rgb_pblue 0x8080ff
	+rgb_pyellow 0xffff80
	+rgb_pmagenta 0xff80ff
	+rgb_pcyan 0x80ffff
	+rgb_grey1 0x222222
	+rgb_grey2 0x444444
	+rgb_grey3 0x666666
	+rgb_grey4 0x888888
	+rgb_grey5 0xaaaaaa
	+rgb_grey6 0xcccccc
	+button_size 50
	+button_col 0xffffffff
	+button_col_down 0xff3f3fff)
