;;;;;;;;;;;;;;;;;;;;;;;
; Screen and dimensions
;;;;;;;;;;;;;;;;;;;;;;;

(defq
	+screen_width 320 +screen_height 240
	+map_width 128 +map_height 16
	+tile_width 16 +tile_height 16
	+panel_height 72 +panel_y (- +screen_height +panel_height)
	+window_width +screen_width +window_height (- +screen_height +panel_height)
	+max_strength 2048 +max_power (/ +max_strength 2))

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

(bits +fsp 0
	(bit collide action))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sprite types and bit masks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bits +ftp 0
	(bit horse spearman wizard footman knight balista cannon oil boarrider carpet
		tower monk beserk skeleton_monk skeleton_horse skeleton enemy_fire
		item man man_banner enemy_banner))

;;;;;;;;;;;;;;;;;;
; Map tile flags
;;;;;;;;;;;;;;;;;;

(bits +fmap 0
	(bit masked stand climb))

;;;;;;;;;;;;;;;;;;;;;
; Control key bits
;;;;;;;;;;;;;;;;;;;;;

(bits +fkey 0
	(bit up down left right keya keyb keyc keyd))

;;;;;;;;;;;;;
; Game flags
;;;;;;;;;;;;;

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
		skeleton_horse skeleton_horse1 skeleton_horse2 skeleton_horse3 skeleton_horse4 skeleton_horse5))

; frm_64x64 (tower)
(enums +frm64x64 0
	(enum tower))

; frm_64x32_l (boarrider, carpet)
(enums +frm64x32_l 0
	(enum boarrider boarrider1 boarrider2 boarrider3 boarrider4 boarrider5
		carpet carpet1 carpet2 carpet3 carpet4 carpet5))

; fanatic_l (player man 32x32 and skeletons)
(enums +frm_fanatic_l 0
	(enum walk walk1 walk2 walk3 walk4 walk5 walk6 walk7
		jump fall climb climb1 climb2 climb3
		masehit masehit1 masehit2
		handbow handbow1 handbow2
		crossbow crossbow1 crossbow2 crossbow3
		naptha naptha1 naptha2
		stance stance1 duck
		lmasehit lmasehit1 lmasehit2
		skeleton skeleton1 skeleton2 skeleton3 skeleton4 skeleton5 skeleton6 skeleton7
		skeleton8 skeleton9 skeleton10 skeleton11 skeleton12 skeleton13 skeleton14
		skeleton_monk))

; frm_32x32_l (spearman, wizard, footman, knight)
(enums +frm_32x32_l 0
	(enum spearman spearman1 spearman2 spearman3 spearman4 spearman5 spearman6 spearman7 spearman8 spearman9 spearman10 spearman11
		wizard wizard1 wizard2 wizard3 wizard4 wizard5 wizard6 wizard7 wizard8 wizard9 wizard10 wizard11
		footman footman1 footman2 footman3 footman4 footman5 footman6 footman7 footman8 footman9 footman10 footman11 footman12 footman13 footman14 footman15 footman16
		knight knight1 knight2 knight3 knight4 knight5 knight6 knight7 knight8 knight9 knight10 knight11 knight12 knight13 knight14 knight15 knight16))

; frm_32c32_l (balista, cannon, oil, beserk, monk)
(enums +frm_32c32_l 0
	(enum balista balista1 balista2 balista3
		cannon cannon1 cannon2 cannon3
		oil oil1 oil2 oil3
		beserk beserk1 beserk2 beserk3 beserk4 beserk5
		monk monk1 monk2 monk3 monk4 monk5))

; frm_32c32 (big explosion, elemental, faces, title letters, mr smith, flag, power, strength)
(enums +frm_32c32 0
	(enum bigexp bigexp1 bigexp2 bigexp3 bigexp4
		elemental elemental1 elemental2 elemental3
		face face1 face2 face3 face4 face5 face6 face7
		letter_o letter_n letter_s letter_l letter_a letter_u letter_g letter_h letter_t
		smith smith1 smith2
		flag power strength))

; frm_32x16_l (barrow, spear)
(enums +frm_32x16_l 0
	(enum barrow spear))

; frm_16x32 (oilshot)
(enums +frm_16x32 0
	(enum oilshot))

; frm_16x64 (sword)
(enums +frm_16x64 0
	(enum sword))

; frm_16x16_l (arrow, mase, lmase, hbow, cbow, napth, footadd, knightadd, bkaddon, flame, blood, skeladd)
(enums +frm_16x16_l 0
	(enum arrow
		mase mase1 mase2
		lmase lmase1 lmase2
		hbow hbow1 hbow2
		cbow cbow1 cbow2 cbow3
		napth napth1
		footadd footadd1 footadd2 footadd3 footadd4
		knightadd knightadd1 knightadd2 knightadd3 knightadd4 knightadd5
		bkaddon bkaddon1 bkaddon2 bkaddon3 bkaddon4 bkaddon5
		flame flame1
		blood blood1 blood2 blood3
		skeladd))

; frm_16x16 (pole, banners, cball, mine, wizshot, nbomb, demon, fires, sight, gold, shield/weapons, spells, diamond, segments, tends, manbits, blooddrip, fizz, manmind, pshot, mshot, bones)
(enums +frm_16x16 0
	(enum pole
		banners banners1 banners2 banners3 banners4 banners5 banners6 banners7 banners8 banners9 banners10 banners11 banners12 banners13 banners14 banners15
		cball
		mine mine1
		wizshot wizshot1 wizshot2 wizshot3
		nbomb nbomb1 nbomb2 nbomb3
		demon demon1 demon2 demon3
		fires fires1
		sight
		gold
		shield shield1 shield2 shield3 shield4 shield5 shield6
		spells spells1 spells2 spells3 spells4 spells5
		diamond diamond1 diamond2 diamond3 diamond4 diamond5 diamond6 diamond7 diamond8 diamond9
		segments segments1
		tends tends1 tends2 tends3
		manbits manbits1 manbits2 manbits3 manbits4
		blooddrip
		fizz fizz1 fizz2
		manmind manmind1 manmind2 manmind3
		pshot pshot1
		mshot mshot1
		bones))

; campain 8x8
(enums +frm_campain 0
	(enum oracle twater tswamp tforest tmountain water swamp forest mountain plague crusade rebellion enemy player))

;;;;;;;;;;;;;;;;;;;;;
; Item frame mapping
;;;;;;;;;;;;;;;;;;;;;

(defq +item_map (pmap
	+item_gold +frm_16x16_gold
	+item_mase +frm_16x16_shield
	+item_big_bow (+ +frm_16x16_shield 1)
	+item_small_bow (+ +frm_16x16_shield 2)
	+item_naptha (+ +frm_16x16_shield 3)
	+item_helper (+ +frm_16x16_shield 4)
	+item_super_mase (+ +frm_16x16_shield 5)
	+item_super_helper (+ +frm_16x16_shield 6)
	+item_yellow_spell +frm_16x16_spells
	+item_black_spell (+ +frm_16x16_spells 1)
	+item_green_spell (+ +frm_16x16_spells 2)
	+item_white_spell (+ +frm_16x16_spells 3)
	+item_red_spell (+ +frm_16x16_spells 4)
	+item_blue_spell (+ +frm_16x16_spells 5)))
