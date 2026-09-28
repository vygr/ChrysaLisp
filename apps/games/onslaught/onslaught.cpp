/*
 *  Onslaught
 *
 *  Created by Christopher Hinsley on 19/08/2008.
 *  Copyright 1989-2008 Christopher Hinsley. All rights reserved.
 *
 */

// #define WANT_SAVEDEMO

#include "onslaught.h"
#include "engine.h"
#include <stdio.h>
#include <string.h>

//////////////////////////////////////////////////////////////////////////////
//
// Onslaught
//
//////////////////////////////////////////////////////////////////////////////

// engine external globals and functions

extern FILE *openbundlefile(const char *, const char *);
extern FILE *openappdatafile(const char *, const char *, const char *);
extern void loadsound(int num, const char *);
Pixmap *loadimage(const char *, int sw, int sh);

extern int GLOBALS_SCROLL_X;
extern int GLOBALS_SCROLL_Y;

extern void sprite_draw_list(Listhead *);
extern void sprite_kill_list(Listhead *);
extern void sprite_kill_list_id(Listhead *, int);
extern Sprite *sprite_find_list_types(Listhead *, int);
extern void sprite_free_list(Listhead *);
extern void sprite_proc_list(Listhead *);
extern Sprite *sprite_collide(Listhead *, int, int, int, int, int);
extern void sprite_draw(Sprite *);
extern void sprite_draw_noflip(Sprite *);
extern void text_draw(Sprite *);

// pixmaps

Pixmap *GLOBALS_PANEL;
Pixmap *GLOBALS_BLOCKS1;
Pixmap *GLOBALS_BLOCKS2;
Pixmap *GLOBALS_BLOCKS3;
Pixmap *GLOBALS_CAMPAIN;
Pixmap *GLOBALS_ASCII;
Pixmap *GLOBALS_FANATIC_L;
Pixmap *GLOBALS_FRM_16X16;
Pixmap *GLOBALS_FRM_16X16_L;
Pixmap *GLOBALS_FRM_16X32;
Pixmap *GLOBALS_FRM_16X64;
Pixmap *GLOBALS_FRM_32C32;
Pixmap *GLOBALS_FRM_32C32_L;
Pixmap *GLOBALS_FRM_32X16_L;
Pixmap *GLOBALS_FRM_32X32_L;
Pixmap *GLOBALS_FRM_64X32_L;
Pixmap *GLOBALS_FRM_64X64;
Pixmap *GLOBALS_FRM_64X64_L;

// prototypes

void game_free_dlists();
void game_proc_dlists();
void init_mine();
void init_enemy();
void blast_area(int, int, int, int);
Sprite *create_enemy_type(int);
Sprite *create_item_type(int);
void drop_item(Sprite *);
int item_carry(int);
void item_collect(Sprite *);
void item_auto_select();
void item_select();
void game_set_map(int);
void getxy(int, int, int, int &, int &);
void getxy_outer(int, int, int &, int &);
void getxy_inner(int, int, int &, int &);
int random(int);
void game_set_level(int);
void game_swap_demo();
void game_mr_smiths();
void game_generate_player_info();
void game_generate_enemy_info();
void game_enemy_map();
void update_panel();
void game_setstate_battle_common();

typedef void (*frame_func)();
void game_setstate_title();
void game_setstate_menu();
void game_setstate_map();
void game_setstate_scores();
void game_setstate_hiscore();
void game_setstate_battle();
void game_setstate_battle_won();
void game_setstate_mind();
void game_setstate_mind_won();
void game_setstate_credits();
void game_setstate_oracle();
void game_setstate_demo();
void game_frame_title();
void game_frame_menu();
void game_frame_map();
void game_frame_scores();
void game_frame_hiscore();
void game_frame_battle();
void game_frame_battle_won();
void game_frame_battle_lost();
void game_frame_mind();
void game_frame_mind_won();
void game_frame_mind_lost();
void game_frame_credits();
void game_frame_oracle();
void game_frame_demo();

typedef void (*cl_func)(Sprite *, Sprite *);
void cl_man_hits_dragging_enemy(Sprite *, Sprite *);
void cl_man_hits_enemy(Sprite *, Sprite *);
void cl_man_hits_monk(Sprite *, Sprite *);
void cl_man_hits(Sprite *, Sprite *);
void cl_enemy_addon_hits_man(Sprite *, Sprite *);
void cl_man_hits_banner(Sprite *, Sprite *);
void cl_man_hits_enemy_banner(Sprite *, Sprite *);
void cl_man_addon_hits(Sprite *, Sprite *);
void cl_man_missile_hits(Sprite *, Sprite *);
void cl_hand_hits_fire(Sprite *, Sprite *);
void cl_hand_hits_item(Sprite *, Sprite *);
void cl_mind_hits_fire(Sprite *, Sprite *);

typedef void (*draw_func)(Sprite *);
void sprite_draw_mind(Sprite *);
void sprite_draw_banner(Sprite *);
void sprite_draw_enter(Sprite *);
void sprite_draw_map(Sprite *);

typedef void (*cp_func)(Sprite *, CP *);
void cp_man(Sprite *, CP *);
void cp_manduck(Sprite *, CP *);
void cp_manitem(Sprite *, CP *);
void cp_manstance(Sprite *, CP *);
void cp_mantrack(Sprite *, CP *);
void cp_mind(Sprite *, CP *);
void cp_hand_fire(Sprite *, CP *);
void cp_hand(Sprite *, CP *);
void cp_bounce(Sprite *, CP *);
void cp_offscreen_no_death(Sprite *, CP *);
void cp_offscreen(Sprite *, CP *);
void cp_banner(Sprite *, CP *);
void cp_big_crossbow(Sprite *, CP *);
void cp_small_crossbow(Sprite *, CP *);
void cp_naptha(Sprite *, CP *);
void cp_nbomb(Sprite *, CP *);
void cp_helper(Sprite *, CP *);
void cp_mine(Sprite *, CP *);
void cp_enemy_fall(Sprite *, CP *);
void cp_enemy_offscreen(Sprite *, CP *);
void cp_enemy_fall_duck(Sprite *, CP *);
void cp_spearman(Sprite *, CP *);
void cp_enemy_duck(Sprite *, CP *);
void cp_wizard(Sprite *, CP *);
void cp_footman(Sprite *, CP *);
void cp_knight(Sprite *, CP *);
void cp_skeleton_monk(Sprite *, CP *);
void cp_skeleton(Sprite *, CP *);
void cp_balista(Sprite *, CP *);
void cp_oil(Sprite *, CP *);
void cp_cannon(Sprite *, CP *);
void cp_beserk(Sprite *, CP *);
void cp_tower(Sprite *, CP *);
void cp_bone(Sprite *, CP *);
void cp_item(Sprite *, CP *);
void cp_letter(Sprite *, CP *);
void cp_sword(Sprite *, CP *);

typedef void (*dt_func)(Sprite *);
void dt_man(Sprite *);
void dt_blat_fx(Sprite *);
void dt_fizz_fx(Sprite *);
void dt_exp_fx(Sprite *);
void dt_hand_fire(Sprite *);
void dt_hand_fire1(Sprite *);
void dt_bomb_fx(Sprite *);
void dt_elemental_explode(Sprite *);
void dt_super_helper_transform(Sprite *);
void dt_mine_explode(Sprite *);
void dt_kill_horse(Sprite *);
void dt_kill_skeleton_horse(Sprite *);
void dt_kill_enemy(Sprite *);
void dt_kill_monk(Sprite *);
void dt_kill_zap(Sprite *);
void dt_kill_skeleton(Sprite *);
void dt_kill_balista(Sprite *);
void dt_kill_tower(Sprite *);
void dt_kill_carpet(Sprite *);
void dt_kill_boarrider(Sprite *);
void dt_skeleton_item(Sprite *);
void dt_drip(Sprite *);
void dt_kill_enemy_noitem(Sprite *);
void dt_kill_zap_noitem(Sprite *);
void dt_kill_bones_noitem(Sprite *);

typedef void (*use_func)(Sprite *);
void use_mase(Sprite *);
void use_big_crossbow(Sprite *);
void use_small_crossbow(Sprite *);
void use_naptha(Sprite *);
void use_helper(Sprite *);
void use_super_mase(Sprite *);
void use_super_helper(Sprite *);
void use_yellow_spell(Sprite *);
void use_black_spell(Sprite *);
void use_green_spell(Sprite *);
void use_white_spell(Sprite *);
void use_red_spell(Sprite *);
void use_blue_spell(Sprite *);
void use_gold(Sprite *);
void use_talisman(Sprite *);

typedef void (*init_func)(int);
void init_enemy_left(int);
void init_enemy_right(int);
void init_balista_left(int);
void init_balista_right(int);

typedef void (*play_func)(Sprite *);
void play_death_score(Sprite *);
void play_horse(Sprite *);
void play_death(Sprite *);
void play_clash1(Sprite *);
void play_clash2(Sprite *);
void play_boar(Sprite *);
void play_roar(Sprite *);
void play_spell(Sprite *);
void play_explode(Sprite *);
void play_out(Sprite *);
void play_twang1(Sprite *);
void play_twang2(Sprite *);
void play_drip(Sprite *);
void play_hooves(Sprite *);
void play_throw(Sprite *);

class vgrad;
class map;
class mindmap;
class autoplay;

// globals

Listhead GLOBALS_MAN_DLIST;
Listhead GLOBALS_BANS_DLIST;
Listhead GLOBALS_ITEM_DLIST;
Listhead GLOBALS_ENEMY_DLIST;
Listhead GLOBALS_MISSILE_DLIST;
Listhead GLOBALS_BODY_DLIST;
Listhead GLOBALS_FX_DLIST;

int GLOBALS_GAME_KEYBOARD = 0;
int GLOBALS_GAME_CONTROLS = 0;
int GLOBALS_GAME_CONTROLS1 = 0;
int GLOBALS_GAME_FLAGS = 0;
int GLOBALS_GAME_DRIP_Y = 0;
int GLOBALS_GAME_DRIP_YVEL = 0;
int GLOBALS_GAME_LEVEL = 0;
unsigned int GLOBALS_GAME_SEED = 0;
unsigned char GLOBALS_GAME_CAMPAINMAP[256];
int GLOBALS_GAME_LOCATION_X = 0;
int GLOBALS_GAME_LOCATION_Y = 0;
int GLOBALS_GAME_LOCATION_LASTX = 0;
int GLOBALS_GAME_LOCATION_LASTY = 0;
int GLOBALS_GAME_LOCATION_OX = 0;
int GLOBALS_GAME_LOCATION_OY = 0;
int GLOBALS_GAME_FRAME_COUNT = 0;
int GLOBALS_GAME_COUNT = 0;
int GLOBALS_GAME_STATE = 0;
int GLOBALS_GAME_STATE_LAST = 0;
int GLOBALS_GAME_STATE_NEXT = 0;
int GLOBALS_GAME_TITLE = 0;
int GLOBALS_GAME_TIMEZONE = 0;
int GLOBALS_GAME_EVENT_COUNT = 0;
int GLOBALS_GAME_PLAGUE_CREATE = 0;
int GLOBALS_GAME_CRUSADE_CREATE = 0;
int GLOBALS_GAME_REBELLION_CREATE = 0;
int GLOBALS_GAME_PLAGUE_DESTROY = 0;
int GLOBALS_GAME_CRUSADE_DESTROY = 0;
int GLOBALS_GAME_REBELLION_DESTROY = 0;
unsigned long GLOBALS_GAME_LASTTIME = 0;
unsigned long GLOBALS_GAME_DOWNTIME = 0;

enum {
  GLOBALS_GAME_MENU_START_COL,
  GLOBALS_GAME_MENU_DEFINE_COL,
  GLOBALS_GAME_MENU_DIFFICULTY_COL,
  GLOBALS_GAME_MENU_MODE_COL,
  GLOBALS_GAME_MENU_SOUND_COL,
  GLOBALS_GAME_MENU_CREDITS_COL
};

int GLOBALS_GAME_MENU[6];
int GLOBALS_GAME_MENU_SELECTED = GLOBALS_GAME_MENU_START_COL;
int GLOBALS_GAME_MENU_MODE_INDEX = 0;
int GLOBALS_GAME_MENU_DIFFICULTY_INDEX = 0;
int GLOBALS_GAME_MENU_SOUND_INDEX = 0;
char *GLOBALS_GAME_MENU_MODE = 0;
char *GLOBALS_GAME_MENU_DIFFICULTY = 0;
char *GLOBALS_GAME_MENU_SOUND = 0;

int GLOBALS_MAN_STRENGTH = MAXSTRENGTH;
int GLOBALS_MAN_POWER = MAXPOWER;
int GLOBALS_MAN_SCORE = 0;
int GLOBALS_MAN_START_SCORE = 0;
int GLOBALS_MAN_BANNER = 0;
int GLOBALS_MAN_EXPERIENCE = 0;
int GLOBALS_MAN_TERRITORY = 0;
char GLOBALS_MAN_INITIALS[4];
char GLOBALS_MAN_NAME[8];
char GLOBALS_MAN_HOMELAND[12];
const char *GLOBALS_MAN_STATUS = 0;
int GLOBALS_MAN_CULT_INDEX = 0;
const char *GLOBALS_MAN_CULT = 0;

int GLOBALS_ITEM_SELECTED = 0;
int GLOBALS_ITEM_USEAGE[8];
int GLOBALS_ITEM_INVENTORY[8];
int GLOBALS_ITEM_DROPCNT = 0;

int GLOBALS_MINE_COUNT = 0;
int GLOBALS_MINE_MAX = 50;

int GLOBALS_ENEMY_STACKINDEX = 0;
int GLOBALS_ENEMY_COUNT = 0;
int GLOBALS_ENEMY_STACK[16];
int GLOBALS_ENEMY_MAX = 50;
int GLOBALS_ENEMY_DELAY = 1;
int GLOBALS_ENEMY_ARMY = 0;
int GLOBALS_ENEMY_WIZSHOT = 0;
int GLOBALS_ENEMY_BANNER = 5;
int GLOBALS_ENEMY_POPULATION = 0;
int GLOBALS_ENEMY_POPULARITY = 0;
int GLOBALS_ENEMY_INFOCOL = RGBWHITE;
int GLOBALS_ENEMY_INFODCOL = RGBWHITE;
int GLOBALS_ENEMY_LORD = 0;
char GLOBALS_ENEMY_KINGDOM[16];
const char *GLOBALS_ENEMY_WIZARDLORD = 0;
const char *GLOBALS_ENEMY_WARBAND = 0;
const char *GLOBALS_ENEMY_CULT = 0;

int GLOBALS_MIND_FIRECNT = 0;
int GLOBALS_MIND_ITEMINDEX = 0;
int GLOBALS_MIND_POWER = 0;

int GLOBALS_SCORE[10] = {10000, 9000, 8000, 7000, 6000,
                         5000,  4000, 3000, 2000, 1000};

char GLOBALS_SCORE_TXT[10][16] = {
    {"NEGROG (CAH)"}, {"RUDMAN (CAH)"}, {"GORVAR (CAH)"}, {"TREGAR (CAH)"},
    {"TATGO  (CAH)"}, {"GONIG  (CAH)"}, {"INDIGO (CAH)"}, {"MOLAT  (CAH)"},
    {"DENGAR (CAH)"}, {"LOGRAA (CAH)"},
};

vgrad *GLOBALS_SKY;
map *GLOBALS_LAND;
mindmap *GLOBALS_MIND;
autoplay *GLOBALS_RECORD;
autoplay *GLOBALS_PLAY;

// equates for sprite types

enum {
  BFTP_HORSE,
  BFTP_SPEARMAN,
  BFTP_WIZARD,
  BFTP_FOOTMAN,
  BFTP_KNIGHT,
  BFTP_BALISTA,
  BFTP_CANNON,
  BFTP_OIL,
  BFTP_BOARRIDER,
  BFTP_CARPET,
  BFTP_TOWER,
  BFTP_MONK,
  BFTP_BESERK,
  BFTP_SKELETON_MONK,
  BFTP_SKELETON_HORSE,
  BFTP_SKELETON,
  BFTP_ENEMY_FIRE,
  BFTP_ITEM,
  BFTP_MAN,
  BFTP_MAN_BANNER,
  BFTP_EMEMY_BANNER
};
#define FTP_HORSE (1 << BFTP_HORSE)
#define FTP_SPEARMAN (1 << BFTP_SPEARMAN)
#define FTP_WIZARD (1 << BFTP_WIZARD)
#define FTP_FOOTMAN (1 << BFTP_FOOTMAN)
#define FTP_KNIGHT (1 << BFTP_KNIGHT)
#define FTP_BALISTA (1 << BFTP_BALISTA)
#define FTP_CANNON (1 << BFTP_CANNON)
#define FTP_OIL (1 << BFTP_OIL)
#define FTP_BOARRIDER (1 << BFTP_BOARRIDER)
#define FTP_CARPET (1 << BFTP_CARPET)
#define FTP_TOWER (1 << BFTP_TOWER)
#define FTP_MONK (1 << BFTP_MONK)
#define FTP_BESERK (1 << BFTP_BESERK)
#define FTP_SKELETON_MONK (1 << BFTP_SKELETON_MONK)
#define FTP_SKELETON_HORSE (1 << BFTP_SKELETON_HORSE)
#define FTP_SKELETON (1 << BFTP_SKELETON)
#define FTP_ENEMY_FIRE (1 << BFTP_ENEMY_FIRE)
#define FTP_ITEM (1 << BFTP_ITEM)
#define FTP_MAN (1 << BFTP_MAN)
#define FTP_MAN_BANNER (1 << BFTP_MAN_BANNER)
#define FTP_EMEMY_BANNER (1 << BFTP_EMEMY_BANNER)

// map tile flags

enum { BFMAP_MASKED, BFMAP_STAND, BFMAP_CLIMB };
#define FMAP_MASKED (1 << BFMAP_MASKED)
#define FMAP_STAND (1 << BFMAP_STAND)
#define FMAP_CLIMB (1 << BFMAP_CLIMB)

// 64x64

enum { FRM_HORSE, FRM_SKELETON_HORSE = FRM_HORSE + 6 };

// 64x64

enum { FRM_TOWER };

// 64x32

enum { FRM_BOARRIDER, FRM_CARPET = FRM_BOARRIDER + 6 };

// man32x32 and skeletons

enum {
  FRM_WALK,
  FRM_JUMP = FRM_WALK + 8,
  FRM_FALL = FRM_JUMP + 1,
  FRM_CLIMB = FRM_FALL + 1,
  FRM_MASEHIT = FRM_CLIMB + 4,
  FRM_HANDBOW = FRM_MASEHIT + 3,
  FRM_CROSSBOW = FRM_HANDBOW + 3,
  FRM_NAPTHA = FRM_CROSSBOW + 4,
  FRM_STANCE = FRM_NAPTHA + 3,
  FRM_DUCK = FRM_STANCE + 2,
  FRM_LMASEHIT = FRM_DUCK + 1,
  FRM_SKELETON = FRM_LMASEHIT + 3,
  FRM_SKELETON_MONK = FRM_SKELETON + 15
};

// 32x32

enum {
  FRM_SPEARMAN,
  FRM_WIZARD = FRM_SPEARMAN + 12,
  FRM_FOOTMAN = FRM_WIZARD + 12,
  FRM_KNIGHT = FRM_FOOTMAN + 17
};

// 32c32_l

enum {
  FRM_BALISTA,
  FRM_CANNON = FRM_BALISTA + 4,
  FRM_OIL = FRM_CANNON + 4,
  FRM_BESERK = FRM_OIL + 4,
  FRM_MONK = FRM_BESERK + 6
};

// 32c32

enum {
  FRM_BIGEXP,
  FRM_ELEMENTAL = FRM_BIGEXP + 5,
  FRM_FACES = FRM_ELEMENTAL + 4,
  FRM_LETTERS = FRM_FACES + 8,
  FRM_SMITH = FRM_LETTERS + 9,
  FRM_FLAG = FRM_SMITH + 3,
  FRM_POWER = FRM_FLAG + 1,
  FRM_STRENGTH = FRM_POWER + 1
};

// 32x16

enum { FRM_BARROW, FRM_SPEAR = FRM_BARROW + 1 };

// 16x32

enum { FRM_OILSHOT };

// 16x64

enum { FRM_SWORD };

// 16x16

enum {
  FRM_ARROW,
  FRM_MASE = FRM_ARROW + 1,
  FRM_LMASE = FRM_MASE + 3,
  FRM_HBOW = FRM_LMASE + 3,
  FRM_CBOW = FRM_HBOW + 3,
  FRM_NAPTH = FRM_CBOW + 4,
  FRM_FOOTADD = FRM_NAPTH + 2,
  FRM_KNIGHTADD = FRM_FOOTADD + 5,
  FRM_BKADDON = FRM_KNIGHTADD + 6,
  FRM_FLAME = FRM_BKADDON + 6,
  FRM_BLOOD = FRM_FLAME + 2,
  FRM_SKELADD = FRM_BLOOD + 4
};

// 16x16

enum {
  FRM_POLE,
  FRM_BANNERS = FRM_POLE + 1,
  FRM_CBALL = FRM_BANNERS + 16,
  FRM_MINE = FRM_CBALL + 1,
  FRM_WIZSHOT = FRM_MINE + 2,
  FRM_NBOMB = FRM_WIZSHOT + 4,
  FRM_DEMON = FRM_NBOMB + 4,
  FRM_FIRES = FRM_DEMON + 4,
  FRM_SIGHT = FRM_FIRES + 2,
  FRM_GOLD = FRM_SIGHT + 1,
  FRM_SHIELD = FRM_GOLD + 1,
  FRM_SPELLS = FRM_SHIELD + 7,
  FRM_DIAMOND = FRM_SPELLS + 6,
  FRM_SEGMENTS = FRM_DIAMOND + 10,
  FRM_TENDS = FRM_SEGMENTS + 2,
  FRM_MANBITS = FRM_TENDS + 4,
  FRM_BLOODDRIP = FRM_MANBITS + 5,
  FRM_FIZZ = FRM_BLOODDRIP + 1,
  FRM_MANMIND = FRM_FIZZ + 3,
  FRM_PSHOT = FRM_MANMIND + 4,
  FRM_MSHOT = FRM_PSHOT + 2,
  FRM_BONES = FRM_MSHOT + 2
};

// campain 8x8

enum {
  FRM_ORACLE,
  FRM_TWATER = FRM_ORACLE + 1,
  FRM_TSWAMP = FRM_TWATER + 1,
  FRM_TFOREST = FRM_TSWAMP + 1,
  FRM_TMOUNTAIN = FRM_TFOREST + 1,
  FRM_WATER = FRM_TMOUNTAIN + 1,
  FRM_SWAMP = FRM_WATER + 1,
  FRM_FOREST = FRM_SWAMP + 1,
  FRM_MOUNTAIN = FRM_FOREST + 1,
  FRM_PLAGUE = FRM_MOUNTAIN + 1,
  FRM_CRUSADE = FRM_PLAGUE + 1,
  FRM_REBELLION = FRM_CRUSADE + 1,
  FRM_ENEMY = FRM_REBELLION + 1,
  FRM_PLAYER = FRM_ENEMY + 1
};

// controls bits

enum {
  BFKEY_UP,
  BFKEY_DOWN,
  BFKEY_LEFT,
  BFKEY_RIGHT,
  BFKEY_KEYB,
  BFKEY_KEYA,
  BFKEY_KEYC,
  BFKEY_KEYD
};
#define FKEY_UP (1 << BFKEY_UP)
#define FKEY_DOWN (1 << BFKEY_DOWN)
#define FKEY_LEFT (1 << BFKEY_LEFT)
#define FKEY_RIGHT (1 << BFKEY_RIGHT)
#define FKEY_KEYA (1 << BFKEY_KEYA)
#define FKEY_KEYB (1 << BFKEY_KEYB)
#define FKEY_KEYC (1 << BFKEY_KEYC)
#define FKEY_KEYD (1 << BFKEY_KEYD)

// flag bits

enum {
  BFFLAG_USE,
  BFFLAG_SELECT,
  BFFLAG_ENEMY,
  BFFLAG_MINE,
  BFFLAG_TITLE,
  BFFLAG_ISELECT
};
#define FFLAG_USE (1 << BFFLAG_USE)
#define FFLAG_SELECT (1 << BFFLAG_SELECT)
#define FFLAG_ENEMY (1 << BFFLAG_ENEMY)
#define FFLAG_MINE (1 << BFFLAG_MINE)
#define FFLAG_TITLE (1 << BFFLAG_TITLE)
#define FFLAG_ISELECT (1 << BFFLAG_ISELECT)

// game states

enum {
  GAME_STATE_TITLE,
  GAME_STATE_MENU,
  GAME_STATE_MAP,
  GAME_STATE_SCORES,
  GAME_STATE_HISCORE,
  GAME_STATE_BATTLE,
  GAME_STATE_BATTLE_WON,
  GAME_STATE_BATTLE_LOST,
  GAME_STATE_MIND,
  GAME_STATE_MIND_WON,
  GAME_STATE_MIND_LOST,
  GAME_STATE_CREDITS,
  GAME_STATE_ORACLE,
  GAME_STATE_DEMO
};

// game battle levels

enum { GAME_LEVEL_FIELD, GAME_LEVEL_SEIGE, GAME_LEVEL_DEFEND, GAME_LEVEL_MIND };

// map flags

unsigned char mapflags1[] = {FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             FMAP_MASKED,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             0,
                             0,
                             0,
                             0,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             0,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             0,
                             0,
                             0,
                             0,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             0,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             FMAP_STAND,
                             FMAP_STAND,
                             FMAP_STAND,
                             FMAP_STAND,
                             FMAP_STAND,
                             FMAP_STAND,
                             FMAP_STAND,
                             FMAP_STAND,
                             FMAP_STAND,
                             FMAP_STAND,
                             (FMAP_CLIMB | FMAP_STAND),
                             (FMAP_CLIMB | FMAP_STAND),
                             (FMAP_CLIMB | FMAP_STAND),
                             (FMAP_CLIMB | FMAP_STAND),
                             (FMAP_CLIMB | FMAP_STAND),
                             (FMAP_CLIMB | FMAP_STAND)};

unsigned char mapflags2[] = {FMAP_MASKED,
                             FMAP_MASKED,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             0,
                             0,
                             0,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             0,
                             FMAP_MASKED,
                             0,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             0,
                             FMAP_MASKED,
                             0,
                             0,
                             0,
                             0,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             FMAP_MASKED,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             FMAP_MASKED,
                             0,
                             0,
                             0,
                             0,
                             FMAP_MASKED,
                             0,
                             0,
                             0,
                             0,
                             FMAP_MASKED,
                             (FMAP_MASKED | FMAP_STAND),
                             FMAP_STAND,
                             FMAP_STAND,
                             FMAP_STAND,
                             (FMAP_MASKED | FMAP_STAND),
                             (FMAP_MASKED | FMAP_STAND),
                             (FMAP_MASKED | FMAP_STAND),
                             FMAP_STAND,
                             FMAP_STAND,
                             FMAP_STAND,
                             (FMAP_MASKED | FMAP_STAND),
                             (FMAP_MASKED | FMAP_STAND),
                             (FMAP_MASKED | FMAP_STAND),
                             (FMAP_MASKED | FMAP_STAND),
                             (FMAP_CLIMB | FMAP_STAND),
                             (FMAP_CLIMB | FMAP_STAND)};

// campain map start

unsigned char campainmap[] = {
    FRM_WATER,     FRM_WATER,    FRM_WATER,    FRM_ENEMY,    FRM_ENEMY,
    FRM_ENEMY,     FRM_ENEMY,    FRM_ENEMY,    FRM_WATER,    FRM_WATER,
    FRM_WATER,     FRM_WATER,    FRM_ENEMY,    FRM_WATER,    FRM_WATER,
    FRM_WATER,     FRM_WATER,    FRM_ENEMY,    FRM_ENEMY,    FRM_ENEMY,
    FRM_ENEMY,     FRM_FOREST,   FRM_FOREST,   FRM_ENEMY,    FRM_ENEMY,
    FRM_WATER,     FRM_WATER,    FRM_ENEMY,    FRM_ENEMY,    FRM_ENEMY,
    FRM_MOUNTAIN,  FRM_WATER,    FRM_ENEMY,    FRM_ENEMY,    FRM_MOUNTAIN,
    FRM_ENEMY,     FRM_FOREST,   FRM_FOREST,   FRM_SWAMP,    FRM_FOREST,
    FRM_ENEMY,     FRM_MOUNTAIN, FRM_MOUNTAIN, FRM_ENEMY,    FRM_FOREST,
    FRM_WATER,     FRM_ENEMY,    FRM_TWATER,   FRM_ENEMY,    FRM_ENEMY,
    FRM_ENEMY,     FRM_TSWAMP,   FRM_SWAMP,    FRM_SWAMP,    FRM_SWAMP,
    FRM_SWAMP,     FRM_SWAMP,    FRM_WATER,    FRM_WATER,    FRM_TMOUNTAIN,
    FRM_ENEMY,     FRM_ENEMY,    FRM_ENEMY,    FRM_ENEMY,    FRM_WATER,
    FRM_WATER,     FRM_ENEMY,    FRM_SWAMP,    FRM_SWAMP,    FRM_SWAMP,
    FRM_SWAMP,     FRM_ENEMY,    FRM_ENEMY,    FRM_WATER,    FRM_WATER,
    FRM_FOREST,    FRM_MOUNTAIN, FRM_ENEMY,    FRM_WATER,    FRM_WATER,
    FRM_ENEMY,     FRM_ENEMY,    FRM_ENEMY,    FRM_SWAMP,    FRM_SWAMP,
    FRM_TFOREST,   FRM_ENEMY,    FRM_FOREST,   FRM_TWATER,   FRM_WATER,
    FRM_FOREST,    FRM_FOREST,   FRM_MOUNTAIN, FRM_MOUNTAIN, FRM_ENEMY,
    FRM_WATER,     FRM_WATER,    FRM_ENEMY,    FRM_ENEMY,    FRM_ENEMY,
    FRM_SWAMP,     FRM_SWAMP,    FRM_ENEMY,    FRM_ENEMY,    FRM_WATER,
    FRM_WATER,     FRM_WATER,    FRM_FOREST,   FRM_MOUNTAIN, FRM_ENEMY,
    FRM_TMOUNTAIN, FRM_ENEMY,    FRM_WATER,    FRM_WATER,    FRM_WATER,
    FRM_ENEMY,     FRM_ENEMY,    FRM_WATER,    FRM_WATER,    FRM_WATER,
    FRM_WATER,     FRM_ENEMY,    FRM_FOREST,   FRM_MOUNTAIN, FRM_MOUNTAIN,
    FRM_MOUNTAIN,  FRM_ENEMY,    FRM_WATER,    FRM_TFOREST,  FRM_FOREST,
    FRM_WATER,     FRM_WATER,    FRM_WATER,    FRM_WATER,    FRM_FOREST,
    FRM_TMOUNTAIN, FRM_ENEMY,    FRM_ENEMY,    FRM_MOUNTAIN, FRM_MOUNTAIN,
    FRM_MOUNTAIN,  FRM_WATER,    FRM_WATER,    FRM_WATER,    FRM_ENEMY,
    FRM_FOREST,    FRM_FOREST,   FRM_ENEMY,    FRM_FOREST,   FRM_FOREST,
    FRM_ENEMY,     FRM_ENEMY,    FRM_FOREST,   FRM_FOREST,   FRM_MOUNTAIN,
    FRM_MOUNTAIN,  FRM_WATER,    FRM_WATER,    FRM_ENEMY,    FRM_ENEMY,
    FRM_ENEMY,     FRM_WATER,    FRM_FOREST,   FRM_ENEMY,    FRM_ENEMY,
    FRM_FOREST,    FRM_ENEMY,    FRM_MOUNTAIN, FRM_ENEMY,    FRM_FOREST,
    FRM_FOREST,    FRM_MOUNTAIN, FRM_MOUNTAIN, FRM_ENEMY,    FRM_ENEMY,
    FRM_ENEMY,     FRM_ENEMY,    FRM_ENEMY,    FRM_ENEMY,    FRM_FOREST,
    FRM_ENEMY,     FRM_ENEMY,    FRM_ENEMY,    FRM_ENEMY,    FRM_TSWAMP,
    FRM_MOUNTAIN,  FRM_MOUNTAIN, FRM_MOUNTAIN, FRM_ENEMY,    FRM_ENEMY,
    FRM_ORACLE,    FRM_TWATER,   FRM_WATER,    FRM_ENEMY,    FRM_ENEMY,
    FRM_ENEMY,     FRM_SWAMP,    FRM_ENEMY,    FRM_ENEMY,    FRM_SWAMP,
    FRM_MOUNTAIN,  FRM_MOUNTAIN, FRM_FOREST,   FRM_MOUNTAIN, FRM_MOUNTAIN,
    FRM_ENEMY,     FRM_ENEMY,    FRM_ENEMY,    FRM_WATER,    FRM_WATER,
    FRM_SWAMP,     FRM_ENEMY,    FRM_ENEMY,    FRM_SWAMP,    FRM_SWAMP,
    FRM_SWAMP,     FRM_SWAMP,    FRM_ENEMY,    FRM_FOREST,   FRM_MOUNTAIN,
    FRM_ENEMY,     FRM_ENEMY,    FRM_WATER,    FRM_ENEMY,    FRM_ENEMY,
    FRM_ENEMY,     FRM_SWAMP,    FRM_WATER,    FRM_ENEMY,    FRM_ENEMY,
    FRM_SWAMP,     FRM_ENEMY,    FRM_ENEMY,    FRM_ENEMY,    FRM_WATER,
    FRM_ENEMY,     FRM_ENEMY,    FRM_MOUNTAIN, FRM_ENEMY,    FRM_ENEMY,
    FRM_TSWAMP,    FRM_ENEMY,    FRM_WATER,    FRM_WATER,    FRM_WATER,
    FRM_ENEMY,     FRM_WATER,    FRM_WATER,    FRM_ENEMY,    FRM_TSWAMP,
    FRM_WATER,     FRM_WATER,    FRM_ENEMY,    FRM_ENEMY,    FRM_ENEMY,
    FRM_ENEMY};

// animation tables

int at_mind_arm[] = {FRM_SEGMENTS, FRM_SEGMENTS, FRM_SEGMENTS,    FRM_SEGMENTS,
                     FRM_SEGMENTS, FRM_SEGMENTS, FRM_SEGMENTS,    FRM_SEGMENTS,
                     FRM_SEGMENTS, FRM_SEGMENTS, FRM_SEGMENTS + 1};

int at_hand_fire[] = {FRM_PSHOT, FRM_PSHOT + 1};

int at_mind_fire[] = {FRM_MSHOT, FRM_MSHOT + 1};

int at_man_stance[] = {FRM_STANCE, FRM_STANCE + 1};

int at_man_upper_mase[] = {FRM_MASEHIT, FRM_MASEHIT + 1, FRM_MASEHIT + 2,
                           FRM_WALK + 4};

int at_man_lower_mase[] = {FRM_LMASEHIT, FRM_LMASEHIT + 1, FRM_LMASEHIT + 2,
                           FRM_DUCK};

int at_upper_mase[] = {FRM_MASE, FRM_MASE + 1, FRM_MASE + 2, -1};

int at_lower_mase[] = {FRM_LMASE, FRM_LMASE + 1, FRM_LMASE + 2, -1};

int at_man_big_crossbow[] = {FRM_CROSSBOW, FRM_CROSSBOW + 1, FRM_CROSSBOW + 2,
                             FRM_CROSSBOW + 3, FRM_WALK};

int at_big_crossbow[] = {FRM_CBOW, FRM_CBOW + 1, FRM_CBOW + 2, FRM_CBOW + 3,
                         -1};

int at_man_small_crossbow[] = {FRM_HANDBOW, FRM_HANDBOW + 1, FRM_HANDBOW + 2,
                               FRM_WALK + 4};

int at_small_crossbow[] = {FRM_HBOW, FRM_HBOW + 1, FRM_HBOW + 2, -1};

int at_man_naptha[] = {FRM_NAPTHA, FRM_NAPTHA + 1, FRM_NAPTHA + 2,
                       FRM_WALK + 4};

int at_naptha[] = {FRM_NAPTH, -2, FRM_NAPTH + 1, -1};

int at_nbomb[] = {FRM_NBOMB, FRM_NBOMB + 1, FRM_NBOMB + 2, FRM_NBOMB + 3};

int at_elemental[] = {FRM_ELEMENTAL,     FRM_ELEMENTAL + 1, FRM_ELEMENTAL + 2,
                      FRM_ELEMENTAL + 3, FRM_ELEMENTAL + 3, -1};

int at_frag[] = {FRM_FIRES, FRM_FIRES + 1};

int at_helper[] = {FRM_DEMON, FRM_DEMON + 1};

int at_super_helper[] = {FRM_DEMON + 2, FRM_DEMON + 3};

int at_fizz[] = {FRM_FIZZ,     FRM_FIZZ + 1, FRM_FIZZ + 2,
                 FRM_FIZZ + 1, FRM_FIZZ,     -1};

int at_blood[] = {FRM_BLOOD, FRM_BLOOD + 1, FRM_BLOOD + 2, FRM_BLOOD + 3, -1};

int at_small_explosion[] = {FRM_BIGEXP,
                            FRM_BIGEXP + 1,
                            FRM_BIGEXP + 2,
                            FRM_BIGEXP + 3,
                            FRM_BIGEXP + 4,
                            FRM_BIGEXP,
                            -1};

int at_mine[] = {FRM_MINE, FRM_MINE + 1};

int at_boarrider[] = {FRM_BOARRIDER, FRM_BOARRIDER + 1, FRM_BOARRIDER + 2,
                      FRM_BOARRIDER + 3};

int at_carpet[] = {FRM_CARPET, FRM_CARPET + 1};

int at_tower[] = {FRM_TOWER,     FRM_TOWER + 1, FRM_TOWER + 2, FRM_TOWER + 3,
                  FRM_TOWER,     FRM_TOWER + 1, FRM_TOWER + 2, FRM_TOWER,
                  FRM_TOWER + 1, FRM_TOWER + 2};

int at_monk[] = {FRM_MONK, FRM_MONK + 1, FRM_MONK + 2};

int at_skeleton_monk[] = {FRM_SKELETON_MONK, FRM_SKELETON_MONK + 1,
                          FRM_SKELETON_MONK + 2};

int at_skeleton_horse[] = {FRM_SKELETON_HORSE, FRM_SKELETON_HORSE + 1,
                           FRM_SKELETON_HORSE + 2, FRM_SKELETON_HORSE + 3};

int at_horse[] = {FRM_HORSE, FRM_HORSE + 1, FRM_HORSE + 2, FRM_HORSE + 3};

int at_spearman[] = {FRM_SPEARMAN + 4, FRM_SPEARMAN + 5, FRM_SPEARMAN,
                     FRM_SPEARMAN + 1, FRM_SPEARMAN + 2, FRM_SPEARMAN + 3};

int at_spearman_throw[] = {FRM_SPEARMAN + 6, FRM_SPEARMAN + 7, FRM_SPEARMAN + 8,
                           FRM_SPEARMAN + 7};

int at_wizard[] = {FRM_WIZARD + 4, FRM_WIZARD + 5, FRM_WIZARD,
                   FRM_WIZARD + 1, FRM_WIZARD + 2, FRM_WIZARD + 3};

int at_wizard_throw[] = {FRM_WIZARD + 6, FRM_WIZARD + 7, FRM_WIZARD + 8,
                         FRM_WIZARD + 7};

int at_footman[] = {FRM_FOOTMAN + 1, FRM_FOOTMAN + 2, FRM_FOOTMAN + 3,
                    FRM_FOOTMAN + 4, FRM_FOOTMAN + 5, FRM_FOOTMAN + 6,
                    FRM_FOOTMAN + 7, FRM_FOOTMAN};

int at_footman_lower[] = {FRM_FOOTMAN + 14, FRM_FOOTMAN + 15, FRM_FOOTMAN + 16,
                          FRM_FOOTMAN + 13};

int at_footman_upper[] = {FRM_FOOTMAN + 8, FRM_FOOTMAN + 9, FRM_FOOTMAN + 10,
                          FRM_FOOTMAN};

int at_footman_upper_mase[] = {FRM_FOOTADD + 2, FRM_FOOTADD + 3,
                               FRM_FOOTADD + 4, -1};

int at_footman_lower_mase[] = {FRM_FOOTADD + 2, FRM_FOOTADD, FRM_FOOTADD + 1,
                               -1};

int at_knight[] = {FRM_KNIGHT + 1, FRM_KNIGHT + 2, FRM_KNIGHT + 3,
                   FRM_KNIGHT + 4, FRM_KNIGHT + 5, FRM_KNIGHT + 6,
                   FRM_KNIGHT + 7, FRM_KNIGHT};

int at_knight_lower[] = {FRM_KNIGHT + 14, FRM_KNIGHT + 15, FRM_KNIGHT + 16,
                         FRM_KNIGHT + 13};

int at_knight_upper[] = {FRM_KNIGHT + 8, FRM_KNIGHT + 9, FRM_KNIGHT + 10,
                         FRM_KNIGHT};

int at_knight_upper_mase[] = {FRM_KNIGHTADD + 3, FRM_KNIGHTADD + 4,
                              FRM_KNIGHTADD + 5, -1};

int at_knight_lower_mase[] = {FRM_KNIGHTADD, FRM_KNIGHTADD + 1,
                              FRM_KNIGHTADD + 2, -1};

int at_balista[] = {FRM_BALISTA, FRM_BALISTA + 1, FRM_BALISTA + 2,
                    FRM_BALISTA + 1};

int at_oil[] = {FRM_OIL,     FRM_OIL + 1, FRM_OIL,
                FRM_OIL + 1, FRM_OIL + 2, FRM_OIL + 1};

int at_cannon[] = {FRM_CANNON, FRM_CANNON,     FRM_CANNON,     FRM_CANNON,
                   FRM_CANNON, FRM_CANNON + 1, FRM_CANNON + 1, FRM_CANNON + 2};

int at_cannon_flame[] = {FRM_FLAME, FRM_FLAME, FRM_FLAME + 1, -1};

int at_beserk[] = {FRM_BESERK,     FRM_BESERK + 1, FRM_BESERK + 2,
                   FRM_BESERK + 3, FRM_BESERK + 4, FRM_BESERK + 5};

int at_beserk_upper_mase[] = {FRM_BKADDON, FRM_BKADDON + 1, FRM_BKADDON + 2,
                              -1};

int at_beserk_lower_mase[] = {FRM_BKADDON + 3, FRM_BKADDON + 4, FRM_BKADDON + 5,
                              -1};

int at_skeleton[] = {FRM_SKELETON + 1, FRM_SKELETON + 2, FRM_SKELETON + 3,
                     FRM_SKELETON + 4, FRM_SKELETON + 5, FRM_SKELETON + 6,
                     FRM_SKELETON + 7, FRM_SKELETON};

int at_skeleton_lower[] = {FRM_SKELETON + 12, FRM_SKELETON + 13,
                           FRM_SKELETON + 14, FRM_SKELETON + 13};

int at_skeleton_upper[] = {FRM_SKELETON + 8, FRM_SKELETON + 9,
                           FRM_SKELETON + 10, FRM_SKELETON + 9};

int at_skeleton_upper_mase[] = {FRM_SKELADD, -2, FRM_SKELADD + 1, -1};

int at_skeleton_lower_mase[] = {FRM_SKELADD + 2, -2, FRM_SKELADD + 3, -1};

int at_bone[] = {FRM_BONES, FRM_BONES + 1, FRM_BONES + 2};

int at_skeleton_item[] = {FRM_BONES + 3, FRM_BONES + 4};

int at_wizard_shot_strength[] = {FRM_WIZSHOT, FRM_WIZSHOT + 1};

int at_wizard_shot_power[] = {FRM_WIZSHOT + 2, FRM_WIZSHOT + 3};

int at_smith[] = {FRM_SMITH,     FRM_SMITH + 1, FRM_SMITH + 2,
                  FRM_SMITH + 1, FRM_SMITH,     -1};

// movement tables

int mt_upper_mase_l[] = {32, 0, 32, 0, 8, 0, 8, 0, -16, 0, -16, 0};

int mt_upper_mase_r[] = {-16, 0, -16, 0, 8, 0, 8, 0, 32, 0, 32, 0};

int mt_lower_mase_l[] = {32, -8, 32, -8, 8, 0, 8, 0, -16, 0, -16, 0};

int mt_lower_mase_r[] = {-16, -8, -16, -8, 8, 0, 8, 0, 32, 0, 32, 0};

int mt_big_crossbow_l[] = {-16, 16, -16, 16, -16, 16, -16, 16,
                           -16, 0,  -16, 0,  -16, 0,  -16, 0};

int mt_big_crossbow_r[] = {32, 16, 32, 16, 32, 16, 32, 16,
                           32, 0,  32, 0,  32, 0,  32, 0};

int mt_small_crossbow_l[] = {0, -16, -16, 0, -16, 0};

int mt_small_crossbow_r[] = {16, -16, 32, 0, 32, 0};

int mt_naptha_l[] = {32, 0, 32, 0,   32, 0,   0, 0,   0,
                     0,  0, 0,  -16, 0,  -16, 0, -16, 0};

int mt_naptha_r[] = {-16, 0, -16, 0,  -16, 0,  0, 0,  0,
                     0,   0, 0,   32, 0,   32, 0, 32, 0};

int mt_carpet[] = {0, -1, 0, -2, 0, -1, 0, 0, 0, 1, 0, 2, 0, 1, 0, 0};

int mt_beserk_upper_mase_l[] = {32, 0, 32, 0,   32, 0,   8, 0,   8,
                                0,  8, 0,  -16, 0,  -16, 0, -16, 0};

int mt_beserk_upper_mase_r[] = {-16, 0, -16, 0,  -16, 0,  8, 0,  8,
                                0,   8, 0,   32, 0,   32, 0, 32, 0};

int mt_beserk_lower_mase_l[] = {32, -8, 32, -8,  32, -8,  8, 0,   8,
                                0,  8,  0,  -16, 0,  -16, 0, -16, 0};

int mt_beserk_lower_mase_r[] = {-16, -8, -16, -8, -16, -8, 8, 0,  8,
                                0,   8,  0,   32, 0,   32, 0, 32, 0};

int mt_footman_upper_mase_l[] = {32, 0, 32, 0, 8, 0, 8, 0, -16, 0, -16, 0};

int mt_footman_upper_mase_r[] = {-16, 0, -16, 0, 8, 0, 8, 0, 32, 0, 32, 0};

int mt_footman_lower_mase_l[] = {32, -3, 32, -3, 16, 0, 16, 0, -16, 0, -16, 0};

int mt_footman_lower_mase_r[] = {-16, -3, -16, -3, 0, 0, 0, 0, 32, 0, 32, 0};

int mt_knight_mase_l[] = {32, 0, 32, 0, 16, 0, 16, 0, -16, 0, -16, 0};

int mt_knight_mase_r[] = {-16, 0, -16, 0, 0, 0, 0, 0, 32, 0, 32, 0};

int mt_skeleton_mase_l[] = {32, 0, 32, 0, 0, 0, 0, 0, -16, 0, -16, 0};

int mt_skeleton_mase_r[] = {-16, 0, -16, 0, 16, 0, 16, 0, 32, 0, 32, 0};

int mt_mind_arm[] = {12, 2,  10, 3,  8,  4,  6,  5,  4,  6,  2,  7,  2,
                     7,  4,  6,  6,  5,  8,  4,  10, 3,  12, 2,  12, 0,
                     12, -2, 10, -3, 8,  -4, 6,  -5, 4,  -6, 2,  -7, 2,
                     -7, 4,  -6, 6,  -5, 8,  -4, 10, -3, 12, -2, 12, 0};

// data tabels

int jump_offsets[] = {FRM_DUCK, 0, FRM_DUCK, 0, FRM_JUMP,     7, FRM_JUMP, 6,
                      FRM_JUMP, 5, FRM_JUMP, 4, FRM_JUMP,     3, FRM_JUMP, 3,
                      FRM_JUMP, 2, FRM_JUMP, 1, FRM_WALK + 1, 1};

int blat_offsets[] = {-32, -32, 0,  -32, 0,   0,   -32, 0,
                      -40, -16, 8,  -16, -16, -40, -16, 8,
                      -24, -16, -8, -16, -16, -24, -16, -8};

int bomb_offsets[] = {-32, -16, -16, -32, 0, -16, -16, 0};

int frag_vectors[] = {0, -8, 6, -6, 8, 0, 6, 6, 0, 8, -6, 6, -8, 0, -6, -6};

int bone_vectors[] = {1, -8, -1, -8, 2, -6, -2, -6};

int manbits_vectors[] = {FRM_MANBITS + 1, 0,  -10, FRM_MANBITS + 4, 1, -8,
                         FRM_MANBITS + 2, -1, -8,  FRM_MANBITS,     2, -6,
                         FRM_MANBITS + 3, -2, -6};

use_func itemtable[] = {
    use_gold,         use_mase,        use_big_crossbow, use_small_crossbow,
    use_naptha,       use_helper,      use_super_mase,   use_super_helper,
    use_yellow_spell, use_black_spell, use_green_spell,  use_white_spell,
    use_red_spell,    use_blue_spell,  use_talisman,     use_talisman,
    use_talisman,     use_talisman,    use_talisman,     use_talisman,
    use_talisman,     use_talisman,    use_talisman,     use_talisman};

int droptable[] = {FRM_SHIELD,     FRM_SHIELD + 1, FRM_SHIELD + 2,
                   FRM_SHIELD + 3, FRM_SHIELD + 4, FRM_SHIELD + 5,
                   FRM_SHIELD + 6, FRM_SPELLS,     FRM_SPELLS + 1,
                   FRM_SPELLS + 2, FRM_SPELLS + 3, FRM_SPELLS + 4,
                   FRM_SPELLS + 4, FRM_SPELLS + 5, FRM_SPELLS + 5};

int minditems[] = {FRM_DIAMOND,     FRM_SPELLS + 5,  FRM_DIAMOND + 5,
                   FRM_SPELLS + 5,  FRM_DIAMOND + 8, FRM_SPELLS + 5,
                   FRM_DIAMOND + 7, FRM_SPELLS + 5,  FRM_DIAMOND + 6,
                   FRM_SPELLS + 5,  FRM_DIAMOND + 9, FRM_SPELLS + 5,
                   FRM_DIAMOND + 4};

int selecttable[] = {19, 9,  11, 9, 12, 16, 14, 17, 15, 10, 18, 13,
                     20, 20, 8,  8, 8,  8,  8,  8,  8,  8,  8,  8};

play_func sfxtable[] = {play_horse,
                        play_death,
                        play_death,
                        play_death,
                        play_death,
                        play_clash1,
                        play_clash2,
                        play_clash2,
                        play_boar,
                        play_spell,
                        0,
                        0,
                        play_spell,
                        play_explode,
                        play_boar,
                        play_death,
                        0,
                        0,
                        0};

int scoretable[] = {50, 5, 10, 15, 20, 10, 10, 10, 25, 25,
                    80, 5, 50, 10, 50, 20, 1,  0,  0};

int popularity[] = {0,    500, 200, 1000, 300, 1600, 700, 800,
                    1800, 400, 900, 1200, 600, 1400, 100, 2000};

init_func init_enemy_table[] = {
    init_enemy_left,    init_enemy_right,   init_enemy_left,
    init_enemy_right,   init_enemy_left,    init_enemy_right,
    init_enemy_left,    init_enemy_right,   init_enemy_left,
    init_enemy_right,   init_balista_left,  init_balista_right,
    init_balista_left,  init_balista_right, init_balista_left,
    init_balista_right, init_enemy_left,    init_enemy_right,
    init_enemy_left,    init_enemy_right,   init_enemy_left,
    init_enemy_right,   init_enemy_left,    init_enemy_right,
    init_balista_left,  init_balista_right, init_enemy_left,
    init_enemy_right,   init_enemy_left,    init_enemy_right,
    init_enemy_left,    init_enemy_right,
};

int enemy_size_table[] = {
    64, 64, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
    64, 32, 64, 32, 64, 64, 32, 32, 32, 32, 32, 32, 64, 64, 32, 32,
};

// map data

unsigned char fieldmap1[] = {
    0x18, 0x15, 0x1b, 0x19, 0x15, 0x29, 0x00, 0x4e, 0x00, 0x00, 0x4e, 0x4b,
    0x4c, 0x4d, 0x00, 0x4e, 0x1f, 0x4e, 0x00, 0x00, 0x4b, 0x4c, 0x4d, 0x4e,
    0x00, 0x00, 0x4e, 0x4b, 0x4c, 0x4c, 0x4c, 0x4c, 0x4d, 0x4e, 0x00, 0x4e,
    0x00, 0x00, 0x4b, 0x4d, 0x4e, 0x00, 0x4b, 0x4c, 0x4c, 0x4d, 0x4e, 0x00,
    0x00, 0x4e, 0x00, 0x4e, 0x00, 0x4b, 0x4c, 0x4c, 0x4c, 0x4d, 0x00, 0x00,
    0x4e, 0x4b, 0x4d, 0x1f, 0x4b, 0x4d, 0x4e, 0x00, 0x4e, 0x4b, 0x4d, 0x00,
    0x00, 0x4e, 0x00, 0x00, 0x00, 0x4b, 0x4c, 0x4c, 0x4d, 0x00, 0x4e, 0x00,
    0x4b, 0x4d, 0x4e, 0x4e, 0x00, 0x57, 0x5b, 0x00, 0x00, 0x4b, 0x4c, 0x4c,
    0x4d, 0x00, 0x4e, 0x00, 0x4e, 0x4b, 0x4d, 0x4e, 0x00, 0x4e, 0x1f, 0x4e,
    0x20, 0x20, 0x00, 0x57, 0x5b, 0x1f, 0x00, 0x4e, 0x00, 0x00, 0x4e, 0x4e,
    0x00, 0x00, 0x4e, 0x4e, 0x54, 0x14, 0x19, 0x14, 0x09, 0x2e, 0x2f, 0x30,
    0x31, 0x0a, 0x05, 0x00, 0x4b, 0x4d, 0x00, 0x00, 0x4e, 0x4e, 0x4e, 0x00,
    0x20, 0x00, 0x4e, 0x4e, 0x4e, 0x00, 0x57, 0x5b, 0x00, 0x4e, 0x00, 0x00,
    0x4e, 0x00, 0x00, 0x00, 0x4e, 0x00, 0x4b, 0x4c, 0x4d, 0x4e, 0x00, 0x4e,
    0x4e, 0x4e, 0x4e, 0x00, 0x4e, 0x4e, 0x00, 0x4e, 0x4e, 0x00, 0x4e, 0x00,
    0x4e, 0x00, 0x00, 0x4e, 0x4e, 0x00, 0x4e, 0x00, 0x00, 0x4e, 0x4e, 0x20,
    0x4e, 0x4e, 0x00, 0x1f, 0x4e, 0x4e, 0x4e, 0x00, 0x4e, 0x00, 0x4b, 0x4d,
    0x00, 0x00, 0x4e, 0x4e, 0x00, 0x00, 0x4e, 0x4e, 0x00, 0x4e, 0x00, 0x4e,
    0x58, 0x59, 0x5a, 0x4e, 0x00, 0x00, 0x00, 0x4e, 0x00, 0x00, 0x00, 0x00,
    0x4e, 0x4e, 0x00, 0x4e, 0x4e, 0x4b, 0x52, 0x4c, 0x52, 0x52, 0x58, 0x59,
    0x5a, 0x20, 0x4e, 0x00, 0x4e, 0x00, 0x00, 0x4e, 0x4b, 0x4c, 0x4d, 0x4e,
    0x25, 0x14, 0x14, 0x18, 0x70, 0x72, 0x7c, 0x7d, 0x79, 0x71, 0x70, 0x00,
    0x00, 0x4b, 0x4c, 0x4c, 0x4d, 0x4e, 0x00, 0x00, 0x27, 0x00, 0x4b, 0x4d,
    0x00, 0x58, 0x59, 0x5a, 0x4b, 0x4c, 0x4d, 0x4e, 0x00, 0x00, 0x4b, 0x4c,
    0x4d, 0x00, 0x4e, 0x00, 0x4b, 0x4c, 0x4c, 0x4c, 0x4d, 0x00, 0x00, 0x4e,
    0x00, 0x00, 0x4b, 0x4c, 0x4d, 0x4e, 0x00, 0x00, 0x00, 0x00, 0x1f, 0x4c,
    0x4d, 0x00, 0x4e, 0x4b, 0x4d, 0x4e, 0x00, 0x20, 0x00, 0x1f, 0x00, 0x20,
    0x00, 0x4b, 0x4c, 0x4c, 0x4d, 0x00, 0x00, 0x00, 0x00, 0x4b, 0x4d, 0x00,
    0x00, 0x00, 0x4b, 0x4c, 0x4d, 0x00, 0x4e, 0x4b, 0x4c, 0x52, 0x4c, 0x4d,
    0x00, 0x4e, 0x4b, 0x4d, 0x4e, 0x00, 0x00, 0x4b, 0x4c, 0x4d, 0x4e, 0x00,
    0x00, 0x00, 0x20, 0x4e, 0x20, 0x20, 0x00, 0x20, 0x4b, 0x52, 0x4d, 0x4e,
    0x00, 0x4b, 0x4c, 0x4d, 0x4e, 0x00, 0x4e, 0x4e, 0x00, 0x2d, 0x14, 0x15,
    0x3a, 0x3b, 0x7a, 0x7b, 0x41, 0x18, 0x14, 0x53, 0x00, 0x00, 0x00, 0x4e,
    0x00, 0x00, 0x4b, 0x4d, 0x28, 0x00, 0x00, 0x00, 0x00, 0x00, 0x28, 0x37,
    0x00, 0x1f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x4e, 0x00, 0x4b, 0x4d, 0x00, 0x00, 0x4e, 0x00, 0x00, 0x4e,
    0x00, 0x00, 0x4b, 0x4d, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x57, 0x5b,
    0x4b, 0x4c, 0x4c, 0x52, 0x4c, 0x52, 0x4d, 0x20, 0x00, 0x00, 0x00, 0x4e,
    0x00, 0x03, 0x50, 0x32, 0x33, 0x34, 0x01, 0x50, 0x00, 0x4e, 0x00, 0x00,
    0x4b, 0x4d, 0x00, 0x00, 0x00, 0x20, 0x00, 0x00, 0x4e, 0x00, 0x4e, 0x00,
    0x00, 0x00, 0x00, 0x4e, 0x00, 0x4e, 0x00, 0x00, 0x1f, 0x50, 0x20, 0x50,
    0x20, 0x20, 0x50, 0x20, 0x50, 0x20, 0x02, 0x1f, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x4b, 0x4d, 0x00, 0x25, 0x0a, 0x3f, 0x49, 0x3c, 0x3d, 0x7e, 0x7f,
    0x42, 0x0a, 0x0a, 0x29, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x20, 0x01, 0x50, 0x04, 0x1f, 0x50, 0x27, 0x38, 0x39, 0x20, 0x50, 0x1f,
    0x32, 0x33, 0x34, 0x03, 0x01, 0x02, 0x4b, 0x4d, 0x4e, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x4e, 0x00, 0x4b, 0x4c, 0x4d, 0x00, 0x4e, 0x00, 0x00, 0x4e,
    0x00, 0x00, 0x20, 0x00, 0x4b, 0x58, 0x59, 0x5a, 0x4e, 0x00, 0x00, 0x20,
    0x00, 0x20, 0x4b, 0x52, 0x4c, 0x4d, 0x00, 0x03, 0x33, 0x70, 0x70, 0x71,
    0x79, 0x70, 0x71, 0x70, 0x29, 0x4b, 0x4d, 0x4e, 0x00, 0x00, 0x4e, 0x00,
    0x4b, 0x52, 0x4d, 0x00, 0x4b, 0x4d, 0x00, 0x00, 0x00, 0x4b, 0x4c, 0x4d,
    0x00, 0x00, 0x4e, 0x25, 0x71, 0x7c, 0x7d, 0x79, 0x72, 0x70, 0x70, 0x79,
    0x70, 0x70, 0x70, 0x70, 0x29, 0x50, 0x00, 0x35, 0x00, 0x00, 0x50, 0x50,
    0x25, 0x71, 0x75, 0x76, 0x79, 0x71, 0x70, 0x72, 0x7c, 0x7d, 0x70, 0x00,
    0x35, 0x00, 0x00, 0x00, 0x00, 0x35, 0x00, 0x25, 0x71, 0x70, 0x79, 0x71,
    0x70, 0x72, 0x79, 0x70, 0x72, 0x79, 0x70, 0x71, 0x70, 0x79, 0x72, 0x70,
    0x70, 0x70, 0x50, 0x01, 0x00, 0x35, 0x35, 0x00, 0x00, 0x00, 0x35, 0x00,
    0x00, 0x00, 0x35, 0x00, 0x00, 0x4e, 0x35, 0x00, 0x4e, 0x50, 0x20, 0x50,
    0x00, 0x50, 0x20, 0x02, 0x05, 0x35, 0x00, 0x20, 0x35, 0x20, 0x00, 0x20,
    0x37, 0x35, 0x04, 0x70, 0x75, 0x14, 0x1d, 0x16, 0x21, 0x19, 0x16, 0x14,
    0x53, 0x00, 0x00, 0x35, 0x00, 0x00, 0x00, 0x35, 0x00, 0x20, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x35, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x54,
    0x14, 0x7a, 0x7b, 0x21, 0x19, 0x16, 0x1a, 0x68, 0x14, 0x1b, 0x19, 0x14,
    0x72, 0x70, 0x29, 0x00, 0x35, 0x25, 0x71, 0x7c, 0x7d, 0x14, 0x19, 0x15,
    0x16, 0x1a, 0x15, 0x10, 0x7a, 0x7b, 0x18, 0x29, 0x00, 0x00, 0x35, 0x00,
    0x35, 0x00, 0x00, 0x54, 0x19, 0x1a, 0x21, 0x16, 0x49, 0x0b, 0x0c, 0x0d,
    0x5c, 0x0a, 0x16, 0x1a, 0x16, 0x23, 0x14, 0x16, 0x10, 0x14, 0x70, 0x70,
    0x50, 0x04, 0x32, 0x33, 0x50, 0x32, 0x33, 0x34, 0x02, 0x00, 0x00, 0x50,
    0x01, 0x50, 0x00, 0x4e, 0x25, 0x71, 0x72, 0x70, 0x71, 0x72, 0x79, 0x70,
    0x70, 0x50, 0x50, 0x20, 0x50, 0x20, 0x02, 0x20, 0x38, 0x39, 0x73, 0x0a,
    0x18, 0x21, 0x1a, 0x1b, 0x1a, 0x14, 0x19, 0x14, 0x29, 0x35, 0x4e, 0x00,
    0x00, 0x35, 0x04, 0x03, 0x50, 0x20, 0x50, 0x01, 0x00, 0x35, 0x00, 0x00,
    0x00, 0x00, 0x35, 0x00, 0x00, 0x00, 0x00, 0x35, 0x2d, 0x7a, 0x7b, 0x16,
    0x16, 0x5d, 0x5e, 0x67, 0x1a, 0x16, 0x16, 0x23, 0x15, 0x19, 0x53, 0x35,
    0x00, 0x54, 0x19, 0x7a, 0x7b, 0x16, 0x21, 0x18, 0x10, 0x18, 0x43, 0x44,
    0x7e, 0x7f, 0x26, 0x32, 0x33, 0x34, 0x00, 0x36, 0x00, 0x36, 0x00, 0x25,
    0x0a, 0x13, 0x71, 0x70, 0x76, 0x70, 0x79, 0x7c, 0x7d, 0x70, 0x1d, 0x06,
    0x19, 0x24, 0x19, 0x1b, 0x16, 0x1a, 0x19, 0x15, 0x70, 0x72, 0x71, 0x7c,
    0x7d, 0x70, 0x70, 0x71, 0x29, 0x35, 0x25, 0x70, 0x71, 0x70, 0x29, 0x35,
    0x54, 0x14, 0x14, 0x19, 0x16, 0x15, 0x21, 0x68, 0x14, 0x70, 0x71, 0x70,
    0x7c, 0x7d, 0x72, 0x79, 0x71, 0x72, 0x71, 0x70, 0x1d, 0x09, 0x10, 0x18,
    0x16, 0x19, 0x16, 0x26, 0x02, 0x00, 0x35, 0x32, 0x33, 0x34, 0x71, 0x71,
    0x70, 0x79, 0x72, 0x70, 0x50, 0x50, 0x00, 0x00, 0x35, 0x00, 0x00, 0x00,
    0x35, 0x00, 0x00, 0x25, 0x14, 0x7a, 0x7b, 0x5f, 0x60, 0x61, 0x62, 0x18,
    0x69, 0x6a, 0x18, 0x24, 0x16, 0x10, 0x29, 0x00, 0x00, 0x36, 0x2d, 0x7a,
    0x7b, 0x14, 0x1a, 0x14, 0x15, 0x15, 0x45, 0x71, 0x70, 0x79, 0x72, 0x7c,
    0x7d, 0x70, 0x37, 0x00, 0x04, 0x33, 0x03, 0x71, 0x70, 0x71, 0x1d, 0x68,
    0x14, 0x16, 0x21, 0x7a, 0x7b, 0x14, 0x71, 0x70, 0x1d, 0x22, 0x21, 0x19,
    0x10, 0x15, 0x16, 0x15, 0x15, 0x18, 0x21, 0x7a, 0x7b, 0x1b, 0x1a, 0x15,
    0x29, 0x00, 0x36, 0x1e, 0x14, 0x26, 0x37, 0x36, 0x00, 0x2d, 0x16, 0x1a,
    0x19, 0x5d, 0x5e, 0x67, 0x16, 0x18, 0x1a, 0x16, 0x7a, 0x7b, 0x14, 0x18,
    0x16, 0x3a, 0x3b, 0x16, 0x71, 0x70, 0x1d, 0x0a, 0x49, 0x0a, 0x18, 0x14,
    0x53, 0x36, 0x25, 0x71, 0x7c, 0x7d, 0x21, 0x1a, 0x16, 0x19, 0x16, 0x18,
    0x71, 0x70, 0x02, 0x01, 0x00, 0x00, 0x35, 0x00, 0x00, 0x35, 0x00, 0x25,
    0x5c, 0x7e, 0x7f, 0x63, 0x64, 0x65, 0x66, 0x6c, 0x6d, 0x6e, 0x6f, 0x24,
    0x19, 0x2c, 0x37, 0x36, 0x36, 0x25, 0x19, 0x7a, 0x7b, 0x1b, 0x2c, 0x25,
    0x41, 0x1a, 0x19, 0x1a, 0x3a, 0x3b, 0x15, 0x7a, 0x7b, 0x2c, 0x38, 0x39,
    0x71, 0x75, 0x70, 0x18, 0x16, 0x5d, 0x5e, 0x67, 0x16, 0x1a, 0x0a, 0x7e,
    0x7f, 0x1c, 0x18, 0x16, 0x70, 0x71, 0x70, 0x1d, 0x0a, 0x21, 0x2e, 0x2f,
    0x30, 0x31, 0x5c, 0x7e, 0x7f, 0x49, 0x1c, 0x3f, 0x16, 0x29, 0x25, 0x08,
    0x5c, 0x0a, 0x38, 0x39, 0x25, 0x18, 0x16, 0x5f, 0x60, 0x61, 0x62, 0x18,
    0x14, 0x1b, 0x10, 0x14, 0x7a, 0x7b, 0x19, 0x10, 0x16, 0x3c, 0x3d, 0x30,
    0x31, 0x5c, 0x73, 0x77, 0x76, 0x73, 0x1d, 0x16, 0x29, 0x36, 0x54, 0x16,
    0x7e, 0x7f, 0x07, 0x4a, 0x2f, 0x2e, 0x2f, 0x30, 0x31, 0x5c, 0x77, 0x73,
    0x50, 0x01, 0x50, 0x00, 0x36, 0x00, 0x36, 0x25, 0x71, 0x72, 0x70, 0x79,
    0x70, 0x7c, 0x7d, 0x70, 0x72, 0x72, 0x79, 0x70, 0x1d, 0x0a, 0x38, 0x39,
    0x05, 0x01, 0x21, 0x7a, 0x7b, 0x15, 0x19, 0x1a, 0x42, 0x0a, 0x3f, 0x0a,
    0x3c, 0x3d, 0x49, 0x7e, 0x7f, 0x0a, 0x79, 0x71, 0x15, 0x16, 0x1b, 0x5f,
    0x60, 0x61, 0x62, 0x43, 0x44, 0x13, 0x79, 0x70, 0x71, 0x70, 0x1d, 0x09,
    0x21, 0x16, 0x14, 0x71, 0x70, 0x71, 0x72, 0x70, 0x71, 0x70, 0x70, 0x70,
    0x70, 0x76, 0x72, 0x75, 0x70, 0x29, 0x54, 0x71, 0x70, 0x72, 0x79, 0x70,
    0x33, 0x0a, 0x0a, 0x63, 0x64, 0x65, 0x66, 0x16, 0x69, 0x6a, 0x6b, 0x16,
    0x7a, 0x7b, 0x43, 0x44, 0x13, 0x71, 0x70, 0x71, 0x79, 0x70, 0x70, 0x71,
    0x71, 0x79, 0x71, 0x72, 0x29, 0x00, 0x25, 0x70, 0x70, 0x71, 0x71, 0x72,
    0x70, 0x7c, 0x7d, 0x79, 0x70, 0x70, 0x72, 0x70, 0x79, 0x71, 0x70, 0x29,
    0x36, 0x36, 0x00, 0x54, 0x18, 0x16, 0x16, 0x21, 0x1a, 0x7a, 0x7b, 0x16,
    0x14, 0x14, 0x21, 0x19, 0x71, 0x70, 0x79, 0x7c, 0x7d, 0x70, 0x1d, 0x7e,
    0x7f, 0x08, 0x0a, 0x06, 0x75, 0x70, 0x75, 0x72, 0x71, 0x79, 0x76, 0x70,
    0x73, 0x73, 0x1d, 0x0b, 0x2f, 0x30, 0x31, 0x63, 0x64, 0x65, 0x66, 0x45,
    0x73, 0x77, 0x2e, 0x2f, 0x30, 0x31, 0x73, 0x73, 0x73, 0x1d, 0x30, 0x31,
    0x06, 0x1c, 0x5c, 0x1c, 0x19, 0x4a, 0x0a, 0x0b, 0x0c, 0x0d, 0x06, 0x14,
    0x53, 0x36, 0x25, 0x1a, 0x16, 0x19, 0x4a, 0x18, 0x75, 0x70, 0x79, 0x77,
    0x70, 0x74, 0x70, 0x6c, 0x6d, 0x6e, 0x6f, 0x16, 0x7a, 0x7b, 0x45, 0x71,
    0x70, 0x1a, 0x19, 0x16, 0x1a, 0x16, 0x18, 0x1a, 0x69, 0x6a, 0x6b, 0x53,
    0x36, 0x00, 0x36, 0x54, 0x1a, 0x1b, 0x16, 0x23, 0x18, 0x7a, 0x7b, 0x21,
    0x1a, 0x19, 0x16, 0x19, 0x16, 0x68, 0x19, 0x53, 0x00, 0x00, 0x36, 0x00,
    0x1e, 0x10, 0x1b, 0x19, 0x16, 0x7a, 0x7b, 0x19, 0x18, 0x10, 0x16, 0x18,
    0x21, 0x19, 0x1a, 0x7a, 0x7b, 0x10, 0x72, 0x70, 0x70, 0x70, 0x79, 0x70,
    0x15, 0x10, 0x15, 0x21, 0x1a, 0x18, 0x15, 0x13, 0x70, 0x71, 0x72, 0x71,
    0x70, 0x71, 0x7c, 0x7d, 0x70, 0x71, 0x72, 0x70, 0x71, 0x70, 0x70, 0x71,
    0x70, 0x72, 0x71, 0x70, 0x70, 0x71, 0x72, 0x70, 0x71, 0x72, 0x70, 0x71,
    0x73, 0x73, 0x72, 0x73, 0x73, 0x77, 0x70, 0x53, 0x36, 0x00, 0x36, 0x25,
    0x71, 0x70, 0x7c, 0x7d, 0x70, 0x71, 0x72, 0x71, 0x70, 0x70, 0x71, 0x70,
    0x71, 0x70, 0x70, 0x1d, 0x7e, 0x7f, 0x73, 0x5c, 0x1c, 0x09, 0x0a, 0x0b,
    0x0c, 0x30, 0x31, 0x6c, 0x6d, 0x6e, 0x6f, 0x29, 0x00, 0x00, 0x00, 0x00,
    0x50, 0x54, 0x2f, 0x24, 0x5c, 0x7e, 0x7f, 0x19, 0x16, 0x21, 0x18, 0x5d,
    0x5e, 0x67, 0x26, 0x00, 0x00, 0x00, 0x00, 0x25, 0x14, 0x1a, 0x19, 0x1a,
    0x21, 0x7a, 0x7b, 0x16, 0x53, 0x2a, 0x2a, 0x54, 0x16, 0x18, 0x21, 0x7a,
    0x7b, 0x16, 0x18, 0x15, 0x16, 0x1a, 0x19, 0x18, 0x1a, 0x15, 0x19, 0x1b,
    0x16, 0x10, 0x13, 0x71, 0x1a, 0x10, 0x19, 0x16, 0x1a, 0x21, 0x7a, 0x7b,
    0x53, 0x2a, 0x00, 0x2a, 0x54, 0x15, 0x16, 0x19, 0x23, 0x18, 0x0a, 0x16,
    0x21, 0x18, 0x10, 0x16, 0x3a, 0x3b, 0x08, 0x13, 0x71, 0x70, 0x71, 0x70,
    0x70, 0x70, 0x26, 0x0f, 0x0e, 0x0f, 0x0f, 0x0e, 0x0e, 0x1e, 0x7a, 0x7b,
    0x16, 0x1b, 0x14, 0x19, 0x1a, 0x16, 0x18, 0x53, 0x2a, 0x54, 0x18, 0x70,
    0x72, 0x79, 0x73, 0x77, 0x73, 0x73, 0x71, 0x70, 0x72, 0x71, 0x70, 0x79,
    0x70, 0x72, 0x71, 0x0e, 0x0f, 0x0e, 0x00, 0x0f, 0x71, 0x70, 0x70, 0x72,
    0x71, 0x71, 0x70, 0x3a, 0x3b, 0x5f, 0x60, 0x61, 0x62, 0x1a, 0x21, 0x0e,
    0x0e, 0x17, 0x0e, 0x0e, 0x19, 0x16, 0x14, 0x18, 0x1a, 0x7a, 0x7b, 0x53,
    0x0e, 0x0f, 0x0e, 0x0f, 0x2a, 0x54, 0x18, 0x7a, 0x7b, 0x1b, 0x19, 0x26,
    0x2d, 0x10, 0x16, 0x21, 0x2e, 0x2f, 0x0c, 0x0d, 0x1c, 0x13, 0x73, 0x18,
    0x3e, 0x3f, 0x40, 0x41, 0x1c, 0x09, 0x7e, 0x7f, 0x29, 0x32, 0x33, 0x34,
    0x50, 0x2d, 0x13, 0x73, 0x74, 0x73, 0x73, 0x49, 0x2e, 0x2f, 0x0c, 0x0d,
    0x3c, 0x3d, 0x13, 0x73, 0x2e, 0x2f, 0x30, 0x31, 0x23, 0x0a, 0x12, 0x11,
    0x12, 0x12, 0x12, 0x11, 0x12, 0x12, 0x7e, 0x7f, 0x5c, 0x2e, 0x2f, 0x2e,
    0x30, 0x31, 0x53, 0x00, 0x03, 0x00, 0x01, 0x2d, 0x09, 0x13, 0x74, 0x73,
    0x73, 0x73, 0x1d, 0x3f, 0x40, 0x46, 0x42, 0x40, 0x5c, 0x18, 0x0a, 0x12,
    0x12, 0x12, 0x12, 0x12, 0x2e, 0x30, 0x31, 0x5c, 0x06, 0x09, 0x49, 0x3c,
    0x3d, 0x63, 0x64, 0x65, 0x66, 0x4a, 0x5c, 0x12, 0x12, 0x11, 0x12, 0x12,
    0x0a, 0x49, 0x2f, 0x30, 0x31, 0x7e, 0x7f, 0x12, 0x12, 0x12, 0x12, 0x11,
    0x12, 0x12, 0x3f, 0x7e, 0x7f, 0x3e, 0x3f, 0x40, 0x2e, 0x2f, 0x30, 0x31,
    0x70, 0x70, 0x79, 0x70, 0x70, 0x72, 0x70, 0x70, 0x72, 0x79, 0x70, 0x70,
    0x70, 0x72, 0x70, 0x71, 0x70, 0x70, 0x70, 0x70, 0x72, 0x70, 0x71, 0x70,
    0x70, 0x72, 0x70, 0x76, 0x70, 0x79, 0x71, 0x70, 0x71, 0x79, 0x71, 0x72,
    0x70, 0x70, 0x71, 0x70, 0x71, 0x72, 0x70, 0x70, 0x70, 0x70, 0x72, 0x70,
    0x70, 0x71, 0x70, 0x71, 0x70, 0x70, 0x70, 0x71, 0x70, 0x71, 0x70, 0x71,
    0x70, 0x70, 0x72, 0x70, 0x70, 0x71, 0x70, 0x72, 0x72, 0x71, 0x71, 0x70,
    0x70, 0x70, 0x71, 0x72, 0x71, 0x70, 0x71, 0x70, 0x70, 0x72, 0x71, 0x70,
    0x71, 0x70, 0x70, 0x71, 0x70, 0x70, 0x76, 0x79, 0x72, 0x71, 0x71, 0x70,
    0x71, 0x70, 0x71, 0x71, 0x72, 0x70, 0x71, 0x70, 0x70, 0x76, 0x70, 0x79,
    0x72, 0x70, 0x70, 0x70, 0x70, 0x70, 0x72, 0x70, 0x70, 0x70, 0x75, 0x70,
    0x70, 0x72, 0x79, 0x70, 0x70, 0x72, 0x79, 0x70};

unsigned char fieldmap2[] = {
    0x4b, 0x4c, 0x4c, 0x4d, 0x4e, 0x4e, 0x4b, 0x4c, 0x4d, 0x4e, 0x00, 0x1f,
    0x00, 0x00, 0x36, 0x4e, 0x4e, 0x4b, 0x4c, 0x4c, 0x4d, 0x4e, 0x4b, 0x4d,
    0x4e, 0x4e, 0x4b, 0x4c, 0x4c, 0x4d, 0x00, 0x35, 0x36, 0x4e, 0x4b, 0x4c,
    0x4d, 0x4e, 0x4b, 0x4c, 0x4c, 0x4d, 0x4e, 0x4e, 0x35, 0x4e, 0x4b, 0x4c,
    0x4d, 0x00, 0x35, 0x00, 0x4e, 0x4b, 0x4c, 0x4c, 0x4c, 0x4d, 0x00, 0x4e,
    0x4e, 0x35, 0x4e, 0x4b, 0x4c, 0x4c, 0x4c, 0x4d, 0x00, 0x35, 0x4e, 0x4b,
    0x4c, 0x4c, 0x4d, 0x4e, 0x25, 0x14, 0x16, 0x26, 0x1e, 0x16, 0x14, 0x1a,
    0x14, 0x29, 0x4b, 0x4d, 0x4e, 0x4b, 0x4d, 0x36, 0x35, 0x36, 0x4b, 0x4d,
    0x4e, 0x4b, 0x4c, 0x4c, 0x4d, 0x4e, 0x4b, 0x4c, 0x4d, 0x4e, 0x4b, 0x4d,
    0x00, 0x4e, 0x4b, 0x4c, 0x4c, 0x4d, 0x4e, 0x00, 0x4e, 0x00, 0x4b, 0x4c,
    0x4c, 0x4c, 0x4d, 0x00, 0x4e, 0x4b, 0x4d, 0x4e, 0x35, 0x1f, 0x36, 0x4b,
    0x4c, 0x4d, 0x4e, 0x4e, 0x4e, 0x4e, 0x00, 0x20, 0x1f, 0x36, 0x1f, 0x4b,
    0x4c, 0x4d, 0x35, 0x00, 0x1f, 0x35, 0x4e, 0x4b, 0x4c, 0x4c, 0x4d, 0x4e,
    0x35, 0x4e, 0x00, 0x4b, 0x4c, 0x4c, 0x4d, 0x4e, 0x36, 0x4e, 0x35, 0x4e,
    0x35, 0x4e, 0x35, 0x4b, 0x4c, 0x4c, 0x4d, 0x00, 0x36, 0x35, 0x4e, 0x4b,
    0x4c, 0x4c, 0x4d, 0x00, 0x35, 0x00, 0x35, 0x4b, 0x4c, 0x4c, 0x4d, 0x4e,
    0x36, 0x4b, 0x4d, 0x35, 0x4b, 0x4c, 0x4c, 0x4d, 0x4e, 0x35, 0x00, 0x35,
    0x36, 0x2a, 0x14, 0x1a, 0x14, 0x14, 0x16, 0x2a, 0x2a, 0x35, 0x4e, 0x36,
    0x35, 0x4b, 0x4c, 0x4d, 0x4e, 0x4b, 0x4c, 0x4d, 0x36, 0x35, 0x36, 0x4e,
    0x35, 0x36, 0x4e, 0x35, 0x4e, 0x4b, 0x4c, 0x4d, 0x4e, 0x4b, 0x4d, 0x36,
    0x35, 0x4e, 0x35, 0x4b, 0x4d, 0x36, 0x4e, 0x36, 0x4b, 0x4d, 0x36, 0x4e,
    0x4b, 0x4d, 0x4e, 0x00, 0x36, 0x20, 0x00, 0x57, 0x5b, 0x4e, 0x35, 0x4e,
    0x4b, 0x4d, 0x01, 0x28, 0x20, 0x33, 0x20, 0x35, 0x00, 0x1f, 0x4e, 0x00,
    0x20, 0x00, 0x35, 0x4e, 0x36, 0x4e, 0x35, 0x4b, 0x4c, 0x4d, 0x00, 0x4e,
    0x35, 0x00, 0x36, 0x4e, 0x35, 0x04, 0x03, 0x01, 0x1f, 0x50, 0x33, 0x05,
    0x36, 0x00, 0x35, 0x4e, 0x4b, 0x4c, 0x4c, 0x4d, 0x35, 0x00, 0x4e, 0x00,
    0x4b, 0x4d, 0x4e, 0x4e, 0x36, 0x4e, 0x00, 0x35, 0x36, 0x35, 0x4e, 0x4b,
    0x4d, 0x36, 0x4e, 0x35, 0x00, 0x36, 0x4b, 0x4d, 0x35, 0x36, 0x1e, 0x14,
    0x16, 0x14, 0x2a, 0x4b, 0x4c, 0x4c, 0x4d, 0x4b, 0x4c, 0x4d, 0x4e, 0x35,
    0x4b, 0x4c, 0x4d, 0x35, 0x35, 0x4e, 0x4b, 0x4c, 0x4d, 0x35, 0x01, 0x50,
    0x01, 0x50, 0x36, 0x35, 0x00, 0x4e, 0x35, 0x00, 0x4b, 0x4c, 0x4d, 0x35,
    0x4e, 0x4b, 0x4c, 0x4d, 0x4e, 0x01, 0x50, 0x32, 0x33, 0x34, 0x05, 0x01,
    0x4e, 0x28, 0x58, 0x59, 0x5a, 0x4e, 0x36, 0x4b, 0x4d, 0x25, 0x70, 0x79,
    0x72, 0x75, 0x72, 0x70, 0x37, 0x20, 0x50, 0x00, 0x27, 0x00, 0x1f, 0x00,
    0x35, 0x36, 0x57, 0x5b, 0x4e, 0x36, 0x35, 0x35, 0x4e, 0x4b, 0x4c, 0x4d,
    0x25, 0x71, 0x7c, 0x7d, 0x71, 0x71, 0x75, 0x71, 0x29, 0x4b, 0x4d, 0x35,
    0x00, 0x35, 0x4e, 0x36, 0x4e, 0x4b, 0x4d, 0x36, 0x00, 0x36, 0x4e, 0x35,
    0x00, 0x01, 0x01, 0x4b, 0x4d, 0x50, 0x36, 0x35, 0x01, 0x50, 0x01, 0x4b,
    0x4d, 0x50, 0x50, 0x35, 0x36, 0x25, 0x14, 0x16, 0x1a, 0x26, 0x01, 0x50,
    0x35, 0x36, 0x01, 0x35, 0x36, 0x50, 0x50, 0x01, 0x35, 0x4e, 0x36, 0x4b,
    0x4d, 0x36, 0x50, 0x25, 0x71, 0x71, 0x70, 0x71, 0x70, 0x70, 0x29, 0x33,
    0x01, 0x35, 0x4e, 0x36, 0x00, 0x35, 0x4e, 0x36, 0x35, 0x50, 0x05, 0x03,
    0x25, 0x70, 0x71, 0x70, 0x79, 0x71, 0x70, 0x71, 0x35, 0x20, 0x00, 0x28,
    0x1f, 0x00, 0x35, 0x4e, 0x36, 0x25, 0x18, 0x16, 0x15, 0x15, 0x16, 0x15,
    0x79, 0x70, 0x70, 0x01, 0x20, 0x50, 0x20, 0x1f, 0x4e, 0x58, 0x59, 0x5a,
    0x35, 0x4e, 0x00, 0x36, 0x00, 0x4e, 0x00, 0x36, 0x04, 0x1e, 0x7a, 0x7b,
    0x53, 0x2a, 0x35, 0x2a, 0x00, 0x35, 0x00, 0x4e, 0x35, 0x36, 0x4e, 0x35,
    0x36, 0x35, 0x00, 0x01, 0x50, 0x50, 0x01, 0x25, 0x70, 0x71, 0x71, 0x70,
    0x79, 0x70, 0x70, 0x71, 0x71, 0x70, 0x79, 0x72, 0x70, 0x71, 0x70, 0x72,
    0x71, 0x70, 0x71, 0x71, 0x70, 0x71, 0x72, 0x71, 0x79, 0x71, 0x70, 0x71,
    0x70, 0x71, 0x71, 0x70, 0x71, 0x70, 0x29, 0x50, 0x25, 0x71, 0x70, 0x71,
    0x1d, 0x16, 0x14, 0x53, 0x16, 0x13, 0x70, 0x75, 0x70, 0x29, 0x50, 0x01,
    0x35, 0x4e, 0x32, 0x33, 0x34, 0x70, 0x71, 0x71, 0x70, 0x1d, 0x16, 0x14,
    0x53, 0x2a, 0x2a, 0x36, 0x1f, 0x27, 0x36, 0x20, 0x20, 0x00, 0x4e, 0x4b,
    0x4d, 0x36, 0x2a, 0x35, 0x2a, 0x54, 0x21, 0x16, 0x26, 0x2d, 0x15, 0x70,
    0x72, 0x70, 0x20, 0x20, 0x50, 0x00, 0x28, 0x1f, 0x4e, 0x4b, 0x4d, 0x00,
    0x1f, 0x35, 0x4e, 0x25, 0x15, 0x15, 0x7a, 0x7b, 0x29, 0x35, 0x4e, 0x4b,
    0x4d, 0x4e, 0x35, 0x4b, 0x4d, 0x4e, 0x04, 0x03, 0x33, 0x25, 0x71, 0x71,
    0x72, 0x71, 0x71, 0x70, 0x1d, 0x15, 0x16, 0x15, 0x53, 0x2a, 0x2a, 0x2a,
    0x35, 0x2a, 0x2a, 0x4b, 0x4d, 0x2a, 0x2a, 0x35, 0x54, 0x15, 0x16, 0x1a,
    0x15, 0x16, 0x16, 0x14, 0x53, 0x2a, 0x2a, 0x36, 0x2a, 0x35, 0x2a, 0x2a,
    0x25, 0x70, 0x71, 0x70, 0x71, 0x70, 0x1d, 0x16, 0x4a, 0x14, 0x29, 0x36,
    0x35, 0x2a, 0x14, 0x16, 0x13, 0x70, 0x71, 0x71, 0x29, 0x25, 0x71, 0x71,
    0x70, 0x1d, 0x16, 0x1b, 0x14, 0x14, 0x53, 0x2a, 0x35, 0x4e, 0x36, 0x4e,
    0x20, 0x20, 0x33, 0x27, 0x28, 0x50, 0x35, 0x4e, 0x1f, 0x00, 0x35, 0x36,
    0x1f, 0x00, 0x54, 0x15, 0x15, 0x21, 0x16, 0x23, 0x19, 0x15, 0x70, 0x72,
    0x71, 0x01, 0x20, 0x20, 0x50, 0x4e, 0x00, 0x00, 0x20, 0x00, 0x36, 0x04,
    0x1e, 0x16, 0x7a, 0x7b, 0x16, 0x29, 0x36, 0x4e, 0x36, 0x00, 0x36, 0x04,
    0x03, 0x25, 0x7c, 0x7d, 0x75, 0x72, 0x1d, 0x18, 0x15, 0x16, 0x15, 0x15,
    0x15, 0x15, 0x53, 0x2a, 0x4e, 0x36, 0x4e, 0x4b, 0x4d, 0x4e, 0x36, 0x4e,
    0x35, 0x36, 0x35, 0x00, 0x35, 0x54, 0x15, 0x26, 0x2d, 0x15, 0x53, 0x2a,
    0x35, 0x4e, 0x36, 0x35, 0x4e, 0x00, 0x4e, 0x35, 0x4e, 0x2a, 0x2a, 0x2a,
    0x2a, 0x25, 0x70, 0x79, 0x70, 0x53, 0x50, 0x33, 0x36, 0x37, 0x2a, 0x25,
    0x14, 0x1b, 0x13, 0x71, 0x71, 0x70, 0x2e, 0x2f, 0x30, 0x31, 0x26, 0x54,
    0x41, 0x26, 0x37, 0x35, 0x36, 0x36, 0x4e, 0x36, 0x71, 0x70, 0x72, 0x79,
    0x7c, 0x7d, 0x32, 0x33, 0x20, 0x00, 0x4e, 0x00, 0x20, 0x00, 0x35, 0x2a,
    0x54, 0x19, 0x21, 0x24, 0x15, 0x2c, 0x1e, 0x19, 0x15, 0x71, 0x71, 0x72,
    0x71, 0x50, 0x33, 0x01, 0x27, 0x00, 0x25, 0x15, 0x16, 0x15, 0x7a, 0x7b,
    0x26, 0x00, 0x35, 0x4e, 0x00, 0x35, 0x25, 0x71, 0x72, 0x79, 0x7a, 0x7b,
    0x18, 0x15, 0x19, 0x15, 0x53, 0x2a, 0x2a, 0x35, 0x2a, 0x2a, 0x36, 0x35,
    0x00, 0x35, 0x00, 0x4e, 0x4e, 0x35, 0x00, 0x1f, 0x00, 0x4e, 0x4e, 0x35,
    0x36, 0x01, 0x54, 0x15, 0x15, 0x53, 0x4e, 0x36, 0x35, 0x4e, 0x00, 0x4e,
    0x35, 0x4e, 0x35, 0x00, 0x57, 0x5b, 0x35, 0x4e, 0x36, 0x35, 0x54, 0x16,
    0x13, 0x71, 0x72, 0x79, 0x71, 0x38, 0x39, 0x35, 0x2a, 0x36, 0x54, 0x14,
    0x16, 0x13, 0x71, 0x70, 0x71, 0x70, 0x77, 0x50, 0x42, 0x53, 0x38, 0x39,
    0x05, 0x33, 0x03, 0x33, 0x10, 0x18, 0x16, 0x15, 0x7a, 0x7b, 0x70, 0x70,
    0x28, 0x50, 0x37, 0x00, 0x27, 0x1f, 0x36, 0x4e, 0x35, 0x2a, 0x54, 0x22,
    0x15, 0x16, 0x15, 0x21, 0x23, 0x18, 0x16, 0x4a, 0x09, 0x71, 0x75, 0x71,
    0x20, 0x25, 0x08, 0x4a, 0x2c, 0x1e, 0x7a, 0x7b, 0x15, 0x29, 0x36, 0x4e,
    0x36, 0x03, 0x33, 0x1e, 0x26, 0x2d, 0x7a, 0x7b, 0x15, 0x53, 0x2a, 0x2a,
    0x00, 0x35, 0x4e, 0x36, 0x36, 0x35, 0x36, 0x4e, 0x1f, 0x36, 0x00, 0x35,
    0x00, 0x4e, 0x00, 0x20, 0x4e, 0x35, 0x4e, 0x36, 0x25, 0x49, 0x3f, 0x16,
    0x15, 0x29, 0x36, 0x00, 0x4e, 0x36, 0x4e, 0x36, 0x35, 0x4e, 0x00, 0x58,
    0x59, 0x5a, 0x36, 0x4e, 0x35, 0x4e, 0x35, 0x54, 0x14, 0x16, 0x15, 0x21,
    0x13, 0x79, 0x72, 0x71, 0x29, 0x01, 0x36, 0x37, 0x2a, 0x16, 0x1b, 0x1b,
    0x26, 0x25, 0x71, 0x72, 0x7c, 0x7d, 0x70, 0x72, 0x71, 0x75, 0x79, 0x75,
    0x16, 0x10, 0x15, 0x41, 0x7e, 0x7f, 0x06, 0x49, 0x70, 0x77, 0x38, 0x39,
    0x28, 0x20, 0x50, 0x1f, 0x50, 0x02, 0x00, 0x27, 0x2a, 0x2a, 0x2a, 0x14,
    0x24, 0x4a, 0x13, 0x71, 0x71, 0x1d, 0x15, 0x13, 0x71, 0x72, 0x71, 0x71,
    0x1d, 0x49, 0x7e, 0x7f, 0x46, 0x47, 0x01, 0x35, 0x25, 0x70, 0x75, 0x70,
    0x72, 0x18, 0x7a, 0x7b, 0x26, 0x36, 0x35, 0x4e, 0x36, 0x00, 0x36, 0x4e,
    0x57, 0x5b, 0x00, 0x00, 0x20, 0x35, 0x4e, 0x4e, 0x36, 0x1f, 0x00, 0x28,
    0x00, 0x36, 0x36, 0x25, 0x70, 0x76, 0x75, 0x70, 0x53, 0x00, 0x35, 0x36,
    0x00, 0x00, 0x35, 0x00, 0x00, 0x57, 0x5b, 0x00, 0x28, 0x00, 0x4e, 0x35,
    0x4e, 0x36, 0x1f, 0x36, 0x2a, 0x54, 0x15, 0x16, 0x16, 0x15, 0x16, 0x13,
    0x7c, 0x7d, 0x79, 0x38, 0x39, 0x33, 0x3a, 0x3b, 0x10, 0x35, 0x2a, 0x4e,
    0x7a, 0x7b, 0x26, 0x2d, 0x16, 0x14, 0x21, 0x14, 0x2f, 0x30, 0x31, 0x71,
    0x70, 0x72, 0x70, 0x76, 0x79, 0x70, 0x72, 0x70, 0x72, 0x71, 0x79, 0x72,
    0x70, 0x70, 0x29, 0x28, 0x50, 0x1f, 0x02, 0x2d, 0x71, 0x71, 0x71, 0x72,
    0x3a, 0x3b, 0x16, 0x16, 0x14, 0x15, 0x15, 0x13, 0x71, 0x76, 0x79, 0x71,
    0x71, 0x48, 0x4a, 0x29, 0x36, 0x25, 0x15, 0x2c, 0x1e, 0x16, 0x7a, 0x7b,
    0x15, 0x29, 0x4e, 0x36, 0x00, 0x35, 0x00, 0x58, 0x59, 0x5a, 0x36, 0x35,
    0x28, 0x00, 0x1f, 0x00, 0x00, 0x20, 0x36, 0x27, 0x1f, 0x35, 0x25, 0x15,
    0x16, 0x2c, 0x1e, 0x15, 0x29, 0x35, 0x00, 0x00, 0x35, 0x36, 0x00, 0x4e,
    0x58, 0x59, 0x5a, 0x4e, 0x27, 0x36, 0x1f, 0x36, 0x00, 0x00, 0x20, 0x00,
    0x35, 0x36, 0x2a, 0x2a, 0x54, 0x14, 0x26, 0x2d, 0x7a, 0x7b, 0x13, 0x71,
    0x72, 0x75, 0x3c, 0x3d, 0x49, 0x29, 0x37, 0x25, 0x7a, 0x7b, 0x16, 0x14,
    0x1b, 0x2c, 0x1e, 0x4a, 0x72, 0x70, 0x71, 0x21, 0x23, 0x15, 0x2c, 0x2a,
    0x54, 0x21, 0x15, 0x16, 0x18, 0x15, 0x23, 0x15, 0x15, 0x15, 0x7c, 0x7d,
    0x73, 0x73, 0x79, 0x73, 0x2e, 0x2f, 0x30, 0x31, 0x3c, 0x3d, 0x41, 0x1a,
    0x41, 0x16, 0x26, 0x2d, 0x15, 0x16, 0x15, 0x15, 0x13, 0x71, 0x72, 0x71,
    0x03, 0x25, 0x49, 0x3f, 0x15, 0x19, 0x7a, 0x7b, 0x16, 0x29, 0x37, 0x00,
    0x4e, 0x36, 0x1f, 0x00, 0x28, 0x00, 0x4e, 0x00, 0x27, 0x4e, 0x20, 0x36,
    0x00, 0x28, 0x4e, 0x28, 0x20, 0x00, 0x35, 0x2a, 0x15, 0x15, 0x16, 0x1a,
    0x15, 0x01, 0x00, 0x35, 0x36, 0x4e, 0x36, 0x1f, 0x35, 0x28, 0x00, 0x1f,
    0x28, 0x36, 0x20, 0x4e, 0x35, 0x00, 0x27, 0x36, 0x4e, 0x36, 0x00, 0x1f,
    0x35, 0x14, 0x16, 0x15, 0x7a, 0x7b, 0x14, 0x26, 0x2d, 0x13, 0x70, 0x72,
    0x76, 0x70, 0x38, 0x39, 0x7e, 0x7f, 0x2e, 0x2f, 0x30, 0x31, 0x70, 0x72,
    0x15, 0x16, 0x10, 0x21, 0x24, 0x23, 0x15, 0x29, 0x36, 0x54, 0x23, 0x21,
    0x14, 0x53, 0x20, 0x00, 0x2d, 0x10, 0x7a, 0x7b, 0x71, 0x72, 0x71, 0x71,
    0x71, 0x72, 0x79, 0x71, 0x71, 0x72, 0x71, 0x1d, 0x42, 0x08, 0x0a, 0x15,
    0x2c, 0x2a, 0x2d, 0x16, 0x14, 0x53, 0x25, 0x13, 0x79, 0x71, 0x76, 0x71,
    0x2c, 0x1e, 0x7e, 0x7f, 0x4a, 0x06, 0x38, 0x39, 0x04, 0x00, 0x20, 0x36,
    0x27, 0x36, 0x1f, 0x36, 0x20, 0x00, 0x28, 0x1f, 0x00, 0x27, 0x00, 0x20,
    0x28, 0x00, 0x57, 0x5b, 0x2a, 0x54, 0x15, 0x16, 0x15, 0x15, 0x29, 0x36,
    0x4e, 0x00, 0x00, 0x20, 0x00, 0x27, 0x00, 0x20, 0x20, 0x00, 0x28, 0x00,
    0x57, 0x5b, 0x20, 0x00, 0x00, 0x00, 0x01, 0x20, 0x03, 0x2d, 0x15, 0x1b,
    0x7a, 0x7b, 0x10, 0x1b, 0x15, 0x14, 0x16, 0x10, 0x1b, 0x13, 0x70, 0x71,
    0x7c, 0x7d, 0x72, 0x70, 0x71, 0x70, 0x1d, 0x16, 0x15, 0x16, 0x16, 0x18,
    0x24, 0x24, 0x15, 0x23, 0x29, 0x25, 0x24, 0x2c, 0x2a, 0x00, 0x27, 0x25,
    0x23, 0x18, 0x7a, 0x7b, 0x3a, 0x3b, 0x41, 0x16, 0x16, 0x14, 0x53, 0x2a,
    0x25, 0x15, 0x16, 0x71, 0x71, 0x72, 0x71, 0x1d, 0x08, 0x4a, 0x46, 0x47,
    0x2c, 0x36, 0x4e, 0x2a, 0x1e, 0x18, 0x15, 0x13, 0x71, 0x72, 0x71, 0x79,
    0x71, 0x72, 0x71, 0x71, 0x71, 0x29, 0x28, 0x50, 0x28, 0x02, 0x20, 0x4e,
    0x28, 0x4e, 0x27, 0x20, 0x00, 0x28, 0x36, 0x28, 0x27, 0x58, 0x59, 0x5a,
    0x1f, 0x00, 0x54, 0x15, 0x16, 0x16, 0x15, 0x29, 0x50, 0x35, 0x00, 0x27,
    0x00, 0x28, 0x00, 0x28, 0x27, 0x00, 0x27, 0x58, 0x59, 0x5a, 0x27, 0x1f,
    0x00, 0x25, 0x10, 0x16, 0x1b, 0x1b, 0x10, 0x15, 0x7a, 0x7b, 0x41, 0x21,
    0x16, 0x0e, 0x17, 0x0f, 0x3a, 0x3b, 0x26, 0x0f, 0x7a, 0x7b, 0x2c, 0x0e,
    0x1b, 0x16, 0x1b, 0x10, 0x2e, 0x2f, 0x30, 0x31, 0x24, 0x24, 0x49, 0x24,
    0x4a, 0x5c, 0x24, 0x15, 0x29, 0x1f, 0x28, 0x50, 0x24, 0x23, 0x7e, 0x7f,
    0x3c, 0x3d, 0x42, 0x4a, 0x5c, 0x26, 0x36, 0x50, 0x02, 0x2d, 0x09, 0x3e,
    0x3f, 0x40, 0x4a, 0x73, 0x73, 0x73, 0x73, 0x48, 0x4a, 0x29, 0x05, 0x25,
    0x5c, 0x08, 0x26, 0x1e, 0x06, 0x4a, 0x2c, 0x1e, 0x08, 0x3e, 0x3f, 0x40,
    0x5c, 0x73, 0x77, 0x73, 0x79, 0x73, 0x73, 0x29, 0x27, 0x50, 0x20, 0x28,
    0x50, 0x20, 0x50, 0x27, 0x20, 0x50, 0x28, 0x50, 0x20, 0x50, 0x33, 0x54,
    0x4a, 0x2c, 0x2d, 0x16, 0x5c, 0x29, 0x50, 0x28, 0x50, 0x27, 0x50, 0x28,
    0x27, 0x01, 0x27, 0x50, 0x28, 0x50, 0x27, 0x28, 0x50, 0x16, 0x3f, 0x5c,
    0x2e, 0x2f, 0x30, 0x31, 0x7e, 0x7f, 0x42, 0x4a, 0x12, 0x12, 0x11, 0x12,
    0x3c, 0x3d, 0x12, 0x12, 0x7e, 0x7f, 0x3f, 0x5c, 0x49, 0x2e, 0x2f, 0x30,
    0x70, 0x71, 0x79, 0x71, 0x71, 0x70, 0x76, 0x70, 0x71, 0x70, 0x79, 0x70,
    0x72, 0x72, 0x70, 0x71, 0x70, 0x72, 0x71, 0x70, 0x72, 0x70, 0x70, 0x71,
    0x71, 0x71, 0x70, 0x72, 0x72, 0x70, 0x70, 0x71, 0x79, 0x70, 0x70, 0x70,
    0x72, 0x70, 0x71, 0x70, 0x70, 0x72, 0x72, 0x70, 0x71, 0x70, 0x70, 0x72,
    0x70, 0x79, 0x71, 0x71, 0x70, 0x71, 0x72, 0x71, 0x70, 0x70, 0x71, 0x71,
    0x79, 0x71, 0x70, 0x72, 0x70, 0x71, 0x70, 0x79, 0x70, 0x72, 0x70, 0x70,
    0x71, 0x70, 0x71, 0x71, 0x70, 0x72, 0x75, 0x72, 0x70, 0x79, 0x72, 0x71,
    0x71, 0x72, 0x70, 0x71, 0x71, 0x72, 0x70, 0x79, 0x70, 0x70, 0x72, 0x70,
    0x79, 0x79, 0x70, 0x72, 0x70, 0x72, 0x75, 0x72, 0x71, 0x71, 0x72, 0x70,
    0x79, 0x72, 0x75, 0x70, 0x71, 0x72, 0x70, 0x70, 0x72, 0x71, 0x72, 0x70,
    0x71, 0x70, 0x75, 0x72, 0x76, 0x71, 0x71, 0x70};

unsigned char fieldmap3[] = {
    0x4c, 0x4c, 0x4d, 0x4b, 0x4c, 0x4c, 0x4d, 0x00, 0x00, 0x4b, 0x4c, 0x4d,
    0x4b, 0x4c, 0x4c, 0x4c, 0x4d, 0x00, 0x4b, 0x4c, 0x4c, 0x4d, 0x4b, 0x4c,
    0x4c, 0x4c, 0x4d, 0x00, 0x36, 0x00, 0x00, 0x36, 0x00, 0x00, 0x4b, 0x4c,
    0x4c, 0x4d, 0x4b, 0x4c, 0x4c, 0x4d, 0x00, 0x00, 0x4b, 0x4c, 0x4c, 0x4d,
    0x4b, 0x4c, 0x4b, 0x4d, 0x4b, 0x4d, 0x36, 0x36, 0x00, 0x00, 0x00, 0x00,
    0x4b, 0x4c, 0x4c, 0x4d, 0x00, 0x00, 0x00, 0x36, 0x00, 0x4b, 0x4c, 0x4c,
    0x4d, 0x4b, 0x4c, 0x4c, 0x4d, 0x00, 0x00, 0x36, 0x00, 0x4b, 0x4c, 0x4c,
    0x4d, 0x4b, 0x4c, 0x4c, 0x4d, 0x36, 0x00, 0x4b, 0x4c, 0x4c, 0x4c, 0x4d,
    0x00, 0x36, 0x00, 0x4b, 0x4c, 0x4c, 0x4d, 0x36, 0x36, 0x00, 0x4b, 0x4d,
    0x36, 0x1f, 0x00, 0x4b, 0x4c, 0x4d, 0x4b, 0x4d, 0x00, 0x36, 0x4b, 0x4c,
    0x4c, 0x4d, 0x4b, 0x4c, 0x4c, 0x4d, 0x4b, 0x4c, 0x35, 0x4b, 0x4c, 0x4d,
    0x00, 0x4b, 0x4c, 0x4d, 0x36, 0x00, 0x4b, 0x4d, 0x36, 0x35, 0x4b, 0x4c,
    0x4c, 0x4d, 0x36, 0x4b, 0x4c, 0x4c, 0x4c, 0x4c, 0x4d, 0x36, 0x00, 0x4b,
    0x4c, 0x4d, 0x36, 0x4b, 0x4c, 0x4c, 0x4c, 0x4d, 0x35, 0x36, 0x00, 0x4b,
    0x4d, 0x36, 0x00, 0x4b, 0x4c, 0x4d, 0x00, 0x35, 0x4b, 0x4d, 0x00, 0x36,
    0x4b, 0x4c, 0x4c, 0x4c, 0x4c, 0x4d, 0x36, 0x35, 0x00, 0x00, 0x35, 0x00,
    0x36, 0x35, 0x00, 0x4b, 0x4c, 0x4c, 0x4c, 0x4d, 0x36, 0x4b, 0x4d, 0x36,
    0x4b, 0x4c, 0x4c, 0x4d, 0x00, 0x35, 0x00, 0x35, 0x00, 0x36, 0x4b, 0x4c,
    0x4c, 0x4d, 0x36, 0x36, 0x4b, 0x4c, 0x4d, 0x00, 0x35, 0x4b, 0x4c, 0x4c,
    0x4c, 0x4d, 0x35, 0x35, 0x4b, 0x4d, 0x36, 0x1f, 0x4b, 0x52, 0x4c, 0x4d,
    0x35, 0x35, 0x4b, 0x4c, 0x4c, 0x4c, 0x4d, 0x36, 0x35, 0x1f, 0x4b, 0x4c,
    0x4c, 0x4d, 0x35, 0x1f, 0x36, 0x35, 0x4b, 0x4d, 0x36, 0x00, 0x4b, 0x4c,
    0x4d, 0x35, 0x04, 0x1f, 0x03, 0x01, 0x01, 0x4f, 0x1f, 0x4b, 0x4d, 0x35,
    0x36, 0x4b, 0x4c, 0x4d, 0x35, 0x36, 0x1f, 0x00, 0x4b, 0x4d, 0x35, 0x00,
    0x35, 0x00, 0x4b, 0x4c, 0x4c, 0x4d, 0x35, 0x36, 0x4b, 0x4c, 0x4c, 0x4c,
    0x4d, 0x36, 0x00, 0x4b, 0x4d, 0x00, 0x00, 0x35, 0x35, 0x00, 0x4b, 0x4d,
    0x35, 0x00, 0x00, 0x35, 0x00, 0x4b, 0x4d, 0x00, 0x4f, 0x50, 0x01, 0x35,
    0x50, 0x35, 0x36, 0x4b, 0x4c, 0x4d, 0x36, 0x00, 0x35, 0x4b, 0x4c, 0x4d,
    0x00, 0x36, 0x36, 0x4b, 0x4c, 0x4c, 0x4d, 0x36, 0x35, 0x4b, 0x4d, 0x00,
    0x35, 0x35, 0x4b, 0x4c, 0x4c, 0x4d, 0x35, 0x4b, 0x4c, 0x4c, 0x4d, 0x1f,
    0x35, 0x00, 0x01, 0x20, 0x4f, 0x27, 0x33, 0x35, 0x37, 0x4b, 0x4d, 0x1f,
    0x35, 0x4b, 0x4c, 0x4c, 0x4d, 0x20, 0x1f, 0x4b, 0x4d, 0x1f, 0x4f, 0x20,
    0x4b, 0x4c, 0x4c, 0x4c, 0x4d, 0x35, 0x36, 0x03, 0x01, 0x7c, 0x7d, 0x71,
    0x71, 0x70, 0x71, 0x78, 0x70, 0x29, 0x4b, 0x4c, 0x4c, 0x4d, 0x00, 0x4b,
    0x4d, 0x35, 0x20, 0x36, 0x00, 0x35, 0x36, 0x4b, 0x4d, 0x1f, 0x37, 0x36,
    0x4b, 0x4c, 0x4c, 0x4d, 0x00, 0x00, 0x35, 0x4b, 0x4d, 0x00, 0x35, 0x36,
    0x00, 0x4b, 0x4c, 0x4c, 0x4d, 0x35, 0x00, 0x36, 0x4b, 0x4c, 0x4c, 0x4d,
    0x01, 0x33, 0x50, 0x71, 0x78, 0x70, 0x71, 0x71, 0x71, 0x70, 0x55, 0x32,
    0x33, 0x34, 0x4b, 0x4c, 0x4c, 0x4d, 0x00, 0x4b, 0x4c, 0x4c, 0x4d, 0x35,
    0x00, 0x00, 0x36, 0x00, 0x4b, 0x4c, 0x4c, 0x4d, 0x00, 0x36, 0x35, 0x4b,
    0x4d, 0x00, 0x35, 0x00, 0x1f, 0x00, 0x01, 0x20, 0x50, 0x70, 0x71, 0x71,
    0x78, 0x79, 0x75, 0x70, 0x38, 0x39, 0x01, 0x20, 0x35, 0x37, 0x35, 0x1f,
    0x35, 0x27, 0x20, 0x50, 0x50, 0x70, 0x78, 0x71, 0x37, 0x4b, 0x4d, 0x35,
    0x32, 0x33, 0x34, 0x71, 0x79, 0x7a, 0x7b, 0x1b, 0x16, 0x15, 0x16, 0x53,
    0x2a, 0x4b, 0x4d, 0x35, 0x00, 0x4b, 0x4d, 0x04, 0x1f, 0x33, 0x20, 0x4f,
    0x01, 0x4b, 0x4d, 0x36, 0x36, 0x20, 0x38, 0x39, 0x4b, 0x4d, 0x00, 0x01,
    0x03, 0x04, 0x4b, 0x4d, 0x04, 0x01, 0x4f, 0x4b, 0x4d, 0x01, 0x33, 0x4b,
    0x4d, 0x02, 0x50, 0x01, 0x35, 0x50, 0x25, 0x71, 0x70, 0x75, 0x71, 0x71,
    0x1d, 0x15, 0x53, 0x54, 0x15, 0x13, 0x70, 0x70, 0x70, 0x77, 0x4f, 0x01,
    0x00, 0x4b, 0x4d, 0x00, 0x35, 0x4b, 0x4d, 0x36, 0x4b, 0x4d, 0x35, 0x36,
    0x00, 0x4b, 0x4d, 0x00, 0x36, 0x35, 0x57, 0x5b, 0x35, 0x00, 0x1f, 0x50,
    0x20, 0x56, 0x71, 0x7c, 0x7d, 0x1d, 0x15, 0x16, 0x15, 0x15, 0x13, 0x70,
    0x79, 0x79, 0x71, 0x70, 0x77, 0x38, 0x39, 0x20, 0x4f, 0x28, 0x71, 0x7c,
    0x7d, 0x71, 0x15, 0x15, 0x38, 0x39, 0x33, 0x7c, 0x7d, 0x71, 0x71, 0x1d,
    0x21, 0x7a, 0x7b, 0x53, 0x2a, 0x2a, 0x2a, 0x35, 0x4b, 0x4d, 0x36, 0x35,
    0x03, 0x01, 0x25, 0x71, 0x71, 0x75, 0x70, 0x78, 0x70, 0x29, 0x36, 0x25,
    0x71, 0x7c, 0x7d, 0x70, 0x29, 0x35, 0x25, 0x70, 0x71, 0x71, 0x29, 0x25,
    0x71, 0x70, 0x78, 0x29, 0x25, 0x71, 0x75, 0x29, 0x25, 0x70, 0x71, 0x71,
    0x70, 0x79, 0x70, 0x71, 0x4a, 0x15, 0x49, 0x3a, 0x3b, 0x29, 0x37, 0x35,
    0x54, 0x15, 0x1a, 0x13, 0x70, 0x71, 0x78, 0x70, 0x55, 0x01, 0x33, 0x35,
    0x35, 0x00, 0x36, 0x4b, 0x4d, 0x00, 0x36, 0x00, 0x35, 0x00, 0x1f, 0x35,
    0x36, 0x58, 0x59, 0x5a, 0x1f, 0x33, 0x20, 0x70, 0x71, 0x71, 0x70, 0x7a,
    0x7b, 0x23, 0x15, 0x1a, 0x16, 0x15, 0x23, 0x15, 0x16, 0x16, 0x1b, 0x13,
    0x71, 0x7c, 0x7d, 0x79, 0x78, 0x71, 0x1d, 0x7a, 0x7b, 0x16, 0x15, 0x1a,
    0x70, 0x72, 0x75, 0x7a, 0x7b, 0x1a, 0x16, 0x15, 0x54, 0x7a, 0x7b, 0x29,
    0x35, 0x36, 0x35, 0x35, 0x35, 0x04, 0x01, 0x25, 0x70, 0x71, 0x70, 0x71,
    0x70, 0x1d, 0x16, 0x15, 0x53, 0x36, 0x35, 0x36, 0x2d, 0x7a, 0x7b, 0x16,
    0x29, 0x25, 0x15, 0x16, 0x1a, 0x15, 0x53, 0x36, 0x54, 0x16, 0x16, 0x29,
    0x56, 0x15, 0x15, 0x29, 0x35, 0x54, 0x15, 0x16, 0x1a, 0x15, 0x16, 0x13,
    0x71, 0x71, 0x76, 0x3c, 0x3d, 0x4f, 0x38, 0x39, 0x01, 0x54, 0x15, 0x15,
    0x16, 0x1b, 0x13, 0x71, 0x79, 0x70, 0x75, 0x55, 0x50, 0x37, 0x36, 0x36,
    0x35, 0x36, 0x35, 0x00, 0x00, 0x35, 0x20, 0x00, 0x00, 0x1f, 0x27, 0x56,
    0x70, 0x75, 0x71, 0x70, 0x1d, 0x15, 0x16, 0x7a, 0x7b, 0x24, 0x3a, 0x3b,
    0x15, 0x23, 0x24, 0x49, 0x1a, 0x69, 0x6a, 0x6b, 0x23, 0x7a, 0x7b, 0x15,
    0x15, 0x15, 0x15, 0x7a, 0x7b, 0x15, 0x15, 0x15, 0x15, 0x16, 0x1a, 0x7a,
    0x7b, 0x1b, 0x53, 0x2a, 0x25, 0x7a, 0x7b, 0x1f, 0x36, 0x35, 0x03, 0x32,
    0x33, 0x77, 0x72, 0x71, 0x79, 0x70, 0x1d, 0x16, 0x21, 0x1b, 0x15, 0x53,
    0x00, 0x00, 0x35, 0x25, 0x16, 0x7a, 0x7b, 0x2c, 0x35, 0x36, 0x1e, 0x4a,
    0x49, 0x1c, 0x15, 0x29, 0x25, 0x15, 0x15, 0x15, 0x15, 0x16, 0x2c, 0x00,
    0x36, 0x00, 0x2a, 0x2a, 0x2a, 0x54, 0x15, 0x16, 0x1a, 0x15, 0x13, 0x70,
    0x71, 0x7c, 0x7d, 0x71, 0x70, 0x29, 0x2d, 0x16, 0x15, 0x53, 0x2a, 0x54,
    0x13, 0x71, 0x79, 0x71, 0x70, 0x38, 0x39, 0x01, 0x00, 0x00, 0x36, 0x36,
    0x00, 0x36, 0x27, 0x1f, 0x35, 0x71, 0x70, 0x79, 0x70, 0x1d, 0x16, 0x15,
    0x1b, 0x16, 0x1a, 0x7a, 0x7b, 0x22, 0x3c, 0x3d, 0x51, 0x70, 0x70, 0x76,
    0x6c, 0x6d, 0x6e, 0x6f, 0x24, 0x7a, 0x7b, 0x15, 0x16, 0x1a, 0x15, 0x7a,
    0x7b, 0x15, 0x16, 0x16, 0x1a, 0x21, 0x16, 0x7a, 0x7b, 0x53, 0x36, 0x35,
    0x1f, 0x7e, 0x7f, 0x20, 0x01, 0x37, 0x25, 0x70, 0x71, 0x71, 0x70, 0x7c,
    0x7d, 0x1d, 0x16, 0x15, 0x16, 0x53, 0x2a, 0x00, 0x36, 0x35, 0x00, 0x36,
    0x1e, 0x7a, 0x7b, 0x16, 0x29, 0x25, 0x70, 0x70, 0x76, 0x70, 0x70, 0x29,
    0x36, 0x54, 0x16, 0x1a, 0x15, 0x15, 0x15, 0x29, 0x00, 0x35, 0x00, 0x35,
    0x00, 0x00, 0x2a, 0x54, 0x15, 0x16, 0x15, 0x1b, 0x15, 0x7a, 0x7b, 0x15,
    0x2c, 0x35, 0x15, 0x41, 0x53, 0x36, 0x36, 0x35, 0x2a, 0x54, 0x13, 0x7c,
    0x7d, 0x70, 0x70, 0x77, 0x55, 0x01, 0x01, 0x00, 0x35, 0x00, 0x28, 0x20,
    0x56, 0x4a, 0x49, 0x3e, 0x51, 0x40, 0x51, 0x16, 0x69, 0x6a, 0x6b, 0x7e,
    0x7f, 0x79, 0x71, 0x70, 0x75, 0x1d, 0x15, 0x13, 0x70, 0x79, 0x71, 0x71,
    0x71, 0x7e, 0x7f, 0x15, 0x23, 0x15, 0x16, 0x7a, 0x7b, 0x1a, 0x15, 0x15,
    0x16, 0x1a, 0x15, 0x7a, 0x7b, 0x01, 0x35, 0x04, 0x71, 0x71, 0x72, 0x71,
    0x71, 0x38, 0x39, 0x4a, 0x23, 0x16, 0x49, 0x7e, 0x7f, 0x2f, 0x30, 0x31,
    0x53, 0x36, 0x00, 0x35, 0x00, 0x00, 0x36, 0x25, 0x3f, 0x7e, 0x7f, 0x4a,
    0x29, 0x35, 0x54, 0x1b, 0x1a, 0x15, 0x2c, 0x37, 0x35, 0x00, 0x54, 0x1b,
    0x16, 0x1a, 0x53, 0x00, 0x36, 0x00, 0x36, 0x00, 0x35, 0x00, 0x00, 0x00,
    0x54, 0x15, 0x16, 0x15, 0x1a, 0x7a, 0x7b, 0x16, 0x15, 0x55, 0x2d, 0x42,
    0x55, 0x36, 0x35, 0x36, 0x00, 0x35, 0x2d, 0x7a, 0x7b, 0x16, 0x13, 0x70,
    0x70, 0x70, 0x77, 0x55, 0x33, 0x56, 0x70, 0x79, 0x70, 0x70, 0x76, 0x70,
    0x70, 0x70, 0x75, 0x6c, 0x6d, 0x6e, 0x6f, 0x70, 0x71, 0x70, 0x1d, 0x1b,
    0x15, 0x15, 0x16, 0x15, 0x15, 0x15, 0x16, 0x15, 0x13, 0x70, 0x71, 0x49,
    0x24, 0x4a, 0x1b, 0x7a, 0x7b, 0x15, 0x16, 0x41, 0x41, 0x15, 0x49, 0x7e,
    0x7f, 0x71, 0x71, 0x77, 0x46, 0x47, 0x13, 0x70, 0x71, 0x71, 0x77, 0x72,
    0x71, 0x71, 0x76, 0x71, 0x71, 0x70, 0x71, 0x70, 0x29, 0x35, 0x00, 0x00,
    0x00, 0x35, 0x25, 0x70, 0x75, 0x79, 0x7c, 0x7d, 0x71, 0x29, 0x36, 0x54,
    0x1c, 0x5c, 0x15, 0x38, 0x39, 0x33, 0x54, 0x49, 0x4a, 0x53, 0x01, 0x00,
    0x00, 0x00, 0x35, 0x00, 0x00, 0x00, 0x35, 0x36, 0x00, 0x54, 0x15, 0x15,
    0x16, 0x7a, 0x7b, 0x15, 0x13, 0x72, 0x70, 0x75, 0x71, 0x29, 0x00, 0x35,
    0x36, 0x25, 0x15, 0x7a, 0x7b, 0x43, 0x44, 0x49, 0x4a, 0x70, 0x71, 0x70,
    0x75, 0x1d, 0x53, 0x36, 0x54, 0x16, 0x16, 0x3a, 0x3b, 0x23, 0x13, 0x70,
    0x71, 0x71, 0x70, 0x15, 0x4a, 0x2e, 0x2f, 0x30, 0x31, 0x0c, 0x0d, 0x15,
    0x1a, 0x16, 0x1a, 0x15, 0x15, 0x15, 0x13, 0x76, 0x72, 0x71, 0x51, 0x7e,
    0x7f, 0x3f, 0x5c, 0x42, 0x7c, 0x7d, 0x76, 0x71, 0x79, 0x71, 0x70, 0x71,
    0x68, 0x48, 0x4a, 0x49, 0x13, 0x71, 0x70, 0x71, 0x1d, 0x16, 0x10, 0x21,
    0x16, 0x15, 0x15, 0x53, 0x00, 0x36, 0x00, 0x36, 0x00, 0x00, 0x36, 0x2a,
    0x2d, 0x21, 0x7a, 0x7b, 0x26, 0x36, 0x35, 0x25, 0x71, 0x71, 0x70, 0x71,
    0x70, 0x75, 0x71, 0x76, 0x71, 0x71, 0x70, 0x29, 0x50, 0x35, 0x50, 0x02,
    0x35, 0x00, 0x00, 0x00, 0x35, 0x36, 0x2a, 0x54, 0x15, 0x7a, 0x7b, 0x16,
    0x15, 0x15, 0x54, 0x16, 0x53, 0x35, 0x00, 0x36, 0x50, 0x01, 0x2d, 0x7a,
    0x7b, 0x45, 0x70, 0x76, 0x70, 0x53, 0x2a, 0x2d, 0x15, 0x2c, 0x50, 0x1f,
    0x36, 0x50, 0x54, 0x3c, 0x3d, 0x24, 0x49, 0x4a, 0x15, 0x16, 0x15, 0x13,
    0x70, 0x79, 0x70, 0x7c, 0x7d, 0x71, 0x70, 0x3e, 0x3f, 0x40, 0x15, 0x1b,
    0x16, 0x15, 0x15, 0x69, 0x6a, 0x6b, 0x75, 0x7c, 0x7d, 0x75, 0x79, 0x75,
    0x7a, 0x7b, 0x15, 0x1a, 0x1b, 0x16, 0x5d, 0x5e, 0x67, 0x13, 0x71, 0x76,
    0x71, 0x1d, 0x16, 0x14, 0x5d, 0x10, 0x1a, 0x15, 0x1b, 0x53, 0x2a, 0x36,
    0x00, 0x57, 0x5b, 0x00, 0x00, 0x1f, 0x25, 0x2e, 0x30, 0x31, 0x7e, 0x7f,
    0x3f, 0x29, 0x1f, 0x00, 0x2a, 0x54, 0x16, 0x16, 0x1a, 0x15, 0x16, 0x1b,
    0x68, 0x67, 0x13, 0x70, 0x71, 0x79, 0x71, 0x71, 0x29, 0x33, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x15, 0x7a, 0x7b, 0x1a, 0x1b, 0x1b, 0x29, 0x2a,
    0x37, 0x00, 0x36, 0x25, 0x71, 0x71, 0x71, 0x7c, 0x7d, 0x71, 0x1d, 0x1a,
    0x53, 0x36, 0x36, 0x15, 0x1a, 0x16, 0x53, 0x20, 0x25, 0x70, 0x79, 0x71,
    0x71, 0x70, 0x76, 0x70, 0x1d, 0x53, 0x54, 0x15, 0x16, 0x15, 0x23, 0x7a,
    0x7b, 0x15, 0x13, 0x79, 0x71, 0x71, 0x30, 0x31, 0x15, 0x15, 0x6c, 0x6d,
    0x6e, 0x6f, 0x70, 0x79, 0x70, 0x1d, 0x15, 0x15, 0x7a, 0x7b, 0x41, 0x43,
    0x44, 0x60, 0x61, 0x62, 0x16, 0x69, 0x6a, 0x6b, 0x3a, 0x3b, 0x5f, 0x60,
    0x61, 0x62, 0x16, 0x14, 0x53, 0x00, 0x00, 0x00, 0x58, 0x59, 0x5a, 0x00,
    0x25, 0x71, 0x70, 0x71, 0x71, 0x70, 0x71, 0x77, 0x75, 0x71, 0x71, 0x29,
    0x36, 0x00, 0x54, 0x15, 0x16, 0x15, 0x60, 0x61, 0x62, 0x15, 0x16, 0x1b,
    0x16, 0x1a, 0x69, 0x6a, 0x70, 0x75, 0x29, 0x01, 0x50, 0x01, 0x00, 0x00,
    0x1e, 0x7a, 0x7b, 0x16, 0x3a, 0x3b, 0x1b, 0x55, 0x38, 0x39, 0x00, 0x36,
    0x54, 0x1a, 0x16, 0x7a, 0x7b, 0x1b, 0x53, 0x37, 0x36, 0x00, 0x56, 0x1a,
    0x1b, 0x53, 0x36, 0x28, 0x1f, 0x2a, 0x2d, 0x16, 0x41, 0x1b, 0x1a, 0x1b,
    0x2c, 0x37, 0x00, 0x54, 0x15, 0x23, 0x24, 0x7a, 0x7b, 0x1b, 0x15, 0x16,
    0x1a, 0x13, 0x71, 0x70, 0x2e, 0x2f, 0x70, 0x71, 0x71, 0x70, 0x3a, 0x3b,
    0x16, 0x15, 0x16, 0x15, 0x7e, 0x7f, 0x42, 0x45, 0x63, 0x64, 0x65, 0x66,
    0x4a, 0x6d, 0x6e, 0x6f, 0x3c, 0x3d, 0x63, 0x64, 0x65, 0x66, 0x5c, 0x4a,
    0x29, 0x01, 0x32, 0x33, 0x34, 0x20, 0x01, 0x25, 0x15, 0x4a, 0x2e, 0x2f,
    0x49, 0x42, 0x2f, 0x49, 0x3f, 0x30, 0x31, 0x5c, 0x29, 0x33, 0x02, 0x2a,
    0x54, 0x63, 0x64, 0x65, 0x66, 0x2e, 0x2f, 0x30, 0x31, 0x6c, 0x6d, 0x6e,
    0x6f, 0x4a, 0x70, 0x71, 0x77, 0x71, 0x29, 0x56, 0x5c, 0x7e, 0x7f, 0x2f,
    0x3c, 0x3d, 0x2f, 0x30, 0x31, 0x4a, 0x55, 0x50, 0x01, 0x2a, 0x2d, 0x7e,
    0x7f, 0x2c, 0x36, 0x38, 0x39, 0x01, 0x2a, 0x54, 0x51, 0x5c, 0x55, 0x27,
    0x20, 0x56, 0x3e, 0x51, 0x42, 0x2e, 0x2f, 0x30, 0x31, 0x38, 0x39, 0x50,
    0x2d, 0x24, 0x22, 0x7e, 0x7f, 0x2e, 0x2f, 0x51, 0x5c, 0x42, 0x3f, 0x4a,
    0x77, 0x71, 0x23, 0x0b, 0x0c, 0x0d, 0x3c, 0x3d, 0x2e, 0x2f, 0x30, 0x31,
    0x70, 0x71, 0x72, 0x71, 0x70, 0x71, 0x71, 0x71, 0x70, 0x71, 0x72, 0x71,
    0x71, 0x70, 0x79, 0x71, 0x70, 0x72, 0x71, 0x70, 0x79, 0x71, 0x71, 0x70,
    0x70, 0x72, 0x71, 0x70, 0x70, 0x71, 0x70, 0x72, 0x78, 0x75, 0x79, 0x76,
    0x71, 0x70, 0x79, 0x71, 0x72, 0x75, 0x72, 0x70, 0x71, 0x71, 0x71, 0x70,
    0x70, 0x71, 0x70, 0x72, 0x71, 0x70, 0x71, 0x70, 0x71, 0x79, 0x70, 0x70,
    0x71, 0x71, 0x72, 0x71, 0x71, 0x70, 0x70, 0x70, 0x70, 0x70, 0x71, 0x70,
    0x72, 0x70, 0x79, 0x71, 0x70, 0x71, 0x70, 0x71, 0x70, 0x70, 0x71, 0x79,
    0x70, 0x71, 0x71, 0x71, 0x75, 0x70, 0x70, 0x79, 0x70, 0x71, 0x71, 0x72,
    0x75, 0x72, 0x70, 0x79, 0x72, 0x70, 0x71, 0x70, 0x71, 0x70, 0x71, 0x72,
    0x70, 0x70, 0x71, 0x72, 0x71, 0x75, 0x71, 0x79, 0x71, 0x72, 0x70, 0x71,
    0x71, 0x70, 0x72, 0x70, 0x70, 0x79, 0x79, 0x70};

unsigned char seigemap[] = {
    0x00, 0x00, 0x00, 0x4c, 0x00, 0x48, 0x49, 0x49, 0x4a, 0x00, 0x4c, 0x00,
    0x00, 0x4c, 0x00, 0x4b, 0x00, 0x00, 0x4c, 0x4b, 0x00, 0x4b, 0x00, 0x48,
    0x4a, 0x00, 0x4b, 0x00, 0x4b, 0x4b, 0x4c, 0x4b, 0x00, 0x4b, 0x00, 0x4b,
    0x00, 0x4b, 0x48, 0x49, 0x49, 0x4a, 0x4c, 0x4b, 0x00, 0x4b, 0x4c, 0x48,
    0x4a, 0x4b, 0x2c, 0x32, 0x33, 0x48, 0x49, 0x4a, 0x4b, 0x2c, 0x32, 0x33,
    0x49, 0x4a, 0x4c, 0x4b, 0x21, 0x28, 0x27, 0x26, 0x27, 0x26, 0x27, 0x26,
    0x27, 0x26, 0x27, 0x26, 0x27, 0x26, 0x27, 0x26, 0x27, 0x26, 0x27, 0x26,
    0x27, 0x2a, 0x3f, 0x48, 0x4a, 0x4b, 0x4c, 0x00, 0x4b, 0x00, 0x48, 0x49,
    0x4a, 0x4b, 0x00, 0x4b, 0x4c, 0x00, 0x00, 0x4b, 0x48, 0x49, 0x32, 0x33,
    0x27, 0x26, 0x27, 0x26, 0x27, 0x26, 0x27, 0x26, 0x27, 0x26, 0x27, 0x26,
    0x27, 0x26, 0x27, 0x26, 0x27, 0x26, 0x27, 0x26, 0x00, 0x48, 0x4a, 0x00,
    0x4c, 0x00, 0x00, 0x00, 0x4b, 0x00, 0x00, 0x48, 0x49, 0x49, 0x4a, 0x4c,
    0x00, 0x00, 0x00, 0x48, 0x49, 0x4a, 0x4b, 0x00, 0x4b, 0x00, 0x4b, 0x48,
    0x49, 0x49, 0x4a, 0x00, 0x4b, 0x48, 0x49, 0x4a, 0x00, 0x4b, 0x00, 0x4b,
    0x4b, 0x00, 0x4b, 0x00, 0x48, 0x49, 0x4a, 0x4b, 0x00, 0x4b, 0x21, 0x1f,
    0x20, 0x22, 0x4b, 0x48, 0x4a, 0x21, 0x1f, 0x20, 0x22, 0x48, 0x4a, 0x21,
    0x4b, 0x29, 0x25, 0x3a, 0x16, 0x6b, 0x25, 0x1b, 0x25, 0x25, 0x16, 0x6b,
    0x6c, 0x3a, 0x6d, 0x6e, 0x16, 0x1b, 0x6d, 0x6e, 0x6c, 0x2b, 0x3c, 0x3d,
    0x3f, 0x48, 0x4a, 0x28, 0x27, 0x26, 0x27, 0x2a, 0x22, 0x4b, 0x4c, 0x48,
    0x49, 0x4a, 0x4b, 0x00, 0x4b, 0x2c, 0x36, 0x37, 0x25, 0x6b, 0x6c, 0x13,
    0x16, 0x25, 0x6b, 0x14, 0x25, 0x13, 0x6c, 0x6b, 0x16, 0x1b, 0x6c, 0x13,
    0x44, 0x45, 0x6b, 0x25, 0x4c, 0x4b, 0x00, 0x00, 0x00, 0x4b, 0x00, 0x4c,
    0x00, 0x4c, 0x00, 0x4b, 0x00, 0x4c, 0x00, 0x00, 0x00, 0x4b, 0x00, 0x4c,
    0x00, 0x4b, 0x00, 0x4b, 0x00, 0x48, 0x4a, 0x4c, 0x00, 0x4b, 0x4c, 0x00,
    0x4b, 0x4c, 0x4b, 0x00, 0x4b, 0x48, 0x4a, 0x00, 0x4b, 0x00, 0x4b, 0x00,
    0x4b, 0x00, 0x00, 0x4b, 0x4b, 0x21, 0x4c, 0x1f, 0x20, 0x4b, 0x22, 0x4b,
    0x21, 0x4b, 0x1f, 0x20, 0x4b, 0x22, 0x7c, 0x7d, 0x7d, 0x73, 0x73, 0x73,
    0x73, 0x71, 0x25, 0x25, 0x25, 0x73, 0x73, 0x73, 0x71, 0x73, 0x7e, 0x7f,
    0x71, 0x73, 0x7e, 0x7f, 0x34, 0x35, 0x3f, 0x3e, 0x40, 0x3d, 0x3c, 0x29,
    0x16, 0x6d, 0x6e, 0x2b, 0x00, 0x22, 0x00, 0x4b, 0x00, 0x48, 0x49, 0x4a,
    0x4b, 0x40, 0x38, 0x39, 0x6b, 0x6d, 0x6e, 0x25, 0x16, 0x6b, 0x3a, 0x6c,
    0x1b, 0x25, 0x6b, 0x3a, 0x16, 0x6c, 0x6b, 0x25, 0x46, 0x47, 0x25, 0x6b,
    0x00, 0x00, 0x48, 0x49, 0x4a, 0x00, 0x00, 0x48, 0x4a, 0x00, 0x28, 0x27,
    0x26, 0x27, 0x26, 0x27, 0x2a, 0x22, 0x00, 0x48, 0x49, 0x49, 0x4a, 0x00,
    0x4b, 0x00, 0x4b, 0x48, 0x49, 0x4a, 0x00, 0x4b, 0x00, 0x4b, 0x00, 0x48,
    0x49, 0x4a, 0x00, 0x28, 0x27, 0x26, 0x27, 0x26, 0x27, 0x2a, 0x22, 0x48,
    0x4a, 0x7c, 0x7d, 0x73, 0x73, 0x7d, 0x70, 0x7a, 0x70, 0x7d, 0x73, 0x73,
    0x7d, 0x7b, 0x48, 0x4a, 0x21, 0x32, 0x2e, 0x24, 0x24, 0x24, 0x25, 0x25,
    0x25, 0x24, 0x24, 0x24, 0x24, 0x24, 0x7e, 0x7f, 0x24, 0x24, 0x7e, 0x7f,
    0x32, 0x33, 0x22, 0x2d, 0x4b, 0x3e, 0x40, 0x35, 0x71, 0x7e, 0x7f, 0x73,
    0x74, 0x70, 0x7d, 0x7b, 0x4b, 0x4c, 0x00, 0x4b, 0x00, 0x4c, 0x34, 0x35,
    0x35, 0x7e, 0x7f, 0x73, 0x73, 0x73, 0x73, 0x73, 0x71, 0x73, 0x73, 0x73,
    0x73, 0x73, 0x71, 0x73, 0x73, 0x73, 0x71, 0x71, 0x4a, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x4b, 0x00, 0x00, 0x21, 0x29, 0x25, 0x25, 0x6d, 0x6e, 0x25,
    0x2b, 0x00, 0x22, 0x00, 0x4b, 0x00, 0x48, 0x49, 0x4a, 0x00, 0x00, 0x00,
    0x00, 0x48, 0x49, 0x4a, 0x00, 0x4c, 0x00, 0x00, 0x00, 0x00, 0x2c, 0x29,
    0x1a, 0x1b, 0x6d, 0x6e, 0x19, 0x2b, 0x4b, 0x22, 0x4b, 0x00, 0x4b, 0x34,
    0x35, 0x4a, 0x6f, 0x40, 0x3d, 0x3f, 0x34, 0x35, 0x00, 0x4b, 0x4b, 0x43,
    0x4c, 0x44, 0x45, 0x6a, 0x25, 0x13, 0x6c, 0x6a, 0x19, 0x13, 0x6c, 0x14,
    0x6b, 0x6c, 0x7e, 0x7f, 0x6a, 0x6a, 0x7e, 0x7f, 0x1d, 0x20, 0x4b, 0x22,
    0x48, 0x49, 0x4a, 0x1f, 0x1e, 0x7e, 0x7f, 0x24, 0x00, 0x6f, 0x48, 0x49,
    0x49, 0x49, 0x49, 0x4a, 0x00, 0x2c, 0x36, 0x37, 0x2f, 0x7e, 0x7f, 0x6a,
    0x6a, 0x6b, 0x13, 0x25, 0x16, 0x25, 0x6a, 0x6c, 0x6b, 0x4f, 0x50, 0x51,
    0x25, 0x6a, 0x25, 0x6b, 0x4c, 0x00, 0x4b, 0x00, 0x48, 0x49, 0x49, 0x4a,
    0x21, 0x00, 0x73, 0x73, 0x71, 0x7e, 0x7f, 0x73, 0x73, 0x74, 0x7d, 0x70,
    0x7d, 0x7d, 0x70, 0x7b, 0x00, 0x00, 0x4b, 0x00, 0x00, 0x00, 0x00, 0x4c,
    0x00, 0x00, 0x4b, 0x00, 0x00, 0x00, 0x40, 0x34, 0x73, 0x73, 0x7e, 0x7f,
    0x73, 0x73, 0x7d, 0x7d, 0x7d, 0x7b, 0x48, 0x36, 0x37, 0x40, 0x3d, 0x3f,
    0x3e, 0x4b, 0x36, 0x37, 0x4a, 0x00, 0x21, 0x4b, 0x00, 0x46, 0x47, 0x6b,
    0x3a, 0x6d, 0x6e, 0x6b, 0x16, 0x1b, 0x6c, 0x6b, 0x6c, 0x6b, 0x7e, 0x7f,
    0x1b, 0x16, 0x7e, 0x7f, 0x1d, 0x20, 0x00, 0x4b, 0x22, 0x00, 0x00, 0x1f,
    0x1e, 0x7e, 0x7f, 0x25, 0x3f, 0x6f, 0x00, 0x4b, 0x00, 0x00, 0x00, 0x00,
    0x4b, 0x40, 0x38, 0x39, 0x2f, 0x7e, 0x7f, 0x6b, 0x6c, 0x3a, 0x25, 0x6c,
    0x16, 0x6b, 0x25, 0x25, 0x3a, 0x52, 0x53, 0x54, 0x1b, 0x6d, 0x6e, 0x6c,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x21, 0x00, 0x40, 0x36, 0x37,
    0x2e, 0x7e, 0x7f, 0x36, 0x37, 0x3f, 0x4b, 0x6f, 0x48, 0x4a, 0x3d, 0x00,
    0x4c, 0x00, 0x00, 0x2d, 0x00, 0x4b, 0x00, 0x2d, 0x2d, 0x00, 0x00, 0x00,
    0x40, 0x3d, 0x3c, 0x36, 0x37, 0x2e, 0x7e, 0x7f, 0x36, 0x37, 0x3c, 0x3d,
    0x3f, 0x2d, 0x00, 0x38, 0x39, 0x2d, 0x3e, 0x2d, 0x00, 0x2d, 0x38, 0x39,
    0x2d, 0x7c, 0x7d, 0x7d, 0x74, 0x73, 0x73, 0x73, 0x71, 0x7e, 0x7f, 0x73,
    0x16, 0x25, 0x6c, 0x6b, 0x73, 0x71, 0x73, 0x71, 0x73, 0x73, 0x7e, 0x7f,
    0x71, 0x73, 0x74, 0x7d, 0x74, 0x7a, 0x7d, 0x73, 0x73, 0x7e, 0x7f, 0x73,
    0x7d, 0x74, 0x7d, 0x74, 0x7d, 0x7b, 0x00, 0x2d, 0x00, 0x00, 0x34, 0x35,
    0x35, 0x73, 0x73, 0x73, 0x73, 0x73, 0x71, 0x73, 0x73, 0x73, 0x73, 0x73,
    0x73, 0x73, 0x73, 0x73, 0x73, 0x7e, 0x7f, 0x71, 0x2d, 0x00, 0x4b, 0x00,
    0x2d, 0x40, 0x3d, 0x3c, 0x3c, 0x3c, 0x38, 0x39, 0x2f, 0x7e, 0x7f, 0x38,
    0x39, 0x00, 0x2d, 0x3d, 0x00, 0x00, 0x3e, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x2d, 0x00, 0x00, 0x00, 0x00, 0x00, 0x2d, 0x00, 0x3e, 0x40, 0x38,
    0x39, 0x2f, 0x7e, 0x7f, 0x38, 0x39, 0x3f, 0x3e, 0x00, 0x00, 0x2d, 0x36,
    0x37, 0x00, 0x2d, 0x40, 0x3d, 0x3c, 0x36, 0x37, 0x00, 0x00, 0x2d, 0x00,
    0x40, 0x34, 0x2e, 0x24, 0x31, 0x7e, 0x7f, 0x24, 0x16, 0x6c, 0x6b, 0x6b,
    0x24, 0x24, 0x24, 0x24, 0x24, 0x24, 0x7e, 0x7f, 0x36, 0x37, 0x3f, 0x00,
    0x2d, 0x00, 0x00, 0x29, 0x30, 0x7e, 0x7f, 0x2b, 0x00, 0x2d, 0x00, 0x41,
    0x00, 0x2d, 0x00, 0x00, 0x00, 0x2c, 0x36, 0x37, 0x2f, 0x6c, 0x25, 0x6b,
    0x6c, 0x16, 0x6a, 0x6a, 0x25, 0x1b, 0x6b, 0x13, 0x16, 0x6a, 0x6a, 0x6c,
    0x6b, 0x7e, 0x7f, 0x2f, 0x00, 0x01, 0x00, 0x2d, 0x00, 0x00, 0x3e, 0x00,
    0x4b, 0x40, 0x34, 0x35, 0x2f, 0x7e, 0x7f, 0x34, 0x35, 0x3f, 0x00, 0x3e,
    0x2d, 0x00, 0x00, 0x00, 0x00, 0x2d, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x27, 0x27, 0x00, 0x00, 0x00, 0x00, 0x2d, 0x32, 0x33, 0x2f, 0x7e, 0x7f,
    0x32, 0x33, 0x2d, 0x00, 0x41, 0x00, 0x41, 0x38, 0x39, 0x3d, 0x3f, 0x41,
    0x3e, 0x00, 0x38, 0x39, 0x41, 0x00, 0x00, 0x00, 0x41, 0x32, 0x2f, 0x6a,
    0x30, 0x7e, 0x7f, 0x6a, 0x16, 0x13, 0x1a, 0x6a, 0x13, 0x1a, 0x18, 0x6b,
    0x6a, 0x6a, 0x7e, 0x7f, 0x38, 0x39, 0x41, 0x00, 0x00, 0x00, 0x41, 0x32,
    0x30, 0x7e, 0x7f, 0x33, 0x00, 0x00, 0x41, 0x00, 0x00, 0x00, 0x00, 0x41,
    0x00, 0x41, 0x38, 0x39, 0x2f, 0x6d, 0x6e, 0x3a, 0x3a, 0x16, 0x6b, 0x25,
    0x3a, 0x25, 0x6c, 0x6b, 0x25, 0x6c, 0x25, 0x6b, 0x3a, 0x7e, 0x7f, 0x2f,
    0x00, 0x2d, 0x00, 0x00, 0x01, 0x00, 0x00, 0x2d, 0x01, 0x00, 0x34, 0x35,
    0x2f, 0x7e, 0x7f, 0x34, 0x35, 0x00, 0x00, 0x01, 0x00, 0x01, 0x08, 0x09,
    0x00, 0x00, 0x00, 0x41, 0x41, 0x00, 0x00, 0x2c, 0x36, 0x37, 0x27, 0x26,
    0x27, 0x26, 0x27, 0x1f, 0x1e, 0x1b, 0x7e, 0x7f, 0x1d, 0x20, 0x27, 0x26,
    0x27, 0x26, 0x27, 0x36, 0x37, 0x3e, 0x41, 0x00, 0x41, 0x2c, 0x36, 0x37,
    0x27, 0x26, 0x27, 0x26, 0x27, 0x6b, 0x25, 0x13, 0x30, 0x7e, 0x7f, 0x1a,
    0x1b, 0x6b, 0x16, 0x25, 0x6b, 0x16, 0x25, 0x6c, 0x13, 0x13, 0x7e, 0x7f,
    0x1d, 0x20, 0x27, 0x26, 0x27, 0x26, 0x27, 0x25, 0x6a, 0x7e, 0x7f, 0x25,
    0x27, 0x26, 0x27, 0x26, 0x27, 0x26, 0x27, 0x26, 0x27, 0x26, 0x34, 0x35,
    0x35, 0x7e, 0x7f, 0x73, 0x73, 0x73, 0x73, 0x73, 0x73, 0x73, 0x71, 0x73,
    0x73, 0x73, 0x73, 0x73, 0x73, 0x73, 0x73, 0x73, 0x00, 0x00, 0x41, 0x00,
    0x00, 0x41, 0x00, 0x00, 0x00, 0x40, 0x36, 0x37, 0x2f, 0x7e, 0x7f, 0x36,
    0x37, 0x3f, 0x01, 0x08, 0x09, 0x0a, 0x05, 0x06, 0x0b, 0x01, 0x00, 0x00,
    0x00, 0x41, 0x00, 0x21, 0x38, 0x39, 0x1b, 0x1a, 0x1b, 0x19, 0x6d, 0x6e,
    0x1e, 0x3a, 0x7e, 0x7f, 0x1d, 0x6d, 0x6e, 0x1b, 0x1a, 0x18, 0x25, 0x38,
    0x39, 0x3f, 0x00, 0x41, 0x00, 0x21, 0x38, 0x39, 0x6c, 0x25, 0x25, 0x16,
    0x25, 0x6c, 0x1b, 0x3a, 0x30, 0x7e, 0x7f, 0x16, 0x3a, 0x1a, 0x18, 0x6b,
    0x6c, 0x16, 0x3a, 0x6b, 0x25, 0x1b, 0x7e, 0x7f, 0x1d, 0x1e, 0x6d, 0x6e,
    0x25, 0x3a, 0x3a, 0x6c, 0x30, 0x7e, 0x7f, 0x6c, 0x3a, 0x6c, 0x16, 0x44,
    0x45, 0x6c, 0x6c, 0x6b, 0x25, 0x16, 0x36, 0x37, 0x2f, 0x7e, 0x7f, 0x6a,
    0x16, 0x13, 0x25, 0x4f, 0x50, 0x51, 0x25, 0x6a, 0x16, 0x6c, 0x44, 0x45,
    0x6a, 0x6a, 0x6c, 0x25, 0x41, 0x01, 0x00, 0x41, 0x01, 0x00, 0x00, 0x41,
    0x41, 0x00, 0x38, 0x39, 0x2f, 0x7e, 0x7f, 0x38, 0x39, 0x00, 0x0a, 0x05,
    0x06, 0x11, 0x10, 0x10, 0x12, 0x00, 0x01, 0x00, 0x01, 0x00, 0x21, 0x40,
    0x32, 0x33, 0x1b, 0x18, 0x25, 0x16, 0x7e, 0x7f, 0x73, 0x71, 0x73, 0x71,
    0x73, 0x7e, 0x7f, 0x2f, 0x16, 0x25, 0x25, 0x32, 0x33, 0x00, 0x01, 0x00,
    0x43, 0x40, 0x32, 0x33, 0x25, 0x6c, 0x6c, 0x16, 0x73, 0x71, 0x71, 0x73,
    0x73, 0x73, 0x73, 0x71, 0x71, 0x73, 0x73, 0x73, 0x73, 0x71, 0x71, 0x73,
    0x73, 0x73, 0x73, 0x71, 0x71, 0x73, 0x7e, 0x7f, 0x73, 0x73, 0x73, 0x73,
    0x71, 0x73, 0x71, 0x73, 0x73, 0x73, 0x73, 0x73, 0x71, 0x25, 0x4f, 0x50,
    0x51, 0x16, 0x38, 0x39, 0x2f, 0x7e, 0x7f, 0x6c, 0x16, 0x25, 0x1b, 0x52,
    0x53, 0x54, 0x6b, 0x14, 0x16, 0x25, 0x46, 0x47, 0x25, 0x6d, 0x6e, 0x16,
    0x00, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x32, 0x33,
    0x2f, 0x7e, 0x7f, 0x32, 0x33, 0x01, 0x11, 0x10, 0x10, 0x0e, 0x42, 0x42,
    0x0f, 0x5a, 0x00, 0x41, 0x00, 0x43, 0x00, 0x01, 0x1f, 0x1e, 0x25, 0x25,
    0x1a, 0x18, 0x7e, 0x7f, 0x25, 0x25, 0x1b, 0x25, 0x25, 0x7e, 0x7f, 0x1b,
    0x18, 0x14, 0x25, 0x1d, 0x20, 0x01, 0x01, 0x21, 0x00, 0x01, 0x1f, 0x1e,
    0x6c, 0x14, 0x1a, 0x18, 0x24, 0x1d, 0x1e, 0x24, 0x24, 0x24, 0x24, 0x1d,
    0x1e, 0x24, 0x24, 0x24, 0x24, 0x1d, 0x1e, 0x24, 0x24, 0x24, 0x24, 0x1d,
    0x1e, 0x24, 0x7e, 0x7f, 0x24, 0x24, 0x24, 0x24, 0x24, 0x24, 0x24, 0x24,
    0x24, 0x24, 0x24, 0x24, 0x24, 0x42, 0x52, 0x53, 0x54, 0x42, 0x32, 0x33,
    0x35, 0x73, 0x73, 0x73, 0x73, 0x71, 0x73, 0x73, 0x73, 0x73, 0x73, 0x73,
    0x71, 0x73, 0x73, 0x73, 0x73, 0x7e, 0x7f, 0x71, 0x01, 0x00, 0x01, 0x00,
    0x00, 0x26, 0x27, 0x26, 0x27, 0x26, 0x1f, 0x1e, 0x1b, 0x7e, 0x7f, 0x1d,
    0x20, 0x27, 0x0e, 0x42, 0x34, 0x0e, 0x5f, 0x60, 0x0f, 0x27, 0x00, 0x01,
    0x21, 0x01, 0x00, 0x00, 0x1f, 0x1e, 0x19, 0x6a, 0x13, 0x6a, 0x7e, 0x7f,
    0x25, 0x13, 0x44, 0x45, 0x13, 0x7e, 0x7f, 0x6a, 0x13, 0x44, 0x45, 0x1d,
    0x20, 0x00, 0x43, 0x00, 0x01, 0x00, 0x1f, 0x1e, 0x25, 0x1b, 0x16, 0x44,
    0x45, 0x1d, 0x1e, 0x6c, 0x13, 0x6a, 0x25, 0x1d, 0x1e, 0x16, 0x44, 0x45,
    0x14, 0x1d, 0x1e, 0x6c, 0x13, 0x13, 0x16, 0x1d, 0x1e, 0x6c, 0x7e, 0x7f,
    0x6b, 0x13, 0x25, 0x6a, 0x6c, 0x13, 0x13, 0x6b, 0x6a, 0x13, 0x6c, 0x6b,
    0x6c, 0x16, 0x55, 0x56, 0x57, 0x25, 0x1d, 0x1e, 0x44, 0x45, 0x13, 0x6b,
    0x16, 0x6c, 0x25, 0x6a, 0x6c, 0x6b, 0x13, 0x13, 0x25, 0x6c, 0x6a, 0x6b,
    0x16, 0x7e, 0x7f, 0x2f, 0x3b, 0x00, 0x3b, 0x3b, 0x00, 0x25, 0x3a, 0x3a,
    0x1b, 0x3a, 0x1d, 0x1e, 0x25, 0x7e, 0x7f, 0x1d, 0x1e, 0x25, 0x0e, 0x23,
    0x23, 0x0e, 0x61, 0x62, 0x0f, 0x3a, 0x01, 0x21, 0x01, 0x00, 0x01, 0x00,
    0x1f, 0x1e, 0x16, 0x24, 0x3a, 0x24, 0x7e, 0x7f, 0x3a, 0x3a, 0x46, 0x47,
    0x3a, 0x7e, 0x7f, 0x24, 0x3a, 0x46, 0x47, 0x1d, 0x20, 0x21, 0x00, 0x3b,
    0x00, 0x3b, 0x1f, 0x1e, 0x3a, 0x3a, 0x16, 0x46, 0x47, 0x1d, 0x1e, 0x15,
    0x3a, 0x3a, 0x6c, 0x1d, 0x1e, 0x16, 0x46, 0x47, 0x6c, 0x1d, 0x1e, 0x16,
    0x3a, 0x3a, 0x16, 0x1d, 0x1e, 0x3a, 0x7e, 0x7f, 0x25, 0x6c, 0x6b, 0x3a,
    0x3a, 0x6c, 0x6b, 0x3a, 0x6c, 0x25, 0x14, 0x3a, 0x3a, 0x16, 0x55, 0x56,
    0x57, 0x6c, 0x1d, 0x1e, 0x46, 0x47, 0x25, 0x3a, 0x16, 0x3a, 0x44, 0x45,
    0x6b, 0x3a, 0x25, 0x6c, 0x1b, 0x6b, 0x25, 0x6c, 0x25, 0x7e, 0x7f, 0x2f,
    0x79, 0x77, 0x78, 0x79, 0x78, 0x78, 0x79, 0x77, 0x78, 0x77, 0x79, 0x79,
    0x77, 0x78, 0x77, 0x79, 0x78, 0x77, 0x79, 0x79, 0x78, 0x79, 0x78, 0x78,
    0x77, 0x79, 0x75, 0x74, 0x7d, 0x74, 0x74, 0x76, 0x77, 0x78, 0x79, 0x79,
    0x78, 0x77, 0x78, 0x78, 0x79, 0x78, 0x78, 0x78, 0x79, 0x78, 0x78, 0x78,
    0x79, 0x77, 0x78, 0x79, 0x78, 0x75, 0x7d, 0x74, 0x7d, 0x76, 0x77, 0x77,
    0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77,
    0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x78, 0x77, 0x77,
    0x77, 0x78, 0x78, 0x79, 0x78, 0x79, 0x78, 0x77, 0x79, 0x77, 0x77, 0x78,
    0x79, 0x77, 0x78, 0x77, 0x79, 0x79, 0x77, 0x77, 0x77, 0x79, 0x77, 0x78,
    0x77, 0x78, 0x77, 0x78, 0x79, 0x78, 0x78, 0x78, 0x77, 0x77, 0x77, 0x79,
    0x78, 0x78, 0x77, 0x79, 0x77, 0x77, 0x79, 0x77};

unsigned char defendmap[] = {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x48, 0x49, 0x49, 0x49, 0x4a, 0x00, 0x4b,
    0x00, 0x00, 0x00, 0x4c, 0x00, 0x48, 0x49, 0x49, 0x49, 0x4a, 0x00, 0x4b,
    0x00, 0x48, 0x49, 0x4a, 0x28, 0x2a, 0x22, 0x4b, 0x4c, 0x00, 0x4b, 0x4b,
    0x00, 0x28, 0x2a, 0x00, 0x26, 0x00, 0x26, 0x00, 0x28, 0x2a, 0x22, 0x00,
    0x4b, 0x00, 0x4b, 0x00, 0x4b, 0x48, 0x49, 0x49, 0x4a, 0x4b, 0x00, 0x48,
    0x4a, 0x00, 0x4b, 0x00, 0x00, 0x00, 0x4b, 0x00, 0x4b, 0x00, 0x48, 0x49,
    0x49, 0x4a, 0x00, 0x08, 0x06, 0x06, 0x0c, 0x06, 0x06, 0x0c, 0x0d, 0x06,
    0x0c, 0x0c, 0x06, 0x06, 0x06, 0x0c, 0x0d, 0x06, 0x06, 0x0b, 0x4b, 0x00,
    0x4b, 0x48, 0x49, 0x4a, 0x00, 0x4b, 0x00, 0x00, 0x48, 0x49, 0x49, 0x4a,
    0x00, 0x00, 0x4b, 0x00, 0x4b, 0x48, 0x4a, 0x4b, 0x00, 0x4b, 0x48, 0x49,
    0x49, 0x4a, 0x4b, 0x00, 0x48, 0x49, 0x4a, 0x4b, 0x26, 0x27, 0x26, 0x27,
    0x26, 0x27, 0x26, 0x27, 0x26, 0x27, 0x26, 0x27, 0x26, 0x27, 0x26, 0x27,
    0x26, 0x27, 0x26, 0x27, 0x26, 0x27, 0x26, 0x27, 0x26, 0x27, 0x26, 0x27,
    0x1f, 0x20, 0x4c, 0x22, 0x48, 0x49, 0x49, 0x4a, 0x21, 0x1f, 0x20, 0x27,
    0x13, 0x27, 0x13, 0x27, 0x1f, 0x20, 0x4b, 0x22, 0x4b, 0x48, 0x49, 0x4a,
    0x00, 0x4b, 0x4b, 0x4c, 0x00, 0x08, 0x0c, 0x06, 0x0c, 0x0d, 0x0c, 0x0c,
    0x06, 0x0b, 0x00, 0x4b, 0x4c, 0x4b, 0x00, 0x00, 0x4b, 0x00, 0x08, 0x06,
    0x0c, 0x0c, 0x0d, 0x0c, 0x06, 0x06, 0x06, 0x0c, 0x0d, 0x06, 0x0d, 0x0c,
    0x06, 0x0d, 0x06, 0x0c, 0x06, 0x06, 0x0b, 0x00, 0x4c, 0x00, 0x4b, 0x00,
    0x4b, 0x4c, 0x08, 0x0d, 0x0c, 0x0c, 0x0d, 0x06, 0x0c, 0x06, 0x09, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x4b, 0x4c, 0x00, 0x00, 0x4b, 0x00, 0x4c, 0x4b,
    0x4b, 0x4c, 0x00, 0x4b, 0x25, 0x6c, 0x3a, 0x25, 0x16, 0x25, 0x1b, 0x3a,
    0x6b, 0x25, 0x18, 0x6d, 0x6e, 0x3a, 0x25, 0x6c, 0x1b, 0x25, 0x16, 0x6b,
    0x3a, 0x25, 0x6b, 0x25, 0x3a, 0x6c, 0x3a, 0x25, 0x1d, 0x20, 0x00, 0x4b,
    0x22, 0x4b, 0x4b, 0x21, 0x00, 0x1f, 0x1e, 0x25, 0x3a, 0x6d, 0x6e, 0x3a,
    0x1d, 0x20, 0x00, 0x4b, 0x22, 0x00, 0x4b, 0x00, 0x4c, 0x4b, 0x00, 0x00,
    0x08, 0x0d, 0x0c, 0x06, 0x0d, 0x0c, 0x06, 0x0d, 0x0c, 0x06, 0x0b, 0x48,
    0x49, 0x49, 0x4a, 0x4b, 0x4c, 0x4b, 0x21, 0x1f, 0x1e, 0x2e, 0x24, 0x24,
    0x24, 0x24, 0x24, 0x24, 0x24, 0x24, 0x24, 0x24, 0x24, 0x24, 0x24, 0x31,
    0x1d, 0x20, 0x22, 0x4b, 0x4c, 0x4b, 0x4c, 0x48, 0x4a, 0x0a, 0x06, 0x0c,
    0x0d, 0x0c, 0x0d, 0x0c, 0x0d, 0x0c, 0x06, 0x0b, 0x00, 0x00, 0x48, 0x49,
    0x49, 0x4a, 0x4b, 0x00, 0x4b, 0x4c, 0x4b, 0x00, 0x4c, 0x00, 0x4b, 0x4c,
    0x73, 0x73, 0x73, 0x73, 0x73, 0x73, 0x73, 0x73, 0x73, 0x73, 0x72, 0x7e,
    0x7f, 0x73, 0x72, 0x73, 0x73, 0x73, 0x73, 0x73, 0x73, 0x73, 0x73, 0x72,
    0x72, 0x72, 0x73, 0x73, 0x73, 0x73, 0x7d, 0x7d, 0x7d, 0x7a, 0x7d, 0x7d,
    0x74, 0x73, 0x73, 0x73, 0x73, 0x7e, 0x7f, 0x73, 0x73, 0x73, 0x74, 0x74,
    0x74, 0x7b, 0x4b, 0x48, 0x4a, 0x00, 0x00, 0x21, 0x4b, 0x1f, 0x20, 0x27,
    0x27, 0x27, 0x27, 0x27, 0x1f, 0x20, 0x00, 0x22, 0x4b, 0x4c, 0x48, 0x4a,
    0x00, 0x43, 0x4b, 0x1f, 0x1e, 0x6a, 0x6a, 0x6a, 0x25, 0x6a, 0x25, 0x6a,
    0x6c, 0x6a, 0x1a, 0x4d, 0x4e, 0x6a, 0x6a, 0x6a, 0x1d, 0x20, 0x4b, 0x22,
    0x48, 0x49, 0x4a, 0x00, 0x43, 0x4b, 0x1f, 0x1e, 0x6a, 0x1a, 0x6a, 0x6b,
    0x6a, 0x1d, 0x20, 0x22, 0x22, 0x48, 0x49, 0x4a, 0x00, 0x4b, 0x4b, 0x4c,
    0x00, 0x4b, 0x48, 0x4a, 0x00, 0x4b, 0x4c, 0x00, 0x24, 0x24, 0x24, 0x24,
    0x24, 0x24, 0x24, 0x24, 0x24, 0x24, 0x24, 0x7e, 0x7f, 0x2e, 0x24, 0x24,
    0x24, 0x24, 0x66, 0x34, 0x35, 0x67, 0x24, 0x16, 0x24, 0x24, 0x24, 0x66,
    0x5b, 0x5c, 0x3c, 0x3c, 0x58, 0x3f, 0x48, 0x4a, 0x40, 0x5b, 0x5c, 0x67,
    0x24, 0x7e, 0x7f, 0x66, 0x1b, 0x35, 0x22, 0x48, 0x49, 0x49, 0x4a, 0x4b,
    0x00, 0x00, 0x21, 0x4b, 0x4c, 0x1f, 0x1e, 0x25, 0x1b, 0x6d, 0x6e, 0x25,
    0x1d, 0x20, 0x4b, 0x4c, 0x22, 0x4b, 0x4b, 0x00, 0x21, 0x48, 0x4a, 0x1f,
    0x1e, 0x25, 0x25, 0x6c, 0x3a, 0x25, 0x6c, 0x25, 0x6d, 0x6e, 0x1b, 0x68,
    0x69, 0x3a, 0x3a, 0x25, 0x1d, 0x20, 0x48, 0x4a, 0x22, 0x00, 0x4c, 0x21,
    0x48, 0x4a, 0x1f, 0x1e, 0x1a, 0x1b, 0x6d, 0x6e, 0x6b, 0x1d, 0x20, 0x4c,
    0x22, 0x22, 0x00, 0x00, 0x4b, 0x4c, 0x00, 0x48, 0x49, 0x4a, 0x00, 0x00,
    0x4b, 0x00, 0x4b, 0x4c, 0x6c, 0x25, 0x6c, 0x1b, 0x19, 0x25, 0x02, 0x03,
    0x25, 0x4d, 0x4e, 0x7e, 0x7f, 0x2f, 0x1b, 0x6a, 0x6c, 0x16, 0x30, 0x63,
    0x64, 0x2f, 0x6b, 0x16, 0x6a, 0x6b, 0x6c, 0x30, 0x5d, 0x5e, 0x49, 0x49,
    0x59, 0x4a, 0x08, 0x09, 0x40, 0x5d, 0x5e, 0x2f, 0x1b, 0x7e, 0x7f, 0x6c,
    0x1d, 0x20, 0x00, 0x22, 0x4c, 0x00, 0x4b, 0x00, 0x7c, 0x7d, 0x70, 0x70,
    0x7d, 0x73, 0x73, 0x72, 0x73, 0x7e, 0x7f, 0x72, 0x73, 0x73, 0x7d, 0x70,
    0x7d, 0x70, 0x7a, 0x70, 0x7d, 0x70, 0x7d, 0x73, 0x73, 0x73, 0x72, 0x73,
    0x73, 0x73, 0x72, 0x73, 0x7e, 0x7f, 0x72, 0x73, 0x73, 0x73, 0x72, 0x73,
    0x73, 0x73, 0x74, 0x7d, 0x70, 0x7b, 0x7c, 0x7d, 0x70, 0x7d, 0x73, 0x73,
    0x73, 0x72, 0x7e, 0x7f, 0x73, 0x73, 0x73, 0x7d, 0x7d, 0x74, 0x7d, 0x7d,
    0x7d, 0x7b, 0x00, 0x00, 0x4b, 0x00, 0x4c, 0x4b, 0x00, 0x00, 0x4b, 0x00,
    0x6c, 0x14, 0x25, 0x13, 0x1b, 0x04, 0x05, 0x06, 0x07, 0x68, 0x69, 0x7e,
    0x7f, 0x2f, 0x16, 0x24, 0x13, 0x1b, 0x6c, 0x1d, 0x1e, 0x6c, 0x25, 0x16,
    0x24, 0x6c, 0x6b, 0x30, 0x34, 0x35, 0x3c, 0x58, 0x3f, 0x0a, 0x05, 0x06,
    0x0b, 0x34, 0x35, 0x2f, 0x13, 0x7e, 0x7f, 0x1b, 0x1d, 0x20, 0x00, 0x4b,
    0x22, 0x00, 0x48, 0x49, 0x4a, 0x4b, 0x6f, 0x58, 0x4b, 0x5b, 0x5c, 0x67,
    0x24, 0x7e, 0x7f, 0x66, 0x5b, 0x5c, 0x3f, 0x6f, 0x48, 0x58, 0x4a, 0x6f,
    0x4c, 0x6f, 0x40, 0x34, 0x35, 0x67, 0x25, 0x1b, 0x19, 0x25, 0x25, 0x25,
    0x7e, 0x7f, 0x25, 0x25, 0x6c, 0x25, 0x16, 0x66, 0x34, 0x35, 0x3f, 0x4b,
    0x6f, 0x48, 0x4a, 0x4b, 0x6f, 0x48, 0x5b, 0x5c, 0x67, 0x6c, 0x7e, 0x7f,
    0x66, 0x5b, 0x5c, 0x3c, 0x58, 0x3f, 0x00, 0x4b, 0x4b, 0x00, 0x48, 0x4a,
    0x00, 0x00, 0x00, 0x00, 0x48, 0x4a, 0x00, 0x00, 0x3a, 0x25, 0x25, 0x6d,
    0x6e, 0x0e, 0x5f, 0x60, 0x0f, 0x73, 0x72, 0x73, 0x73, 0x72, 0x73, 0x2f,
    0x6d, 0x6e, 0x3a, 0x1d, 0x1e, 0x25, 0x25, 0x02, 0x03, 0x25, 0x1b, 0x30,
    0x1b, 0x35, 0x4b, 0x59, 0x4c, 0x11, 0x10, 0x10, 0x12, 0x34, 0x35, 0x2f,
    0x6a, 0x7e, 0x7f, 0x72, 0x73, 0x73, 0x74, 0x70, 0x74, 0x7d, 0x70, 0x7b,
    0x4b, 0x00, 0x58, 0x59, 0x00, 0x5d, 0x5e, 0x19, 0x6a, 0x7e, 0x7f, 0x6c,
    0x5d, 0x5e, 0x4a, 0x58, 0x4b, 0x59, 0x4b, 0x6f, 0x48, 0x58, 0x4a, 0x63,
    0x64, 0x25, 0x6c, 0x25, 0x16, 0x25, 0x14, 0x25, 0x7e, 0x7f, 0x25, 0x6c,
    0x25, 0x25, 0x1b, 0x25, 0x63, 0x64, 0x22, 0x48, 0x58, 0x49, 0x49, 0x4a,
    0x6f, 0x4c, 0x5d, 0x5e, 0x16, 0x6a, 0x7e, 0x7f, 0x25, 0x5d, 0x5e, 0x4c,
    0x59, 0x48, 0x49, 0x49, 0x4a, 0x00, 0x00, 0x4b, 0x48, 0x49, 0x49, 0x4a,
    0x00, 0x00, 0x00, 0x00, 0x73, 0x73, 0x72, 0x7e, 0x7f, 0x0e, 0x61, 0x62,
    0x0f, 0x67, 0x24, 0x24, 0x24, 0x24, 0x66, 0x73, 0x7e, 0x7f, 0x72, 0x73,
    0x73, 0x25, 0x04, 0x05, 0x06, 0x07, 0x25, 0x30, 0x5b, 0x5c, 0x00, 0x48,
    0x49, 0x0e, 0x42, 0x42, 0x0f, 0x5b, 0x5c, 0x2f, 0x24, 0x7e, 0x7f, 0x1b,
    0x1d, 0x20, 0x4c, 0x58, 0x00, 0x4b, 0x6f, 0x00, 0x48, 0x49, 0x59, 0x49,
    0x49, 0x34, 0x35, 0x1b, 0x6b, 0x7e, 0x7f, 0x1a, 0x34, 0x35, 0x48, 0x59,
    0x49, 0x49, 0x49, 0x58, 0x4a, 0x59, 0x00, 0x1f, 0x1e, 0x25, 0x6a, 0x13,
    0x6a, 0x13, 0x4d, 0x4e, 0x7e, 0x7f, 0x25, 0x4d, 0x4e, 0x13, 0x6a, 0x25,
    0x1d, 0x20, 0x00, 0x22, 0x59, 0x00, 0x4c, 0x4b, 0x6f, 0x4b, 0x34, 0x35,
    0x1b, 0x6b, 0x7e, 0x7f, 0x25, 0x34, 0x35, 0x22, 0x2d, 0x4b, 0x00, 0x00,
    0x2d, 0x00, 0x4b, 0x2d, 0x00, 0x00, 0x00, 0x2d, 0x00, 0x2d, 0x4b, 0x00,
    0x24, 0x24, 0x24, 0x7e, 0x7f, 0x73, 0x73, 0x73, 0x73, 0x2f, 0x25, 0x6c,
    0x6a, 0x6c, 0x25, 0x24, 0x7e, 0x7f, 0x2e, 0x24, 0x24, 0x30, 0x11, 0x10,
    0x10, 0x12, 0x2f, 0x30, 0x5d, 0x5e, 0x22, 0x00, 0x4b, 0x0e, 0x1b, 0x10,
    0x0f, 0x5d, 0x5e, 0x2f, 0x13, 0x7e, 0x7f, 0x6c, 0x1d, 0x20, 0x2d, 0x59,
    0x00, 0x4c, 0x6f, 0x2d, 0x2d, 0x4b, 0x00, 0x4b, 0x00, 0x5b, 0x5c, 0x18,
    0x6c, 0x7e, 0x7f, 0x1b, 0x5b, 0x5c, 0x3f, 0x2d, 0x4b, 0x00, 0x00, 0x59,
    0x2d, 0x4b, 0x00, 0x1f, 0x1e, 0x3a, 0x3a, 0x6c, 0x16, 0x3a, 0x68, 0x69,
    0x7e, 0x7f, 0x15, 0x68, 0x69, 0x1a, 0x18, 0x3a, 0x1d, 0x20, 0x4b, 0x2d,
    0x22, 0x4b, 0x00, 0x48, 0x58, 0x49, 0x5b, 0x5c, 0x16, 0x6c, 0x7e, 0x7f,
    0x25, 0x5b, 0x5c, 0x2d, 0x22, 0x00, 0x2d, 0x00, 0x4b, 0x00, 0x00, 0x00,
    0x08, 0x09, 0x4b, 0x00, 0x00, 0x00, 0x2d, 0x00, 0x6b, 0x1b, 0x19, 0x7e,
    0x7f, 0x2e, 0x24, 0x24, 0x24, 0x6c, 0x1b, 0x19, 0x24, 0x13, 0x6b, 0x1b,
    0x7e, 0x7f, 0x2f, 0x16, 0x6b, 0x30, 0x0e, 0x42, 0x42, 0x0f, 0x2f, 0x30,
    0x35, 0x34, 0x00, 0x22, 0x2d, 0x0e, 0x1c, 0x34, 0x0f, 0x34, 0x35, 0x2f,
    0x6a, 0x7e, 0x7f, 0x73, 0x73, 0x73, 0x74, 0x7d, 0x7d, 0x74, 0x74, 0x7d,
    0x7b, 0x00, 0x00, 0x2d, 0x2d, 0x5d, 0x5e, 0x25, 0x6a, 0x7e, 0x7f, 0x16,
    0x5d, 0x5e, 0x3f, 0x00, 0x2d, 0x08, 0x09, 0x00, 0x00, 0x2d, 0x40, 0x73,
    0x73, 0x73, 0x72, 0x73, 0x73, 0x73, 0x72, 0x73, 0x7e, 0x7f, 0x72, 0x73,
    0x73, 0x73, 0x72, 0x73, 0x73, 0x73, 0x7d, 0x7d, 0x70, 0x7b, 0x2d, 0x00,
    0x59, 0x00, 0x5d, 0x5e, 0x18, 0x25, 0x7e, 0x7f, 0x25, 0x5d, 0x5e, 0x00,
    0x41, 0x22, 0x00, 0x00, 0x41, 0x00, 0x41, 0x0a, 0x05, 0x0d, 0x0b, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x13, 0x1a, 0x6a, 0x7e, 0x7f, 0x2f, 0x1b, 0x4f,
    0x50, 0x51, 0x25, 0x1b, 0x4f, 0x50, 0x51, 0x16, 0x7e, 0x7f, 0x2f, 0x16,
    0x6c, 0x30, 0x0e, 0x10, 0x10, 0x0f, 0x2f, 0x30, 0x63, 0x64, 0x2d, 0x4b,
    0x22, 0x65, 0x58, 0x6f, 0x65, 0x63, 0x64, 0x2f, 0x24, 0x7e, 0x7f, 0x25,
    0x63, 0x64, 0x41, 0x00, 0x2d, 0x00, 0x08, 0x09, 0x2d, 0x00, 0x41, 0x00,
    0x00, 0x63, 0x64, 0x6b, 0x25, 0x7e, 0x7f, 0x16, 0x63, 0x64, 0x00, 0x41,
    0x08, 0x05, 0x06, 0x0b, 0x41, 0x00, 0x41, 0x63, 0x64, 0x64, 0x64, 0x64,
    0x64, 0x64, 0x64, 0x64, 0x7e, 0x7f, 0x64, 0x64, 0x64, 0x64, 0x64, 0x64,
    0x64, 0x64, 0x00, 0x2d, 0x58, 0x00, 0x41, 0x00, 0x00, 0x2d, 0x63, 0x64,
    0x25, 0x25, 0x7e, 0x7f, 0x25, 0x63, 0x64, 0x3c, 0x58, 0x3f, 0x22, 0x41,
    0x00, 0x41, 0x00, 0x11, 0x10, 0x10, 0x12, 0x41, 0x00, 0x00, 0x00, 0x00,
    0x1a, 0x18, 0x24, 0x7e, 0x7f, 0x2f, 0x16, 0x52, 0x53, 0x54, 0x6b, 0x16,
    0x52, 0x53, 0x54, 0x16, 0x7e, 0x7f, 0x2f, 0x1b, 0x6b, 0x30, 0x0e, 0x1c,
    0x1b, 0x0f, 0x2f, 0x25, 0x1d, 0x20, 0x00, 0x00, 0x2d, 0x22, 0x59, 0x58,
    0x2d, 0x1f, 0x1e, 0x2f, 0x13, 0x7e, 0x7f, 0x6c, 0x1d, 0x20, 0x00, 0x00,
    0x00, 0x0a, 0x05, 0x06, 0x0b, 0x00, 0x00, 0x00, 0x41, 0x1f, 0x1e, 0x6b,
    0x25, 0x7e, 0x7f, 0x6b, 0x1d, 0x20, 0x00, 0x00, 0x11, 0x10, 0x10, 0x12,
    0x5a, 0x00, 0x41, 0x1f, 0x1e, 0x4f, 0x50, 0x51, 0x1b, 0x4f, 0x50, 0x51,
    0x7e, 0x7f, 0x6b, 0x6b, 0x6c, 0x4f, 0x50, 0x51, 0x1d, 0x20, 0x41, 0x00,
    0x59, 0x41, 0x00, 0x00, 0x41, 0x00, 0x1f, 0x1e, 0x25, 0x6a, 0x7e, 0x7f,
    0x6b, 0x1d, 0x20, 0x00, 0x59, 0x41, 0x00, 0x22, 0x00, 0x00, 0x00, 0x0e,
    0x42, 0x42, 0x0f, 0x5a, 0x00, 0x00, 0x00, 0x00, 0x18, 0x4d, 0x4e, 0x7e,
    0x7f, 0x2f, 0x16, 0x55, 0x56, 0x57, 0x4d, 0x4e, 0x55, 0x56, 0x57, 0x18,
    0x7e, 0x7f, 0x2f, 0x13, 0x25, 0x30, 0x0e, 0x5f, 0x60, 0x0f, 0x2f, 0x1b,
    0x1d, 0x20, 0x00, 0x41, 0x00, 0x41, 0x22, 0x59, 0x41, 0x1f, 0x1e, 0x2f,
    0x6a, 0x7e, 0x7f, 0x25, 0x1d, 0x20, 0x27, 0x26, 0x27, 0x11, 0x10, 0x10,
    0x12, 0x26, 0x27, 0x26, 0x27, 0x1f, 0x1e, 0x25, 0x14, 0x7e, 0x7f, 0x25,
    0x1d, 0x20, 0x27, 0x26, 0x0e, 0x42, 0x42, 0x0f, 0x27, 0x26, 0x27, 0x1f,
    0x1e, 0x52, 0x53, 0x54, 0x13, 0x52, 0x53, 0x54, 0x7e, 0x7f, 0x6c, 0x14,
    0x13, 0x52, 0x53, 0x54, 0x1d, 0x20, 0x27, 0x26, 0x27, 0x26, 0x27, 0x26,
    0x27, 0x26, 0x1f, 0x1e, 0x25, 0x25, 0x7e, 0x7f, 0x25, 0x1d, 0x20, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x22, 0x00, 0x00, 0x0e, 0x1c, 0x34, 0x0f, 0x26,
    0x27, 0x26, 0x00, 0x00, 0x3a, 0x68, 0x69, 0x7e, 0x7f, 0x2f, 0x16, 0x55,
    0x56, 0x57, 0x68, 0x69, 0x55, 0x56, 0x57, 0x3a, 0x7e, 0x7f, 0x2f, 0x3a,
    0x6b, 0x30, 0x0e, 0x61, 0x62, 0x0f, 0x2f, 0x25, 0x1d, 0x20, 0x00, 0x00,
    0x41, 0x00, 0x00, 0x22, 0x00, 0x1f, 0x1e, 0x2f, 0x3a, 0x7e, 0x7f, 0x25,
    0x1d, 0x1e, 0x6c, 0x3a, 0x6b, 0x0e, 0x23, 0x1b, 0x0f, 0x3a, 0x3a, 0x6b,
    0x6c, 0x1d, 0x1e, 0x25, 0x25, 0x7e, 0x7f, 0x25, 0x1d, 0x1e, 0x25, 0x3a,
    0x0e, 0x1c, 0x23, 0x0f, 0x1a, 0x1b, 0x25, 0x1d, 0x1e, 0x55, 0x56, 0x57,
    0x3a, 0x55, 0x56, 0x57, 0x7e, 0x7f, 0x6b, 0x6c, 0x3a, 0x55, 0x56, 0x57,
    0x1d, 0x1e, 0x6c, 0x1b, 0x19, 0x3a, 0x16, 0x3a, 0x1b, 0x25, 0x1d, 0x1e,
    0x25, 0x14, 0x7e, 0x7f, 0x25, 0x1d, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x22, 0x00, 0x0e, 0x23, 0x23, 0x0f, 0x19, 0x16, 0x3a, 0x00, 0x00,
    0x78, 0x79, 0x79, 0x78, 0x79, 0x77, 0x78, 0x78, 0x77, 0x79, 0x79, 0x78,
    0x77, 0x79, 0x78, 0x79, 0x78, 0x78, 0x79, 0x78, 0x77, 0x78, 0x79, 0x77,
    0x78, 0x79, 0x77, 0x78, 0x79, 0x77, 0x75, 0x74, 0x7d, 0x7d, 0x74, 0x7d,
    0x76, 0x77, 0x79, 0x78, 0x77, 0x78, 0x79, 0x78, 0x79, 0x77, 0x79, 0x77,
    0x79, 0x78, 0x79, 0x77, 0x78, 0x77, 0x78, 0x79, 0x77, 0x78, 0x79, 0x77,
    0x79, 0x78, 0x79, 0x77, 0x79, 0x78, 0x77, 0x78, 0x79, 0x77, 0x78, 0x77,
    0x79, 0x78, 0x77, 0x77, 0x77, 0x78, 0x79, 0x78, 0x78, 0x77, 0x78, 0x79,
    0x79, 0x78, 0x77, 0x78, 0x77, 0x78, 0x77, 0x78, 0x79, 0x77, 0x78, 0x79,
    0x77, 0x79, 0x78, 0x77, 0x78, 0x77, 0x78, 0x79, 0x78, 0x77, 0x79, 0x79,
    0x78, 0x79, 0x79, 0x75, 0x74, 0x7d, 0x74, 0x7d, 0x7d, 0x74, 0x76, 0x77,
    0x79, 0x78, 0x77, 0x79, 0x78, 0x77, 0x77, 0x77};

// collision tables

cl_func man_collision_handlers[] = {cl_man_hits_dragging_enemy,
                                    0,
                                    0,
                                    cl_man_hits_enemy,
                                    cl_man_hits_enemy,
                                    cl_man_hits,
                                    cl_man_hits,
                                    cl_man_hits,
                                    cl_man_hits_dragging_enemy,
                                    cl_man_hits_dragging_enemy,
                                    cl_man_hits_dragging_enemy,
                                    cl_man_hits_monk,
                                    cl_man_hits,
                                    cl_man_hits_monk,
                                    cl_man_hits_dragging_enemy,
                                    cl_man_hits_enemy,
                                    cl_man_hits,
                                    0,
                                    cl_enemy_addon_hits_man,
                                    cl_man_hits_banner,
                                    cl_man_hits_enemy_banner};

cl_func man_addon_collision_handlers[] = {
    cl_man_addon_hits, cl_man_addon_hits, cl_man_addon_hits, cl_man_addon_hits,
    cl_man_addon_hits, cl_man_addon_hits, cl_man_addon_hits, cl_man_addon_hits,
    cl_man_addon_hits, cl_man_addon_hits, cl_man_addon_hits, cl_man_addon_hits,
    cl_man_addon_hits, cl_man_addon_hits, cl_man_addon_hits, cl_man_addon_hits,
    cl_man_addon_hits, cl_man_addon_hits, cl_man_addon_hits, cl_man_addon_hits,
    cl_man_addon_hits};

cl_func man_missile_collision_handlers[] = {
    cl_man_missile_hits, cl_man_missile_hits, cl_man_missile_hits,
    cl_man_missile_hits, cl_man_missile_hits, cl_man_missile_hits,
    cl_man_missile_hits, cl_man_missile_hits, cl_man_missile_hits,
    cl_man_missile_hits, cl_man_missile_hits, cl_man_missile_hits,
    cl_man_missile_hits, cl_man_missile_hits, cl_man_missile_hits,
    cl_man_missile_hits, cl_man_missile_hits, cl_man_missile_hits,
    cl_man_missile_hits, cl_man_missile_hits, cl_man_missile_hits};

cl_func mind_collision_handlers[] = {0,
                                     0,
                                     0,
                                     0,
                                     0,
                                     0,
                                     0,
                                     0,
                                     0,
                                     0,
                                     0,
                                     0,
                                     0,
                                     0,
                                     0,
                                     0,
                                     cl_hand_hits_fire,
                                     cl_hand_hits_item,
                                     cl_mind_hits_fire,
                                     0,
                                     0};

int blackspell[] = {FRM_SPELLS,     1, FRM_SPELLS,     1, FRM_SPELLS + 2, 1,
                    FRM_SPELLS + 2, 1, FRM_SHIELD + 6, 3};

frame_func init_state_table[] = {
    game_setstate_title,      game_setstate_menu,       game_setstate_map,
    game_setstate_scores,     game_setstate_hiscore,    game_setstate_battle,
    game_setstate_battle_won, game_setstate_battle_won, game_setstate_mind,
    game_setstate_mind_won,   game_setstate_mind_won,   game_setstate_credits,
    game_setstate_oracle,     game_setstate_demo};

frame_func frame_state_table[] = {
    game_frame_title,      game_frame_menu,        game_frame_map,
    game_frame_scores,     game_frame_hiscore,     game_frame_battle,
    game_frame_battle_won, game_frame_battle_lost, game_frame_mind,
    game_frame_mind_won,   game_frame_mind_lost,   game_frame_credits,
    game_frame_oracle,     game_frame_demo};

int lettertable[] = {0,   ((WINDOW_HEIGHT / 2) - 64) + 48, FRM_LETTERS,
                     36,  ((WINDOW_HEIGHT / 2) - 64) + 48, FRM_LETTERS + 1,
                     72,  ((WINDOW_HEIGHT / 2) - 64) + 48, FRM_LETTERS + 2,
                     108, ((WINDOW_HEIGHT / 2) - 64) + 48, FRM_LETTERS + 3,
                     144, ((WINDOW_HEIGHT / 2) - 64) + 48, FRM_LETTERS + 4,
                     180, ((WINDOW_HEIGHT / 2) - 64) + 48, FRM_LETTERS + 5,
                     216, ((WINDOW_HEIGHT / 2) - 64) + 48, FRM_LETTERS + 6,
                     252, ((WINDOW_HEIGHT / 2) - 64) + 48, FRM_LETTERS + 7,
                     288, ((WINDOW_HEIGHT / 2) - 64) + 48, FRM_LETTERS + 8};

// army tables

int army0[] = {BFTP_SPEARMAN, BFTP_SPEARMAN, BFTP_SPEARMAN, BFTP_SPEARMAN,
               BFTP_SPEARMAN, BFTP_BESERK,   BFTP_SPEARMAN, BFTP_SPEARMAN,
               BFTP_SPEARMAN, BFTP_SPEARMAN, BFTP_SPEARMAN, BFTP_BESERK,
               BFTP_SPEARMAN, BFTP_SPEARMAN, BFTP_SPEARMAN, BFTP_SPEARMAN,
               BFTP_SPEARMAN, BFTP_BESERK};

int army1[] = {BFTP_WIZARD, BFTP_WIZARD, BFTP_WIZARD, BFTP_WIZARD, BFTP_WIZARD,
               BFTP_CARPET, BFTP_WIZARD, BFTP_WIZARD, BFTP_WIZARD, BFTP_WIZARD,
               BFTP_WIZARD, BFTP_CARPET, BFTP_WIZARD, BFTP_WIZARD, BFTP_WIZARD,
               BFTP_WIZARD, BFTP_WIZARD, BFTP_CARPET};

int army2[] = {BFTP_FOOTMAN, BFTP_FOOTMAN, BFTP_FOOTMAN, BFTP_FOOTMAN,
               BFTP_FOOTMAN, BFTP_CANNON,  BFTP_FOOTMAN, BFTP_FOOTMAN,
               BFTP_FOOTMAN, BFTP_FOOTMAN, BFTP_FOOTMAN, BFTP_CANNON,
               BFTP_FOOTMAN, BFTP_FOOTMAN, BFTP_FOOTMAN, BFTP_FOOTMAN,
               BFTP_FOOTMAN, BFTP_FOOTMAN};

int army3[] = {BFTP_BOARRIDER, BFTP_BOARRIDER, BFTP_BOARRIDER, BFTP_BOARRIDER,
               BFTP_BOARRIDER, BFTP_MONK,      BFTP_SPEARMAN,  BFTP_SPEARMAN,
               BFTP_SPEARMAN,  BFTP_SPEARMAN,  BFTP_SPEARMAN,  BFTP_MONK,
               BFTP_SPEARMAN,  BFTP_SPEARMAN,  BFTP_SPEARMAN,  BFTP_SPEARMAN,
               BFTP_SPEARMAN,  BFTP_MONK};

int army4[] = {BFTP_KNIGHT,   BFTP_KNIGHT, BFTP_KNIGHT, BFTP_KNIGHT,
               BFTP_KNIGHT,   BFTP_TOWER,  BFTP_KNIGHT, BFTP_KNIGHT,
               BFTP_KNIGHT,   BFTP_KNIGHT, BFTP_TOWER,  BFTP_KNIGHT,
               BFTP_KNIGHT,   BFTP_KNIGHT, BFTP_KNIGHT, BFTP_KNIGHT,
               BFTP_SPEARMAN, BFTP_KNIGHT};

int army5[] = {BFTP_HORSE,  BFTP_HORSE,     BFTP_HORSE,  BFTP_HORSE,
               BFTP_HORSE,  BFTP_BOARRIDER, BFTP_KNIGHT, BFTP_KNIGHT,
               BFTP_KNIGHT, BFTP_KNIGHT,    BFTP_KNIGHT, BFTP_SPEARMAN,
               BFTP_KNIGHT, BFTP_KNIGHT,    BFTP_KNIGHT, BFTP_KNIGHT,
               BFTP_KNIGHT, BFTP_SPEARMAN};

int army6[] = {BFTP_BESERK, BFTP_BESERK,   BFTP_BESERK, BFTP_BESERK,
               BFTP_BESERK, BFTP_SPEARMAN, BFTP_BESERK, BFTP_BESERK,
               BFTP_BESERK, BFTP_BESERK,   BFTP_BESERK, BFTP_SPEARMAN,
               BFTP_BESERK, BFTP_BESERK,   BFTP_BESERK, BFTP_BESERK,
               BFTP_BESERK, BFTP_SPEARMAN};

int army7[] = {BFTP_CARPET, BFTP_CARPET, BFTP_CARPET,  BFTP_CARPET, BFTP_CARPET,
               BFTP_TOWER,  BFTP_CARPET, BFTP_CARPET,  BFTP_CARPET, BFTP_CARPET,
               BFTP_TOWER,  BFTP_CARPET, BFTP_CARPET,  BFTP_CARPET, BFTP_CARPET,
               BFTP_CARPET, BFTP_CARPET, BFTP_SPEARMAN};

int army8[] = {BFTP_BALISTA, BFTP_BALISTA, BFTP_BALISTA, BFTP_BALISTA,
               BFTP_CANNON,  BFTP_BALISTA, BFTP_BALISTA, BFTP_BALISTA,
               BFTP_BALISTA, BFTP_BALISTA, BFTP_BALISTA, BFTP_CANNON,
               BFTP_FOOTMAN, BFTP_FOOTMAN, BFTP_FOOTMAN, BFTP_FOOTMAN,
               BFTP_FOOTMAN, BFTP_FOOTMAN};

int army9[] = {BFTP_CANNON,  BFTP_CANNON,  BFTP_CANNON,  BFTP_CANNON,
               BFTP_CANNON,  BFTP_FOOTMAN, BFTP_CANNON,  BFTP_CANNON,
               BFTP_CANNON,  BFTP_CANNON,  BFTP_CANNON,  BFTP_FOOTMAN,
               BFTP_FOOTMAN, BFTP_FOOTMAN, BFTP_FOOTMAN, BFTP_FOOTMAN,
               BFTP_FOOTMAN, BFTP_FOOTMAN};

int army10[] = {BFTP_FOOTMAN, BFTP_FOOTMAN, BFTP_FOOTMAN, BFTP_FOOTMAN,
                BFTP_FOOTMAN, BFTP_HORSE,   BFTP_OIL,     BFTP_OIL,
                BFTP_OIL,     BFTP_OIL,     BFTP_OIL,     BFTP_KNIGHT,
                BFTP_FOOTMAN, BFTP_FOOTMAN, BFTP_FOOTMAN, BFTP_FOOTMAN,
                BFTP_FOOTMAN, BFTP_KNIGHT};

int army11[] = {BFTP_MONK,   BFTP_MONK,   BFTP_MONK, BFTP_MONK, BFTP_MONK,
                BFTP_BESERK, BFTP_MONK,   BFTP_MONK, BFTP_MONK, BFTP_MONK,
                BFTP_BESERK, BFTP_MONK,   BFTP_MONK, BFTP_MONK, BFTP_MONK,
                BFTP_MONK,   BFTP_BESERK, BFTP_MONK};

int army12[] = {BFTP_TOWER,    BFTP_TOWER,    BFTP_TOWER,    BFTP_TOWER,
                BFTP_TOWER,    BFTP_BALISTA,  BFTP_TOWER,    BFTP_TOWER,
                BFTP_TOWER,    BFTP_TOWER,    BFTP_TOWER,    BFTP_OIL,
                BFTP_SPEARMAN, BFTP_SPEARMAN, BFTP_SPEARMAN, BFTP_SPEARMAN,
                BFTP_SPEARMAN, BFTP_BALISTA};

int army13[] = {BFTP_SKELETON_HORSE, BFTP_SKELETON_HORSE, BFTP_SKELETON_HORSE,
                BFTP_SKELETON_MONK,  BFTP_SKELETON_MONK,  BFTP_SKELETON_MONK,
                BFTP_SKELETON,       BFTP_SKELETON,       BFTP_SKELETON_HORSE,
                BFTP_SKELETON_MONK,  BFTP_SKELETON_MONK,  BFTP_SKELETON_MONK,
                BFTP_SKELETON,       BFTP_SKELETON,       BFTP_SKELETON_HORSE,
                BFTP_SKELETON_MONK,  BFTP_SKELETON_MONK,  BFTP_SKELETON_MONK};

int *armytable[] = {army0, army1, army2, army3,  army4,  army5,  army6,
                    army7, army8, army9, army10, army11, army12, army13};

// string data

const char *status_names[] = {"SCUM",   "WARRIOR", "FANATIC", "CHAMPION",
                              "HERO",   "BARRON",  "LORD",    "LEGEND",
                              "REAPER", "DEMIGOD"};

const char *cult_names[] = {"EAGLE",    "LIGHTNING", "ARDUCK", "SHESH",
                            "BOAR",     "STAG",      "EYE",    "MOON",
                            "HOLY",     "HAND",      "ZORACK", "BROBEK",
                            "MOUNTAIN", "FOREST",    "RIMOG",  "DEATH"};

const char *lord_names[] = {"COMMANDER", "CHIEFTAIN", "LORD", "DEMON"};

const char *army_names[] = {
    "HILLMEN",  "NECROMANTIC", "ROBBER",     "BOARRIDER", "MERCENARY",
    "KNIGHTLY", "BERSERKER",   "MYSTIC",     "BALISTIC",  "POWDER",
    "CAULDRON", "MONASTIC",    "JUGGERNAUT", "PLAGUE"};

char loc_name_0[] = {TINK(RGBWHITE) TMOVE(240, ((WINDOW_HEIGHT / 2) - 64) + 48)
                         TTEXT("THE") TREL(-24, 8) TTEXT("ORACLE OF")
                             TREL(-72, 8) TTEXT("GARGORE") TEND};

char loc_name_1[] = {TINK(RGBWHITE) TMOVE(240, ((WINDOW_HEIGHT / 2) - 64) + 48)
                         TTEXT("THIS IS A") TREL(-72, 8) TTEXT("WATER")
                             TREL(-40, 8) TTEXT("TEMPLE") TEND};

char loc_name_2[] = {TINK(RGBWHITE) TMOVE(240, ((WINDOW_HEIGHT / 2) - 64) + 48)
                         TTEXT("THIS IS A") TREL(-72, 8) TTEXT("SWAMP")
                             TREL(-40, 8) TTEXT("TEMPLE") TEND};

char loc_name_3[] = {TINK(RGBWHITE) TMOVE(240, ((WINDOW_HEIGHT / 2) - 64) + 48)
                         TTEXT("THIS IS A") TREL(-72, 8) TTEXT("FOREST")
                             TREL(-48, 8) TTEXT("TEMPLE") TEND};

char loc_name_4[] = {TINK(RGBWHITE) TMOVE(240, ((WINDOW_HEIGHT / 2) - 64) + 48)
                         TTEXT("THIS IS A") TREL(-72, 8) TTEXT("MOUNTAIN")
                             TREL(-64, 8) TTEXT("TEMPLE") TEND};

char loc_name_5[] = {TINK(RGBCYAN) TMOVE(240, ((WINDOW_HEIGHT / 2) - 64) + 48)
                         TTEXT("THIS IS A") TREL(-72, 8) TTEXT("WATER")
                             TREL(-40, 8) TTEXT("LOCATION") TEND};

char loc_name_6[] = {TINK(RGBYELLOW) TMOVE(240, ((WINDOW_HEIGHT / 2) - 64) + 48)
                         TTEXT("THIS IS A") TREL(-72, 8) TTEXT("SWAMP")
                             TREL(-40, 8) TTEXT("LOCATION") TEND};

char loc_name_7[] = {TINK(RGBGREEN) TMOVE(240, ((WINDOW_HEIGHT / 2) - 64) + 48)
                         TTEXT("THIS IS A") TREL(-72, 8) TTEXT("FOREST")
                             TREL(-48, 8) TTEXT("LOCATION") TEND};

char loc_name_8[] = {TINK(RGBWHITE) TMOVE(240, ((WINDOW_HEIGHT / 2) - 64) + 48)
                         TTEXT("THIS IS A") TREL(-72, 8) TTEXT("MOUNTAIN")
                             TREL(-64, 8) TTEXT("LOCATION") TEND};

char loc_name_9[] = {TINK(RGBYELLOW) TMOVE(240, ((WINDOW_HEIGHT / 2) - 64) + 48)
                         TTEXT("THIS IS A") TREL(-72, 8) TTEXT("PLAYER")
                             TREL(-48, 8) TTEXT("LOCATION") TEND};

char *location_names[] = {loc_name_0, loc_name_1, loc_name_2, loc_name_3,
                          loc_name_4, loc_name_5, loc_name_6, loc_name_7,
                          loc_name_8, loc_name_9, loc_name_9, loc_name_9,
                          loc_name_9, loc_name_9};

char txt_credits[] = {
    TINK(RGBWHITE) TMOVE(8, ((WINDOW_HEIGHT / 2) - 64) + 16)
        TTEXT("COPYRIGHT (C) 1989-2009 CHRIS HINSLEY") TMOVE(
            80, ((WINDOW_HEIGHT / 2) - 64) + 24) TTEXT("ALL RIGHTS RESERVED")
            TINK(RGBGREEN) TMOVE(64, ((WINDOW_HEIGHT / 2) - 64) +
                                         48) TTEXT("GAME DESIGN AND CONCEPT")
                TMOVE(112, ((WINDOW_HEIGHT / 2) - 64) + 64) TTEXT("PROGRAMMING")
                    TMOVE(120, ((WINDOW_HEIGHT / 2) - 64) + 80) TTEXT(
                        "GRAPHICS") TMOVE(96, ((WINDOW_HEIGHT / 2) - 64) + 96)
                        TTEXT("SOUND AND MUSIC") TINK(RGBYELLOW) TMOVE(
                            24, ((WINDOW_HEIGHT / 2) - 64) + 56)
                            TTEXT("CHRIS HINSLEY AND NIGEL BROWNJOHN") TMOVE(
                                104, ((WINDOW_HEIGHT / 2) - 64) +
                                         72) TTEXT("CHRIS HINSLEY")
                                TMOVE(24, ((WINDOW_HEIGHT / 2) - 64) + 88)
                                    TTEXT("CHRIS HINSLEY AND NIGEL BROWNJOHN")
                                        TMOVE(104,
                                              ((WINDOW_HEIGHT / 2) - 64) + 104)
                                            TTEXT("CHRIS HINSLEY") TEND};

char txt_lettab[] = {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K',
                     'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
                     'W', 'X', 'Y', 'Z', '(', ')', '.', ' ', '?', '[', ']'};

char txt_askname[] = {TINK(RGBWHITE) TMOVE(72, ((WINDOW_HEIGHT / 2) - 64) + 32)
                          TTEXT("PLEASE ENTER INITIALS")
                              TMOVE(136, ((WINDOW_HEIGHT / 2) - 64) + 48)
                                  TINK(RGBGREEN) TTEXT("(   )") TREL(-32, 0)
                                      TINK(RGBCYAN)
                                          TSTRING(GLOBALS_MAN_INITIALS) TEND};

char txt_nohiscore[] = {
    TINK(RGBWHITE) TMOVE(24, ((WINDOW_HEIGHT / 2) - 64) + 48)
        TTEXT("YOU HAVE NOT ATTAINED ENOUGH GLORY")
            TMOVE(56, ((WINDOW_HEIGHT / 2) - 64) + 56)
                TTEXT("TO ENTER THE HISTORY BOOKS")
                    TMOVE(80, ((WINDOW_HEIGHT / 2) - 64) + 72)
                        TTEXT("YOUR GLORY WAS ") TINK(RGBYELLOW)
                            TNUMBER(GLOBALS_MAN_SCORE) TEND};

char txt_intutor[] = {TINK(RGBWHITE) TMOVE(32, ((WINDOW_HEIGHT / 2) - 64) + 48)
                          TTEXT("TUTOR MODE WILL NEVER ENABLE YOU")
                              TMOVE(56, ((WINDOW_HEIGHT / 2) - 64) + 56)
                                  TTEXT("TO ENTER THE HISTORY BOOKS") TMOVE(
                                      80, ((WINDOW_HEIGHT / 2) - 64) + 72)
                                      TTEXT("YOUR GLORY WAS ") TINK(RGBYELLOW)
                                          TNUMBER(GLOBALS_MAN_SCORE) TEND};

char txt_scores_items[] = {
    TINK(RGBRED) TMOVE(104, ((WINDOW_HEIGHT / 2) - 64) + 16) TTEXT(
        "HALL OF GLORY") TINK(RGBGREEN) TMOVE(80,
                                              ((WINDOW_HEIGHT / 2) - 64) + 32)
        TSTRING(GLOBALS_SCORE_TXT[0]) TMOVE(
            80, ((WINDOW_HEIGHT / 2) - 64) + 40) TSTRING(GLOBALS_SCORE_TXT[1])
            TMOVE(80, ((WINDOW_HEIGHT / 2) - 64) + 48) TSTRING(
                GLOBALS_SCORE_TXT[2]) TMOVE(80, ((WINDOW_HEIGHT / 2) - 64) + 56)
                TSTRING(GLOBALS_SCORE_TXT[3]) TMOVE(
                    80, ((WINDOW_HEIGHT / 2) - 64) +
                            64) TSTRING(GLOBALS_SCORE_TXT[4])
                    TMOVE(80, ((WINDOW_HEIGHT / 2) - 64) + 72) TSTRING(
                        GLOBALS_SCORE_TXT
                            [5]) TMOVE(80, ((WINDOW_HEIGHT / 2) - 64) + 80)
                        TSTRING(GLOBALS_SCORE_TXT[6]) TMOVE(
                            80,
                            ((WINDOW_HEIGHT / 2) - 64) +
                                88) TSTRING(GLOBALS_SCORE_TXT[7])
                            TMOVE(80, ((WINDOW_HEIGHT / 2) - 64) + 96) TSTRING(
                                GLOBALS_SCORE_TXT
                                    [8]) TMOVE(80, ((WINDOW_HEIGHT / 2) - 64) + 104)
                                TSTRING(GLOBALS_SCORE_TXT[9]) TINK(RGBYELLOW) TMOVE(
                                    192,
                                    ((WINDOW_HEIGHT / 2) - 64) +
                                        32) TNUMBER(GLOBALS_SCORE[0])
                                    TMOVE(192, ((WINDOW_HEIGHT / 2) - 64) + 40) TNUMBER(
                                        GLOBALS_SCORE
                                            [1]) TMOVE(192, ((WINDOW_HEIGHT / 2) - 64) + 48)
                                        TNUMBER(GLOBALS_SCORE[2]) TMOVE(
                                            192,
                                            ((WINDOW_HEIGHT / 2) - 64) +
                                                56) TNUMBER(GLOBALS_SCORE[3])
                                            TMOVE(192,
                                                  ((WINDOW_HEIGHT / 2) - 64) +
                                                      64)
                                                TNUMBER(GLOBALS_SCORE[4]) TMOVE(
                                                    192,
                                                    ((WINDOW_HEIGHT / 2) - 64) +
                                                        72)
                                                    TNUMBER(GLOBALS_SCORE[5]) TMOVE(
                                                        192,
                                                        ((WINDOW_HEIGHT / 2) -
                                                         64) +
                                                            80)
                                                        TNUMBER(GLOBALS_SCORE[6]) TMOVE(
                                                            192,
                                                            ((WINDOW_HEIGHT /
                                                              2) -
                                                             64) +
                                                                88)
                                                            TNUMBER(GLOBALS_SCORE[7]) TMOVE(
                                                                192,
                                                                ((WINDOW_HEIGHT /
                                                                  2) -
                                                                 64) +
                                                                    96)
                                                                TNUMBER(
                                                                    GLOBALS_SCORE
                                                                        [8])
                                                                    TMOVE(
                                                                        192,
                                                                        ((WINDOW_HEIGHT /
                                                                          2) -
                                                                         64) +
                                                                            104)
                                                                        TNUMBER(
                                                                            GLOBALS_SCORE
                                                                                [9])
                                                                            TEND};

char txt_menu_items[] = {
    TMOVE(112, ((WINDOW_HEIGHT / 2) - 64) +
                   24) TIINK(GLOBALS_GAME_MENU[GLOBALS_GAME_MENU_START_COL])
        TTEXT("START GAME") TMOVE(112, ((WINDOW_HEIGHT / 2) - 64) + 40) TIINK(
            GLOBALS_GAME_MENU[GLOBALS_GAME_MENU_DEFINE_COL]) TTEXT("LOAD GAME")
            TMOVE(112, ((WINDOW_HEIGHT / 2) - 64) + 56) TIINK(
                GLOBALS_GAME_MENU[GLOBALS_GAME_MENU_DIFFICULTY_COL])
                TISTRING(GLOBALS_GAME_MENU_DIFFICULTY)
                    TMOVE(112, ((WINDOW_HEIGHT / 2) - 64) + 72) TIINK(
                        GLOBALS_GAME_MENU[GLOBALS_GAME_MENU_MODE_COL])
                        TISTRING(GLOBALS_GAME_MENU_MODE)
                            TMOVE(112, ((WINDOW_HEIGHT / 2) - 64) + 88) TIINK(
                                GLOBALS_GAME_MENU[GLOBALS_GAME_MENU_SOUND_COL])
                                TISTRING(GLOBALS_GAME_MENU_SOUND) TMOVE(
                                    112, ((WINDOW_HEIGHT / 2) - 64) + 104)
                                    TIINK(GLOBALS_GAME_MENU
                                              [GLOBALS_GAME_MENU_CREDITS_COL])
                                        TTEXT("GAME CREDITS") TEND};

char txt_menu_easy[] = {TTEXT("EASY MODE") TEND};

char txt_menu_hard[] = {TTEXT("HARD MODE") TEND};

char *txt_menu_dif[] = {txt_menu_easy, txt_menu_hard};

char txt_menu_tutor[] = {TTEXT("TUTOR MODE") TEND};

char txt_menu_assist[] = {TTEXT("ASSIST MODE") TEND};

char txt_menu_human[] = {TTEXT("HUMAN MODE") TEND};

char *txt_menu_mode[] = {txt_menu_tutor, txt_menu_assist, txt_menu_human};

char txt_menu_on[] = {TTEXT("SOUND ON") TEND};

char txt_menu_off[] = {TTEXT("SOUND OFF") TEND};

char *txt_menu_sound[] = {txt_menu_on, txt_menu_off};

char txt_map_player_headings[] = {
    TINK(RGBYELLOW) TMOVE(8, ((WINDOW_HEIGHT / 2) - 64) + 32) TTEXT("NAME")
        TMOVE(8, ((WINDOW_HEIGHT / 2) - 64) + 48) TTEXT("HOMELAND") TMOVE(
            8, ((WINDOW_HEIGHT / 2) - 64) +
                   64) TTEXT("CULT") TMOVE(8, ((WINDOW_HEIGHT / 2) - 64) + 80)
            TTEXT("STATUS") TMOVE(8, ((WINDOW_HEIGHT / 2) - 64) + 96) TTEXT(
                "TERRITORY") TMOVE(8, ((WINDOW_HEIGHT / 2) - 64) +
                                          112) TTEXT("GLORY") TINK(RGBDYELLOW)
                TMOVE(8, ((WINDOW_HEIGHT / 2) - 64) + 40) TSTRING(
                    GLOBALS_MAN_NAME) TMOVE(8, ((WINDOW_HEIGHT / 2) - 64) + 56)
                    TSTRING(GLOBALS_MAN_HOMELAND) TMOVE(
                        8, ((WINDOW_HEIGHT / 2) - 64) +
                               72) TISTRING(GLOBALS_MAN_CULT)
                        TMOVE(8, ((WINDOW_HEIGHT / 2) - 64) +
                                     88) TISTRING(GLOBALS_MAN_STATUS)
                            TMOVE(8, ((WINDOW_HEIGHT / 2) - 64) +
                                         104) TNUMBER(GLOBALS_MAN_TERRITORY)
                                TMOVE(8, ((WINDOW_HEIGHT / 2) - 64) + 120)
                                    TNUMBER(GLOBALS_MAN_SCORE) TINK(RGBMAGENTA)
                                        TMOVE(88, ((WINDOW_HEIGHT / 2) - 64) +
                                                      0)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8) TMOVE(224, ((WINDOW_HEIGHT / 2) - 64) + 0)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8)('Z' + 5),
    TREL(-8, 8) TEND};

char txt_map_enemy_headings[] = {
    TIINK(GLOBALS_ENEMY_INFOCOL) TMOVE(232, ((WINDOW_HEIGHT / 2) - 64) + 32)
        TTEXT("KINGDOM") TMOVE(232, ((WINDOW_HEIGHT / 2) - 64) + 48) TTEXT(
            "WIZARDLORD") TMOVE(232, ((WINDOW_HEIGHT / 2) - 64) + 64)
            TTEXT("CULT") TMOVE(232, ((WINDOW_HEIGHT / 2) - 64) + 80) TTEXT(
                "WARBAND") TMOVE(232, ((WINDOW_HEIGHT / 2) - 64) + 96)
                TTEXT("POPULARITY") TMOVE(232, ((WINDOW_HEIGHT / 2) - 64) + 112)
                    TTEXT("POPULATION") TIINK(GLOBALS_ENEMY_INFODCOL) TMOVE(
                        232, ((WINDOW_HEIGHT / 2) - 64) +
                                 40) TSTRING(GLOBALS_ENEMY_KINGDOM)
                        TMOVE(232, ((WINDOW_HEIGHT / 2) - 64) +
                                       56) TISTRING(GLOBALS_ENEMY_WIZARDLORD)
                            TMOVE(232, ((WINDOW_HEIGHT / 2) - 64) +
                                           72) TISTRING(GLOBALS_ENEMY_CULT)
                                TMOVE(232, ((WINDOW_HEIGHT / 2) - 64) + 88)
                                    TISTRING(GLOBALS_ENEMY_WARBAND) TMOVE(
                                        232, ((WINDOW_HEIGHT / 2) - 64) + 104)
                                        TNUMBER(GLOBALS_ENEMY_POPULARITY) TMOVE(
                                            232,
                                            ((WINDOW_HEIGHT / 2) - 64) + 120)
                                            TNUMBER(GLOBALS_ENEMY_POPULATION)
                                                TEND};

char hint_txt_0[] = {TMOVE(32, ((WINDOW_HEIGHT / 2) - 64) + 56)
                         TTEXT("TRY TO KEEP YOUR WEAPON TURNOVER")
                             TMOVE(80, ((WINDOW_HEIGHT / 2) - 64) + 64)
                                 TTEXT("AS HIGH AS POSSIBLE !") TEND};

char hint_txt_1[] = {TMOVE(40, ((WINDOW_HEIGHT / 2) - 64) + 56)
                         TTEXT("TRY GOING OVERGROUND IF FACED")
                             TMOVE(64, ((WINDOW_HEIGHT / 2) - 64) + 64)
                                 TTEXT("WITH A VERY STRONG ARMY !") TEND};

char hint_txt_2[] = {TMOVE(32, ((WINDOW_HEIGHT / 2) - 64) + 56)
                         TTEXT("ALWAYS HAVE AS MANY BLUE DEMONS")
                             TMOVE(56, ((WINDOW_HEIGHT / 2) - 64) + 64)
                                 TTEXT("FLYING AROUND AS POSSIBLE !") TEND};

char hint_txt_3[] = {TMOVE(50, ((WINDOW_HEIGHT / 2) - 64) + 56)
                         TTEXT("THE SMALL CROSSBOW IS THE BEST")
                             TMOVE(40, ((WINDOW_HEIGHT / 2) - 64) + 64)
                                 TTEXT("WEAPON WHEN FACED WITH MONKS !") TEND};

char hint_txt_4[] = {TMOVE(32, ((WINDOW_HEIGHT / 2) - 64) + 56)
                         TTEXT("WHITE SPELLS ARE VERY USEFULL IF")
                             TMOVE(48, ((WINDOW_HEIGHT / 2) - 64) + 64)
                                 TTEXT("SAVED FOR USE ON THE TOWER !") TEND};

char hint_txt_5[] = {TMOVE(32, ((WINDOW_HEIGHT / 2) - 64) + 56)
                         TTEXT("YELLOW SPELLS ARE MORE EFFECTIVE")
                             TMOVE(80, ((WINDOW_HEIGHT / 2) - 64) + 64)
                                 TTEXT("WHEN YOU ARE DUCKING !") TEND};

char hint_txt_6[] = {TMOVE(32, ((WINDOW_HEIGHT / 2) - 64) + 56)
                         TTEXT("MAXIMIZE YELLOW SPELL FIREBALLS")
                             TMOVE(72, ((WINDOW_HEIGHT / 2) - 64) + 64)
                                 TTEXT("BY RUNNING AFTER THEM !") TEND};

char hint_txt_7[] = {TMOVE(48, ((WINDOW_HEIGHT / 2) - 64) + 56)
                         TTEXT("MINES CAN BE USED AS WEAPONS")
                             TMOVE(48, ((WINDOW_HEIGHT / 2) - 64) + 64)
                                 TTEXT("IF YOU HAVE ENOUGH STRENGTH !") TEND};

char hint_txt_8[] = {TMOVE(40, ((WINDOW_HEIGHT / 2) - 64) + 56)
                         TTEXT("USE LEDGE TO LEFT OF TOWER TO")
                             TMOVE(40, ((WINDOW_HEIGHT / 2) - 64) + 64)
                                 TTEXT("REDUCE FLAG LEVEL ON WAY UP !") TEND};

char hint_txt_9[] = {TMOVE(48, ((WINDOW_HEIGHT / 2) - 64) + 56)
                         TTEXT("GOT A HELL OF A NERVE ASKING")
                             TMOVE(64, ((WINDOW_HEIGHT / 2) - 64) + 64)
                                 TTEXT("FOR HINTS AT THIS LEVEL !") TEND};

char hint_txt_10[] = {TMOVE(64, ((WINDOW_HEIGHT / 2) - 64) + 56)
                          TTEXT("YOU GET NO HINTS IN EASY")
                              TMOVE(88, ((WINDOW_HEIGHT / 2) - 64) + 64)
                                  TTEXT("MODE OR TUTOR MODE") TEND};

char *oracle_hints[] = {hint_txt_0, hint_txt_1, hint_txt_2, hint_txt_3,
                        hint_txt_4, hint_txt_5, hint_txt_6, hint_txt_7,
                        hint_txt_8, hint_txt_9};

// sprite class templates

class sp_man : public Sprite {
public:
  AT at;
  CP cp1;
  CP cp2;
  CP cp3;
  CL cl1;
  CL cl2;
  CL cl3;
  CL cl4;
  CL cl5;
  CP cp4;
  int stop;

  sp_man() {
    init(0, 0, 32, 32, sprite_draw, GLOBALS_FANATIC_L, FRM_WALK, 1, 0, FTP_MAN,
         0, dt_man, 0, 0, 0);
    at.init(0, at_man_upper_mase, sizeof(at_man_upper_mase));
    cp1.init(cp_manitem);
    cp2.init(cp_man);
    cp3.init(cp_manduck);
    cl1.init(GLOBALS_MISSILE_DLIST, -1, man_collision_handlers);
    cl2.init(GLOBALS_ENEMY_DLIST,
             (FTP_TOWER | FTP_HORSE | FTP_BOARRIDER | FTP_CARPET |
              FTP_SKELETON_HORSE),
             man_collision_handlers);
    cl3.init(GLOBALS_ENEMY_DLIST, (FTP_KNIGHT | FTP_SKELETON | FTP_MONK),
             man_collision_handlers);
    cl4.init(GLOBALS_ENEMY_DLIST, (FTP_FOOTMAN), man_collision_handlers);
    cl5.init(GLOBALS_BANS_DLIST, -1, man_collision_handlers);
    cp4.init(cp_mantrack);
    stop = 0;
  }
};

class sp_mind : public Sprite {
public:
  MV mv;
  CP cp;
  CL cl;
  int stop;

  sp_mind() {
    init(-64, -64, 32, 32, sprite_draw_mind, GLOBALS_FRM_32C32, FRM_FACES, 0, 0,
         0, 0, dt_blat_fx, 8, 0, 0);
    mv.init(0, 0, 0, 0, 8, 8);
    cp.init(cp_mind);
    cl.init(GLOBALS_MAN_DLIST, -1, mind_collision_handlers);
    stop = 0;
  }
};

class sp_mind_fire : public Sprite {
public:
  ML ml;
  AT at;
  CP cp;
  int stop;

  sp_mind_fire() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_MSHOT, 0, 0,
         FTP_ENEMY_FIRE, 200, dt_fizz_fx, 0, 0, 0);
    ml.init(5);
    at.init(1, at_mind_fire, sizeof(at_mind_fire));
    cp.init(cp_hand_fire);
    stop = 0;
  }
};

class sp_hand : public Sprite {
public:
  CP cp;
  CL cl1;
  CL cl2;
  int stop;

  sp_hand() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_MANMIND, 0, 0, 0,
         0, dt_exp_fx, 0, 0, 0);
    cp.init(cp_hand);
    cl1.init(GLOBALS_ITEM_DLIST, -1, mind_collision_handlers);
    cl2.init(GLOBALS_MISSILE_DLIST, -1, mind_collision_handlers);
    stop = 0;
  }
};

class sp_hand_fire : public Sprite {
public:
  ML ml;
  AT at;
  CP cp;
  int stop;

  sp_hand_fire() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_PSHOT, 0, 0,
         FTP_MAN, 1, dt_hand_fire, 0, 0, 0);
    ml.init(8);
    at.init(2, at_hand_fire, sizeof(at_hand_fire));
    cp.init(cp_hand_fire);
    stop = 0;
  }
};

class sp_manstance : public Sprite {
public:
  AT at;
  CP cp1;
  CP cp2;
  CP cp3;
  int stop;

  sp_manstance() {
    init(-64, -64, 32, 32, sprite_draw, GLOBALS_FANATIC_L, FRM_STANCE, 0, 0, 0,
         0, 0, 0, 0, 0);
    at.init(3, at_man_stance, sizeof(at_man_stance));
    cp1.init(cp_manstance);
    cp2.init(cp_manduck);
    cp3.init(cp_mantrack);
    stop = 0;
  }
};

class sp_manbits : public Sprite {
public:
  MV mv;
  CP cp1;
  CP cp2;
  int stop;

  sp_manbits() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_MANBITS, 0, 0, 0,
         0, dt_fizz_fx, 0, 0, 0);
    mv.init(0, 0, 0, 1, 3, 10);
    cp1.init(cp_offscreen_no_death);
    cp2.init(cp_bounce);
    stop = 0;
  }
};

class sp_banner : public Sprite {
public:
  CP cp;
  int stop;

  sp_banner() {
    init(-64, -64, 16, 48, sprite_draw_banner, GLOBALS_FRM_16X16, FRM_BANNERS,
         0, 0, FTP_MAN_BANNER, 0, dt_fizz_fx, 0, 0, 0);
    cp.init(cp_banner);
    stop = 0;
  }
};

class sp_mase : public Sprite {
public:
  MT mt;
  AT at;
  CL cl1;
  CL cl2;
  int stop;

  sp_mase() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16_L, FRM_MASE, -1, 0, 0,
         1, 0, 0, 0, 0);
    mt.init(1, mt_upper_mase_l, sizeof(mt_upper_mase_l));
    at.init(2, at_upper_mase, sizeof(at_upper_mase));
    cl1.init(GLOBALS_ENEMY_DLIST, -1, man_addon_collision_handlers);
    cl2.init(GLOBALS_MISSILE_DLIST, -1, man_addon_collision_handlers);
    stop = 0;
  }
};

class sp_big_crossbow : public Sprite {
public:
  MT mt;
  AT at;
  CP cp;
  int stop;

  sp_big_crossbow() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16_L, FRM_CBOW, -1, 0, 0,
         0, 0, 0, 0, 0);
    mt.init(1, mt_big_crossbow_l, sizeof(mt_big_crossbow_l));
    at.init(2, at_big_crossbow, sizeof(at_big_crossbow));
    cp.init(cp_big_crossbow);
    stop = 0;
  }
};

class sp_big_arrow : public Sprite {
public:
  MV mv;
  CP cp;
  CL cl1;
  CL cl2;
  int stop;

  sp_big_arrow() {
    init(-64, -64, 32, 16, sprite_draw, GLOBALS_FRM_32X16_L, FRM_BARROW, -1, 0,
         0, 4, 0, 0, 0, 0);
    mv.init(0, 0, 0, 0, 10, 0);
    cp.init(cp_offscreen_no_death);
    cl1.init(GLOBALS_ENEMY_DLIST, -1, man_missile_collision_handlers);
    cl2.init(GLOBALS_MISSILE_DLIST, -1, man_missile_collision_handlers);
    stop = 0;
  }
};

class sp_small_crossbow : public Sprite {
public:
  MT mt;
  AT at;
  CP cp;
  int stop;

  sp_small_crossbow() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16_L, FRM_HBOW, -1, 0, 0,
         0, 0, 0, 0, 0);
    mt.init(1, mt_small_crossbow_l, sizeof(mt_small_crossbow_l));
    at.init(1, at_small_crossbow, sizeof(at_small_crossbow));
    cp.init(cp_small_crossbow);
    stop = 0;
  }
};

class sp_small_arrow : public Sprite {
public:
  MV mv;
  CP cp;
  CL cl1;
  CL cl2;
  int stop;

  sp_small_arrow() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16_L, FRM_ARROW, -1, 0,
         0, 1, 0, 0, 0, 0);
    mv.init(0, 0, 0, 0, 16, 0);
    cp.init(cp_offscreen_no_death);
    cl1.init(GLOBALS_ENEMY_DLIST, -1, man_missile_collision_handlers);
    cl2.init(GLOBALS_MISSILE_DLIST, -1, man_missile_collision_handlers);
    stop = 0;
  }
};

class sp_naptha : public Sprite {
public:
  MT mt;
  AT at;
  CP cp;
  int stop;

  sp_naptha() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16_L, FRM_NAPTH, -1, 0,
         0, 0, 0, 0, 0, 0);
    mt.init(1, mt_naptha_l, sizeof(mt_naptha_l));
    at.init(3, at_naptha, sizeof(at_naptha));
    cp.init(cp_naptha);
    stop = 0;
  }
};

class sp_nbomb : public Sprite {
public:
  MV mv;
  AT at;
  CP cp;
  int stop;

  sp_nbomb() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_NBOMB, 0, 0, 0,
         0, dt_bomb_fx, 0, 0, 0);
    mv.init(0, -4, 0, 1, 10, 8);
    at.init(1, at_nbomb, sizeof(at_nbomb));
    cp.init(cp_nbomb);
    stop = 0;
  }
};

class sp_elemental : public Sprite {
public:
  AT at;
  int stop;

  sp_elemental() {
    init(-64, -64, 32, 32, sprite_draw, GLOBALS_FRM_32C32, FRM_ELEMENTAL, 0, 0,
         0, 0, dt_elemental_explode, 0, 0, 0);
    at.init(3, at_elemental, sizeof(at_elemental));
    stop = 0;
  }
};

class sp_frag : public Sprite {
public:
  MV mv;
  AT at;
  CP cp;
  CL cl1;
  CL cl2;
  int stop;

  sp_frag() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_FIRES, 0, 0, 0,
         1000, 0, 0, 0, 0);
    mv.init(0, 0, 0, 0, 8, 8);
    at.init(1, at_frag, sizeof(at_frag));
    cp.init(cp_offscreen_no_death);
    cl1.init(GLOBALS_ENEMY_DLIST, -1, man_missile_collision_handlers);
    cl2.init(GLOBALS_MISSILE_DLIST, -1, man_missile_collision_handlers);
    stop = 0;
  }
};

class sp_helper : public Sprite {
public:
  MV mv;
  AT at;
  CP cp;
  CL cl1;
  CL cl2;
  int stop;

  sp_helper() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_DEMON, 0, 0, 0,
         10, dt_fizz_fx, 128, 0, 0);
    mv.init(0, 0, 0, 0, 8, 8);
    at.init(6, at_helper, sizeof(at_helper));
    cp.init(cp_helper);
    cl1.init(GLOBALS_ENEMY_DLIST, -1, man_missile_collision_handlers);
    cl2.init(GLOBALS_MISSILE_DLIST, -1, man_missile_collision_handlers);
    stop = 0;
  }
};

class sp_super_helper : public Sprite {
public:
  MV mv;
  AT at;
  CP cp;
  CL cl1;
  CL cl2;
  int stop;

  sp_super_helper() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_DEMON + 2, 0, 0,
         0, 20, dt_super_helper_transform, 32, 0, 0);
    mv.init(0, 0, 0, 0, 8, 8);
    at.init(6, at_super_helper, sizeof(at_super_helper));
    cp.init(cp_helper);
    cl1.init(GLOBALS_ENEMY_DLIST, -1, man_missile_collision_handlers);
    cl2.init(GLOBALS_MISSILE_DLIST, -1, man_missile_collision_handlers);
    stop = 0;
  }
};

class sp_fizz : public Sprite {
public:
  AT at;
  CP cp;
  int stop;

  sp_fizz() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_FIZZ, 0, 0, 0, 0,
         0, 0, 0, 0);
    at.init(1, at_fizz, sizeof(at_fizz));
    cp.init(cp_offscreen);
    stop = 0;
  }
};

class sp_blood : public Sprite {
public:
  MV mv;
  AT at;
  CP cp;
  int stop;

  sp_blood() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16_L, FRM_BLOOD, -1, 0,
         0, 0, 0, 0, 0, 0);
    mv.init(0, -4, 0, 1, 100, 100);
    at.init(2, at_blood, sizeof(at_blood));
    cp.init(cp_offscreen);
    stop = 0;
  }
};

class sp_small_explosion : public Sprite {
public:
  AT at;
  int stop;

  sp_small_explosion() {
    init(-64, -64, 32, 32, sprite_draw, GLOBALS_FRM_32C32, FRM_BIGEXP, 0, 0, 0,
         0, 0, 0, 0, 0);
    at.init(2, at_small_explosion, sizeof(at_small_explosion));
    stop = 0;
  }
};

class sp_mine : public Sprite {
public:
  CP cp;
  AT at;
  int stop;

  sp_mine() {
    init(-64, -64, 16, 8, sprite_draw, GLOBALS_FRM_16X16, FRM_MINE, 0, 0,
         FTP_ENEMY_FIRE, 150, dt_mine_explode, 0, 0, 0);
    cp.init(cp_mine);
    at.init(1, at_mine, sizeof(at_mine));
    stop = 0;
  }
};

class sp_enemy : public Sprite {
public:
  MV mv;
  AT at;
  CP cp;

  sp_enemy() { cp.init(cp_enemy_offscreen); }
};

class sp_horse : public sp_enemy {
public:
  CP cp2;
  int stop;

  sp_horse() {
    init(-64, -64, 64, 64, sprite_draw, GLOBALS_FRM_64X64_L, FRM_HORSE, -1, 0,
         FTP_HORSE, 6, dt_kill_horse, 4, FRM_HORSE + 4, FRM_HORSE + 5);
    mv.init(0, 0, 0, 0, 8, 8);
    at.init(1, at_horse, sizeof(at_horse));
    cp2.init(cp_enemy_fall);
    stop = 0;
  }
};

class sp_skeleton_horse : public sp_enemy {
public:
  CP cp2;
  int stop;

  sp_skeleton_horse() {
    init(-64, -64, 64, 64, sprite_draw, GLOBALS_FRM_64X64_L, FRM_SKELETON_HORSE,
         -1, 0, FTP_SKELETON_HORSE, 6, dt_kill_skeleton_horse, 4,
         FRM_SKELETON_HORSE + 4, 0);
    mv.init(0, 0, 0, 0, 8, 8);
    at.init(1, at_skeleton_horse, sizeof(at_skeleton_horse));
    cp2.init(cp_enemy_fall);
    stop = 0;
  }
};

class sp_spearman : public sp_enemy {
public:
  CP cp2;
  CP cp3;
  CP cp4;
  int stop;

  sp_spearman() {
    init(-64, -64, 32, 32, sprite_draw, GLOBALS_FRM_32X32_L, FRM_SPEARMAN, -1,
         0, FTP_SPEARMAN, 1, dt_kill_enemy, 0x60001, FRM_SPEARMAN + 10,
         FRM_SPEARMAN + 9);
    mv.init(0, 0, 0, 0, 6, 8);
    at.init(1, at_spearman, sizeof(at_spearman));
    cp2.init(cp_enemy_fall_duck);
    cp3.init(cp_spearman);
    cp4.init(cp_enemy_duck);
    stop = 0;
  }
};

class sp_wizard : public sp_enemy {
public:
  CP cp2;
  CP cp3;
  CP cp4;
  int stop;

  sp_wizard() {
    init(-64, -64, 32, 32, sprite_draw, GLOBALS_FRM_32X32_L, FRM_WIZARD, -1, 0,
         FTP_WIZARD, 2, dt_kill_enemy, 0x60001, FRM_WIZARD + 10,
         FRM_WIZARD + 9);
    mv.init(0, 0, 0, 0, 6, 8);
    at.init(1, at_wizard, sizeof(at_wizard));
    cp2.init(cp_enemy_fall_duck);
    cp3.init(cp_wizard);
    cp4.init(cp_enemy_duck);
    stop = 0;
  }
};

class sp_footman : public sp_enemy {
public:
  CP cp2;
  CP cp3;
  CP cp4;
  int stop;

  sp_footman() {
    init(-64, -64, 32, 32, sprite_draw, GLOBALS_FRM_32X32_L, FRM_FOOTMAN, -1, 0,
         FTP_FOOTMAN, 3, dt_kill_enemy, 1, FRM_FOOTMAN + 12, FRM_FOOTMAN + 11);
    mv.init(0, 0, 0, 0, 4, 8);
    at.init(1, at_footman, sizeof(at_footman));
    cp2.init(cp_enemy_fall_duck);
    cp3.init(cp_footman);
    cp4.init(cp_enemy_duck);
    stop = 0;
  }
};

class sp_footman_mase : public Sprite {
public:
  MT mt;
  AT at;
  CL cl;
  int stop;

  sp_footman_mase() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16_L, FRM_FOOTADD, -1, 0,
         0, 4, 0, 0, 0, 0);
    mt.init(1, mt_footman_upper_mase_l, sizeof(mt_footman_upper_mase_l));
    at.init(2, at_footman_upper_mase, sizeof(at_footman_upper_mase));
    cl.init(GLOBALS_MAN_DLIST, FTP_MAN, man_collision_handlers);
    stop = 0;
  }
};

class sp_knight : public sp_enemy {
public:
  CP cp2;
  CP cp3;
  CP cp4;
  int stop;

  sp_knight() {
    init(-64, -64, 32, 32, sprite_draw, GLOBALS_FRM_32X32_L, FRM_KNIGHT, -1, 0,
         FTP_KNIGHT, 5, dt_kill_enemy, 1, FRM_KNIGHT + 12, FRM_KNIGHT + 11);
    mv.init(0, 0, 0, 0, 4, 8);
    at.init(1, at_knight, sizeof(at_knight));
    cp2.init(cp_enemy_fall_duck);
    cp3.init(cp_knight);
    cp4.init(cp_enemy_duck);
    stop = 0;
  }
};

class sp_knight_mase : public Sprite {
public:
  MT mt;
  AT at;
  CL cl;
  int stop;

  sp_knight_mase() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16_L, FRM_KNIGHTADD, -1,
         0, 0, 8, 0, 0, 0, 0);
    mt.init(1, mt_knight_mase_l, sizeof(mt_knight_mase_l));
    at.init(2, at_knight_upper_mase, sizeof(at_knight_upper_mase));
    cl.init(GLOBALS_MAN_DLIST, FTP_MAN, man_collision_handlers);
    stop = 0;
  }
};

class sp_monk : public sp_enemy {
public:
  CP cp2;
  int stop;

  sp_monk() {
    init(-64, -64, 32, 32, sprite_draw, GLOBALS_FRM_32C32_L, FRM_MONK, -1, 0,
         FTP_MONK, 1, dt_kill_monk, 1, FRM_MONK + 2, 0);
    mv.init(0, 0, 0, 0, 3, 8);
    at.init(1, at_monk, sizeof(at_monk));
    cp2.init(cp_enemy_fall);
    stop = 0;
  }
};

class sp_skeleton_monk : public sp_enemy {
public:
  CP cp2;
  CP cp3;
  int stop;

  sp_skeleton_monk() {
    init(-64, -64, 32, 32, sprite_draw, GLOBALS_FANATIC_L, FRM_SKELETON_MONK,
         -1, 0, FTP_SKELETON_MONK, 8, dt_kill_zap, 1, FRM_SKELETON_MONK + 2,
         16);
    mv.init(0, 0, 0, 0, 3, 8);
    at.init(1, at_skeleton_monk, sizeof(at_skeleton_monk));
    cp2.init(cp_enemy_fall);
    cp3.init(cp_skeleton_monk);
    stop = 0;
  }
};

class sp_skeleton : public sp_enemy {
public:
  CP cp2;
  CP cp3;
  CP cp4;
  int stop;

  sp_skeleton() {
    init(-64, -64, 32, 32, sprite_draw, GLOBALS_FANATIC_L, FRM_SKELETON, -1, 0,
         FTP_SKELETON, 5, dt_kill_skeleton, 1, FRM_SKELETON + 11,
         FRM_SKELETON + 11);
    mv.init(0, 0, 0, 0, 4, 8);
    at.init(1, at_skeleton, sizeof(at_skeleton));
    cp2.init(cp_enemy_fall);
    cp3.init(cp_skeleton);
    cp4.init(cp_enemy_duck);
    stop = 0;
  }
};

class sp_skeleton_mase : public Sprite {
public:
  MT mt;
  AT at;
  CL cl;
  int stop;

  sp_skeleton_mase() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16_L, FRM_SKELADD, -1, 0,
         0, 8, 0, 0, 0, 0);
    mt.init(1, mt_skeleton_mase_l, sizeof(mt_skeleton_mase_l));
    at.init(2, at_skeleton_upper_mase, sizeof(at_skeleton_upper_mase));
    cl.init(GLOBALS_MAN_DLIST, FTP_MAN, man_collision_handlers);
    stop = 0;
  }
};

class sp_balista : public Sprite {
public:
  AT at;
  CP cp1;
  CP cp2;
  int stop;

  sp_balista() {
    init(-64, -64, 32, 32, sprite_draw, GLOBALS_FRM_32C32_L, FRM_BALISTA, -1, 0,
         FTP_BALISTA, 4, dt_kill_balista, 1, 0, FRM_BALISTA + 3);
    at.init(10, at_balista, sizeof(at_balista));
    cp1.init(cp_enemy_offscreen);
    cp2.init(cp_balista);
    stop = 0;
  }
};

class sp_balista_arrow : public Sprite {
public:
  MV mv;
  CP cp;
  int stop;

  sp_balista_arrow() {
    init(-64, -64, 32, 16, sprite_draw, GLOBALS_FRM_32X16_L, FRM_BARROW, -1, 0,
         FTP_ENEMY_FIRE, 8, 0, 0, 0, 0);
    mv.init(0, 0, 0, 0, 16, 0);
    cp.init(cp_offscreen);
    stop = 0;
  }
};

class sp_oil : public Sprite {
public:
  AT at;
  CP cp1;
  CP cp2;
  int stop;

  sp_oil() {
    init(-64, -64, 32, 32, sprite_draw, GLOBALS_FRM_32C32_L, FRM_OIL, -1, 0,
         FTP_OIL, 4, dt_kill_balista, 1, 0, FRM_OIL + 3);
    at.init(4, at_oil, sizeof(at_oil));
    cp1.init(cp_enemy_offscreen);
    cp2.init(cp_oil);
    stop = 0;
  }
};

class sp_oil_drop : public Sprite {
public:
  MV mv;
  CP cp;
  int stop;

  sp_oil_drop() {
    init(-64, -64, 16, 32, sprite_draw, GLOBALS_FRM_16X32, FRM_OILSHOT, -1, 0,
         FTP_ENEMY_FIRE, 48, dt_exp_fx, 0, 0, 0);
    mv.init(0, 0, 0, 1, 0, 10);
    cp.init(cp_offscreen_no_death);
    stop = 0;
  }
};

class sp_cannon : public Sprite {
public:
  AT at;
  CP cp1;
  CP cp2;
  int stop;

  sp_cannon() {
    init(-64, -64, 32, 32, sprite_draw, GLOBALS_FRM_32C32_L, FRM_CANNON, -1, 0,
         FTP_CANNON, 4, dt_kill_balista, 1, 0, FRM_CANNON + 3);
    at.init(4, at_cannon, sizeof(at_cannon));
    cp1.init(cp_enemy_offscreen);
    cp2.init(cp_cannon);
    stop = 0;
  }
};

class sp_cannon_flame : public Sprite {
public:
  AT at;
  CP cp;
  int stop;

  sp_cannon_flame() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16_L, FRM_FLAME, -1, 0,
         0, 0, 0, 0, 0, 0);
    at.init(4, at_cannon_flame, sizeof(at_cannon_flame));
    cp.init(cp_offscreen);
    stop = 0;
  }
};

class sp_cannon_ball : public Sprite {
public:
  MV mv;
  CP cp;
  int stop;

  sp_cannon_ball() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_CBALL, -1, 0,
         FTP_ENEMY_FIRE, 24, dt_exp_fx, 0, 0, 0);
    mv.init(0, 0, 0, 0, 12, 0);
    cp.init(cp_offscreen_no_death);
    stop = 0;
  }
};

class sp_beserk : public Sprite {
public:
  AT at;
  CP cp1;
  CP cp2;
  CP cp3;
  int stop;

  sp_beserk() {
    init(-64, -64, 32, 32, sprite_draw, GLOBALS_FRM_32C32_L, FRM_BESERK, -1, 0,
         FTP_BESERK, 20, dt_kill_zap, 1, FRM_BESERK + 2, 0);
    at.init(3, at_beserk, sizeof(at_beserk));
    cp1.init(cp_enemy_offscreen);
    cp2.init(cp_enemy_duck);
    cp3.init(cp_beserk);
    stop = 0;
  }
};

class sp_beserk_mase : public Sprite {
public:
  MT mt;
  AT at;
  CL cl;
  int stop;

  sp_beserk_mase() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16_L, FRM_BKADDON, -1, 0,
         0, 8, 0, 0, 0, 0);
    mt.init(1, mt_beserk_upper_mase_l, sizeof(mt_beserk_upper_mase_l));
    at.init(3, at_beserk_upper_mase, sizeof(at_beserk_upper_mase));
    cl.init(GLOBALS_MAN_DLIST, FTP_MAN, man_collision_handlers);
    stop = 0;
  }
};

class sp_tower : public sp_enemy {
public:
  CP cp2;
  int stop;

  sp_tower() {
    init(-64, -64, 64, 64, sprite_draw_noflip, GLOBALS_FRM_64X64, FRM_TOWER, -1,
         0, FTP_TOWER, 8, dt_kill_tower, 4, 0, 0);
    mv.init(0, 0, 0, 0, 6, 0);
    at.init(2, at_tower, sizeof(at_tower));
    cp2.init(cp_tower);
    stop = 0;
  }
};

class sp_carpet : public sp_enemy {
public:
  MT mt;
  int stop;

  sp_carpet() {
    init(-64, -64, 64, 32, sprite_draw, GLOBALS_FRM_64X32_L, FRM_CARPET, -1, 0,
         FTP_CARPET, 4, dt_kill_carpet, 2, FRM_CARPET, 0);
    mv.init(0, 0, 0, 0, 6, 0);
    at.init(2, at_carpet, sizeof(at_carpet));
    mt.init(1, mt_carpet, sizeof(mt_carpet));
    stop = 0;
  }
};

class sp_boarrider : public sp_enemy {
public:
  CP cp2;
  int stop;

  sp_boarrider() {
    init(-64, -64, 64, 32, sprite_draw, GLOBALS_FRM_64X32_L, FRM_BOARRIDER, -1,
         0, FTP_BOARRIDER, 2, dt_kill_boarrider, 2, FRM_BOARRIDER + 4,
         FRM_BOARRIDER + 5);
    mv.init(0, 0, 0, 0, 6, 10);
    at.init(1, at_boarrider, sizeof(at_boarrider));
    cp2.init(cp_enemy_fall);
    stop = 0;
  }
};

class sp_body : public Sprite {
public:
  MV mv;
  CP cp;
  int stop;

  sp_body() {
    init(-64, -64, 32, 32, sprite_draw, GLOBALS_FRM_32X32_L, 0, -1, 0, 0, 0, 0,
         0, 0, 0);
    mv.init(0, -6, 0, 1, 100, 100);
    cp.init(cp_offscreen);
    stop = 0;
  }
};

class sp_bone : public Sprite {
public:
  MV mv;
  CP cp1;
  AT at;
  CP cp2;
  int stop;

  sp_bone() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_BONES, -1, 0, 0,
         0, dt_fizz_fx, 20, 0, 0);
    mv.init(0, 0, 0, 1, 3, 10);
    cp1.init(cp_offscreen_no_death);
    at.init(1, at_bone, sizeof(at_bone));
    cp2.init(cp_bone);
    stop = 0;
  }
};

class sp_skeleton_item : public Sprite {
public:
  MV mv;
  CP cp1;
  CP cp2;
  AT at;
  int stop;

  sp_skeleton_item() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_BONES + 3, 0, 0,
         0, 0, dt_skeleton_item, 0, 0, 24);
    mv.init(0, -9, 0, 1, 0, 10);
    cp1.init(cp_offscreen_no_death);
    cp2.init(cp_item);
    at.init(1, at_skeleton_item, sizeof(at_skeleton_item));
    stop = 0;
  }
};

class sp_spear : public Sprite {
public:
  MV mv;
  CP cp;
  int stop;

  sp_spear() {
    init(-64, -64, 32, 16, sprite_draw, GLOBALS_FRM_32X16_L, FRM_SPEAR, -1, 0,
         FTP_ENEMY_FIRE, 4, 0, 0, 0, 0);
    mv.init(0, 0, 0, 0, 12, 0);
    cp.init(cp_offscreen);
    stop = 0;
  }
};

class sp_wizard_shot : public Sprite {
public:
  MV mv;
  AT at;
  CP cp;
  int stop;

  sp_wizard_shot() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_WIZSHOT, -1, 0,
         FTP_ENEMY_FIRE, 16, dt_fizz_fx, 0, 0, 0);
    mv.init(0, 0, 0, 0, 8, 0);
    at.init(1, at_wizard_shot_strength, sizeof(at_wizard_shot_strength));
    cp.init(cp_offscreen_no_death);
    stop = 0;
  }
};

class sp_smith : public Sprite {
public:
  AT at;
  int stop;

  sp_smith() {
    init(-64, -64, 32, 32, sprite_draw, GLOBALS_FRM_32C32, FRM_SMITH, 0, 0, 0,
         0, 0, 0, 0, 0);
    at.init(4, at_smith, sizeof(at_smith));
    stop = 0;
  }
};

class sp_letter : public Sprite {
public:
  ML ml;
  CP cp;
  int stop;

  sp_letter() {
    init(-64, -64, 32, 32, sprite_draw, GLOBALS_FRM_32C32, 0, 0, 0, 0, 0, 0, 0,
         0, 0);
    ml.init(32);
    cp.init(cp_letter);
    stop = 0;
  }
};

class sp_sword : public Sprite {
public:
  ML ml;
  CP cp;
  int stop;

  sp_sword() {
    init(152, -64, 16, 64, sprite_draw, GLOBALS_FRM_16X64, FRM_SWORD, 0, 0, 0,
         0, 0, 0, 0, 0);
    ml.init(24);
    cp.init(cp_sword);
    stop = 0;
  }
};

class sp_drip : public Sprite {
public:
  MV mv;
  CP cp;
  int stop;

  sp_drip() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_BLOODDRIP, 0, 0,
         0, 0, dt_drip, 0, 0, 0);
    mv.init(0, 0, 0, 1, 0, 10);
    cp.init(cp_offscreen);
    stop = 0;
  }
};

class sp_mase_item : public Sprite {
public:
  MV mv;
  CP cp1;
  CP cp2;
  int stop;

  sp_mase_item() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_SHIELD, 0, 0,
         FTP_ITEM, 48, dt_fizz_fx, 1, 0, 96);
    mv.init(0, -9, 0, 1, 0, 10);
    cp1.init(cp_offscreen_no_death);
    cp2.init(cp_item);
    stop = 0;
  }
};

class sp_big_bow_item : public Sprite {
public:
  MV mv;
  CP cp1;
  CP cp2;
  int stop;

  sp_big_bow_item() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_SHIELD + 1, 0, 0,
         FTP_ITEM, 8, dt_fizz_fx, 2, 0, 64);
    mv.init(0, -9, 0, 1, 0, 10);
    cp1.init(cp_offscreen_no_death);
    cp2.init(cp_item);
    stop = 0;
  }
};

class sp_small_bow_item : public Sprite {
public:
  MV mv;
  CP cp1;
  CP cp2;
  int stop;

  sp_small_bow_item() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_SHIELD + 2, 0, 0,
         FTP_ITEM, 16, dt_fizz_fx, 1, 0, 64);
    mv.init(0, -9, 0, 1, 0, 10);
    cp1.init(cp_offscreen_no_death);
    cp2.init(cp_item);
    stop = 0;
  }
};

class sp_naptha_item : public Sprite {
public:
  MV mv;
  CP cp1;
  CP cp2;
  int stop;

  sp_naptha_item() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_SHIELD + 3, 0, 0,
         FTP_ITEM, 6, dt_fizz_fx, 2, 0, 48);
    mv.init(0, -9, 0, 1, 0, 10);
    cp1.init(cp_offscreen_no_death);
    cp2.init(cp_item);
    stop = 0;
  }
};

class sp_helper_item : public Sprite {
public:
  MV mv;
  CP cp1;
  CP cp2;
  int stop;

  sp_helper_item() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_SHIELD + 4, 0, 0,
         FTP_ITEM, 4, dt_fizz_fx, 3, 0, 48);
    mv.init(0, -9, 0, 1, 0, 10);
    cp1.init(cp_offscreen_no_death);
    cp2.init(cp_item);
    stop = 0;
  }
};

class sp_super_mase_item : public Sprite {
public:
  MV mv;
  CP cp1;
  CP cp2;
  int stop;

  sp_super_mase_item() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_SHIELD + 5, 0, 0,
         FTP_ITEM, 8, dt_fizz_fx, 2, 0, 16);
    mv.init(0, -9, 0, 1, 0, 10);
    cp1.init(cp_offscreen_no_death);
    cp2.init(cp_item);
    stop = 0;
  }
};

class sp_super_helper_item : public Sprite {
public:
  MV mv;
  CP cp1;
  CP cp2;
  int stop;

  sp_super_helper_item() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_SHIELD + 6, 0, 0,
         FTP_ITEM, 3, dt_fizz_fx, 4, 0, 8);
    mv.init(0, -9, 0, 1, 0, 10);
    cp1.init(cp_offscreen_no_death);
    cp2.init(cp_item);
    stop = 0;
  }
};

class sp_yellow_spell_item : public Sprite {
public:
  MV mv;
  CP cp1;
  CP cp2;
  int stop;

  sp_yellow_spell_item() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_SPELLS, 0, 0,
         FTP_ITEM, 1, dt_fizz_fx, 0, 0, 16);
    mv.init(0, -9, 0, 1, 0, 10);
    cp1.init(cp_offscreen_no_death);
    cp2.init(cp_item);
    stop = 0;
  }
};

class sp_black_spell_item : public Sprite {
public:
  MV mv;
  CP cp1;
  CP cp2;
  int stop;

  sp_black_spell_item() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_SPELLS + 1, 0, 0,
         FTP_ITEM, 1, dt_fizz_fx, 0, 0, 8);
    mv.init(0, -9, 0, 1, 0, 10);
    cp1.init(cp_offscreen_no_death);
    cp2.init(cp_item);
    stop = 0;
  }
};

class sp_green_spell_item : public Sprite {
public:
  MV mv;
  CP cp1;
  CP cp2;
  int stop;

  sp_green_spell_item() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_SPELLS + 2, 0, 0,
         FTP_ITEM, 1, dt_fizz_fx, 0, 0, 16);
    mv.init(0, -9, 0, 1, 0, 10);
    cp1.init(cp_offscreen_no_death);
    cp2.init(cp_item);
    stop = 0;
  }
};

class sp_white_spell_item : public Sprite {
public:
  MV mv;
  CP cp1;
  CP cp2;
  int stop;

  sp_white_spell_item() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_SPELLS + 3, 0, 0,
         FTP_ITEM, 1, dt_fizz_fx, 0, 0, 32);
    mv.init(0, -9, 0, 1, 0, 10);
    cp1.init(cp_offscreen_no_death);
    cp2.init(cp_item);
    stop = 0;
  }
};

class sp_red_spell_item : public Sprite {
public:
  MV mv;
  CP cp1;
  CP cp2;
  int stop;

  sp_red_spell_item() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_SPELLS + 4, 0, 0,
         FTP_ITEM, 1, dt_fizz_fx, 0, 0, 32);
    mv.init(0, -9, 0, 1, 0, 10);
    cp1.init(cp_offscreen_no_death);
    cp2.init(cp_item);
    stop = 0;
  }
};

class sp_blue_spell_item : public Sprite {
public:
  MV mv;
  CP cp1;
  CP cp2;
  int stop;

  sp_blue_spell_item() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_SPELLS + 5, 0, 0,
         FTP_ITEM, 1, dt_fizz_fx, 0, 0, 32);
    mv.init(0, -9, 0, 1, 0, 10);
    cp1.init(cp_offscreen_no_death);
    cp2.init(cp_item);
    stop = 0;
  }
};

class sp_gold_item : public Sprite {
public:
  MV mv;
  CP cp1;
  CP cp2;
  int stop;

  sp_gold_item() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_GOLD, 0, 0,
         FTP_ITEM, 1, dt_fizz_fx, 0, 0, 64);
    mv.init(0, -9, 0, 1, 0, 10);
    cp1.init(cp_offscreen_no_death);
    cp2.init(cp_item);
    stop = 0;
  }
};

class sp_talisman_item : public Sprite {
public:
  int stop;

  sp_talisman_item() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_DIAMOND, 0, 0,
         FTP_ITEM, 3, dt_fizz_fx, 0, 0, 32);
    stop = 0;
  }
};

class sp_string_object : public Sprite {
public:
  int stop;

  sp_string_object() {
    init(0, 0, SCREEN_WIDTH, 128, text_draw, GLOBALS_ASCII, 0, 0, 0, 0, 0, 0, 0,
         0, 0);
    stop = 0;
  }
};

class sp_enter_object : public Sprite {
public:
  int stop;

  sp_enter_object() {
    init(0, 0, SCREEN_WIDTH, 128, sprite_draw_enter, GLOBALS_ASCII, 0, 0, 0, 0,
         0, 0, 0, 0, 0);
    stop = 0;
  }
};

class sp_map : public Sprite {
public:
  int stop;

  sp_map() {
    init(-128, -128, 128, 128, sprite_draw_map, GLOBALS_CAMPAIN, 0, 0, 0, 0, 0,
         0, 0, 0, 0);
    stop = 0;
  }
};

class sp_sight : public Sprite {
public:
  int stop;

  sp_sight() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, FRM_SIGHT, 0, 0, 0,
         0, 0, 0, 0, 0);
    stop = 0;
  }
};

class sp_16x16_object : public Sprite {
public:
  int stop;

  sp_16x16_object() {
    init(-64, -64, 16, 16, sprite_draw, GLOBALS_FRM_16X16, 0, 0, 0, 0, 0, 0, 0,
         0, 0);
    stop = 0;
  }
};

class sp_32x32_object : public Sprite {
public:
  int stop;

  sp_32x32_object() {
    init(-64, -64, 32, 32, sprite_draw, GLOBALS_FRM_32X32_L, 0, 0, 0, 0, 0, 0,
         0, 0, 0);
    stop = 0;
  }
};

// vgrad class

class vgrad {
public:
  int redstart;
  int grnstart;
  int blustart;
  int redstop;
  int grnstop;
  int blustop;

  vgrad() {}

  vgrad(int c1, int c2) { setcols(c1, c2); }

  void setcols(int c1, int c2) {
    redstart = (c1 >> 16) & 0xff;
    grnstart = (c1 >> 8) & 0xff;
    blustart = (c1)&0xff;
    redstop = (c2 >> 16) & 0xff;
    grnstop = (c2 >> 8) & 0xff;
    blustop = (c2)&0xff;
  }

  void draw() {
    GLshort vertices[8];
    GLubyte colors[16];

    vertices[0] = 0;
    vertices[1] = GLOBALS_SCROLL_Y;
    vertices[2] = WINDOW_WIDTH;
    vertices[3] = GLOBALS_SCROLL_Y;
    vertices[4] = 0;
    vertices[5] = GLOBALS_SCROLL_Y + (MAP_HEIGHT * TILE_HEIGHT);
    vertices[6] = vertices[2];
    vertices[7] = vertices[5];

    colors[0] = redstart;
    colors[1] = grnstart;
    colors[2] = blustart;
    colors[3] = 0;

    colors[4] = redstart;
    colors[5] = grnstart;
    colors[6] = blustart;
    colors[7] = 0;

    colors[8] = redstop;
    colors[9] = grnstop;
    colors[10] = blustop;
    colors[11] = 0;

    colors[12] = redstop;
    colors[13] = grnstop;
    colors[14] = blustop;
    colors[15] = 0;

    // setup for gradient quad
    glDisable(GL_BLEND);
    glEnableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisable(GL_TEXTURE_2D);

    glVertexPointer(2, GL_SHORT, 0, vertices);
    glColorPointer(4, GL_UNSIGNED_BYTE, 0, colors);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);

    // set state back to sprite drawing
    glEnable(GL_BLEND);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
    glEnable(GL_TEXTURE_2D);
  }
};

// map class

class map {
public:
  unsigned char *maparray, *flagsarray;
  Pixmap *mappix;
  int mapw, maph;

  map() { clrmap(); }

  map(Pixmap *b, unsigned char *m, unsigned char *f, int w, int h) {
    setmap(b, m, f, w, h);
  }

  void setmap(Pixmap *b, unsigned char *m, unsigned char *f, int w, int h) {
    mappix = b;
    maparray = m;
    flagsarray = f;
    mapw = w;
    maph = h;
  }

  void clrmap() { mappix = 0; }

  void draw() {
    if (mappix == 0)
      return;

    unsigned char *marray = maparray, *marray1;
    unsigned char index;
    int x, y, sx;

    sx = (-GLOBALS_SCROLL_X / TILE_WIDTH);
    y = (-GLOBALS_SCROLL_Y / TILE_HEIGHT);
    marray += (sx + (y * mapw));
    sx = (GLOBALS_SCROLL_X + (sx * TILE_WIDTH));
    y = (GLOBALS_SCROLL_Y + (y * TILE_HEIGHT));
    do {
      marray1 = marray;
      x = sx;
      do {
        // blit tile
        index = *marray1;
        if (index != 0) {
          int fx, fy;
          mappix->getframe(index, fx, fy);
          if ((flagsarray[index] & FMAP_MASKED) != 0) {
            mappix->blit(x, y, fx, fy, TILE_WIDTH, TILE_HEIGHT);
          } else {
            mappix->blit(x, y, fx, fy, TILE_WIDTH, TILE_HEIGHT);
          }
        }
        x += TILE_WIDTH;
        marray1++;
      } while (x < WINDOW_WIDTH);
      y += TILE_HEIGHT;
      marray += mapw;
    } while (y < WINDOW_HEIGHT);
  }

  int getmapflags(int x, int y) {
    if ((x >= 0) && (x < (TILE_WIDTH * mapw))) {
      if ((y >= 0) && (y < (TILE_HEIGHT * maph))) {
        x = x / TILE_WIDTH;
        y = (y / TILE_HEIGHT) * mapw;
        return (flagsarray[maparray[x + y]]);
      }
    }
    return FMAP_STAND;
  }
};

// mind map class

class mindmap {
public:
  int plane1_xoffset;
  int plane1_yoffset;
  int plane2_xoffset;
  int plane2_yoffset;
  Pixmap *mappix;

  mindmap() { clrmap(); }

  mindmap(Pixmap *b) { setmap(b); }

  void setmap(Pixmap *b) {
    mappix = b;
    setoffsets(0, 0, 0, 0);
  }

  void clrmap() { mappix = 0; }

  void draw() {
    if (mappix == 0)
      return;

    int x, y;

    // plane 1
    y = -plane1_yoffset;
    do {
      x = -plane1_xoffset;
      do {
        mappix->blit(x, y, 0, 0, TILE_WIDTH, TILE_HEIGHT);
        x += TILE_WIDTH;
      } while (x < WINDOW_WIDTH);
      y += TILE_HEIGHT;
    } while (y < WINDOW_HEIGHT);

    // plane 2
    y = -plane2_yoffset;
    do {
      x = -plane2_xoffset;
      do {
        mappix->blit(x, y, 0, TILE_HEIGHT, TILE_WIDTH, TILE_HEIGHT);
        mappix->blit(x + TILE_WIDTH, y, 0, TILE_HEIGHT * 3, TILE_WIDTH,
                     TILE_HEIGHT);
        mappix->blit(x, y + TILE_HEIGHT, 0, TILE_HEIGHT * 2, TILE_WIDTH,
                     TILE_HEIGHT);
        x += TILE_WIDTH * 2;
      } while (x < WINDOW_WIDTH);
      y += TILE_HEIGHT * 2;
    } while (y < WINDOW_HEIGHT);
  }

  void getoffsets(int &x1, int &y1, int &x2, int &y2) {
    x1 = plane1_xoffset;
    y1 = plane1_yoffset;
    x2 = plane2_xoffset;
    y2 = plane2_yoffset;
  }

  void setoffsets(int x1, int y1, int x2, int y2) {
    plane1_xoffset = x1;
    plane1_yoffset = y1;
    plane2_xoffset = x2;
    plane2_yoffset = y2;
  }
};

// autoplay class

class autoplay {
public:
  unsigned char *buf;
  int bsize;

  int pindex;
  int rindex;
  int count;
  int keys;
  int score;
  int game_seed;
  int item_selected;
  int game_location_x;
  int game_location_y;
  int game_timezone;
  int game_level;
  int man_power;
  int man_strength;
  int game_flags;
  int game_frame_count;
  int game_menu_difficulty;
  int game_menu_mode;
  unsigned char game_campainmap[sizeof(GLOBALS_GAME_CAMPAINMAP)];
  int item_inventory[sizeof(GLOBALS_ITEM_INVENTORY) / sizeof(int)];
  int item_useage[sizeof(GLOBALS_ITEM_USEAGE) / sizeof(int)];

  autoplay() { memset(this, 0, sizeof(autoplay)); }

  ~autoplay() {
    if (buf != 0) {
      delete buf;
    }
  }

  inline void setscore(int val) { score = val; }

  inline int getscore() { return (score); }

  void reset() {
    count = 0;
    keys = -1;
    pindex = 0;
    rindex = 0;
  }

  void playreset() {
    count = 0;
    keys = -1;
    pindex = 0;
  }

  void savestate() {
    game_seed = GLOBALS_GAME_SEED;
    item_selected = GLOBALS_ITEM_SELECTED;
    game_location_x = GLOBALS_GAME_LOCATION_X;
    game_location_y = GLOBALS_GAME_LOCATION_Y;
    game_timezone = GLOBALS_GAME_TIMEZONE;
    game_level = GLOBALS_GAME_LEVEL;
    man_power = GLOBALS_MAN_POWER;
    man_strength = GLOBALS_MAN_STRENGTH;
    game_flags = GLOBALS_GAME_FLAGS;
    game_frame_count = GLOBALS_GAME_FRAME_COUNT;

    game_menu_difficulty = GLOBALS_GAME_MENU_DIFFICULTY_INDEX;
    game_menu_mode = GLOBALS_GAME_MENU_MODE_INDEX;

    memcpy(game_campainmap, GLOBALS_GAME_CAMPAINMAP,
           sizeof(GLOBALS_GAME_CAMPAINMAP));
    memcpy(item_inventory, GLOBALS_ITEM_INVENTORY,
           sizeof(GLOBALS_ITEM_INVENTORY));
    memcpy(item_useage, GLOBALS_ITEM_USEAGE, sizeof(GLOBALS_ITEM_USEAGE));
  }

  void loadstate() {
    GLOBALS_GAME_SEED = game_seed;
    GLOBALS_ITEM_SELECTED = item_selected;
    GLOBALS_GAME_LOCATION_X = game_location_x;
    GLOBALS_GAME_LOCATION_Y = game_location_y;
    GLOBALS_GAME_TIMEZONE = game_timezone;
    GLOBALS_GAME_LEVEL = game_level;
    GLOBALS_MAN_POWER = man_power;
    GLOBALS_MAN_STRENGTH = man_strength;
    GLOBALS_GAME_FLAGS = game_flags;
    GLOBALS_GAME_FRAME_COUNT = game_frame_count;

    GLOBALS_GAME_MENU_DIFFICULTY_INDEX = game_menu_difficulty;
    GLOBALS_GAME_MENU_DIFFICULTY = txt_menu_dif[game_menu_difficulty];
    GLOBALS_GAME_MENU_MODE_INDEX = game_menu_mode;
    GLOBALS_GAME_MENU_MODE = txt_menu_mode[game_menu_mode];

    memcpy(GLOBALS_GAME_CAMPAINMAP, game_campainmap,
           sizeof(GLOBALS_GAME_CAMPAINMAP));
    memcpy(GLOBALS_ITEM_INVENTORY, item_inventory,
           sizeof(GLOBALS_ITEM_INVENTORY));
    memcpy(GLOBALS_ITEM_USEAGE, item_useage, sizeof(GLOBALS_ITEM_USEAGE));
  }

  void recordkeys(int k) {
    if (keys == -1) {
      keys = k;
      return;
    }
    if ((keys != k) || (count == 255)) {
      if (rindex == bsize) {
        int nsize = (bsize + 2048);
        unsigned char *nbuf = new unsigned char[nsize];
        if (nbuf != 0) {
          if (buf != 0) {
            memcpy(nbuf, buf, bsize);
            delete buf;
          }
          buf = nbuf;
          bsize = nsize;
        } else {
          return;
        }
      }
      buf[rindex] = keys;
      buf[rindex + 1] = count;
      rindex += 2;
      keys = k;
      count = 0;
    } else {
      count++;
    }
  }

  int playkeys() {
    if (count != 0) {
      count--;
    } else {
      if ((pindex == rindex) || (buf == 0)) {
        return -1;
      }
      keys = buf[pindex];
      count = buf[pindex + 1];
      pindex += 2;
    }
    return keys;
  }

  void load() {
    int ssize;

    // first try user data file
    FILE *stream = openappdatafile("demo", "dat", "rb");
    if (stream == 0) {
      // ok we will have to go with the pre recorded
      stream = openbundlefile("demo", "dat");
    }
    if (stream != 0) {
      fread(&ssize, 1, sizeof(int), stream);

      if (ssize > bsize) {
        unsigned char *nbuf = new unsigned char[ssize];
        if (nbuf != 0) {
          if (buf != 0) {
            delete buf;
          }
          buf = nbuf;
          bsize = ssize;
        } else {
          fclose(stream);
          return;
        }
      }
      fread(buf, 1, ssize, stream);
      fread(&pindex, 1, sizeof(autoplay) - 8, stream);
      fclose(stream);
    }
  }

  void save() {
    if (buf != 0) {
      FILE *stream = openappdatafile("demo", "dat", "wb");
      if (stream != 0) {
        fwrite(&rindex, 1, sizeof(int), stream);
        fwrite(buf, 1, rindex, stream);
        fwrite(&pindex, 1, sizeof(autoplay) - 8, stream);
        fclose(stream);
      }
    }
  }
};

Onslaught::Onslaught() {}

bool Onslaught::GameInit(float w, float h) {
  GLOBALS_MAN_DLIST.init();
  GLOBALS_BANS_DLIST.init();
  GLOBALS_ITEM_DLIST.init();
  GLOBALS_ENEMY_DLIST.init();
  GLOBALS_MISSILE_DLIST.init();
  GLOBALS_BODY_DLIST.init();
  GLOBALS_FX_DLIST.init();

  GLOBALS_SKY = new vgrad(RGBRED, RGBYELLOW);
  GLOBALS_LAND = new map();
  GLOBALS_MIND = new mindmap();
  GLOBALS_RECORD = new autoplay();
  GLOBALS_PLAY = new autoplay();

  GLOBALS_ENEMY_STACKINDEX = 0;
  GLOBALS_GAME_STATE = GAME_STATE_TITLE;
  GLOBALS_GAME_STATE_LAST = -1;
  GLOBALS_GAME_STATE_NEXT = GAME_STATE_MENU;
  GLOBALS_GAME_MENU_DIFFICULTY_INDEX = 0;
  GLOBALS_GAME_MENU_DIFFICULTY = txt_menu_dif[0];
  GLOBALS_GAME_MENU_MODE_INDEX = 0;
  GLOBALS_GAME_MENU_MODE = txt_menu_mode[0];
  GLOBALS_GAME_MENU_SOUND_INDEX = 0;
  GLOBALS_GAME_MENU_SOUND = txt_menu_sound[0];
  GLOBALS_GAME_MENU[GLOBALS_GAME_MENU_START_COL] = RGBWHITE;
  GLOBALS_GAME_MENU[GLOBALS_GAME_MENU_DEFINE_COL] = RGBRED;
  GLOBALS_GAME_MENU[GLOBALS_GAME_MENU_DIFFICULTY_COL] = RGBRED;
  GLOBALS_GAME_MENU[GLOBALS_GAME_MENU_MODE_COL] = RGBRED;
  GLOBALS_GAME_MENU[GLOBALS_GAME_MENU_SOUND_COL] = RGBRED;
  GLOBALS_GAME_MENU[GLOBALS_GAME_MENU_CREDITS_COL] = RGBRED;
  GLOBALS_GAME_MENU_SELECTED = GLOBALS_GAME_MENU_START_COL;

  // load sound buffers
  loadsound(SOUND_EXPLODE, "explode");
  loadsound(SOUND_DRIP, "drip");
  loadsound(SOUND_HORSE, "horse");
  loadsound(SOUND_SPELL, "spell");
  loadsound(SOUND_TWANG1, "twang1");
  loadsound(SOUND_TWANG2, "twang2");
  loadsound(SOUND_DEATH1, "death1");
  loadsound(SOUND_DEATH2, "death2");
  loadsound(SOUND_CLASH1, "clash1");
  loadsound(SOUND_CLASH2, "clash2");
  loadsound(SOUND_HOOVES, "hooves");
  loadsound(SOUND_ROAR, "roar");
  loadsound(SOUND_THROW, "throw");
  loadsound(SOUND_OUT, "out");
  loadsound(SOUND_BOAR, "boar");

  // load images
  GLOBALS_ASCII = loadimage("ascii", 8, 8);
  GLOBALS_CAMPAIN = loadimage("campain", 8, 8);
  GLOBALS_PANEL = loadimage("panel", 320, 72);
  GLOBALS_BLOCKS1 = loadimage("blocks1", 16, 16);
  GLOBALS_BLOCKS2 = loadimage("blocks2", 16, 16);
  GLOBALS_BLOCKS3 = loadimage("blocks3", 16, 16);
  GLOBALS_FANATIC_L = loadimage("fanatic_l", 32, 32);
  GLOBALS_FRM_16X16 = loadimage("frm_16x16", 16, 16);
  GLOBALS_FRM_16X16_L = loadimage("frm_16x16_l", 16, 16);
  GLOBALS_FRM_16X32 = loadimage("frm_16x32", 16, 32);
  GLOBALS_FRM_16X64 = loadimage("frm_16x64", 16, 64);
  GLOBALS_FRM_32C32 = loadimage("frm_32c32", 32, 32);
  GLOBALS_FRM_32C32_L = loadimage("frm_32c32_l", 32, 32);
  GLOBALS_FRM_32X16_L = loadimage("frm_32x16_l", 32, 16);
  GLOBALS_FRM_32X32_L = loadimage("frm_32x32_l", 32, 32);
  GLOBALS_FRM_64X32_L = loadimage("frm_64x32_l", 64, 32);
  GLOBALS_FRM_64X64 = loadimage("frm_64x64", 64, 64);
  GLOBALS_FRM_64X64_L = loadimage("frm_64x64_l", 64, 64);

  // load demo resource
  GLOBALS_PLAY->load();

  return true;
}

void Onslaught::GameDeinit() {
  game_free_dlists();
  delete GLOBALS_SKY;
  delete GLOBALS_LAND;
  delete GLOBALS_MIND;
  delete GLOBALS_RECORD;
  delete GLOBALS_PLAY;

  // release images
  delete GLOBALS_PANEL;
  delete GLOBALS_BLOCKS1;
  delete GLOBALS_BLOCKS2;
  delete GLOBALS_BLOCKS3;
  delete GLOBALS_CAMPAIN;
  delete GLOBALS_FANATIC_L;
  delete GLOBALS_FRM_16X16;
  delete GLOBALS_FRM_16X16_L;
  delete GLOBALS_FRM_16X32;
  delete GLOBALS_FRM_16X64;
  delete GLOBALS_FRM_32C32;
  delete GLOBALS_FRM_32C32_L;
  delete GLOBALS_FRM_32X16_L;
  delete GLOBALS_FRM_32X32_L;
  delete GLOBALS_FRM_64X32_L;
  delete GLOBALS_FRM_64X64;
  delete GLOBALS_FRM_64X64_L;
  delete GLOBALS_ASCII;
}

void Onslaught::GameReset() {
  // reset back to menu
  GLOBALS_GAME_STATE_NEXT = GAME_STATE_MENU;
  GLOBALS_GAME_STATE = GAME_STATE_TITLE;
}

void Onslaught::GameFrame() {
  // copy over keystate
  GLOBALS_GAME_CONTROLS = GLOBALS_GAME_KEYBOARD;
  GLOBALS_GAME_CONTROLS1 = GLOBALS_GAME_KEYBOARD;

  // game state management
  do {
    if (GLOBALS_GAME_STATE != GLOBALS_GAME_STATE_LAST) {
      GLOBALS_GAME_STATE_LAST = GLOBALS_GAME_STATE;
      // init this state
      (*init_state_table[GLOBALS_GAME_STATE])();
    }
    // frame of this state
    (*frame_state_table[GLOBALS_GAME_STATE])();
  } while (GLOBALS_GAME_STATE != GLOBALS_GAME_STATE_LAST);

  // process sprites
  game_proc_dlists();

  // composite screen
  GLOBALS_SKY->draw();
  GLOBALS_LAND->draw();
  GLOBALS_MIND->draw();
  sprite_draw_list(&GLOBALS_BANS_DLIST);
  sprite_draw_list(&GLOBALS_ITEM_DLIST);
  sprite_draw_list(&GLOBALS_ENEMY_DLIST);
  sprite_draw_list(&GLOBALS_MAN_DLIST);
  sprite_draw_list(&GLOBALS_BODY_DLIST);
  sprite_draw_list(&GLOBALS_MISSILE_DLIST);
  sprite_draw_list(&GLOBALS_FX_DLIST);

  // blit panel to back buffer
  GLOBALS_PANEL->blit(0, WINDOW_HEIGHT, 0, 0, WINDOW_WIDTH, PANEL_HEIGHT);

  // blit pannel icons
  update_panel();

  // next frame
  GLOBALS_GAME_FRAME_COUNT++;
}

void Onslaught::GameSave() {
  // save highscore table
  FILE *stream = openappdatafile("highscore", "app", "wb");
  if (stream) {
    fwrite(&GLOBALS_SCORE, 1, sizeof(GLOBALS_SCORE), stream);
    fwrite(&GLOBALS_SCORE_TXT, 1, sizeof(GLOBALS_SCORE_TXT), stream);

    fclose(stream);
  }

  // save game state file
  stream = openappdatafile("oldstate", "app", "wb");
  if (stream) {
    // do a little fudging to protect us from trouble
    if (GLOBALS_GAME_STATE == GAME_STATE_TITLE) {
      // in title sequence so try save state we are going to, unless demo mode
      if (GLOBALS_GAME_STATE_NEXT == GAME_STATE_DEMO) {
        GLOBALS_GAME_STATE_NEXT = GAME_STATE_MENU;
      }
      GLOBALS_GAME_STATE = GLOBALS_GAME_STATE_NEXT;
    }

    // check for demo mode
    if (GLOBALS_GAME_STATE == GAME_STATE_DEMO) {
      // we are in demo mode so restore real settings
      // we don't want to save the demo settings !
      GLOBALS_RECORD->loadstate();
    }

    // save state
    fwrite(&GLOBALS_GAME_STATE, 1, sizeof(GLOBALS_GAME_STATE), stream);

    // save menu setup
    fwrite(&GLOBALS_GAME_MENU_SOUND_INDEX, 1,
           sizeof(GLOBALS_GAME_MENU_SOUND_INDEX), stream);
    fwrite(&GLOBALS_GAME_MENU_DIFFICULTY_INDEX, 1,
           sizeof(GLOBALS_GAME_MENU_DIFFICULTY_INDEX), stream);
    fwrite(&GLOBALS_GAME_MENU_MODE_INDEX, 1,
           sizeof(GLOBALS_GAME_MENU_MODE_INDEX), stream);

    // save generics
    fwrite(&GLOBALS_GAME_SEED, 1, sizeof(GLOBALS_GAME_SEED), stream);
    fwrite(&GLOBALS_GAME_TIMEZONE, 1, sizeof(GLOBALS_GAME_TIMEZONE), stream);
    fwrite(&GLOBALS_GAME_LEVEL, 1, sizeof(GLOBALS_GAME_LEVEL), stream);

    // save man details
    fwrite(&GLOBALS_MAN_SCORE, 1, sizeof(GLOBALS_MAN_SCORE), stream);
    fwrite(&GLOBALS_MAN_POWER, 1, sizeof(GLOBALS_MAN_POWER), stream);
    fwrite(&GLOBALS_MAN_STRENGTH, 1, sizeof(GLOBALS_MAN_STRENGTH), stream);
    fwrite(&GLOBALS_MAN_BANNER, 1, sizeof(GLOBALS_MAN_BANNER), stream);
    fwrite(&GLOBALS_MAN_NAME, 1, sizeof(GLOBALS_MAN_NAME), stream);
    fwrite(&GLOBALS_MAN_HOMELAND, 1, sizeof(GLOBALS_MAN_HOMELAND), stream);
    fwrite(&GLOBALS_MAN_CULT_INDEX, 1, sizeof(GLOBALS_MAN_CULT_INDEX), stream);

    // save map details
    fwrite(&GLOBALS_GAME_LOCATION_X, 1, sizeof(GLOBALS_GAME_LOCATION_X),
           stream);
    fwrite(&GLOBALS_GAME_LOCATION_Y, 1, sizeof(GLOBALS_GAME_LOCATION_Y),
           stream);
    fwrite(&GLOBALS_GAME_LOCATION_LASTX, 1, sizeof(GLOBALS_GAME_LOCATION_LASTX),
           stream);
    fwrite(&GLOBALS_GAME_LOCATION_LASTY, 1, sizeof(GLOBALS_GAME_LOCATION_LASTY),
           stream);
    fwrite(&GLOBALS_GAME_LOCATION_OX, 1, sizeof(GLOBALS_GAME_LOCATION_OX),
           stream);
    fwrite(&GLOBALS_GAME_LOCATION_OY, 1, sizeof(GLOBALS_GAME_LOCATION_OY),
           stream);
    fwrite(&GLOBALS_GAME_CAMPAINMAP, 1, sizeof(GLOBALS_GAME_CAMPAINMAP),
           stream);

    // save panel details
    fwrite(&GLOBALS_ITEM_SELECTED, 1, sizeof(GLOBALS_ITEM_SELECTED), stream);
    fwrite(&GLOBALS_ITEM_INVENTORY, 1, sizeof(GLOBALS_ITEM_INVENTORY), stream);
    fwrite(&GLOBALS_ITEM_USEAGE, 1, sizeof(GLOBALS_ITEM_USEAGE), stream);

    fclose(stream);
  }
}

void Onslaught::GameLoad() {
  // load highscore table
  FILE *stream = openappdatafile("highscore", "app", "rb");
  if (stream) {
    fread(&GLOBALS_SCORE, 1, sizeof(GLOBALS_SCORE), stream);
    fread(&GLOBALS_SCORE_TXT, 1, sizeof(GLOBALS_SCORE_TXT), stream);

    fclose(stream);
  }

  stream = openappdatafile("oldstate", "app", "rb");
  if (stream) {
    // load the state we should go to after a title sequence
    fread(&GLOBALS_GAME_STATE_NEXT, 1, sizeof(GLOBALS_GAME_STATE_NEXT), stream);

    // load menu setup
    fread(&GLOBALS_GAME_MENU_SOUND_INDEX, 1,
          sizeof(GLOBALS_GAME_MENU_SOUND_INDEX), stream);
    fread(&GLOBALS_GAME_MENU_DIFFICULTY_INDEX, 1,
          sizeof(GLOBALS_GAME_MENU_DIFFICULTY_INDEX), stream);
    fread(&GLOBALS_GAME_MENU_MODE_INDEX, 1,
          sizeof(GLOBALS_GAME_MENU_MODE_INDEX), stream);
    GLOBALS_GAME_MENU_SOUND = txt_menu_sound[GLOBALS_GAME_MENU_SOUND_INDEX];
    GLOBALS_GAME_MENU_DIFFICULTY =
        txt_menu_dif[GLOBALS_GAME_MENU_DIFFICULTY_INDEX];
    GLOBALS_GAME_MENU_MODE = txt_menu_mode[GLOBALS_GAME_MENU_MODE_INDEX];

    // save generics
    fread(&GLOBALS_GAME_SEED, 1, sizeof(GLOBALS_GAME_SEED), stream);
    fread(&GLOBALS_GAME_TIMEZONE, 1, sizeof(GLOBALS_GAME_TIMEZONE), stream);
    fread(&GLOBALS_GAME_LEVEL, 1, sizeof(GLOBALS_GAME_LEVEL), stream);

    // load man details
    fread(&GLOBALS_MAN_SCORE, 1, sizeof(GLOBALS_MAN_SCORE), stream);
    fread(&GLOBALS_MAN_POWER, 1, sizeof(GLOBALS_MAN_POWER), stream);
    fread(&GLOBALS_MAN_STRENGTH, 1, sizeof(GLOBALS_MAN_STRENGTH), stream);
    fread(&GLOBALS_MAN_BANNER, 1, sizeof(GLOBALS_MAN_BANNER), stream);
    fread(&GLOBALS_MAN_NAME, 1, sizeof(GLOBALS_MAN_NAME), stream);
    fread(&GLOBALS_MAN_HOMELAND, 1, sizeof(GLOBALS_MAN_HOMELAND), stream);
    fread(&GLOBALS_MAN_CULT_INDEX, 1, sizeof(GLOBALS_MAN_CULT_INDEX), stream);
    GLOBALS_MAN_CULT = cult_names[GLOBALS_MAN_CULT_INDEX];

    // load map details
    fread(&GLOBALS_GAME_LOCATION_X, 1, sizeof(GLOBALS_GAME_LOCATION_X), stream);
    fread(&GLOBALS_GAME_LOCATION_Y, 1, sizeof(GLOBALS_GAME_LOCATION_Y), stream);
    fread(&GLOBALS_GAME_LOCATION_LASTX, 1, sizeof(GLOBALS_GAME_LOCATION_LASTX),
          stream);
    fread(&GLOBALS_GAME_LOCATION_LASTY, 1, sizeof(GLOBALS_GAME_LOCATION_LASTY),
          stream);
    fread(&GLOBALS_GAME_LOCATION_OX, 1, sizeof(GLOBALS_GAME_LOCATION_OX),
          stream);
    fread(&GLOBALS_GAME_LOCATION_OY, 1, sizeof(GLOBALS_GAME_LOCATION_OY),
          stream);
    fread(&GLOBALS_GAME_CAMPAINMAP, 1, sizeof(GLOBALS_GAME_CAMPAINMAP), stream);

    // load panel details
    fread(&GLOBALS_ITEM_SELECTED, 1, sizeof(GLOBALS_ITEM_SELECTED), stream);
    fread(&GLOBALS_ITEM_INVENTORY, 1, sizeof(GLOBALS_ITEM_INVENTORY), stream);
    fread(&GLOBALS_ITEM_USEAGE, 1, sizeof(GLOBALS_ITEM_USEAGE), stream);

    // set up for game play
    game_generate_enemy_info();
    game_setstate_battle_common();

    // fudge the state
    switch (GLOBALS_GAME_STATE_NEXT) {
    case GAME_STATE_BATTLE:
    case GAME_STATE_BATTLE_WON:
    case GAME_STATE_BATTLE_LOST:
    case GAME_STATE_MIND:
    case GAME_STATE_MIND_WON:
    case GAME_STATE_MIND_LOST:
    case GAME_STATE_ORACLE: {
      // go back to map
      GLOBALS_GAME_STATE_NEXT = GAME_STATE_MAP;
    }
    default:
      break;
    }

    // goto title first
    GLOBALS_GAME_STATE = GAME_STATE_TITLE;

    fclose(stream);
  }
}

void Onslaught::ButtonDown(int button) {
  switch (button) {
  case 'U':
    GLOBALS_GAME_KEYBOARD = GLOBALS_GAME_KEYBOARD | (1 << BFKEY_UP);
    break;
  case 'D':
    GLOBALS_GAME_KEYBOARD = GLOBALS_GAME_KEYBOARD | (1 << BFKEY_DOWN);
    break;
  case 'L':
    GLOBALS_GAME_KEYBOARD = GLOBALS_GAME_KEYBOARD | (1 << BFKEY_LEFT);
    break;
  case 'R':
    GLOBALS_GAME_KEYBOARD = GLOBALS_GAME_KEYBOARD | (1 << BFKEY_RIGHT);
    break;
  case '1':
    GLOBALS_GAME_KEYBOARD = GLOBALS_GAME_KEYBOARD | (1 << BFKEY_KEYA);
    break;
  case '2':
    GLOBALS_GAME_KEYBOARD = GLOBALS_GAME_KEYBOARD | (1 << BFKEY_KEYB);
    break;
  case '3':
    GLOBALS_GAME_KEYBOARD = GLOBALS_GAME_KEYBOARD | (1 << BFKEY_KEYC);
    break;
  case '4':
    GLOBALS_GAME_KEYBOARD = GLOBALS_GAME_KEYBOARD | (1 << BFKEY_KEYD);
    break;
  default:
    break;
  }
}

void Onslaught::ButtonUp(int button) {
  switch (button) {
  case 'U':
    GLOBALS_GAME_KEYBOARD = GLOBALS_GAME_KEYBOARD & ~(1 << BFKEY_UP);
    break;
  case 'D':
    GLOBALS_GAME_KEYBOARD = GLOBALS_GAME_KEYBOARD & ~(1 << BFKEY_DOWN);
    break;
  case 'L':
    GLOBALS_GAME_KEYBOARD = GLOBALS_GAME_KEYBOARD & ~(1 << BFKEY_LEFT);
    break;
  case 'R':
    GLOBALS_GAME_KEYBOARD = GLOBALS_GAME_KEYBOARD & ~(1 << BFKEY_RIGHT);
    break;
  case '1':
    GLOBALS_GAME_KEYBOARD = GLOBALS_GAME_KEYBOARD & ~(1 << BFKEY_KEYA);
    break;
  case '2':
    GLOBALS_GAME_KEYBOARD = GLOBALS_GAME_KEYBOARD & ~(1 << BFKEY_KEYB);
    break;
  case '3':
    GLOBALS_GAME_KEYBOARD = GLOBALS_GAME_KEYBOARD & ~(1 << BFKEY_KEYC);
    break;
  case '4':
    GLOBALS_GAME_KEYBOARD = GLOBALS_GAME_KEYBOARD & ~(1 << BFKEY_KEYD);
    break;
  default:
    break;
  }
}

// game state and frame functions

void game_setstate_title() {
  // free dlists
  game_free_dlists();

  // set no map
  game_set_map(-1);

  // set sky colors and position
  GLOBALS_SKY->setcols(RGBBLACK, RGBBLACK);
  GLOBALS_SCROLL_X = 0;
  GLOBALS_SCROLL_Y = 0;

  // set title letter pointer and flag
  GLOBALS_GAME_TITLE = 0;
  GLOBALS_GAME_COUNT = 0;
  GLOBALS_GAME_FLAGS |= FFLAG_TITLE;
}

void game_setstate_menu() {
  // free dlists
  game_free_dlists();

  // add menu string
  sp_string_object *menu = new sp_string_object();
  if (menu != 0) {
    GLOBALS_FX_DLIST.addhead(menu);
    menu->SPRITE_USER1 = (int)txt_menu_items;
  }

  // clear frame counter
  GLOBALS_GAME_COUNT = 0;
}

void game_setstate_map() {
  // free dlists
  game_free_dlists();

  // set no map
  game_set_map(-1);

  // set man status and experience
  int score = GLOBALS_MAN_SCORE;
  if (score > 999999) {
    score = 999999;
    GLOBALS_MAN_SCORE = score;
  }
  score /= 65536;
  if (score > 9) {
    score = 9;
  }
  GLOBALS_MAN_EXPERIENCE = score;
  GLOBALS_MAN_STATUS = status_names[score];

  // add player objects
  sp_string_object *string = new sp_string_object();
  if (string != 0) {
    GLOBALS_MAN_DLIST.addhead(string);
    string->SPRITE_USER1 = (int)txt_map_player_headings;
  }
  sp_16x16_object *ban = new sp_16x16_object();
  if (ban != 0) {
    GLOBALS_MAN_DLIST.addhead(ban);
    ban->SPRITE_FRAME = GLOBALS_MAN_BANNER;
    ban->SPRITE_X = 16;
    ban->SPRITE_Y = (WINDOW_HEIGHT / 2) - 56;
  }
  sp_32x32_object *man = new sp_32x32_object();
  if (man != 0) {
    GLOBALS_MAN_DLIST.addhead(man);
    man->SPRITE_PIX = GLOBALS_FANATIC_L;
    man->SPRITE_FRAME = FRM_STANCE;
    man->SPRITE_X = 48;
    man->SPRITE_Y = (WINDOW_HEIGHT / 2) - 64;
  }

  // add map object
  sp_map *map = new sp_map();
  if (map != 0) {
    GLOBALS_MAN_DLIST.addhead(map);
    map->SPRITE_X = 96;
    map->SPRITE_Y = (WINDOW_HEIGHT / 2) - 64;
  }

  // add sight object
  sp_sight *sight = new sp_sight();
  if (sight != 0) {
    GLOBALS_MAN_DLIST.addtail(sight);
    sight->SPRITE_X = ((GLOBALS_GAME_LOCATION_X * 8) + 92);
    sight->SPRITE_Y =
        ((WINDOW_HEIGHT / 2) - 64) + ((GLOBALS_GAME_LOCATION_Y * 8) - 4);
  }
}

void game_setstate_scores() {
  // free dlists
  game_free_dlists();

  // add scores string
  sp_string_object *scores = new sp_string_object();
  if (scores != 0) {
    GLOBALS_FX_DLIST.addhead(scores);
    scores->SPRITE_USER1 = (int)txt_scores_items;
  }
  GLOBALS_GAME_COUNT = 0;
}

void game_setstate_hiscore() {
  // free dlists
  game_free_dlists();

  // add scores string
  sp_string_object *string = new sp_string_object();
  if (string != 0) {
    GLOBALS_FX_DLIST.addhead(string);
    if (GLOBALS_GAME_MENU_MODE == txt_menu_tutor) {
      // no hi score in tutor mode
      string->SPRITE_USER1 = (int)txt_intutor;
    } else if (GLOBALS_MAN_SCORE <= GLOBALS_SCORE[9]) {
      // no hi score
      string->SPRITE_USER1 = (int)txt_nohiscore;
    } else {
      // enter hi score
      string->SPRITE_USER1 = (int)txt_askname;
      GLOBALS_MAN_INITIALS[0] = 0;

      // add enter object
      sp_enter_object *sfx = new sp_enter_object();
      if (sfx != 0) {
        GLOBALS_FX_DLIST.addhead(sfx);
        sfx->SPRITE_USER1 = (int)txt_lettab;
        sfx->SPRITE_USER2 = (int)GLOBALS_MAN_INITIALS;
      }

      // add sight object
      sp_sight *sfx1 = new sp_sight();
      if (sfx1 != 0) {
        GLOBALS_FX_DLIST.addtail(sfx1);
        sfx1->SPRITE_X = (WINDOW_WIDTH / 2) - 12;
        sfx1->SPRITE_Y = (WINDOW_HEIGHT / 2) + 12;
      }
    }
  }
}

void game_setstate_battle_common() {
  // set globals
  GLOBALS_ENEMY_STACKINDEX = 0;
  GLOBALS_MINE_COUNT = 0;
  GLOBALS_ENEMY_COUNT = 0;
  GLOBALS_ENEMY_DELAY = 1;
  GLOBALS_ITEM_DROPCNT = 0;
  GLOBALS_GAME_FLAGS = (FFLAG_SELECT | FFLAG_USE);

  // free dlists
  game_free_dlists();

  // set level
  game_set_level(GLOBALS_GAME_LEVEL);

  // set sky colors
  int rand = random(6);
  int col = RGBWHITE;
  if (rand == 0) {
    col = ((0xfefefe & RGBGREEN) >> 1) + ((0xfefefe & RGBWHITE) >> 1);
  } else if (rand == 1) {
    col = ((0xfefefe & RGBBLUE) >> 1) + ((0xfefefe & RGBWHITE) >> 1);
  } else if (rand == 2) {
    col = ((0xfefefe & RGBCYAN) >> 1) + ((0xfefefe & RGBWHITE) >> 1);
  } else if (rand == 3) {
    col = ((0xfefefe & RGBMAGENTA) >> 1) + ((0xfefefe & RGBWHITE) >> 1);
  } else if (rand == 4) {
    col = ((0xfefefe & RGBYELLOW) >> 1) + ((0xfefefe & RGBWHITE) >> 1);
  } else if (rand == 5) {
    col = ((0xfefefe & RGBRED) >> 1) + ((0xfefefe & RGBWHITE) >> 1);
  }
  vgrad *sky = GLOBALS_SKY;
  sky->setcols(col, RGBWHITE);
}

void game_setstate_battle() {
  // remember start score
  GLOBALS_MAN_START_SCORE = GLOBALS_MAN_SCORE;

  // set recording state and reset
  GLOBALS_RECORD->savestate();
  GLOBALS_RECORD->reset();

  // common battle init code
  game_setstate_battle_common();
}

void game_setstate_battle_won() {
  // set globals
  GLOBALS_GAME_COUNT = 0;

  // replace man sprite
  Sprite *man = sprite_find_list_types(&GLOBALS_MAN_DLIST, FTP_MAN);
  if (man) {
    man->SPRITE_DEATH = 0;
    int frame = man->SPRITE_FRAME;
    sprite_kill_list_id(&GLOBALS_MAN_DLIST, man->SPRITE_ID);

    // add stance sprite
    sp_manstance *stance = new sp_manstance();
    if (stance) {
      GLOBALS_MAN_DLIST.addhead(stance);
      stance->SPRITE_X = man->SPRITE_X;
      stance->SPRITE_Y = man->SPRITE_Y;
      stance->SPRITE_W = man->SPRITE_W;
      stance->SPRITE_H = man->SPRITE_H;
      stance->SPRITE_DIR = man->SPRITE_DIR;
      stance->SPRITE_FRAME = frame;
      stance->SPRITE_USER1 = man->SPRITE_USER1;
      stance->SPRITE_USER2 = man->SPRITE_USER2;
    }
  }
}

void game_setstate_mind() {
  // free dlists
  game_free_dlists();

  // set mind map
  game_set_map(GAME_LEVEL_MIND);

  // add player sfx
  sp_hand *hand = new sp_hand();
  if (hand != 0) {
    GLOBALS_MAN_DLIST.addhead(hand);
    int x, y;
    getxy_inner(hand->SPRITE_W, hand->SPRITE_H, x, y);
    hand->SPRITE_X = (x & -8);
    hand->SPRITE_Y = (y & -8);
  }

  // add mind
  sp_mind *mind = new sp_mind();
  if (mind != 0) {
    GLOBALS_ENEMY_DLIST.addhead(mind);
    mind->SPRITE_X = 144;
    mind->SPRITE_Y = ((WINDOW_HEIGHT / 2) - 64) + 48;
    mind->SPRITE_FRAME = GLOBALS_ENEMY_LORD;
  }

  // set globals
  GLOBALS_GAME_COUNT = 128;
  GLOBALS_MIND_FIRECNT = 0;
  GLOBALS_MIND_ITEMINDEX = 0;

  // save power value
  GLOBALS_MIND_POWER = GLOBALS_MAN_POWER;
}

void game_setstate_mind_won() {
  // set globals
  GLOBALS_GAME_COUNT = 0;
}

void game_setstate_credits() {
  // free dlists
  game_free_dlists();

  // add scores string
  sp_string_object *string = new sp_string_object();
  if (string != 0) {
    GLOBALS_FX_DLIST.addhead(string);
    string->SPRITE_USER1 = (int)txt_credits;
  }
}

void game_setstate_oracle() {
  // free dlists
  game_free_dlists();

  // set mind map
  game_set_map(GAME_LEVEL_MIND);

  // add string
  sp_string_object *string = new sp_string_object();
  if (string != 0) {
    GLOBALS_FX_DLIST.addhead(string);
    char *hint = hint_txt_10;
    if ((GLOBALS_GAME_MENU_DIFFICULTY != txt_menu_easy) &&
        (GLOBALS_GAME_MENU_MODE != txt_menu_tutor)) {
      hint = oracle_hints[GLOBALS_MAN_EXPERIENCE];
    }
    string->SPRITE_USER1 = (int)hint;
    string->SPRITE_USER2 = RGBWHITE;
  }
}

void game_setstate_demo() {
  // save current state
  GLOBALS_RECORD->savestate();

  // set up for play battle
  game_generate_player_info();

  // load demo state
  GLOBALS_PLAY->loadstate();
  game_enemy_map();
  GLOBALS_PLAY->loadstate();
  game_setstate_battle_common();
  GLOBALS_PLAY->playreset();
}

void game_frame_title() {
  // cycle through the letters
  int index = GLOBALS_GAME_TITLE;
  int flags = GLOBALS_GAME_FLAGS;
  if (index == (sizeof(lettertable) / sizeof(int))) {
    int cnt = GLOBALS_GAME_COUNT;
    if ((flags & FFLAG_TITLE) != 0) {
      // last letter in place
      cnt = 46;
    }
    if (cnt != 0) {
      cnt--;
      GLOBALS_GAME_COUNT = cnt;
      if (cnt == 32) {
        // add sword
        sp_sword *sfx = new sp_sword();
        if (sfx != 0) {
          GLOBALS_ITEM_DLIST.addhead(sfx);
          sfx->ml.sprite_ml_init(sfx, sfx->SPRITE_X, sfx->SPRITE_Y, 152,
                                 ((WINDOW_HEIGHT / 2) - 64) + 32);
        }
      }
      if (cnt == 0) {
        // go to next state
        GLOBALS_GAME_STATE = GLOBALS_GAME_STATE_NEXT;
      }
    }
  } else if ((flags & FFLAG_TITLE) != 0) {
    sp_letter *sfx = new sp_letter();
    if (sfx != 0) {
      GLOBALS_ITEM_DLIST.addhead(sfx);
      int x1 = lettertable[index];
      int y1 = lettertable[index + 1];
      sfx->SPRITE_FRAME = lettertable[index + 2];
      int x, y;
      getxy_outer(sfx->SPRITE_W, sfx->SPRITE_H, x, y);
      sfx->ml.sprite_ml_init(sfx, x, y, x1, y1);
      index += 3;
      GLOBALS_GAME_TITLE = index;
    }
  }
  flags &= ~FFLAG_TITLE;
  GLOBALS_GAME_FLAGS = flags;

  // wait for key release
  int controls = GLOBALS_GAME_CONTROLS;
  if (controls != 0) {
    if ((GLOBALS_GAME_FLAGS & FFLAG_SELECT) == 0) {
      GLOBALS_GAME_FLAGS |= FFLAG_SELECT;

      // go to next state
      GLOBALS_GAME_STATE = GLOBALS_GAME_STATE_NEXT;
    }
  } else {
    // set nothing pressed
    GLOBALS_GAME_FLAGS &= ~FFLAG_SELECT;
  }
}

extern Onslaught *game;

void game_frame_menu() {
  // process mr smiths
  game_mr_smiths();

  // selections
  int selected = GLOBALS_GAME_MENU_SELECTED;
  int controls = GLOBALS_GAME_CONTROLS;
  if (controls != 0) {
    // clear demo count
    GLOBALS_GAME_COUNT = 0;

    if ((GLOBALS_GAME_FLAGS & FFLAG_SELECT) == 0) {
      GLOBALS_GAME_FLAGS |= FFLAG_SELECT;
      if ((controls & FKEY_DOWN) != 0) {
        GLOBALS_GAME_MENU[selected] = RGBRED;
        selected++;
        if (selected > GLOBALS_GAME_MENU_CREDITS_COL) {
          selected = GLOBALS_GAME_MENU_START_COL;
        }
        GLOBALS_GAME_MENU[selected] = RGBWHITE;
      } else if ((controls & FKEY_UP) != 0) {
        GLOBALS_GAME_MENU[selected] = RGBRED;
        selected--;
        if (selected < GLOBALS_GAME_MENU_START_COL) {
          selected = GLOBALS_GAME_MENU_CREDITS_COL;
        }
        GLOBALS_GAME_MENU[selected] = RGBWHITE;
      } else if ((controls & FKEY_KEYB) != 0) {
        int index;
        switch (selected) {
        case GLOBALS_GAME_MENU_START_COL:
          // start game
          GLOBALS_GAME_STATE = GAME_STATE_MAP;
          game_generate_player_info();
          break;
        case GLOBALS_GAME_MENU_DEFINE_COL:
          // load game
          game->GameLoad();
          break;
        case GLOBALS_GAME_MENU_DIFFICULTY_COL:
          // swap difficulty
          index = GLOBALS_GAME_MENU_DIFFICULTY_INDEX;
          index++;
          if (index == (sizeof(txt_menu_dif) / sizeof(int))) {
            index = 0;
          }
          GLOBALS_GAME_MENU_DIFFICULTY_INDEX = index;
          GLOBALS_GAME_MENU_DIFFICULTY = txt_menu_dif[index];
          break;
        case GLOBALS_GAME_MENU_MODE_COL:
          // item mode select
          index = GLOBALS_GAME_MENU_MODE_INDEX;
          index++;
          if (index == (sizeof(txt_menu_mode) / sizeof(int))) {
            index = 0;
          }
          GLOBALS_GAME_MENU_MODE_INDEX = index;
          GLOBALS_GAME_MENU_MODE = txt_menu_mode[index];
          break;
        case GLOBALS_GAME_MENU_SOUND_COL:
          // sound on or off
          index = GLOBALS_GAME_MENU_SOUND_INDEX;
          index++;
          if (index == (sizeof(txt_menu_sound) / sizeof(int))) {
            index = 0;
          }
          GLOBALS_GAME_MENU_SOUND_INDEX = index;
          GLOBALS_GAME_MENU_SOUND = txt_menu_sound[index];
          break;
        case GLOBALS_GAME_MENU_CREDITS_COL:
          // go to credits
          GLOBALS_GAME_STATE = GAME_STATE_CREDITS;
        }
      }
    }
  } else {
    // set nothing pressed
    GLOBALS_GAME_FLAGS &= ~FFLAG_SELECT;

    // displayed long enough ?
    int cnt = GLOBALS_GAME_COUNT;
    cnt++;
    GLOBALS_GAME_COUNT = cnt;
    if (cnt == 256) {
      // go to demo via title
      GLOBALS_GAME_STATE_NEXT = GAME_STATE_DEMO;
      GLOBALS_GAME_STATE = GAME_STATE_TITLE;
    }
  }
  GLOBALS_GAME_MENU_SELECTED = selected;
}

void game_map_replace(int from, int to) {
  int i = 0;
  do {
    if (GLOBALS_GAME_CAMPAINMAP[i] == from) {
      GLOBALS_GAME_CAMPAINMAP[i] = to;
    }
    i++;
  } while (i != sizeof(GLOBALS_GAME_CAMPAINMAP));
}

int game_map_count(int type) {
  int cnt = 0;
  int i = 0;
  do {
    if (GLOBALS_GAME_CAMPAINMAP[i] == type) {
      cnt++;
    }
    i++;
  } while (i != sizeof(GLOBALS_GAME_CAMPAINMAP));
  return (cnt);
}

void game_enemy_map() {
  game_generate_enemy_info();

  int x = GLOBALS_GAME_LOCATION_X;
  int y = GLOBALS_GAME_LOCATION_Y;
  sprite_free_list(&GLOBALS_ENEMY_DLIST);
  int type = GLOBALS_GAME_CAMPAINMAP[y * 16 + x];
  if ((type != FRM_PLAYER) && (type >= FRM_PLAGUE)) {
    // enemy stats
    sp_string_object *string = new sp_string_object();
    if (string != 0) {
      GLOBALS_ENEMY_DLIST.addhead(string);
      string->SPRITE_USER1 = (int)txt_map_enemy_headings;
    }
    sp_16x16_object *ban = new sp_16x16_object();
    if (ban != 0) {
      GLOBALS_ENEMY_DLIST.addhead(ban);
      ban->SPRITE_FRAME = GLOBALS_ENEMY_BANNER;
      ban->SPRITE_X = 240;
      ban->SPRITE_Y = ((WINDOW_HEIGHT / 2) - 56);
    }
    sp_32x32_object *lord = new sp_32x32_object();
    if (lord != 0) {
      GLOBALS_ENEMY_DLIST.addhead(lord);
      lord->SPRITE_PIX = GLOBALS_FRM_32C32;
      lord->SPRITE_FRAME = GLOBALS_ENEMY_LORD;
      lord->SPRITE_X = 272;
      lord->SPRITE_Y = ((WINDOW_HEIGHT / 2) - 64);
    }
  } else {
    // location stats
    sp_string_object *string = new sp_string_object();
    if (string != 0) {
      GLOBALS_ENEMY_DLIST.addhead(string);
      string->SPRITE_USER1 = (int)location_names[type];
    }
  }
}

void game_frame_map() {
  int x, y, type, lastx, lasty, rand;
  unsigned char *map = GLOBALS_GAME_CAMPAINMAP;

  // grow map buffer
  unsigned char growbuf[sizeof(GLOBALS_GAME_CAMPAINMAP)];

  // grow plagues etc
  int cnt = GLOBALS_GAME_EVENT_COUNT;
  cnt++;
  if (cnt == 200) {
    lastx = GLOBALS_GAME_LOCATION_LASTX;
    lasty = GLOBALS_GAME_LOCATION_LASTY;

    // create any new plague events
    cnt = GLOBALS_GAME_PLAGUE_CREATE - 1;
    if (cnt < 0) {
      cnt = random(10);
      cnt -= GLOBALS_MAN_EXPERIENCE;
      rand = random(sizeof(GLOBALS_GAME_CAMPAINMAP));
      if (((rand & 0xf) != lastx) || ((rand >> 4) != lasty)) {
        if (map[rand] > FRM_PLAGUE) {
          map[rand] = FRM_PLAGUE;
        }
      }
    }
    GLOBALS_GAME_PLAGUE_CREATE = cnt;

    // create any new crusade events
    cnt = GLOBALS_GAME_CRUSADE_CREATE - 1;
    if (cnt < 0) {
      cnt = random(10);
      cnt -= GLOBALS_MAN_EXPERIENCE;
      rand = random(sizeof(GLOBALS_GAME_CAMPAINMAP));
      if (((rand & 0xf) != lastx) || ((rand >> 4) != lasty)) {
        if (map[rand] == FRM_ENEMY) {
          map[rand] = FRM_CRUSADE;
        }
      }
    }
    GLOBALS_GAME_CRUSADE_CREATE = cnt;

    // create any new rebellion events
    cnt = GLOBALS_GAME_REBELLION_CREATE - 1;
    if (cnt < 0) {
      cnt = random(10);
      cnt -= GLOBALS_MAN_EXPERIENCE;
      rand = random(sizeof(GLOBALS_GAME_CAMPAINMAP));
      if (((rand & 0xf) != lastx) || ((rand >> 4) != lasty)) {
        if (map[rand] == FRM_PLAYER) {
          map[rand] = FRM_REBELLION;
        }
      }
    }
    GLOBALS_GAME_REBELLION_CREATE = cnt;

    // destroy any plague events
    cnt = GLOBALS_GAME_PLAGUE_DESTROY;
    cnt--;
    if (cnt <= 0) {
      game_map_replace(FRM_PLAGUE, FRM_ENEMY);
      cnt = random(10);
      cnt += 10;
      cnt += GLOBALS_MAN_EXPERIENCE;
    }
    GLOBALS_GAME_PLAGUE_DESTROY = cnt;

    // destroy any crusade events
    cnt = GLOBALS_GAME_CRUSADE_DESTROY;
    cnt--;
    if (cnt <= 0) {
      game_map_replace(FRM_CRUSADE, FRM_ENEMY);
      cnt = random(10);
      cnt += 10;
      cnt += GLOBALS_MAN_EXPERIENCE;
    }
    GLOBALS_GAME_CRUSADE_DESTROY = cnt;

    // destroy any rebellion events
    cnt = GLOBALS_GAME_REBELLION_DESTROY;
    cnt--;
    if (cnt <= 0) {
      game_map_replace(FRM_REBELLION, FRM_ENEMY);
      cnt = random(10);
      cnt += 10;
      cnt += GLOBALS_MAN_EXPERIENCE;
    }
    GLOBALS_GAME_REBELLION_DESTROY = cnt;

    // grow any plagues etc
    memcpy(growbuf, map, sizeof(GLOBALS_GAME_CAMPAINMAP));
    y = 0;
    do {
      x = 0;
      do {
        type = growbuf[y * 16 + x];
        switch (type) {
        case FRM_PLAGUE:
          x--;
          if (x >= 0) {
            if ((x != lastx) || (y != lasty)) {
              if (growbuf[y * 16 + x] > FRM_PLAGUE) {
                map[y * 16 + x] = type;
              }
            }
          }
          x += 2;
          if (x < 16) {
            if ((x != lastx) || (y != lasty)) {
              if (growbuf[y * 16 + x] > FRM_PLAGUE) {
                map[y * 16 + x] = type;
              }
            }
          }
          x--;
          y--;
          if (y >= 0) {
            if ((x != lastx) || (y != lasty)) {
              if (growbuf[y * 16 + x] > FRM_PLAGUE) {
                map[y * 16 + x] = type;
              }
            }
          }
          y += 2;
          if (y < 16) {
            if ((x != lastx) || (y != lasty)) {
              if (growbuf[y * 16 + x] > FRM_PLAGUE) {
                map[y * 16 + x] = type;
              }
            }
          }
          y--;
          break;
        case FRM_CRUSADE:
          x--;
          if (x >= 0) {
            if ((x != lastx) || (y != lasty)) {
              if (growbuf[y * 16 + x] > FRM_CRUSADE) {
                map[y * 16 + x] = type;
              }
            }
          }
          x += 2;
          if (x < 16) {
            if ((x != lastx) || (y != lasty)) {
              if (growbuf[y * 16 + x] > FRM_CRUSADE) {
                map[y * 16 + x] = type;
              }
            }
          }
          x--;
          y--;
          if (y >= 0) {
            if ((x != lastx) || (y != lasty)) {
              if (growbuf[y * 16 + x] > FRM_CRUSADE) {
                map[y * 16 + x] = type;
              }
            }
          }
          y += 2;
          if (y < 16) {
            if ((x != lastx) || (y != lasty)) {
              if (growbuf[y * 16 + x] > FRM_CRUSADE) {
                map[y * 16 + x] = type;
              }
            }
          }
          y--;
          break;
        case FRM_REBELLION:
          x--;
          if (x >= 0) {
            if ((x != lastx) || (y != lasty)) {
              if (growbuf[y * 16 + x] == FRM_PLAYER) {
                map[y * 16 + x] = type;
              }
            }
          }
          x += 2;
          if (x < 16) {
            if ((x != lastx) || (y != lasty)) {
              if (growbuf[y * 16 + x] == FRM_PLAYER) {
                map[y * 16 + x] = type;
              }
            }
          }
          x--;
          y--;
          if (y >= 0) {
            if ((x != lastx) || (y != lasty)) {
              if (growbuf[y * 16 + x] == FRM_PLAYER) {
                map[y * 16 + x] = type;
              }
            }
          }
          y += 2;
          if (y < 16) {
            if ((x != lastx) || (y != lasty)) {
              if (growbuf[y * 16 + x] == FRM_PLAYER) {
                map[y * 16 + x] = type;
              }
            }
          }
          y--;
          break;
        default:
          break;
        }
        x++;
      } while (x != 16);
      y++;
    } while (y != 16);

    // count remaining territory
    cnt = game_map_count(FRM_PLAYER);
    GLOBALS_MAN_TERRITORY = cnt;

    // reset map event counter
    cnt = 0;
  }
  GLOBALS_GAME_EVENT_COUNT = cnt;

  // move location
  int controls = GLOBALS_GAME_CONTROLS;
  if (controls != 0) {
    x = GLOBALS_GAME_LOCATION_X;
    y = GLOBALS_GAME_LOCATION_Y;
    int ox = x;
    int oy = y;
    if ((GLOBALS_GAME_FLAGS & FFLAG_SELECT) == 0) {
      GLOBALS_GAME_FLAGS |= FFLAG_SELECT;
      if ((controls & FKEY_KEYB) != 0) {
        // attack location
        type = GLOBALS_GAME_CAMPAINMAP[y * 16 + x];
        if (type != FRM_PLAYER) {
          if (type == FRM_ORACLE) {
            // oracle location
            GLOBALS_GAME_STATE_NEXT = GAME_STATE_ORACLE;
            GLOBALS_GAME_STATE = GAME_STATE_TITLE;
          } else if (type >= FRM_PLAGUE) {
            // attack enemy location
            GLOBALS_GAME_STATE_NEXT = GAME_STATE_BATTLE;
            GLOBALS_GAME_STATE = GAME_STATE_TITLE;
          } else if ((type >= FRM_TWATER) && (type <= FRM_TMOUNTAIN)) {
            // temple combat location
            GLOBALS_ENEMY_LORD = FRM_FACES;
            GLOBALS_GAME_STATE_NEXT = GAME_STATE_MIND;
            GLOBALS_GAME_STATE = GAME_STATE_TITLE;
          }
        }
      } else if ((controls & FKEY_KEYA) != 0) {
        // use item
        int index = GLOBALS_ITEM_SELECTED;
        int item = GLOBALS_ITEM_INVENTORY[index];
        if ((item >= (FRM_DIAMOND + 5)) && (item <= (FRM_DIAMOND + 9))) {
          switch (item) {
          case (FRM_DIAMOND + 5):
            // gamble
            lastx = GLOBALS_GAME_LOCATION_LASTX;
            lasty = GLOBALS_GAME_LOCATION_LASTY;
            rand = random(sizeof(GLOBALS_GAME_CAMPAINMAP));
            if (((rand & 0xF) != lastx) || ((rand >> 4) != lasty)) {
              type = map[rand];
              if (type == FRM_PLAYER) {
                // player so give to random enemy
                map[rand] = random(4) + FRM_PLAGUE;
              } else if (type >= FRM_PLAGUE) {
                // enemy so give to player
                map[rand] = FRM_PLAYER;
              }
            }
            break;
          case (FRM_DIAMOND + 6):
            // autowin
            rand = random(sizeof(GLOBALS_GAME_CAMPAINMAP));
            type = map[rand];
            if (type >= FRM_PLAGUE) {
              map[rand] = FRM_PLAYER;
            }
            break;
          case (FRM_DIAMOND + 7):
            // zap plagues
            game_map_replace(FRM_PLAGUE, FRM_ENEMY);
            break;
          case (FRM_DIAMOND + 8):
            // zap crusades
            game_map_replace(FRM_CRUSADE, FRM_ENEMY);
            break;
          case (FRM_DIAMOND + 9):
            // zap rebellions
            game_map_replace(FRM_REBELLION, FRM_PLAYER);
            break;
          }
          GLOBALS_ITEM_INVENTORY[index] = 0;
          GLOBALS_MAN_TERRITORY = game_map_count(FRM_PLAYER);
        }
      } else if ((controls & FKEY_UP) != 0) {
        if (y != 0)
          y--;
      } else if ((controls & FKEY_DOWN) != 0) {
        if (y != 15)
          y++;
      } else if ((controls & FKEY_LEFT) != 0) {
        if (x != 0)
          x--;
      } else if ((controls & FKEY_RIGHT) != 0) {
        if (x != 15)
          x++;
      }
    }

    // valid movements only
    if (GLOBALS_GAME_LOCATION_OX != -1) {
      // has one legal move
      ox = GLOBALS_GAME_LOCATION_OX;
      oy = GLOBALS_GAME_LOCATION_OY;
      if ((x == ox) && (y == oy)) {
        GLOBALS_GAME_LOCATION_OX = -1;
      newlocation:
        GLOBALS_GAME_LOCATION_X = x;
        GLOBALS_GAME_LOCATION_Y = y;
        type = GLOBALS_GAME_CAMPAINMAP[y * 16 + x];
        if (type == FRM_PLAYER) {
          // save last known player location
          GLOBALS_GAME_LOCATION_LASTX = x;
          GLOBALS_GAME_LOCATION_LASTY = y;
        }
        sp_sight *sight = (sp_sight *)GLOBALS_MAN_DLIST.gettail();
        sight->SPRITE_X = ((x * 8) + 92);
        sight->SPRITE_Y = ((WINDOW_HEIGHT / 2) - 64) + ((y * 8) - 4);
      }
    } else {
      type = GLOBALS_GAME_CAMPAINMAP[y * 16 + x];
      if (type == FRM_PLAYER) {
        ox = -1;
      } else if (type >= FRM_PLAGUE) {
      } else if (type < FRM_WATER) {
        ox = -1;
      } else {
        type = item_carry(type + (FRM_DIAMOND - FRM_WATER));
        if (type != -1) {
          ox = -1;
        }
      }
      GLOBALS_GAME_LOCATION_OX = ox;
      GLOBALS_GAME_LOCATION_OY = oy;
      goto newlocation;
    }
  } else {
    // no key pressed
    GLOBALS_GAME_FLAGS &= ~FFLAG_SELECT;
  }

  // update panel and enemy panel
  item_select();
  game_enemy_map();
}

void game_frame_scores() {
  // process mr smiths
  game_mr_smiths();

  if (GLOBALS_GAME_CONTROLS != 0) {
    // key pressed
    if ((GLOBALS_GAME_FLAGS & FFLAG_SELECT) == 0) {
      // nothing pressed
      GLOBALS_GAME_FLAGS |= FFLAG_SELECT;

      // go to menu via title
      GLOBALS_GAME_STATE_NEXT = GAME_STATE_MENU;
      GLOBALS_GAME_STATE = GAME_STATE_TITLE;
    }
  } else {
    // set nothing pressed
    GLOBALS_GAME_FLAGS &= ~FFLAG_SELECT;

    // displayed long enough ?
    int cnt = GLOBALS_GAME_COUNT;
    cnt++;
    GLOBALS_GAME_COUNT = cnt;
    if (cnt == 256) {
      // go to menu via title
      GLOBALS_GAME_STATE_NEXT = GAME_STATE_MENU;
      GLOBALS_GAME_STATE = GAME_STATE_TITLE;
    }
  }
}

void game_frame_hiscore() {
  // process mr smiths
  game_mr_smiths();

  // selections
  int controls = GLOBALS_GAME_CONTROLS;
  if (controls != 0) {
    sp_enter_object *enter = (sp_enter_object *)GLOBALS_FX_DLIST.gethead();
    char *text = (char *)enter->SPRITE_USER1;
    char *initials = (char *)enter->SPRITE_USER2;
    if ((GLOBALS_GAME_FLAGS & FFLAG_SELECT) == 0) {
      char ch;
      char *letter;
      int score, index;
      Sprite *sfx;
      GLOBALS_GAME_FLAGS |= FFLAG_SELECT;
      if (text == txt_nohiscore)
        goto nohiscore;
      if (text == txt_intutor)
        goto nohiscore;
      if ((controls & FKEY_LEFT) != 0) {
        text--;
        if (text < txt_lettab) {
          text = txt_lettab + (sizeof(txt_lettab)) - 1;
        }
      } else if ((controls & FKEY_RIGHT) != 0) {
        text++;
        if (text >= (txt_lettab + sizeof(txt_lettab))) {
          text = txt_lettab;
        }
      } else if ((controls & FKEY_KEYB) != 0) {
        // explosion on letter
        sfx = (Sprite *)GLOBALS_FX_DLIST.gettail();
        dt_exp_fx(sfx);
        letter = (text + 16);
        if (letter >= txt_lettab + (sizeof(txt_lettab))) {
          letter = txt_lettab + (letter - (txt_lettab + (sizeof(txt_lettab))));
        }
        ch = *letter;
        if (ch == '[') {
          // backspace
          if (initials != GLOBALS_MAN_INITIALS) {
            *--initials = 0;
          }
        } else if (ch == ']') {
          // enter, so insert initials in hi score table
          index = 0;
          score = GLOBALS_MAN_SCORE;
          while (score <= GLOBALS_SCORE[index]) {
            index++;
          }
          memmove(
              &GLOBALS_SCORE[index + 1], &GLOBALS_SCORE[index],
              (sizeof(GLOBALS_SCORE) - (index * sizeof(int)) - sizeof(int)));
          memmove(&GLOBALS_SCORE_TXT[index + 1], &GLOBALS_SCORE_TXT[index],
                  (sizeof(GLOBALS_SCORE_TXT) - (index * 16) - 16));
          GLOBALS_SCORE[index] = score;
          memcpy(&GLOBALS_SCORE_TXT[index][0], "       (   )", 12);
          memcpy(&GLOBALS_SCORE_TXT[index][0], GLOBALS_MAN_NAME,
                 strlen(GLOBALS_MAN_NAME));
          memcpy(&GLOBALS_SCORE_TXT[index][8], GLOBALS_MAN_INITIALS,
                 strlen(GLOBALS_MAN_INITIALS));
        nohiscore:
          // go to scores via title
          GLOBALS_GAME_STATE_NEXT = GAME_STATE_SCORES;
          GLOBALS_GAME_STATE = GAME_STATE_TITLE;
        } else {
          // insert ch
          if (initials != (&GLOBALS_MAN_INITIALS[3])) {
            initials++[0] = ch;
            initials[0] = 0;
          }
        }
      }
    }
    enter->SPRITE_USER1 = (int)text;
    enter->SPRITE_USER2 = (int)initials;
  } else {
    // set nothing pressed
    GLOBALS_GAME_FLAGS &= ~FFLAG_SELECT;
  }
}

void game_frame_battle() {
  // recording
  GLOBALS_RECORD->recordkeys(GLOBALS_GAME_CONTROLS);

  init_mine();
  init_enemy();

  // check if man is going through death or not
  if (sprite_find_list_types(&GLOBALS_MAN_DLIST, FTP_MAN)) {
    // check for lost
    if (GLOBALS_ENEMY_STACKINDEX ==
        (sizeof(GLOBALS_ENEMY_STACK) / sizeof(int))) {
      GLOBALS_GAME_STATE = GAME_STATE_BATTLE_LOST;
      game_swap_demo();
      return;
    }

    // check for won
    if ((GLOBALS_ENEMY_DLIST.gethead()->getsucc()) == 0) {
      if ((GLOBALS_GAME_FLAGS & FFLAG_ENEMY) != 0) {
        if (GLOBALS_ENEMY_STACKINDEX == 0) {
          GLOBALS_GAME_STATE = GAME_STATE_BATTLE_WON;
          game_swap_demo();
          return;
        }
      }
    }
  }

  // check for death sequence ended
  if ((GLOBALS_MAN_DLIST.gethead()->getsucc()) == 0) {
    // dead so to hiscore after title
    GLOBALS_GAME_STATE_NEXT = GAME_STATE_HISCORE;
    GLOBALS_GAME_STATE = GAME_STATE_TITLE;
    game_swap_demo();
  }
}

void game_frame_battle_won() {
  // delay for a while
  int cnt = GLOBALS_GAME_COUNT;
  cnt++;
  GLOBALS_GAME_COUNT = cnt;
  if (cnt == 96) {
    // won battle
    int level = GLOBALS_GAME_LEVEL;
    int state;
    switch (level) {
    case GAME_LEVEL_FIELD:
      level = GAME_LEVEL_SEIGE;
      state = GAME_STATE_BATTLE;
      break;
    case GAME_LEVEL_SEIGE:
      state = GAME_STATE_MIND;
      break;
    default:
      level = GAME_LEVEL_FIELD;
      state = GAME_STATE_BATTLE;
    }
    GLOBALS_GAME_LEVEL = level;
    GLOBALS_GAME_STATE_NEXT = state;
    GLOBALS_GAME_STATE = GAME_STATE_TITLE;
  }
}

void game_frame_battle_lost() {
  // delay for a while
  int cnt = GLOBALS_GAME_COUNT;
  cnt++;
  GLOBALS_GAME_COUNT = cnt;
  if (cnt == 96) {
    // loose battle
    int level = GLOBALS_GAME_LEVEL;
    int state;
    switch (level) {
    case GAME_LEVEL_FIELD:
      level = GAME_LEVEL_DEFEND;
      state = GAME_STATE_BATTLE;
      break;
    case GAME_LEVEL_SEIGE:
      level = GAME_LEVEL_FIELD;
      state = GAME_STATE_BATTLE;
      break;
    default:
      state = GAME_STATE_MIND;
    }
    GLOBALS_GAME_LEVEL = level;
    GLOBALS_GAME_STATE_NEXT = state;
    GLOBALS_GAME_STATE = GAME_STATE_TITLE;
  }
}

void game_frame_mind() {
  // new item ?
  int cnt = GLOBALS_GAME_COUNT;
  cnt--;
  if (cnt == 0) {
    // kill any existing items
    sprite_kill_list(&GLOBALS_ITEM_DLIST);

    // new item
    int location = GLOBALS_GAME_LOCATION_X + (GLOBALS_GAME_LOCATION_Y * 16);
    int type = GLOBALS_GAME_CAMPAINMAP[location];
    if ((type >= FRM_TWATER) && (type <= FRM_TMOUNTAIN)) {
      // get next item from table
      int itemindex = GLOBALS_MIND_ITEMINDEX;
      if (itemindex == 0) {
        type -= FRM_TWATER;
        type += FRM_DIAMOND;
      } else {
        type = minditems[itemindex];
      }
      itemindex++;
      if (itemindex == sizeof(minditems) / sizeof(int)) {
        itemindex = 0;
      }
      GLOBALS_MIND_ITEMINDEX = itemindex;
    } else {
      // allways blue spell in normal mind combat
      type = FRM_SPELLS + 5;
    }
    sp_talisman_item *item = new sp_talisman_item();
    if (item != 0) {
      GLOBALS_ITEM_DLIST.addtail(item);
      int x, y;
      getxy_inner(item->SPRITE_W, item->SPRITE_H, x, y);
      item->SPRITE_X = x;
      item->SPRITE_Y = y;
      item->SPRITE_FRAME = type;

      // mr smith drops item
      sp_smith *sfx = new sp_smith();
      if (sfx != 0) {
        GLOBALS_BODY_DLIST.addtail(sfx);
        sfx->SPRITE_X = (x - 8);
        sfx->SPRITE_Y = (y - 8);
      }
    }

    // reset count
    cnt = 96;
  }
  GLOBALS_GAME_COUNT = cnt;

  // check lost
  if (GLOBALS_MAN_POWER == 0) {
    GLOBALS_GAME_STATE = GAME_STATE_MIND_LOST;
  }

  // check won
  if ((GLOBALS_ENEMY_DLIST.gethead())->getsucc() == 0) {
    GLOBALS_GAME_STATE = GAME_STATE_MIND_WON;
  }
}

void game_frame_mind_won() {
  // delay for a while
  int cnt = GLOBALS_GAME_COUNT;
  cnt++;
  GLOBALS_GAME_COUNT = cnt;
  if (cnt == 64) {
    // won mind
    int x = GLOBALS_GAME_LOCATION_X;
    int y = GLOBALS_GAME_LOCATION_Y;
    unsigned char *map = GLOBALS_GAME_CAMPAINMAP;
    int location = (y * 16) + x;
    int type = map[location];
    int state = GAME_STATE_MAP;
    if (type >= FRM_PLAGUE) {
      if (GLOBALS_GAME_LEVEL == GAME_LEVEL_SEIGE) {
        // won terrain location
        GLOBALS_MAN_TERRITORY++;
        map[location] = FRM_PLAYER;
        GLOBALS_GAME_LOCATION_LASTX = x;
        GLOBALS_GAME_LOCATION_LASTY = y;
        GLOBALS_GAME_LOCATION_OX = -1;
        GLOBALS_MAN_SCORE += GLOBALS_ENEMY_POPULARITY;
      } else {
        // won defending mind combat
        GLOBALS_GAME_LEVEL = GAME_LEVEL_DEFEND;
        state = GAME_STATE_BATTLE;
      }
    }
    GLOBALS_GAME_STATE_NEXT = state;
    GLOBALS_GAME_STATE = GAME_STATE_TITLE;

    // restore power value
    GLOBALS_MAN_POWER = GLOBALS_MIND_POWER;
  }
}

void game_frame_mind_lost() {
  // delay for a while
  int cnt = GLOBALS_GAME_COUNT;
  cnt++;
  GLOBALS_GAME_COUNT = cnt;
  if (cnt == 64) {
    // lost mind
    unsigned char *map = GLOBALS_GAME_CAMPAINMAP;
    int location = (GLOBALS_GAME_LOCATION_Y * 16) + GLOBALS_GAME_LOCATION_X;
    int type = map[location];
    int state = GAME_STATE_MAP;
    if (type >= FRM_PLAGUE) {
      if (GLOBALS_GAME_LEVEL == GAME_LEVEL_DEFEND) {
        // lost terrain location
        cnt = GLOBALS_MAN_TERRITORY;
        cnt--;
        if (cnt == 0) {
          // game over so to hiscore via title
          state = GAME_STATE_HISCORE;
        } else {
          // lost to attacked type
          GLOBALS_MAN_TERRITORY = cnt;
          location =
              (GLOBALS_GAME_LOCATION_LASTY * 16) + GLOBALS_GAME_LOCATION_LASTX;
          map[location] = type;
          GLOBALS_GAME_LOCATION_OX = -1;
          int rand = random(sizeof(GLOBALS_GAME_CAMPAINMAP));
          for (;;) {
            if (map[rand] == FRM_PLAYER)
              break;
            rand = ((rand + 1) & 0xFF);
          }
          GLOBALS_GAME_LOCATION_X = (rand & 0xF);
          GLOBALS_GAME_LOCATION_Y = (rand >> 4);
          GLOBALS_GAME_LOCATION_LASTX = (rand & 0xF);
          GLOBALS_GAME_LOCATION_LASTY = (rand >> 4);
        }
      } else {
        // lost siege mind combat
        GLOBALS_GAME_LEVEL = GAME_LEVEL_SEIGE;
        state = GAME_STATE_BATTLE;
      }
    } else {
      // lost all talismens if temple mind combat
      int index = 0;
      do {
        int item = GLOBALS_ITEM_INVENTORY[index];
        if ((item >= FRM_DIAMOND) && (item <= FRM_DIAMOND + 9)) {
          GLOBALS_ITEM_INVENTORY[index] = 0;
        }
        index++;
      } while (index != (sizeof(GLOBALS_ITEM_INVENTORY) / sizeof(int)));
    }
    GLOBALS_GAME_STATE_NEXT = state;
    GLOBALS_GAME_STATE = GAME_STATE_TITLE;

    // restore power value
    GLOBALS_MAN_POWER = GLOBALS_MIND_POWER;
  }
}

void game_frame_credits() {
  // process mr smiths
  game_mr_smiths();

  if (GLOBALS_GAME_CONTROLS != 0) {
    // key pressed
    if ((GLOBALS_GAME_FLAGS & FFLAG_SELECT) == 0) {
      // nothing pressed
      GLOBALS_GAME_FLAGS |= FFLAG_SELECT;

      GLOBALS_GAME_STATE = GAME_STATE_MENU;
    }
  } else {
    // set nothing pressed
    GLOBALS_GAME_FLAGS &= ~FFLAG_SELECT;
  }
}

void game_frame_oracle() {
  // scroll map
  mindmap *map = GLOBALS_MIND;
  int x1, y1, x2, y2;
  map->getoffsets(x1, y1, x2, y2);
  y1 = ((y1 + 1) & 0xf);
  y2 = ((y2 - 1) & 0x1f);
  map->setoffsets(x1, y1, x2, y2);

  // process mr smiths
  game_mr_smiths();

  // wait for key release
  if (GLOBALS_GAME_CONTROLS != 0) {
    if ((GLOBALS_GAME_FLAGS & FFLAG_SELECT) == 0) {
      GLOBALS_GAME_FLAGS |= FFLAG_SELECT;

      // go to map via title
      GLOBALS_GAME_STATE_NEXT = GAME_STATE_MAP;
      GLOBALS_GAME_STATE = GAME_STATE_TITLE;
    }
  } else {
    // set nothing pressed
    GLOBALS_GAME_FLAGS &= ~FFLAG_SELECT;
  }
}

void game_frame_demo() {
  // playback
  int keys = GLOBALS_PLAY->playkeys();
  if ((keys != -1) && (GLOBALS_GAME_CONTROLS1 == 0) &&
      sprite_find_list_types(&GLOBALS_MAN_DLIST, FTP_MAN)) {
    GLOBALS_GAME_CONTROLS = keys;
    init_mine();
    init_enemy();
  } else {
    // load current state
    GLOBALS_GAME_CONTROLS = 0;
    GLOBALS_RECORD->loadstate();
    GLOBALS_GAME_STATE_NEXT = GAME_STATE_SCORES;
    GLOBALS_GAME_STATE = GAME_STATE_TITLE;
  }
}

// game functions

int random(int range) {
  unsigned int seed = GLOBALS_GAME_SEED;
  seed *= 17;
  seed ^= 0xa5a5a5a5;
  GLOBALS_GAME_SEED = seed;
  seed = seed >> 16;
  range *= seed;
  return (range >> 16);
}

void game_swap_demo() {
  // flush last keys through
  autoplay *rauto = GLOBALS_RECORD;
  rauto->recordkeys(-1);

  // better level score than demo ?
  autoplay *pauto = GLOBALS_PLAY;
  int dscore = pauto->getscore();
  int score = GLOBALS_MAN_SCORE - GLOBALS_MAN_START_SCORE;
  if (score > dscore) {
    // new best battle score so save as demo mode
    GLOBALS_RECORD = pauto;
    GLOBALS_PLAY = rauto;
    rauto->setscore(score);
    rauto->save();
  }
}

char *game_pick_part(char *buffer) {
  int rand = random(2);
  int chars;
  const char *string;
  if (rand == 0) {
    // 3 letter sylabel
    rand = random(17);
    chars = 3;
    string = "GARRAGVARGERMONGORBALFAGGAFBANVERLATTOGROGTATMANRUD";
  } else {
    // 2 letter sylabel
    rand = random(7);
    chars = 2;
    string = "GOATITISETINMI";
  }
  rand *= chars;
  string += rand;
  do {
    *buffer = *string;
    string++;
    buffer++;
    chars--;
  } while (chars != 0);
  return (buffer);
}

void game_generate_player_info() {
  // set globals
  GLOBALS_MAN_POWER = (MAXPOWER / 2);
  GLOBALS_MAN_STRENGTH = MAXSTRENGTH;
  GLOBALS_MAN_SCORE = 0;
  GLOBALS_GAME_TIMEZONE = 0;

  // setup item inventory
  GLOBALS_ITEM_SELECTED = 0;
  GLOBALS_ITEM_USEAGE[0] = 48;
  memset(GLOBALS_ITEM_INVENTORY, 0, sizeof(GLOBALS_ITEM_INVENTORY));
  GLOBALS_ITEM_INVENTORY[0] = FRM_SHIELD;

  // set campain map
  memcpy(GLOBALS_GAME_CAMPAINMAP, campainmap, sizeof(GLOBALS_GAME_CAMPAINMAP));
  unsigned char *map = GLOBALS_GAME_CAMPAINMAP;
  int rand = random(sizeof(GLOBALS_GAME_CAMPAINMAP));
  for (;;) {
    if (map[rand] == FRM_ENEMY)
      break;
    rand++;
    rand &= 0xff;
  }
  map[rand] = FRM_PLAYER;
  GLOBALS_GAME_LOCATION_X = (rand & 0xF);
  GLOBALS_GAME_LOCATION_Y = (rand >> 4);
  GLOBALS_GAME_LOCATION_LASTX = (rand & 0xF);
  GLOBALS_GAME_LOCATION_LASTY = (rand >> 4);
  GLOBALS_GAME_LOCATION_OX = -1;
  GLOBALS_MAN_TERRITORY = 1;

  // pick random name
  char *buffer = GLOBALS_MAN_NAME;
  buffer = game_pick_part(buffer);
  buffer = game_pick_part(buffer);
  *buffer = 0;

  // pick random homeland
  buffer = GLOBALS_MAN_HOMELAND;
  rand = random(2);
  rand += 2;
  do {
    buffer = game_pick_part(buffer);
    rand--;
  } while (rand != 0);
  *buffer = 0;

  // pick random cult
  rand = random(sizeof(cult_names) / sizeof(int));
  GLOBALS_MAN_CULT = cult_names[rand];
  GLOBALS_MAN_CULT_INDEX = rand;
  rand += FRM_BANNERS;
  GLOBALS_MAN_BANNER = rand;

  // set enemy info for this location
  game_generate_enemy_info();
}

void game_generate_enemy_info() {
  // save current seed, set temp seed
  int seed = GLOBALS_GAME_SEED;
  int location = (GLOBALS_GAME_LOCATION_X + (GLOBALS_GAME_LOCATION_Y * 16));
  GLOBALS_GAME_SEED = (location + GLOBALS_GAME_TIMEZONE);

  // pick random kingdom
  char *buffer = GLOBALS_ENEMY_KINGDOM;
  int rand = random(2);
  rand += 2;
  do {
    buffer = game_pick_part(buffer);
    rand--;
  } while (rand != 0);
  *buffer = 0;

  // pick random cult
  rand = random(sizeof(cult_names) / sizeof(int));
  GLOBALS_ENEMY_CULT = cult_names[rand];
  rand += FRM_BANNERS;
  GLOBALS_ENEMY_BANNER = rand;

  // pick random wizardlord
  rand = random(sizeof(lord_names) / sizeof(int));
  GLOBALS_ENEMY_WIZARDLORD = lord_names[rand];
  rand *= 2;
  rand += FRM_FACES;
  GLOBALS_ENEMY_LORD = rand;

  // pick random mine maximum
  rand = random(3);
  rand++;
  GLOBALS_MINE_MAX = rand;

  // pick random enemy maximum and population
  if (GLOBALS_GAME_MENU_DIFFICULTY == txt_menu_easy) {
    // easy
    rand = 15000;
  } else {
    // hard
    rand = 30000;
  }
  rand = random(rand);
  GLOBALS_ENEMY_POPULATION = (rand + 10000);
  rand /= 10000;
  GLOBALS_ENEMY_MAX = (rand + 2);
  switch (rand) {
  case 0:
    rand = RGBGREEN;
    break;
  case 1:
    rand = RGBYELLOW;
    break;
  default:
    rand = RGBRED;
  }
  GLOBALS_ENEMY_INFOCOL = rand;
  GLOBALS_ENEMY_INFODCOL = ((rand >> 1) & 0x7f7f7f);

  // pick random wizard shot
  rand = random(2);
  GLOBALS_ENEMY_WIZSHOT = rand;

  // pick random army or plague
  int type = GLOBALS_GAME_CAMPAINMAP[location];
  rand = (sizeof(armytable) / sizeof(int)) - 1;
  if (type != FRM_PLAGUE) {
    rand = random(rand);
  }
  GLOBALS_ENEMY_ARMY = rand;
  GLOBALS_ENEMY_WARBAND = army_names[rand];

  // set level
  if (type == FRM_REBELLION) {
    rand = GAME_LEVEL_DEFEND;
  } else {
    rand = GAME_LEVEL_FIELD;
  }
  GLOBALS_GAME_LEVEL = rand;

  // pick popularity
  rand = GLOBALS_MAN_BANNER;
  rand -= GLOBALS_ENEMY_BANNER;
  if (rand < 0) {
    rand = -rand;
  }
  GLOBALS_ENEMY_POPULARITY = popularity[rand];

  // restore current seed
  GLOBALS_GAME_SEED = seed;
}

void game_mr_smiths() {
  // process mr smiths
  if ((GLOBALS_GAME_FRAME_COUNT & 3) == 0) {
    sp_smith *sfx = new sp_smith();
    if (sfx != 0) {
      GLOBALS_BODY_DLIST.addhead(sfx);
      sfx->SPRITE_X = random(WINDOW_WIDTH - 32);
      sfx->SPRITE_Y = random(WINDOW_HEIGHT - 32);
    }
  }
}

void game_set_map(int level) {
  //-1=no map showing
  map *land = GLOBALS_LAND;
  mindmap *mind = GLOBALS_MIND;
  land->clrmap();
  mind->clrmap();
  int rand;
  switch (level) {
  case GAME_LEVEL_FIELD:
    rand = random(3);
    if (rand == 0) {
      land->setmap(GLOBALS_BLOCKS1, fieldmap1, mapflags1, MAP_WIDTH,
                   MAP_HEIGHT);
    } else if (rand == 1) {
      land->setmap(GLOBALS_BLOCKS1, fieldmap2, mapflags1, MAP_WIDTH,
                   MAP_HEIGHT);
    } else {
      land->setmap(GLOBALS_BLOCKS1, fieldmap3, mapflags1, MAP_WIDTH,
                   MAP_HEIGHT);
    }
    break;
  case GAME_LEVEL_SEIGE:
    land->setmap(GLOBALS_BLOCKS2, seigemap, mapflags2, MAP_WIDTH, MAP_HEIGHT);
    break;
  case GAME_LEVEL_DEFEND:
    land->setmap(GLOBALS_BLOCKS2, defendmap, mapflags2, MAP_WIDTH, MAP_HEIGHT);
    break;
  case GAME_LEVEL_MIND:
    mind->setmap(GLOBALS_BLOCKS3);
    break;
  default:
    break;
  }
}

void game_set_level(int level) {
  // add man sprite
  Sprite *man = new sp_man();
  if (man != 0) {
    GLOBALS_MAN_DLIST.addhead(man);
    man->SPRITE_X = 8;
    man->SPRITE_Y = 208;
  }

  // set correct map
  int mx, my, ex, ey;
  switch (level) {
  case GAME_LEVEL_SEIGE:
    // siege battle
    game_set_map(GAME_LEVEL_SEIGE);
    mx = 16;
    my = 192;
    ex = 2016;
    ey = 0;
    break;
  case GAME_LEVEL_DEFEND:
    // defending battle
    game_set_map(GAME_LEVEL_DEFEND);
    mx = 16;
    my = 0;
    ex = 2016;
    ey = 192;
    break;
  default:
    // field battle
    game_set_map(GAME_LEVEL_FIELD);
    mx = 16;
    my = 128;
    ex = 2016;
    ey = 128;
  }

  // man banner
  sp_banner *ban = new sp_banner();
  if (ban != 0) {
    GLOBALS_BANS_DLIST.addhead(ban);
    ban->SPRITE_FRAME = GLOBALS_MAN_BANNER;
    ban->SPRITE_X = mx;
    ban->SPRITE_Y = my;
  }

  // enemy banner
  ban = new sp_banner();
  if (ban != 0) {
    GLOBALS_BANS_DLIST.addhead(ban);
    ban->SPRITE_FRAME = GLOBALS_ENEMY_BANNER;
    ban->SPRITE_TYPE = FTP_EMEMY_BANNER;
    ban->SPRITE_X = ex;
    ban->SPRITE_Y = ey;
  }
}

void update_panel() {
  // draw enemy stack indicator
  int val = GLOBALS_ENEMY_STACKINDEX * 2;
  GLOBALS_FRM_32C32->blit(32 - 3, WINDOW_HEIGHT + (8 + (32 - val)), 0,
                          ((FRM_FLAG * 32 + 32) - val), 32, val);
  GLOBALS_FRM_32C32->blit(256 + 6, WINDOW_HEIGHT + (8 + (32 - val)), 0,
                          ((FRM_FLAG * 32 + 32) - val), 32, val);

  // draw power indicator
  val = GLOBALS_MAN_POWER;
  if (val > MAXPOWER) {
    val = MAXPOWER;
  } else if (val < 0) {
    val = 0;
  }
  val = ((val * 32) / MAXPOWER);
  GLOBALS_FRM_32C32->blit(160 - 32, WINDOW_HEIGHT + (40 + (32 - val)), 0,
                          ((FRM_POWER * 32 + 32) - val), 32, val);

  // draw strength indicator
  val = GLOBALS_MAN_STRENGTH;
  if (val > MAXSTRENGTH) {
    val = MAXSTRENGTH;
  } else if (val < 0) {
    val = 0;
  }
  val = ((val * 32) / MAXSTRENGTH);
  GLOBALS_FRM_32C32->blit(160, WINDOW_HEIGHT + (40 + (32 - val)), 0,
                          ((FRM_STRENGTH * 32 + 32) - val), 32, val);

  // draw items and selector
  int index = 0;
  int item;
  int x = ((SCREEN_WIDTH / 2) - (16 * 4));
  int selected = GLOBALS_ITEM_SELECTED;
  do {
    item = GLOBALS_ITEM_INVENTORY[index];
    if (item != 0) {
      GLOBALS_FRM_16X16->blit(x, WINDOW_HEIGHT + 8, 0, (item * 16), 16, 16);
    }
    if (index == selected) {
      GLOBALS_FRM_16X16->blit(x, WINDOW_HEIGHT + 8, 0, (FRM_SIGHT * 16), 16,
                              16);
    }
    x += 16;
    index++;
  } while (index != (sizeof(GLOBALS_ITEM_INVENTORY) / sizeof(int)));

  // draw blood drips on axes
  int y = GLOBALS_GAME_DRIP_Y;
  if (y != 0) {
    int val = GLOBALS_GAME_DRIP_YVEL;
    y += val;
    val++;
    GLOBALS_GAME_DRIP_YVEL = val;
    if (y >= SCREEN_HEIGHT) {
      GLOBALS_GAME_DRIP_Y = 0;
      GLOBALS_GAME_DRIP_YVEL = 0;
    } else {
      int sx, sy;
      GLOBALS_FRM_16X16->getframe(FRM_BLOODDRIP, sx, sy);
      GLOBALS_GAME_DRIP_Y = y;
      GLOBALS_FRM_16X16->blit(2, y, sx, sy, 16, 16);
      GLOBALS_FRM_16X16->blit((SCREEN_WIDTH - 18), y, sx, sy, 16, 16);
    }
  }
}

void game_proc_dlists() {
  // process sprites
  sprite_proc_list(&GLOBALS_MAN_DLIST);
  sprite_proc_list(&GLOBALS_BANS_DLIST);
  sprite_proc_list(&GLOBALS_ITEM_DLIST);
  sprite_proc_list(&GLOBALS_ENEMY_DLIST);
  sprite_proc_list(&GLOBALS_MISSILE_DLIST);
  sprite_proc_list(&GLOBALS_BODY_DLIST);
  sprite_proc_list(&GLOBALS_FX_DLIST);
}

void game_free_dlists() {
  // process sprites
  sprite_free_list(&GLOBALS_MAN_DLIST);
  sprite_free_list(&GLOBALS_BANS_DLIST);
  sprite_free_list(&GLOBALS_ITEM_DLIST);
  sprite_free_list(&GLOBALS_ENEMY_DLIST);
  sprite_free_list(&GLOBALS_MISSILE_DLIST);
  sprite_free_list(&GLOBALS_BODY_DLIST);
  sprite_free_list(&GLOBALS_FX_DLIST);
}

Sprite *create_item_type(int type) {
  Sprite *node;
  switch (type) {
  case FRM_GOLD:
    node = (Sprite *)new sp_gold_item();
    break;
  case FRM_SHIELD:
    node = (Sprite *)new sp_mase_item();
    break;
  case FRM_SHIELD + 1:
    node = (Sprite *)new sp_big_bow_item();
    break;
  case FRM_SHIELD + 2:
    node = (Sprite *)new sp_small_bow_item();
    break;
  case FRM_SHIELD + 3:
    node = (Sprite *)new sp_naptha_item();
    break;
  case FRM_SHIELD + 4:
    node = (Sprite *)new sp_helper_item();
    break;
  case FRM_SHIELD + 5:
    node = (Sprite *)new sp_super_mase_item();
    break;
  case FRM_SHIELD + 6:
    node = (Sprite *)new sp_super_helper_item();
    break;
  case FRM_SPELLS:
    node = (Sprite *)new sp_yellow_spell_item();
    break;
  case FRM_SPELLS + 1:
    node = (Sprite *)new sp_black_spell_item();
    break;
  case FRM_SPELLS + 2:
    node = (Sprite *)new sp_green_spell_item();
    break;
  case FRM_SPELLS + 3:
    node = (Sprite *)new sp_white_spell_item();
    break;
  case FRM_SPELLS + 4:
    node = (Sprite *)new sp_red_spell_item();
    break;
  case FRM_SPELLS + 5:
    node = (Sprite *)new sp_blue_spell_item();
    break;
  default:
    node = 0;
  }
  return (node);
}

Sprite *create_enemy_type(int type) {
  Sprite *node;
  switch (type) {
  case BFTP_HORSE:
    node = (Sprite *)new sp_horse();
    break;
  case BFTP_SPEARMAN:
    node = (Sprite *)new sp_spearman();
    break;
  case BFTP_WIZARD:
    node = (Sprite *)new sp_wizard();
    break;
  case BFTP_FOOTMAN:
    node = (Sprite *)new sp_footman();
    break;
  case BFTP_KNIGHT:
    node = (Sprite *)new sp_knight();
    break;
  case BFTP_BALISTA:
    node = (Sprite *)new sp_balista();
    break;
  case BFTP_CANNON:
    node = (Sprite *)new sp_cannon();
    break;
  case BFTP_OIL:
    node = (Sprite *)new sp_oil();
    break;
  case BFTP_BOARRIDER:
    node = (Sprite *)new sp_boarrider();
    break;
  case BFTP_CARPET:
    node = (Sprite *)new sp_carpet();
    break;
  case BFTP_TOWER:
    node = (Sprite *)new sp_tower();
    break;
  case BFTP_MONK:
    node = (Sprite *)new sp_monk();
    break;
  case BFTP_BESERK:
    node = (Sprite *)new sp_beserk();
    break;
  case BFTP_SKELETON_MONK:
    node = (Sprite *)new sp_skeleton_monk();
    break;
  case BFTP_SKELETON_HORSE:
    node = (Sprite *)new sp_skeleton_horse();
    break;
  case BFTP_SKELETON:
    node = (Sprite *)new sp_skeleton();
    break;
  default:
    node = 0;
  }
  return (node);
}

void getxy(int w, int h, int dir, int &x, int &y) {
  int x1, y1, flags1, flags2;
  map *land;

  x = -GLOBALS_SCROLL_X;
  y1 = -GLOBALS_SCROLL_Y;
  w--;
  if (dir == 1) {
    x += (WINDOW_WIDTH - 1);
    x1 = (x + w);
  } else {
    x1 = x;
    x -= w;
  }
  y = random(TILE_HEIGHT * ((WINDOW_HEIGHT / TILE_HEIGHT) - 1));
  y = ((y + y1) & -TILE_HEIGHT);
  land = GLOBALS_LAND;
  for (;;) {
    if (x < 0) {
      flags1 = FMAP_STAND;
    } else {
      flags1 = land->getmapflags(x, y);
    }
    if (x1 >= (TILE_WIDTH * MAP_WIDTH)) {
      flags2 = FMAP_STAND;
    } else {
      flags2 = land->getmapflags(x1, y);
    }
    if (((flags1 & FMAP_STAND) != 0) && ((flags2 & FMAP_STAND) != 0)) {
      if (((flags1 & FMAP_CLIMB) == 0) && ((flags2 & FMAP_CLIMB) == 0))
        break;
    }
    y += TILE_HEIGHT;
  }
  y -= h;
  if ((y < y1) || (y >= (y1 + WINDOW_HEIGHT))) {
    // invalid cords
    x = -1;
    y = -1;
  }
}

void getxy_outer(int w, int h, int &x, int &y) {
  int rand = random(4);
  if (rand == 0) {
    // top edge
    rand = random((WINDOW_WIDTH + (2 * w)));
    x = (rand - w);
    y = -h;
  } else if (rand == 1) {
    // bottom edge
    rand = random((WINDOW_WIDTH + (2 * w)));
    x = (rand - w);
    y = WINDOW_HEIGHT;
  } else if (rand == 2) {
    // left edge
    rand = random((WINDOW_HEIGHT + (2 * h)));
    y = (rand - h);
    x = -w;
  } else {
    // right edge
    rand = random((WINDOW_HEIGHT + (2 * h)));
    y = (rand - h);
    x = WINDOW_WIDTH;
  }
}

void getxy_inner(int w, int h, int &x, int &y) {
  int rand = random(4);
  if (rand == 0) {
    // top edge
    x = random(WINDOW_WIDTH - w);
    y = 0;
  } else if (rand == 1) {
    // bottom edge
    x = random(WINDOW_WIDTH - w);
    y = (WINDOW_HEIGHT - h);
  } else if (rand == 2) {
    // left edge
    y = random(WINDOW_HEIGHT - h);
    x = 0;
  } else {
    // right edge
    y = random(WINDOW_HEIGHT - h);
    x = (WINDOW_WIDTH - w);
  }
}

void init_mine() {
  sp_man *man;
  Sprite *hit;
  int dir, x, y, cnt;

  if (GLOBALS_GAME_LEVEL != GAME_LEVEL_DEFEND) {
    cnt = GLOBALS_MINE_COUNT;
    if (cnt != GLOBALS_MINE_MAX) {
      if ((GLOBALS_GAME_FLAGS & FFLAG_MINE) != 0)
        goto clrmine;
      man = (sp_man *)sprite_find_list_types(&GLOBALS_MAN_DLIST, FTP_MAN);
      if (man) {
        dir = man->SPRITE_DIR;
        getxy(16, 8, dir, x, y);
        if ((x >= 0) && (y >= 0) && (x < ((MAP_WIDTH * TILE_WIDTH) - 16))) {
          hit = sprite_collide(&GLOBALS_BANS_DLIST, (x - 28), y, 72, 8,
                               FTP_ENEMY_FIRE);
          if (hit == 0) {
            hit = new sp_mine();
            if (hit != 0) {
              GLOBALS_BANS_DLIST.addtail(hit);
              hit->SPRITE_X = x;
              hit->SPRITE_Y = y;
              cnt++;
              GLOBALS_MINE_COUNT = cnt;
            }
          }
        }
      }
    clrmine:
      GLOBALS_GAME_FLAGS &= ~FFLAG_MINE;
    }
  }
}

void init_enemy() {
  int cnt = GLOBALS_ENEMY_COUNT;
  if (cnt < GLOBALS_ENEMY_MAX) {
    int delay = GLOBALS_ENEMY_DELAY;
    delay--;
    GLOBALS_ENEMY_DELAY = delay;
    if (delay < 0) {
      delay = random(8);
      delay += 8;
      GLOBALS_ENEMY_DELAY = delay;
      sp_man *man =
          (sp_man *)sprite_find_list_types(&GLOBALS_MAN_DLIST, FTP_MAN);
      if (man) {
        int dir = man->SPRITE_DIR;
        int flag = 0;
        int type;
        if (dir == 1) {
        onright:
          if ((flag & 2) != 0)
            return;
          flag |= 2;
          if ((GLOBALS_GAME_FLAGS & FFLAG_ENEMY) != 0)
            goto onleft;
          int *enemy = armytable[GLOBALS_ENEMY_ARMY];
          type = random(sizeof(army0) / 3);
          type = enemy[(type + (GLOBALS_GAME_LEVEL * (sizeof(army0) / 3))) /
                       sizeof(int)];
          (*init_enemy_table[type * 2 + 1])(type);
        } else {
        onleft:
          if ((flag & 1) != 0)
            return;
          flag |= 1;
          int index = GLOBALS_ENEMY_STACKINDEX;
          if (index == 0)
            goto onright;
          type = GLOBALS_ENEMY_STACK[index - 1];
          (*init_enemy_table[type * 2])(type);
        }
      }
    }
  }
}

void item_auto_select() {
  // select best item
  int index = 0;
  int newindex = 0;
  int cnt = 0;
  do {
    int item = GLOBALS_ITEM_INVENTORY[index];
    if (item != 0) {
      item -= FRM_GOLD;
      if (selecttable[item] > cnt) {
        cnt = selecttable[item];
        newindex = index;
      }
    }
    index++;
  } while (index != (sizeof(GLOBALS_ITEM_INVENTORY) / sizeof(int)));
  if (cnt != 0) {
    GLOBALS_ITEM_SELECTED = newindex;
  }
}

void item_select() {
  // move selector left/right
  int controls = GLOBALS_GAME_CONTROLS;
  int flags = GLOBALS_GAME_FLAGS;

  if ((controls & (FKEY_KEYC | FKEY_KEYD)) != 0) {
    // left/right pressed
    int item = GLOBALS_ITEM_SELECTED;
    if ((flags & FFLAG_ISELECT) == 0) {
      flags |= FFLAG_ISELECT;
      if ((controls & FKEY_KEYC) == 0) {
        item++;
        if (item > 7) {
          item = 0;
        }
      } else {
        item--;
        if (item < 0) {
          item = (sizeof(GLOBALS_ITEM_INVENTORY) / sizeof(int)) - 1;
        }
      }
    }
    GLOBALS_ITEM_SELECTED = item;
  } else {
    // left/right not pressed
    flags &= ~FFLAG_ISELECT;
  }
  GLOBALS_GAME_FLAGS = flags;
}

int item_carry(int item) {
  int index = (sizeof(GLOBALS_ITEM_INVENTORY) / sizeof(int)) - 1;
  do {
    if (GLOBALS_ITEM_INVENTORY[index] == item)
      break;
    index--;
  } while (index != -1);
  return (index);
}

void item_collect(Sprite *node) {
  int frame = node->SPRITE_FRAME;
  int index = (sizeof(GLOBALS_ITEM_INVENTORY) / sizeof(int)) - 1;
  int newindex = -1;
  do {
    int carry = GLOBALS_ITEM_INVENTORY[index];
    if (carry == frame)
      break;
    if (carry == 0) {
      // remember any free space
      newindex = index;
    }
    index--;
  } while (index != -1);
  if (index != -1) {
    // carry same allready so update
    newindex = index;
  }
  if (newindex != -1) {
    // fill new space or update carried
    GLOBALS_ITEM_INVENTORY[newindex] = frame;
    GLOBALS_ITEM_USEAGE[newindex] = node->SPRITE_HITPNT;
    killsprite(node);
  }
}

void drop_item(Sprite *node) {
  int x = node->SPRITE_X;
  if ((x >= 0) && (x <= ((TILE_WIDTH * MAP_WIDTH) - 32))) {
    int cnt = GLOBALS_ITEM_DROPCNT;
    int y, type;
    cnt--;
    if (cnt <= 0) {
      // gold bag
      cnt = random(12);
      cnt += 12;
      cnt += GLOBALS_MAN_EXPERIENCE;
      x = -GLOBALS_SCROLL_X;
      y = -GLOBALS_SCROLL_Y;
      x += random(WINDOW_WIDTH - 16);
      type = FRM_GOLD;
    } else {
      type = droptable[random(sizeof(droptable) / sizeof(int))];
      x += 8;
      y = (node->SPRITE_Y + 8);
    }
    GLOBALS_ITEM_DROPCNT = cnt;
    sp_gold_item *item = (sp_gold_item *)create_item_type(type);
    if (item != 0) {
      GLOBALS_ITEM_DLIST.addtail(item);
      item->SPRITE_X = x;
      item->SPRITE_Y = y;
      cnt = item->SPRITE_HITPNT;
      if (item->SPRITE_USER1 != 0) {
        cnt += (GLOBALS_MAN_EXPERIENCE / item->SPRITE_USER1);
      }
      item->SPRITE_HITPNT = cnt;
      if (type == FRM_GOLD) {
        item->mv.MV_YVEL = 0;
      }
    }
  }
}

void blast_area(int x, int y, int w, int h) {
  Sprite *hit;
  for (;;) {
    hit = sprite_collide(&GLOBALS_ENEMY_DLIST, x, y, w, h, -1);
    if (hit == 0)
      break;
    sprite_kill_list_id(&GLOBALS_ENEMY_DLIST, hit->SPRITE_ID);
    play_death_score(hit);
  }
  for (;;) {
    hit = sprite_collide(&GLOBALS_MISSILE_DLIST, x, y, w, h, -1);
    if (hit == 0)
      break;
    killsprite(hit);
    play_death_score(hit);
  }
  for (;;) {
    hit = sprite_collide(&GLOBALS_BANS_DLIST, x, y, w, h, FTP_ENEMY_FIRE);
    if (hit == 0)
      break;
    killsprite(hit);
    play_death_score(hit);
  }
}

// extra game defined drawing routines

void sprite_draw_mind(Sprite *node) {
  int x = node->SPRITE_X;
  int y = node->SPRITE_Y;
  int num = node->SPRITE_USER1 & 0xffff;
  int sx, sy, frame, cnt, fx, fy;

  // draw arms
  int segindex = (GLOBALS_GAME_FRAME_COUNT ^ -1);
  segindex = segindex % (sizeof(at_mind_arm) / sizeof(int));
  int armindex = node->SPRITE_USER3;
  Pixmap *pix = GLOBALS_FRM_16X16;

  sx = (x + 24);
  sy = (y + 8);
  cnt = num;
  while (cnt > 0) {
    frame = at_mind_arm[segindex];
    segindex++;
    if (segindex == sizeof(at_mind_arm) / sizeof(int)) {
      segindex = 0;
    }
    pix->blit(sx, sy, 0, (frame * 16), 16, 16);
    sx += mt_mind_arm[armindex];
    sy += mt_mind_arm[armindex + 1];
    armindex += 2;
    if (armindex == sizeof(mt_mind_arm) / sizeof(int)) {
      armindex = 0;
    }
    cnt--;
  }
  pix->getframe((FRM_TENDS + 1), fx, fy);
  pix->blit(sx, sy, fx, fy, 16, 16);

  sx = (x - 8);
  sy = (y + 8);
  cnt = num;
  while (cnt > 0) {
    frame = at_mind_arm[segindex];
    segindex++;
    if (segindex == sizeof(at_mind_arm) / sizeof(int)) {
      segindex = 0;
    }
    pix->blit(sx, sy, 0, (frame * 16), 16, 16);
    sx -= mt_mind_arm[armindex];
    sy += mt_mind_arm[armindex + 1];
    armindex += 2;
    if (armindex == sizeof(mt_mind_arm) / sizeof(int)) {
      armindex = 0;
    }
    cnt--;
  }
  pix->getframe((FRM_TENDS + 3), fx, fy);
  pix->blit(sx, sy, fx, fy, 16, 16);

  sx = (x + 8);
  sy = (y + 24);
  cnt = num;
  while (cnt > 0) {
    frame = at_mind_arm[segindex];
    segindex++;
    if (segindex == sizeof(at_mind_arm) / sizeof(int)) {
      segindex = 0;
    }
    pix->blit(sx, sy, 0, (frame * 16), 16, 16);
    sx += mt_mind_arm[armindex + 1];
    sy += mt_mind_arm[armindex];
    armindex += 2;
    if (armindex == sizeof(mt_mind_arm) / sizeof(int)) {
      armindex = 0;
    }
    cnt--;
  }
  pix->getframe((FRM_TENDS + 2), fx, fy);
  pix->blit(sx, sy, fx, fy, 16, 16);

  sx = (x + 8);
  sy = (y - 8);
  cnt = num;
  while (cnt > 0) {
    frame = at_mind_arm[segindex];
    segindex++;
    if (segindex == sizeof(at_mind_arm) / sizeof(int)) {
      segindex = 0;
    }
    pix->blit(sx, sy, 0, (frame * 16), 16, 16);
    sx += mt_mind_arm[armindex + 1];
    sy -= mt_mind_arm[armindex];
    armindex += 2;
    if (armindex == sizeof(mt_mind_arm) / sizeof(int)) {
      armindex = 0;
    }
    cnt--;
  }
  pix->getframe((FRM_TENDS), fx, fy);
  pix->blit(sx, sy, fx, fy, 16, 16);

  // draw body
  pix = node->SPRITE_PIX;
  pix->getframe((node->SPRITE_FRAME), fx, fy);
  pix->blit(x, y, fx, fy, node->SPRITE_W, node->SPRITE_H);
}

void sprite_draw_banner(Sprite *node) {
  Pixmap *pix = node->SPRITE_PIX;
  pix->blit(node->SPRITE_X + GLOBALS_SCROLL_X,
            node->SPRITE_Y + GLOBALS_SCROLL_Y, 0, node->SPRITE_FRAME * 16,
            node->SPRITE_W, 16);
  pix->blit(node->SPRITE_X + GLOBALS_SCROLL_X,
            node->SPRITE_Y + GLOBALS_SCROLL_Y + 16, 0, FRM_POLE * 16,
            node->SPRITE_W, 16);
  pix->blit(node->SPRITE_X + GLOBALS_SCROLL_X,
            node->SPRITE_Y + GLOBALS_SCROLL_Y + 32, 0, FRM_POLE * 16,
            node->SPRITE_W, 16);
}

void sprite_draw_enter(Sprite *node) {
  Pixmap *pix = node->SPRITE_PIX;
  char *string = (char *)node->SPRITE_USER1;
  int y = WINDOW_HEIGHT / 2;
  int rows = 3;
  do {
    int cols = 11;
    int x = 72;
    do {
      pix->blit(x, y, 0, (((*string) - 32) * 8), 8, 8);
      x += 16;
      cols--;
      string++;
      if (string == txt_lettab + sizeof(txt_lettab)) {
        string = txt_lettab;
      }
    } while (cols != 0);
    rows--;
    y += 16;
  } while (rows != 0);
}

void sprite_draw_map(Sprite *node) {
  int x = node->SPRITE_X;
  int y = node->SPRITE_Y;
  int x1 = x + (8 * 16);
  int y1 = y + (8 * 16);
  Pixmap *pix = node->SPRITE_PIX;
  unsigned char *map = GLOBALS_GAME_CAMPAINMAP;
  do {
    x = node->SPRITE_X;
    do {
      pix->blit(x, y, 0, (*map) * 8, 8, 8);
      map++;
      x += 8;
    } while (x != x1);
    y += 8;
  } while (y != y1);
}

// init routines

void init_enemy_left(int type) {
  int x, y, w, h;
  sp_enemy *node;

  w = enemy_size_table[type * 2];
  h = enemy_size_table[type * 2 + 1];
  getxy(w, h, -1, x, y);
  if (y >= 0) {
    node = (sp_enemy *)create_enemy_type(type);
    if (node != 0) {
      GLOBALS_ENEMY_DLIST.addtail(node);
      GLOBALS_ENEMY_STACKINDEX--;
      node->SPRITE_X = x;
      node->SPRITE_Y = y;
      node->SPRITE_DIR = 1;
      node->mv.MV_XVEL = node->mv.MV_MAXPX;
      GLOBALS_ENEMY_COUNT += node->SPRITE_USER1 & 0xffff;

      // on crusade ?
      if (GLOBALS_GAME_CAMPAINMAP[GLOBALS_GAME_LOCATION_X +
                                  (GLOBALS_GAME_LOCATION_Y * 16)] ==
          FRM_CRUSADE) {
        node->SPRITE_HITPNT = (node->SPRITE_HITPNT << 1);
      }
    }
  }
}

void init_enemy_right(int type) {
  int x, y, w, h;
  sp_enemy *node;

  w = enemy_size_table[type * 2];
  h = enemy_size_table[type * 2 + 1];
  getxy(w, h, 1, x, y);
  if (y >= 0) {
    node = (sp_enemy *)create_enemy_type(type);
    if (node != 0) {
      GLOBALS_ENEMY_DLIST.addtail(node);
      node->SPRITE_X = x;
      node->SPRITE_Y = y;
      node->SPRITE_DIR = -1;
      node->mv.MV_XVEL = node->mv.MV_MAXNX;
      GLOBALS_ENEMY_COUNT += node->SPRITE_USER1 & 0xffff;

      // on crusade ?
      if (GLOBALS_GAME_CAMPAINMAP[GLOBALS_GAME_LOCATION_X +
                                  (GLOBALS_GAME_LOCATION_Y * 16)] ==
          FRM_CRUSADE) {
        node->SPRITE_HITPNT = (node->SPRITE_HITPNT << 1);
      }
    }
  }
}

void init_balista_left(int type) {
  int x, y, w, h;
  Sprite *node;

  w = enemy_size_table[type * 2];
  h = enemy_size_table[type * 2 + 1];
  getxy(w, h, -1, x, y);
  if ((y >= 0) && (x >= 0) && (x < ((MAP_WIDTH * TILE_WIDTH) - 32))) {
    node = sprite_collide(&GLOBALS_ENEMY_DLIST, x, y, w, h,
                          (FTP_BALISTA | FTP_CANNON | FTP_OIL | FTP_BESERK));
    if (node == 0) {
      node = create_enemy_type(type);
      if (node != 0) {
        GLOBALS_ENEMY_DLIST.addtail(node);
        GLOBALS_ENEMY_STACKINDEX--;
        node->SPRITE_X = x;
        node->SPRITE_Y = y;
        node->SPRITE_DIR = 1;
        GLOBALS_ENEMY_COUNT += node->SPRITE_USER1 & 0xffff;

        // on crusade ?
        if (GLOBALS_GAME_CAMPAINMAP[GLOBALS_GAME_LOCATION_X +
                                    (GLOBALS_GAME_LOCATION_Y * 16)] ==
            FRM_CRUSADE) {
          node->SPRITE_HITPNT = (node->SPRITE_HITPNT << 1);
        }
      }
    }
  }
}

void init_balista_right(int type) {
  int x, y, w, h;
  Sprite *node;

  w = enemy_size_table[type * 2];
  h = enemy_size_table[type * 2 + 1];
  getxy(w, h, 1, x, y);
  if ((y >= 0) && (x >= 0) && (x < ((MAP_WIDTH * TILE_WIDTH) - 32))) {
    node = sprite_collide(&GLOBALS_ENEMY_DLIST, x, y, w, h,
                          (FTP_BALISTA | FTP_CANNON | FTP_OIL | FTP_BESERK));
    if (node == 0) {
      node = create_enemy_type(type);
      if (node != 0) {
        GLOBALS_ENEMY_DLIST.addtail(node);
        node->SPRITE_X = x;
        node->SPRITE_Y = y;
        node->SPRITE_DIR = -1;
        GLOBALS_ENEMY_COUNT += node->SPRITE_USER1 & 0xffff;

        // on crusade ?
        if (GLOBALS_GAME_CAMPAINMAP[GLOBALS_GAME_LOCATION_X +
                                    (GLOBALS_GAME_LOCATION_Y * 16)] ==
            FRM_CRUSADE) {
          node->SPRITE_HITPNT = (node->SPRITE_HITPNT << 1);
        }
      }
    }
  }
}

// component handlers

void cp_man(Sprite *node, CP *cp) {
  sp_man *man = (sp_man *)node;
  int x, y, y1, h, xv, yv, jump, jv, frame, index, item, cnt;
  int controls = GLOBALS_GAME_CONTROLS;
  map *land = GLOBALS_LAND;
  unsigned char flags1, flags2;
  Sprite *wep;

  // get current position
  x = man->SPRITE_X;
  y = man->SPRITE_Y;
  h = man->SPRITE_H;
  xv = man->SPRITE_USER1;
  yv = man->SPRITE_USER2;

  // check for jumping
  jump = man->SPRITE_USER3;
  if (jump != 0) {
  dojump:
    // jumping
    man->SPRITE_FRAME = jump_offsets[jump];
    jv = jump_offsets[jump + 1];
    jump += 2;
    if (jv != 0) {
      x += xv;
      y -= jv;
    }
    if (jump == (sizeof(jump_offsets) / sizeof(int))) {
      jump = 0;
    }
    man->SPRITE_USER3 = jump;
  } else {
    // check for falling
    y1 = (h + yv + y);
    flags1 = land->getmapflags((x + 8), y1);
    flags2 = land->getmapflags((x + 23), y1);
    if (((flags1 & (FMAP_CLIMB | FMAP_STAND)) == 0) &&
        ((flags2 & (FMAP_CLIMB | FMAP_STAND)) == 0)) {
      // falling
      wep = (Sprite *)man->getpred();
      if (wep->getpred() != 0) {
        // test if this is wepon addon, if so kill it
        if (man->SPRITE_ID == wep->SPRITE_ID) {
          killsprite(wep);
        }
      }
      man->SPRITE_FRAME = FRM_FALL;
      man->at.AT_SPEED = 0;
      yv++;
      if (yv > 10) {
        xv = 0;
        yv = 10;
      }
      x += xv;
      y += yv;
    } else {
      // on floor allready ?
      if (yv != 0) {
        if (yv == 10) {
          // duck due to impact
          man->SPRITE_FRAME = FRM_DUCK;
          xv = 0;
        } else {
          man->SPRITE_FRAME = FRM_WALK;
        }
        y = ((y1 & -TILE_HEIGHT) - 32);
        yv = 0;
      }
      if (man->at.AT_SPEED != 0) {
        if ((controls & FKEY_KEYB) == 0) {
          GLOBALS_GAME_FLAGS &= ~FFLAG_USE;
        }
        if (man->at.AT_INDEX == 0) {
          man->at.AT_SPEED = 0;
        }
      } else {
        if ((controls & FKEY_UP) != 0) {
          y1 = (y + h - 1);
          flags1 = land->getmapflags((x + 4), y1);
          flags2 = land->getmapflags((x + 27), y1);
          if (((flags1 & (FMAP_CLIMB)) != 0) &&
              ((flags2 & (FMAP_CLIMB)) != 0)) {
            // climb up
            y -= 4;
            x += 4;
            x &= -TILE_WIDTH;
            man->SPRITE_FRAME = (((y >> 2) & 3) + FRM_CLIMB);
            xv = 0;
          } else {
            // jump
            jump = 0;
            goto dojump;
          }
        } else {
          if ((controls & FKEY_KEYB) != 0) {
            if ((GLOBALS_GAME_FLAGS & FFLAG_USE) == 0) {
              // use selected item
              GLOBALS_GAME_FLAGS |= FFLAG_USE;
              if (GLOBALS_GAME_MENU_MODE == txt_menu_tutor) {
                // select best weapon
                item_auto_select();
              }
              index = GLOBALS_ITEM_SELECTED;
              item = GLOBALS_ITEM_INVENTORY[index];
              if (item != 0) {
              use_item:
                // got item in this slot
                cnt = GLOBALS_ITEM_USEAGE[index];
                cnt--;
                GLOBALS_ITEM_USEAGE[index] = cnt;
                if (cnt <= 0) {
                  // item used up
                  GLOBALS_ITEM_INVENTORY[index] = 0;
                }
                (*itemtable[item - FRM_GOLD])(man);
              } else {
                // shout out
                play_out(man);
                if (GLOBALS_GAME_MENU_MODE == txt_menu_assist) {
                  // select best weapon
                  item_auto_select();
                  index = GLOBALS_ITEM_SELECTED;
                  item = GLOBALS_ITEM_INVENTORY[index];
                  if (item != 0)
                    goto use_item;
                }
              }
            }
          } else {
            GLOBALS_GAME_FLAGS &= ~FFLAG_USE;
            if ((controls & FKEY_DOWN) != 0) {
              y1 = (y + h);
              flags1 = land->getmapflags((x + 4), y1);
              flags2 = land->getmapflags((x + 27), y1);
              if (((flags1 & (FMAP_CLIMB)) != 0) &&
                  ((flags2 & (FMAP_CLIMB)) != 0)) {
                // climb down
                y += 4;
                x += 4;
                x &= -TILE_WIDTH;
                man->SPRITE_FRAME = (((y >> 2) & 3) + FRM_CLIMB);
              } else {
                // duck
                man->SPRITE_FRAME = FRM_DUCK;
              }
              xv = 0;
            } else {
              if ((controls & FKEY_LEFT) != 0) {
                x -= 4;
                y &= -TILE_HEIGHT;
                xv = -4;
                man->SPRITE_DIR = -1;
                frame = man->SPRITE_FRAME;
                frame++;
                if (frame >= (FRM_WALK + 8)) {
                  frame = FRM_WALK;
                }
                man->SPRITE_FRAME = frame;
              } else if ((controls & FKEY_RIGHT) != 0) {
                x += 4;
                y &= -TILE_HEIGHT;
                xv = 4;
                man->SPRITE_DIR = 1;
                frame = man->SPRITE_FRAME;
                frame++;
                if (frame >= (FRM_WALK + 8)) {
                  frame = FRM_WALK;
                }
                man->SPRITE_FRAME = frame;
              } else {
                // no movement
                xv = 0;
              }
            }
          }
        }
      }
    }
  }

  // save new position
  if (x < 0) {
    x = 0;
  } else if (x > ((MAP_WIDTH * TILE_WIDTH) - 32)) {
    x = ((MAP_WIDTH * TILE_WIDTH) - 32);
  }
  man->SPRITE_X = x;
  man->SPRITE_Y = y;
  man->SPRITE_USER1 = xv;
  man->SPRITE_USER2 = yv;

  // check if dead
  if ((GLOBALS_MAN_POWER == 0) || (GLOBALS_MAN_STRENGTH == 0)) {
    sprite_kill_list_id(&GLOBALS_MAN_DLIST, man->SPRITE_ID);
  }
}

void cp_manduck(Sprite *node, CP *cp) {
  int h;

  h = node->SPRITE_H;
  if (node->SPRITE_FRAME < FRM_DUCK) {
    // not on duck
    if (h != 32) {
      node->SPRITE_Y -= 16;
      node->SPRITE_H = 32;
    }
  } else {
    // on duck
    if (h == 32) {
      node->SPRITE_Y += 16;
      node->SPRITE_H = 16;
    }
  }
}

void cp_manitem(Sprite *node, CP *cp) {
  // move selector left/right
  item_select();

  // pickup items
  if ((GLOBALS_GAME_MENU_MODE == txt_menu_tutor) ||
      ((GLOBALS_GAME_CONTROLS & FKEY_KEYA) != 0)) {
    // try pickup
    Sprite *hit =
        sprite_collide(&GLOBALS_ITEM_DLIST, node->SPRITE_X, node->SPRITE_Y,
                       node->SPRITE_W, node->SPRITE_H, FTP_ITEM);
    if (hit != 0) {
      item_collect(hit);
    }
  }

  // if empty then give a few mase swings
  if ((GLOBALS_GAME_FRAME_COUNT & 127) == 0) {
    int index = 0;
    int item;
    do {
      item = GLOBALS_ITEM_INVENTORY[index];
      if (item != 0) {
        if (item < (FRM_SPELLS + 3))
          return;
      }
      index++;
    } while (index != (sizeof(GLOBALS_ITEM_INVENTORY) / sizeof(int)));
    if (index == (sizeof(GLOBALS_ITEM_INVENTORY) / sizeof(int))) {
      // try to give mase in empty slot
      index = 0;
      do {
        if (GLOBALS_ITEM_INVENTORY[index] == 0) {
          // 6 swings of a mase
          GLOBALS_ITEM_INVENTORY[index] = FRM_SHIELD;
          GLOBALS_ITEM_USEAGE[index] = 6;
          break;
        }
        index++;
      } while (index != (sizeof(GLOBALS_ITEM_INVENTORY) / sizeof(int)));
    }
  }
}

void cp_manstance(Sprite *node, CP *cp) {
  sp_man *man = (sp_man *)node;

  // get current position
  int x = man->SPRITE_X;
  int y = man->SPRITE_Y;
  int xv = man->SPRITE_USER1;
  int yv = man->SPRITE_USER2;

  // check for falling
  map *land = GLOBALS_LAND;
  int y1 = (y + man->SPRITE_H + yv);
  int flags1 = land->getmapflags(x + 8, y1);
  int flags2 = land->getmapflags(x + 23, y1);
  if (((flags1 & (FMAP_CLIMB | FMAP_STAND)) == 0) &&
      ((flags2 & (FMAP_CLIMB | FMAP_STAND)) == 0)) {
    // falling
    man->at.AT_SPEED = 0;
    man->SPRITE_FRAME = FRM_FALL;
    yv++;
    if (yv > 10) {
      xv = 0;
      yv = 10;
    }
    x += xv;
    y += yv;
  } else {
    // on floor allready ?
    man->at.AT_SPEED = 4;
    if (yv != 0) {
      if (yv == 10) {
        // duck due to impact
        man->SPRITE_FRAME = FRM_DUCK;
      } else {
        man->SPRITE_FRAME = FRM_STANCE;
      }
      y = ((y1 & -TILE_HEIGHT) - 32);
      yv = 0;
    }
    xv = 0;
  }

  // save new position
  if (x < 0) {
    x = 0;
  } else if (x > ((MAP_WIDTH * TILE_WIDTH) - 32)) {
    x = ((MAP_WIDTH * TILE_WIDTH) - 32);
  }
  man->SPRITE_X = x;
  man->SPRITE_Y = y;
  man->SPRITE_USER1 = xv;
  man->SPRITE_USER2 = yv;
}

void cp_mantrack(Sprite *node, CP *cp) {
  int x, y;

  x = (node->SPRITE_X - ((WINDOW_WIDTH / 2) - (node->SPRITE_W / 2)));
  if (x < 0) {
    x = 0;
  } else if (x > ((MAP_WIDTH * TILE_WIDTH) - WINDOW_WIDTH)) {
    x = ((MAP_WIDTH * TILE_WIDTH) - WINDOW_WIDTH);
  }
  y = ((node->SPRITE_Y + node->SPRITE_H) - 16) - (WINDOW_HEIGHT / 2);
  if (y < 0) {
    y = 0;
  } else if (y > ((MAP_HEIGHT * TILE_HEIGHT) - WINDOW_HEIGHT)) {
    y = ((MAP_HEIGHT * TILE_HEIGHT) - WINDOW_HEIGHT);
  }
  GLOBALS_SCROLL_X = -x;
  GLOBALS_SCROLL_Y = -y;
}

void cp_mind(Sprite *node, CP *cp) {
  sp_mind *enemy = (sp_mind *)node;

  int x, y;
  int cnt = enemy->SPRITE_USER1 >> 16;
  cnt--;
  if (cnt <= 0) {
    // set new delay
    random(16);
    cnt += 32;

    // set new homeing cords
    int x = random(32);
    x += 128;
    y = random(16);
    y += ((WINDOW_HEIGHT / 2) - 64) + 48;
    enemy->SPRITE_USER2 = x + (y << 16);
  }
  enemy->SPRITE_USER1 = (enemy->SPRITE_USER1 & 0xffff) + (cnt << 16);

  // home in
  int xacc;
  x = enemy->SPRITE_USER2 & 0xffff;
  y = enemy->SPRITE_USER2 >> 16;
  if (x >= enemy->SPRITE_X) {
    xacc = 1;
  } else {
    xacc = -1;
  }
  enemy->mv.MV_XACC = xacc;
  int yacc;
  if (y >= enemy->SPRITE_Y) {
    yacc = 1;
  } else {
    yacc = -1;
  }
  enemy->mv.MV_YACC = yacc;

  // close mouth ?
  cnt = GLOBALS_GAME_FRAME_COUNT;
  if ((cnt & 0xf) == 0) {
    enemy->SPRITE_FRAME =
        (((enemy->SPRITE_FRAME - FRM_FACES) & ~1) + FRM_FACES);
  }

  // waggle arms
  int index = enemy->SPRITE_USER3;
  index += 2;
  if (index == (sizeof(mt_mind_arm) / sizeof(int))) {
    index = 0;
  }
  enemy->SPRITE_USER3 = index;

  // fire ?
  if ((cnt % 10) == 0) {
    sp_mind_fire *sfx = new sp_mind_fire();
    if (sfx != 0) {
      GLOBALS_MISSILE_DLIST.addtail(sfx);
      x = (enemy->SPRITE_X + 8);
      y = (enemy->SPRITE_Y + 8);
      int x1, y1;

      // fire to edge or hand
      sp_hand *hand = (sp_hand *)GLOBALS_MAN_DLIST.gethead();
      if (hand->getsucc() == 0 || (random(4) > 0)) {
        getxy_outer(sfx->SPRITE_W, sfx->SPRITE_H, x1, y1);
      } else {
        x1 = hand->SPRITE_X;
        y1 = hand->SPRITE_Y;
      }
      sfx->ml.sprite_ml_init(sfx, x, y, x1, y1);
    }
  }
}

void cp_hand_fire(Sprite *node, CP *cp) {
  sp_mind_fire *enemy = (sp_mind_fire *)node;
  if (enemy->ml.ML_COUNT == -1) {
    killsprite(enemy);
  }
}

void cp_hand(Sprite *node, CP *cp) {
  // get current position
  int x = node->SPRITE_X;
  int y = node->SPRITE_Y;

  // move hand
  int controls = GLOBALS_GAME_CONTROLS;
  if ((controls & (FKEY_LEFT | FKEY_UP)) != 0) {
    if (x == 0) {
      y += 8;
      if (y >= (WINDOW_HEIGHT - 16)) {
        y = (WINDOW_HEIGHT - 16);
        x += 8;
      }
    } else if (x == (WINDOW_WIDTH - 16)) {
      y -= 8;
      if (y <= 0) {
        y = 0;
        x -= 8;
      }
    } else if (y == 0) {
      x -= 8;
      if (x <= 0) {
        x = 0;
        y += 8;
      }
    } else {
      x += 8;
      if (x >= (WINDOW_WIDTH - 16)) {
        x = (WINDOW_WIDTH - 16);
        y += 8;
      }
    }
  } else if ((controls & (FKEY_RIGHT | FKEY_DOWN)) != 0) {
    if (x == 0) {
      y -= 8;
      if (y <= 0) {
        y = 0;
        x += 8;
      }
    } else if (x == (WINDOW_WIDTH - 16)) {
      y += 8;
      if (y >= (WINDOW_HEIGHT - 16)) {
        y = (WINDOW_HEIGHT - 16);
        x -= 8;
      }
    } else if (y == 0) {
      x += 8;
      if (x >= (WINDOW_WIDTH - 16)) {
        x = (WINDOW_WIDTH - 16);
        y += 8;
      }
    } else {
      x -= 8;
      if (x <= 0) {
        x = 0;
        y -= 8;
      }
    }
  }

  // save new position
  node->SPRITE_X = x;
  node->SPRITE_Y = y;

  // fire ?
  if ((controls & FKEY_KEYB) == 0) {
    GLOBALS_GAME_FLAGS &= ~FFLAG_SELECT;
  } else {
    if ((GLOBALS_GAME_FLAGS & FFLAG_SELECT) == 0) {
      GLOBALS_GAME_FLAGS |= FFLAG_SELECT;
      int cnt = GLOBALS_MIND_FIRECNT;
      if (cnt < 3) {
        sp_hand_fire *sfx = new sp_hand_fire();
        if (sfx != 0) {
          GLOBALS_MAN_DLIST.addtail(sfx);
          cnt++;
          GLOBALS_MIND_FIRECNT = cnt;
          sp_mind *mind = (sp_mind *)GLOBALS_ENEMY_DLIST.gethead();
          int x1, y1;
          if (mind->getsucc() != 0) {
            x1 = mind->SPRITE_X + 8;
            y1 = mind->SPRITE_Y + 8;
          } else {
            x1 = WINDOW_WIDTH / 2 - 8;
            y1 = WINDOW_HEIGHT / 2 - 8;
          }
          sfx->ml.sprite_ml_init(sfx, x, y, x1, y1);
          play_spell(sfx);
        }
      }
    }
  }

  // set frame
  int frame;
  if (x == 0) {
    frame = (FRM_MANMIND + 3);
  } else if (x == (WINDOW_WIDTH - 16)) {
    frame = (FRM_MANMIND + 1);
  } else if (y == 0) {
    frame = (FRM_MANMIND);
  } else {
    frame = (FRM_MANMIND + 2);
  }
  node->SPRITE_FRAME = frame;

  // scroll map
  int xs;
  if (x < ((WINDOW_WIDTH / 3) - 8)) {
    xs = -1;
  } else if (x < (WINDOW_WIDTH - ((WINDOW_WIDTH / 3) - 8))) {
    xs = 0;
  } else {
    xs = 1;
  }
  int ys;
  if (y < ((WINDOW_HEIGHT / 3) - 8)) {
    ys = -1;
  } else if (y < (WINDOW_HEIGHT - ((WINDOW_HEIGHT / 3) - 8))) {
    ys = 0;
  } else {
    ys = 1;
  }
  mindmap *map = GLOBALS_MIND;
  int x1, y1, x2, y2;
  map->getoffsets(x1, y1, x2, y2);
  x1 = ((x1 + xs) & 0xf);
  y1 = ((y1 + ys) & 0xf);
  xs *= 2;
  ys *= 2;
  x2 = ((x2 + xs) & 0x1f);
  y2 = ((y2 + ys) & 0x1f);
  map->setoffsets(x1, y1, x2, y2);
}

void cp_bounce(Sprite *node, CP *cp) {
  sp_gold_item *item = (sp_gold_item *)node;
  int yv = item->mv.MV_YVEL;
  int y = item->SPRITE_Y;
  int h = item->SPRITE_H;
  y = ((y + yv + h) & -TILE_HEIGHT);
  int oy = item->SPRITE_USER3;
  item->SPRITE_USER3 = y;
  if ((y != oy) && (yv > 0)) {
    int x = item->SPRITE_X + (item->SPRITE_W >> 1);
    map *land = GLOBALS_LAND;
    int flags = land->getmapflags(x, y);
    if ((flags & FMAP_STAND) != 0) {
      y -= h;
      item->SPRITE_Y = y;
      item->mv.MV_YVEL = -yv;
      dt_fizz_fx(item);
      if (item->SPRITE_FRAME == (FRM_MANBITS + 1)) {
        play_clash1(item);
      }
    }
  }
}

void cp_offscreen_no_death(Sprite *node, CP *cp) {
  int sx = -GLOBALS_SCROLL_X;
  int sy = -GLOBALS_SCROLL_Y;
  int x, y;

  x = node->SPRITE_X;
  if (x < (sx + WINDOW_WIDTH)) {
    y = node->SPRITE_Y;
    if (y < (sy + WINDOW_HEIGHT)) {
      x += node->SPRITE_W;
      if (x > sx) {
        y += node->SPRITE_H;
        if (y > sy) {
          // on screen
          return;
        }
      }
    }
  }
  // offscreen, no death
  node->SPRITE_DEATH = 0;
  killsprite(node);
}

void cp_offscreen(Sprite *node, CP *cp) {
  int sx = -GLOBALS_SCROLL_X;
  int sy = -GLOBALS_SCROLL_Y;
  int x, y;

  x = node->SPRITE_X;
  if (x < (sx + WINDOW_WIDTH)) {
    y = node->SPRITE_Y;
    if (y < (sy + WINDOW_HEIGHT)) {
      x += node->SPRITE_W;
      if (x > sx) {
        y += node->SPRITE_H;
        if (y > sy) {
          // on screen
          return;
        }
      }
    }
  }
  // offscreen
  killsprite(node);
}

void cp_banner(Sprite *node, CP *cp) {
  sp_fizz *sfx = new sp_fizz();
  if (sfx != 0) {
    GLOBALS_FX_DLIST.addtail(sfx);
    int rand = random(32);
    int x = node->SPRITE_X + (rand - 16);
    rand = random(32);
    int y = node->SPRITE_Y + (rand - 16);
    sfx->SPRITE_X = x;
    sfx->SPRITE_Y = y;
  }
}

void cp_big_crossbow(Sprite *node, CP *cp) {
  sp_big_crossbow *addon = (sp_big_crossbow *)node;
  if (addon->mt.MT_INDEX == 14) {
    sp_big_arrow *wep = new sp_big_arrow();
    if (wep != 0) {
      GLOBALS_MAN_DLIST.addtail(wep);
      int dir = addon->SPRITE_DIR;
      int x = addon->SPRITE_X;
      int maxx = wep->mv.MV_MAXPX;
      maxx *= dir;
      if (dir == 1) {
        x += 8;
      } else {
        x -= 24;
      }
      wep->SPRITE_X = x;
      wep->SPRITE_Y = addon->SPRITE_Y;
      wep->SPRITE_DIR = dir;
      wep->mv.MV_XVEL = maxx;
      play_twang1(wep);
    }
  }
}

void cp_small_crossbow(Sprite *node, CP *cp) {
  sp_small_crossbow *addon = (sp_small_crossbow *)node;
  if (addon->mt.MT_INDEX == 0) {
    sp_small_arrow *wep = new sp_small_arrow();
    if (wep != 0) {
      GLOBALS_MAN_DLIST.addtail(wep);
      int dir = addon->SPRITE_DIR;
      int x = addon->SPRITE_X;
      int maxx = wep->mv.MV_MAXPX;
      maxx *= dir;
      if (dir == 1) {
        x += 16;
      } else {
        x -= 16;
      }
      wep->SPRITE_X = x;
      wep->SPRITE_Y = addon->SPRITE_Y;
      wep->SPRITE_DIR = dir;
      wep->mv.MV_XVEL = maxx;
      play_twang2(wep);
    }
  }
}

void cp_naptha(Sprite *node, CP *cp) {
  sp_naptha *addon = (sp_naptha *)node;
  if (addon->mt.MT_INDEX == 14) {
    sp_nbomb *wep = new sp_nbomb();
    if (wep != 0) {
      GLOBALS_MAN_DLIST.addtail(wep);
      int dir = addon->SPRITE_DIR;
      int x = addon->SPRITE_X;
      int maxx = wep->mv.MV_MAXPX;
      maxx *= dir;
      if (dir == 1) {
        x += 4;
      } else {
        x -= 4;
      }
      wep->SPRITE_X = x;
      wep->SPRITE_Y = addon->SPRITE_Y;
      wep->mv.MV_XVEL = maxx;
    }
  }
}

void cp_nbomb(Sprite *node, CP *cp) {
  sp_nbomb *wep = (sp_nbomb *)node;
  if (wep->mv.MV_YVEL == 5) {
    killsprite(wep);
    blast_area(wep->SPRITE_X - 24, wep->SPRITE_Y - 24, 64, 64);
  }
}

void cp_helper(Sprite *node, CP *cp) {
  sp_helper *helper = (sp_helper *)node;
  int life = helper->SPRITE_USER1;
  life--;
  if (life != 0) {
    helper->SPRITE_USER1 = life;
    Sprite *enemy = (Sprite *)GLOBALS_ENEMY_DLIST.gethead();
    if (enemy->getsucc() != 0) {
      // home in
      int x = (((enemy->SPRITE_W >> 1) + enemy->SPRITE_X) - 8);
      int acc = -1;
      if (x >= helper->SPRITE_X) {
        acc = 1;
      }
      helper->mv.MV_XACC = acc;

      int y = (((enemy->SPRITE_H >> 1) + enemy->SPRITE_Y) - 8);
      acc = -1;
      if (y >= helper->SPRITE_Y) {
        acc = 1;
      }
      helper->mv.MV_YACC = acc;
      dt_fizz_fx(helper);
    } else {
      // stop homeing
      helper->mv.MV_XVEL = 0;
      helper->mv.MV_YVEL = 0;
      helper->mv.MV_XACC = 0;
      helper->mv.MV_YACC = 0;
    }
  } else {
    killsprite(helper);
  }
}

void cp_mine(Sprite *node, CP *cp) {
  int sx = -GLOBALS_SCROLL_X;
  int sy = -GLOBALS_SCROLL_Y;
  int x, y;

  x = node->SPRITE_X;
  if (x < (sx + WINDOW_WIDTH)) {
    y = node->SPRITE_Y;
    if (y < (sy + WINDOW_HEIGHT)) {
      x += node->SPRITE_W;
      if (x > sx) {
        y += node->SPRITE_H;
        if (y > sy) {
          // on screen
          return;
        }
      }
    }
  }
  // offscreen, no death
  GLOBALS_MINE_COUNT--;
  node->SPRITE_DEATH = 0;
  killsprite(node);
}

void cp_enemy_fall(Sprite *node, CP *cp) {
  sp_enemy *enemy = (sp_enemy *)node;
  int x = enemy->SPRITE_X;
  int y = (enemy->SPRITE_Y + enemy->SPRITE_H);
  int dir = enemy->SPRITE_DIR;
  if (dir == 1) {
    x += (enemy->SPRITE_W - 1);
  }
  map *land = GLOBALS_LAND;
  int flags = land->getmapflags(x, y);
  if ((flags & FMAP_STAND) == 0) {
    // not on floor
    enemy->mv.MV_YACC = 1;
    if (enemy->mv.MV_MAXPX <= enemy->mv.MV_YVEL) {
      // max fall speed
      if (enemy->mv.MV_XVEL != 0) {
        // slow down
        enemy->mv.MV_XVEL -= dir;
      }
    }
    enemy->SPRITE_FRAME = enemy->SPRITE_USER2;
    enemy->at.AT_SPEED = 0;
  } else {
    if (enemy->mv.MV_YACC != 0) {
      // not on floor allready
      enemy->SPRITE_Y = (enemy->SPRITE_Y & -TILE_HEIGHT);
      enemy->mv.MV_YVEL = 0;
      enemy->mv.MV_YACC = 0;
      enemy->mv.MV_XVEL = (enemy->mv.MV_MAXPX * dir);
      enemy->at.AT_SPEED = 1;
      if ((enemy->SPRITE_TYPE &
           (FTP_HORSE | FTP_BOARRIDER | FTP_SKELETON_HORSE)) != 0) {
        // hooves noise
        play_hooves(enemy);
      }
    }
  }
}

void cp_enemy_offscreen(Sprite *node, CP *cp) {
  int sx = -GLOBALS_SCROLL_X;
  int sy = -GLOBALS_SCROLL_Y;
  int x, y;

  x = node->SPRITE_X;
  if (x < (sx + WINDOW_WIDTH)) {
    y = node->SPRITE_Y;
    if (y < (sy + WINDOW_HEIGHT)) {
      x += node->SPRITE_W;
      if (x > sx) {
        y += node->SPRITE_H;
        if (y > sy) {
          // on screen
          return;
        }
      }
    }
  }
  // offscreen, no death
  if (node->SPRITE_DIR < 0) {
    int index = GLOBALS_ENEMY_STACKINDEX;
    if (index != (sizeof(GLOBALS_ENEMY_STACK) / sizeof(int))) {
      // stack enemy
      unsigned int t = node->SPRITE_TYPE;
      int type = BFTP_HORSE;
      while ((t & 1) == 0) {
        t = t >> 1;
        type++;
      }
      GLOBALS_ENEMY_STACK[index] = type;
      index++;
      GLOBALS_ENEMY_STACKINDEX = index;
    }
  }
  GLOBALS_ENEMY_COUNT -= node->SPRITE_USER1 & 0xffff;
  node->SPRITE_DEATH = 0;
  sprite_kill_list_id(&GLOBALS_ENEMY_DLIST, node->SPRITE_ID);
}

void cp_enemy_fall_duck(Sprite *node, CP *cp) {
  sp_enemy *enemy = (sp_enemy *)node;
  int x = enemy->SPRITE_X;
  int y = enemy->SPRITE_Y + enemy->SPRITE_H;
  int dir = enemy->SPRITE_DIR;
  if (dir == 1) {
    x += (enemy->SPRITE_W - 1);
  }
  map *land = GLOBALS_LAND;
  int flags = land->getmapflags(x, y);
  if ((flags & FMAP_STAND) == 0) {
    // not on floor
    enemy->mv.MV_YACC = 1;
    if (enemy->mv.MV_MAXPX <= enemy->mv.MV_YVEL) {
      // max fall speed
      if (enemy->mv.MV_XVEL != 0) {
        // slow down
        enemy->mv.MV_XVEL -= dir;
      }
    }
    enemy->SPRITE_FRAME = enemy->SPRITE_USER2;
    enemy->at.AT_SPEED = 0;
  } else {
    // on floor
    enemy->SPRITE_Y = (enemy->SPRITE_Y & -TILE_HEIGHT);
    if (enemy->mv.MV_YACC != 0) {
      // not on floor allready
      enemy->mv.MV_YVEL = 0;
      enemy->mv.MV_YACC = 0;
      enemy->mv.MV_XVEL = (enemy->mv.MV_MAXPX * dir);
      enemy->at.AT_SPEED = 1;
      enemy->SPRITE_FRAME = enemy->SPRITE_USER2 + 1;
    }
  }
}

void cp_enemy_duck(Sprite *node, CP *cp) {
  int h;

  h = node->SPRITE_H;
  if (node->SPRITE_FRAME < (node->SPRITE_USER2 + 1)) {
    // not on duck
    if (h != 32) {
      node->SPRITE_Y -= 16;
      node->SPRITE_H = 32;
    }
  } else {
    // on duck
    if (h == 32) {
      node->SPRITE_Y += 16;
      node->SPRITE_H = 16;
    }
  }
}

void cp_wizard(Sprite *node, CP *cp) {
  sp_wizard *enemy = (sp_wizard *)node;
  if (enemy->mv.MV_YACC == 0) {
    int cnt = (enemy->SPRITE_USER1 >> 16);
    if (cnt != 0) {
      cnt--;
      if (cnt == 0) {
        enemy->at.AT_TABLE = at_wizard_throw;
        enemy->at.AT_LENGTH = (sizeof(at_wizard_throw) / sizeof(int));
        enemy->at.AT_COUNT = 2;
        enemy->at.AT_SPEED = 2;
        enemy->at.AT_INDEX = 0;
        enemy->mv.MV_XVEL = 0;
      }
      enemy->SPRITE_USER1 = (enemy->SPRITE_USER1 & 0xffff) + (cnt << 16);
    } else {
      int index = enemy->at.AT_INDEX;
      if (index != 0) {
        if ((index == 2) && (enemy->at.AT_COUNT == 0)) {
          sp_wizard_shot *wep = new sp_wizard_shot();
          if (wep != 0) {
            GLOBALS_MISSILE_DLIST.addtail(wep);
            int dir = enemy->SPRITE_DIR;
            int x = enemy->SPRITE_X;
            if (dir == 1) {
              x += 16;
            }
            wep->SPRITE_X = x;
            wep->SPRITE_Y = enemy->SPRITE_Y;
            wep->SPRITE_DIR = dir;
            wep->mv.MV_XVEL = (wep->mv.MV_MAXPX * dir);
            int *at;
            if (GLOBALS_ENEMY_WIZSHOT == 0) {
              at = at_wizard_shot_power;
            } else {
              at = at_wizard_shot_strength;
            }
            wep->at.AT_TABLE = at;
          }
        }
      } else {
        enemy->at.AT_TABLE = at_wizard;
        enemy->at.AT_LENGTH = (sizeof(at_wizard) / sizeof(int));
        enemy->at.AT_COUNT = 1;
        enemy->at.AT_SPEED = 1;
        enemy->mv.MV_XVEL = (enemy->mv.MV_MAXPX * enemy->SPRITE_DIR);
        cnt = random(16);
        cnt += 16;
        enemy->SPRITE_USER1 = (enemy->SPRITE_USER1 & 0xffff) + (cnt << 16);
      }
    }
  }
}

void cp_spearman(Sprite *node, CP *cp) {
  sp_spearman *enemy = (sp_spearman *)node;
  if (enemy->mv.MV_YACC == 0) {
    int cnt = (enemy->SPRITE_USER1 >> 16);
    if (cnt != 0) {
      cnt--;
      if (cnt == 0) {
        enemy->at.AT_TABLE = at_spearman_throw;
        enemy->at.AT_LENGTH = (sizeof(at_spearman_throw) / sizeof(int));
        enemy->at.AT_COUNT = 2;
        enemy->at.AT_SPEED = 2;
        enemy->at.AT_INDEX = 0;
        enemy->mv.MV_XVEL = 0;
      }
      enemy->SPRITE_USER1 = (enemy->SPRITE_USER1 & 0xffff) + (cnt << 16);
    } else {
      int index = enemy->at.AT_INDEX;
      if (index != 0) {
        if ((index == 2) && (enemy->at.AT_COUNT == 0)) {
          sp_spear *wep = new sp_spear();
          if (wep != 0) {
            GLOBALS_MISSILE_DLIST.addtail(wep);
            int dir = enemy->SPRITE_DIR;
            int x = enemy->SPRITE_X;
            x += (dir * 16);
            wep->SPRITE_X = x;
            wep->SPRITE_Y = enemy->SPRITE_Y;
            wep->SPRITE_DIR = dir;
            wep->mv.MV_XVEL = (wep->mv.MV_MAXPX * dir);
          }
        }
      } else {
        enemy->at.AT_TABLE = at_spearman;
        enemy->at.AT_LENGTH = (sizeof(at_spearman) / sizeof(int));
        enemy->at.AT_COUNT = 1;
        enemy->at.AT_SPEED = 1;
        enemy->mv.MV_XVEL = (enemy->mv.MV_MAXPX * enemy->SPRITE_DIR);
        cnt = random(8);
        cnt += 16;
        enemy->SPRITE_USER1 = (enemy->SPRITE_USER1 & 0xffff) + (cnt << 16);
      }
    }
  }
}

void cp_footman(Sprite *node, CP *cp) {
  sp_footman *enemy = (sp_footman *)node;
  int flags = enemy->SPRITE_FLAGS;
  if ((flags & FSP_ACTION) == 0) {
    if ((flags & FSP_COLLIDE) != 0) {
      sp_footman_mase *wep = new sp_footman_mase();
      if (wep != 0) {
        enemy->addnodeb(wep);
        flags |= FSP_ACTION;
        int dir = enemy->SPRITE_DIR;
        int rand = random(2);
        int *at, *mt, *atm;
        if (rand == 0) {
          // upper swipe
          atm = at_footman_upper;
          at = at_footman_upper_mase;
          if (dir == 1) {
            mt = mt_footman_upper_mase_r;
          } else {
            mt = mt_footman_upper_mase_l;
          }
        } else {
          // lower swipe
          atm = at_footman_lower;
          at = at_footman_lower_mase;
          if (dir == 1) {
            mt = mt_footman_lower_mase_r;
          } else {
            mt = mt_footman_lower_mase_l;
          }
        }
        enemy->at.AT_TABLE = atm;
        enemy->at.AT_LENGTH = (sizeof(at_footman_lower) / sizeof(int));
        enemy->at.AT_COUNT = 2;
        enemy->at.AT_SPEED = 2;
        enemy->at.AT_INDEX = 0;
        wep->at.AT_TABLE = at;
        wep->mt.MT_TABLE = mt;
        wep->mt.MT_TRACK = enemy;
        wep->SPRITE_DIR = dir;
        wep->SPRITE_ID = enemy->SPRITE_ID;
      }
    }
  } else {
    if (enemy->at.AT_INDEX == 0) {
      flags &= ~(FSP_ACTION | FSP_COLLIDE);
      enemy->at.AT_TABLE = at_footman;
      enemy->at.AT_LENGTH = (sizeof(at_footman) / sizeof(int));
      enemy->at.AT_COUNT = 1;
      enemy->at.AT_SPEED = 1;
      enemy->mv.MV_XVEL = (enemy->mv.MV_MAXPX * enemy->SPRITE_DIR);
    }
  }
  enemy->SPRITE_FLAGS = flags;
}

void cp_knight(Sprite *node, CP *cp) {
  sp_knight *enemy = (sp_knight *)node;
  int flags = enemy->SPRITE_FLAGS;
  if ((flags & FSP_ACTION) == 0) {
    if ((flags & FSP_COLLIDE) != 0) {
      sp_knight_mase *wep = new sp_knight_mase();
      if (wep != 0) {
        enemy->addnodeb(wep);
        flags |= FSP_ACTION;
        sp_man *man =
            (sp_man *)sprite_find_list_types(&GLOBALS_MAN_DLIST, FTP_MAN);
        int *at, *mt, *atm;
        int dir = enemy->SPRITE_DIR;
        if (man && man->SPRITE_FRAME < FRM_DUCK) {
          // upper swipe
          atm = at_knight_upper;
          at = at_knight_upper_mase;
          if (dir == 1) {
            mt = mt_footman_upper_mase_r;
          } else {
            mt = mt_footman_upper_mase_l;
          }
        } else {
          // lower swipe
          atm = at_knight_lower;
          at = at_knight_lower_mase;
          if (dir == 1) {
            mt = mt_knight_mase_r;
          } else {
            mt = mt_knight_mase_l;
          }
        }
        enemy->at.AT_TABLE = atm;
        enemy->at.AT_LENGTH = (sizeof(at_knight_lower) / sizeof(int));
        enemy->at.AT_COUNT = 2;
        enemy->at.AT_SPEED = 2;
        enemy->at.AT_INDEX = 0;
        wep->at.AT_TABLE = at;
        wep->mt.MT_TABLE = mt;
        wep->mt.MT_TRACK = enemy;
        wep->SPRITE_DIR = dir;
        wep->SPRITE_ID = enemy->SPRITE_ID;
      }
    }
  } else {
    if (enemy->at.AT_INDEX == 0) {
      flags &= ~(FSP_ACTION | FSP_COLLIDE);
      enemy->at.AT_TABLE = at_knight;
      enemy->at.AT_LENGTH = (sizeof(at_knight) / sizeof(int));
      enemy->at.AT_COUNT = 1;
      enemy->at.AT_SPEED = 1;
      enemy->mv.MV_XVEL = (enemy->mv.MV_MAXPX * enemy->SPRITE_DIR);
    }
  }
  enemy->SPRITE_FLAGS = flags;
}

void cp_skeleton_monk(Sprite *node, CP *cp) {
  int cnt = node->SPRITE_USER3;
  cnt--;
  if (cnt <= 0) {
    cnt = random(20);
    cnt += 20;
    sp_skeleton_item *item = new sp_skeleton_item();
    if (item != 0) {
      GLOBALS_ITEM_DLIST.addtail(item);
      item->SPRITE_X = (node->SPRITE_X + 8);
      item->SPRITE_Y = (node->SPRITE_Y - 8);
      item->SPRITE_DIR = node->SPRITE_DIR;
      dt_fizz_fx(item);
    }
  }
  node->SPRITE_USER3 = cnt;
}

void cp_skeleton(Sprite *node, CP *cp) {
  sp_skeleton *enemy = (sp_skeleton *)node;
  int flags = enemy->SPRITE_FLAGS;
  if ((flags & FSP_ACTION) == 0) {
    if ((flags & FSP_COLLIDE) != 0) {
      sp_skeleton_mase *wep = new sp_skeleton_mase();
      if (wep != 0) {
        enemy->addnodeb(wep);
        flags |= FSP_ACTION;
        int rand = random(2);
        int *at, *mt, *atm;
        if (rand == 0) {
          // upper swipe
          atm = at_skeleton_upper;
          at = at_skeleton_upper_mase;
        } else {
          // lower swipe
          atm = at_skeleton_lower;
          at = at_skeleton_lower_mase;
        }
        int dir = enemy->SPRITE_DIR;
        if (dir == 1) {
          mt = mt_skeleton_mase_r;
        } else {
          mt = mt_skeleton_mase_l;
        }
        enemy->at.AT_TABLE = atm;
        enemy->at.AT_LENGTH = (sizeof(at_skeleton_lower) / sizeof(int));
        enemy->at.AT_COUNT = 2;
        enemy->at.AT_SPEED = 2;
        enemy->at.AT_INDEX = 0;
        wep->at.AT_TABLE = at;
        wep->mt.MT_TABLE = mt;
        wep->mt.MT_TRACK = enemy;
        wep->SPRITE_DIR = dir;
        wep->SPRITE_ID = enemy->SPRITE_ID;
      }
    }
  } else {
    if (enemy->at.AT_INDEX == 0) {
      flags &= ~(FSP_ACTION | FSP_COLLIDE);
      enemy->at.AT_TABLE = at_skeleton;
      enemy->at.AT_LENGTH = (sizeof(at_skeleton) / sizeof(int));
      enemy->at.AT_COUNT = 1;
      enemy->at.AT_SPEED = 1;
      enemy->mv.MV_XVEL = (enemy->mv.MV_MAXPX * enemy->SPRITE_DIR);
    }
  }
  enemy->SPRITE_FLAGS = flags;
}

void cp_balista(Sprite *node, CP *cp) {
  sp_balista *enemy = (sp_balista *)node;
  if ((enemy->at.AT_COUNT == 0) && (enemy->at.AT_INDEX == 3)) {
    sp_balista_arrow *wep = new sp_balista_arrow();
    if (wep != 0) {
      GLOBALS_MISSILE_DLIST.addtail(wep);
      int dir = enemy->SPRITE_DIR;
      int x = enemy->SPRITE_X;
      x += (dir * 8);
      wep->SPRITE_X = x;
      wep->SPRITE_Y = enemy->SPRITE_Y;
      wep->SPRITE_DIR = dir;
      wep->mv.MV_XVEL = wep->mv.MV_MAXPX * dir;
      play_twang1(enemy);
    }
  }
}

void cp_oil(Sprite *node, CP *cp) {
  sp_oil *enemy = (sp_oil *)node;
  if ((enemy->at.AT_COUNT == 0) && (enemy->at.AT_INDEX == 5)) {
    sp_oil_drop *wep = new sp_oil_drop();
    if (wep != 0) {
      GLOBALS_MISSILE_DLIST.addtail(wep);
      wep->SPRITE_X = enemy->SPRITE_X + 8;
      wep->SPRITE_Y = enemy->SPRITE_Y + 16;
      play_spell(enemy);
    }
  }
}

void cp_cannon(Sprite *node, CP *cp) {
  sp_cannon *enemy = (sp_cannon *)node;
  if ((enemy->at.AT_COUNT == 3) && (enemy->at.AT_INDEX == 5)) {
    sp_cannon_flame *wep = new sp_cannon_flame();
    if (wep != 0) {
      enemy->addnodeb(wep);
      int dir = enemy->SPRITE_DIR;
      int x = enemy->SPRITE_X;
      if (dir == 1) {
        x += 32;
      } else {
        x -= 16;
      }
      int y = enemy->SPRITE_Y + 16;
      wep->SPRITE_X = x;
      wep->SPRITE_Y = y;
      wep->SPRITE_DIR = dir;
      wep->SPRITE_ID = enemy->SPRITE_ID;
      sp_cannon_ball *wep = new sp_cannon_ball();
      if (wep != 0) {
        GLOBALS_MISSILE_DLIST.addtail(wep);
        if (dir == 1) {
          x = enemy->SPRITE_X + 16;
        } else {
          x = enemy->SPRITE_X;
        }
        wep->SPRITE_X = x;
        wep->SPRITE_Y = y;
        wep->SPRITE_DIR = dir;
        wep->mv.MV_XVEL = wep->mv.MV_MAXPX * dir;
        play_explode(enemy);
      }
    }
  }
}

void cp_beserk(Sprite *node, CP *cp) {
  sp_beserk *enemy = (sp_beserk *)node;
  int index = enemy->at.AT_INDEX;
  if ((enemy->at.AT_COUNT == 2) && ((index == 0) || (index == 3))) {
    sp_beserk_mase *wep = new sp_beserk_mase();
    if (wep != 0) {
      enemy->addnodeb(wep);
      int dir = enemy->SPRITE_DIR;
      int *atable, *mtable;
      if (index != 0) {
        atable = at_beserk_lower_mase;
        if (dir == 1) {
          mtable = mt_beserk_lower_mase_r;
        } else {
          mtable = mt_beserk_lower_mase_l;
        }
      } else {
        atable = at_beserk_upper_mase;
        if (dir == 1) {
          mtable = mt_beserk_upper_mase_r;
        } else {
          mtable = mt_beserk_upper_mase_l;
        }
      }
      wep->at.AT_TABLE = atable;
      wep->mt.MT_TABLE = mtable;
      wep->mt.MT_TRACK = enemy;
      wep->SPRITE_DIR = dir;
      wep->SPRITE_ID = enemy->SPRITE_ID;
    }
  }
}

void cp_tower(Sprite *node, CP *cp) {
  sp_tower *enemy = (sp_tower *)node;
  int x = enemy->SPRITE_X;
  int y = (enemy->SPRITE_Y + enemy->SPRITE_H);
  if (enemy->SPRITE_DIR == 1) {
    x += (enemy->SPRITE_W - 1);
  }
  map *land = GLOBALS_LAND;
  int flags = land->getmapflags(x, y);
  if ((flags & FMAP_STAND) == 0) {
    enemy->SPRITE_DIR = -enemy->SPRITE_DIR;
    enemy->mv.MV_XVEL = -enemy->mv.MV_XVEL;
  }
  if ((enemy->at.AT_INDEX == 4) && (enemy->at.AT_COUNT == 0)) {
    x = enemy->SPRITE_X;
    y = enemy->SPRITE_Y;
    sp_spear *mis = new sp_spear();
    if (mis != 0) {
      GLOBALS_MISSILE_DLIST.addtail(mis);
      mis->SPRITE_X = x;
      mis->SPRITE_Y = y;
      mis->mv.MV_XVEL = mis->mv.MV_MAXNX;
      mis->SPRITE_DIR = -1;
    }
    mis = new sp_spear();
    if (mis != 0) {
      GLOBALS_MISSILE_DLIST.addtail(mis);
      mis->SPRITE_X = x;
      mis->SPRITE_Y = y + 32;
      mis->mv.MV_XVEL = mis->mv.MV_MAXNX;
      mis->SPRITE_DIR = -1;
    }
    mis = new sp_spear();
    if (mis != 0) {
      GLOBALS_MISSILE_DLIST.addtail(mis);
      mis->SPRITE_X = x + 32;
      mis->SPRITE_Y = y;
      mis->mv.MV_XVEL = mis->mv.MV_MAXPX;
      mis->SPRITE_DIR = 1;
    }
    mis = new sp_spear();
    if (mis != 0) {
      GLOBALS_MISSILE_DLIST.addtail(mis);
      mis->SPRITE_X = x + 32;
      mis->SPRITE_Y = y + 32;
      mis->mv.MV_XVEL = mis->mv.MV_MAXPX;
      mis->SPRITE_DIR = 1;
    }
  }
}

void cp_bone(Sprite *node, CP *cp) {
  int cnt = node->SPRITE_USER1;
  cnt--;
  if (cnt <= 0) {
    killsprite(node);
  } else {
    node->SPRITE_USER1 = cnt;
    cp_bounce(node, cp);
  }
}

void cp_item(Sprite *node, CP *cp) {
  sp_gold_item *item = (sp_gold_item *)node;
  if (item->mv.MV_YACC != 0) {
    int yv = item->mv.MV_YVEL;
    int y = item->SPRITE_Y;
    int h = item->SPRITE_H;
    y = ((y + yv + h) & -TILE_HEIGHT);
    int oy = item->SPRITE_USER1;
    item->SPRITE_USER1 = y;
    if ((y != oy) && (yv > 0)) {
      int x = item->SPRITE_X + (item->SPRITE_W >> 1);
      map *land = GLOBALS_LAND;
      int flags = land->getmapflags(x, y);
      if (((flags & FMAP_CLIMB) == 0) && ((flags & FMAP_STAND) != 0)) {
        y -= h;
        item->SPRITE_Y = y;
        item->mv.MV_YVEL = 0;
        item->mv.MV_YACC = 0;
        dt_fizz_fx(item);
      }
    }
  } else {
    // stood still so start timer
    int cnt = item->SPRITE_USER3;
    cnt--;
    if (cnt <= 0) {
      killsprite(item);
    } else {
      item->SPRITE_USER3 = cnt;
    }
  }
}

void cp_letter(Sprite *node, CP *cp) {
  sp_letter *letter = (sp_letter *)node;
  if (letter->ml.ML_SPEED != 0) {
    if (letter->ml.ML_COUNT >= 0) {
      dt_fizz_fx(letter);
    } else {
      letter->ml.ML_SPEED = 0;
      dt_bomb_fx(letter);
      GLOBALS_GAME_FLAGS |= FFLAG_TITLE;
    }
  }
}

void cp_sword(Sprite *node, CP *cp) {
  sp_sword *sword = (sp_sword *)node;
  if ((sword->ml.ML_SPEED != 0) && (sword->ml.ML_COUNT < 0)) {
    sword->ml.ML_SPEED = 0;
    dt_fizz_fx(sword);
    sp_drip *drip = new sp_drip();
    if (drip != 0) {
      sword->addnode(drip);
      drip->SPRITE_X = sword->SPRITE_X;
      drip->SPRITE_Y = sword->SPRITE_Y + 32;
      play_clash1(sword);
    }
  }
}

// use weapon handlers

void use_mase(Sprite *node) {
  sp_man *man = (sp_man *)node;
  sp_mase *wep = new sp_mase();
  if (wep != 0) {
    man->addnodeb(wep);
    int dir = man->SPRITE_DIR;
    int *mt, *at, *atm;
    if (man->SPRITE_FRAME >= FRM_DUCK) {
      atm = at_man_lower_mase;
      at = at_lower_mase;
      if (dir == 1) {
        mt = mt_lower_mase_r;
      } else {
        mt = mt_lower_mase_l;
      }
    } else {
      atm = at_man_upper_mase;
      at = at_upper_mase;
      if (dir == 1) {
        mt = mt_upper_mase_r;
      } else {
        mt = mt_upper_mase_l;
      }
    }
    man->at.AT_TABLE = atm;
    man->at.AT_LENGTH = (sizeof(at_man_lower_mase) / sizeof(int));
    man->at.AT_SPEED = 2;
    man->at.AT_COUNT = 2;
    man->at.AT_INDEX = 0;
    wep->at.AT_TABLE = at;
    wep->mt.MT_TABLE = mt;
    wep->mt.MT_TRACK = man;
    wep->SPRITE_DIR = dir;
    wep->SPRITE_ID = man->SPRITE_ID;
    play_throw(man);
  }
}

void use_big_crossbow(Sprite *node) {
  sp_man *man = (sp_man *)node;
  sp_big_crossbow *wep = new sp_big_crossbow();
  if (wep != 0) {
    man->addnodeb(wep);
    int dir = man->SPRITE_DIR;
    int *mt;
    if (dir == 1) {
      mt = mt_big_crossbow_r;
    } else {
      mt = mt_big_crossbow_l;
    }
    man->at.AT_TABLE = at_man_big_crossbow;
    man->at.AT_LENGTH = (sizeof(at_man_big_crossbow) / sizeof(int));
    man->at.AT_SPEED = 2;
    man->at.AT_COUNT = 2;
    man->at.AT_INDEX = 0;
    wep->mt.MT_TABLE = mt;
    wep->mt.MT_TRACK = man;
    wep->SPRITE_DIR = dir;
    wep->SPRITE_ID = man->SPRITE_ID;
  }
}

void use_small_crossbow(Sprite *node) {
  sp_man *man = (sp_man *)node;
  sp_small_crossbow *wep = new sp_small_crossbow();
  if (wep != 0) {
    man->addnodeb(wep);
    int dir = man->SPRITE_DIR;
    int *mt;
    if (dir == 1) {
      mt = mt_small_crossbow_r;
    } else {
      mt = mt_small_crossbow_l;
    }
    man->at.AT_TABLE = at_man_small_crossbow;
    man->at.AT_LENGTH = (sizeof(at_man_small_crossbow) / sizeof(int));
    man->at.AT_SPEED = 1;
    man->at.AT_COUNT = 1;
    man->at.AT_INDEX = 0;
    wep->mt.MT_TABLE = mt;
    wep->mt.MT_TRACK = man;
    wep->SPRITE_DIR = dir;
    wep->SPRITE_ID = man->SPRITE_ID;
  }
}

void use_naptha(Sprite *node) {
  sp_man *man = (sp_man *)node;
  sp_naptha *wep = new sp_naptha();
  if (wep != 0) {
    man->addnodeb(wep);
    int dir = man->SPRITE_DIR;
    int *mt;
    if (dir == 1) {
      mt = mt_naptha_r;
    } else {
      mt = mt_naptha_l;
    }
    man->at.AT_TABLE = at_man_naptha;
    man->at.AT_LENGTH = (sizeof(at_man_naptha) / sizeof(int));
    man->at.AT_SPEED = 3;
    man->at.AT_COUNT = 3;
    man->at.AT_INDEX = 0;
    wep->mt.MT_TABLE = mt;
    wep->mt.MT_TRACK = man;
    wep->SPRITE_DIR = dir;
    wep->SPRITE_ID = man->SPRITE_ID;
    play_throw(man);
  }
}

void use_helper(Sprite *node) {
  sp_helper *wep = new sp_helper();
  if (wep != 0) {
    GLOBALS_MAN_DLIST.addtail(wep);
    wep->SPRITE_X = node->SPRITE_X + 8;
    wep->SPRITE_Y = node->SPRITE_Y + ((node->SPRITE_H >> 1) - 8);
  }
}

void use_super_mase(Sprite *node) {
  sp_man *man = (sp_man *)node;
  sp_mase *wep = new sp_mase();
  if (wep != 0) {
    man->addnodeb(wep);
    int dir = man->SPRITE_DIR;
    int *mt, *at, *atm;
    if (man->SPRITE_FRAME >= FRM_DUCK) {
      atm = at_man_lower_mase;
      at = at_lower_mase;
      if (dir == 1) {
        mt = mt_lower_mase_r;
      } else {
        mt = mt_lower_mase_l;
      }
    } else {
      atm = at_man_upper_mase;
      at = at_upper_mase;
      if (dir == 1) {
        mt = mt_upper_mase_r;
      } else {
        mt = mt_upper_mase_l;
      }
    }
    man->at.AT_TABLE = atm;
    man->at.AT_LENGTH = (sizeof(at_man_lower_mase) / sizeof(int));
    man->at.AT_SPEED = 2;
    man->at.AT_COUNT = 2;
    man->at.AT_INDEX = 0;
    wep->at.AT_TABLE = at;
    wep->mt.MT_TABLE = mt;
    wep->mt.MT_TRACK = man;
    wep->SPRITE_DIR = dir;
    wep->SPRITE_ID = man->SPRITE_ID;
    wep->SPRITE_HITPNT = 1000;
    play_throw(man);
  }
}

void use_super_helper(Sprite *node) {
  sp_super_helper *wep = new sp_super_helper();
  if (wep != 0) {
    GLOBALS_MAN_DLIST.addtail(wep);
    wep->SPRITE_X = node->SPRITE_X + 8;
    wep->SPRITE_Y = node->SPRITE_Y + ((node->SPRITE_H >> 1) - 8);
  }
}

void use_yellow_spell(Sprite *node) {
  sp_elemental *wep = new sp_elemental();
  if (wep != 0) {
    GLOBALS_MAN_DLIST.addtail(wep);
    wep->SPRITE_X = node->SPRITE_X;
    wep->SPRITE_Y = node->SPRITE_Y - 32;
    play_roar(wep);
  }
}

void use_black_spell(Sprite *node) {
  int index = random(sizeof(blackspell) / (sizeof(int) * 2));
  index *= 2;
  int item = blackspell[index];
  int use = blackspell[index + 1];
  index = 0;
  do {
    GLOBALS_ITEM_INVENTORY[index] = item;
    GLOBALS_ITEM_USEAGE[index] = use;
    index++;
  } while (index != (sizeof(GLOBALS_ITEM_INVENTORY) / sizeof(int)));
  play_spell(node);
}

Listnode *use_green_spell_cb(Listhead *list, Listnode *node, void *user) {
  sp_enemy *enemy = (sp_enemy *)node;
  if (enemy->SPRITE_DEATH == dt_kill_enemy) {
    enemy->SPRITE_DEATH = dt_kill_zap;
  }
  killsprite(enemy);
  play_death_score(enemy);
  return (0);
}

void use_green_spell(Sprite *node) {
  GLOBALS_ENEMY_DLIST.enumerate(use_green_spell_cb, 0);
  play_spell(node);
}

Listnode *use_white_spell_cb(Listhead *list, Listnode *node, void *user) {
  sp_enemy *enemy = (sp_enemy *)node;
  if ((enemy->SPRITE_TYPE &
       (FTP_CANNON | FTP_BALISTA | FTP_OIL | FTP_BESERK)) == 0) {
    enemy->mv.MV_XVEL = 0;
    enemy->mv.MV_MAXPX = 0;
    enemy->mv.MV_MAXNX = 0;
  }
  return (0);
}

void use_white_spell(Sprite *node) {
  GLOBALS_ENEMY_DLIST.enumerate(use_white_spell_cb, 0);
  play_spell(node);
}

void use_red_spell(Sprite *node) {
  int val = GLOBALS_MAN_STRENGTH;
  val += MAXSTRENGTH / 5;
  if (val > MAXSTRENGTH) {
    val = MAXSTRENGTH;
  }
  GLOBALS_MAN_STRENGTH = val;
  play_spell(node);
}

void use_blue_spell(Sprite *node) {
  int val = GLOBALS_MAN_POWER;
  val += MAXPOWER / 5;
  if (val > MAXPOWER) {
    val = MAXPOWER;
  }
  GLOBALS_MAN_POWER = val;
  play_spell(node);
}

void use_gold(Sprite *node) {
  GLOBALS_MAN_SCORE += 500;
  play_spell(node);
}

Listnode *use_talisman_cb1(Listhead *list, Listnode *node, void *user) {
  Sprite *enemy = (Sprite *)node;
  killsprite(enemy);
  play_death_score(enemy);
  return (0);
}

Listnode *use_talisman_cb2(Listhead *list, Listnode *node, void *user) {
  Sprite *enemy = (Sprite *)node;
  if (enemy->SPRITE_TYPE == FTP_ENEMY_FIRE) {
    killsprite(enemy);
  }
  return (0);
}

void use_talisman(Sprite *node) {
  GLOBALS_MISSILE_DLIST.enumerate(use_talisman_cb1, 0);
  GLOBALS_BANS_DLIST.enumerate(use_talisman_cb2, 0);
  play_spell(node);
}

// death handlers

void dt_man(Sprite *node) {
  int *table = manbits_vectors;
  int x = (node->SPRITE_W >> 1) + node->SPRITE_X - 8;
  int y = (node->SPRITE_H >> 1) + node->SPRITE_Y - 8;
  int index = 0;
  do {
    sp_manbits *sfx = new sp_manbits();
    if (sfx != 0) {
      GLOBALS_MAN_DLIST.addtail(sfx);
      sfx->SPRITE_X = x;
      sfx->SPRITE_Y = y;
      sfx->SPRITE_FRAME = table[index];
      sfx->mv.MV_XVEL = table[index + 1];
      sfx->mv.MV_YVEL = table[index + 2];
    }
    index += 3;
  } while (index != (sizeof(manbits_vectors) / sizeof(int)));
  dt_exp_fx(node);
}

void dt_blat_fx(Sprite *node) {
  int *table = blat_offsets;
  int x = (node->SPRITE_W >> 1) + node->SPRITE_X;
  int y = (node->SPRITE_H >> 1) + node->SPRITE_Y;
  int index = 0;
  do {
    sp_small_explosion *sfx = new sp_small_explosion();
    if (sfx != 0) {
      GLOBALS_FX_DLIST.addtail(sfx);
      sfx->SPRITE_X = x + table[index];
      sfx->SPRITE_Y = y + table[index + 1];
    }
    index += 2;
  } while (index != (sizeof(blat_offsets) / sizeof(int)));
  play_explode(node);
}

void dt_fizz_fx(Sprite *node) {
  sp_fizz *sfx = new sp_fizz();
  if (sfx != 0) {
    GLOBALS_FX_DLIST.addtail(sfx);
    sfx->SPRITE_X = (((node->SPRITE_W >> 1) + node->SPRITE_X) - 8);
    sfx->SPRITE_Y = (((node->SPRITE_H >> 1) + node->SPRITE_Y) - 8);
  }
}

void dt_exp_fx(Sprite *node) {
  sp_small_explosion *sfx = new sp_small_explosion();
  if (sfx != 0) {
    GLOBALS_FX_DLIST.addtail(sfx);
    sfx->SPRITE_X = (((node->SPRITE_W >> 1) + node->SPRITE_X) - 16);
    sfx->SPRITE_Y = (((node->SPRITE_H >> 1) + node->SPRITE_Y) - 16);
    play_explode(node);
  }
}

void dt_hand_fire(Sprite *node) {
  GLOBALS_MIND_FIRECNT--;
  dt_fizz_fx(node);
}

void dt_hand_fire1(Sprite *node) {
  GLOBALS_MIND_FIRECNT--;
  dt_exp_fx(node);
}

void dt_bomb_fx(Sprite *node) {
  int *table = bomb_offsets;
  int x = (node->SPRITE_W >> 1) + node->SPRITE_X;
  int y = (node->SPRITE_H >> 1) + node->SPRITE_Y;
  int index = 0;
  do {
    sp_small_explosion *sfx = new sp_small_explosion();
    if (sfx != 0) {
      GLOBALS_FX_DLIST.addtail(sfx);
      sfx->SPRITE_X = x + table[index];
      sfx->SPRITE_Y = y + table[index + 1];
    }
    index += 2;
  } while (index != (sizeof(bomb_offsets) / sizeof(int)));
  play_explode(node);
}

void dt_elemental_explode(Sprite *node) {
  int *table = frag_vectors;
  int x = 8 + node->SPRITE_X;
  int y = 8 + node->SPRITE_Y;
  int index = 0;
  do {
    sp_frag *wep = new sp_frag();
    if (wep != 0) {
      GLOBALS_MAN_DLIST.addtail(wep);
      wep->SPRITE_X = x;
      wep->SPRITE_Y = y;
      wep->mv.MV_XVEL = table[index];
      wep->mv.MV_YVEL = table[index + 1];
    }
    index += 2;
  } while (index != (sizeof(frag_vectors) / sizeof(int)));
  dt_exp_fx(node);
}

void dt_super_helper_transform(Sprite *node) {
  sp_elemental *wep = new sp_elemental();
  if (wep != 0) {
    GLOBALS_MAN_DLIST.addtail(wep);
    wep->SPRITE_X = node->SPRITE_X - 8;
    wep->SPRITE_Y = node->SPRITE_Y - 8;
    play_roar(node);
  }
}

void dt_mine_explode(Sprite *node) {
  GLOBALS_GAME_FLAGS |= FFLAG_MINE;
  GLOBALS_MINE_COUNT--;
  dt_blat_fx(node);
  blast_area((node->SPRITE_X - 32), (node->SPRITE_Y - 36), 80, 80);
}

void dt_kill_horse(Sprite *node) {
  sp_knight *enemy = new sp_knight();
  if (enemy != 0) {
    GLOBALS_ENEMY_DLIST.addtail(enemy);
    enemy->SPRITE_X = node->SPRITE_X + 16;
    enemy->SPRITE_Y = node->SPRITE_Y;
    int dir = node->SPRITE_DIR;
    enemy->mv.MV_XVEL = enemy->mv.MV_MAXPX * dir;
    enemy->SPRITE_DIR = dir;
    GLOBALS_ENEMY_COUNT += enemy->SPRITE_USER1 & 0xffff;
  }
  dt_kill_enemy_noitem(node);
}

void dt_kill_skeleton_horse(Sprite *node) {
  sp_skeleton *enemy = new sp_skeleton();
  if (enemy != 0) {
    GLOBALS_ENEMY_DLIST.addtail(enemy);
    enemy->SPRITE_X = node->SPRITE_X + 16;
    enemy->SPRITE_Y = node->SPRITE_Y;
    int dir = node->SPRITE_DIR;
    enemy->mv.MV_XVEL = enemy->mv.MV_MAXPX * dir;
    enemy->SPRITE_DIR = dir;
    GLOBALS_ENEMY_COUNT += enemy->SPRITE_USER1 & 0xffff;
  }
  dt_kill_bones_noitem(node);
}

void dt_kill_enemy(Sprite *node) {
  drop_item(node);
  dt_kill_enemy_noitem(node);
}

void dt_kill_enemy_noitem(Sprite *node) {
  GLOBALS_ENEMY_COUNT -= node->SPRITE_USER1 & 0xffff;
  sp_body *sfx = new sp_body();
  if (sfx != 0) {
    GLOBALS_BODY_DLIST.addtail(sfx);
    sfx->SPRITE_X = node->SPRITE_X;
    sfx->SPRITE_Y = node->SPRITE_Y;
    sfx->SPRITE_W = node->SPRITE_W;
    int h = node->SPRITE_H;
    if (h == 16) {
      // if ducking make sure to set the height to 32
      h = 32;
    }
    sfx->SPRITE_H = h;
    sfx->SPRITE_FRAME = node->SPRITE_USER3;
    int dir = node->SPRITE_DIR;
    sfx->SPRITE_DIR = dir;
    sfx->mv.MV_XVEL = -(dir * 4);
    sfx->SPRITE_PIX = node->SPRITE_PIX;
  }
}

void dt_kill_bones_noitem(Sprite *node) {
  GLOBALS_ENEMY_COUNT -= node->SPRITE_USER1 & 0xffff;
  int *table = bone_vectors;
  int x = node->SPRITE_X - 8;
  int y = node->SPRITE_Y - 8;
  int index = 0;
  do {
    sp_bone *sfx = new sp_bone();
    if (sfx != 0) {
      GLOBALS_FX_DLIST.addtail(sfx);
      sfx->SPRITE_X = x;
      sfx->SPRITE_Y = y;
      sfx->mv.MV_XVEL = table[index];
      sfx->mv.MV_YVEL = table[index + 1];
    }
    index += 2;
  } while (index != (sizeof(bone_vectors) / sizeof(int)));
}

void dt_kill_monk(Sprite *node) {
  GLOBALS_GAME_FLAGS |= FFLAG_MINE;
  dt_blat_fx(node);
  blast_area((node->SPRITE_X - 24), (node->SPRITE_Y - 24), 80, 80);
  drop_item(node);
  GLOBALS_ENEMY_COUNT -= node->SPRITE_USER1 & 0xffff;
}

void dt_kill_zap(Sprite *node) {
  drop_item(node);
  dt_kill_zap_noitem(node);
}

void dt_kill_zap_noitem(Sprite *node) {
  GLOBALS_ENEMY_COUNT -= node->SPRITE_USER1 & 0xffff;
  int x = node->SPRITE_X;
  int y = node->SPRITE_Y;
  int w = node->SPRITE_W;
  int h = node->SPRITE_H;
  w -= 16;
  h -= 16;
  int cnt = 8;
  do {
    sp_fizz *sfx = new sp_fizz();
    if (sfx != 0) {
      GLOBALS_FX_DLIST.addtail(sfx);
      int rx = random(w);
      int ry = random(h);
      rx += x;
      ry += y;
      sfx->SPRITE_X = rx;
      sfx->SPRITE_Y = ry;
    }
    cnt--;
  } while (cnt != 0);
}

void dt_kill_skeleton(Sprite *node) {
  drop_item(node);
  dt_kill_bones_noitem(node);
}

void dt_kill_balista(Sprite *node) {
  sp_footman *enemy = new sp_footman();
  if (enemy != 0) {
    GLOBALS_ENEMY_DLIST.addtail(enemy);
    enemy->SPRITE_X = node->SPRITE_X;
    enemy->SPRITE_Y = node->SPRITE_Y;
    int dir = node->SPRITE_DIR;
    enemy->mv.MV_XVEL = enemy->mv.MV_MAXPX * dir;
    enemy->SPRITE_DIR = dir;
    GLOBALS_ENEMY_COUNT += enemy->SPRITE_USER1 & 0xffff;
  }
  dt_kill_enemy_noitem(node);
}

void dt_kill_tower(Sprite *node) {
  int x = node->SPRITE_X;
  int y = node->SPRITE_Y;
  sp_spearman *enemy = new sp_spearman();
  if (enemy != 0) {
    GLOBALS_ENEMY_DLIST.addtail(enemy);
    enemy->SPRITE_X = x;
    enemy->SPRITE_Y = y;
    enemy->mv.MV_XVEL = enemy->mv.MV_MAXNX;
    enemy->SPRITE_DIR = -1;
    GLOBALS_ENEMY_COUNT += enemy->SPRITE_USER1 & 0xffff;
  }
  enemy = new sp_spearman();
  if (enemy != 0) {
    GLOBALS_ENEMY_DLIST.addtail(enemy);
    enemy->SPRITE_X = x + 32;
    enemy->SPRITE_Y = y;
    enemy->mv.MV_XVEL = enemy->mv.MV_MAXPX;
    enemy->SPRITE_DIR = 1;
    GLOBALS_ENEMY_COUNT += enemy->SPRITE_USER1 & 0xffff;
  }
  enemy = new sp_spearman();
  if (enemy != 0) {
    GLOBALS_ENEMY_DLIST.addtail(enemy);
    enemy->SPRITE_X = x;
    enemy->SPRITE_Y = y + 32;
    enemy->mv.MV_XVEL = enemy->mv.MV_MAXNX;
    enemy->SPRITE_DIR = -1;
    GLOBALS_ENEMY_COUNT += enemy->SPRITE_USER1 & 0xffff;
  }
  enemy = new sp_spearman();
  if (enemy != 0) {
    GLOBALS_ENEMY_DLIST.addtail(enemy);
    enemy->SPRITE_X = x + 32;
    enemy->SPRITE_Y = y + 32;
    enemy->mv.MV_XVEL = enemy->mv.MV_MAXPX;
    enemy->SPRITE_DIR = 1;
    GLOBALS_ENEMY_COUNT += enemy->SPRITE_USER1 & 0xffff;
  }
  dt_blat_fx(node);
  GLOBALS_ENEMY_COUNT -= node->SPRITE_USER1 & 0xffff;
}

void dt_kill_carpet(Sprite *node) {
  int x = node->SPRITE_X;
  int y = node->SPRITE_Y;
  int dir = node->SPRITE_DIR;
  sp_wizard *enemy = new sp_wizard();
  if (enemy != 0) {
    GLOBALS_ENEMY_DLIST.addtail(enemy);
    enemy->SPRITE_X = x;
    enemy->SPRITE_Y = y;
    enemy->mv.MV_XVEL = enemy->mv.MV_MAXPX * dir;
    enemy->SPRITE_DIR = dir;
    GLOBALS_ENEMY_COUNT += enemy->SPRITE_USER1 & 0xffff;
  }
  enemy = new sp_wizard();
  if (enemy != 0) {
    GLOBALS_ENEMY_DLIST.addtail(enemy);
    enemy->SPRITE_X = x + 32;
    enemy->SPRITE_Y = y;
    enemy->mv.MV_XVEL = enemy->mv.MV_MAXPX * dir;
    enemy->SPRITE_DIR = dir;
    GLOBALS_ENEMY_COUNT += enemy->SPRITE_USER1 & 0xffff;
  }
  dt_kill_zap_noitem(node);
}

void dt_kill_boarrider(Sprite *node) {
  sp_spearman *enemy = new sp_spearman();
  if (enemy != 0) {
    GLOBALS_ENEMY_DLIST.addtail(enemy);
    enemy->SPRITE_X = node->SPRITE_X + 16;
    enemy->SPRITE_Y = node->SPRITE_Y;
    int dir = node->SPRITE_DIR;
    enemy->mv.MV_XVEL = enemy->mv.MV_MAXPX * dir;
    enemy->SPRITE_DIR = dir;
    GLOBALS_ENEMY_COUNT += enemy->SPRITE_USER1 & 0xffff;
  }
  dt_kill_enemy_noitem(node);
}

void dt_skeleton_item(Sprite *node) {
  sp_skeleton *enemy = new sp_skeleton();
  if (enemy != 0) {
    GLOBALS_ENEMY_DLIST.addtail(enemy);
    enemy->SPRITE_X = node->SPRITE_X - 8;
    enemy->SPRITE_Y = node->SPRITE_Y - 16;
    int dir = node->SPRITE_DIR;
    enemy->mv.MV_XVEL = enemy->mv.MV_MAXPX * dir;
    enemy->SPRITE_DIR = dir;
    GLOBALS_ENEMY_COUNT += enemy->SPRITE_USER1 & 0xffff;
    dt_fizz_fx(enemy);
  }
}

void dt_drip(Sprite *node) { play_drip(node); }

// collision handlers

void cl_hand_hits_fire(Sprite *node, Sprite *hit) {
  killsprite(hit);
  hit->SPRITE_DEATH = dt_exp_fx;
  int power = GLOBALS_MAN_POWER;
  power -= hit->SPRITE_HITPNT;
  if (power < 0) {
    power = 0;
  }
  GLOBALS_MAN_POWER = power;
  if (power == 0) {
    killsprite(node);
  }
}

void cl_hand_hits_item(Sprite *node, Sprite *hit) {
  int item = hit->SPRITE_FRAME;
  if (item == (FRM_SPELLS + 5)) {
    // blue spell
    item = item_carry((FRM_DIAMOND + 4));
    int power;
    if (item == -1) {
      // not carrying mind talisman
      power = MAXPOWER / 5;
    } else {
      // carrying mind talisman
      power = MAXPOWER / 10;
    }
    power += GLOBALS_MAN_POWER;
    if (power > MAXPOWER) {
      power = MAXPOWER;
    }
    GLOBALS_MAN_POWER = power;
    killsprite(hit);
  } else {
    // try to pick up item
    item_collect(hit);
  }
  play_clash2(node);
}

void cl_mind_hits_fire(Sprite *node, Sprite *hit) {
  // kill hitter
  killsprite(hit);
  hit->SPRITE_DEATH = dt_hand_fire1;

  // open mouth
  node->SPRITE_FRAME = (((node->SPRITE_FRAME - FRM_FACES) | 1) + FRM_FACES);

  // reduce hit points
  int cnt = node->SPRITE_HITPNT;
  cnt -= hit->SPRITE_HITPNT;
  node->SPRITE_HITPNT = cnt;
  if (cnt <= 0) {
    // reduce segments
    node->SPRITE_HITPNT = 8;
    cnt = node->SPRITE_USER1 & 0xffff;
    cnt--;
    if (cnt < 0) {
      cnt = 0;
    }
    node->SPRITE_USER1 = (node->SPRITE_USER1 & 0xffff0000) + cnt;
    if (cnt == 0) {
      // dead
      killsprite(node);
    }
  }
}

void cl_man_missile_hits(Sprite *node, Sprite *hit) {
  int type =
      hit->SPRITE_TYPE &
      (FTP_ENEMY_FIRE | FTP_CANNON | FTP_OIL | FTP_BALISTA | FTP_TOWER |
       FTP_CARPET | FTP_SKELETON_MONK | FTP_SKELETON_HORSE | FTP_SKELETON);
  if (type != 0) {
    // not flesh
    dt_fizz_fx(node);
  } else {
    // flesh
    sp_blood *sfx = new sp_blood();
    if (sfx != 0) {
      GLOBALS_FX_DLIST.addtail(sfx);
      sfx->SPRITE_X = (((node->SPRITE_W >> 1) + node->SPRITE_X) - 8);
      sfx->SPRITE_Y = (((node->SPRITE_H >> 1) + node->SPRITE_Y) - 8);
      int dir = node->SPRITE_DIR;
      if (dir == 0) {
        dir = -hit->SPRITE_DIR;
      }
      sfx->SPRITE_DIR = dir;
      sfx->mv.MV_XVEL = (dir * 4);
    }
  }
  int ohitpnt = node->SPRITE_HITPNT;
  int hitpnt = (ohitpnt - hit->SPRITE_HITPNT);
  if (hitpnt <= 0) {
    // has killed weapon
    killsprite(node);
  } else {
    node->SPRITE_HITPNT = hitpnt;
  }
  hitpnt = hit->SPRITE_HITPNT;
  hitpnt -= ohitpnt;
  if (hitpnt <= 0) {
    // has killed it
    sprite_kill_list_id(&GLOBALS_ENEMY_DLIST, hit->SPRITE_ID);
    if (ohitpnt >= 500) {
      // magic death ?
      if (hit->SPRITE_DEATH == dt_kill_enemy) {
        hit->SPRITE_DEATH = dt_kill_zap;
      }
    }
    play_death_score(hit);
  } else {
    hit->SPRITE_HITPNT = hitpnt;
  }
}

void cl_man_addon_hits(Sprite *node, Sprite *hit) {
  int rand = random(2);
  if (rand == 0) {
    play_clash1(node);
  } else {
    play_clash2(node);
  }
  int type =
      hit->SPRITE_TYPE &
      (FTP_ENEMY_FIRE | FTP_CANNON | FTP_OIL | FTP_BALISTA | FTP_TOWER |
       FTP_CARPET | FTP_SKELETON_MONK | FTP_SKELETON_HORSE | FTP_SKELETON);
  if (type != 0) {
    // not flesh
    dt_fizz_fx(node);
  } else {
    // flesh
    sp_blood *sfx = new sp_blood();
    if (sfx != 0) {
      GLOBALS_FX_DLIST.addtail(sfx);
      sfx->SPRITE_X = (((node->SPRITE_W >> 1) + node->SPRITE_X) - 8);
      sfx->SPRITE_Y = (((node->SPRITE_H >> 1) + node->SPRITE_Y) - 8);
      int dir = node->SPRITE_DIR;
      if (dir == 0) {
        dir = hit->SPRITE_DIR;
        dir = -dir;
      }
      sfx->SPRITE_DIR = dir;
      sfx->mv.MV_XVEL = (dir * 4);
    }
  }
  int hitpnt = hit->SPRITE_HITPNT;
  hitpnt -= node->SPRITE_HITPNT;
  if (hitpnt <= 0) {
    // has killed it
    sprite_kill_list_id(&GLOBALS_ENEMY_DLIST, hit->SPRITE_ID);
    if (node->SPRITE_HITPNT >= 500) {
      // magic death ?
      if (hit->SPRITE_DEATH == dt_kill_enemy) {
        hit->SPRITE_DEATH = dt_kill_zap;
      }
    }
    play_death_score(hit);
  } else {
    hit->SPRITE_HITPNT = hitpnt;
  }
}

void cl_man_hits_enemy_banner(Sprite *node, Sprite *hit) {
  // signal captured
  if ((node->SPRITE_Y + node->SPRITE_H) == (hit->SPRITE_Y + hit->SPRITE_H)) {
    GLOBALS_GAME_FLAGS |= FFLAG_ENEMY;
    int frame = hit->SPRITE_FRAME;
    killsprite(hit);
    int power = GLOBALS_MAN_POWER;
    if (GLOBALS_MAN_BANNER == frame) {
      // alide
      power /= 2;
    } else {
      // not alide
      power *= 2;
      if (power > MAXPOWER) {
        power = MAXPOWER;
      }
    }
    GLOBALS_MAN_POWER = power;
    play_spell(node);
  }
}

void cl_man_hits_banner(Sprite *node, Sprite *hit) {
  if (GLOBALS_MAN_POWER < (MAXPOWER / 2)) {
    GLOBALS_MAN_POWER = (MAXPOWER / 2);
  }
}

void cl_enemy_addon_hits_man(Sprite *node, Sprite *hit) {
  int cnt = GLOBALS_MAN_STRENGTH;
  cnt -= node->SPRITE_HITPNT;
  if (cnt < 0) {
    cnt = 0;
  }
  GLOBALS_MAN_STRENGTH = cnt;
  dt_fizz_fx(node);
}

void cl_man_hits_monk(Sprite *node, Sprite *hit) {
  int cnt = GLOBALS_MAN_STRENGTH;
  cnt -= 200;
  if (cnt < 0) {
    cnt = 0;
  }
  GLOBALS_MAN_STRENGTH = cnt;
  killsprite(hit);
}

void cl_man_hits(Sprite *node, Sprite *hit) {
  int frame = hit->SPRITE_FRAME;
  int hitpnt = hit->SPRITE_HITPNT;
  int cnt;

  if ((frame >= FRM_MINE) && (frame <= (FRM_MINE + 1))) {
    // hit mine
    if ((node->SPRITE_Y + node->SPRITE_H) != (hit->SPRITE_Y + hit->SPRITE_H))
      return;
    cnt = GLOBALS_MAN_STRENGTH;
    cnt -= hitpnt;
    if (cnt < 0) {
      cnt = 0;
    }
    GLOBALS_MAN_STRENGTH = cnt;
    killsprite(hit);
    return;
  }
  killsprite(hit);
  if ((frame >= (FRM_WIZSHOT + 2)) && (frame <= (FRM_WIZSHOT + 3))) {
    // hit wizard shot
    cnt = GLOBALS_MAN_POWER;
    cnt -= hitpnt;
    if (cnt < 0) {
      cnt = 0;
    }
    GLOBALS_MAN_POWER = cnt;
    dt_fizz_fx(hit);
  } else {
    // hit something else
    cnt = GLOBALS_MAN_STRENGTH;
    cnt -= hitpnt;
    if (cnt < 0) {
      cnt = 0;
    }
    GLOBALS_MAN_STRENGTH = cnt;
    if (hit->SPRITE_DEATH != 0)
      return;
    sp_blood *sfx = new sp_blood();
    if (sfx != 0) {
      GLOBALS_FX_DLIST.addtail(sfx);
      sfx->SPRITE_X = (((hit->SPRITE_W >> 1) + hit->SPRITE_X) - 8);
      sfx->SPRITE_Y = (((hit->SPRITE_H >> 1) + hit->SPRITE_Y) - 8);
      int dir = hit->SPRITE_DIR;
      sfx->SPRITE_DIR = dir;
      sfx->mv.MV_XVEL = dir * 4;
    }
  }
}

void cl_man_hits_enemy(Sprite *node, Sprite *hit) {
  sp_enemy *enemy = (sp_enemy *)hit;
  int frame = node->SPRITE_FRAME;
  if ((frame >= FRM_CLIMB) && (frame <= FRM_CLIMB + 3))
    goto haveat;
  if ((node->SPRITE_Y + node->SPRITE_H) ==
      (enemy->SPRITE_Y + enemy->SPRITE_H)) {
  haveat:
    if (enemy->mv.MV_YACC == 0) {
      enemy->SPRITE_FLAGS |= FSP_COLLIDE;
      enemy->mv.MV_XVEL = 0;
    }
  }
}

void cl_man_hits_dragging_enemy(Sprite *node, Sprite *hit) {
  int type = hit->SPRITE_TYPE & (FTP_TOWER | FTP_CARPET | FTP_SKELETON_HORSE);
  int hitpnt = hit->SPRITE_HITPNT;
  int cnt;
  if (type != 0) {
    // power
    hitpnt = ((hitpnt >> 2) + 1);
    cnt = GLOBALS_MAN_POWER;
    cnt -= hitpnt;
    if (cnt < 0) {
      cnt = 0;
    }
    GLOBALS_MAN_POWER = cnt;
  } else {
    // strength
    hitpnt = ((hitpnt >> 1) + 1);
    cnt = GLOBALS_MAN_STRENGTH;
    cnt -= hitpnt;
    if (cnt < 0) {
      cnt = 0;
    }
    GLOBALS_MAN_STRENGTH = cnt;
  }
  int xv = ((sp_enemy *)hit)->mv.MV_XVEL;
  if (xv != 0) {
    // not frozen
    xv = xv / 2;
    int x = node->SPRITE_X + xv;
    if (x < 0) {
      x = 0;
    } else if (x > ((MAP_WIDTH * TILE_WIDTH) - 32)) {
      x = ((MAP_WIDTH * TILE_WIDTH) - 32);
    }
    node->SPRITE_X = x;
  }
}

// play sounds

void play_spotfx(Sprite *node, int sfx) {
  if (GLOBALS_GAME_MENU_SOUND == txt_menu_on) {
    int pan = (node->SPRITE_X + (node->SPRITE_W / 2)) + GLOBALS_SCROLL_X;
    pan -= (WINDOW_WIDTH / 2);
    pan *= PAN_RANGE;
    pan /= (WINDOW_WIDTH / 2);
    if (pan < -PAN_RANGE) {
      pan = -PAN_RANGE;
    } else if (pan > PAN_RANGE) {
      pan = PAN_RANGE;
    }
    Play(sfx, pan);
  }
}

void play_death_score(Sprite *node) {
  int type = node->SPRITE_TYPE;
  if (type != 0) {
    int *table = scoretable;
    play_func *stable = sfxtable;
    while ((type & 1) == 0) {
      type = type >> 1;
      table++;
      stable++;
    }
    GLOBALS_MAN_SCORE += *table;
    play_func sfx = *stable;
    if (sfx != 0) {
      (*sfx)(node);
    }

    // start panel drip if not allready
    if (GLOBALS_GAME_DRIP_Y == 0) {
      GLOBALS_GAME_DRIP_Y = WINDOW_HEIGHT + 32;
    }
  }
}

void play_out(Sprite *node) { play_spotfx(node, SOUND_OUT); }

void play_drip(Sprite *node) { play_spotfx(node, SOUND_DRIP); }

void play_horse(Sprite *node) { play_spotfx(node, SOUND_HORSE); }

void play_death(Sprite *node) {
  int sfx;
  unsigned int num = random(2);
  if (num == 0) {
    // death 1
    sfx = SOUND_DEATH1;
  } else {
    // death 2
    sfx = SOUND_DEATH2;
  }
  play_spotfx(node, sfx);
}

void play_throw(Sprite *node) { play_spotfx(node, SOUND_THROW); }

void play_explode(Sprite *node) { play_spotfx(node, SOUND_EXPLODE); }

void play_twang1(Sprite *node) { play_spotfx(node, SOUND_TWANG1); }

void play_twang2(Sprite *node) { play_spotfx(node, SOUND_TWANG2); }

void play_spell(Sprite *node) { play_spotfx(node, SOUND_SPELL); }

void play_roar(Sprite *node) { play_spotfx(node, SOUND_ROAR); }

void play_boar(Sprite *node) { play_spotfx(node, SOUND_BOAR); }

void play_hooves(Sprite *node) { play_spotfx(node, SOUND_HOOVES); }

void play_clash1(Sprite *node) { play_spotfx(node, SOUND_CLASH1); }

void play_clash2(Sprite *node) { play_spotfx(node, SOUND_CLASH2); }
