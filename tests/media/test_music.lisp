(import "apps/media/jukebox/music.inc")

(report-header "Media: Music, the Jukebox's player for an app with no window")

;the music has three mailboxes of a select list from its slot, here 1 to 3
(defq tm_select (task-mboxes 4) tm_music (Music tm_select 1)
	tm_a (cat "tests/scratch/music_a_" (str (pii-pid)) ".txt")
	tm_b (cat "tests/scratch/music_b_" (str (pii-pid)) ".txt"))

(assert-eq "no songs, no music" :nil (. tm_music :start (list)))
(assert-eq "and nothing playing" :nil (. tm_music :title))
(assert-eq "mail not to its mailboxes is the app's" :nil (. tm_music :mail 0 ""))
(assert-eq "nor after them" :nil (. tm_music :mail 4 ""))

;two songs that are files but not modules, and one that is not there
(save "not a song" tm_a)
(save "nor this" tm_b)
(assert-eq "songs to play, the music on" :t
	(. tm_music :start (list (list "gone" "tests/scratch/music_none.txt") (list "a" tm_a) (list "b" tm_b))))
(assert-eq "one that can not be read passed over" "a" (. tm_music :title))

;the player says each is not a module, and the next is played; when none
;of them is the music goes off. Its own mailboxes are all it is handed
(defq tm_titles (list "a"))
(while (. tm_music :title)
	(defq tm_idx (mail-select tm_select) tm_msg (mail-read (elem-get tm_select tm_idx)))
	(assert-true "its mail is its own" (. tm_music :mail tm_idx tm_msg))
	(and (defq tm_title (. tm_music :title)) (nql tm_title (last tm_titles))
		(push tm_titles tm_title)))
(assert-eq "on to the next, and off when none is a module" "a b" (join tm_titles " "))

;a song that is a module, a MOD of one silent pattern made here, played
;quiet: the player says nothing of it, and is still there after it, told
;of the next, which is not a module, and says so
(defun tm-zeros (n) (pad "" n "\x00"))
(defq tm_m (cat "tests/scratch/music_m_" (str (pii-pid)) ".mod"))
(save (cat "test" (tm-zeros 16) (tm-zeros 22) "\x00\x10\x00\x40\x00\x00\x00\x01"
	(tm-zeros (* 30 30)) "\x01\x7f" (tm-zeros 128) "M.K." (tm-zeros 1024) (pad "" 32 "\x40")) tm_m)
(assert-eq "a module to play, the music on" :t
	(. tm_music :start (list (list "m" tm_m) (list "a" tm_a))))
(assert-eq "it plays" "m" (. tm_music :title))
(. tm_music :move 1)
(assert-eq "the next" "a" (. tm_music :title))
(until (= (defq tm_idx (mail-select tm_select)) 2)
	(. tm_music :mail tm_idx (mail-read (elem-get tm_select tm_idx))))
(. tm_music :mail tm_idx (mail-read (elem-get tm_select tm_idx)))
(assert-eq "the player there after a quiet song says the next is not a module, and the music is on to the one after" "m" (. tm_music :title))

(pii-remove tm_a)
(pii-remove tm_b)
(pii-remove tm_m)
(. tm_music :stop)
(undef (env) 'tm_select 'tm_music 'tm_a 'tm_b 'tm_m 'tm_titles 'tm_idx 'tm_msg 'tm_title)
