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

(pii-remove tm_a)
(pii-remove tm_b)
(. tm_music :stop)
(undef (env) 'tm_select 'tm_music 'tm_a 'tm_b 'tm_titles 'tm_idx 'tm_msg 'tm_title)
