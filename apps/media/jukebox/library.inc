;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; where the modules come from: the user's favourites, kept in the tree's
; downloads/mods/ to play offline, and Modland's archive, a folder of each
; tracker, an artist's folder in each, read from the listing it gives. And
; its charts: the favourites of those who keep it, folders as of its
; modules, and its playlists, of compos, music disks and mags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/net/url.inc")
(import "lib/text/searching.inc")

(defq +modland "https://modland.com/pub/modules/" +modland_pub "https://modland.com/pub/"
	;the folders of the trackers whose modules play here
	+modland_kinds ''("Protracker" "Noisetracker" "Soundtracker" "Startrekker AM" "Fasttracker"
		"Screamtracker 3" "Fasttracker 2" "Impulsetracker"
		"OctaMED MMD0" "OctaMED MMD1" "OctaMED MMD2" "OctaMED MMD3" "OctaMED MMDC" "AHX")
	;the folders of the charts, of pub/
	+chart_places ''("favourites" "playlists"))

(defun kept-path (url)
	; (kept-path url) -> path
	;where a module or a listing fetched is kept, of the tree's downloads/,
	;by its address: the host and its folders, each a plain name, so none
	;climbs out. A folder's listing is its .listing
	(defq parts (filter (# (and (nempty? %0) (nql %0 ".") (nql %0 "..")))
			(split (slice url (length "https://") -1) "/")))
	(apply (const cat) (cat (list "downloads/mods")
		(map (lambda (part)
				(cat "/" (apply (const cat) (map (# (if (bfind %0 (const (char-class "A-Za-z0-9._-"))) %0 "_"))
					(url-decode part)))))
			parts)
		(if (ends-with "/" url) (list "/.listing") (list)))))

(defun listing-read (html)
	; (listing-read html) -> ((name folder?) ...)
	;the entries of a folder's listing, by the names of its links, a line
	;each, what comes after href= between its quotes: those that end / are
	;folders, none of the sort links, the way up or other places
	(defq out (list))
	(lines! (lambda (line)
			(defq parts (split line "\q"))
			(each! (lambda (part)
					(when (ends-with "href=" part)
						(defq name (elem-get parts (inc (!))))
						(unless (or (eql name "") (starts-with "?" name) (starts-with "/" name)
								(starts-with "." name) (starts-with "http" name))
							(push out (list name (ends-with "/" name))))))
				(list parts) 0 (dec (length parts))))
		(string-stream html))
	out)

(defun entry-title (name)
	; (entry-title name) -> str
	;a name as it is shown, its escapes undone and a folder's / gone
	(url-decode (if (ends-with "/" name) (slice name 0 -2) name)))

(defun playlist-read (text)
	; (playlist-read text) -> ((title url) ...)
	;the modules of an m3u playlist that play here, a line each, the
	;address of one of Modland's at a mirror of it: what comes after its
	;pub/modules/, of a tracker played here, at Modland itself
	(defq out (list))
	(lines! (lambda (line)
			(defq parts (split line (const (char-class "/\r"))))
			(and (defq at (some (# (and (eql %0 "modules") (> (!) 0)
						(eql (elem-get parts (dec (!))) "pub") (!))) parts))
				(< (+ at 2) (length parts))
				(find (url-decode (elem-get parts (inc at))) +modland_kinds)
				;each part escaped again: a playlist escapes its spaces
				;and not a #, which would end the address there
				(push out (list (entry-title (last parts))
					(cat +modland (join (map! (# (url-encode (url-decode %0))) (list parts) (inc at) (length parts)) "/")))))
			;on to the next line
			:nil)
		(string-stream text))
	out)

(defun name-stem (name)
	; (name-stem name) -> str
	;a module's name as two are compared: its escapes undone, its ending
	;and the dots before it gone, in lower case
	(setq name (url-decode name))
	(to-lower (trim-end (if (defq at (rfind "." name)) (slice name 0 (dec at)) name) ".")))

(defun name-ending (name)
	; (name-ending name) -> str
	;a module's ending, in lower case, "" if none
	(to-lower (if (defq at (rfind "." name)) (slice name at -1) "")))

(defun stem-in-order? (short long from)
	; (stem-in-order? short long from) -> :t | :nil
	;are the letters of short in long, in order, from that place of it
	(defq i 0 n (length short))
	(each! (# (and (< i n) (eql %0 (elem-get short i)) (setq i (inc i)))) (list long) from (length long))
	(= i n))

(defun stem-within? (short long)
	; (stem-within? short long) -> :t | :nil
	;are the letters of short in long, in order, from the start of one of
	;its words: a DOS name of 8 made from a longer, millpwr of millenium
	;powerup, nostal of sid nostalgica
	(and (nempty? short)
		(some! (lambda (c)
				(and (eql c (first short))
					(or (= (!) 0) (found? " _-" (elem-get long (dec (!)))))
					(stem-in-order? short long (!))))
			(list long) :nil 0 (length long))))

(defun name-plain (name)
	; (name-plain name) -> str
	;a module's name as its letters and digits only, as two are compared
	;that differ by their marks, everythings good of everything's good
	(apply (const cat) (map (# (if (bfind %0 (const (char-class "a-z0-9"))) %0 "")) (name-stem name))))

(defun word-after? (short long)
	; (word-after? short long) -> :t | :nil
	;is long short and a word or more after it, polish xmas v2 of polish
	;xmas
	(and (> (length long) (length short)) (starts-with short long)
		(not (bfind (elem-get long (length short)) (const (char-class "a-z0-9"))))))

(defun plain-shortened? (long short)
	; (plain-shortened? long short) -> :t | :nil
	;is short long with some of its letters and digits taken out, three
	;quarters or more left, the first kept: rom1theme of romtheme
	(and (< 0 (length short) (length long)) (>= (* 4 (length short)) (* 3 (length long)))
		(eql (first short) (first long)) (stem-in-order? short long 0)))

(defun only-one (entries fn)
	; (only-one entries fn) -> :nil | name
	;the name of the only module of a listing's entries fn says :t of
	(and (= (length (defq like (filter fn entries))) 1) (first (first like))))

(defun entry-renamed (html name)
	; (entry-renamed html name) -> :nil | name
	;the module of a folder's listing that one not there now is under,
	;(entry-renamed-names)
	(entry-renamed-names (map (const first) (filter (lambda ((& folder)) (not folder)) (listing-read html))) name))

(defun entry-renamed-names (names name)
	; (entry-renamed-names names name) -> :nil | name
	;the module of a folder's names that one not there now is under.
	;Modland has renamed some that playlists still name: the same name but
	;for its ending, supernova.ft is supernova.mod now; the same letters
	;and digits, of marks added or taken, the only one there it could be,
	;bubble toast win.mod is bubble toast (win).mod; the same name and a
	;word or more after it, of the same ending, the only one there it could
	;be, polish xmas.mod is polish xmas v2.mod; the same letters and digits
	;but some taken out, three quarters or more of them left, of the same
	;ending, the only one there it could be, rom-1-theme.mod is
	;rom-theme.mod, but never one with letters of its own, brainfood-#2 is
	;not brainfood-#3; or a DOS name of 8 or fewer
	;made from the name it has now, of the same ending, the only one there
	;it could be, millpwr.xm is millenium powerup.xm and nostal.xm sid
	;nostalgica.xm
	(defq stem (name-stem name) plain (name-plain name) ending (name-ending (url-decode name))
		modules (map (# (list %0)) names))
	(or (some (lambda ((entry &ignore)) (if (eql (name-stem entry) stem) entry)) modules)
		(and (nempty? plain) (only-one modules (lambda ((entry &ignore)) (eql (name-plain entry) plain))))
		(and (nempty? stem) (only-one modules (lambda ((entry &ignore))
			(and (eql (name-ending (url-decode entry)) ending) (word-after? stem (name-stem entry))))))
		(and (nempty? plain) (only-one modules (lambda ((entry &ignore))
			(and (eql (name-ending (url-decode entry)) ending) (plain-shortened? plain (name-plain entry))))))
		(and (<= (length stem) 8)
			(only-one modules (lambda ((entry &ignore))
				(and (eql (name-ending (url-decode entry)) ending) (stem-within? stem (name-stem entry))))))))

(defun coop-parent (url)
	; (coop-parent url) -> :nil | url
	;the folder a folder of work with another artist, coop-, is in, the
	;artist's own: Modland has moved some up out of one, two steps.it of
	;Siren/coop-Sirrus/ is in Siren/
	(defq at (rfind "/" (slice url 0 -2)))
	(and at (starts-with "coop-" (slice url at -2)) (slice url 0 at)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Modland's list of all it has, allmods.zip, where a module is now, of
; those a playlist names where they were: an artist's folder renamed,
; work moved into a folder with another, coop-, ./index.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +allmods "https://modland.com/allmods.zip")

(defun allmods-played (text)
	; (allmods-played text) -> str
	;the paths of allmods.txt, a size and a path a line, of the trackers
	;played here, a path a line
	(defq out (string-stream (str-alloc 1048576))
		kinds (map (# (cat %0 "/")) +modland_kinds))
	(lines! (lambda (line)
			(and (defq tab (find "\t" line)) (defq path (slice line (inc tab) -1))
				(some (# (starts-with %0 path)) kinds)
				(write-line out path))
			:nil)
		(string-stream text))
	(str out))

(defun allmods-index (played)
	; (allmods-index played) -> (by_name by_folder)
	;the paths of (allmods-played) by their file's name, and by their
	;folder, each in lower case
	(defq by_name (Fmap 16381) by_folder (Fmap 4093))
	(lines! (lambda (path)
			(defq at (ifn (rfind "/" path) 0)
				key (to-lower (slice path at -1)) folder (to-lower (slice path 0 (max 0 (dec at)))))
			(if (defq paths (. by_name :find key)) (push paths path) (. by_name :insert key (list path)))
			(if (defq paths (. by_folder :find folder)) (push paths path) (. by_folder :insert folder (list path)))
			:nil)
		(string-stream played))
	(list by_name by_folder))

(defun folder-renamed (index folder name)
	; (folder-renamed index folder name) -> :nil | path
	;the module of a folder of the index that one of that name not there
	;now is under, by the rules of a name renamed, (entry-renamed-names)
	(and (defq paths (. (second index) :find (to-lower folder)))
		(defq now (entry-renamed-names (map (const path-name) paths) name))
		(some (# (if (eql (path-name %0) now) %0)) paths)))

(defun folder-coop-up (folder)
	; (folder-coop-up folder) -> :nil | folder
	;the folder a folder of work with another artist, coop-, is in
	(and (defq at (rfind "/" folder)) (starts-with "coop-" (slice folder at -1)) (slice folder 0 (dec at))))

(defun allmods-resolve (index path)
	; (allmods-resolve index path) -> :nil | path
	;where a module a playlist names is now, :nil if Modland has it no
	;more: (allmods-find), else by the rules of a name renamed in its
	;folder, or in the artist's own above a folder of work with another
	(defq at (ifn (rfind "/" path) 0) folder (slice path 0 (max 0 (dec at))) name (slice path at -1))
	(or (allmods-find index path)
		(folder-renamed index folder name)
		(and (defq up (folder-coop-up folder)) (folder-renamed index up name))))

(defun path-words (path)
	; (path-words path) -> (word ...)
	;the words of a path's folders, in lower case, letters and digits
	(filter (# (nempty? %0))
		(split (to-lower (slice path 0 (ifn (rfind "/" path) 0))) (const (char-class " -/:-@[-`{-~")))))

(defun allmods-find (index path)
	; (allmods-find index path) -> :nil | path
	;where a module of that path is now, a path of pub/modules/ unescaped:
	;the same path but for its case; else the module of that name in the
	;same tracker's folder, or any played here if there is none there; of
	;several, the one whose folders share most words with its own, if one
	;shares most, Jam & Spoon's funky dog.mod is Spoon/coop-Jam's
	(defq low (to-lower path) tracker (cat (first (split low "/")) "/")
		all (ifn (. (first index) :find (to-lower (path-name path))) (list)))
	(cond
		((some (# (if (eql (to-lower %0) low) %0)) all))
		((empty? all) :nil)
		(:t (defq near (filter (# (starts-with tracker (to-lower %0))) all))
			(if (empty? near) (setq near all))
			(if (= (length near) 1) (first near)
				;each word of its folders a whole word query, ignoring case
				(defq words (map (# (slice (query %0 :t :nil :t) 0 2)) (path-words path))
					scores (map (lambda (p)
						(defq folders (to-lower (slice p 0 (ifn (rfind "/" p) 0))))
						(length (filter (lambda ((engine meta)) (. engine :match? folders meta)) words))) near)
					best (reduce (const max) scores 0))
				(and (> best 0) (= (length (filter (# (= %0 best)) scores)) 1)
					(elem-get near (find best scores)))))))

(defun modland-url (path)
	; (modland-url path) -> url
	;the address of a path of Modland's pub/modules/, each part escaped
	(cat +modland (join (map (const url-encode) (split path "/")) "/")))


(defun modland-path (url)
	; (modland-path url) -> path
	;the path of pub/modules/ of an address of Modland's, unescaped
	(join (map (const url-decode) (split (slice url (length +modland) -1) "/")) "/"))

