;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; where the modules come from: the user's favourites, kept in the tree's
; downloads/mods/ to play offline, and Modland's archive, a folder of each
; tracker, an artist's folder in each, read from the listing it gives. And
; its charts: the favourites of those who keep it, folders as of its
; modules, and its playlists, of compos, music disks and mags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/net/url.inc")

(defq +modland "https://modland.com/pub/modules/" +modland_pub "https://modland.com/pub/"
	;the folders of the trackers whose modules play here
	+modland_kinds ''("Protracker" "Noisetracker" "Soundtracker" "Startrekker AM" "Fasttracker"
		"Screamtracker 3" "Fasttracker 2" "Impulsetracker")
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

(defun stem-within? (short long)
	; (stem-within? short long) -> :t | :nil
	;are the letters of short in long, in order, the first of each the
	;same: a DOS name of 8 made from a longer, millpwr of millenium powerup
	(defq i 0 n (length short))
	(each (# (and (< i n) (eql %0 (elem-get short i)) (setq i (inc i)))) long)
	(and (> n 0) (= i n) (eql (first short) (first long))))

(defun entry-renamed (html name)
	; (entry-renamed html name) -> :nil | name
	;the module of a folder's listing that one not there now is under.
	;Modland has renamed some that playlists still name: the same name but
	;for its ending, supernova.ft is supernova.mod now; or a DOS name of 8
	;or fewer made from the name it has now, of the same ending, the only
	;one there it could be, millpwr.xm is millenium powerup.xm
	(defq stem (name-stem name) ending (name-ending (url-decode name))
		modules (filter (lambda ((& folder)) (not folder)) (listing-read html)))
	(or (some (lambda ((entry &ignore)) (if (eql (name-stem entry) stem) entry)) modules)
		(and (<= (length stem) 8)
			(= (length (defq like (filter (lambda ((entry &ignore))
					(and (eql (name-ending (url-decode entry)) ending) (stem-within? stem (name-stem entry))))
				modules))) 1)
			(first (first like)))))
