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
				(push out (list (entry-title (last parts))
					(cat +modland (join (slice parts (inc at) -1) "/")))))
			;on to the next line
			:nil)
		(string-stream text))
	out)
