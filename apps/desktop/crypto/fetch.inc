;the prices, had from the first of the sites that answers, so one that is
;down, or turns the app away, is gone round: CoinGecko over https, by
;lib/net/tls.inc, then Coinranking over http. Each gives its coins in its
;own way, and they come out the same, a list a coin:
;(symbol name price change rank sparkline), the numbers fixed

(import "service/net/app.inc")
(import "lib/net/http.inc")
(import "lib/net/json.inc")

(defun crypto-q (val)
	; (crypto-q val) -> fixed | :nil
	;a number as a site gives it, a str, a whole number or a fixed
	(cond
		((str? val) (if (nempty? val) (n2q (str-to-num val))))
		((num? val) (n2q val))))

(defun crypto-spark (pts)
	; (crypto-spark pts) -> (fixed ...)
	;the points of a trend, those that are numbers
	(reduce (lambda (out pt) (if (defq val (crypto-q pt)) (push out val) out)) pts (list)))

(defun crypto-coin (sym name price change rank spark)
	; (crypto-coin sym name price change rank spark) -> coin
	(list (to-upper (ifn sym "")) (ifn name "") (ifn (crypto-q price) 0.0)
		(ifn (crypto-q change) 0.0) (ifn rank 0) spark))

(defun crypto-gecko (body)
	; (crypto-gecko body) -> (coin ...) | :nil
	;a list of coins, the trend a week of hours, of which the last day is kept;
	;turned away it is an object
	(when (starts-with "[" body)
		(map (lambda (c)
				(defq spark (crypto-spark (pfind (pfind c :sparkline_in_7d) :price)))
				(crypto-coin (pfind c :symbol) (pfind c :name) (pfind c :current_price)
					(pfind c :price_change_percentage_24h) (pfind c :market_cap_rank)
					(slice spark (max 0 (- (length spark) 25)) -1)))
			(json-parse body))))

(defun crypto-ranking (body)
	; (crypto-ranking body) -> (coin ...) | :nil
	;an object that says if it is a success, the numbers strs, the trend a day
	(and (starts-with "{" body)
		(eql (pfind (defq json (json-parse body)) :status) "success")
		(map (lambda (c)
				(crypto-coin (pfind c :symbol) (pfind c :name) (pfind c :price)
					(pfind c :change) (pfind c :rank) (crypto-spark (pfind c :sparkline))))
			(pfind (pfind json :data) :coins))))

(defq *crypto_sites* (list
	(list "CoinGecko" "https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd&per_page=6&page=1&sparkline=true" crypto-gecko)
	(list "Coinranking" "http://api.coinranking.com/v2/coins?limit=6" crypto-ranking)))

(defun crypto-site (url parse)
	; (crypto-site url parse) -> (coin ...) | :nil
	;what a site says is from outside: one that does not parse is a site that
	;did not answer
	(defq coins :nil)
	(catch (when (defq resp (http-get url))
			(setq coins (parse (trim (http-body-str resp)))))
		:t)
	(if (nempty? coins) coins))

(defun crypto-fetch (&optional sites)
	; (crypto-fetch [sites]) -> (site (coin ...)) | :nil
	;the first site of sites that answers, and its coins
	(ensure-net-service)
	(some (lambda ((site url parse)) (if (defq coins (crypto-site url parse)) (list site coins)))
		(ifn sites *crypto_sites*)))
