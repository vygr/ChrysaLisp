(import "usr/env.inc")
(import "service/net/app.inc")
(import "lib/net/http.inc")
(import "lib/net/json.inc")
(import "././utils.inc")

(report-header "Crypto Ticker Data & Config Tests")

; Test 1: Config roundtrip
(defq
	*config* :nil
	*config_version* 1
	*config_file* (cat *env_home* "crypto_test.tre")
	*selected_symbol* "BTC")

(defun config-default ()
	(scatter (Emap)
		:version *config_version*
		:selected_symbol "BTC"))

(defun config-save ()
	(if (not *config*)
		(setq *config* (Emap)))
	(scatter *config*
		:version *config_version*
		:selected_symbol *selected_symbol*)
	(when (defq stream (file-stream *config_file* +file_open_write))
		(tree-save stream *config*)))

(defun config-load ()
	(defq old_config :nil)
	(if (defq stream (file-stream *config_file*))
		(setq old_config (tree-load stream)))
	(if (or (not old_config) (/= (. old_config :find :version) *config_version*))
		(setq *config* (config-default))
		(setq *config* old_config))
	(setq *selected_symbol* (. *config* :find :selected_symbol))
	(if (not (str? *selected_symbol*))
		(setq *selected_symbol* "BTC")))

(setq *selected_symbol* "ETH")
(config-save)
(setq *selected_symbol* :nil *config* :nil)
(config-load)

(assert-eq "config roundtrip selected_symbol" "ETH" *selected_symbol*)

; Test 2: Live HTTP fetch and parsing from Coinranking
(ensure-net-service)
(defq resp (http-get "http://api.coinranking.com/v2/coins?limit=5"))
(assert-true "http response received" (if resp :t :nil))

(when resp
	(defq body (http-body-str resp))
	(assert-true "http body non-empty" (and body (> (length body) 0)))
	(assert-true "http body starts with {" (starts-with "{" (trim body)))
	(defq json (json-parse body))
	(assert-true "json parse successful" (if json :t :nil))
	(when json
		(defq coins (pfind (pfind json :data) :coins))
		(assert-true "coins list non-empty" (and (list?? coins) (> (length coins) 0)))
		(defq btc (first coins))
		(assert-true "first coin has symbol" (str? (pfind btc :symbol)))
		(assert-true "first coin has price" (str? (pfind btc :price)))
		(assert-true "first coin has change" (str? (pfind btc :change)))
		(defq spark (pfind btc :sparkline))
		(assert-true "sparkline points present" (and (list?? spark) (>= (length spark) 10)))))

(print-summary)

(stream-flush (io-stream "stdout"))
(task-sleep 500000)
(pii-exit)
