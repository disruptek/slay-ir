(defn- decode-hex [arg]
  (let [result (string/from-bytes (scan-number arg 16))]
    #(print (string/format "%%%s -> %s" arg result))
    result))

(defn- encode-hex [arg]
  (let [result (string/format "%%%02x" (first (string/bytes arg)))]
    #(print (string/format "%s -> %s" arg result))
    result))

(def- encoding-grammar
  `according to rfc3986, always unescapes +`
  ~{:unreserved (<- (+ :w "-" "." "_" "~"))
    :reserved (<- (+ :gen-delims :sub-delims))
    :gen-delims (+ ":" "/" "?" "#" "[" "]" "@")
    :sub-delims (+ "!" "$" "&" "'" "(" ")" "*" "+" "," ";" "=")
    :do-escape (/ (<- 1) ,encode-hex)
    :un-escape (+ (/ (<- "+") " ") (* "%" (/ (<- (2 :h)) ,decode-hex)))
    :unescaped (+ :unreserved :safe :un-escape :reserved)
    :escaped (+ :unreserved :safe :do-escape)})

(defn decode-grammar
  `create a decode grammar with a set of safe characters`
  [&named safe]
  (peg/compile
    (merge encoding-grammar
           ~{:safe (<- (set ,safe))
             :main (any :unescaped)})))

(defn encode-grammar
  `create a encode grammar with a set of safe characters`
  [&named safe]
  (peg/compile
    (merge encoding-grammar
           ~{:safe (<- (set ,safe))
             :main (any :escaped)})))

(def default-decode-grammar
  `default grammar for decode; considers "/" safe`
  (decode-grammar :safe "/"))

(def default-encode-grammar
  `default grammar for encode; considers "/" safe`
  (encode-grammar :safe "/"))

(defn decode
  `decode a string; optional grammar defaults to default-decode-grammar`
  [str &opt grammar]
  (default grammar default-decode-grammar)
  (string ;(peg/match grammar (string str))))

(defn encode
  `encode a string; optional grammar defaults to default-encode-grammar`
  [str &opt grammar]
  (default grammar default-encode-grammar)
  (string ;(peg/match grammar (string str))))
