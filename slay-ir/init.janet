(import spork/json)

(defn encode-json
  `encode a value into slay/ir json`
  [value]
  (let [ir-encode (first (get (dyn 'slay-ir/encode) :ref))]
    (json/encode (ir-encode value))))

(defn decode-json
  `decode a value from slay/ir json`
  [js]
  (let [ir-decode (first (get (dyn 'slay-ir/decode) :ref))]
    (ir-decode (json/decode js true false))))
