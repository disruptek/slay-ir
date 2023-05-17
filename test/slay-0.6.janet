(use spork/test)
(use slay-ir/0.6)

(defn round-trip
  [value]
  (let [encoded (ir-encode value)
        decoded (ir-decode encoded)]
    (unless (deep= decoded value)
      (error (string/format "round-trip failed: %q -> %q -> %q"
                            value encoded decoded)))))

(round-trip 5)
(assert (deep= (ir-encode nil) {:none 0}))
(assert (deep= (ir-decode {:none 0}) :null))
(round-trip true)
(round-trip false)
(round-trip "str")
(round-trip ["a" "b" "c"])
(assert (deep= (ir-encode @["a" nil]) @["a" {:none 0}]))
(assert (deep= (ir-decode @["a" {:none 0}]) @["a" :null]))
(assert (deep= (ir-encode ["a" nil]) @{:tuple @["a" {:none 0}]}))
(assert (deep= (ir-decode @{:tuple @["a" {:none 0}]}) ["a" :null]))
(round-trip @[1 "a" true])
(round-trip @{1 "a"})
(assert (deep= (ir-encode ["a" nil]) @{:tuple @["a" {:none 0}]}))
(assert (deep= (ir-decode @{:tuple @["a" {:none 0}]}) ["a" :null]))
(assert (deep= (ir-encode {"a" @[true nil]}) @{:dict @[["a" @[true {:none 0}]]]}))
(assert (deep= (ir-decode {:dict [["a" [true {:none 0}]]]}) @{"a" [true :null]}))
