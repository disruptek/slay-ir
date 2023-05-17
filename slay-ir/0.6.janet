#
# slay/ir 0.6
#

(import spork/json :as json)

(varfn ir-encode
  `create something we can turn into valid json slay/ir`
  [value]
  (match (type value)
    :keyword
      (string value)   # we'll perform the :keyword -> "keyword" ourselves
    :number
      value
    :string
      value
    :boolean
      value
    :nil
      {:none 0}        # we'll perform the nil->:null ourselves
    :array
      (map ir-encode value)
    :tuple
      @{:tuple (map ir-encode value)}
    :table
      @{:dict (pairs (table ;(map ir-encode (kvs value))))}
    :struct
      @{:dict (pairs (table ;(map ir-encode (kvs value))))}
    other
      (error (string/format "unsupported input: %q" other))))

(varfn ir-decode [] nil)  # mutual recursion idiom

(defn- ir-decode-list
  `a place to debug/extend list decoding`
  [value]
  (case (type value)
    :array value
    :tuple value
    (array ;(ir-decode value))))

(defn- flatten1
  `flatten a list of lists into a single list whatfer table creation`
  [inputs]
  (match inputs
    [[key value] & tail]
    @[key value ;(flatten1 tail)]
    [] @[]))

(defn- ir-decode-ptr
  `currently a no-op`
  [value]
  (ir-decode value))

(defn- ir-decode-extension
  `decode a so-called slay "extension" type`
  [js]
  (match js
    {:none value}
      :null
    {:ptr value}
      (ir-decode-ptr (ir-decode value))
    {:str value}
      (string (ir-decode value))
    {:dict [[key value] & rest]}
      (table (ir-decode key) (ir-decode value) ;(flatten1 rest))
    {:list value}
      (array ;(ir-decode value))
    {:tuple value}
      (tuple ;(ir-decode value))
    {:set value}
      (tuple ;(ir-decode value))
    other
      (do
        (print (string/format "unsupported extension: %q" other))
        other)))

(varfn ir-decode
  `decode any json input into native values`
  [js]
  (match (type js)
    :keyword                        # we only support the :null keyword
    (if (= js :null) nil (error (string "unsupported keyword: " js)))
    :number
    js
    :string
    js
    :boolean
    js
    :nil
    nil
    :array                          # arrays are native
    (map ir-decode js)
    :tuple                          # unlikely, but we'll support it
    (tuple ;(map ir-decode js))
    :table                          # slay "extensions"
    (ir-decode-extension js)
    :struct                         # unlikely, but we'll support it
    (ir-decode-extension js)
    other
    (error (string/format "unsupported input: %q" other))))

(defn encode-json
  `encode a value into slay/ir json`
  [value]
  (json/encode (ir-encode value)))

(defn decode-json
  `decode a value from slay/ir json`
  [js]
  (ir-decode (json/decode js true false)))
