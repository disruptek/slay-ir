#
# slay/ir 0.6
#
(import spork/json)
(import spork/base64)
(import slay-ir/uri-encoding :as url)

(varfn encode
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
      (map encode value)
    :tuple
      @{:tuple (map encode value)}
    :table
      @{:dict (pairs (table ;(map encode (kvs value))))}
    :struct
      @{:dict (pairs (table ;(map encode (kvs value))))}
    other
      (error (string/format "unsupported input: %q" other))))

(varfn decode [] nil)  # mutual recursion idiom

(defn- decode-list
  `a place to debug/extend list decoding`
  [value]
  (case (type value)
    :array value
    :tuple value
    (array ;(decode value))))

(defn- flatten1
  `flatten a list of lists into a single list whatfer table creation`
  [inputs]
  (match inputs
    [[key value] & tail]
    @[key value ;(flatten1 tail)]
    [] @[]))

(defn- decode-ptr
  `decode a crude pointer`
  [pointer]
  (let [[scheme path] (string/split ":" pointer 0 2)]
    (case scheme
      "str"      (url/decode path)
      "int"      (parse path)
      "float"    (parse path)
      "bool"     (parse (string/ascii-lower path))
      "none"     :null
      "json"     (json/decode (url/decode path) true false)
      "base64"   (base64/decode (url/decode path))
      (error (string/format "unsupported pointer: %q" pointer)))))

(defn- decode-extension
  `decode a so-called slay "extension" type`
  [js]
  (match js
    {:none value}
      :null
    {:ptr value}
      (decode-ptr (decode value))
    {:str value}
      (case (type value)
        :string value
        (string (decode value)))
    {:dict value}
      (table ;(map decode (flatten1 value)))
    {:list value}
      (array ;(decode value))
    {:tuple value}
      (tuple ;(decode value))
    {:set value}
      (tuple ;(decode value))
    other
      (do
        (print (string/format "unsupported extension: %q" other))
        other)))

(varfn decode
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
    (map decode js)
    :tuple                          # unlikely, but we'll support it
    (tuple ;(map decode js))
    :table                          # slay "extensions"
    (decode-extension js)
    :struct                         # unlikely, but we'll support it
    (decode-extension js)
    other
    (error (string/format "unsupported input: %q" other))))
