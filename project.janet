(declare-project
  :name "slay-ir"
  :description ```ir for slay```
  :license "GPL-3.0-or-later"
  :url "https://github.com/disruptek/slay-ir"
  :repo "git+https://github.com/disruptek/slay-ir.git"
  :dependencies
    ["https://github.com/janet-lang/spork"]
  :version "0.0.0")

(declare-source
  :prefix "slay-ir"
  :source ["slay-ir/init.janet"
           "slay-ir/0.4.janet"
           "slay-ir/0.6.janet"])
