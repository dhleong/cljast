# cljast

A Clojure library/app for parsing Java code, providing errors, suggestions,
etc., intended for integration with Vim or other editors. Uses the Eclipse JDT
for parsing and binding resolution.

## Usage

While the primary intended use will be as a local webserver that can interact
with Vim for smart code completion, etc., you can use the methods
in `core` quite easily, as well:

```clojure
(def ast (read-ast "path/to/Foo.java"))
(get-errors ast)
;=>
    ({:text "Syntax error on token \".\", ; expected", 
      :pos 133, 
      :line 8, 
      :col 15, 
      :length 1})

(identify ast 25 19) ; line, col
;=> 
    {:what type-method
     :type "net.dhleong.test.Magic"
     :returns "net.dhleong.test.Magic"
     :name "newInstance"
     :args ["java.lang.String"]
     :javadoc "Create Magic\n@return a new piece of Magic"
     }
```

See `core_test.clj` for more examples

## License

Copyright © 2014 Daniel Leong

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
