#lang br/demo/jsonic-b
// a line comment
[
  @$ 'null $@,
  @$ #f $@,
  @$ (* 6 7) $@,
  @$ "string" $@,
  @$ (list "array" "of" "strings") $@,
  @$ (hash 'key-1 42
          'key-2 "value"
          'key-3 (hash 'subkey 21)) $@
]