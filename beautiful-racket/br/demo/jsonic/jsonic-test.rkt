#lang br/demo/jsonic

{"number": @$(* 6 7)$@,
         "string": @$(string-append "foo" "bar")$@,
         "array": @$(range 5)$@,
         "object": @$(hash "k1" "valstring" "k2" 42)$@
         //   "bar" :
         }