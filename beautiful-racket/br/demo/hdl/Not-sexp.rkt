#lang s-exp br/demo/hdl/expander

#|
CHIP Not {
          IN in;
          OUT out;
             
          PARTS:
          Nand(a=in, b=in, out=out);
}
|#

(chip-program Not
              (in-spec (in 8) (a))
              (out-spec (out 8))
              (part-spec (part Nand ((a) (in)) ((b) (in)) ((out) (out)))))