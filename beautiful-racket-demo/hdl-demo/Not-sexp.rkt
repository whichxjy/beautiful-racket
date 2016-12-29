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
              (in-spec (in))
              (out-spec (out))
              (part Nand (a in) (b in) (out out)))

