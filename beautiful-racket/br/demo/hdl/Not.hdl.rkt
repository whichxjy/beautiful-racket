#lang br/demo/hdl

CHIP Not {
          IN in[8];
          OUT out;
             
          PARTS:
          Nand(a[2..4]=in, b=011, c=true, out[3]=v);
}



