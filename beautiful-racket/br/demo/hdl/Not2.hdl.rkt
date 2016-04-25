#lang br/demo/hdl

CHIP Not {
          IN a, b, c, d;
             OUT x, y, z;

             PARTS:
             Nand(a=a, b=a, out=x);
             Nand(a=a, b=a, out=y);
             Nand(a=a, b=a, out=z);
             }



