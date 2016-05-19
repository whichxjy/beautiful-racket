#lang br/demo/hdl

// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/01/Mux.hdl

/** 
 * Multiplexor:
 * out = a if sel == 0
 *       b otherwise
 */

CHIP Mux {
    IN a, b, sel;
    OUT out;

    PARTS:
    Not(in=sel, out=sel-opposite);
    Not(in=a, out=not-a);
    Or(a=not-a, b=sel-opposite, out=maybe-a);
    Not(in=b, out=not-b);
    Or(a=not-b, b=sel, out=maybe-b);
    Or(a=maybe-a, b=b, out=out);
}
