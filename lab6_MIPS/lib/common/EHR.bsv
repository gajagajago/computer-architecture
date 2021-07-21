/******************************************************************************
 * Copyright (c) 2009-2011 Nirav Dave
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory under DARPA/AFRL contract (FA8750-10-C-0237)
 * ("CTSRD"), as part of the DARPA CRASH research programme.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 ******************************************************************************
 *
 * Authors: 
 *   Nirav Dave <ndave@ndave.org / ndave@csl.sri.com>
 * 
 ******************************************************************************
 *
 * Description: Parameterized Ephemeral History Register Implementation
 * 
******************************************************************************/

//XXX timing errors abound. Some work is needed

import Vector::*;
import RWire::*;

typedef  Vector#(n_sz, Reg#(alpha)) EHR#(type n_sz, type alpha);

// Adds one layer of EHRness onto a preexisting register (modulo the
// actual write). If this is the last emphemeral register interface this
// effectively adds another layer.
//
// the write are not done in this so that we can explicitly use the last
// read for writing explicitly saving the value. Otherwise, the structure
// differs when nothing is written and we get a separate read and write mux
// chain

// XXX this method scheduling pragma does not appear to work, but should make
// i0_.read SB i0_.write
//
// Without this we do not get read_n < write_n which would gives us the
// correct total ordering for the EHR
//(* method_scheduling = "fst_read SB fst_write"*)

module mkVirtualReg#(Reg#(alpha) state, Reg#(alpha) base)(Tuple2#(Reg#(alpha), Reg#(alpha))) provisos(Bits#(alpha,asz));
  RWire#(alpha) w  <- mkRWire();
   
  Reg#(alpha) i0 = interface Reg
                      method _read();
                       return base._read(); 
                     endmethod
                     method Action _write(x);
                       w.wset(x);
                       //state <= x;
                     endmethod
                   endinterface;
  Reg#(alpha) i1 = interface Reg
                     method _read     = fromMaybe(base._read, w.wget());
                     method _write(x) = noAction; // never used
                   endinterface;
  return (tuple2(i0,i1));
endmodule


//Creates an EHR module by layering virtual registers
//
//  .idx[i] holds read_i and write_i methods

//XXX we should a mkEHRWrapper to wrap register implementations and share this

module mkEHR#(alpha init)(EHR#(n,alpha)) provisos(Bits#(alpha, asz)); 
  Reg#(alpha) r <- mkReg(init);

  Vector#(n,Reg#(alpha)) vidx = newVector();
  // placeholder ifc
  Reg #(alpha) old = r;
  // make interfaces
  for(Integer i=0; i < valueOf(n); i = i + 1)
    begin
       Tuple2#(Reg#(alpha),Reg#(alpha)) tinf <- mkVirtualReg(r,old);
       vidx[i] = tinf.fst();
       old = tinf.snd();
    end

  rule updateR;
      r <= vidx[valueOf(n)-1]._read;
  endrule

  return vidx;
endmodule

module mkEHRU(EHR#(n,alpha)) provisos(Bits#(alpha, asz)); 
  Reg#(alpha) r <- mkRegU;

  Vector#(n,Reg#(alpha)) vidx = newVector();
  // placeholder ifc
  Reg #(alpha) old = r;
  // make interfaces
  for(Integer i=0; i < valueOf(n); i = i + 1)
    begin
       Tuple2#(Reg#(alpha),Reg#(alpha)) tinf <- mkVirtualReg(r,old);
       vidx[i] = tinf.fst();
       old = tinf.snd();
    end

  rule updateR;
      r <= vidx[valueOf(n)-1]._read;
  endrule

  return vidx;
endmodule

//interface IEHR#(type n_sz, type alpha);
//  interface EHR#(n_sz, alpha) idx;
//endinterface  

// (* synthesize *)
// module mkEHR4(EHR#(4,Bit#(4)));
//    EHR#(4,Bit#(4)) r <- mkEHR(0);
//    interface idx = r;
// endmodule
