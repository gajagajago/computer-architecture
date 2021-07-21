import MemTypes::*;
import RegFile::*;

typedef 4 LineNum;
typedef Bit#(LineNum) LineIdx;

interface NextAddressPredictor;
    method Addr prediction(Addr pc);
    method Action update(Addr pc, Addr target);
endinterface

(* synthesize *)
module mkBTB(NextAddressPredictor);
    RegFile#(LineIdx, Addr)    tagArr <- mkRegFileFull;
    RegFile#(LineIdx, Addr) targetArr <- mkRegFileFull;                                  

    method Addr prediction(Addr pc);
		return (pc + 4);
    endmethod

    method Action update(Addr pc, Addr target);
		noAction;
    endmethod
endmodule
