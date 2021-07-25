import Types::*;
import ProcTypes::*;
import RegFile::*;
import Vector::*;

// indexSize is the number of bits in the index
interface Btb#(numeric type indexSize);
    method Addr predPc(Addr pc);
    method Action update(Addr thispc, Addr nextpc);
endinterface

//typedef Bit#(2) PrdState;

typedef enum { ST, WT, WNT, SNT } PrdState deriving (Bits, Eq);

// BTB use full tags, and should be only updated for BRANCH/JUMP instructions
// so it ALWAYS predicts pc+4 for NON-BRANCH instructions
module mkBtb( Btb#(indexSize) ) provisos( Add#(indexSize,a__,32), NumAlias#(TSub#(TSub#(AddrSz, 2), indexSize), tagSize) );
    Vector#(TExp#(indexSize), Reg#(Addr))          targets <- replicateM(mkRegU);
    Vector#(TExp#(indexSize), Reg#(Bit#(tagSize)))    tags <- replicateM(mkRegU);
    
    // 1 Bit Prediction model: "Valid"
    //Vector#(TExp#(indexSize), Reg#(Bool))            valid <- replicateM(mkReg(False));

    // 2 Bit Prediction model, "prdStates" initial value = Weakly Not Taken
    Vector#(TExp#(indexSize), Reg#(PrdState)) prdStates <- replicateM(mkReg(WNT)); 

    function Bit#(indexSize) getIndex(Addr pc) = truncate(pc >> 2);
    function Bit#(tagSize) getTag(Addr pc) = truncateLSB(pc);

    function Bool takeBr(Bit#(indexSize) index) = prdStates[index] == ST || prdStates[index] == WT;
    
    function PrdState setState(PrdState currState, Bool brTaken);
	    case(currState)
		    ST: return brTaken ? ST : WT;
		    WT: return brTaken ? ST : WNT;
 		    WNT: return brTaken ? WT : SNT;
 		    SNT: return brTaken ? WNT : SNT;
 	    endcase 
    endfunction

    method Addr predPc(Addr pc);
        let index = getIndex(pc);
        let tag = getTag(pc);

        if(takeBr(index) && (tag == tags[index])) begin
            return targets[index];
        end else begin
            return (pc + 4);
        end
    endmethod

    method Action update(Addr thisPc, Addr nextPc);
    	// thisPc = pc, nextPc = execInst.addr (real ppc)
	// what if pred: BT => nextPc = BNT addr? 	    
        let index = getIndex(thisPc);
    	let tag = getTag(thisPc);

        if( nextPc != thisPc + 4 ) begin // actual: T case
            // update entry
            //valid[index] <= True; // should reset to WNT or inherit?
            tags[index] <= tag;
            targets[index] <= nextPc;
	    prdStates[index] <= setState(prdStates[index], True);
        end
	else begin
	    prdStates[index] <= setState(prdStates[index], False);
	end
	//
	//else if (tag == tags[index]) valid[index] <= False; // what if case - should pred to pc + 4
    endmethod
endmodule

