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
    
	let lo_index = 0; let hi_index = fromInteger(2**valueOf(indexSize) - 1);

	RegFile#(Bit#(indexSize), Addr) targetsRf <- mkRegFileWCF(lo_index, hi_index);
	RegFile#(Bit#(indexSize), Bit#(tagSize)) tagsRf <- mkRegFileWCF(lo_index, hi_index);
	RegFile#(Bit#(indexSize), PrdState) prdStatesRf <- mkRegFileWCF(lo_index, hi_index);

//	Vector#(TExp#(indexSize), Reg#(Addr))          targets <- replicateM(mkRegU);
  //  Vector#(TExp#(indexSize), Reg#(Bit#(tagSize)))    tags <- replicateM(mkRegU);
    
    // 1 Bit Prediction model: "Valid"
    //Vector#(TExp#(indexSize), Reg#(Bool))            valid <- replicateM(mkReg(False));

    // 2 Bit Prediction model, "prdStates" initial value = Weakly Not Taken
    //Vector#(TExp#(indexSize), Reg#(PrdState)) prdStates <- replicateM(mkReg(WNT)); 

    function Bit#(indexSize) getIndex(Addr pc) = truncate(pc >> 2);
    function Bit#(tagSize) getTag(Addr pc) = truncateLSB(pc);
    function Bool takeBr(Bit#(indexSize) idx) = prdStatesRf.sub(idx) == ST || prdStatesRf.sub(idx) == WT;
    
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

        if (takeBr(index) && (tag == tagsRf.sub(index))) begin
	    //$display("TAG MATCHED -- TAKE BR -- idx = ", index);
	    //$display("tag = ", tag);
	    //$display("ppc = ", targets[index]);
            return targetsRf.sub(index);
        end 
	else begin
            return (pc + 4);
        end
    endmethod

    method Action update(Addr thisPc, Addr nextPc);
        let index = getIndex(thisPc);
    	let tag = getTag(thisPc);

	if (tagsRf.sub(index) == tag) begin // Tag was matched
		if (nextPc != thisPc + 4) begin // Actual = brTaken
			let prevState = prdStatesRf.sub(index);
			prdStatesRf.upd(index, setState(prevState, True));
			$display("Tag was MATCHED && Actual Br TAKEN");
			$display("Index: ", index, " Tag: ", tag, " new State: ", setState(prevState, True));
		end
		else begin // Actual = brNotTaken
                        let prevState = prdStatesRf.sub(index);
			prdStatesRf.upd(index, setState(prevState, False));
                        $display("Tag was MATCHED && Actual Br NT");
                        $display("Index: ", index, " Tag: ", tag, " new State: ", setState(prevState, False));
		end
	end
	else begin // Tag was unmatched, so prd = pc + 4
		if (nextPc != thisPc + 4) begin // Actual = brTaken
			tagsRf.upd(index, tag);
			targetsRf.upd(index, nextPc);
			prdStatesRf.upd(index, WT);
                        $display("Tag was UNMATCHED && Actual Br TAKEN");
                        $display("Index: ", index, " Tag: ", tag, " new State: 1");
		end
		else begin
                        tagsRf.upd(index, tag);
                        targetsRf.upd(index, nextPc);
                        prdStatesRf.upd(index, WNT);
                        $display("Tag was UNMATCHED && Actual Br NT");
                        $display("Index: ", index, " Tag: ", tag, " new State: 2");
		end
	end

	/*
        if( nextPc != thisPc + 4 ) begin 
            tags[index] <= tag;
            targets[index] <= nextPc;
	    $display("Updated index = ", index, " tag = ", tag, " target = ", nextPc, " state = ", setState(prdStates[index], True));
	    prdStates[index] <= setState(prdStates[index], True);
        end
	else begin
	    prdStates[index] <= setState(prdStates[index], False);
	end
	*/
    endmethod
endmodule

