import Types::*;
import ProcTypes::*;
import Ehr::*;
import ConfigReg::*;
import Fifo::*;

typedef enum {Ctr, Mem, J_, Jr_, Br_} InstCntType deriving(Bits, Eq);

/*Exercise 4*/
/* TODO: Replace dummies to implement incMissInstTypeCnt */
typedef enum {J_, Jr_, Br_} InstMissCntType deriving(Bits, Eq);

interface CsrFile;
    method Action start(Data id);
    method Bool started;
    method Data rd(CsrIndx idx);
    method Action wr(Maybe#(CsrIndx) idx, Data val);
    method ActionValue#(CpuToHostData) cpuToHost;
    method Action incInstTypeCnt(InstCntType inst);
    method Action incBPMissCnt();
    method Action incMissInstTypeCnt(InstMissCntType inst);
endinterface

(* synthesize *)
module mkCsrFile(CsrFile);
    Reg#(Bool) startReg <- mkConfigReg(False);

    // CSR 
    Reg#(Data) numInsts <- mkConfigReg(0); // csrInstret -- read only
    Reg#(Data) cycles <- mkReg(0); // csrCycle -- read only
    Reg#(Data) coreId <- mkConfigReg(0); // csrMhartid -- read only
    
    Reg#(Data) numMem  <- mkConfigReg(0);
    Reg#(Data) numCtr  <- mkConfigReg(0);
    Reg#(Data) numBPMiss <- mkConfigReg(0);

    Reg#(Data) numJ <- mkConfigReg(0);
    Reg#(Data) numJMiss <- mkConfigReg(0);
    Reg#(Data) numJr <- mkConfigReg(0);
    Reg#(Data) numJrMiss <- mkConfigReg(0);
    Reg#(Data) numBr <- mkConfigReg(0);
    Reg#(Data) numBrMiss <- mkConfigReg(0);

    Fifo#(2, CpuToHostData) toHostFifo <- mkCFFifo; // csrMtohost -- write only
    Fifo#(2, Tuple3#(CsrIndx, Data, Data)) csrFifo <- mkCFFifo;

    rule count (startReg);
        cycles <= cycles + 1;
        $display("\nCycle %d ----------------------------------------------------", cycles);
    endrule

    method Action start(Data id) if(!startReg);
        startReg <= True;
        cycles <= 0;
	coreId <= id;
    endmethod

    method Bool started;
        return startReg;
    endmethod

    method Data rd(CsrIndx idx);
        return (case(idx)
                    csrCycle: cycles;
                    csrInstret: numInsts;
                    csrMhartid: coreId;
    		    default: ?;
                endcase);
    endmethod
    
    method Action wr(Maybe#(CsrIndx) csrIdx, Data val);
        if(csrIdx matches tagged Valid .idx) begin
            case (idx)
                csrMtohost: begin

                    $fwrite(stderr, "===========================\n");
                    $fwrite(stderr, "Specific type of executed instructions\n");
                    $fwrite(stderr, "Ctr              : %d\n", numCtr);
                    $fwrite(stderr, "Mem              : %d\n", numMem);
                    $fwrite(stderr, "\nMispredicted       : %d\n", numBPMiss);
                    $fwrite(stderr, "==========================================\n");

                    /*Exercise_4*/
                    /* TODO: Implement below to output the counted values */
                    $fwrite(stderr, "Misprediction detail\n");
                    $fwrite(stderr, "J               : %d / %d\n", numJMiss, numJ);
                    $fwrite(stderr, "JR              : %d / %d\n", numJrMiss, numJr);
                    $fwrite(stderr, "BR              : %d / %d\n" , numBrMiss, numBr);
                    $fwrite(stderr, "==========================================\n");

                    // high 16 bits encodes type, low 16 bits are data
                    Bit#(16) hi = truncateLSB(val);
                    Bit#(16) lo = truncate(val);
                    toHostFifo.enq(CpuToHostData {
                        c2hType: unpack(truncate(hi)),
                        data: lo,
                        data2: numInsts
                    });
                end
            endcase
        end
        numInsts <= numInsts + 1;
    endmethod


    method Action incInstTypeCnt(InstCntType inst);
      case(inst)
              J_ : begin 
		      numCtr <= numCtr + 1; 
		      numJ <= numJ + 1;
	      end
      	      Jr_ : begin 
		      numCtr <= numCtr + 1;
		      numJr <= numJr + 1;
	      end
              Br_ : begin
		      numCtr <= numCtr + 1; 
		      numBr <= numBr + 1;
	      end
	      Mem : numMem <= numMem + 1;
        endcase
    endmethod

    method Action incMissInstTypeCnt(InstMissCntType inst);
        /*Exercise_4*/
        /* TODO: implement incMissInstTypeCnt */

            case(inst)
		    J_ : numJMiss <= numJMiss + 1;
		    Jr_: numJrMiss <= numJrMiss + 1;
		    Br_: numBrMiss <= numBrMiss + 1;
	    endcase
    endmethod

    method Action incBPMissCnt();
      numBPMiss <= numBPMiss + 1;
    endmethod

    method ActionValue#(CpuToHostData) cpuToHost;
        toHostFifo.deq;
        return toHostFifo.first;
    endmethod

endmodule
