import Types::*;
import ProcTypes::*;
import CMemTypes::*;
import MemInit::*;
import RFile::*;
import IMemory::*;
import DMemory::*;
import Decode::*;
import Exec::*;
import CsrFile::*;
import Fifo::*;
import Scoreboard::*;
import GetPut::*;

typedef struct {
  Instruction inst;
  Addr pc;
  Addr ppc;
  Bool epoch;
} Fetch2Decode deriving(Bits, Eq);

typedef struct {
  DecodedInst dInst;
  Addr pc;
  Addr ppc;
  Bool epoch;
  Data rVal1;
  Data rVal2;
  Data csrVal;
} Decode2Execute deriving(Bits, Eq);

typedef struct {
	ExecInst eInst;
} Execute2Memory deriving(Bits, Eq);

typedef struct {
	ExecInst eInst;
} Memory2WriteBack deriving(Bits, Eq);


(*synthesize*)
module mkProc(Proc);
  Reg#(Addr)    pc  <- mkRegU;
  RFile         rf  <- mkBypassRFile; 
  IMemory     iMem  <- mkIMemory;
  DMemory     dMem  <- mkDMemory;
  CsrFile     csrf <- mkCsrFile;

  // Control hazard handling Elements : 3 Epoch registers and 2 BypassFifo
  Reg#(Bool) fEpoch <- mkRegU;
  Reg#(Bool) dEpoch <- mkRegU;
  Reg#(Bool) eEpoch <- mkRegU;
  Fifo#(1, Addr)  execRedirect <- mkBypassFifo;
  Fifo#(1, Addr) execRedirectToDecode <- mkBypassFifo; 

  // 4 Pipeline Fifos between stages
  Fifo#(1, Fetch2Decode)  f2d <- mkPipelineFifo;
  Fifo#(1, Decode2Execute)  d2e <- mkPipelineFifo;
  Fifo#(1, Execute2Memory)  e2m <- mkPipelineFifo;
  Fifo#(1, Memory2WriteBack)  m2w <- mkPipelineFifo;

  // Data hazard handling Element : Scoreboard
  Scoreboard#(4) sb <- mkPipelineScoreboard;

/* TODO: Lab 6-1: Implement 5-stage pipelined processor with scoreboard. */
  rule doFetch(csrf.started);
	  if(execRedirect.notEmpty) begin
      	    	  execRedirect.deq;
      	    	  pc <= execRedirect.first;
      	    	  fEpoch <= !fEpoch;
	  end
	  else begin
	  	  let inst = iMem.req(pc);
		  $display(showInst(inst));
      	    	  $display("pc: ", pc);
		  let ppc = pc + 4;
	  	  f2d.enq(Fetch2Decode{inst:inst, pc:pc, ppc:ppc, epoch:fEpoch});
	  	  pc <= ppc;
 	  end
  endrule

  rule doDecode(csrf.started);
	  $display("decode");
	  if (execRedirectToDecode.notEmpty) begin
		  execRedirectToDecode.deq;
		  dEpoch <= !dEpoch;
	  end
	  else begin
		  let x = f2d.first;
		  let inst = x.inst;
		  let pc = x.pc;
		  let ppc = x.ppc;
	  	  let fEpoch = x.epoch;
		  
		  if(fEpoch == dEpoch) begin
			  let dInst = decode(inst);
			  let stall = sb.search1(dInst.src1) || sb.search2(dInst.src2);
			  
			  if(!stall) begin
 				  let rVal1 = isValid(dInst.src1) ? rf.rd1(validValue(dInst.src1)) : ?;
				  let rVal2 = isValid(dInst.src2) ? rf.rd2(validValue(dInst.src2)) : ?;
				  let csrVal = isValid(dInst.csr) ? csrf.rd(validValue(dInst.csr)) : ?;
				  
				  d2e.enq(Decode2Execute{dInst:dInst, pc:pc, ppc:ppc, epoch:fEpoch, rVal1:rVal1, rVal2:rVal2, csrVal:csrVal});
				  sb.insert(dInst.dst);
				  f2d.deq; // when stall, do not deq from f2d;
			  end
		  end
		  else f2d.deq;
	  end
  endrule

  rule doExecute(csrf.started);
	  let x = d2e.first;
	  let iEpoch = x.epoch;

	  if(iEpoch == eEpoch) begin 
		  let dInst = x.dInst;  let csrVal = x.csrVal;
       	   	  let pc = x.pc;        let ppc = x.ppc;
		  let rVal1 = x.rVal1;  let rVal2 = x.rVal2;
			   
		  let eInst = exec(dInst, rVal1, rVal2, pc, ppc, csrVal);              
		  e2m.enq(Execute2Memory{eInst:eInst});
			   
		  if(eInst.mispredict) begin
			  eEpoch <= !eEpoch;
			  execRedirect.enq(eInst.addr);
			  execRedirectToDecode.enq(eInst.addr);
		  end
	  end

	  d2e.deq;
  endrule

  rule doMemory(csrf.started);
	  let eInst = e2m.first.eInst;
  	  let iType = eInst.iType;
	  
	  case(iType)
		  Ld :begin
			  let d <- dMem.req(MemReq{op: Ld, addr: eInst.addr, data: ?});
			  eInst.data = d;
		  end
		  St :let d <- dMem.req(MemReq{op: St, addr: eInst.addr, data: eInst.data});
		  Unsupported :begin
	  		  $fwrite(stderr, "ERROR: Executing unsupported instruction\n");
			  $finish;
		  end
	  endcase

	  m2w.enq(Memory2WriteBack{eInst:eInst});
	  e2m.deq;
  endrule

  rule doWriteBack(csrf.started);
	  let eInst = m2w.first.eInst;
	  
	  if (isValid(eInst.dst)) rf.wr(fromMaybe(?, eInst.dst), eInst.data);
	  csrf.wr(eInst.iType == Csrw ? eInst.csr : Invalid, eInst.data);
	  
	  sb.remove;
	  m2w.deq;
  endrule

  method ActionValue#(CpuToHostData) cpuToHost;
    let retV <- csrf.cpuToHost;
    return retV;
  endmethod

  method Action hostToCpu(Bit#(32) startpc) if (!csrf.started);
    csrf.start(0);
    eEpoch <= False;
    dEpoch <= False;
    fEpoch <= False;
    pc <= startpc;
  endmethod

  interface iMemInit = iMem.init;
  interface dMemInit = dMem.init;

endmodule
