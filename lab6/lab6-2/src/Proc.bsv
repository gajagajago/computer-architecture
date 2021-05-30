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
//import Scoreboard::*;
import GetPut::*;
//import Ehr::*;

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

typedef struct {
	Bit#(2) aluOp;
	Bit#(1) aluSrc;
	Bit#(1) branch;
	Bit#(1) memRead;
	Bit#(1) memWrite;
	Bit#(1) regWrite;
	Bit#(1) memtoReg;
} ControlSignals deriving(Bits, Eq);
(*synthesize*)
module mkProc(Proc);
  Reg#(Addr)    pc  <- mkRegU;
//	Ehr#(2, Addr) pc <- mkEhr(0);
  RFile         rf  <- mkBypassRFile; 
  //RFile         rf  <- mkRFile;
  IMemory     iMem  <- mkIMemory;
  DMemory     dMem  <- mkDMemory;
  CsrFile     csrf <- mkCsrFile;

  // Control hazard handling Elements : 3 Epoch registers and 2 BypassFifo
  Reg#(Bool) fEpoch <- mkRegU;
  Reg#(Bool) dEpoch <- mkRegU;
  Reg#(Bool) eEpoch <- mkRegU;
  Fifo#(1, Addr)  execRedirect <- mkBypassFifo;
 // Fifo#(1, Addr)  execRedirect <- mkPipelineFifo;
  Fifo#(1, Addr) execRedirectToDecode <- mkBypassFifo; 
  //Fifo#(1, Addr)  execRedirectToDecode <- mkPipelineFifo;


  // 4 Pipeline Fifos between stages
  //Fifo#(1, Fetch2Decode)  f2d <- mkBypassFifo;
  //Fifo#(1, Decode2Execute)  d2e <- mkBypassFifo;
  //Fifo#(1, Execute2Memory)  e2m <- mkBypassFifo;
  //Fifo#(1, Memory2WriteBack)  m2w <- mkBypassFifo;
  Fifo#(1, Fetch2Decode)  f2d <- mkBypassFifo;
  Fifo#(1, Decode2Execute)  d2e <- mkPipelineFifo;
  Fifo#(1, Execute2Memory)  e2m <- mkBypassFifo;
  Fifo#(1, Memory2WriteBack)  m2w <- mkBypassFifo;

  //Fifo#(1, ControlSignals) ctrlf2d <- mkPipelineFifo;
  //Fifo#(1, ControlSignals) ctrld2e <- mkPipelineFifo;
  //Fifo#(1, ControlSignals) ctrle2m <- mkPipelineFifo;
  //Fifo#(1, ControlSignals) ctrlm2w <- mkPipelineFifo;
  Fifo#(1, ControlSignals) ctrlf2d <- mkBypassFifo;
  Fifo#(1, ControlSignals) ctrld2e <- mkBypassFifo;
  Fifo#(1, ControlSignals) ctrle2m <- mkBypassFifo;
  Fifo#(1, ControlSignals) ctrlm2w <- mkBypassFifo;

/* TODO: Lab 6-2: Implement 5-stage pipelined processor with forwarding unit. */
  rule doFetch(csrf.started);
	  if(execRedirect.notEmpty) begin
      	    	  execRedirect.deq;
      	    	  pc <= execRedirect.first;
      	    	  fEpoch <= !fEpoch;
	  end
	  else begin
	  	  let inst = iMem.req(pc);
		  //pretty print instruction of current cycle
		  let show = showInst(inst);
		  $display(show);

		  Opcode opcode = getICode(inst);
		  let ctrl;
		  case(opcode)
 			  opOp: begin 
				  ctrl = ControlSignals{aluOp:2'b10, aluSrc:1'b0, branch:1'b0, memRead:1'b0, memWrite:1'b0, regWrite:1'b1, memtoReg:1'b0};
				  $display("opOp");
			  end
 			 opLoad: begin 
				 ctrl = ControlSignals{aluOp:2'b00, aluSrc:1'b1, branch:1'b0, memRead:1'b1, memWrite:1'b0, regWrite:1'b1, memtoReg:1'b1};
				 $display("opLoad");
			 end
 			opStore: begin
				ctrl = ControlSignals{aluOp:2'b00, aluSrc:1'b1, branch:1'b0, memRead:1'b0, memWrite:1'b1, regWrite:1'b0, memtoReg:1'b0};
				$display("opStore");
			end
 		       opBranch: begin 
		       		ctrl = ControlSignals{aluOp:2'b01, aluSrc:1'b0, branch:1'b1, memRead:1'b0, memWrite:1'b0, regWrite:1'b0, memtoReg:1'b0};
				$display("opBrach");
			end
		       default: begin 
			       ctrl = ControlSignals{aluOp:2'b00, aluSrc:1'b0, branch:1'b0, memRead:1'b0, memWrite:1'b0, regWrite:1'b0, memtoReg:1'b0};
			       $display("default");
		       end
		  endcase

		  ctrlf2d.enq(ctrl);
      	    	  let ppc = pc + 4;
	  	  f2d.enq(Fetch2Decode{inst:inst, pc:pc, ppc:ppc, epoch:fEpoch});
	  	  pc <= ppc;
 	  end
  endrule

  rule doDecode(csrf.started);
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
		  let ctrl = ctrlf2d.first;

		  if(fEpoch == dEpoch) begin
			  let dInst = decode(inst);
			  Bool stall = False; //temp
			  if(!stall) begin
 				  let rVal1 = isValid(dInst.src1) ? rf.rd1(validValue(dInst.src1)) : ?;
				  let rVal2 = isValid(dInst.src2) ? rf.rd2(validValue(dInst.src2)) : ?;
				  let csrVal = isValid(dInst.csr) ? csrf.rd(validValue(dInst.csr)) : ?;
				  
				  d2e.enq(Decode2Execute{dInst:dInst, pc:pc, ppc:ppc, epoch:fEpoch, rVal1:rVal1, rVal2:rVal2, csrVal:csrVal});
				  f2d.deq; // when stall, do not deq from f2d;
				  ctrld2e.enq(ctrl);
				  ctrlf2d.deq;
			  end
		  end
		  else begin
			  f2d.deq;
			  ctrlf2d.deq;
		  end
	  end
  endrule

  rule doExecute(csrf.started);
	  let x = d2e.first;
	  let iEpoch = x.epoch;

	  if(iEpoch == eEpoch) begin 
		  let dInst = x.dInst;  let csrVal = x.csrVal;
       	   	  let pc = x.pc;        let ppc = x.ppc;
		  let rVal1 = x.rVal1;  let rVal2 = x.rVal2;

		  // Exec hazard detection
		  
			   
		  let eInst = exec(dInst, rVal1, rVal2, pc, ppc, csrVal);              
		  e2m.enq(Execute2Memory{eInst:eInst});
			   
		  if(eInst.mispredict) begin
			  eEpoch <= !eEpoch;
			  execRedirect.enq(eInst.addr);
			  execRedirectToDecode.enq(eInst.addr);
			  //pc[0] <= eInst.addr;
		  end
	  end

	 // ctrld2e.deq;
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
		  St :begin
			  let d <- dMem.req(MemReq{op: St, addr: eInst.addr, data: eInst.data});
		  end
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
