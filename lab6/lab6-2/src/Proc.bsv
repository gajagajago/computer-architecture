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
import Ehr::*;

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
  RFile         rf  <- mkBypassRFile; 
  IMemory     iMem  <- mkIMemory;
  DMemory     dMem  <- mkDMemory;
  CsrFile     csrf <- mkCsrFile;

  // Control hazard handling Elements : 3 Epoch registers and 2 BypassFifo
  Reg#(Bool) fEpoch <- mkRegU;
  Reg#(Bool) dEpoch <- mkRegU;
  Reg#(Bool) eEpoch <- mkRegU;
  Fifo#(1, Addr)  execRedirect <- mkBypassFifo;
  //Fifo#(1, Addr)  execRedirect <- mkPipelineFifo;
  Fifo#(1, Addr) execRedirectToDecode <- mkBypassFifo; 
  //Fifo#(1, Addr)  execRedirectToDecode <- mkPipelineFifo;


  // 4 Pipeline Fifos between stages
  //Fifo#(1, Fetch2Decode)  f2d <- mkBypassFifo;
  //Fifo#(1, Decode2Execute)  d2e <- mkBypassFifo;
  //Fifo#(1, Execute2Memory)  e2m <- mkBypassFifo;
  Fifo#(1, Memory2WriteBack)  m2w <- mkBypassFifo;
  Fifo#(1, Fetch2Decode)  f2d <- mkPipelineFifo;
  Fifo#(1, Decode2Execute)  d2e <- mkPipelineFifo;
  Fifo#(1, Execute2Memory)  e2m <- mkPipelineFifo;
  //Fifo#(1, Memory2WriteBack)  m2w <- mkPipelineFifo;

  //Fifo#(1, ControlSignals) ctrlf2d <- mkPipelineFifo;
  Fifo#(1, ControlSignals) ctrld2e <- mkPipelineFifo;
  Fifo#(1, ControlSignals) ctrle2m <- mkPipelineFifo;
  //Fifo#(1, ControlSignals) ctrlm2w <- mkPipelineFifo;
  //Fifo#(1, ControlSignals) ctrlf2d <- mkBypassFifo;
  //Fifo#(1, ControlSignals) ctrld2e <- mkBypassFifo;
  //Fifo#(1, ControlSignals) ctrle2m <- mkBypassFifo;
  Fifo#(1, ControlSignals) ctrlm2w <- mkBypassFifo;

 // Fifo#(1, Bit#(1)) stallSignd2f <- mkPipelineFifo;

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
		  $display("Inst at Fetch : ");		  
 		  $display(show);

		  //if(stallSignd2f.notEmpty) begin
		//	  stallSignd2f.deq;
		//	  $display("currently stalled, Fetch do not update pc");
		  //end
		 // else begin
			  $display("not in stall condition. update pc and f2d");
			  let ppc = pc + 4;
			  f2d.enq(Fetch2Decode{inst:inst, pc:pc, ppc:ppc, epoch:fEpoch});
			  pc <= ppc;
		 // end
 	  end
  endrule

  rule doDecode(csrf.started);
	  if (execRedirectToDecode.notEmpty) begin
		  execRedirectToDecode.deq;
		  dEpoch <= !dEpoch;
	  end
	  else begin
		  let x = f2d.first;// f2d.deq;
		  let inst = x.inst;
		  let pc = x.pc;
		  let ppc = x.ppc;
	  	  let fEpoch = x.epoch;

		  if(fEpoch == dEpoch) begin
			  $display("reached decode epoch comparison");
			  let dInst = decode(inst);

			  // Generate Control Signals for d2e, e2m, m2w
			  let ctrl;
			  Opcode opcode = getICode(inst);
 
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


			  Bool stall = (ctrl.memRead == 1'b1 && ((fromMaybe(?,d2e.first.dInst.dst) == fromMaybe(?,dInst.src1)) || (fromMaybe(?,d2e.first.dInst.dst) == fromMaybe(?,dInst.src2))));

			  if(!stall) begin
				  ctrld2e.enq(ctrl); // Original ctrl is passed to d2e
				  f2d.deq;
			  end
			  else begin // Stall condition
				  // ctrl value for ID/EX = 0 
				  ctrld2e.enq(ControlSignals{aluOp:2'b00, aluSrc:1'b0, branch:1'b0, memRead:1'b0, memWrite:1'b0, regWrite:1'b0, memtoReg:1'b0});
				  // not update pc and IF/ID reg
				  // stallSignd2f.enq(1'b1);
				  // when stall, do not deq from f2d
			  end

			  // No matter stall or not stall, still enq inst into d2e
			  let rVal1 = isValid(dInst.src1) ? rf.rd1(validValue(dInst.src1)) : ?;
                          let rVal2 = isValid(dInst.src2) ? rf.rd2(validValue(dInst.src2)) : ?;
                          let csrVal = isValid(dInst.csr) ? csrf.rd(validValue(dInst.csr)) : ?;
                          
			  d2e.enq(Decode2Execute{dInst:dInst, pc:pc, ppc:ppc, epoch:fEpoch, rVal1:rVal1, rVal2:rVal2, csrVal:csrVal});
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

		  Bool execHazardA = False; Bool execHazardB = False;
		  Bool memHazardA = False; Bool memHazardB = False;

		  // Exec hazard detection
		  if(e2m.notEmpty) begin

		  	let e2mInst = e2m.first.eInst; // ex < mem // this is where the prob is.
		  	let e2mInstDst = fromMaybe(?, e2mInst.dst);
		  	let e2mCtrl = ctrle2m.first; // e2m control is write

		  	if ( (e2mCtrl.regWrite == 1'b1) && (e2mInstDst != 0) ) begin
				if (e2mInstDst == fromMaybe(?, dInst.src1)) execHazardA = True;
			  	if (e2mInstDst == fromMaybe(?, dInst.src2)) execHazardB = True;
		 	 end

		  	if (execHazardA) rVal1 = e2mInst.data;
		  	if (execHazardB) rVal2 = e2mInst.data;
		  end

		  if(m2w.notEmpty) begin
		  	let m2wInst = m2w.first.eInst;
			let m2wInstDst = fromMaybe(?, m2wInst.dst);
			let m2wCtrl = ctrlm2w.first;

			if ( (m2wCtrl.regWrite == 1'b1) && (m2wInstDst != 0)) begin
				if ((!execHazardA) && (m2wInstDst == fromMaybe(?, dInst.src1))) memHazardA = True;
				if ((!execHazardB) && (m2wInstDst == fromMaybe(?, dInst.src2))) memHazardB = True;
			end

			if (memHazardA) rVal1 = m2wInst.data;
			if (memHazardB) rVal2 = m2wInst.data;
		  end

		  let eInst = exec(dInst, rVal1, rVal2, pc, ppc, csrVal);              
		  
		  e2m.enq(Execute2Memory{eInst:eInst});
		  ctrle2m.enq(ctrld2e.first);

		  if(eInst.mispredict) begin
			  eEpoch <= !eEpoch;
			  execRedirect.enq(eInst.addr);
			  execRedirectToDecode.enq(eInst.addr);
		  end
	  end
	  
	  ctrld2e.deq;
	  d2e.deq;
  endrule

  rule doMemory(csrf.started);
	  ctrle2m.deq;

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
	  ctrlm2w.enq(ctrle2m.first);
  endrule

  rule doWriteBack(csrf.started);
	  ctrlm2w.deq;
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
