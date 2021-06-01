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

  Fifo#(1, Addr)         execRedirect <- mkBypassFifo; 
  Fifo#(1, Addr) execRedirectToDecode <- mkBypassFifo;

  // 4 Pipeline Fifos between stage
  Fifo#(1, Fetch2Decode)  f2d <- mkPipelineFifo;
  Fifo#(1, Decode2Execute)  d2e <- mkPipelineFifo;
  Fifo#(1, Execute2Memory)  e2m <- mkPipelineFifo;
  Fifo#(1, Memory2WriteBack)  m2w <- mkPipelineFifo;

  // 3 Forwarding Units
  Fifo#(1, Decode2Execute) execFwd <- mkBypassFifo;
  Fifo#(1, Execute2Memory) memFwd <- mkBypassFifo;
  Fifo#(1, Memory2WriteBack) writeFwd <- mkBypassFifo;

 /* TODO:  Lab 6-2: Implement 5-stage pipelined processor with forwarding method. 
           You should first define proper forwarding units using BypassFiFo*/
  rule doFetch(csrf.started);
    if(execRedirect.notEmpty) begin
      execRedirect.deq;
      pc <= execRedirect.first;
      fEpoch <= !fEpoch;
    end
    else begin
            let inst = iMem.req(pc);
            let ppc = pc + 4; pc <= ppc;
            f2d.enq(Fetch2Decode{inst:inst, pc:pc, ppc:ppc, epoch:fEpoch});
    end
  endrule

  rule doDecode(csrf.started);
	if (execRedirectToDecode.notEmpty) begin
          	execRedirectToDecode.deq;
            	dEpoch <= !dEpoch;
        end
	else begin
		Fetch2Decode x = f2d.first;

		if(x.epoch != dEpoch) f2d.deq;
		else begin
			DecodedInst dInst = decode(x.inst);

			// Checking for Stall
        		Bool stall = False; 
			if (execFwd.notEmpty) begin
				Bool prevInstLd = execFwd.first.dInst.iType == Ld;
				Bool use1 = validValue(execFwd.first.dInst.dst) == validValue(dInst.src1);				   
 			       	Bool use2 = validValue(execFwd.first.dInst.dst) == validValue(dInst.src2);

				stall = prevInstLd && (use1 || use2);
				execFwd.deq;
			end
        	
			if(stall) begin 
				Instruction nop = 32'b00000000000000000000000000010011;
				dInst = decode(nop); // When stall, 1) Insert nop 2) Prevent update of pc & f2d by not dequeing from f2d
                	end
                	else f2d.deq;
			
			// Regardless of stall or not stall, still enq inst into d2e
                        let rVal1 = isValid(dInst.src1) ? rf.rd1(validValue(dInst.src1)) : ?;
                        let rVal2 = isValid(dInst.src2) ? rf.rd2(validValue(dInst.src2)) : ?;
                        let csrVal = isValid(dInst.csr) ? csrf.rd(validValue(dInst.csr)) : ?;

			d2e.enq(Decode2Execute{dInst:dInst,pc:x.pc,ppc:x.ppc,epoch:x.epoch,rVal1:rVal1, rVal2:rVal2,csrVal:csrVal});
		end
	end
  endrule

  rule doExecute(csrf.started);
	  Decode2Execute x = d2e.first; d2e.deq;

	  if (x.epoch == eEpoch) begin
		  execFwd.enq(x); // BypassFifo execFwd is used for stall checking

		  Bool execHazardA = False; Bool execHazardB = False;
                  Bool memHazardA = False; Bool memHazardB = False;

		  // Exec hazard detection
		  if (memFwd.notEmpty) begin
			 let prevItype = memFwd.first.eInst.iType;
  		       	 Bool prevInstWrite = prevItype == Alu || prevItype == Auipc || prevItype == J || prevItype == Jr || prevItype == Ld;
			 Bool notWriteTo0 = validValue(memFwd.first.eInst.dst) != 0;
			 Bool useA = validValue(memFwd.first.eInst.dst) == validValue(x.dInst.src1);
			 Bool useB = validValue(memFwd.first.eInst.dst) == validValue(x.dInst.src2);

			 execHazardA = prevInstWrite && notWriteTo0 && useA;
			 execHazardB = prevInstWrite && notWriteTo0 && useB;

			 memFwd.deq;
		  end

		  // Mem hazard detection
		  if (writeFwd.notEmpty) begin
                         let secondPrevItype = writeFwd.first.eInst.iType;
                         Bool secondPrevInstWrite = secondPrevItype == Alu || secondPrevItype == Auipc || secondPrevItype == J || secondPrevItype == Jr || secondPrevItype == Ld;
                         Bool notWriteTo0 = validValue(writeFwd.first.eInst.dst) != 0;
                         Bool useA = validValue(writeFwd.first.eInst.dst) == validValue(x.dInst.src1);
                         Bool useB = validValue(writeFwd.first.eInst.dst) == validValue(x.dInst.src2);

                         memHazardA = !execHazardA && secondPrevInstWrite && notWriteTo0 && useA;
                         memHazardB = !execHazardB && secondPrevInstWrite && notWriteTo0 && useB;

                         writeFwd.deq;
                  end

		  // Result of Forwarding
		  let rVal1 = execHazardA ? memFwd.first.eInst.data : memHazardA ? writeFwd.first.eInst.data : x.rVal1;
		  let rVal2 = execHazardB ? memFwd.first.eInst.data : memHazardB ? writeFwd.first.eInst.data : x.rVal2;

		  ExecInst eInst = exec(x.dInst, rVal1, rVal2, x.pc, x.ppc, x.csrVal);
		  e2m.enq(Execute2Memory{eInst:eInst});

		  if(eInst.mispredict) begin
                          eEpoch <= !eEpoch;
                          execRedirect.enq(eInst.addr);
                          execRedirectToDecode.enq(eInst.addr);
                  end
	  end
  endrule

  rule doMemory(csrf.started);
	  Execute2Memory x = e2m.first; e2m.deq;
	  memFwd.enq(x);

	  case(x.eInst.iType)
                  Ld :begin
                          let d <- dMem.req(MemReq{op: Ld, addr: x.eInst.addr, data: ?});
                          x.eInst.data = d;
                  end
                  St : let d <- dMem.req(MemReq{op: St, addr: x.eInst.addr, data: x.eInst.data});
                  Unsupported :begin
                          $fwrite(stderr, "ERROR: Executing unsupported instruction\n");
                          $finish;
                  end
          endcase

	  m2w.enq(Memory2WriteBack{eInst:x.eInst});
  endrule

  rule doWriteBack(csrf.started);
	  Memory2WriteBack x = m2w.first; m2w.deq;
	  writeFwd.enq(x);

	  if (isValid(x.eInst.dst)) rf.wr(validValue(x.eInst.dst), x.eInst.data);
          csrf.wr(x.eInst.iType == Csrw ? x.eInst.csr : Invalid, x.eInst.data);
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
