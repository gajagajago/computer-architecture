import Types::*;
import ProcTypes::*;
import CMemTypes::*;
import RFile::*;
import IMemory::*;
import DMemory::*;
import Decode::*;
import Exec::*;
import CsrFile::*;
import Vector::*;
import Fifo::*;
import Ehr::*;
import GetPut::*;

typedef struct {
  Instruction inst;
  Addr pc;
  Addr ppc;
  Bool epoch;
} Fetch2Decode deriving(Bits, Eq);

typedef struct {
	DecodedInst dInst;
	Bool epoch;
	Addr pc;
	Addr ppc;
} Decode2Execute deriving(Bits, Eq);

(*synthesize*)
module mkProc(Proc);
  Reg#(Addr)    pc  <- mkRegU;
  RFile         rf  <- mkRFile;
  IMemory     iMem  <- mkIMemory;
  DMemory     dMem  <- mkDMemory;
  CsrFile     csrf <- mkCsrFile;

  Reg#(ProcStatus)   stat		<- mkRegU;
  Fifo#(1,ProcStatus) statRedirect <- mkBypassFifo;

  // Control hazard handling Elements
  Reg#(Bool) fEpoch <- mkRegU;
  Reg#(Bool) eEpoch <- mkRegU;

  Fifo#(1, Addr)         execRedirect <- mkBypassFifo;

  Fifo#(2, Fetch2Decode)  f2d <- mkPipelineFifo;
  Fifo#(2, Decode2Execute) d2e <- mkPipelineFifo; 

  Bool memReady = iMem.init.done() && dMem.init.done();
  rule test (!memReady);
    let e = tagged InitDone;
    iMem.init.request.put(e);
    dMem.init.request.put(e);
  endrule


  rule doFetch(csrf.started && stat == AOK);
	/* Exercise_2 */
	/*TODO: 
	Remove 1-cycle inefficiency when execRedirect is used. */
   	let inst = iMem.req(pc);
   	let ppc = pc + 4;

    if(execRedirect.notEmpty) begin
      execRedirect.deq;
      pc <= execRedirect.first;
      fEpoch <= !fEpoch;
    end
    else begin
      pc <= ppc;
      f2d.enq(Fetch2Decode{inst:inst, pc:pc, ppc:ppc, epoch:fEpoch});
    end
    $display("Fetch : from Pc %d , \n", pc);
  endrule

  rule doDecode(csrf.started && stat == AOK);
	let x = f2d.first; f2d.deq;
	if (x.epoch == eEpoch) begin
		let dInst = decode(x.inst);
		d2e.enq(Decode2Execute{dInst: dInst, epoch: x.epoch, pc: x.pc, ppc: x.ppc});
	end
  endrule

  rule doRest(csrf.started && stat == AOK);
	/* Exercise_1 */
	/* TODO: 
	Divide the doRest rule into doDecode, doRest rules 
	to implement 3-stage pipelined processor */
	let x = d2e.first;
    	let dInst   = x.dInst;
    	let pc   = x.pc;
    	let ppc    = x.ppc;
    	let iEpoch = x.epoch;
    	d2e.deq;

    if(iEpoch == eEpoch) begin
        // Register Read 
        let rVal1 = isValid(dInst.src1) ? rf.rd1(validValue(dInst.src1)) : ?;
        let rVal2 = isValid(dInst.src2) ? rf.rd2(validValue(dInst.src2)) : ?;
        let csrVal = isValid(dInst.csr) ? csrf.rd(validValue(dInst.csr)) : ?;

    	// Execute         
        let eInst = exec(dInst, rVal1, rVal2, pc, ppc, csrVal);               

        if(eInst.mispredict) begin
          eEpoch <= !eEpoch;
          execRedirect.enq(eInst.addr);
          $display("jump! :mispredicted, address %d ", eInst.addr);
        end

      //Memory 
      let iType = eInst.iType;
      case(iType)
        Ld :
        begin
          let d <- dMem.req(MemReq{op: Ld, addr: eInst.addr, data: ?});
          eInst.data = d;
        end

        St:
        begin
          let d <- dMem.req(MemReq{op: St, addr: eInst.addr, data: eInst.data});
        end
        Unsupported :
        begin
          $fwrite(stderr, "ERROR: Executing unsupported instruction\n");
          $finish;
        end
      endcase

      //WriteBack 
      if (isValid(eInst.dst)) begin
          rf.wr(fromMaybe(?, eInst.dst), eInst.data);
      end

      // Csrf
      csrf.wr(eInst.iType == Csrw ? eInst.csr : Invalid, eInst.data);

      case(iType)
                J: csrf.incInstTypeCnt(J_);
                Jr: csrf.incInstTypeCnt(Jr_);
                Br: csrf.incInstTypeCnt(Br_);
                Ld, St : csrf.incInstTypeCnt(Mem);
      endcase

      if(eInst.mispredict) begin
              csrf.incBPMissCnt;
      	      case(iType)
              	      J: csrf.incMissInstTypeCnt(J_);
		      Jr: csrf.incMissInstTypeCnt(Jr_);
		      Br: csrf.incMissInstTypeCnt(Br_);
	      endcase
      end
  end
  endrule

  rule upd_Stat(csrf.started);
	$display("Stat update");
  	statRedirect.deq;
    stat <= statRedirect.first;
  endrule

  method ActionValue#(CpuToHostData) cpuToHost;
    let retV <- csrf.cpuToHost;
    return retV;
  endmethod

  method Action hostToCpu(Bit#(32) startpc) if (!csrf.started && memReady);
    csrf.start(0);
    eEpoch <= False;
    fEpoch <= False;
    pc <= startpc;
    stat <= AOK;
  endmethod

  interface iMemInit = iMem.init;
  interface dMemInit = dMem.init;

endmodule
