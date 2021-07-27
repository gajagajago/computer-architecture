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
import Btb::*;

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
} Decode2Excute deriving (Bits, Eq);

typedef Maybe#(ExecInst) Exec2Memory;
typedef Maybe#(ExecInst) Memory2WriteBack;
typedef 4 IndexSize;

(*synthesize*)
module mkProc(Proc);
	Reg#(Addr)       pc        <- mkRegU;
	RFile            rf        <- mkBypassRFile;
	IMemory          iMem      <- mkIMemory;
	DMemory          dMem      <- mkDMemory;
	CsrFile          csrf      <- mkCsrFile;

	// Control hazard handling Elements
	Reg#(Bool)	fEpoch <- mkRegU;
	Reg#(Bool)	eEpoch <- mkRegU;

	Fifo#(1, Addr) execRedirect <- mkBypassFifo;
	Scoreboard#(4) sb <- mkPipelineScoreboard; 

	Fifo#(1, Fetch2Decode) f2d <- mkPipelineFifo;
	Fifo#(1, Decode2Excute) d2e <- mkPipelineFifo;
	Fifo#(1, Exec2Memory) e2m <- mkPipelineFifo;
	Fifo#(1, Memory2WriteBack) m2w <- mkPipelineFifo;

	Btb#(IndexSize) btb <- mkBtb;

	rule doFetch(csrf.started);
		$display("fetch");
		let inst = iMem.req(pc);
		$display("Fetch\nInst : ", showInst(inst), "\npc: %d", pc);	
		if(execRedirect.notEmpty)
		begin
			execRedirect.deq;
			fEpoch <= !fEpoch;
			pc <= execRedirect.first;
			$display("\nexec redirect not empty\nnewpc: %d", execRedirect.first);
		end
		else
		begin
			let ppc = btb.predPc(pc);
			//let ppc = pc+4;
			pc <= ppc;
			f2d.enq(Fetch2Decode{inst: inst, pc: pc, ppc: ppc, epoch: fEpoch});
			$display("\nexec redirect empty\nppc: %d", ppc);
		end
	endrule

	rule doDecode(csrf.started);
		$display("decode");
		let inst = f2d.first.inst;
		let ipc = f2d.first.pc;
		let ppc = f2d.first.ppc;
		let epoch = f2d.first.epoch;
		let dInst = decode(inst);
		Data rVal1 = isValid(dInst.src1) ? rf.rd1(validValue(dInst.src1)) : 0;
		Data rVal2 = isValid(dInst.src2) ? rf.rd2(validValue(dInst.src2)) : 0;
		let csrVal = isValid(dInst.csr) ? csrf.rd(validValue(dInst.csr)) : ?;

		let stall = (sb.search1(dInst.src1) || sb.search2(dInst.src2));
		if(!stall)
		begin
			sb.insert(dInst.dst);
			d2e.enq(Decode2Excute{dInst: dInst, pc: ipc, ppc: ppc, epoch: epoch, rVal1: rVal1, rVal2: rVal2, csrVal: csrVal});
			f2d.deq;
		end
	endrule

	rule doExecute(csrf.started);
		$display("execute");
		let x = d2e.first;
		let dInst = x.dInst;
		let ipc = x.pc;
		let ppc = x.ppc;
		let inEp = x.epoch;		
		let rVal1 = x.rVal1;
		let rVal2 = x.rVal2;
		let csrVal = x.csrVal;
		if(inEp == eEpoch)
		begin		
			let eInst = exec(dInst, rVal1, rVal2, ipc, ppc, csrVal);
			if(eInst.mispredict)
			begin
		   	 	eEpoch <= !eEpoch;
				execRedirect.enq(eInst.addr);
				$display("MisPredict, new pc : %d ", eInst.addr, " itype = ", eInst.iType, " ppc = %d", ppc);
			end

			if (eInst.iType == Br || eInst.iType == J || eInst.iType == Jr) begin
				$display("Update BTB");
				btb.update(ipc, eInst.addr);
			end

			e2m.enq(Valid(eInst));
		end
		else
		begin
			e2m.enq(Invalid);
		end
		d2e.deq;	
	endrule

	rule doMemory(csrf.started);
		$display("memory");		
		e2m.deq;		
		if(isValid(e2m.first))
		begin
			let eInst = validValue(e2m.first);	
			let iType = eInst.iType;
			
			$display("iType: ", iType);		
			case(iType)
				Ld :
				begin
					let d <- dMem.req(MemReq{op: Ld, addr: eInst.addr, data: ?});
					eInst.data = d;
				end
				St :
				begin
					let d <- dMem.req(MemReq{op: St, addr: eInst.addr, data: eInst.data});
				end
				Unsupported :
				begin
					$fwrite(stderr, "ERROR: Executing unsupported instruction\n");
					$finish;
				end
			endcase		
			m2w.enq(Valid(eInst));
		end
		else
		begin
			m2w.enq(Invalid);
		end
	endrule

	rule doWriteBack(csrf.started);
		$display("write back");
		m2w.deq;
		sb.remove;
		if(isValid(m2w.first))
		begin
			let eInst = validValue(m2w.first);
			if(isValid(eInst.dst))
			begin
				rf.wr(fromMaybe(?, eInst.dst), eInst.data);
			end
			csrf.wr(eInst.iType == Csrw ? eInst.csr : Invalid, eInst.data);
		end
	endrule

	method ActionValue#(CpuToHostData) cpuToHost;	
		let retV <- csrf.cpuToHost;
		return retV;
	endmethod

	method Action hostToCpu(Bit#(32) startpc) if (!csrf.started);
		csrf.start(0);
		pc <= startpc;
	endmethod

	interface iMemInit = iMem.init;
    interface dMemInit = dMem.init;
endmodule



































		


































































































