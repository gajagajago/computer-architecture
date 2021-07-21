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
} Decode2Excute deriving (Bits, Eq);

typedef Maybe#(ExecInst) Exec2Memory;
typedef Maybe#(ExecInst) Memory2WriteBack;

typedef struct {
	Bool isLoad;
	RIndx dst;
	Data dstValue;
} MemRedirect deriving (Bits, Eq);

typedef struct {
	RIndx dst;
	Data dstValue;
} WBRedirect deriving (Bits, Eq);

(*synthesize*)
module mkProc(Proc);
	Reg#(Addr)       pc        <- mkRegU;
	RFile            rf        <- mkBypassRFile;
	IMemory          iMem      <- mkIMemory;
	DMemory          dMem      <- mkDMemory;
	CsrFile          csrf      <- mkCsrFile;

	Bool memReady = iMem.init.done() && dMem.init.done();
    rule test (!memReady);
		let e = tagged InitDone;
		iMem.init.request.put(e);
		dMem.init.request.put(e);
    endrule

	Reg#(Bool)	fEpoch <- mkRegU;
	Reg#(Bool)	eEpoch <- mkRegU;

	Reg#(Data) val1 <- mkRegU;
	Reg#(Data) val2 <- mkRegU;  
	Reg#(Bit#(1)) stalled <- mkRegU;

	Fifo#(1, Addr) execRedirect <- mkBypassFifo;

	Fifo#(1, Fetch2Decode) f2d <- mkPipelineFifo;
	Fifo#(1, Decode2Excute) d2e <- mkPipelineFifo;
	Fifo#(1, Exec2Memory) e2m <- mkPipelineFifo;
	Fifo#(1, Memory2WriteBack) m2w <- mkPipelineFifo;

	Fifo#(1, MemRedirect) memRedirect <- mkBypassFifo;
	Fifo#(1, WBRedirect) wbRedirect <- mkBypassFifo;

	rule doFetch(csrf.started);			
		let inst = iMem.req(pc);
		if(execRedirect.notEmpty)
		begin
			execRedirect.deq;
			fEpoch <= !fEpoch;
			pc <= execRedirect.first;
		end
		else
		begin
			let ppc = pc + 4;
			pc <= ppc;
			f2d.enq(Fetch2Decode{inst: inst, pc: pc, ppc: ppc, epoch: fEpoch});
		end	
	endrule

	rule doDecode(csrf.started);
		let inst = f2d.first.inst;
		let ipc = f2d.first.pc;
		let ppc = f2d.first.ppc;
		let epoch = f2d.first.epoch;
		let dInst = decode(inst);

		Data rVal1 = isValid(dInst.src1) ? rf.rd1(validValue(dInst.src1)) : 0;
		Data rVal2 = isValid(dInst.src2) ? rf.rd2(validValue(dInst.src2)) : 0;
		let csrVal = isValid(dInst.csr) ? csrf.rd(validValue(dInst.csr)) : ?;

		d2e.enq(Decode2Excute{dInst: dInst, pc: ipc, ppc: ppc, epoch: epoch, rVal1: rVal1, rVal2: rVal2, csrVal: csrVal});
		f2d.deq;	
	endrule

	rule doExcute(csrf.started);
		let x = d2e.first;
		let dInst = x.dInst;
		let ipc = x.pc;
		let ppc = x.ppc;
		let inEp = x.epoch;		

		let rVal1 = x.rVal1;
		let rVal2 = x.rVal2;
		if(stalled == 1)
		begin
			rVal1 = val1;
			rVal2 = val2;
		end

		let csrVal = x.csrVal;
		let stall = False;

		let src1 = dInst.src1;
		let src2 = dInst.src2;

    	let redirect_flag = 0; //For debug

		if(wbRedirect.notEmpty)
		begin
			let redirect_w = wbRedirect.first;
			if(isValid(src1))
			begin
				if(redirect_w.dst == validValue(src1))
				begin
					if(inEp == eEpoch)									
					rVal1 = redirect_w.dstValue;
				end
			end
			if(isValid(src2))
			begin
				if(redirect_w.dst == validValue(src2))
				begin
					if(inEp == eEpoch)							
					rVal2 = redirect_w.dstValue;
				end
			end
			wbRedirect.deq;
			end
		if(memRedirect.notEmpty)
		begin
			let redirect_m = memRedirect.first;
			if(isValid(src1))
			begin
				if(redirect_m.dst == validValue(src1))
				begin
					if(inEp == eEpoch)				
					rVal1 = redirect_m.dstValue;
					if(redirect_m.isLoad)
					begin
						stall = True;
					end
				end					
			end
			if(isValid(src2))
			begin
				if(redirect_m.dst == validValue(src2))
				begin
					if(inEp == eEpoch)									
					rVal2 = redirect_m.dstValue;
					if(redirect_m.isLoad)
					begin
						stall = True;
					end
				end	
			end
			memRedirect.deq;
		end	

		if(inEp == eEpoch)
		begin			
			if(!stall)
			begin
				let eInst = exec(dInst, rVal1, rVal2, ipc, ppc, csrVal);			
				if(eInst.mispredict)
				begin
		   	 		eEpoch <= !eEpoch;
	    			execRedirect.enq(eInst.addr);			
				end
				d2e.deq;
				e2m.enq(Valid(eInst));		
				stalled <= 0;		
			end	
			else
			begin
				stalled <= 1;
				val1 <= rVal1;
				val2 <= rVal2;
			end
		end
		else
		begin
			e2m.enq(Invalid);
			d2e.deq;		
		end
	endrule

	rule doMemory(csrf.started);			
		e2m.deq;			
		if(isValid(e2m.first))
		begin
			let eInst = validValue(e2m.first);	
			let iType = eInst.iType;		

			if(iType == Ld)
			begin
				memRedirect.enq(MemRedirect{isLoad: True,dst: validValue(eInst.dst),dstValue: 0});
			end
			else if(isValid(eInst.dst))
			begin
				memRedirect.enq(MemRedirect{isLoad: False,dst: validValue(eInst.dst),dstValue: eInst.data});
			end			

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
					$fwrite(stderr, "ERROR: Executing unsupported instruction : Memory Stage\n");
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
		m2w.deq;
		if(isValid(m2w.first))
		begin
			let eInst = validValue(m2w.first);
			if(isValid(eInst.dst))
			begin
				wbRedirect.enq(WBRedirect{dst: validValue(eInst.dst),dstValue: eInst.data});
				rf.wr(fromMaybe(?, eInst.dst), eInst.data);
			end
			csrf.wr(eInst.iType == Csrw ? eInst.csr : Invalid, eInst.data);
		end
	endrule

	method ActionValue#(CpuToHostData) cpuToHost;	
		let retV <- csrf.cpuToHost;
		return retV;
	endmethod

	method Action hostToCpu(Bit#(32) startpc) if (!csrf.started && memReady);
		csrf.start(0);
		pc <= startpc;
		val1 <= 0;
		val2 <= 0;
		stalled <= 0;
	endmethod

	interface iMemInit = iMem.init;
    interface dMemInit = dMem.init;
endmodule



































		


































































































