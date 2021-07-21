import ProcTypes::*;
import MemTypes::*;
import RFile::*;
import IMemory::*;
import DMemory::*;
import Decode::*;
import Exec::*;
import EHRPipeReg::*;
import EHR::*;

interface Proc;
	method Bit#(32) cpuToHost(Bit#(2) req);
	method Action hostToCpu(Bit#(32) startpc);
endinterface

(* synthesize *)
module mkProc(Proc);
	Reg#(Addr)  pc    <- mkRegU;
	RFile       rf    <- mkEHRBypassRFile;
	IMemory   iMem    <- mkIMemory;
	DMemory   dMem    <- mkDMemory;

	Reg#(Bool) fEpoch <- mkReg(False);
	Reg#(Bool) eEpoch <- mkReg(False);

	EHRSPipeReg#(TypeDecode2Execute) itr <- mkEHRSPipeReg(getDstE);
	EHRSPipeReg#(TypeExecute2Commit)  cr <- mkEHRSPipeReg(getDstC);

	EHRBypassReg#(Addr) nextPC <- mkEHRBypassReg;

	//For Host-CPU communication. Don't touch these.
	EHR#(2, Data)    cp0_tohost <- mkEHR(0);
	Reg#(Bool)     cp0_fromhost <- mkReg(False);
	EHR#(2, Bool)   cp0_statsEn <- mkEHR(False);
	Reg#(Stat)       num_cycles <- mkReg(0);
	Reg#(Stat)         num_inst <- mkReg(0);

	rule doFetch(cp0_fromhost && itr.notFull);
		/* 
			Things to do

			instruction fetching
		   	instruction decoding
			register read
			enqueue the data to pipeline register
			determine the next pc
			pc & epoch handling for miss prediction
		*/
    endrule

	rule doExecute(itr.notEmpty && cr.notFull);
		/*
		Things to do: When epochs are same each other,

		execute
		memory operation
		enqueue the data to pipeline register
		next pc & epoch handling for miss prediction 
		host-cpu communication
		*/

	   	//For host-cpu communication. 
		if(eInst.iType==Mtc0) begin
			if(eInst.wrMtc0) begin
				cp0_statsEn[0] <= unpack(truncate(rVal2));
			end
			else begin
				cp0_tohost[0] <= rVal2;
			end
		end
		if(cp0_statsEn[0]) begin
			num_inst <= num_inst + 1;
		end
	endrule

	rule doCommit(cr.notEmpty);
		/*
			Things to do: when instruction is write type,

			Write the data on the register file
		*/
	endrule

	//Don't touch below here
	rule doCyc(cp0_statsEn[0]);
		num_cycles <= num_cycles+1;
	endrule

	method Bit#(32) cpuToHost(Bit#(2) req);
		return (case (req)
		0: cp0_tohost[0];
		1: pc;
		2: num_inst;
		3: num_cycles;
		endcase);
	endmethod
	method Action hostToCpu(Bit#(32) startpc);
		cp0_fromhost <= True;
		pc <= startpc;
	endmethod
endmodule

