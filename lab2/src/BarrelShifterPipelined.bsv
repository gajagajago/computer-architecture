import Multiplexer::*;
import FIFO::*;
import FIFOF::*;
import Vector::*;
import SpecialFIFOs::*;

/* Interface of the basic right shifter module */
interface BarrelShifterRightPipelined;
	method Action shift_request(Bit#(64) operand, Bit#(6) shamt, Bit#(1) val);
	method ActionValue#(Bit#(64)) shift_response();
endinterface

module mkBarrelShifterRightPipelined(BarrelShifterRightPipelined);
	FIFOF#( Tuple3#( Bit#(64), Bit#(6), Bit#(1) )) inFifo <- mkFIFOF;
	FIFOF#( Bit#(64)) outFifo <- mkFIFOF;
	FIFOF#( Tuple3#( Bit#(64), Bit#(6), Bit#(1) )) fifo1 <- mkFIFOF;
	FIFOF#( Tuple3#( Bit#(64), Bit#(6), Bit#(1) )) fifo2 <- mkFIFOF;
        FIFOF#( Tuple3#( Bit#(64), Bit#(6), Bit#(1) )) fifo3 <- mkFIFOF;
        FIFOF#( Tuple3#( Bit#(64), Bit#(6), Bit#(1) )) fifo4 <- mkFIFOF;
        FIFOF#( Tuple3#( Bit#(64), Bit#(6), Bit#(1) )) fifo5 <- mkFIFOF;

	rule stage1;
		let x = inFifo.first();
		let operand = multiplexer64(tpl_2(x)[0], tpl_1(x), { tpl_3(x) == 0 ? {'0} : {'1}, tpl_1(x)[63:1] });
		let shamt = tpl_2(x) >> 1;
		fifo1.enq(tuple3(operand, shamt, tpl_3(x))); inFifo.deq();
	endrule

	rule stage2;
		let x = fifo1.first();
		let operand = multiplexer64(tpl_2(x)[0], tpl_1(x), { tpl_3(x) == 0 ? {'0} : {'1}, tpl_1(x)[63:2] });
		let shamt = tpl_2(x) >> 1;
		fifo2.enq(tuple3(operand, shamt, tpl_3(x))); fifo1.deq();
	endrule

	rule stage3;
		let x = fifo2.first();
		let operand = multiplexer64(tpl_2(x)[0], tpl_1(x), { tpl_3(x) == 0 ? {'0} : {'1}, tpl_1(x)[63:4] });		
		let shamt = tpl_2(x) >> 1;
		fifo3.enq(tuple3(operand, shamt, tpl_3(x))); fifo2.deq();
	endrule

	rule stage4;
		let x = fifo3.first();
		let operand = multiplexer64(tpl_2(x)[0], tpl_1(x), { tpl_3(x) == 0 ? {'0} : {'1}, tpl_1(x)[63:8] });
		let shamt = tpl_2(x) >> 1;
		fifo4.enq(tuple3(operand, shamt, tpl_3(x))); fifo3.deq();
	endrule

	rule stage5;
		let x = fifo4.first();
		let operand = multiplexer64(tpl_2(x)[0], tpl_1(x), { tpl_3(x) == 0 ? {'0} : {'1}, tpl_1(x)[63:16] });
		let shamt = tpl_2(x) >> 1;
		fifo5.enq(tuple3(operand, shamt, tpl_3(x))); fifo4.deq();
	endrule

	rule stage6;
		let x = fifo5.first();
		let operand = multiplexer64(tpl_2(x)[0], tpl_1(x), { tpl_3(x) == 0 ? {'0} : {'1}, tpl_1(x)[63:32] });
		outFifo.enq(operand); fifo5.deq();
	endrule

	method Action shift_request(Bit#(64) operand, Bit#(6) shamt, Bit#(1) val);
		inFifo.enq(tuple3(operand, shamt, val));
	endmethod

	method ActionValue#(Bit#(64)) shift_response();
		outFifo.deq;
		return outFifo.first;
	endmethod
endmodule

interface BarrelShifterRightLogicalPipelined;
	method Action shift_request(Bit#(64) operand, Bit#(6) shamt);
	method ActionValue#(Bit#(64)) shift_response();
endinterface

typedef BarrelShifterRightLogicalPipelined BarrelShifterRightArithmeticPipelined;
typedef BarrelShifterRightLogicalPipelined BarrelShifterLeftPipelined;

module mkBarrelShifterLeftPipelined(BarrelShifterLeftPipelined);
	let bsrp <- mkBarrelShifterRightPipelined;

	method Action shift_request(Bit#(64) operand, Bit#(6) shamt);
		let lav = reverseBits(operand);
		bsrp.shift_request(lav, shamt, 1'b0);
	endmethod

	method ActionValue#(Bit#(64)) shift_response();
		let res <- bsrp.shift_response();
		return reverseBits(res);
	endmethod
endmodule

module mkBarrelShifterRightLogicalPipelined(BarrelShifterRightLogicalPipelined);
	let bsrp <- mkBarrelShifterRightPipelined;

	method Action shift_request(Bit#(64) operand, Bit#(6) shamt);
                bsrp.shift_request(operand, shamt, 1'b0);
        endmethod

        method ActionValue#(Bit#(64)) shift_response();
                let res <- bsrp.shift_response();
                return res;
        endmethod

endmodule

module mkBarrelShifterRightArithmeticPipelined(BarrelShifterRightArithmeticPipelined);
	let bsrp <- mkBarrelShifterRightPipelined;

	method Action shift_request(Bit#(64) operand, Bit#(6) shamt);
		bsrp.shift_request(operand, shamt, operand[63]);
	endmethod

	method ActionValue#(Bit#(64)) shift_response();
		let res <- bsrp.shift_response();
		return res;
	endmethod
endmodule
