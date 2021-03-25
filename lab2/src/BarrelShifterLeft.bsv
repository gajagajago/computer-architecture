import BarrelShifterRight::*;

interface BarrelShifterLeft;
	method ActionValue#(Bit#(64)) leftShift(Bit#(64) val, Bit#(6) shiftAmt);
endinterface

module mkBarrelShifterLeft(BarrelShifterLeft);
	let bsr <- mkBarrelShifterRightLogical;
	method ActionValue#(Bit#(64)) leftShift(Bit#(64) val, Bit#(6) shiftAmt);
		let lav = reverseBits(val);
		let shifted_lav <- bsr.rightShift(lav, shiftAmt);
		let res = reverseBits(shifted_lav);

		return res;
	endmethod
endmodule
