import Multiplexer::*;

interface BarrelShifterRight;
  method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt, Bit#(1) shiftValue);
endinterface

module mkBarrelShifterRight(BarrelShifterRight);

  	method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt, Bit#(1) shiftValue);

		Bit#(64) temp;

		for (Integer i = 0; i < 6; i = i + 1) begin
			case(i)
				0: temp = { shiftValue == 0 ? {'0} : {'1}, val[63:1] };
				1: temp = { shiftValue == 0 ? {'0} : {'1}, val[63:2] };
				2: temp = { shiftValue == 0 ? {'0} : {'1}, val[63:4] };
                                3: temp = { shiftValue == 0 ? {'0} : {'1}, val[63:8] };
                                4: temp = { shiftValue == 0 ? {'0} : {'1}, val[63:16] };
                                5: temp = { shiftValue == 0 ? {'0} : {'1}, val[63:32] };
			endcase

			val = multiplexer64(shiftAmt[i], val, temp);
		end

		return val;
  	endmethod
endmodule

interface BarrelShifterRightLogical;
  method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt);
endinterface

module mkBarrelShifterRightLogical(BarrelShifterRightLogical);
	let bsr <- mkBarrelShifterRight;
       
	method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt);
		let result <- bsr.rightShift(val, shiftAmt, 0);
		return result;
	endmethod
endmodule

typedef BarrelShifterRightLogical BarrelShifterRightArithmetic;

module mkBarrelShifterRightArithmetic(BarrelShifterRightArithmetic);
	let bsr <- mkBarrelShifterRight;
	
	method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt);
		let result <- bsr.rightShift(val, shiftAmt, val[63]);
		return result;
	endmethod
endmodule
