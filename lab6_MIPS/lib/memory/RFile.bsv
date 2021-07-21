import Vector::*;
import ConfigReg::*;
import ProcTypes::*;
import MemTypes::*;
import EHR::*;

interface RFile;
    method Action wr( Rindx rindx, Data data );
    method Data rd1( Rindx rindx );
    method Data rd2( Rindx rindx );
endinterface

(* synthesize *)
module mkRFile( RFile );
    Vector#(32, Reg#(Data)) rfile <- replicateM(mkReg(0));

    function Data read(Rindx rindx);
        return rfile[rindx];
    endfunction
   
    method Action wr( Rindx rindx, Data data );
        if(rindx!=0) rfile[rindx] <= data;
    endmethod

    method Data rd1( Rindx rindx ) = read(rindx);
    method Data rd2( Rindx rindx ) = read(rindx);
endmodule

(* synthesize *)
module mkConfigRFile( RFile );
    Vector#(32, Reg#(Data)) rfile <- replicateM(mkConfigReg(0));

    function Data read(Rindx rindx);
        return rfile[rindx];
    endfunction
   
    method Action wr( Rindx rindx, Data data );
        if(rindx!=0) rfile[rindx] <= data;
    endmethod

    method Data rd1( Rindx rindx ) = read(rindx);
    method Data rd2( Rindx rindx ) = read(rindx);
endmodule

typedef struct{
    Rindx i;
    Data  d;
} TypeRFWr deriving(Bits, Eq);

(* synthesize *)
module mkBypassRFile( RFile );
    Vector#(32, Reg#(Data)) rfile <- replicateM(mkConfigReg(0));
    Wire#(Maybe#(TypeRFWr)) wrWire <- mkDWire(Invalid);

    Bool wrValid = isValid(wrWire);
    TypeRFWr wrValue = unJust(wrWire);

    method Action wr( Rindx rindx, Data data );
        wrWire <= Valid(TypeRFWr{i:rindx, d:data});
        if(rindx!=0) rfile[rindx] <= data;
    endmethod

    method Data rd1( Rindx rindx );
        return wrValid && wrValue.i==rindx ? wrValue.d : rfile[rindx];
    endmethod

    method Data rd2( Rindx rindx );
        return wrValid && wrValue.i==rindx ? wrValue.d : rfile[rindx];
    endmethod
endmodule

(* synthesize *)
module mkEHRBypassRFile( RFile );
    Vector#(32, EHR#(2, Data)) rfile <- replicateM(mkEHR(0));

    method Action wr( Rindx rindx, Data data );
        if(rindx!=0) rfile[rindx][0] <= data;
    endmethod

    method Data rd1( Rindx rindx );
        return rfile[rindx][1];
    endmethod

    method Data rd2( Rindx rindx );
        return rfile[rindx][1];
    endmethod
endmodule
