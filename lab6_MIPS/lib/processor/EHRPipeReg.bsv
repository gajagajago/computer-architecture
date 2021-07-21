import ProcTypes::*;
import EHR::*;

interface EHRPipeReg#(type dataT);
    method Action enq(dataT d);
    method Action deq;
    method Action clear;
    method dataT first;
    method Bool notEmpty;
    method Bool notFull;
endinterface

module mkEHRPipeReg(EHRPipeReg#(dataT)) provisos(Bits#(dataT, dataSz));
    Reg#(dataT)   dReg <- mkRegU;
    EHR#(3, Bool) vReg <- mkEHR(False);

    method Action enq(dataT d) if(!vReg[0] || !vReg[1]);
        dReg <= d;
        vReg[1] <= True;
    endmethod
    method Action deq if(vReg[0]);
        vReg[0] <= False;
    endmethod
    method Action clear;
        vReg[2] <= False;
    endmethod
    method first = dReg;
    method notEmpty = vReg[0];
    method notFull = !vReg[0] || !vReg[1];
endmodule

interface EHRBypassReg#(type dataT);
    method Action enq(dataT d);
    method Action deq;
    method Action clear;
    method dataT first;
    method Bool notEmpty;
    method Bool notFull;
endinterface

module mkEHRBypassReg(EHRBypassReg#(dataT)) provisos(Bits#(dataT, dataSz));
    EHR#(4, dataT) dReg <- mkEHRU;
    EHR#(4, Bool)  vReg <- mkEHR(False);

    method Action enq(dataT d) if(!vReg[0]);
        dReg[0] <= d;
        vReg[0] <= True;
    endmethod
    method Action deq if(vReg[1]);
        vReg[1] <= False;
    endmethod
    method Action clear;
        vReg[2] <= False;
    endmethod
    method first = dReg[1];
    method notEmpty = vReg[1];
    method notFull = !vReg[0];
endmodule

interface EHRSPipeReg#(type dataT);
    method Action enq(dataT d);
    method Action deq;
    method Action clear;
    method dataT first;
    method Bool notEmpty;
    method Bool notFull;
    method ActionValue#(Bool) search(Maybe#(Rindx) s1, Maybe#(Rindx) s2);
endinterface

module mkEHRSPipeReg#(function Maybe#(Rindx) getDst(dataT d))(EHRSPipeReg#(dataT)) provisos(Bits#(dataT, dataSz));
    EHRPipeReg#(dataT)   pQ <- mkEHRPipeReg;
    EHRBypassReg#(dataT) sQ <- mkEHRBypassReg;

    method Action enq(dataT d) if(pQ.notFull);
        pQ.enq(d);
    endmethod
    method Action deq if(pQ.notEmpty);
        pQ.deq;
        sQ.enq(pQ.first);
    endmethod
    method Action clear;
        pQ.clear;
    endmethod
    method first = pQ.first;
    method notEmpty = pQ.notEmpty;
    method notFull = pQ.notFull;
    method ActionValue#(Bool) search(Maybe#(Rindx) s1, Maybe#(Rindx) s2);
        if(sQ.notEmpty) begin
            sQ.deq;
            return dataHazard(s1, s2, getDst(sQ.first));
        end
		else begin
            return False;
		end
    endmethod
endmodule
