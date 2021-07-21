import ProcTypes::*;
import ConfigReg::*;

interface PipeReg#(type dataT);
    method Action enq(dataT d);
    method Action deq;
    method Action clear;
    method dataT first;
    method Bool notEmpty;
    method Bool notFull;
endinterface

module mkPipeReg(PipeReg#(dataT)) provisos(Bits#(dataT, dataSz));
    Reg#(dataT)   dReg <- mkRegU;
    Reg#(Bool)    vReg <- mkReg(False);
    Wire#(Bool) deqing <- mkUnsafeDWire(False);
    Wire#(Bool) enqing <- mkUnsafeDWire(False);
    Wire#(Bool) clring <- mkUnsafeDWire(False);

    rule doV;
        vReg <= clring ? False :
                !vReg && enqing ? True :
                vReg && deqing && !enqing ? False :
                vReg;
    endrule

    method Action enq(dataT d) if(!vReg || deqing);
        dReg <= d;
        enqing <= True;
    endmethod
    method Action deq if(vReg);
        deqing <= True;
    endmethod
    method Action clear;
        clring <= True;
    endmethod
    method first = dReg;
    method notEmpty = vReg;
    method notFull = !vReg || deqing;
endmodule

interface SPipeReg#(type dataT);
    method Action enq(dataT d);
    method Action deq;
    method Action clear;
    method dataT first;
    method Bool notEmpty;
    method Bool notFull;
    method Bool search(Maybe#(Rindx) s1, Maybe#(Rindx) s2);
endinterface

module mkSPipeReg#(function Maybe#(Rindx) getDst(dataT d))(SPipeReg#(dataT)) provisos(Bits#(dataT, dataSz));
    Reg#(dataT)   dReg <- mkRegU;
    Reg#(Bool)    vReg <- mkReg(False);
    Wire#(Bool) deqing <- mkUnsafeDWire(False);
    Wire#(Bool) enqing <- mkUnsafeDWire(False);
    Wire#(Bool) clring <- mkUnsafeDWire(False);

    rule doV;
        vReg <= clring ? False :
                !vReg && enqing ? True :
                vReg && deqing && !enqing ? False :
                vReg;
    endrule

    method Action enq(dataT d) if(!vReg || deqing);
        dReg <= d;
        enqing <= True;
    endmethod
    method Action deq if(vReg);
        deqing <= True;
    endmethod
    method Action clear;
        clring <= True;
    endmethod
    method first = dReg;
    method notEmpty = vReg;
    method notFull = !vReg || deqing;
    method Bool search(Maybe#(Rindx) s1, Maybe#(Rindx) s2);
        return vReg && dataHazard(s1, s2, getDst(dReg));
    endmethod
endmodule

module mkSADPipeReg#(function Maybe#(Rindx) getDst(dataT d))(SPipeReg#(dataT)) provisos(Bits#(dataT, dataSz));
    Reg#(dataT)   dReg <- mkRegU;
    Reg#(Bool)    vReg <- mkReg(False);
    Wire#(Bool) deqing <- mkDWire(False);
    Wire#(Bool) enqing <- mkDWire(False);
    Wire#(Bool) clring <- mkDWire(False);

    rule doV;
        vReg <= clring ? False :
                !vReg && enqing ? True :
                vReg && deqing && !enqing ? False :
                vReg;
    endrule

    method Action enq(dataT d) if(!vReg || deqing);
        dReg <= d;
        enqing <= True;
    endmethod
    method Action deq if(vReg);
        deqing <= True;
    endmethod
    method Action clear;
        clring <= True;
    endmethod
    method first = dReg;
    method notEmpty = vReg;
    method notFull = !vReg || deqing;
    method Bool search(Maybe#(Rindx) s1, Maybe#(Rindx) s2);
        return !deqing && vReg && dataHazard(s1, s2, getDst(dReg));
    endmethod
endmodule

interface BypassReg#(type dataT);
    method Action enq(dataT d);
    method Action deq;
    method Action clear;
    method dataT first;
    method Bool notEmpty;
    method Bool notFull;
endinterface

module mkBypassReg(BypassReg#(dataT)) provisos(Bits#(dataT, dataSz));
    Reg#(dataT)   dReg <- mkRegU;
    Reg#(Bool)    vReg <- mkReg(False);
    Wire#(dataT) dWire <- mkUnsafeDWire(?);
    Wire#(Bool) deqing <- mkUnsafeDWire(False);
    Wire#(Bool) enqing <- mkUnsafeDWire(False);
    Wire#(Bool) clring <- mkUnsafeDWire(False);

    rule doV;
        vReg <= clring ? False :
                !vReg && enqing && !deqing ? True :
                vReg && deqing ? False :
                vReg;
    endrule

    rule doD;
        dReg <= !vReg && enqing && !deqing ? dWire : dReg;
    endrule

    method Action enq(dataT d) if(!vReg);
        dWire <= d;
        enqing <= True;
    endmethod
    method Action deq if(vReg || enqing);
        deqing <= True;
    endmethod
    method Action clear;
        clring <= True;
    endmethod
    method first = !vReg ? dWire : dReg;
    method notEmpty = vReg || enqing;
    method notFull = !vReg;
endmodule
