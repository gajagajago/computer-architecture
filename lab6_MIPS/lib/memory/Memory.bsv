import RegFile::*;
import MemTypes::*;

typedef 4 Latency;
interface Memory;
    method Action iReq(Addr r);
    method ActionValue#(MemResp) iResp;

    method Action dReq(MemReq r);
    method ActionValue#(MemResp) dResp;
endinterface

(* synthesize *)
module mkMemory(Memory);
    RegFile#(Bit#(26), Bit#(32)) iMem <- mkRegFileFullLoad("code.mem");
    RegFile#(Bit#(26), Bit#(32)) dMem <- mkRegFileFullLoad("code.mem");

    Reg#(MemResp) iRespReg <- mkRegU;
    Reg#(Bit#(4))    iWait <- mkReg(0);
    Reg#(MemResp) dRespReg <- mkRegU;
    Reg#(Bit#(4))    dWait <- mkReg(0);
    
    rule doI(iWait!=0 && iWait!=fromInteger(valueOf(Latency)));
        iWait <= iWait + 1;
    endrule

    rule doD(dWait!=0 && dWait!=fromInteger(valueOf(Latency)));
        dWait <= dWait + 1;
    endrule

    method Action iReq(Addr r) if(iWait==0);
        iRespReg <= iMem.sub(truncate(r>>2));
        iWait <= 1;
    endmethod
    method ActionValue#(MemResp) iResp if(iWait==fromInteger(valueOf(Latency)));
        iWait <= 0;
        return iRespReg;
    endmethod

    method Action dReq(MemReq r) if(dWait==0);
        if(r.op==St)
            dMem.upd(truncate(r.addr>>2), r.data);
        dRespReg <= dMem.sub(truncate(r.addr>>2));
        dWait <= 1;
    endmethod
    method ActionValue#(MemResp) dResp if(dWait==fromInteger(valueOf(Latency)));
        dWait <= 0;
        return dRespReg;
    endmethod
endmodule

