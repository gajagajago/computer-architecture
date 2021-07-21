import RegFile::*;
import MemTypes::*;

interface DMemory;
    method ActionValue#(MemResp) req(MemReq r);
endinterface

(* synthesize *)
module mkDMemory(DMemory);
    RegFile#(Bit#(26), Bit#(32)) mem <- mkRegFileFullLoad("code.mem");

    method ActionValue#(MemResp) req(MemReq r);
        if(r.op==St)
            mem.upd(truncate(r.addr>>2), r.data);
        return mem.sub(truncate(r.addr>>2));
    endmethod
endmodule

