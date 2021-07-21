import RegFile::*;
import MemTypes::*;

interface IMemory;
    method MemResp req(Addr a);
endinterface

(* synthesize *)
module mkIMemory(IMemory);
    RegFile#(Bit#(26), Bit#(32)) mem <- mkRegFileFullLoad("code.mem");

    method MemResp req(Addr a);
        return mem.sub(truncate(a>>2));
    endmethod
endmodule

