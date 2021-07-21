import ProcTypes::*;
import MemTypes::*;

function Data alu(Data a, Data b, AluFunc func);
    Data res = case(func)
        Add   : (a + b);
        Sub   : (a - b);
        And   : (a & b);
        Or    : (a | b);
        Xor   : (a ^ b);
        Nor   : ~(a | b);
        Slt   : slt(a, b);
        Sltu  : sltu(a, b);
        LShift: (a << b[4:0]);
        RShift: (a >> b[4:0]);
        Sra   : signedShiftRight(a, b[4:0]);
    endcase;
    return res;
endfunction

function Bool aluBr(Data a, Data b, BrType brComp);
	Bool brTaken = case(brComp)
	    Eq     : (a == b);
	    Neq    : (a != b);
	    Le     : signedLE(a, 0);
	    Lt     : signedLT(a, 0);
	    Ge     : signedGE(a, 0);
	    Gt     : signedGT(a, 0);
	    T      : True;
	    default: False;
	endcase;
  return brTaken;
endfunction

function Addr brAddrCalc(Addr pc, Data val, IType iType, Data imm);
    Addr targetAddr = case (iType)
        J, Jal   : {pc[31:28], imm[27:0]};
        Jr, Jalr : val;
        default  : (pc + imm);
    endcase;
    return targetAddr;
endfunction

function ExecInst exec(DecodedInst dInst, Data rVal1, Data rVal2, Addr pc, Addr ppc, Addr epc);
    ExecInst eInst = ?;
    Data aluVal2 = dInst.immValid ? dInst.imm : rVal2;

    let aluRes = alu(rVal1, aluVal2, dInst.aluFunc);
    let brAddr = brAddrCalc((pc+4), rVal1, dInst.iType, dInst.imm);

    eInst.iType = dInst.iType;
    eInst.addr = dInst.excep==Eret ? epc : dInst.excep==Mult ? 32'h1010 : memType(dInst.iType) ? aluRes : brAddr;
    eInst.data = dInst.iType==St ? rVal2 : (dInst.iType==Jal || dInst.iType==Jalr) ? (pc+4) : aluRes;
    eInst.brTaken = aluBr(rVal1, aluVal2, dInst.brComp);
    eInst.misprediction = eInst.brTaken ? brAddr!=ppc : (pc+4)!=ppc;
    eInst.rDst = dInst.rDst;
    eInst.excep = dInst.excep;
    eInst.wrMtc0 = dInst.wrMtc0;

    return eInst;
endfunction

function RslvJmp resolveJump(DecodedInst dInst, Data val, Addr pc);
    RslvJmp rJmp = ?;

    rJmp.addr = case (dInst.iType)
        J, Jal  : {pc[31:28], dInst.imm[27:0]};
        Jr, Jalr: val;
    endcase;
    rJmp.taken = dInst.brComp==T;
    
    return rJmp;
endfunction
