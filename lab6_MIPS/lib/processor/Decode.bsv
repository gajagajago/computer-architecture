import ProcTypes::*;
import MemTypes::*;

function DecodedInst decode(Data inst, Bool cp0_statsEn, Bool cp0_fromhost, Data cp0_tohost);
    DecodedInst dInst = unpack(0);
	let opcode = inst[ 31 : 26 ];
	let rs     = inst[ 25 : 21 ];
	let rt     = inst[ 20 : 16 ];
	let rd     = inst[ 15 : 11 ];
	let shamt  = inst[ 10 :  6 ];
	let funct  = inst[  5 :  0 ];
	let imm    = inst[ 15 :  0 ];
	let target = inst[ 25 :  0 ];

    case ( getInstDType(inst) )
        LdSt: begin
            dInst.iType = opcode==opLW ? Ld : St;
            dInst.aluFunc = Add;
            dInst.rDst  = Valid(rt);
            dInst.rSrc1 = Valid(rs);
            dInst.rSrc2 = Valid(rt);
            dInst.imm = signExtend(imm);
		    dInst.immValid = True;
		end

        IAlu: begin
            dInst.iType = Alu;
            dInst.aluFunc = case (opcode)
                opADDIU, opLUI: Add;
                opSLTI: Slt;
                opSLTIU: Sltu;
                opANDI: And;
                opORI: Or;
                opXORI: Xor;
            endcase;
            dInst.rDst  = Valid(rt);
            dInst.rSrc1 = Valid(rs);
            dInst.imm = case (opcode)
                opADDIU, opSLTI, opSLTIU: signExtend(imm);
                opLUI: {imm, 16'b0};
                default: zeroExtend(imm);
            endcase;
            dInst.immValid = True;
        end

        IShift: begin
            dInst.iType = Alu;
            dInst.aluFunc = case (funct)
                fcSLL: LShift;
                fcSRL: RShift;
                fcSRA: Sra;
            endcase;
            dInst.rDst  = Valid(rd);
            dInst.rSrc1 = Valid(rt);
            dInst.imm = zeroExtend(shamt);
            dInst.immValid = True;
        end

        RShift: begin
            dInst.iType = Alu;
            dInst.aluFunc = case (funct)
                fcSLLV: LShift;
                fcSRLV: RShift;
                fcSRAV: Sra;
            endcase;
            dInst.rDst  = Valid(rd);
            dInst.rSrc1 = Valid(rt);
            dInst.rSrc2 = Valid(rs);
            dInst.immValid = False;
        end

        RAlu: begin
            dInst.iType = funct==fcMULT ? Nop : Alu;
            dInst.aluFunc = case (funct)
                fcADDU: Add;
                fcSUBU: Sub;
                fcAND : And;
                fcOR  : Or;
                fcXOR : Xor;
                fcNOR : Nor;
                fcSLT : Slt;
                fcSLTU: Sltu;
                fcMULT: ?;
            endcase;
            dInst.rDst  = Valid(rd);
            dInst.rSrc1 = Valid(rs);
            dInst.rSrc2 = Valid(rt);
            dInst.immValid = False;
            dInst.excep = funct==fcMULT ? Mult : None;
        end
        
        J: begin
            dInst.iType = opcode==opJ ? J : Jal;
            dInst.brComp = T;
            dInst.rDst  = Valid(31);
            dInst.imm = zeroExtend({target,2'b00});
            dInst.immValid = False;
        end

        Jr: begin
            dInst.iType = funct==fcJR ? Jr : Jalr;
            dInst.brComp = T;
            dInst.rDst  = Valid(rd);
            dInst.rSrc1 = Valid(rs);
            dInst.immValid = False;
        end

        Br: begin
            dInst.iType = Br;
            dInst.brComp = case(opcode)
                opBEQ: Eq;
                opBNE: Neq;
                opBLEZ: Le;
                opBGTZ: Gt;
                opRT: (rt==rtBLTZ ? Lt : Ge);
            endcase;
            dInst.rSrc1 = Valid(rs);
            dInst.rSrc2 = Valid(rt);
            dInst.imm = signExtend(imm) << 2;
            dInst.immValid = False;
        end

        Other: begin
            dInst.iType = rs==rsERET ? Nop : rs==rsMFC0 ? Mfc0 : Mtc0;
            dInst.aluFunc = Add;
            dInst.rDst  = Valid(rt);
            dInst.rSrc1 = Valid(rs);
            dInst.rSrc2 = Valid(rt);
            dInst.imm =  case ( rd )
                5'd10 : zeroExtend(pack(cp0_statsEn));
                5'd20 : zeroExtend(pack(cp0_fromhost));
                5'd21 : cp0_tohost;
            endcase;
            dInst.immValid = True;
            dInst.excep = rs==rsERET ? Eret : None;
            dInst.wrMtc0 = rd==5'd10;
        end
        
        default: dInst.iType = Nop;
    endcase

    return dInst;
endfunction
