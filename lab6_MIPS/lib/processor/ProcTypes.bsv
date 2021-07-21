import MemTypes::*;

typedef Bit#(32) Stat;

typedef Bit#(5)  Rindx;
typedef Bit#(16) Simm;
typedef Bit#(16) Zimm;
typedef Bit#(5)  Shamt;
typedef Bit#(26) Target;
typedef Bit#(5)  CP0indx;

typedef enum {Fetch, Execute} TypeStage deriving(Bits, Eq);

typedef enum {Nop, Alu, Ld, St, J, Jr, Jal, Jalr, Br, Mfc0, Mtc0} IType deriving(Bits, Eq);
typedef enum {F, T, Eq, Neq, Le, Lt, Ge, Gt} BrType deriving(Bits, Eq);
typedef enum {Add, Sub, And, Or, Xor, Nor, Slt, Sltu, LShift, RShift, Sra} AluFunc deriving(Bits, Eq);
typedef enum {Nop, RAlu, IAlu, RShift, IShift, LdSt, J, Jr, Br, Other} InstDType deriving(Bits, Eq);
typedef enum {None, Mult, Eret} Excep deriving(Bits, Eq);

typedef struct {
    IType         iType;
    AluFunc       aluFunc;
    BrType        brComp;
    Maybe#(Rindx) rDst;
    Maybe#(Rindx) rSrc1;
    Maybe#(Rindx) rSrc2;
    Bit#(32)      imm;
    Bool          immValid;
    Excep         excep;
    Bool          wrMtc0;
} DecodedInst deriving(Bits, Eq);

typedef struct {
    IType         iType;
    Maybe#(Rindx) rDst;
    Data          data;
    Addr          addr;
    Bool          brTaken;
    Bool          misprediction;
    Excep         excep;
    Bool          wrMtc0;
} ExecInst deriving(Bits, Eq);

typedef struct {
    Addr   addr;
    Bool   taken;
} RslvJmp deriving(Bits, Eq);

typedef struct {
    Addr    pc;
    Addr    ppc;
    Bool    epoch;
    Bool    eEpoch;
    MemResp inst;
} TypeFetch2Decode deriving(Bits, Eq);

typedef struct {
    Addr        pc;
    Addr        ppc;
    Bool        epoch;
    MemResp     inst;
    DecodedInst dInst;
    Data        rVal1;
    Data        rVal2;
} TypeDecode2Execute deriving(Bits, Eq);

typedef struct {
    Addr     pc;
    ExecInst eInst;
} TypeExecute2Commit deriving(Bits, Eq);

Bit#(6) opFUNC  = 6'b000000;  Bit#(6) fcSLL   = 6'b000000;
Bit#(6) opRT    = 6'b000001;  Bit#(6) fcSRL   = 6'b000010;
Bit#(6) opRS    = 6'b010000;  Bit#(6) fcSRA   = 6'b000011;
                              Bit#(6) fcSLLV  = 6'b000100;
Bit#(6) opLW    = 6'b100011;  Bit#(6) fcSRLV  = 6'b000110;
Bit#(6) opSW    = 6'b101011;  Bit#(6) fcSRAV  = 6'b000111;
                              Bit#(6) fcADDU  = 6'b100001;
Bit#(6) opADDIU = 6'b001001;  Bit#(6) fcSUBU  = 6'b100011;
Bit#(6) opSLTI  = 6'b001010;  Bit#(6) fcAND   = 6'b100100;
Bit#(6) opSLTIU = 6'b001011;  Bit#(6) fcOR    = 6'b100101;
Bit#(6) opANDI  = 6'b001100;  Bit#(6) fcXOR   = 6'b100110;
Bit#(6) opORI   = 6'b001101;  Bit#(6) fcNOR   = 6'b100111;
Bit#(6) opXORI  = 6'b001110;  Bit#(6) fcSLT   = 6'b101010;
Bit#(6) opLUI   = 6'b001111;  Bit#(6) fcSLTU  = 6'b101011;
                              Bit#(6) fcMULT  = 6'b011000;
Bit#(6) opJ     = 6'b000010;
Bit#(6) opJAL   = 6'b000011;
Bit#(6) fcJR    = 6'b001000;
Bit#(6) fcJALR  = 6'b001001;
Bit#(6) opBEQ   = 6'b000100;
Bit#(6) opBNE   = 6'b000101;
Bit#(6) opBLEZ  = 6'b000110;
Bit#(6) opBGTZ  = 6'b000111;
Bit#(5) rtBLTZ  = 5'b00000;
Bit#(5) rtBGEZ  = 5'b00001;

Bit#(5) rsMFC0  = 5'b00000;
Bit#(5) rsMTC0  = 5'b00100;
Bit#(5) rsERET  = 5'b10000;

function InstDType getInstDType(Bit#(32) inst);
    return case (inst[31:26])
        opADDIU, opSLTI, opSLTIU, opANDI, opORI, opXORI, opLUI: IAlu;
        opLW, opSW: LdSt;
        opJ, opJAL: J;
        opBEQ, opBNE, opBLEZ, opBGTZ, opRT: Br;
        opFUNC: case (inst[5:0])
            fcSLL, fcSRL, fcSRA: IShift;
            fcSLLV, fcSRLV, fcSRAV: RShift;
            fcJR, fcJALR: Jr;
            default: RAlu;
        endcase
        opRS: Other;
        default: Nop;
    endcase;
endfunction

function Bool memType( IType iType );
    return (iType==Ld || iType==St);
endfunction

function Bool regWriteType( IType iType );
    return (iType==Ld || iType==Alu || iType==Jal || iType==Jalr || iType==Mfc0);
endfunction

function Bit#(32) slt( Bit#(32) val1, Bit#(32) val2 );
    return zeroExtend( pack( signedLT(val1,val2) ) );
endfunction

function Bit#(32) sltu( Bit#(32) val1, Bit#(32) val2 );
    return zeroExtend( pack( val1 < val2 ) );
endfunction

function Bool dataHazard(Maybe#(Rindx) src1, Maybe#(Rindx) src2, Maybe#(Rindx) dst);
    return (isValid(dst) && ((isValid(src1) && unJust(dst)==unJust(src1)) ||
                             (isValid(src2) && unJust(dst)==unJust(src2))));
endfunction

function Maybe#(Rindx) getDstE(TypeDecode2Execute it);
    return it.dInst.rDst;
endfunction

function Maybe#(Rindx) getDstC(TypeExecute2Commit it);
    return it.eInst.rDst;
endfunction

typedef union tagged             
{
  struct { Rindx rbase; Rindx rdst;  Simm offset;  } LW;
  struct { Rindx rbase; Rindx rsrc;  Simm offset;  } SW; 
  struct { Rindx rbase; Rindx rdst;  Simm offset;  } LL;
  struct { Rindx rbase; Rindx rsrc;  Simm offset;  } SC; 

  struct { Rindx rsrc;  Rindx rdst;  Simm imm;     } ADDIU;
  struct { Rindx rsrc;  Rindx rdst;  Simm imm;     } SLTI;
  struct { Rindx rsrc;  Rindx rdst;  Simm imm;     } SLTIU;
  struct { Rindx rsrc;  Rindx rdst;  Zimm imm;     } ANDI;
  struct { Rindx rsrc;  Rindx rdst;  Zimm imm;     } ORI;
  struct { Rindx rsrc;  Rindx rdst;  Zimm imm;     } XORI;
  struct {              Rindx rdst;  Zimm imm;     } LUI;

  struct { Rindx rsrc;  Rindx rdst;  Shamt shamt;  } SLL;
  struct { Rindx rsrc;  Rindx rdst;  Shamt shamt;  } SRL;
  struct { Rindx rsrc;  Rindx rdst;  Shamt shamt;  } SRA;
  struct { Rindx rsrc;  Rindx rdst;  Rindx rshamt; } SLLV;
  struct { Rindx rsrc;  Rindx rdst;  Rindx rshamt; } SRLV;
  struct { Rindx rsrc;  Rindx rdst;  Rindx rshamt; } SRAV;
  struct { Rindx rsrc1; Rindx rsrc2; Rindx rdst;   } ADDU;
  struct { Rindx rsrc1; Rindx rsrc2; Rindx rdst;   } SUBU;
  struct { Rindx rsrc1; Rindx rsrc2; Rindx rdst;   } AND;
  struct { Rindx rsrc1; Rindx rsrc2; Rindx rdst;   } OR;
  struct { Rindx rsrc1; Rindx rsrc2; Rindx rdst;   } XOR;
  struct { Rindx rsrc1; Rindx rsrc2; Rindx rdst;   } NOR;
  struct { Rindx rsrc1; Rindx rsrc2; Rindx rdst;   } SLT;
  struct { Rindx rsrc1; Rindx rsrc2; Rindx rdst;   } SLTU;
  struct { Rindx rsrc1; Rindx rsrc2;               } MULT;

  struct { Target target;                          } J;
  struct { Target target;                          } JAL;
  struct { Rindx rsrc;                             } JR;
  struct { Rindx rsrc;  Rindx rdst;                } JALR;
  struct { Rindx rsrc1; Rindx rsrc2; Simm offset;  } BEQ;
  struct { Rindx rsrc1; Rindx rsrc2; Simm offset;  } BNE;
  struct { Rindx rsrc;  Simm offset;               } BLEZ;
  struct { Rindx rsrc;  Simm offset;               } BGTZ;
  struct { Rindx rsrc;  Simm offset;               } BLTZ;
  struct { Rindx rsrc;  Simm offset;               } BGEZ;

  struct { Rindx rdst;  CP0indx cop0src;           } MFC0;
  struct { Rindx rsrc;  CP0indx cop0dst;           } MTC0;
  struct {                                         } ERET; 

  void                                               ILLEGAL;
}
Instr deriving(Eq);

instance Bits#(Instr,32);
  function Bit#(32) pack( Instr instr );
    case ( instr ) matches
      tagged LW    .it : return { opLW,    it.rbase, it.rdst,  it.offset };
      tagged SW    .it : return { opSW,    it.rbase, it.rsrc,  it.offset };

      tagged ADDIU .it : return { opADDIU, it.rsrc,  it.rdst,  it.imm                      }; 
      tagged SLTI  .it : return { opSLTI,  it.rsrc,  it.rdst,  it.imm                      }; 
      tagged SLTIU .it : return { opSLTIU, it.rsrc,  it.rdst,  it.imm                      }; 
      tagged ANDI  .it : return { opANDI,  it.rsrc,  it.rdst,  it.imm                      }; 
      tagged ORI   .it : return { opORI,   it.rsrc,  it.rdst,  it.imm                      }; 
      tagged XORI  .it : return { opXORI,  it.rsrc,  it.rdst,  it.imm                      }; 
      tagged LUI   .it : return { opLUI,   5'b0,     it.rdst,  it.imm                      };

      tagged SLL   .it : return { opFUNC,  5'b0,     it.rsrc,  it.rdst,   it.shamt, fcSLL  }; 
      tagged SRL   .it : return { opFUNC,  5'b0,     it.rsrc,  it.rdst,   it.shamt, fcSRL  }; 
      tagged SRA   .it : return { opFUNC,  5'b0,     it.rsrc,  it.rdst,   it.shamt, fcSRA  }; 

      tagged SLLV  .it : return { opFUNC,  it.rshamt, it.rsrc, it.rdst,   5'b0,     fcSLLV }; 
      tagged SRLV  .it : return { opFUNC,  it.rshamt, it.rsrc, it.rdst,   5'b0,     fcSRLV }; 
      tagged SRAV  .it : return { opFUNC,  it.rshamt, it.rsrc, it.rdst,   5'b0,     fcSRAV }; 

      tagged ADDU  .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcADDU }; 
      tagged SUBU  .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcSUBU }; 
      tagged AND   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcAND  }; 
      tagged OR    .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcOR   }; 
      tagged XOR   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcXOR  }; 
      tagged NOR   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcNOR  }; 
      tagged SLT   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcSLT  }; 
      tagged SLTU  .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcSLTU }; 
      tagged MULT  .it : return { opFUNC,  it.rsrc1, it.rsrc2,    5'b0,   5'b0,     fcMULT }; 

      tagged J     .it : return { opJ,     it.target                                       }; 
      tagged JAL   .it : return { opJAL,   it.target                                       }; 
      tagged JR    .it : return { opFUNC,  it.rsrc,  5'b0,     5'b0,      5'b0,     fcJR   };
      tagged JALR  .it : return { opFUNC,  it.rsrc,  5'b0,     it.rdst,   5'b0,     fcJALR };
      tagged BEQ   .it : return { opBEQ,   it.rsrc1, it.rsrc2, it.offset                   }; 
      tagged BNE   .it : return { opBNE,   it.rsrc1, it.rsrc2, it.offset                   }; 
      tagged BLEZ  .it : return { opBLEZ,  it.rsrc,  5'b0,     it.offset                   }; 
      tagged BGTZ  .it : return { opBGTZ,  it.rsrc,  5'b0,     it.offset                   }; 
      tagged BLTZ  .it : return { opRT,    it.rsrc,  rtBLTZ,   it.offset                   }; 
      tagged BGEZ  .it : return { opRT,    it.rsrc,  rtBGEZ,   it.offset                   }; 

      tagged MFC0  .it : return { opRS,    rsMFC0,   it.rdst,  it.cop0src, 11'b0           }; 
      tagged MTC0  .it : return { opRS,    rsMTC0,   it.rsrc,  it.cop0dst, 11'b0           };  
      tagged ERET  .it : return { ?                                                        };  
    endcase
  endfunction

  function Instr unpack( Bit#(32) instrBits );
    let opcode = instrBits[ 31 : 26 ];
    let rs     = instrBits[ 25 : 21 ];
    let rt     = instrBits[ 20 : 16 ];
    let rd     = instrBits[ 15 : 11 ];
    let shamt  = instrBits[ 10 :  6 ];
    let funct  = instrBits[  5 :  0 ];
    let imm    = instrBits[ 15 :  0 ];
    let target = instrBits[ 25 :  0 ];

    case ( opcode )
      opLW        : return LW    { rbase:rs, rdst:rt,  offset:imm  };
      opSW        : return SW    { rbase:rs, rsrc:rt,  offset:imm  };

      opADDIU     : return ADDIU { rsrc:rs,  rdst:rt,  imm:imm     };
      opSLTI      : return SLTI  { rsrc:rs,  rdst:rt,  imm:imm     };
      opSLTIU     : return SLTIU { rsrc:rs,  rdst:rt,  imm:imm     };
      opANDI      : return ANDI  { rsrc:rs,  rdst:rt,  imm:imm     };
      opORI       : return ORI   { rsrc:rs,  rdst:rt,  imm:imm     };
      opXORI      : return XORI  { rsrc:rs,  rdst:rt,  imm:imm     };
      opLUI       : return LUI   {           rdst:rt,  imm:imm     };
      opJ         : return J     { target:target                   };
      opJAL       : return JAL   { target:target                   };
      opBEQ       : return BEQ   { rsrc1:rs, rsrc2:rt, offset:imm  };
      opBNE       : return BNE   { rsrc1:rs, rsrc2:rt, offset:imm  };
      opBLEZ      : return BLEZ  { rsrc:rs,  offset:imm            };
      opBGTZ      : return BGTZ  { rsrc:rs,  offset:imm            };

      opFUNC  : 
        case ( funct )
          fcSLL   : return SLL   { rsrc:rt,  rdst:rd,  shamt:shamt };
          fcSRL   : return SRL   { rsrc:rt,  rdst:rd,  shamt:shamt };
          fcSRA   : return SRA   { rsrc:rt,  rdst:rd,  shamt:shamt };
          fcSLLV  : return SLLV  { rsrc:rt,  rdst:rd,  rshamt:rs   };
          fcSRLV  : return SRLV  { rsrc:rt,  rdst:rd,  rshamt:rs   };
          fcSRAV  : return SRAV  { rsrc:rt,  rdst:rd,  rshamt:rs   };
          fcADDU  : return ADDU  { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcSUBU  : return SUBU  { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcAND   : return AND   { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcOR    : return OR    { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcXOR   : return XOR   { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcNOR   : return NOR   { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcSLT   : return SLT   { rsrc1:rs, rsrc2:rt, rdst:rd     }; 
          fcSLTU  : return SLTU  { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcMULT  : return MULT  { rsrc1:rs, rsrc2:rt              };
          fcJR    : return JR    { rsrc:rs                         };
          fcJALR  : return JALR  { rsrc:rs,  rdst:rd               };
          default : return ILLEGAL;
        endcase

      opRT : 
        case ( rt )
          rtBLTZ  : return BLTZ  { rsrc:rs,  offset:imm            };
          rtBGEZ  : return BGEZ  { rsrc:rs,  offset:imm            };
          default : return ILLEGAL;
        endcase

      opRS : 
        case ( rs )
          rsMFC0  : return MFC0  { rdst:rt,  cop0src:rd            };
          rsMTC0  : return MTC0  { rsrc:rt,  cop0dst:rd            };
          rsERET  : return ERET  {                                 };
          default : return ILLEGAL;
        endcase

      default : return ILLEGAL;
    endcase
  endfunction
endinstance

typeclass Traceable#( type item_t );
  function Action traceFull( String loc, String traceTag, item_t item );
endtypeclass

instance Traceable#(Instr);
  function Action traceFull( String loc, String ttag, Instr inst );
    case ( inst ) matches
      tagged LW    .it : $display("%s: %s lw r%0d, 0x%x(r%0d)", loc, ttag, it.rdst, it.offset, it.rbase );
      tagged SW    .it : $display("%s: %s sw r%0d, 0x%x(r%0d)", loc, ttag, it.rsrc, it.offset, it.rbase );
      tagged LL    .it : $display("%s: %s ll r%0d, 0x%x(r%0d)", loc, ttag, it.rdst, it.offset, it.rbase );
      tagged SC    .it : $display("%s: %s sc r%0d, 0x%x(r%0d)", loc, ttag, it.rsrc, it.offset, it.rbase );

      tagged ADDIU .it : $display("%s: %s addiu r%0d, r%0d, 0x%x", loc, ttag, it.rdst, it.rsrc, it.imm );
      tagged SLTI  .it : $display("%s: %s slti r%0d, r%0d, 0x%x", loc,  ttag, it.rdst, it.rsrc, it.imm );
      tagged SLTIU .it : $display("%s: %s sltiu r%0d, r%0d, 0x%x", loc, ttag, it.rdst, it.rsrc, it.imm );
      tagged ANDI  .it : $display("%s: %s andi r%0d, r%0d, 0x%x", loc,  ttag, it.rdst, it.rsrc, it.imm );
      tagged ORI   .it : $display("%s: %s ori r%0d, r%0d, 0x%x", loc,   ttag, it.rdst, it.rsrc, it.imm );
      tagged XORI  .it : $display("%s: %s xori r%0d, r%0d, 0x%x", loc,  ttag, it.rdst, it.rsrc, it.imm );
      tagged LUI   .it : $display("%s: %s lui r%0d, 0x%x", loc,         ttag, it.rdst, it.imm );
                                      
      tagged SLL   .it : $display("%s: %s sll r%0d, r%0d, %0d", loc,   ttag, it.rdst, it.rsrc, it.shamt );
      tagged SRL   .it : $display("%s: %s srl r%0d, r%0d, %0d", loc,   ttag, it.rdst, it.rsrc, it.shamt );
      tagged SRA   .it : $display("%s: %s sra r%0d, r%0d, %0d", loc,   ttag, it.rdst, it.rsrc, it.shamt );
      tagged SLLV  .it : $display("%s: %s sllv r%0d, r%0d, r%0d", loc, ttag, it.rdst, it.rsrc, it.rshamt );
      tagged SRLV  .it : $display("%s: %s srlv r%0d, r%0d, r%0d", loc, ttag, it.rdst, it.rsrc, it.rshamt );
      tagged SRAV  .it : $display("%s: %s srav r%0d, r%0d, r%0d", loc, ttag, it.rdst, it.rsrc, it.rshamt );
                                      
      tagged ADDU  .it : $display("%s: %s addu r%0d, r%0d, r%0d", loc, ttag, it.rdst, it.rsrc1, it.rsrc2 );
      tagged SUBU  .it : $display("%s: %s subu r%0d, r%0d, r%0d", loc, ttag, it.rdst, it.rsrc1, it.rsrc2 );
      tagged AND   .it : $display("%s: %s and r%0d, r%0d, r%0d", loc,  ttag, it.rdst, it.rsrc1, it.rsrc2 );
      tagged OR    .it : $display("%s: %s or r%0d, r%0d, r%0d", loc,   ttag, it.rdst, it.rsrc1, it.rsrc2 );
      tagged XOR   .it : $display("%s: %s xor r%0d, r%0d, r%0d", loc,  ttag, it.rdst, it.rsrc1, it.rsrc2 );
      tagged NOR   .it : $display("%s: %s nor r%0d, r%0d, r%0d", loc,  ttag, it.rdst, it.rsrc1, it.rsrc2 );
      tagged SLT   .it : $display("%s: %s slt r%0d, r%0d, r%0d", loc,  ttag, it.rdst, it.rsrc1, it.rsrc2 );
      tagged SLTU  .it : $display("%s: %s sltu r%0d, r%0d, r%0d", loc, ttag, it.rdst, it.rsrc1, it.rsrc2 );
      tagged MULT  .it : $display("%s: %s mult r%0d, r%0d", loc, ttag, it.rsrc1, it.rsrc2 );
                                      
      tagged J     .it : $display("%s: %s j 0x%x", loc,    ttag, it.target );
      tagged JAL   .it : $display("%s: %s jal 0x%x", loc,  ttag, it.target );
      tagged JR    .it : $display("%s: %s jr r%0d", loc,   ttag, it.rsrc );
      tagged JALR  .it : $display("%s: %s jalr r%0d", loc, ttag, it.rsrc );
      tagged BEQ   .it : $display("%s: %s beq r%0d, r%0d, 0x%x", loc, ttag, it.rsrc1, it.rsrc2, it.offset );
      tagged BNE   .it : $display("%s: %s bne r%0d, r%0d, 0x%x", loc, ttag, it.rsrc1, it.rsrc2, it.offset );
      tagged BLEZ  .it : $display("%s: %s blez r%0d, 0x%x", loc, ttag, it.rsrc, it.offset );
      tagged BGTZ  .it : $display("%s: %s bgtz r%0d, 0x%x", loc, ttag, it.rsrc, it.offset );
      tagged BLTZ  .it : $display("%s: %s bltz r%0d, 0x%x", loc, ttag, it.rsrc, it.offset );
      tagged BGEZ  .it : $display("%s: %s bgez r%0d, 0x%x", loc, ttag, it.rsrc, it.offset );
                                      
      tagged MFC0  .it : $display("%s: %s mfc0 r%0d, cpr%0d", loc, ttag, it.rdst, it.cop0src );
      tagged MTC0  .it : $display("%s: %s mtc0 r%0d, cpr%0d", loc, ttag, it.rsrc, it.cop0dst );
      tagged ERET  .it : $display("%s: %s eret", loc, ttag );

      tagged ILLEGAL   : $display("%s: %s illegal instruction", loc, ttag );
    endcase
  endfunction
endinstance
