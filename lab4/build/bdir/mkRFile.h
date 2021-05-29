/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Tue May 18 00:47:12 KST 2021
 * 
 */

/* Generation options: */
#ifndef __mkRFile_h__
#define __mkRFile_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"


/* Class declaration for the mkRFile module */
class MOD_mkRFile : public Module {
 
 /* Clock handles */
 private:
  tClock __clk_handle_0;
 
 /* Clock gate handles */
 public:
  tUInt8 *clk_gate[0];
 
 /* Instantiation parameters */
 public:
 
 /* Module state */
 public:
  MOD_Reg<tUInt32> INST_rfile_0;
  MOD_Reg<tUInt32> INST_rfile_1;
  MOD_Reg<tUInt32> INST_rfile_10;
  MOD_Reg<tUInt32> INST_rfile_11;
  MOD_Reg<tUInt32> INST_rfile_12;
  MOD_Reg<tUInt32> INST_rfile_13;
  MOD_Reg<tUInt32> INST_rfile_14;
  MOD_Reg<tUInt32> INST_rfile_15;
  MOD_Reg<tUInt32> INST_rfile_16;
  MOD_Reg<tUInt32> INST_rfile_17;
  MOD_Reg<tUInt32> INST_rfile_18;
  MOD_Reg<tUInt32> INST_rfile_19;
  MOD_Reg<tUInt32> INST_rfile_2;
  MOD_Reg<tUInt32> INST_rfile_20;
  MOD_Reg<tUInt32> INST_rfile_21;
  MOD_Reg<tUInt32> INST_rfile_22;
  MOD_Reg<tUInt32> INST_rfile_23;
  MOD_Reg<tUInt32> INST_rfile_24;
  MOD_Reg<tUInt32> INST_rfile_25;
  MOD_Reg<tUInt32> INST_rfile_26;
  MOD_Reg<tUInt32> INST_rfile_27;
  MOD_Reg<tUInt32> INST_rfile_28;
  MOD_Reg<tUInt32> INST_rfile_29;
  MOD_Reg<tUInt32> INST_rfile_3;
  MOD_Reg<tUInt32> INST_rfile_30;
  MOD_Reg<tUInt32> INST_rfile_31;
  MOD_Reg<tUInt32> INST_rfile_4;
  MOD_Reg<tUInt32> INST_rfile_5;
  MOD_Reg<tUInt32> INST_rfile_6;
  MOD_Reg<tUInt32> INST_rfile_7;
  MOD_Reg<tUInt32> INST_rfile_8;
  MOD_Reg<tUInt32> INST_rfile_9;
 
 /* Constructor */
 public:
  MOD_mkRFile(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
  tUInt8 PORT_RST_N;
 
 /* Port definitions */
 public:
 
 /* Publicly accessible definitions */
 public:
 
 /* Local definitions */
 private:
  tUInt32 DEF__read__h1890;
  tUInt32 DEF__read__h1859;
  tUInt32 DEF__read__h1828;
  tUInt32 DEF__read__h1797;
  tUInt32 DEF__read__h1766;
  tUInt32 DEF__read__h1735;
  tUInt32 DEF__read__h1704;
  tUInt32 DEF__read__h1673;
  tUInt32 DEF__read__h1642;
  tUInt32 DEF__read__h1611;
  tUInt32 DEF__read__h1580;
  tUInt32 DEF__read__h1549;
  tUInt32 DEF__read__h1518;
  tUInt32 DEF__read__h1487;
  tUInt32 DEF__read__h1456;
  tUInt32 DEF__read__h1425;
  tUInt32 DEF__read__h1394;
  tUInt32 DEF__read__h1363;
  tUInt32 DEF__read__h1332;
  tUInt32 DEF__read__h1301;
  tUInt32 DEF__read__h1270;
  tUInt32 DEF__read__h1239;
  tUInt32 DEF__read__h1208;
  tUInt32 DEF__read__h1177;
  tUInt32 DEF__read__h1146;
  tUInt32 DEF__read__h1115;
  tUInt32 DEF__read__h1084;
  tUInt32 DEF__read__h1053;
  tUInt32 DEF__read__h1022;
  tUInt32 DEF__read__h991;
  tUInt32 DEF__read__h960;
  tUInt32 DEF__read__h929;
 
 /* Rules */
 public:
 
 /* Methods */
 public:
  void METH_wr(tUInt8 ARG_wr_rindx, tUInt32 ARG_wr_data);
  tUInt8 METH_RDY_wr();
  tUInt32 METH_rd1(tUInt8 ARG_rd1_rindx);
  tUInt8 METH_RDY_rd1();
  tUInt32 METH_rd2(tUInt8 ARG_rd2_rindx);
  tUInt8 METH_RDY_rd2();
 
 /* Reset routines */
 public:
  void reset_RST_N(tUInt8 ARG_rst_in);
 
 /* Static handles to reset routines */
 public:
 
 /* Pointers to reset fns in parent module for asserting output resets */
 private:
 
 /* Functions for the parent module to register its reset fns */
 public:
 
 /* Functions to set the elaborated clock id */
 public:
  void set_clk_0(char const *s);
 
 /* State dumping routine */
 public:
  void dump_state(unsigned int indent);
 
 /* VCD dumping routines */
 public:
  unsigned int dump_VCD_defs(unsigned int levels);
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkRFile &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkRFile &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkRFile &backing);
};

#endif /* ifndef __mkRFile_h__ */
