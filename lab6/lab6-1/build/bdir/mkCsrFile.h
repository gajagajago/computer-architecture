/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Tue Jun  1 22:40:07 KST 2021
 * 
 */

/* Generation options: */
#ifndef __mkCsrFile_h__
#define __mkCsrFile_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"


/* Class declaration for the mkCsrFile module */
class MOD_mkCsrFile : public Module {
 
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
  MOD_ConfigReg<tUInt32> INST_coreId;
  MOD_Reg<tUInt32> INST_cycles;
  MOD_ConfigReg<tUInt32> INST_numInsts;
  MOD_ConfigReg<tUInt8> INST_startReg;
  MOD_Reg<tUInt8> INST_toHostFifo_clearReq_ehrReg;
  MOD_Wire<tUInt8> INST_toHostFifo_clearReq_ignored_wires_0;
  MOD_Wire<tUInt8> INST_toHostFifo_clearReq_ignored_wires_1;
  MOD_Reg<tUInt8> INST_toHostFifo_clearReq_virtual_reg_0;
  MOD_Reg<tUInt8> INST_toHostFifo_clearReq_virtual_reg_1;
  MOD_Wire<tUInt8> INST_toHostFifo_clearReq_wires_0;
  MOD_Wire<tUInt8> INST_toHostFifo_clearReq_wires_1;
  MOD_Reg<tUInt64> INST_toHostFifo_data_0;
  MOD_Reg<tUInt64> INST_toHostFifo_data_1;
  MOD_Reg<tUInt8> INST_toHostFifo_deqP;
  MOD_Reg<tUInt8> INST_toHostFifo_deqReq_ehrReg;
  MOD_Wire<tUInt8> INST_toHostFifo_deqReq_ignored_wires_0;
  MOD_Wire<tUInt8> INST_toHostFifo_deqReq_ignored_wires_1;
  MOD_Wire<tUInt8> INST_toHostFifo_deqReq_ignored_wires_2;
  MOD_Reg<tUInt8> INST_toHostFifo_deqReq_virtual_reg_0;
  MOD_Reg<tUInt8> INST_toHostFifo_deqReq_virtual_reg_1;
  MOD_Reg<tUInt8> INST_toHostFifo_deqReq_virtual_reg_2;
  MOD_Wire<tUInt8> INST_toHostFifo_deqReq_wires_0;
  MOD_Wire<tUInt8> INST_toHostFifo_deqReq_wires_1;
  MOD_Wire<tUInt8> INST_toHostFifo_deqReq_wires_2;
  MOD_Reg<tUInt8> INST_toHostFifo_empty;
  MOD_Reg<tUInt8> INST_toHostFifo_enqP;
  MOD_Reg<tUInt64> INST_toHostFifo_enqReq_ehrReg;
  MOD_Wire<tUInt64> INST_toHostFifo_enqReq_ignored_wires_0;
  MOD_Wire<tUInt64> INST_toHostFifo_enqReq_ignored_wires_1;
  MOD_Wire<tUInt64> INST_toHostFifo_enqReq_ignored_wires_2;
  MOD_Reg<tUInt8> INST_toHostFifo_enqReq_virtual_reg_0;
  MOD_Reg<tUInt8> INST_toHostFifo_enqReq_virtual_reg_1;
  MOD_Reg<tUInt8> INST_toHostFifo_enqReq_virtual_reg_2;
  MOD_Wire<tUInt64> INST_toHostFifo_enqReq_wires_0;
  MOD_Wire<tUInt64> INST_toHostFifo_enqReq_wires_1;
  MOD_Wire<tUInt64> INST_toHostFifo_enqReq_wires_2;
  MOD_Reg<tUInt8> INST_toHostFifo_full;
 
 /* Constructor */
 public:
  MOD_mkCsrFile(tSimStateHdl simHdl, char const *name, Module *parent);
 
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
  tUInt8 DEF_toHostFifo_empty__h5544;
  tUInt8 DEF_toHostFifo_full__h5515;
  tUInt8 DEF_startReg_read____d93;
 
 /* Local definitions */
 private:
  tUInt8 DEF_IF_toHostFifo_enqReq_wires_1_whas_THEN_toHostF_ETC___d13;
  tUInt8 DEF_IF_toHostFifo_clearReq_wires_0_whas__3_THEN_to_ETC___d46;
  tUInt64 DEF_toHostFifo_enqReq_wires_1_wget____d5;
  tUInt64 DEF_toHostFifo_enqReq_wires_0_wget____d8;
  tUInt64 DEF_toHostFifo_enqReq_ehrReg___d10;
  tUInt32 DEF__read__h110;
  tUInt32 DEF_x_numInst__h6837;
  tUInt8 DEF_toHostFifo_clearReq_wires_0_whas____d43;
  tUInt8 DEF_toHostFifo_clearReq_wires_0_wget____d44;
  tUInt8 DEF_toHostFifo_clearReq_ehrReg___d45;
  tUInt8 DEF_toHostFifo_deqReq_ehrReg___d37;
  tUInt8 DEF_toHostFifo_enqReq_wires_1_whas____d4;
  tUInt8 DEF_toHostFifo_enqReq_wires_0_whas____d7;
  tUInt8 DEF_x__h7204;
  tUInt64 DEF_toHostFifo_enqReq_ehrReg_0_BITS_49_TO_0___d25;
  tUInt8 DEF_toHostFifo_enqReq_ehrReg_0_BIT_50___d11;
  tUInt64 DEF_IF_toHostFifo_enqReq_wires_1_whas_THEN_toHostF_ETC___d27;
  tUInt8 DEF_IF_toHostFifo_deqReq_wires_1_whas__3_THEN_toHo_ETC___d39;
 
 /* Rules */
 public:
  void RL_toHostFifo_enqReq_canonicalize();
  void RL_toHostFifo_deqReq_canonicalize();
  void RL_toHostFifo_clearReq_canonicalize();
  void RL_toHostFifo_canonicalize();
  void RL_count();
 
 /* Methods */
 public:
  void METH_start(tUInt32 ARG_start_id);
  tUInt8 METH_RDY_start();
  tUInt8 METH_started();
  tUInt8 METH_RDY_started();
  tUInt32 METH_rd(tUInt32 ARG_rd_idx);
  tUInt8 METH_RDY_rd();
  void METH_wr(tUInt32 ARG_wr_idx, tUInt32 ARG_wr_val);
  tUInt8 METH_RDY_wr();
  tUInt64 METH_cpuToHost();
  tUInt8 METH_RDY_cpuToHost();
 
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
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkCsrFile &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkCsrFile &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkCsrFile &backing);
};

#endif /* ifndef __mkCsrFile_h__ */
