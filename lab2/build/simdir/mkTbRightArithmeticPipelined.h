/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Thu Mar 25 02:08:51 KST 2021
 * 
 */

/* Generation options: */
#ifndef __mkTbRightArithmeticPipelined_h__
#define __mkTbRightArithmeticPipelined_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"


/* Class declaration for the mkTbRightArithmeticPipelined module */
class MOD_mkTbRightArithmeticPipelined : public Module {
 
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
  MOD_Fifo<tUWide> INST_bsrap_bsrp_fifo1;
  MOD_Fifo<tUWide> INST_bsrap_bsrp_fifo2;
  MOD_Fifo<tUWide> INST_bsrap_bsrp_fifo3;
  MOD_Fifo<tUWide> INST_bsrap_bsrp_fifo4;
  MOD_Fifo<tUWide> INST_bsrap_bsrp_fifo5;
  MOD_Fifo<tUWide> INST_bsrap_bsrp_inFifo;
  MOD_Fifo<tUInt64> INST_bsrap_bsrp_outFifo;
  MOD_Reg<tUInt32> INST_correct;
  MOD_Reg<tUInt32> INST_input_cycle;
  MOD_Reg<tUInt32> INST_output_cycle;
  MOD_Reg<tUInt8> INST_randomShift_init;
  MOD_Reg<tUInt8> INST_randomVal_init;
  MOD_Fifo<tUWide> INST_valFifo;
 
 /* Constructor */
 public:
  MOD_mkTbRightArithmeticPipelined(tSimStateHdl simHdl, char const *name, Module *parent);
 
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
  tUInt32 DEF_x__h2719;
  tUInt32 DEF_x__h2368;
  tUInt8 DEF_output_cycle_00_EQ_128___d101;
 
 /* Local definitions */
 private:
  tUInt64 DEF_x__h2134;
  tUInt64 DEF_TASK_getRandom___d93;
  tUWide DEF_TASK_getRandom_3_CONCAT_TASK_getRandom_4_BITS__ETC___d96;
  tUWide DEF_bsrap_bsrp_fifo5_first____d73;
  tUWide DEF_bsrap_bsrp_fifo4_first____d59;
  tUWide DEF_bsrap_bsrp_fifo3_first____d45;
  tUWide DEF_bsrap_bsrp_fifo2_first____d31;
  tUWide DEF_bsrap_bsrp_fifo1_first____d17;
  tUWide DEF_bsrap_bsrp_inFifo_first____d4;
  tUWide DEF_valFifo_first____d113;
  tUWide DEF_IF_bsrap_bsrp_fifo4_first__9_BIT_1_0_THEN_IF_b_ETC___d69;
  tUWide DEF_IF_bsrap_bsrp_fifo3_first__5_BIT_1_6_THEN_IF_b_ETC___d55;
  tUWide DEF_IF_bsrap_bsrp_fifo2_first__1_BIT_1_2_THEN_IF_b_ETC___d41;
  tUWide DEF_IF_bsrap_bsrp_inFifo_first_BIT_1_THEN_bsrap_bs_ETC___d13;
  tUWide DEF_IF_bsrap_bsrp_fifo1_first__7_BIT_1_8_THEN_IF_b_ETC___d27;
  tUWide DEF_TASK_getRandom_3_CONCAT_TASK_getRandom_4_BITS__ETC___d98;
 
 /* Rules */
 public:
  void RL_bsrap_bsrp_stage1();
  void RL_bsrap_bsrp_stage2();
  void RL_bsrap_bsrp_stage3();
  void RL_bsrap_bsrp_stage4();
  void RL_bsrap_bsrp_stage5();
  void RL_bsrap_bsrp_stage6();
  void RL_randomVal_initialize();
  void RL_randomShift_initialize();
  void RL_test_request();
  void RL_test_response();
 
 /* Methods */
 public:
 
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
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkTbRightArithmeticPipelined &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkTbRightArithmeticPipelined &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkTbRightArithmeticPipelined &backing);
};

#endif /* ifndef __mkTbRightArithmeticPipelined_h__ */
