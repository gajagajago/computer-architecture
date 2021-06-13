/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Sun Jun 13 01:07:20 KST 2021
 * 
 */
#include "bluesim_primitives.h"
#include "module_brAddrCalc.h"


/* Constructor */
MOD_module_brAddrCalc::MOD_module_brAddrCalc(tSimStateHdl simHdl, char const *name, Module *parent)
  : Module(simHdl, name, parent)
{
  symbol_count = 0u;
  init_symbols_0();
}


/* Symbol init fns */

void MOD_module_brAddrCalc::init_symbols_0()
{
}


/* Rule actions */


/* Methods */

tUInt32 MOD_module_brAddrCalc::METH_brAddrCalc(tUInt32 ARG_brAddrCalc_pc,
					       tUInt32 ARG_brAddrCalc_val,
					       tUInt8 ARG_brAddrCalc_iType,
					       tUInt32 ARG_brAddrCalc_imm,
					       tUInt8 ARG_brAddrCalc_taken)
{
  tUInt32 DEF_brAddrCalc_pc_PLUS_brAddrCalc_imm___d2;
  tUInt32 DEF_brAddrCalc_val_PLUS_brAddrCalc_imm_BITS_31_TO__ETC___d6;
  tUInt32 DEF_pcPlus4__h27;
  tUInt32 DEF_IF_brAddrCalc_taken_THEN_brAddrCalc_pc_PLUS_br_ETC___d9;
  tUInt32 DEF_x__h58;
  tUInt32 DEF_x_BITS_31_TO_1___h60;
  tUInt32 PORT_brAddrCalc;
  DEF_x__h58 = ARG_brAddrCalc_val + ARG_brAddrCalc_imm;
  DEF_x_BITS_31_TO_1___h60 = (tUInt32)(DEF_x__h58 >> 1u);
  DEF_pcPlus4__h27 = ARG_brAddrCalc_pc + 4u;
  DEF_brAddrCalc_val_PLUS_brAddrCalc_imm_BITS_31_TO__ETC___d6 = (DEF_x_BITS_31_TO_1___h60 << 1u) | (tUInt32)((tUInt8)0u);
  DEF_brAddrCalc_pc_PLUS_brAddrCalc_imm___d2 = ARG_brAddrCalc_pc + ARG_brAddrCalc_imm;
  DEF_IF_brAddrCalc_taken_THEN_brAddrCalc_pc_PLUS_br_ETC___d9 = ARG_brAddrCalc_taken ? DEF_brAddrCalc_pc_PLUS_brAddrCalc_imm___d2 : DEF_pcPlus4__h27;
  switch (ARG_brAddrCalc_iType) {
  case (tUInt8)4u:
    PORT_brAddrCalc = DEF_brAddrCalc_pc_PLUS_brAddrCalc_imm___d2;
    break;
  case (tUInt8)5u:
    PORT_brAddrCalc = DEF_brAddrCalc_val_PLUS_brAddrCalc_imm_BITS_31_TO__ETC___d6;
    break;
  case (tUInt8)6u:
    PORT_brAddrCalc = DEF_IF_brAddrCalc_taken_THEN_brAddrCalc_pc_PLUS_br_ETC___d9;
    break;
  default:
    PORT_brAddrCalc = DEF_pcPlus4__h27;
  }
  return PORT_brAddrCalc;
}

tUInt8 MOD_module_brAddrCalc::METH_RDY_brAddrCalc()
{
  tUInt8 PORT_RDY_brAddrCalc;
  tUInt8 DEF_CAN_FIRE_brAddrCalc;
  DEF_CAN_FIRE_brAddrCalc = (tUInt8)1u;
  PORT_RDY_brAddrCalc = DEF_CAN_FIRE_brAddrCalc;
  return PORT_RDY_brAddrCalc;
}


/* Reset routines */


/* Static handles to reset routines */


/* Functions for the parent module to register its reset fns */


/* Functions to set the elaborated clock id */


/* State dumping routine */
void MOD_module_brAddrCalc::dump_state(unsigned int indent)
{
}


/* VCD dumping routines */

unsigned int MOD_module_brAddrCalc::dump_VCD_defs(unsigned int levels)
{
  vcd_write_scope_start(sim_hdl, inst_name);
  vcd_num = vcd_reserve_ids(sim_hdl, 0u);
  unsigned int num = vcd_num;
  for (unsigned int clk = 0u; clk < bk_num_clocks(sim_hdl); ++clk)
    vcd_add_clock_def(sim_hdl, this, bk_clock_name(sim_hdl, clk), bk_clock_vcd_num(sim_hdl, clk));
  vcd_write_scope_end(sim_hdl);
  return num;
}

void MOD_module_brAddrCalc::dump_VCD(tVCDDumpType dt,
				     unsigned int levels,
				     MOD_module_brAddrCalc &backing)
{
}
