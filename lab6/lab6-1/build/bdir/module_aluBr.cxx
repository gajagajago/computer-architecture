/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Sun May 30 01:34:33 KST 2021
 * 
 */
#include "bluesim_primitives.h"
#include "module_aluBr.h"


/* Constructor */
MOD_module_aluBr::MOD_module_aluBr(tSimStateHdl simHdl, char const *name, Module *parent)
  : Module(simHdl, name, parent)
{
  symbol_count = 0u;
  init_symbols_0();
}


/* Symbol init fns */

void MOD_module_aluBr::init_symbols_0()
{
}


/* Rule actions */


/* Methods */

tUInt8 MOD_module_aluBr::METH_aluBr(tUInt32 ARG_aluBr_a,
				    tUInt32 ARG_aluBr_b,
				    tUInt8 ARG_aluBr_brFunc)
{
  tUInt8 DEF_aluBr_a_EQ_aluBr_b___d2;
  tUInt8 DEF_NOT_aluBr_a_SLT_aluBr_b___d10;
  tUInt8 DEF_aluBr_brFunc_EQ_6___d13;
  tUInt8 DEF_NOT_aluBr_a_ULT_aluBr_b___d12;
  tUInt8 DEF_aluBr_a_ULT_aluBr_b___d8;
  tUInt8 DEF_aluBr_a_SLT_aluBr_b___d6;
  tUInt8 DEF_NOT_aluBr_a_EQ_aluBr_b___d4;
  tUInt8 PORT_aluBr;
  DEF_aluBr_a_SLT_aluBr_b___d6 = primSLT8(1u,
					  32u,
					  (tUInt32)(ARG_aluBr_a),
					  32u,
					  (tUInt32)(ARG_aluBr_b));
  DEF_aluBr_a_ULT_aluBr_b___d8 = ARG_aluBr_a < ARG_aluBr_b;
  DEF_NOT_aluBr_a_ULT_aluBr_b___d12 = !DEF_aluBr_a_ULT_aluBr_b___d8;
  DEF_aluBr_brFunc_EQ_6___d13 = ARG_aluBr_brFunc == (tUInt8)6u;
  DEF_NOT_aluBr_a_SLT_aluBr_b___d10 = !DEF_aluBr_a_SLT_aluBr_b___d6;
  DEF_aluBr_a_EQ_aluBr_b___d2 = ARG_aluBr_a == ARG_aluBr_b;
  DEF_NOT_aluBr_a_EQ_aluBr_b___d4 = !DEF_aluBr_a_EQ_aluBr_b___d2;
  switch (ARG_aluBr_brFunc) {
  case (tUInt8)0u:
    PORT_aluBr = DEF_aluBr_a_EQ_aluBr_b___d2;
    break;
  case (tUInt8)1u:
    PORT_aluBr = DEF_NOT_aluBr_a_EQ_aluBr_b___d4;
    break;
  case (tUInt8)2u:
    PORT_aluBr = DEF_aluBr_a_SLT_aluBr_b___d6;
    break;
  case (tUInt8)3u:
    PORT_aluBr = DEF_aluBr_a_ULT_aluBr_b___d8;
    break;
  case (tUInt8)4u:
    PORT_aluBr = DEF_NOT_aluBr_a_SLT_aluBr_b___d10;
    break;
  case (tUInt8)5u:
    PORT_aluBr = DEF_NOT_aluBr_a_ULT_aluBr_b___d12;
    break;
  default:
    PORT_aluBr = DEF_aluBr_brFunc_EQ_6___d13;
  }
  return PORT_aluBr;
}

tUInt8 MOD_module_aluBr::METH_RDY_aluBr()
{
  tUInt8 PORT_RDY_aluBr;
  tUInt8 DEF_CAN_FIRE_aluBr;
  DEF_CAN_FIRE_aluBr = (tUInt8)1u;
  PORT_RDY_aluBr = DEF_CAN_FIRE_aluBr;
  return PORT_RDY_aluBr;
}


/* Reset routines */


/* Static handles to reset routines */


/* Functions for the parent module to register its reset fns */


/* Functions to set the elaborated clock id */


/* State dumping routine */
void MOD_module_aluBr::dump_state(unsigned int indent)
{
}


/* VCD dumping routines */

unsigned int MOD_module_aluBr::dump_VCD_defs(unsigned int levels)
{
  vcd_write_scope_start(sim_hdl, inst_name);
  vcd_num = vcd_reserve_ids(sim_hdl, 0u);
  unsigned int num = vcd_num;
  for (unsigned int clk = 0u; clk < bk_num_clocks(sim_hdl); ++clk)
    vcd_add_clock_def(sim_hdl, this, bk_clock_name(sim_hdl, clk), bk_clock_vcd_num(sim_hdl, clk));
  vcd_write_scope_end(sim_hdl);
  return num;
}

void MOD_module_aluBr::dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_module_aluBr &backing)
{
}
