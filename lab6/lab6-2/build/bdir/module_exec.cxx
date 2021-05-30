/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Sun May 30 19:01:00 KST 2021
 * 
 */
#include "bluesim_primitives.h"
#include "module_exec.h"


/* Constructor */
MOD_module_exec::MOD_module_exec(tSimStateHdl simHdl, char const *name, Module *parent)
  : Module(simHdl, name, parent),
    INST_instance_brAddrCalc_2(simHdl, "instance_brAddrCalc_2", this),
    INST_instance_alu_1(simHdl, "instance_alu_1", this),
    INST_instance_aluBr_0(simHdl, "instance_aluBr_0", this),
    DEF_exec_dInst_BIT_78_CONCAT_IF_exec_dInst_BIT_78__ETC___d38(79u)
{
  PORT_exec_dInst.setSize(108u);
  PORT_exec_dInst.clear();
  PORT_exec.setSize(89u);
  PORT_exec.clear();
  symbol_count = 5u;
  symbols = new tSym[symbol_count];
  init_symbols_0();
}


/* Symbol init fns */

void MOD_module_exec::init_symbols_0()
{
  init_symbol(&symbols[0u], "exec", SYM_PORT, &PORT_exec, 89u);
  init_symbol(&symbols[1u], "exec_dInst", SYM_PORT, &PORT_exec_dInst, 108u);
  init_symbol(&symbols[2u], "instance_alu_1", SYM_MODULE, &INST_instance_alu_1);
  init_symbol(&symbols[3u], "instance_aluBr_0", SYM_MODULE, &INST_instance_aluBr_0);
  init_symbol(&symbols[4u], "instance_brAddrCalc_2", SYM_MODULE, &INST_instance_brAddrCalc_2);
}


/* Rule actions */


/* Methods */

tUWide MOD_module_exec::METH_exec(tUWide ARG_exec_dInst,
				  tUInt32 ARG_exec_rVal1,
				  tUInt32 ARG_exec_rVal2,
				  tUInt32 ARG_exec_pc,
				  tUInt32 ARG_exec_ppc,
				  tUInt32 ARG_exec_csrVal)
{
  tUInt8 DEF_exec_dInst_BIT_96_CONCAT_IF_exec_dInst_BIT_96__ETC___d5;
  tUInt32 DEF_exec_pc_PLUS_4___d15;
  tUInt32 DEF_x__h255;
  tUInt32 DEF_x__h404;
  tUInt32 DEF_aluVal2__h29;
  tUInt32 DEF_aluRes__h30;
  tUInt8 DEF_aluBr___d32;
  tUInt32 DEF_IF_exec_dInst_BIT_65_7_THEN_exec_dInst_BITS_64_ETC___d19;
  tUInt8 DEF_exec_dInst_BITS_107_TO_104___d1;
  tUInt32 DEF_brAddr__h36;
  PORT_exec_dInst = ARG_exec_dInst;
  DEF_exec_dInst_BITS_107_TO_104___d1 = ARG_exec_dInst.get_bits_in_word8(3u, 8u, 4u);
  DEF_IF_exec_dInst_BIT_65_7_THEN_exec_dInst_BITS_64_ETC___d19 = primExtract32(32u,
									       108u,
									       ARG_exec_dInst,
									       32u,
									       64u,
									       32u,
									       33u);
  DEF_aluBr___d32 = INST_instance_aluBr_0.METH_aluBr(ARG_exec_rVal1,
						     ARG_exec_rVal2,
						     ARG_exec_dInst.get_bits_in_word8(3u, 1u, 3u));
  DEF_brAddr__h36 = INST_instance_brAddrCalc_2.METH_brAddrCalc(ARG_exec_pc,
							       ARG_exec_rVal1,
							       DEF_exec_dInst_BITS_107_TO_104___d1,
							       DEF_IF_exec_dInst_BIT_65_7_THEN_exec_dInst_BITS_64_ETC___d19,
							       DEF_aluBr___d32);
  DEF_aluVal2__h29 = ARG_exec_dInst.get_bits_in_word8(2u,
						      1u,
						      1u) ? DEF_IF_exec_dInst_BIT_65_7_THEN_exec_dInst_BITS_64_ETC___d19 : ARG_exec_rVal2;
  DEF_aluRes__h30 = INST_instance_alu_1.METH_alu(ARG_exec_rVal1,
						 DEF_aluVal2__h29,
						 ARG_exec_dInst.get_bits_in_word8(3u, 4u, 4u));
  switch (DEF_exec_dInst_BITS_107_TO_104___d1) {
  case (tUInt8)2u:
  case (tUInt8)3u:
    DEF_x__h404 = DEF_aluRes__h30;
    break;
  default:
    DEF_x__h404 = DEF_brAddr__h36;
  }
  DEF_exec_pc_PLUS_4___d15 = ARG_exec_pc + 4u;
  switch (DEF_exec_dInst_BITS_107_TO_104___d1) {
  case (tUInt8)7u:
    DEF_x__h255 = ARG_exec_csrVal;
    break;
  case (tUInt8)8u:
    DEF_x__h255 = ARG_exec_rVal1;
    break;
  case (tUInt8)3u:
    DEF_x__h255 = ARG_exec_rVal2;
    break;
  case (tUInt8)4u:
  case (tUInt8)5u:
    DEF_x__h255 = DEF_exec_pc_PLUS_4___d15;
    break;
  case (tUInt8)9u:
    DEF_x__h255 = ARG_exec_pc + DEF_IF_exec_dInst_BIT_65_7_THEN_exec_dInst_BITS_64_ETC___d19;
    break;
  default:
    DEF_x__h255 = DEF_aluRes__h30;
  }
  DEF_exec_dInst_BIT_78_CONCAT_IF_exec_dInst_BIT_78__ETC___d38.set_bits_in_word(32767u & (((((tUInt32)(ARG_exec_dInst.get_bits_in_word8(2u,
																	14u,
																	1u))) << 14u) | (ARG_exec_dInst.get_bits_in_word32(2u,
																							   2u,
																							   12u) << 2u)) | (tUInt32)((tUInt8)(DEF_x__h255 >> 30u))),
										2u,
										0u,
										15u).set_whole_word((((tUInt32)(1073741823u & DEF_x__h255)) << 2u) | (tUInt32)((tUInt8)(DEF_x__h404 >> 30u)),
												    1u).set_whole_word(((((tUInt32)(1073741823u & DEF_x__h404)) << 2u) | (((tUInt32)(!(DEF_brAddr__h36 == ARG_exec_ppc))) << 1u)) | (tUInt32)(DEF_aluBr___d32),
														       0u);
  DEF_exec_dInst_BIT_96_CONCAT_IF_exec_dInst_BIT_96__ETC___d5 = (tUInt8)63u & ((ARG_exec_dInst.get_bits_in_word8(3u,
														 0u,
														 1u) << 5u) | ARG_exec_dInst.get_bits_in_word8(2u,
																			       27u,
																			       5u));
  PORT_exec.set_bits_in_word(33554431u & (((((tUInt32)(DEF_exec_dInst_BITS_107_TO_104___d1)) << 21u) | (((tUInt32)(DEF_exec_dInst_BIT_96_CONCAT_IF_exec_dInst_BIT_96__ETC___d5)) << 15u)) | DEF_exec_dInst_BIT_78_CONCAT_IF_exec_dInst_BIT_78__ETC___d38.get_bits_in_word32(2u,
																																	    0u,
																																	    15u)),
			     2u,
			     0u,
			     25u).set_whole_word(DEF_exec_dInst_BIT_78_CONCAT_IF_exec_dInst_BIT_78__ETC___d38.get_whole_word(1u),
						 1u).set_whole_word(DEF_exec_dInst_BIT_78_CONCAT_IF_exec_dInst_BIT_78__ETC___d38.get_whole_word(0u),
								    0u);
  return PORT_exec;
}

tUInt8 MOD_module_exec::METH_RDY_exec()
{
  tUInt8 PORT_RDY_exec;
  tUInt8 DEF_CAN_FIRE_exec;
  DEF_CAN_FIRE_exec = (tUInt8)1u;
  PORT_RDY_exec = DEF_CAN_FIRE_exec;
  return PORT_RDY_exec;
}


/* Reset routines */


/* Static handles to reset routines */


/* Functions for the parent module to register its reset fns */


/* Functions to set the elaborated clock id */


/* State dumping routine */
void MOD_module_exec::dump_state(unsigned int indent)
{
}


/* VCD dumping routines */

unsigned int MOD_module_exec::dump_VCD_defs(unsigned int levels)
{
  vcd_write_scope_start(sim_hdl, inst_name);
  vcd_num = vcd_reserve_ids(sim_hdl, 3u);
  unsigned int num = vcd_num;
  for (unsigned int clk = 0u; clk < bk_num_clocks(sim_hdl); ++clk)
    vcd_add_clock_def(sim_hdl, this, bk_clock_name(sim_hdl, clk), bk_clock_vcd_num(sim_hdl, clk));
  vcd_write_def(sim_hdl, num++, "exec_dInst_BIT_78_CONCAT_IF_exec_dInst_BIT_78__ETC___d38", 79u);
  vcd_write_def(sim_hdl, num++, "exec", 89u);
  vcd_write_def(sim_hdl, num++, "exec_dInst", 108u);
  if (levels != 1u)
  {
    unsigned int l = levels == 0u ? 0u : levels - 1u;
    num = INST_instance_aluBr_0.dump_VCD_defs(l);
    num = INST_instance_alu_1.dump_VCD_defs(l);
    num = INST_instance_brAddrCalc_2.dump_VCD_defs(l);
  }
  vcd_write_scope_end(sim_hdl);
  return num;
}

void MOD_module_exec::dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_module_exec &backing)
{
  vcd_defs(dt, backing);
  if (levels != 1u)
    vcd_submodules(dt, levels - 1u, backing);
}

void MOD_module_exec::vcd_defs(tVCDDumpType dt, MOD_module_exec &backing)
{
  unsigned int num = vcd_num;
  if (dt == VCD_DUMP_XS)
  {
    vcd_write_x(sim_hdl, num++, 79u);
    vcd_write_x(sim_hdl, num++, 89u);
    vcd_write_x(sim_hdl, num++, 108u);
  }
  else
    if (dt == VCD_DUMP_CHANGES)
    {
      if ((backing.DEF_exec_dInst_BIT_78_CONCAT_IF_exec_dInst_BIT_78__ETC___d38) != DEF_exec_dInst_BIT_78_CONCAT_IF_exec_dInst_BIT_78__ETC___d38)
      {
	vcd_write_val(sim_hdl, num, DEF_exec_dInst_BIT_78_CONCAT_IF_exec_dInst_BIT_78__ETC___d38, 79u);
	backing.DEF_exec_dInst_BIT_78_CONCAT_IF_exec_dInst_BIT_78__ETC___d38 = DEF_exec_dInst_BIT_78_CONCAT_IF_exec_dInst_BIT_78__ETC___d38;
      }
      ++num;
      if ((backing.PORT_exec) != PORT_exec)
      {
	vcd_write_val(sim_hdl, num, PORT_exec, 89u);
	backing.PORT_exec = PORT_exec;
      }
      ++num;
      if ((backing.PORT_exec_dInst) != PORT_exec_dInst)
      {
	vcd_write_val(sim_hdl, num, PORT_exec_dInst, 108u);
	backing.PORT_exec_dInst = PORT_exec_dInst;
      }
      ++num;
    }
    else
    {
      vcd_write_val(sim_hdl, num++, DEF_exec_dInst_BIT_78_CONCAT_IF_exec_dInst_BIT_78__ETC___d38, 79u);
      backing.DEF_exec_dInst_BIT_78_CONCAT_IF_exec_dInst_BIT_78__ETC___d38 = DEF_exec_dInst_BIT_78_CONCAT_IF_exec_dInst_BIT_78__ETC___d38;
      vcd_write_val(sim_hdl, num++, PORT_exec, 89u);
      backing.PORT_exec = PORT_exec;
      vcd_write_val(sim_hdl, num++, PORT_exec_dInst, 108u);
      backing.PORT_exec_dInst = PORT_exec_dInst;
    }
}

void MOD_module_exec::vcd_submodules(tVCDDumpType dt, unsigned int levels, MOD_module_exec &backing)
{
  INST_instance_aluBr_0.dump_VCD(dt, levels, backing.INST_instance_aluBr_0);
  INST_instance_alu_1.dump_VCD(dt, levels, backing.INST_instance_alu_1);
  INST_instance_brAddrCalc_2.dump_VCD(dt, levels, backing.INST_instance_brAddrCalc_2);
}
