/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Mon May 31 15:52:11 KST 2021
 * 
 */

/* Generation options: */
#ifndef __module_exec_h__
#define __module_exec_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"
#include "module_brAddrCalc.h"
#include "module_alu.h"
#include "module_aluBr.h"


/* Class declaration for the module_exec module */
class MOD_module_exec : public Module {
 
 /* Clock handles */
 private:
 
 /* Clock gate handles */
 public:
  tUInt8 *clk_gate[0];
 
 /* Instantiation parameters */
 public:
 
 /* Module state */
 public:
  MOD_module_brAddrCalc INST_instance_brAddrCalc_2;
  MOD_module_alu INST_instance_alu_1;
  MOD_module_aluBr INST_instance_aluBr_0;
 
 /* Constructor */
 public:
  MOD_module_exec(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
 
 /* Port definitions */
 public:
  tUWide PORT_exec_dInst;
  tUWide PORT_exec;
 
 /* Publicly accessible definitions */
 public:
 
 /* Local definitions */
 private:
  tUWide DEF_exec_dInst_BIT_78_CONCAT_IF_exec_dInst_BIT_78__ETC___d38;
 
 /* Rules */
 public:
 
 /* Methods */
 public:
  tUWide METH_exec(tUWide ARG_exec_dInst,
		   tUInt32 ARG_exec_rVal1,
		   tUInt32 ARG_exec_rVal2,
		   tUInt32 ARG_exec_pc,
		   tUInt32 ARG_exec_ppc,
		   tUInt32 ARG_exec_csrVal);
  tUInt8 METH_RDY_exec();
 
 /* Reset routines */
 public:
 
 /* Static handles to reset routines */
 public:
 
 /* Pointers to reset fns in parent module for asserting output resets */
 private:
 
 /* Functions for the parent module to register its reset fns */
 public:
 
 /* Functions to set the elaborated clock id */
 public:
 
 /* State dumping routine */
 public:
  void dump_state(unsigned int indent);
 
 /* VCD dumping routines */
 public:
  unsigned int dump_VCD_defs(unsigned int levels);
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_module_exec &backing);
  void vcd_defs(tVCDDumpType dt, MOD_module_exec &backing);
  void vcd_submodules(tVCDDumpType dt, unsigned int levels, MOD_module_exec &backing);
};

#endif /* ifndef __module_exec_h__ */
