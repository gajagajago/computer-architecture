/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Mon May 17 22:25:39 KST 2021
 * 
 */

/* Generation options: */
#ifndef __module_brAddrCalc_h__
#define __module_brAddrCalc_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"


/* Class declaration for the module_brAddrCalc module */
class MOD_module_brAddrCalc : public Module {
 
 /* Clock handles */
 private:
 
 /* Clock gate handles */
 public:
  tUInt8 *clk_gate[0];
 
 /* Instantiation parameters */
 public:
 
 /* Module state */
 public:
 
 /* Constructor */
 public:
  MOD_module_brAddrCalc(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
 
 /* Port definitions */
 public:
 
 /* Publicly accessible definitions */
 public:
 
 /* Local definitions */
 private:
 
 /* Rules */
 public:
 
 /* Methods */
 public:
  tUInt32 METH_brAddrCalc(tUInt32 ARG_brAddrCalc_pc,
			  tUInt32 ARG_brAddrCalc_val,
			  tUInt8 ARG_brAddrCalc_iType,
			  tUInt32 ARG_brAddrCalc_imm,
			  tUInt8 ARG_brAddrCalc_taken);
  tUInt8 METH_RDY_brAddrCalc();
 
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
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_module_brAddrCalc &backing);
};

#endif /* ifndef __module_brAddrCalc_h__ */
