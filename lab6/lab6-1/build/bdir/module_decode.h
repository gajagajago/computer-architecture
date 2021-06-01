/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Tue Jun  1 22:40:07 KST 2021
 * 
 */

/* Generation options: */
#ifndef __module_decode_h__
#define __module_decode_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"


/* Class declaration for the module_decode module */
class MOD_module_decode : public Module {
 
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
  MOD_module_decode(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
 
 /* Port definitions */
 public:
  tUWide PORT_decode;
 
 /* Publicly accessible definitions */
 public:
 
 /* Local definitions */
 private:
  tUWide DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d186;
  tUWide DEF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decode_i_ETC___d185;
  tUWide DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011_6_CONCAT__ETC___d184;
 
 /* Rules */
 public:
 
 /* Methods */
 public:
  tUWide METH_decode(tUInt32 ARG_decode_inst);
  tUInt8 METH_RDY_decode();
 
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
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_module_decode &backing);
  void vcd_defs(tVCDDumpType dt, MOD_module_decode &backing);
};

#endif /* ifndef __module_decode_h__ */
