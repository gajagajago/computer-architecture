/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Sun Jun 13 20:13:13 KST 2021
 * 
 */

/* Generation options: */
#ifndef __mkCache_h__
#define __mkCache_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"
#include "mkCacheSetAssociative.h"


/* Class declaration for the mkCache module */
class MOD_mkCache : public Module {
 
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
  MOD_mkCacheSetAssociative INST_cacheSetAssociative;
 
 /* Constructor */
 public:
  MOD_mkCache(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
  tUInt8 PORT_RST_N;
 
 /* Port definitions */
 public:
  tUWide PORT_req_r;
  tUWide PORT_memReq;
 
 /* Publicly accessible definitions */
 public:
 
 /* Local definitions */
 private:
 
 /* Rules */
 public:
 
 /* Methods */
 public:
  void METH_req(tUWide ARG_req_r);
  tUInt8 METH_RDY_req();
  tUInt32 METH_resp();
  tUInt8 METH_RDY_resp();
  tUWide METH_memReq();
  tUInt8 METH_RDY_memReq();
  void METH_memResp(tUInt32 ARG_memResp_r);
  tUInt8 METH_RDY_memResp();
  tUInt32 METH_getMissCnt();
  tUInt8 METH_RDY_getMissCnt();
  tUInt32 METH_getTotalReq();
  tUInt8 METH_RDY_getTotalReq();
 
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
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkCache &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkCache &backing);
  void vcd_submodules(tVCDDumpType dt, unsigned int levels, MOD_mkCache &backing);
};

#endif /* ifndef __mkCache_h__ */
