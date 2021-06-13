/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Sun Jun 13 19:38:05 KST 2021
 * 
 */
#include "bluesim_primitives.h"
#include "mkCache.h"


/* Constructor */
MOD_mkCache::MOD_mkCache(tSimStateHdl simHdl, char const *name, Module *parent)
  : Module(simHdl, name, parent),
    __clk_handle_0(BAD_CLOCK_HANDLE),
    INST_cacheSetAssociative(simHdl, "cacheSetAssociative", this),
    PORT_RST_N((tUInt8)1u)
{
  PORT_req_r.setSize(65u);
  PORT_req_r.clear();
  PORT_memReq.setSize(66u);
  PORT_memReq.clear();
  symbol_count = 3u;
  symbols = new tSym[symbol_count];
  init_symbols_0();
}


/* Symbol init fns */

void MOD_mkCache::init_symbols_0()
{
  init_symbol(&symbols[0u], "cacheSetAssociative", SYM_MODULE, &INST_cacheSetAssociative);
  init_symbol(&symbols[1u], "memReq", SYM_PORT, &PORT_memReq, 66u);
  init_symbol(&symbols[2u], "req_r", SYM_PORT, &PORT_req_r, 65u);
}


/* Rule actions */


/* Methods */

void MOD_mkCache::METH_req(tUWide ARG_req_r)
{
  PORT_req_r = ARG_req_r;
  INST_cacheSetAssociative.METH_req(ARG_req_r);
}

tUInt8 MOD_mkCache::METH_RDY_req()
{
  tUInt8 DEF_CAN_FIRE_req;
  tUInt8 PORT_RDY_req;
  DEF_CAN_FIRE_req = INST_cacheSetAssociative.METH_RDY_req();
  PORT_RDY_req = DEF_CAN_FIRE_req;
  return PORT_RDY_req;
}

tUInt32 MOD_mkCache::METH_resp()
{
  tUInt32 PORT_resp;
  tUInt32 DEF_AVMeth_cacheSetAssociative_resp;
  DEF_AVMeth_cacheSetAssociative_resp = INST_cacheSetAssociative.METH_resp();
  PORT_resp = DEF_AVMeth_cacheSetAssociative_resp;
  return PORT_resp;
}

tUInt8 MOD_mkCache::METH_RDY_resp()
{
  tUInt8 DEF_CAN_FIRE_resp;
  tUInt8 PORT_RDY_resp;
  DEF_CAN_FIRE_resp = INST_cacheSetAssociative.METH_RDY_resp();
  PORT_RDY_resp = DEF_CAN_FIRE_resp;
  return PORT_RDY_resp;
}

tUWide MOD_mkCache::METH_memReq()
{
  tUWide DEF_AVMeth_cacheSetAssociative_memReq(66u, false);
  DEF_AVMeth_cacheSetAssociative_memReq = INST_cacheSetAssociative.METH_memReq();
  PORT_memReq = DEF_AVMeth_cacheSetAssociative_memReq;
  return PORT_memReq;
}

tUInt8 MOD_mkCache::METH_RDY_memReq()
{
  tUInt8 DEF_CAN_FIRE_memReq;
  tUInt8 PORT_RDY_memReq;
  DEF_CAN_FIRE_memReq = INST_cacheSetAssociative.METH_RDY_memReq();
  PORT_RDY_memReq = DEF_CAN_FIRE_memReq;
  return PORT_RDY_memReq;
}

void MOD_mkCache::METH_memResp(tUInt32 ARG_memResp_r)
{
  INST_cacheSetAssociative.METH_memResp(ARG_memResp_r);
}

tUInt8 MOD_mkCache::METH_RDY_memResp()
{
  tUInt8 DEF_CAN_FIRE_memResp;
  tUInt8 PORT_RDY_memResp;
  DEF_CAN_FIRE_memResp = INST_cacheSetAssociative.METH_RDY_memResp();
  PORT_RDY_memResp = DEF_CAN_FIRE_memResp;
  return PORT_RDY_memResp;
}

tUInt32 MOD_mkCache::METH_getMissCnt()
{
  tUInt32 PORT_getMissCnt;
  PORT_getMissCnt = INST_cacheSetAssociative.METH_getMissCnt();
  return PORT_getMissCnt;
}

tUInt8 MOD_mkCache::METH_RDY_getMissCnt()
{
  tUInt8 DEF_CAN_FIRE_getMissCnt;
  tUInt8 PORT_RDY_getMissCnt;
  DEF_CAN_FIRE_getMissCnt = (tUInt8)1u;
  PORT_RDY_getMissCnt = DEF_CAN_FIRE_getMissCnt;
  return PORT_RDY_getMissCnt;
}

tUInt32 MOD_mkCache::METH_getTotalReq()
{
  tUInt32 PORT_getTotalReq;
  PORT_getTotalReq = INST_cacheSetAssociative.METH_getTotalReq();
  return PORT_getTotalReq;
}

tUInt8 MOD_mkCache::METH_RDY_getTotalReq()
{
  tUInt8 DEF_CAN_FIRE_getTotalReq;
  tUInt8 PORT_RDY_getTotalReq;
  DEF_CAN_FIRE_getTotalReq = (tUInt8)1u;
  PORT_RDY_getTotalReq = DEF_CAN_FIRE_getTotalReq;
  return PORT_RDY_getTotalReq;
}


/* Reset routines */

void MOD_mkCache::reset_RST_N(tUInt8 ARG_rst_in)
{
  PORT_RST_N = ARG_rst_in;
  INST_cacheSetAssociative.reset_RST_N(ARG_rst_in);
}


/* Static handles to reset routines */


/* Functions for the parent module to register its reset fns */


/* Functions to set the elaborated clock id */

void MOD_mkCache::set_clk_0(char const *s)
{
  __clk_handle_0 = bk_get_or_define_clock(sim_hdl, s);
}


/* State dumping routine */
void MOD_mkCache::dump_state(unsigned int indent)
{
  printf("%*s%s:\n", indent, "", inst_name);
  INST_cacheSetAssociative.dump_state(indent + 2u);
}


/* VCD dumping routines */

unsigned int MOD_mkCache::dump_VCD_defs(unsigned int levels)
{
  vcd_write_scope_start(sim_hdl, inst_name);
  vcd_num = vcd_reserve_ids(sim_hdl, 3u);
  unsigned int num = vcd_num;
  for (unsigned int clk = 0u; clk < bk_num_clocks(sim_hdl); ++clk)
    vcd_add_clock_def(sim_hdl, this, bk_clock_name(sim_hdl, clk), bk_clock_vcd_num(sim_hdl, clk));
  vcd_write_def(sim_hdl, bk_clock_vcd_num(sim_hdl, __clk_handle_0), "CLK", 1u);
  vcd_write_def(sim_hdl, num++, "RST_N", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "memReq", 66u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "req_r", 65u);
  if (levels != 1u)
  {
    unsigned int l = levels == 0u ? 0u : levels - 1u;
    num = INST_cacheSetAssociative.dump_VCD_defs(l);
  }
  vcd_write_scope_end(sim_hdl);
  return num;
}

void MOD_mkCache::dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkCache &backing)
{
  vcd_defs(dt, backing);
  if (levels != 1u)
    vcd_submodules(dt, levels - 1u, backing);
}

void MOD_mkCache::vcd_defs(tVCDDumpType dt, MOD_mkCache &backing)
{
  unsigned int num = vcd_num;
  if (dt == VCD_DUMP_XS)
  {
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 66u);
    vcd_write_x(sim_hdl, num++, 65u);
  }
  else
    if (dt == VCD_DUMP_CHANGES)
    {
      if ((backing.PORT_RST_N) != PORT_RST_N)
      {
	vcd_write_val(sim_hdl, num, PORT_RST_N, 1u);
	backing.PORT_RST_N = PORT_RST_N;
      }
      ++num;
      if ((backing.PORT_memReq) != PORT_memReq)
      {
	vcd_write_val(sim_hdl, num, PORT_memReq, 66u);
	backing.PORT_memReq = PORT_memReq;
      }
      ++num;
      if ((backing.PORT_req_r) != PORT_req_r)
      {
	vcd_write_val(sim_hdl, num, PORT_req_r, 65u);
	backing.PORT_req_r = PORT_req_r;
      }
      ++num;
    }
    else
    {
      vcd_write_val(sim_hdl, num++, PORT_RST_N, 1u);
      backing.PORT_RST_N = PORT_RST_N;
      vcd_write_val(sim_hdl, num++, PORT_memReq, 66u);
      backing.PORT_memReq = PORT_memReq;
      vcd_write_val(sim_hdl, num++, PORT_req_r, 65u);
      backing.PORT_req_r = PORT_req_r;
    }
}

void MOD_mkCache::vcd_submodules(tVCDDumpType dt, unsigned int levels, MOD_mkCache &backing)
{
  INST_cacheSetAssociative.dump_VCD(dt, levels, backing.INST_cacheSetAssociative);
}
