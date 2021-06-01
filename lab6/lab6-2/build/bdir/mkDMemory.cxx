/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Tue Jun  1 22:49:00 KST 2021
 * 
 */
#include "bluesim_primitives.h"
#include "mkDMemory.h"


/* String declarations */
static std::string const __str_literal_1("memory.vmh", 10u);


/* Constructor */
MOD_mkDMemory::MOD_mkDMemory(tSimStateHdl simHdl, char const *name, Module *parent)
  : Module(simHdl, name, parent),
    __clk_handle_0(BAD_CLOCK_HANDLE),
    INST_mem(simHdl, "mem", this, __str_literal_1, 16u, 32u, 0u, 65535u, (tUInt8)0u),
    INST_memInit_initialized(simHdl, "memInit_initialized", this, 1u, (tUInt8)1u, (tUInt8)0u),
    PORT_RST_N((tUInt8)1u)
{
  PORT_init_request_put.setSize(65u);
  PORT_init_request_put.clear();
  PORT_req_r.setSize(65u);
  PORT_req_r.clear();
  symbol_count = 5u;
  symbols = new tSym[symbol_count];
  init_symbols_0();
}


/* Symbol init fns */

void MOD_mkDMemory::init_symbols_0()
{
  init_symbol(&symbols[0u], "init_request_put", SYM_PORT, &PORT_init_request_put, 65u);
  init_symbol(&symbols[1u], "mem", SYM_MODULE, &INST_mem);
  init_symbol(&symbols[2u], "memInit_initialized", SYM_MODULE, &INST_memInit_initialized);
  init_symbol(&symbols[3u], "memInit_initialized__h256", SYM_DEF, &DEF_memInit_initialized__h256, 1u);
  init_symbol(&symbols[4u], "req_r", SYM_PORT, &PORT_req_r, 65u);
}


/* Rule actions */


/* Methods */

tUInt32 MOD_mkDMemory::METH_req(tUWide ARG_req_r)
{
  tUInt32 DEF_req__avValue1;
  tUInt8 DEF_req_r_BIT_64___d1;
  tUInt32 DEF_index__h258;
  tUInt32 DEF_x__h306;
  tUInt32 PORT_req;
  PORT_req_r = ARG_req_r;
  DEF_x__h306 = ARG_req_r.get_whole_word(0u);
  DEF_index__h258 = ARG_req_r.get_bits_in_word32(1u, 2u, 16u);
  DEF_req_r_BIT_64___d1 = ARG_req_r.get_bits_in_word8(2u, 0u, 1u);
  DEF_req__avValue1 = INST_mem.METH_sub(DEF_index__h258);
  PORT_req = DEF_req__avValue1;
  if (DEF_req_r_BIT_64___d1)
    INST_mem.METH_upd(DEF_index__h258, DEF_x__h306);
  return PORT_req;
}

tUInt8 MOD_mkDMemory::METH_RDY_req()
{
  tUInt8 PORT_RDY_req;
  tUInt8 DEF_CAN_FIRE_req;
  DEF_memInit_initialized__h256 = INST_memInit_initialized.METH_read();
  DEF_CAN_FIRE_req = DEF_memInit_initialized__h256;
  PORT_RDY_req = DEF_CAN_FIRE_req;
  return PORT_RDY_req;
}

void MOD_mkDMemory::METH_init_request_put(tUWide ARG_init_request_put)
{
  tUInt8 DEF_init_request_put_BIT_64___d5;
  PORT_init_request_put = ARG_init_request_put;
  DEF_init_request_put_BIT_64___d5 = ARG_init_request_put.get_bits_in_word8(2u, 0u, 1u);
  if (DEF_init_request_put_BIT_64___d5)
    INST_memInit_initialized.METH_write((tUInt8)1u);
}

tUInt8 MOD_mkDMemory::METH_RDY_init_request_put()
{
  tUInt8 PORT_RDY_init_request_put;
  tUInt8 DEF_CAN_FIRE_init_request_put;
  DEF_memInit_initialized__h256 = INST_memInit_initialized.METH_read();
  DEF_CAN_FIRE_init_request_put = !DEF_memInit_initialized__h256;
  PORT_RDY_init_request_put = DEF_CAN_FIRE_init_request_put;
  return PORT_RDY_init_request_put;
}

tUInt8 MOD_mkDMemory::METH_init_done()
{
  tUInt8 PORT_init_done;
  DEF_memInit_initialized__h256 = INST_memInit_initialized.METH_read();
  PORT_init_done = DEF_memInit_initialized__h256;
  return PORT_init_done;
}

tUInt8 MOD_mkDMemory::METH_RDY_init_done()
{
  tUInt8 PORT_RDY_init_done;
  tUInt8 DEF_CAN_FIRE_init_done;
  DEF_CAN_FIRE_init_done = (tUInt8)1u;
  PORT_RDY_init_done = DEF_CAN_FIRE_init_done;
  return PORT_RDY_init_done;
}


/* Reset routines */

void MOD_mkDMemory::reset_RST_N(tUInt8 ARG_rst_in)
{
  PORT_RST_N = ARG_rst_in;
  INST_memInit_initialized.reset_RST(ARG_rst_in);
}


/* Static handles to reset routines */


/* Functions for the parent module to register its reset fns */


/* Functions to set the elaborated clock id */

void MOD_mkDMemory::set_clk_0(char const *s)
{
  __clk_handle_0 = bk_get_or_define_clock(sim_hdl, s);
}


/* State dumping routine */
void MOD_mkDMemory::dump_state(unsigned int indent)
{
  printf("%*s%s:\n", indent, "", inst_name);
  INST_mem.dump_state(indent + 2u);
  INST_memInit_initialized.dump_state(indent + 2u);
}


/* VCD dumping routines */

unsigned int MOD_mkDMemory::dump_VCD_defs(unsigned int levels)
{
  vcd_write_scope_start(sim_hdl, inst_name);
  vcd_num = vcd_reserve_ids(sim_hdl, 6u);
  unsigned int num = vcd_num;
  for (unsigned int clk = 0u; clk < bk_num_clocks(sim_hdl); ++clk)
    vcd_add_clock_def(sim_hdl, this, bk_clock_name(sim_hdl, clk), bk_clock_vcd_num(sim_hdl, clk));
  vcd_write_def(sim_hdl, bk_clock_vcd_num(sim_hdl, __clk_handle_0), "CLK", 1u);
  vcd_write_def(sim_hdl, num++, "RST_N", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "memInit_initialized__h256", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "init_request_put", 65u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "req_r", 65u);
  num = INST_mem.dump_VCD_defs(num);
  num = INST_memInit_initialized.dump_VCD_defs(num);
  vcd_write_scope_end(sim_hdl);
  return num;
}

void MOD_mkDMemory::dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkDMemory &backing)
{
  vcd_defs(dt, backing);
  vcd_prims(dt, backing);
}

void MOD_mkDMemory::vcd_defs(tVCDDumpType dt, MOD_mkDMemory &backing)
{
  unsigned int num = vcd_num;
  if (dt == VCD_DUMP_XS)
  {
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 65u);
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
      if ((backing.DEF_memInit_initialized__h256) != DEF_memInit_initialized__h256)
      {
	vcd_write_val(sim_hdl, num, DEF_memInit_initialized__h256, 1u);
	backing.DEF_memInit_initialized__h256 = DEF_memInit_initialized__h256;
      }
      ++num;
      if ((backing.PORT_init_request_put) != PORT_init_request_put)
      {
	vcd_write_val(sim_hdl, num, PORT_init_request_put, 65u);
	backing.PORT_init_request_put = PORT_init_request_put;
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
      vcd_write_val(sim_hdl, num++, DEF_memInit_initialized__h256, 1u);
      backing.DEF_memInit_initialized__h256 = DEF_memInit_initialized__h256;
      vcd_write_val(sim_hdl, num++, PORT_init_request_put, 65u);
      backing.PORT_init_request_put = PORT_init_request_put;
      vcd_write_val(sim_hdl, num++, PORT_req_r, 65u);
      backing.PORT_req_r = PORT_req_r;
    }
}

void MOD_mkDMemory::vcd_prims(tVCDDumpType dt, MOD_mkDMemory &backing)
{
  INST_mem.dump_VCD(dt, backing.INST_mem);
  INST_memInit_initialized.dump_VCD(dt, backing.INST_memInit_initialized);
}
