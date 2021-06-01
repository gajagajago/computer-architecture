/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Tue Jun  1 22:49:00 KST 2021
 * 
 */
#include "bluesim_primitives.h"
#include "mkTestBench.h"


/* String declarations */
static std::string const __str_literal_1("==================================\n", 35u);
static std::string const __str_literal_3("Executed Instructions : %d\n", 27u);
static std::string const __str_literal_2("Number of Cycles      : %d\n", 27u);
static std::string const __str_literal_6("Program Exceeded the maximum cycle %d\n", 38u);
static std::string const __str_literal_5("Result                :     FAILED %d\n", 38u);
static std::string const __str_literal_4("Result                :     PASSED\n", 35u);


/* Constructor */
MOD_mkTestBench::MOD_mkTestBench(tSimStateHdl simHdl, char const *name, Module *parent)
  : Module(simHdl, name, parent),
    __clk_handle_0(BAD_CLOCK_HANDLE),
    INST_cycle(simHdl, "cycle", this, 32u, 0u, (tUInt8)0u),
    INST_proc(simHdl, "proc", this),
    INST_tState(simHdl, "tState", this, 2u, (tUInt8)0u, (tUInt8)0u),
    PORT_RST_N((tUInt8)1u)
{
  symbol_count = 7u;
  symbols = new tSym[symbol_count];
  init_symbols_0();
}


/* Symbol init fns */

void MOD_mkTestBench::init_symbols_0()
{
  init_symbol(&symbols[0u], "cycle", SYM_MODULE, &INST_cycle);
  init_symbol(&symbols[1u], "proc", SYM_MODULE, &INST_proc);
  init_symbol(&symbols[2u], "RL_countCycle", SYM_RULE);
  init_symbol(&symbols[3u], "RL_halt", SYM_RULE);
  init_symbol(&symbols[4u], "RL_run", SYM_RULE);
  init_symbol(&symbols[5u], "RL_start", SYM_RULE);
  init_symbol(&symbols[6u], "tState", SYM_MODULE, &INST_tState);
}


/* Rule actions */

void MOD_mkTestBench::RL_start()
{
  INST_proc.METH_hostToCpu(0u);
  INST_tState.METH_write((tUInt8)1u);
}

void MOD_mkTestBench::RL_countCycle()
{
  tUInt32 DEF_x__h240;
  tUInt8 DEF_NOT_cycle_EQ_300000___d8;
  tUInt8 DEF_cycle_EQ_300000___d7;
  DEF__read__h122 = INST_cycle.METH_read();
  DEF_cycle_EQ_300000___d7 = DEF__read__h122 == 300000u;
  DEF_NOT_cycle_EQ_300000___d8 = !DEF_cycle_EQ_300000___d7;
  DEF_x__h240 = DEF__read__h122 + 1u;
  if (DEF_cycle_EQ_300000___d7)
    INST_tState.METH_write((tUInt8)2u);
  if (DEF_NOT_cycle_EQ_300000___d8)
    INST_cycle.METH_write(DEF_x__h240);
}

void MOD_mkTestBench::RL_run()
{
  tUInt8 DEF_proc_cpuToHost_2_BITS_49_TO_48_3_EQ_0_4_AND_pr_ETC___d18;
  tUInt8 DEF_proc_cpuToHost_2_BITS_49_TO_48_3_EQ_0_4_AND_NO_ETC___d20;
  tUInt8 DEF_proc_cpuToHost_2_BITS_49_TO_48_3_EQ_0___d14;
  tUInt8 DEF_proc_cpuToHost_2_BITS_47_TO_32_6_EQ_0___d17;
  tUInt32 DEF_proc_cpuToHost_2_BITS_47_TO_32___d16;
  tUInt32 DEF_proc_cpuToHost_2_BITS_31_TO_0___d15;
  tUInt64 DEF_proc_cpuToHost___d12;
  tUInt64 DEF_AVMeth_proc_cpuToHost;
  DEF__read__h122 = INST_cycle.METH_read();
  DEF_AVMeth_proc_cpuToHost = INST_proc.METH_cpuToHost();
  DEF_proc_cpuToHost___d12 = DEF_AVMeth_proc_cpuToHost;
  DEF_proc_cpuToHost_2_BITS_31_TO_0___d15 = (tUInt32)(DEF_proc_cpuToHost___d12);
  DEF_proc_cpuToHost_2_BITS_47_TO_32___d16 = (tUInt32)(65535u & (DEF_proc_cpuToHost___d12 >> 32u));
  DEF_proc_cpuToHost_2_BITS_47_TO_32_6_EQ_0___d17 = DEF_proc_cpuToHost_2_BITS_47_TO_32___d16 == 0u;
  DEF_proc_cpuToHost_2_BITS_49_TO_48_3_EQ_0___d14 = ((tUInt8)(DEF_proc_cpuToHost___d12 >> 48u)) == (tUInt8)0u;
  DEF_proc_cpuToHost_2_BITS_49_TO_48_3_EQ_0_4_AND_NO_ETC___d20 = DEF_proc_cpuToHost_2_BITS_49_TO_48_3_EQ_0___d14 && !DEF_proc_cpuToHost_2_BITS_47_TO_32_6_EQ_0___d17;
  DEF_proc_cpuToHost_2_BITS_49_TO_48_3_EQ_0_4_AND_pr_ETC___d18 = DEF_proc_cpuToHost_2_BITS_49_TO_48_3_EQ_0___d14 && DEF_proc_cpuToHost_2_BITS_47_TO_32_6_EQ_0___d17;
  if (!(PORT_RST_N == (tUInt8)0u))
  {
    if (DEF_proc_cpuToHost_2_BITS_49_TO_48_3_EQ_0___d14)
      dollar_fwrite(sim_hdl, this, "32,s", 2147483650u, &__str_literal_1);
    if (DEF_proc_cpuToHost_2_BITS_49_TO_48_3_EQ_0___d14)
      dollar_fwrite(sim_hdl, this, "32,s,32", 2147483650u, &__str_literal_2, DEF__read__h122);
    if (DEF_proc_cpuToHost_2_BITS_49_TO_48_3_EQ_0___d14)
      dollar_fwrite(sim_hdl,
		    this,
		    "32,s,32",
		    2147483650u,
		    &__str_literal_3,
		    DEF_proc_cpuToHost_2_BITS_31_TO_0___d15);
    if (DEF_proc_cpuToHost_2_BITS_49_TO_48_3_EQ_0_4_AND_pr_ETC___d18)
      dollar_fwrite(sim_hdl, this, "32,s", 2147483650u, &__str_literal_4);
    if (DEF_proc_cpuToHost_2_BITS_49_TO_48_3_EQ_0_4_AND_NO_ETC___d20)
      dollar_fwrite(sim_hdl,
		    this,
		    "32,s,16",
		    2147483650u,
		    &__str_literal_5,
		    DEF_proc_cpuToHost_2_BITS_47_TO_32___d16);
    if (DEF_proc_cpuToHost_2_BITS_49_TO_48_3_EQ_0___d14)
      dollar_fwrite(sim_hdl, this, "32,s", 2147483650u, &__str_literal_1);
    if (DEF_proc_cpuToHost_2_BITS_49_TO_48_3_EQ_0___d14)
      dollar_finish(sim_hdl, "32", 1u);
  }
}

void MOD_mkTestBench::RL_halt()
{
  tUInt32 DEF_signed_300000___d22;
  DEF_signed_300000___d22 = 300000u;
  if (!(PORT_RST_N == (tUInt8)0u))
  {
    dollar_fwrite(sim_hdl, this, "32,s,-32", 2147483650u, &__str_literal_6, DEF_signed_300000___d22);
    dollar_finish(sim_hdl, "32", 1u);
  }
}


/* Methods */


/* Reset routines */

void MOD_mkTestBench::reset_RST_N(tUInt8 ARG_rst_in)
{
  PORT_RST_N = ARG_rst_in;
  INST_tState.reset_RST(ARG_rst_in);
  INST_proc.reset_RST_N(ARG_rst_in);
  INST_cycle.reset_RST(ARG_rst_in);
}


/* Static handles to reset routines */


/* Functions for the parent module to register its reset fns */


/* Functions to set the elaborated clock id */

void MOD_mkTestBench::set_clk_0(char const *s)
{
  __clk_handle_0 = bk_get_or_define_clock(sim_hdl, s);
}


/* State dumping routine */
void MOD_mkTestBench::dump_state(unsigned int indent)
{
  printf("%*s%s:\n", indent, "", inst_name);
  INST_cycle.dump_state(indent + 2u);
  INST_proc.dump_state(indent + 2u);
  INST_tState.dump_state(indent + 2u);
}


/* VCD dumping routines */

unsigned int MOD_mkTestBench::dump_VCD_defs(unsigned int levels)
{
  vcd_write_scope_start(sim_hdl, inst_name);
  vcd_num = vcd_reserve_ids(sim_hdl, 4u);
  unsigned int num = vcd_num;
  for (unsigned int clk = 0u; clk < bk_num_clocks(sim_hdl); ++clk)
    vcd_add_clock_def(sim_hdl, this, bk_clock_name(sim_hdl, clk), bk_clock_vcd_num(sim_hdl, clk));
  vcd_write_def(sim_hdl, bk_clock_vcd_num(sim_hdl, __clk_handle_0), "CLK", 1u);
  vcd_write_def(sim_hdl, num++, "RST_N", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h122", 32u);
  num = INST_cycle.dump_VCD_defs(num);
  num = INST_tState.dump_VCD_defs(num);
  if (levels != 1u)
  {
    unsigned int l = levels == 0u ? 0u : levels - 1u;
    num = INST_proc.dump_VCD_defs(l);
  }
  vcd_write_scope_end(sim_hdl);
  return num;
}

void MOD_mkTestBench::dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkTestBench &backing)
{
  vcd_defs(dt, backing);
  vcd_prims(dt, backing);
  if (levels != 1u)
    vcd_submodules(dt, levels - 1u, backing);
}

void MOD_mkTestBench::vcd_defs(tVCDDumpType dt, MOD_mkTestBench &backing)
{
  unsigned int num = vcd_num;
  if (dt == VCD_DUMP_XS)
  {
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 32u);
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
      if ((backing.DEF__read__h122) != DEF__read__h122)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h122, 32u);
	backing.DEF__read__h122 = DEF__read__h122;
      }
      ++num;
    }
    else
    {
      vcd_write_val(sim_hdl, num++, PORT_RST_N, 1u);
      backing.PORT_RST_N = PORT_RST_N;
      vcd_write_val(sim_hdl, num++, DEF__read__h122, 32u);
      backing.DEF__read__h122 = DEF__read__h122;
    }
}

void MOD_mkTestBench::vcd_prims(tVCDDumpType dt, MOD_mkTestBench &backing)
{
  INST_cycle.dump_VCD(dt, backing.INST_cycle);
  INST_tState.dump_VCD(dt, backing.INST_tState);
}

void MOD_mkTestBench::vcd_submodules(tVCDDumpType dt, unsigned int levels, MOD_mkTestBench &backing)
{
  INST_proc.dump_VCD(dt, levels, backing.INST_proc);
}
