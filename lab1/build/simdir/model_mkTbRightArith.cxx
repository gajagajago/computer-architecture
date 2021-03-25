/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Thu Mar 25 02:13:46 KST 2021
 * 
 */
#include "bluesim_primitives.h"
#include "model_mkTbRightArith.h"

#include <cstdlib>
#include <time.h>
#include "bluesim_kernel_api.h"
#include "bs_vcd.h"
#include "bs_reset.h"


/* Constructor */
MODEL_mkTbRightArith::MODEL_mkTbRightArith()
{
  mkTbRightArith_instance = NULL;
}

/* Function for creating a new model */
void * new_MODEL_mkTbRightArith()
{
  MODEL_mkTbRightArith *model = new MODEL_mkTbRightArith();
  return (void *)(model);
}

/* Schedule functions */

static void schedule_posedge_CLK(tSimStateHdl simHdl, void *instance_ptr)
       {
	 MOD_mkTbRightArith &INST_top = *((MOD_mkTbRightArith *)(instance_ptr));
	 tUInt8 DEF_INST_top_DEF_randomVal_init__h124;
	 tUInt8 DEF_INST_top_DEF_randomShift_init__h226;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_randomVal_initialize;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_randomVal_initialize;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_randomShift_initialize;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_randomShift_initialize;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_test;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_test;
	 DEF_INST_top_DEF_randomShift_init__h226 = INST_top.INST_randomShift_init.METH_read();
	 DEF_INST_top_DEF_CAN_FIRE_RL_randomShift_initialize = !DEF_INST_top_DEF_randomShift_init__h226;
	 DEF_INST_top_DEF_WILL_FIRE_RL_randomShift_initialize = DEF_INST_top_DEF_CAN_FIRE_RL_randomShift_initialize;
	 DEF_INST_top_DEF_randomVal_init__h124 = INST_top.INST_randomVal_init.METH_read();
	 DEF_INST_top_DEF_CAN_FIRE_RL_randomVal_initialize = !DEF_INST_top_DEF_randomVal_init__h124;
	 DEF_INST_top_DEF_WILL_FIRE_RL_randomVal_initialize = DEF_INST_top_DEF_CAN_FIRE_RL_randomVal_initialize;
	 INST_top.DEF__read__h65 = INST_top.INST_cycle.METH_read();
	 INST_top.DEF_cycle_EQ_128___d6 = (INST_top.DEF__read__h65) == 128u;
	 DEF_INST_top_DEF_CAN_FIRE_RL_test = INST_top.DEF_cycle_EQ_128___d6 || (DEF_INST_top_DEF_randomVal_init__h124 && DEF_INST_top_DEF_randomShift_init__h226);
	 DEF_INST_top_DEF_WILL_FIRE_RL_test = DEF_INST_top_DEF_CAN_FIRE_RL_test;
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_test)
	   INST_top.RL_test();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_randomVal_initialize)
	   INST_top.RL_randomVal_initialize();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_randomShift_initialize)
	   INST_top.RL_randomShift_initialize();
	 if (do_reset_ticks(simHdl))
	 {
	   INST_top.INST_cycle.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_randomVal_init.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_randomShift_init.rst_tick__clk__1((tUInt8)1u);
	 }
       };

/* Model creation/destruction functions */

void MODEL_mkTbRightArith::create_model(tSimStateHdl simHdl, bool master)
{
  sim_hdl = simHdl;
  init_reset_request_counters(sim_hdl);
  mkTbRightArith_instance = new MOD_mkTbRightArith(sim_hdl, "top", NULL);
  bk_get_or_define_clock(sim_hdl, "CLK");
  if (master)
  {
    bk_alter_clock(sim_hdl, bk_get_clock_by_name(sim_hdl, "CLK"), CLK_LOW, false, 0llu, 5llu, 5llu);
    bk_use_default_reset(sim_hdl);
  }
  bk_set_clock_event_fn(sim_hdl,
			bk_get_clock_by_name(sim_hdl, "CLK"),
			schedule_posedge_CLK,
			NULL,
			(tEdgeDirection)(POSEDGE));
  (mkTbRightArith_instance->set_clk_0)("CLK");
}
void MODEL_mkTbRightArith::destroy_model()
{
  delete mkTbRightArith_instance;
  mkTbRightArith_instance = NULL;
}
void MODEL_mkTbRightArith::reset_model(bool asserted)
{
  (mkTbRightArith_instance->reset_RST_N)(asserted ? (tUInt8)0u : (tUInt8)1u);
}
void * MODEL_mkTbRightArith::get_instance()
{
  return mkTbRightArith_instance;
}

/* Fill in version numbers */
void MODEL_mkTbRightArith::get_version(unsigned int *year,
				       unsigned int *month,
				       char const **annotation,
				       char const **build)
{
  *year = 0u;
  *month = 0u;
  *annotation = NULL;
  *build = "ad73d8a";
}

/* Get the model creation time */
time_t MODEL_mkTbRightArith::get_creation_time()
{
  
  /* Wed Mar 24 17:13:46 UTC 2021 */
  return 1616606026llu;
}

/* Control run-time licensing */
tUInt64 MODEL_mkTbRightArith::skip_license_check()
{
  return 0llu;
}

/* State dumping function */
void MODEL_mkTbRightArith::dump_state()
{
  (mkTbRightArith_instance->dump_state)(0u);
}

/* VCD dumping functions */
MOD_mkTbRightArith & mkTbRightArith_backing(tSimStateHdl simHdl)
{
  static MOD_mkTbRightArith *instance = NULL;
  if (instance == NULL)
  {
    vcd_set_backing_instance(simHdl, true);
    instance = new MOD_mkTbRightArith(simHdl, "top", NULL);
    vcd_set_backing_instance(simHdl, false);
  }
  return *instance;
}
void MODEL_mkTbRightArith::dump_VCD_defs()
{
  (mkTbRightArith_instance->dump_VCD_defs)(vcd_depth(sim_hdl));
}
void MODEL_mkTbRightArith::dump_VCD(tVCDDumpType dt)
{
  (mkTbRightArith_instance->dump_VCD)(dt, vcd_depth(sim_hdl), mkTbRightArith_backing(sim_hdl));
}
