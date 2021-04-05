/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Tue Apr  6 01:45:01 KST 2021
 * 
 */

/* Generation options: */
#ifndef __mkFftPipelined_h__
#define __mkFftPipelined_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"
#include "mkBfly4.h"


/* Class declaration for the mkFftPipelined module */
class MOD_mkFftPipelined : public Module {
 
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
  MOD_mkBfly4 INST_bfly_0_0;
  MOD_mkBfly4 INST_bfly_0_1;
  MOD_mkBfly4 INST_bfly_0_10;
  MOD_mkBfly4 INST_bfly_0_11;
  MOD_mkBfly4 INST_bfly_0_12;
  MOD_mkBfly4 INST_bfly_0_13;
  MOD_mkBfly4 INST_bfly_0_14;
  MOD_mkBfly4 INST_bfly_0_15;
  MOD_mkBfly4 INST_bfly_0_2;
  MOD_mkBfly4 INST_bfly_0_3;
  MOD_mkBfly4 INST_bfly_0_4;
  MOD_mkBfly4 INST_bfly_0_5;
  MOD_mkBfly4 INST_bfly_0_6;
  MOD_mkBfly4 INST_bfly_0_7;
  MOD_mkBfly4 INST_bfly_0_8;
  MOD_mkBfly4 INST_bfly_0_9;
  MOD_mkBfly4 INST_bfly_1_0;
  MOD_mkBfly4 INST_bfly_1_1;
  MOD_mkBfly4 INST_bfly_1_10;
  MOD_mkBfly4 INST_bfly_1_11;
  MOD_mkBfly4 INST_bfly_1_12;
  MOD_mkBfly4 INST_bfly_1_13;
  MOD_mkBfly4 INST_bfly_1_14;
  MOD_mkBfly4 INST_bfly_1_15;
  MOD_mkBfly4 INST_bfly_1_2;
  MOD_mkBfly4 INST_bfly_1_3;
  MOD_mkBfly4 INST_bfly_1_4;
  MOD_mkBfly4 INST_bfly_1_5;
  MOD_mkBfly4 INST_bfly_1_6;
  MOD_mkBfly4 INST_bfly_1_7;
  MOD_mkBfly4 INST_bfly_1_8;
  MOD_mkBfly4 INST_bfly_1_9;
  MOD_mkBfly4 INST_bfly_2_0;
  MOD_mkBfly4 INST_bfly_2_1;
  MOD_mkBfly4 INST_bfly_2_10;
  MOD_mkBfly4 INST_bfly_2_11;
  MOD_mkBfly4 INST_bfly_2_12;
  MOD_mkBfly4 INST_bfly_2_13;
  MOD_mkBfly4 INST_bfly_2_14;
  MOD_mkBfly4 INST_bfly_2_15;
  MOD_mkBfly4 INST_bfly_2_2;
  MOD_mkBfly4 INST_bfly_2_3;
  MOD_mkBfly4 INST_bfly_2_4;
  MOD_mkBfly4 INST_bfly_2_5;
  MOD_mkBfly4 INST_bfly_2_6;
  MOD_mkBfly4 INST_bfly_2_7;
  MOD_mkBfly4 INST_bfly_2_8;
  MOD_mkBfly4 INST_bfly_2_9;
  MOD_Reg<tUWide> INST_fifo1_data_0;
  MOD_Reg<tUWide> INST_fifo1_data_1;
  MOD_Reg<tUInt8> INST_fifo1_deqEn_dummy2_0;
  MOD_Reg<tUInt8> INST_fifo1_deqEn_dummy2_1;
  MOD_Reg<tUInt8> INST_fifo1_deqEn_dummy2_2;
  MOD_Wire<tUInt8> INST_fifo1_deqEn_dummy_0_0;
  MOD_Wire<tUInt8> INST_fifo1_deqEn_dummy_0_1;
  MOD_Wire<tUInt8> INST_fifo1_deqEn_dummy_0_2;
  MOD_Wire<tUInt8> INST_fifo1_deqEn_dummy_1_0;
  MOD_Wire<tUInt8> INST_fifo1_deqEn_dummy_1_1;
  MOD_Wire<tUInt8> INST_fifo1_deqEn_dummy_1_2;
  MOD_Wire<tUInt8> INST_fifo1_deqEn_dummy_2_0;
  MOD_Wire<tUInt8> INST_fifo1_deqEn_dummy_2_1;
  MOD_Wire<tUInt8> INST_fifo1_deqEn_dummy_2_2;
  MOD_Wire<tUInt8> INST_fifo1_deqEn_lat_0;
  MOD_Wire<tUInt8> INST_fifo1_deqEn_lat_1;
  MOD_Wire<tUInt8> INST_fifo1_deqEn_lat_2;
  MOD_Reg<tUInt8> INST_fifo1_deqEn_rl;
  MOD_Reg<tUInt8> INST_fifo1_deqP_dummy2_0;
  MOD_Reg<tUInt8> INST_fifo1_deqP_dummy2_1;
  MOD_Reg<tUInt8> INST_fifo1_deqP_dummy2_2;
  MOD_Wire<tUInt8> INST_fifo1_deqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_fifo1_deqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_fifo1_deqP_dummy_0_2;
  MOD_Wire<tUInt8> INST_fifo1_deqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_fifo1_deqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_fifo1_deqP_dummy_1_2;
  MOD_Wire<tUInt8> INST_fifo1_deqP_dummy_2_0;
  MOD_Wire<tUInt8> INST_fifo1_deqP_dummy_2_1;
  MOD_Wire<tUInt8> INST_fifo1_deqP_dummy_2_2;
  MOD_Wire<tUInt8> INST_fifo1_deqP_lat_0;
  MOD_Wire<tUInt8> INST_fifo1_deqP_lat_1;
  MOD_Wire<tUInt8> INST_fifo1_deqP_lat_2;
  MOD_Reg<tUInt8> INST_fifo1_deqP_rl;
  MOD_Reg<tUInt8> INST_fifo1_enqEn_dummy2_0;
  MOD_Reg<tUInt8> INST_fifo1_enqEn_dummy2_1;
  MOD_Reg<tUInt8> INST_fifo1_enqEn_dummy2_2;
  MOD_Wire<tUInt8> INST_fifo1_enqEn_dummy_0_0;
  MOD_Wire<tUInt8> INST_fifo1_enqEn_dummy_0_1;
  MOD_Wire<tUInt8> INST_fifo1_enqEn_dummy_0_2;
  MOD_Wire<tUInt8> INST_fifo1_enqEn_dummy_1_0;
  MOD_Wire<tUInt8> INST_fifo1_enqEn_dummy_1_1;
  MOD_Wire<tUInt8> INST_fifo1_enqEn_dummy_1_2;
  MOD_Wire<tUInt8> INST_fifo1_enqEn_dummy_2_0;
  MOD_Wire<tUInt8> INST_fifo1_enqEn_dummy_2_1;
  MOD_Wire<tUInt8> INST_fifo1_enqEn_dummy_2_2;
  MOD_Wire<tUInt8> INST_fifo1_enqEn_lat_0;
  MOD_Wire<tUInt8> INST_fifo1_enqEn_lat_1;
  MOD_Wire<tUInt8> INST_fifo1_enqEn_lat_2;
  MOD_Reg<tUInt8> INST_fifo1_enqEn_rl;
  MOD_Reg<tUInt8> INST_fifo1_enqP_dummy2_0;
  MOD_Reg<tUInt8> INST_fifo1_enqP_dummy2_1;
  MOD_Reg<tUInt8> INST_fifo1_enqP_dummy2_2;
  MOD_Wire<tUInt8> INST_fifo1_enqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_fifo1_enqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_fifo1_enqP_dummy_0_2;
  MOD_Wire<tUInt8> INST_fifo1_enqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_fifo1_enqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_fifo1_enqP_dummy_1_2;
  MOD_Wire<tUInt8> INST_fifo1_enqP_dummy_2_0;
  MOD_Wire<tUInt8> INST_fifo1_enqP_dummy_2_1;
  MOD_Wire<tUInt8> INST_fifo1_enqP_dummy_2_2;
  MOD_Wire<tUInt8> INST_fifo1_enqP_lat_0;
  MOD_Wire<tUInt8> INST_fifo1_enqP_lat_1;
  MOD_Wire<tUInt8> INST_fifo1_enqP_lat_2;
  MOD_Reg<tUInt8> INST_fifo1_enqP_rl;
  MOD_Reg<tUInt8> INST_fifo1_tempData_dummy2_0;
  MOD_Reg<tUInt8> INST_fifo1_tempData_dummy2_1;
  MOD_Wire<tUWide> INST_fifo1_tempData_dummy_0_0;
  MOD_Wire<tUWide> INST_fifo1_tempData_dummy_0_1;
  MOD_Wire<tUWide> INST_fifo1_tempData_dummy_1_0;
  MOD_Wire<tUWide> INST_fifo1_tempData_dummy_1_1;
  MOD_Wire<tUWide> INST_fifo1_tempData_lat_0;
  MOD_Wire<tUWide> INST_fifo1_tempData_lat_1;
  MOD_Reg<tUWide> INST_fifo1_tempData_rl;
  MOD_Reg<tUInt8> INST_fifo1_tempEnqP_dummy2_0;
  MOD_Reg<tUInt8> INST_fifo1_tempEnqP_dummy2_1;
  MOD_Wire<tUInt8> INST_fifo1_tempEnqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_fifo1_tempEnqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_fifo1_tempEnqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_fifo1_tempEnqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_fifo1_tempEnqP_lat_0;
  MOD_Wire<tUInt8> INST_fifo1_tempEnqP_lat_1;
  MOD_Reg<tUInt8> INST_fifo1_tempEnqP_rl;
  MOD_Reg<tUWide> INST_fifo2_data_0;
  MOD_Reg<tUWide> INST_fifo2_data_1;
  MOD_Reg<tUInt8> INST_fifo2_deqEn_dummy2_0;
  MOD_Reg<tUInt8> INST_fifo2_deqEn_dummy2_1;
  MOD_Reg<tUInt8> INST_fifo2_deqEn_dummy2_2;
  MOD_Wire<tUInt8> INST_fifo2_deqEn_dummy_0_0;
  MOD_Wire<tUInt8> INST_fifo2_deqEn_dummy_0_1;
  MOD_Wire<tUInt8> INST_fifo2_deqEn_dummy_0_2;
  MOD_Wire<tUInt8> INST_fifo2_deqEn_dummy_1_0;
  MOD_Wire<tUInt8> INST_fifo2_deqEn_dummy_1_1;
  MOD_Wire<tUInt8> INST_fifo2_deqEn_dummy_1_2;
  MOD_Wire<tUInt8> INST_fifo2_deqEn_dummy_2_0;
  MOD_Wire<tUInt8> INST_fifo2_deqEn_dummy_2_1;
  MOD_Wire<tUInt8> INST_fifo2_deqEn_dummy_2_2;
  MOD_Wire<tUInt8> INST_fifo2_deqEn_lat_0;
  MOD_Wire<tUInt8> INST_fifo2_deqEn_lat_1;
  MOD_Wire<tUInt8> INST_fifo2_deqEn_lat_2;
  MOD_Reg<tUInt8> INST_fifo2_deqEn_rl;
  MOD_Reg<tUInt8> INST_fifo2_deqP_dummy2_0;
  MOD_Reg<tUInt8> INST_fifo2_deqP_dummy2_1;
  MOD_Reg<tUInt8> INST_fifo2_deqP_dummy2_2;
  MOD_Wire<tUInt8> INST_fifo2_deqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_fifo2_deqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_fifo2_deqP_dummy_0_2;
  MOD_Wire<tUInt8> INST_fifo2_deqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_fifo2_deqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_fifo2_deqP_dummy_1_2;
  MOD_Wire<tUInt8> INST_fifo2_deqP_dummy_2_0;
  MOD_Wire<tUInt8> INST_fifo2_deqP_dummy_2_1;
  MOD_Wire<tUInt8> INST_fifo2_deqP_dummy_2_2;
  MOD_Wire<tUInt8> INST_fifo2_deqP_lat_0;
  MOD_Wire<tUInt8> INST_fifo2_deqP_lat_1;
  MOD_Wire<tUInt8> INST_fifo2_deqP_lat_2;
  MOD_Reg<tUInt8> INST_fifo2_deqP_rl;
  MOD_Reg<tUInt8> INST_fifo2_enqEn_dummy2_0;
  MOD_Reg<tUInt8> INST_fifo2_enqEn_dummy2_1;
  MOD_Reg<tUInt8> INST_fifo2_enqEn_dummy2_2;
  MOD_Wire<tUInt8> INST_fifo2_enqEn_dummy_0_0;
  MOD_Wire<tUInt8> INST_fifo2_enqEn_dummy_0_1;
  MOD_Wire<tUInt8> INST_fifo2_enqEn_dummy_0_2;
  MOD_Wire<tUInt8> INST_fifo2_enqEn_dummy_1_0;
  MOD_Wire<tUInt8> INST_fifo2_enqEn_dummy_1_1;
  MOD_Wire<tUInt8> INST_fifo2_enqEn_dummy_1_2;
  MOD_Wire<tUInt8> INST_fifo2_enqEn_dummy_2_0;
  MOD_Wire<tUInt8> INST_fifo2_enqEn_dummy_2_1;
  MOD_Wire<tUInt8> INST_fifo2_enqEn_dummy_2_2;
  MOD_Wire<tUInt8> INST_fifo2_enqEn_lat_0;
  MOD_Wire<tUInt8> INST_fifo2_enqEn_lat_1;
  MOD_Wire<tUInt8> INST_fifo2_enqEn_lat_2;
  MOD_Reg<tUInt8> INST_fifo2_enqEn_rl;
  MOD_Reg<tUInt8> INST_fifo2_enqP_dummy2_0;
  MOD_Reg<tUInt8> INST_fifo2_enqP_dummy2_1;
  MOD_Reg<tUInt8> INST_fifo2_enqP_dummy2_2;
  MOD_Wire<tUInt8> INST_fifo2_enqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_fifo2_enqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_fifo2_enqP_dummy_0_2;
  MOD_Wire<tUInt8> INST_fifo2_enqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_fifo2_enqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_fifo2_enqP_dummy_1_2;
  MOD_Wire<tUInt8> INST_fifo2_enqP_dummy_2_0;
  MOD_Wire<tUInt8> INST_fifo2_enqP_dummy_2_1;
  MOD_Wire<tUInt8> INST_fifo2_enqP_dummy_2_2;
  MOD_Wire<tUInt8> INST_fifo2_enqP_lat_0;
  MOD_Wire<tUInt8> INST_fifo2_enqP_lat_1;
  MOD_Wire<tUInt8> INST_fifo2_enqP_lat_2;
  MOD_Reg<tUInt8> INST_fifo2_enqP_rl;
  MOD_Reg<tUInt8> INST_fifo2_tempData_dummy2_0;
  MOD_Reg<tUInt8> INST_fifo2_tempData_dummy2_1;
  MOD_Wire<tUWide> INST_fifo2_tempData_dummy_0_0;
  MOD_Wire<tUWide> INST_fifo2_tempData_dummy_0_1;
  MOD_Wire<tUWide> INST_fifo2_tempData_dummy_1_0;
  MOD_Wire<tUWide> INST_fifo2_tempData_dummy_1_1;
  MOD_Wire<tUWide> INST_fifo2_tempData_lat_0;
  MOD_Wire<tUWide> INST_fifo2_tempData_lat_1;
  MOD_Reg<tUWide> INST_fifo2_tempData_rl;
  MOD_Reg<tUInt8> INST_fifo2_tempEnqP_dummy2_0;
  MOD_Reg<tUInt8> INST_fifo2_tempEnqP_dummy2_1;
  MOD_Wire<tUInt8> INST_fifo2_tempEnqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_fifo2_tempEnqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_fifo2_tempEnqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_fifo2_tempEnqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_fifo2_tempEnqP_lat_0;
  MOD_Wire<tUInt8> INST_fifo2_tempEnqP_lat_1;
  MOD_Reg<tUInt8> INST_fifo2_tempEnqP_rl;
  MOD_Reg<tUWide> INST_inFifo_data_0;
  MOD_Reg<tUWide> INST_inFifo_data_1;
  MOD_Reg<tUInt8> INST_inFifo_deqEn_dummy2_0;
  MOD_Reg<tUInt8> INST_inFifo_deqEn_dummy2_1;
  MOD_Reg<tUInt8> INST_inFifo_deqEn_dummy2_2;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_dummy_0_0;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_dummy_0_1;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_dummy_0_2;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_dummy_1_0;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_dummy_1_1;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_dummy_1_2;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_dummy_2_0;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_dummy_2_1;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_dummy_2_2;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_lat_0;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_lat_1;
  MOD_Wire<tUInt8> INST_inFifo_deqEn_lat_2;
  MOD_Reg<tUInt8> INST_inFifo_deqEn_rl;
  MOD_Reg<tUInt8> INST_inFifo_deqP_dummy2_0;
  MOD_Reg<tUInt8> INST_inFifo_deqP_dummy2_1;
  MOD_Reg<tUInt8> INST_inFifo_deqP_dummy2_2;
  MOD_Wire<tUInt8> INST_inFifo_deqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_inFifo_deqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_inFifo_deqP_dummy_0_2;
  MOD_Wire<tUInt8> INST_inFifo_deqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_inFifo_deqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_inFifo_deqP_dummy_1_2;
  MOD_Wire<tUInt8> INST_inFifo_deqP_dummy_2_0;
  MOD_Wire<tUInt8> INST_inFifo_deqP_dummy_2_1;
  MOD_Wire<tUInt8> INST_inFifo_deqP_dummy_2_2;
  MOD_Wire<tUInt8> INST_inFifo_deqP_lat_0;
  MOD_Wire<tUInt8> INST_inFifo_deqP_lat_1;
  MOD_Wire<tUInt8> INST_inFifo_deqP_lat_2;
  MOD_Reg<tUInt8> INST_inFifo_deqP_rl;
  MOD_Reg<tUInt8> INST_inFifo_enqEn_dummy2_0;
  MOD_Reg<tUInt8> INST_inFifo_enqEn_dummy2_1;
  MOD_Reg<tUInt8> INST_inFifo_enqEn_dummy2_2;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_dummy_0_0;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_dummy_0_1;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_dummy_0_2;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_dummy_1_0;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_dummy_1_1;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_dummy_1_2;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_dummy_2_0;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_dummy_2_1;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_dummy_2_2;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_lat_0;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_lat_1;
  MOD_Wire<tUInt8> INST_inFifo_enqEn_lat_2;
  MOD_Reg<tUInt8> INST_inFifo_enqEn_rl;
  MOD_Reg<tUInt8> INST_inFifo_enqP_dummy2_0;
  MOD_Reg<tUInt8> INST_inFifo_enqP_dummy2_1;
  MOD_Reg<tUInt8> INST_inFifo_enqP_dummy2_2;
  MOD_Wire<tUInt8> INST_inFifo_enqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_inFifo_enqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_inFifo_enqP_dummy_0_2;
  MOD_Wire<tUInt8> INST_inFifo_enqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_inFifo_enqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_inFifo_enqP_dummy_1_2;
  MOD_Wire<tUInt8> INST_inFifo_enqP_dummy_2_0;
  MOD_Wire<tUInt8> INST_inFifo_enqP_dummy_2_1;
  MOD_Wire<tUInt8> INST_inFifo_enqP_dummy_2_2;
  MOD_Wire<tUInt8> INST_inFifo_enqP_lat_0;
  MOD_Wire<tUInt8> INST_inFifo_enqP_lat_1;
  MOD_Wire<tUInt8> INST_inFifo_enqP_lat_2;
  MOD_Reg<tUInt8> INST_inFifo_enqP_rl;
  MOD_Reg<tUInt8> INST_inFifo_tempData_dummy2_0;
  MOD_Reg<tUInt8> INST_inFifo_tempData_dummy2_1;
  MOD_Wire<tUWide> INST_inFifo_tempData_dummy_0_0;
  MOD_Wire<tUWide> INST_inFifo_tempData_dummy_0_1;
  MOD_Wire<tUWide> INST_inFifo_tempData_dummy_1_0;
  MOD_Wire<tUWide> INST_inFifo_tempData_dummy_1_1;
  MOD_Wire<tUWide> INST_inFifo_tempData_lat_0;
  MOD_Wire<tUWide> INST_inFifo_tempData_lat_1;
  MOD_Reg<tUWide> INST_inFifo_tempData_rl;
  MOD_Reg<tUInt8> INST_inFifo_tempEnqP_dummy2_0;
  MOD_Reg<tUInt8> INST_inFifo_tempEnqP_dummy2_1;
  MOD_Wire<tUInt8> INST_inFifo_tempEnqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_inFifo_tempEnqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_inFifo_tempEnqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_inFifo_tempEnqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_inFifo_tempEnqP_lat_0;
  MOD_Wire<tUInt8> INST_inFifo_tempEnqP_lat_1;
  MOD_Reg<tUInt8> INST_inFifo_tempEnqP_rl;
  MOD_Reg<tUWide> INST_outFifo_data_0;
  MOD_Reg<tUWide> INST_outFifo_data_1;
  MOD_Reg<tUInt8> INST_outFifo_deqEn_dummy2_0;
  MOD_Reg<tUInt8> INST_outFifo_deqEn_dummy2_1;
  MOD_Reg<tUInt8> INST_outFifo_deqEn_dummy2_2;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_dummy_0_0;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_dummy_0_1;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_dummy_0_2;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_dummy_1_0;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_dummy_1_1;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_dummy_1_2;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_dummy_2_0;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_dummy_2_1;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_dummy_2_2;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_lat_0;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_lat_1;
  MOD_Wire<tUInt8> INST_outFifo_deqEn_lat_2;
  MOD_Reg<tUInt8> INST_outFifo_deqEn_rl;
  MOD_Reg<tUInt8> INST_outFifo_deqP_dummy2_0;
  MOD_Reg<tUInt8> INST_outFifo_deqP_dummy2_1;
  MOD_Reg<tUInt8> INST_outFifo_deqP_dummy2_2;
  MOD_Wire<tUInt8> INST_outFifo_deqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_outFifo_deqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_outFifo_deqP_dummy_0_2;
  MOD_Wire<tUInt8> INST_outFifo_deqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_outFifo_deqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_outFifo_deqP_dummy_1_2;
  MOD_Wire<tUInt8> INST_outFifo_deqP_dummy_2_0;
  MOD_Wire<tUInt8> INST_outFifo_deqP_dummy_2_1;
  MOD_Wire<tUInt8> INST_outFifo_deqP_dummy_2_2;
  MOD_Wire<tUInt8> INST_outFifo_deqP_lat_0;
  MOD_Wire<tUInt8> INST_outFifo_deqP_lat_1;
  MOD_Wire<tUInt8> INST_outFifo_deqP_lat_2;
  MOD_Reg<tUInt8> INST_outFifo_deqP_rl;
  MOD_Reg<tUInt8> INST_outFifo_enqEn_dummy2_0;
  MOD_Reg<tUInt8> INST_outFifo_enqEn_dummy2_1;
  MOD_Reg<tUInt8> INST_outFifo_enqEn_dummy2_2;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_dummy_0_0;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_dummy_0_1;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_dummy_0_2;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_dummy_1_0;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_dummy_1_1;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_dummy_1_2;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_dummy_2_0;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_dummy_2_1;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_dummy_2_2;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_lat_0;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_lat_1;
  MOD_Wire<tUInt8> INST_outFifo_enqEn_lat_2;
  MOD_Reg<tUInt8> INST_outFifo_enqEn_rl;
  MOD_Reg<tUInt8> INST_outFifo_enqP_dummy2_0;
  MOD_Reg<tUInt8> INST_outFifo_enqP_dummy2_1;
  MOD_Reg<tUInt8> INST_outFifo_enqP_dummy2_2;
  MOD_Wire<tUInt8> INST_outFifo_enqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_outFifo_enqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_outFifo_enqP_dummy_0_2;
  MOD_Wire<tUInt8> INST_outFifo_enqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_outFifo_enqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_outFifo_enqP_dummy_1_2;
  MOD_Wire<tUInt8> INST_outFifo_enqP_dummy_2_0;
  MOD_Wire<tUInt8> INST_outFifo_enqP_dummy_2_1;
  MOD_Wire<tUInt8> INST_outFifo_enqP_dummy_2_2;
  MOD_Wire<tUInt8> INST_outFifo_enqP_lat_0;
  MOD_Wire<tUInt8> INST_outFifo_enqP_lat_1;
  MOD_Wire<tUInt8> INST_outFifo_enqP_lat_2;
  MOD_Reg<tUInt8> INST_outFifo_enqP_rl;
  MOD_Reg<tUInt8> INST_outFifo_tempData_dummy2_0;
  MOD_Reg<tUInt8> INST_outFifo_tempData_dummy2_1;
  MOD_Wire<tUWide> INST_outFifo_tempData_dummy_0_0;
  MOD_Wire<tUWide> INST_outFifo_tempData_dummy_0_1;
  MOD_Wire<tUWide> INST_outFifo_tempData_dummy_1_0;
  MOD_Wire<tUWide> INST_outFifo_tempData_dummy_1_1;
  MOD_Wire<tUWide> INST_outFifo_tempData_lat_0;
  MOD_Wire<tUWide> INST_outFifo_tempData_lat_1;
  MOD_Reg<tUWide> INST_outFifo_tempData_rl;
  MOD_Reg<tUInt8> INST_outFifo_tempEnqP_dummy2_0;
  MOD_Reg<tUInt8> INST_outFifo_tempEnqP_dummy2_1;
  MOD_Wire<tUInt8> INST_outFifo_tempEnqP_dummy_0_0;
  MOD_Wire<tUInt8> INST_outFifo_tempEnqP_dummy_0_1;
  MOD_Wire<tUInt8> INST_outFifo_tempEnqP_dummy_1_0;
  MOD_Wire<tUInt8> INST_outFifo_tempEnqP_dummy_1_1;
  MOD_Wire<tUInt8> INST_outFifo_tempEnqP_lat_0;
  MOD_Wire<tUInt8> INST_outFifo_tempEnqP_lat_1;
  MOD_Reg<tUInt8> INST_outFifo_tempEnqP_rl;
 
 /* Constructor */
 public:
  MOD_mkFftPipelined(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
  tUInt8 PORT_RST_N;
 
 /* Port definitions */
 public:
  tUWide PORT_enq_in;
  tUWide PORT_deq;
 
 /* Publicly accessible definitions */
 public:
  tUInt8 DEF_outFifo_deqEn_rl__h77711;
  tUInt8 DEF_inFifo_enqEn_rl__h30725;
  tUInt8 DEF_fifo2_deqEn_rl__h168965;
  tUInt8 DEF_fifo2_enqEn_rl__h167609;
  tUInt8 DEF_fifo1_deqEn_rl__h123338;
  tUInt8 DEF_fifo1_enqEn_rl__h121982;
  tUInt8 DEF_outFifo_enqEn_rl__h76355;
  tUInt8 DEF_inFifo_deqEn_rl__h32084;
 
 /* Local definitions */
 private:
  tUInt8 DEF_IF_outFifo_tempEnqP_lat_0_whas__78_THEN_outFif_ETC___d183;
  tUInt8 DEF_outFifo_deqEn_lat_1_whas____d160;
  tUInt8 DEF_outFifo_enqEn_lat_1_whas____d150;
  tUInt8 DEF_IF_inFifo_tempEnqP_lat_0_whas__1_THEN_inFifo_t_ETC___d56;
  tUInt8 DEF_inFifo_deqEn_lat_1_whas____d33;
  tUInt8 DEF_inFifo_enqEn_lat_1_whas____d23;
  tUWide DEF_outFifo_tempData_lat_1_wget____d169;
  tUWide DEF_outFifo_tempData_lat_0_wget____d171;
  tUWide DEF_inFifo_tempData_lat_1_wget____d42;
  tUWide DEF_inFifo_tempData_lat_0_wget____d44;
  tUInt8 DEF_outFifo_tempEnqP_rl___d181;
  tUInt8 DEF_outFifo_tempEnqP_lat_0_wget____d179;
  tUInt8 DEF_inFifo_tempEnqP_rl___d54;
  tUInt8 DEF_inFifo_tempEnqP_lat_0_wget____d52;
  tUInt8 DEF_outFifo_tempEnqP_lat_0_whas____d178;
  tUInt8 DEF_outFifo_deqEn_lat_1_wget____d161;
  tUInt8 DEF_outFifo_deqEn_lat_0_whas____d162;
  tUInt8 DEF_outFifo_deqEn_lat_0_wget____d163;
  tUInt8 DEF_outFifo_enqEn_lat_1_wget____d151;
  tUInt8 DEF_outFifo_enqEn_lat_0_whas____d152;
  tUInt8 DEF_outFifo_enqEn_lat_0_wget____d153;
  tUInt8 DEF_inFifo_tempEnqP_lat_0_whas____d51;
  tUInt8 DEF_inFifo_deqEn_lat_1_wget____d34;
  tUInt8 DEF_inFifo_deqEn_lat_0_whas____d35;
  tUInt8 DEF_inFifo_deqEn_lat_0_wget____d36;
  tUInt8 DEF_inFifo_enqEn_lat_1_wget____d24;
  tUInt8 DEF_inFifo_enqEn_lat_0_whas____d25;
  tUInt8 DEF_inFifo_enqEn_lat_0_wget____d26;
  tUInt8 DEF_x__h76018;
  tUInt8 DEF_x__h30385;
  tUInt8 DEF_outFifo_tempEnqP_lat_0_wget__79_BIT_3___d180;
  tUInt8 DEF_inFifo_tempEnqP_lat_0_wget__2_BIT_3___d53;
  tUWide DEF_IF_outFifo_tempData_dummy2_1_46_THEN_IF_outFif_ETC___d247;
  tUWide DEF_IF_outFifo_tempData_lat_0_whas__70_THEN_outFif_ETC___d173;
  tUWide DEF_IF_outFifo_tempData_lat_1_whas__68_THEN_outFif_ETC___d174;
  tUWide DEF_IF_inFifo_tempData_dummy2_1_19_THEN_IF_inFifo__ETC___d120;
  tUWide DEF_IF_inFifo_tempData_lat_0_whas__3_THEN_inFifo_t_ETC___d46;
  tUWide DEF_IF_inFifo_tempData_lat_1_whas__1_THEN_inFifo_t_ETC___d47;
  tUInt8 DEF_IF_outFifo_tempEnqP_lat_0_whas__78_THEN_outFif_ETC___d193;
  tUInt8 DEF_IF_outFifo_deqP_lat_1_whas__40_THEN_outFifo_de_ETC___d146;
  tUInt8 DEF_IF_outFifo_enqP_lat_1_whas__30_THEN_outFifo_en_ETC___d136;
  tUInt8 DEF_IF_inFifo_tempEnqP_lat_0_whas__1_THEN_inFifo_t_ETC___d66;
  tUInt8 DEF_IF_inFifo_deqP_lat_1_whas__3_THEN_inFifo_deqP__ETC___d19;
  tUInt8 DEF_IF_inFifo_enqP_lat_1_whas_THEN_inFifo_enqP_lat_ETC___d9;
  tUInt8 DEF__0_CONCAT_DONTCARE___d123;
  tUInt8 DEF_x__h76019;
  tUInt8 DEF_x__h30386;
  tUInt8 DEF_IF_fifo2_tempEnqP_lat_0_whas__30_THEN_fifo2_te_ETC___d435;
  tUInt8 DEF_fifo2_deqEn_lat_1_whas____d412;
  tUInt8 DEF_fifo2_enqEn_lat_1_whas____d402;
  tUInt8 DEF_IF_fifo1_tempEnqP_lat_0_whas__04_THEN_fifo1_te_ETC___d309;
  tUInt8 DEF_fifo1_deqEn_lat_1_whas____d286;
  tUInt8 DEF_fifo1_enqEn_lat_1_whas____d276;
  tUWide DEF_fifo2_tempData_rl__h157545;
  tUWide DEF_fifo2_tempData_lat_1_wget____d421;
  tUWide DEF_fifo2_tempData_lat_0_wget____d423;
  tUWide DEF_fifo2_data_1__h315202;
  tUWide DEF_fifo2_data_0__h315177;
  tUWide DEF_fifo1_tempData_rl__h111918;
  tUWide DEF_fifo1_tempData_lat_1_wget____d295;
  tUWide DEF_fifo1_tempData_lat_0_wget____d297;
  tUWide DEF_fifo1_data_1__h270756;
  tUWide DEF_fifo1_data_0__h270731;
  tUWide DEF_outFifo_tempData_rl__h66291;
  tUWide DEF_outFifo_data_1__h340424;
  tUWide DEF_outFifo_data_0__h340399;
  tUWide DEF_inFifo_tempData_rl__h20658;
  tUWide DEF_inFifo_data_1__h226308;
  tUWide DEF_inFifo_data_0__h226283;
  tUInt8 DEF_fifo2_tempEnqP_rl___d433;
  tUInt8 DEF_fifo2_tempEnqP_lat_0_wget____d431;
  tUInt8 DEF_fifo1_tempEnqP_rl___d307;
  tUInt8 DEF_fifo1_tempEnqP_lat_0_wget____d305;
  tUInt8 DEF_upd__h274439;
  tUInt8 DEF_upd__h168091;
  tUInt8 DEF_upd__h168112;
  tUInt8 DEF_upd__h273885;
  tUInt8 DEF_upd__h167838;
  tUInt8 DEF_upd__h167859;
  tUInt8 DEF_upd__h229993;
  tUInt8 DEF_upd__h122464;
  tUInt8 DEF_upd__h122485;
  tUInt8 DEF_upd__h229437;
  tUInt8 DEF_upd__h122211;
  tUInt8 DEF_upd__h122232;
  tUInt8 DEF_upd__h330303;
  tUInt8 DEF_upd__h76837;
  tUInt8 DEF_upd__h76858;
  tUInt8 DEF_upd__h318331;
  tUInt8 DEF_upd__h76584;
  tUInt8 DEF_upd__h76605;
  tUInt8 DEF_upd__h185445;
  tUInt8 DEF_upd__h31207;
  tUInt8 DEF_upd__h31228;
  tUInt8 DEF_upd__h327313;
  tUInt8 DEF_upd__h30954;
  tUInt8 DEF_upd__h30975;
  tUInt8 DEF_fifo2_tempEnqP_lat_0_whas____d430;
  tUInt8 DEF_fifo2_deqEn_lat_1_wget____d413;
  tUInt8 DEF_fifo2_deqEn_lat_0_whas____d414;
  tUInt8 DEF_fifo2_deqEn_lat_0_wget____d415;
  tUInt8 DEF_fifo2_enqEn_lat_1_wget____d403;
  tUInt8 DEF_fifo2_enqEn_lat_0_whas____d404;
  tUInt8 DEF_fifo2_enqEn_lat_0_wget____d405;
  tUInt8 DEF_fifo1_tempEnqP_lat_0_whas____d304;
  tUInt8 DEF_fifo1_deqEn_lat_1_wget____d287;
  tUInt8 DEF_fifo1_deqEn_lat_0_whas____d288;
  tUInt8 DEF_fifo1_deqEn_lat_0_wget____d289;
  tUInt8 DEF_fifo1_enqEn_lat_1_wget____d277;
  tUInt8 DEF_fifo1_enqEn_lat_0_whas____d278;
  tUInt8 DEF_fifo1_enqEn_lat_0_wget____d279;
  tUInt8 DEF_x__h167273;
  tUInt8 DEF_x__h167272;
  tUInt8 DEF_x__h121646;
  tUInt8 DEF_x__h121645;
  tUInt8 DEF_fifo2_tempEnqP_lat_0_wget__31_BIT_3___d432;
  tUInt8 DEF_fifo1_tempEnqP_lat_0_wget__05_BIT_3___d306;
  tUWide DEF_IF_fifo2_tempData_dummy2_1_98_THEN_IF_fifo2_te_ETC___d499;
  tUWide DEF_IF_fifo2_tempData_lat_0_whas__22_THEN_fifo2_te_ETC___d425;
  tUWide DEF_IF_fifo2_tempData_lat_1_whas__20_THEN_fifo2_te_ETC___d426;
  tUWide DEF_IF_fifo1_tempData_dummy2_1_72_THEN_IF_fifo1_te_ETC___d373;
  tUWide DEF_IF_fifo1_tempData_lat_0_whas__96_THEN_fifo1_te_ETC___d299;
  tUWide DEF_IF_fifo1_tempData_lat_1_whas__94_THEN_fifo1_te_ETC___d300;
  tUInt8 DEF_IF_fifo2_tempEnqP_lat_0_whas__30_THEN_fifo2_te_ETC___d445;
  tUInt8 DEF_IF_fifo2_deqP_lat_1_whas__92_THEN_fifo2_deqP_l_ETC___d398;
  tUInt8 DEF_IF_fifo2_enqP_lat_1_whas__82_THEN_fifo2_enqP_l_ETC___d388;
  tUInt8 DEF_IF_fifo1_tempEnqP_lat_0_whas__04_THEN_fifo1_te_ETC___d319;
  tUInt8 DEF_IF_fifo1_deqP_lat_1_whas__66_THEN_fifo1_deqP_l_ETC___d272;
  tUInt8 DEF_IF_fifo1_enqP_lat_1_whas__56_THEN_fifo1_enqP_l_ETC___d262;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2610;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2607;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1200;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1197;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1905;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1902;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d3203;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d3185;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2604;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1194;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1899;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d3167;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2601;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1191;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1896;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d3149;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2598;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1188;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1893;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d3131;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2595;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1185;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1890;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d3113;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2592;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1182;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1887;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d3095;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2589;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1179;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1884;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d3077;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2586;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1176;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1881;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d3059;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2583;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1173;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1878;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d3041;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2580;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1170;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1875;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d3023;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2577;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1167;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1872;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d3005;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2574;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1164;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1869;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d2987;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2571;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1161;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1866;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d2969;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2568;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1158;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1863;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d2951;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2565;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1155;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1860;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d2933;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2562;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1152;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1857;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d2915;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2523;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1113;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1818;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d2897;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2484;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1074;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1779;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d2879;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2445;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d1035;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1740;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d2861;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2406;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d996;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1701;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d2843;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2367;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d957;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1662;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d2825;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2328;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d918;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1623;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d2807;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2289;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d879;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1584;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d2789;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2250;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d840;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1545;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d2771;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2211;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d801;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1506;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d2753;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2172;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d762;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1467;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d2735;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2133;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d723;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1428;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d2717;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2094;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d684;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1389;
  tUWide DEF_SEL_ARR_outFifo_data_0_643_BITS_1023_TO_1016_6_ETC___d2699;
  tUWide DEF_bfly_2_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d2055;
  tUWide DEF_bfly_0_15_bfly4_2251552690364292668_SEL_ARR_in_ETC___d645;
  tUWide DEF_bfly_1_15_bfly4_2251552690364292668_SEL_ARR_fi_ETC___d1350;
 
 /* Rules */
 public:
  void RL_inFifo_enqP_canon();
  void RL_inFifo_deqP_canon();
  void RL_inFifo_enqEn_canon();
  void RL_inFifo_deqEn_canon();
  void RL_inFifo_tempData_canon();
  void RL_inFifo_tempEnqP_canon();
  void RL_inFifo_canonicalize();
  void RL_outFifo_enqP_canon();
  void RL_outFifo_deqP_canon();
  void RL_outFifo_enqEn_canon();
  void RL_outFifo_deqEn_canon();
  void RL_outFifo_tempData_canon();
  void RL_outFifo_tempEnqP_canon();
  void RL_outFifo_canonicalize();
  void RL_fifo1_enqP_canon();
  void RL_fifo1_deqP_canon();
  void RL_fifo1_enqEn_canon();
  void RL_fifo1_deqEn_canon();
  void RL_fifo1_tempData_canon();
  void RL_fifo1_tempEnqP_canon();
  void RL_fifo1_canonicalize();
  void RL_fifo2_enqP_canon();
  void RL_fifo2_deqP_canon();
  void RL_fifo2_enqEn_canon();
  void RL_fifo2_deqEn_canon();
  void RL_fifo2_tempData_canon();
  void RL_fifo2_tempEnqP_canon();
  void RL_fifo2_canonicalize();
  void RL_stage0();
  void RL_stage1();
  void RL_stage2();
 
 /* Methods */
 public:
  void METH_enq(tUWide ARG_enq_in);
  tUInt8 METH_RDY_enq();
  tUWide METH_deq();
  tUInt8 METH_RDY_deq();
 
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
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkFftPipelined &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkFftPipelined &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkFftPipelined &backing);
  void vcd_submodules(tVCDDumpType dt, unsigned int levels, MOD_mkFftPipelined &backing);
};

#endif /* ifndef __mkFftPipelined_h__ */
