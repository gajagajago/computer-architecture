/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Mon May 17 21:33:31 KST 2021
 * 
 */

/* Generation options: */
#ifndef __mkProc_h__
#define __mkProc_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"
#include "mkCsrFile.h"
#include "mkDMemory.h"
#include "mkIMemory.h"
#include "mkRFile.h"
#include "module_exec.h"
#include "module_decode.h"


/* Class declaration for the mkProc module */
class MOD_mkProc : public Module {
 
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
  MOD_mkCsrFile INST_csrf;
  MOD_Reg<tUWide> INST_d2e_data_0;
  MOD_Reg<tUWide> INST_d2e_data_1;
  MOD_Reg<tUInt8> INST_d2e_deqP_ehrReg;
  MOD_Wire<tUInt8> INST_d2e_deqP_ignored_wires_0;
  MOD_Wire<tUInt8> INST_d2e_deqP_ignored_wires_1;
  MOD_Reg<tUInt8> INST_d2e_deqP_virtual_reg_0;
  MOD_Reg<tUInt8> INST_d2e_deqP_virtual_reg_1;
  MOD_Wire<tUInt8> INST_d2e_deqP_wires_0;
  MOD_Wire<tUInt8> INST_d2e_deqP_wires_1;
  MOD_Reg<tUInt8> INST_d2e_empty_ehrReg;
  MOD_Wire<tUInt8> INST_d2e_empty_ignored_wires_0;
  MOD_Wire<tUInt8> INST_d2e_empty_ignored_wires_1;
  MOD_Wire<tUInt8> INST_d2e_empty_ignored_wires_2;
  MOD_Reg<tUInt8> INST_d2e_empty_virtual_reg_0;
  MOD_Reg<tUInt8> INST_d2e_empty_virtual_reg_1;
  MOD_Reg<tUInt8> INST_d2e_empty_virtual_reg_2;
  MOD_Wire<tUInt8> INST_d2e_empty_wires_0;
  MOD_Wire<tUInt8> INST_d2e_empty_wires_1;
  MOD_Wire<tUInt8> INST_d2e_empty_wires_2;
  MOD_Reg<tUInt8> INST_d2e_enqP_ehrReg;
  MOD_Wire<tUInt8> INST_d2e_enqP_ignored_wires_0;
  MOD_Wire<tUInt8> INST_d2e_enqP_ignored_wires_1;
  MOD_Reg<tUInt8> INST_d2e_enqP_virtual_reg_0;
  MOD_Reg<tUInt8> INST_d2e_enqP_virtual_reg_1;
  MOD_Wire<tUInt8> INST_d2e_enqP_wires_0;
  MOD_Wire<tUInt8> INST_d2e_enqP_wires_1;
  MOD_Reg<tUInt8> INST_d2e_full_ehrReg;
  MOD_Wire<tUInt8> INST_d2e_full_ignored_wires_0;
  MOD_Wire<tUInt8> INST_d2e_full_ignored_wires_1;
  MOD_Wire<tUInt8> INST_d2e_full_ignored_wires_2;
  MOD_Reg<tUInt8> INST_d2e_full_virtual_reg_0;
  MOD_Reg<tUInt8> INST_d2e_full_virtual_reg_1;
  MOD_Reg<tUInt8> INST_d2e_full_virtual_reg_2;
  MOD_Wire<tUInt8> INST_d2e_full_wires_0;
  MOD_Wire<tUInt8> INST_d2e_full_wires_1;
  MOD_Wire<tUInt8> INST_d2e_full_wires_2;
  MOD_mkDMemory INST_dMem;
  MOD_Reg<tUInt8> INST_eEpoch;
  MOD_Reg<tUInt32> INST_execRedirect_data_0_ehrReg;
  MOD_Wire<tUInt32> INST_execRedirect_data_0_ignored_wires_0;
  MOD_Wire<tUInt32> INST_execRedirect_data_0_ignored_wires_1;
  MOD_Reg<tUInt8> INST_execRedirect_data_0_virtual_reg_0;
  MOD_Reg<tUInt8> INST_execRedirect_data_0_virtual_reg_1;
  MOD_Wire<tUInt32> INST_execRedirect_data_0_wires_0;
  MOD_Wire<tUInt32> INST_execRedirect_data_0_wires_1;
  MOD_Wire<tUInt8> INST_execRedirect_deqP_ignored_wires_0;
  MOD_Wire<tUInt8> INST_execRedirect_deqP_ignored_wires_1;
  MOD_Reg<tUInt8> INST_execRedirect_deqP_virtual_reg_0;
  MOD_Reg<tUInt8> INST_execRedirect_deqP_virtual_reg_1;
  MOD_Wire<tUInt8> INST_execRedirect_deqP_wires_0;
  MOD_Wire<tUInt8> INST_execRedirect_deqP_wires_1;
  MOD_Reg<tUInt8> INST_execRedirect_empty_ehrReg;
  MOD_Wire<tUInt8> INST_execRedirect_empty_ignored_wires_0;
  MOD_Wire<tUInt8> INST_execRedirect_empty_ignored_wires_1;
  MOD_Wire<tUInt8> INST_execRedirect_empty_ignored_wires_2;
  MOD_Reg<tUInt8> INST_execRedirect_empty_virtual_reg_0;
  MOD_Reg<tUInt8> INST_execRedirect_empty_virtual_reg_1;
  MOD_Reg<tUInt8> INST_execRedirect_empty_virtual_reg_2;
  MOD_Wire<tUInt8> INST_execRedirect_empty_wires_0;
  MOD_Wire<tUInt8> INST_execRedirect_empty_wires_1;
  MOD_Wire<tUInt8> INST_execRedirect_empty_wires_2;
  MOD_Wire<tUInt8> INST_execRedirect_enqP_ignored_wires_0;
  MOD_Wire<tUInt8> INST_execRedirect_enqP_ignored_wires_1;
  MOD_Reg<tUInt8> INST_execRedirect_enqP_virtual_reg_0;
  MOD_Reg<tUInt8> INST_execRedirect_enqP_virtual_reg_1;
  MOD_Wire<tUInt8> INST_execRedirect_enqP_wires_0;
  MOD_Wire<tUInt8> INST_execRedirect_enqP_wires_1;
  MOD_Reg<tUInt8> INST_execRedirect_full_ehrReg;
  MOD_Wire<tUInt8> INST_execRedirect_full_ignored_wires_0;
  MOD_Wire<tUInt8> INST_execRedirect_full_ignored_wires_1;
  MOD_Wire<tUInt8> INST_execRedirect_full_ignored_wires_2;
  MOD_Reg<tUInt8> INST_execRedirect_full_virtual_reg_0;
  MOD_Reg<tUInt8> INST_execRedirect_full_virtual_reg_1;
  MOD_Reg<tUInt8> INST_execRedirect_full_virtual_reg_2;
  MOD_Wire<tUInt8> INST_execRedirect_full_wires_0;
  MOD_Wire<tUInt8> INST_execRedirect_full_wires_1;
  MOD_Wire<tUInt8> INST_execRedirect_full_wires_2;
  MOD_Reg<tUWide> INST_f2d_data_0;
  MOD_Reg<tUWide> INST_f2d_data_1;
  MOD_Reg<tUInt8> INST_f2d_deqP_ehrReg;
  MOD_Wire<tUInt8> INST_f2d_deqP_ignored_wires_0;
  MOD_Wire<tUInt8> INST_f2d_deqP_ignored_wires_1;
  MOD_Reg<tUInt8> INST_f2d_deqP_virtual_reg_0;
  MOD_Reg<tUInt8> INST_f2d_deqP_virtual_reg_1;
  MOD_Wire<tUInt8> INST_f2d_deqP_wires_0;
  MOD_Wire<tUInt8> INST_f2d_deqP_wires_1;
  MOD_Reg<tUInt8> INST_f2d_empty_ehrReg;
  MOD_Wire<tUInt8> INST_f2d_empty_ignored_wires_0;
  MOD_Wire<tUInt8> INST_f2d_empty_ignored_wires_1;
  MOD_Wire<tUInt8> INST_f2d_empty_ignored_wires_2;
  MOD_Reg<tUInt8> INST_f2d_empty_virtual_reg_0;
  MOD_Reg<tUInt8> INST_f2d_empty_virtual_reg_1;
  MOD_Reg<tUInt8> INST_f2d_empty_virtual_reg_2;
  MOD_Wire<tUInt8> INST_f2d_empty_wires_0;
  MOD_Wire<tUInt8> INST_f2d_empty_wires_1;
  MOD_Wire<tUInt8> INST_f2d_empty_wires_2;
  MOD_Reg<tUInt8> INST_f2d_enqP_ehrReg;
  MOD_Wire<tUInt8> INST_f2d_enqP_ignored_wires_0;
  MOD_Wire<tUInt8> INST_f2d_enqP_ignored_wires_1;
  MOD_Reg<tUInt8> INST_f2d_enqP_virtual_reg_0;
  MOD_Reg<tUInt8> INST_f2d_enqP_virtual_reg_1;
  MOD_Wire<tUInt8> INST_f2d_enqP_wires_0;
  MOD_Wire<tUInt8> INST_f2d_enqP_wires_1;
  MOD_Reg<tUInt8> INST_f2d_full_ehrReg;
  MOD_Wire<tUInt8> INST_f2d_full_ignored_wires_0;
  MOD_Wire<tUInt8> INST_f2d_full_ignored_wires_1;
  MOD_Wire<tUInt8> INST_f2d_full_ignored_wires_2;
  MOD_Reg<tUInt8> INST_f2d_full_virtual_reg_0;
  MOD_Reg<tUInt8> INST_f2d_full_virtual_reg_1;
  MOD_Reg<tUInt8> INST_f2d_full_virtual_reg_2;
  MOD_Wire<tUInt8> INST_f2d_full_wires_0;
  MOD_Wire<tUInt8> INST_f2d_full_wires_1;
  MOD_Wire<tUInt8> INST_f2d_full_wires_2;
  MOD_Reg<tUInt8> INST_fEpoch;
  MOD_mkIMemory INST_iMem;
  MOD_Reg<tUInt32> INST_pc;
  MOD_mkRFile INST_rf;
  MOD_Reg<tUInt8> INST_stat;
  MOD_Reg<tUInt8> INST_statRedirect_data_0_ehrReg;
  MOD_Wire<tUInt8> INST_statRedirect_data_0_ignored_wires_0;
  MOD_Wire<tUInt8> INST_statRedirect_data_0_ignored_wires_1;
  MOD_Reg<tUInt8> INST_statRedirect_data_0_virtual_reg_0;
  MOD_Reg<tUInt8> INST_statRedirect_data_0_virtual_reg_1;
  MOD_Wire<tUInt8> INST_statRedirect_data_0_wires_0;
  MOD_Wire<tUInt8> INST_statRedirect_data_0_wires_1;
  MOD_Wire<tUInt8> INST_statRedirect_deqP_ignored_wires_0;
  MOD_Wire<tUInt8> INST_statRedirect_deqP_ignored_wires_1;
  MOD_Reg<tUInt8> INST_statRedirect_deqP_virtual_reg_0;
  MOD_Reg<tUInt8> INST_statRedirect_deqP_virtual_reg_1;
  MOD_Wire<tUInt8> INST_statRedirect_deqP_wires_0;
  MOD_Wire<tUInt8> INST_statRedirect_deqP_wires_1;
  MOD_Reg<tUInt8> INST_statRedirect_empty_ehrReg;
  MOD_Wire<tUInt8> INST_statRedirect_empty_ignored_wires_0;
  MOD_Wire<tUInt8> INST_statRedirect_empty_ignored_wires_1;
  MOD_Wire<tUInt8> INST_statRedirect_empty_ignored_wires_2;
  MOD_Reg<tUInt8> INST_statRedirect_empty_virtual_reg_0;
  MOD_Reg<tUInt8> INST_statRedirect_empty_virtual_reg_1;
  MOD_Reg<tUInt8> INST_statRedirect_empty_virtual_reg_2;
  MOD_Wire<tUInt8> INST_statRedirect_empty_wires_0;
  MOD_Wire<tUInt8> INST_statRedirect_empty_wires_1;
  MOD_Wire<tUInt8> INST_statRedirect_empty_wires_2;
  MOD_Wire<tUInt8> INST_statRedirect_enqP_ignored_wires_0;
  MOD_Wire<tUInt8> INST_statRedirect_enqP_ignored_wires_1;
  MOD_Reg<tUInt8> INST_statRedirect_enqP_virtual_reg_0;
  MOD_Reg<tUInt8> INST_statRedirect_enqP_virtual_reg_1;
  MOD_Wire<tUInt8> INST_statRedirect_enqP_wires_0;
  MOD_Wire<tUInt8> INST_statRedirect_enqP_wires_1;
  MOD_Reg<tUInt8> INST_statRedirect_full_ehrReg;
  MOD_Wire<tUInt8> INST_statRedirect_full_ignored_wires_0;
  MOD_Wire<tUInt8> INST_statRedirect_full_ignored_wires_1;
  MOD_Wire<tUInt8> INST_statRedirect_full_ignored_wires_2;
  MOD_Reg<tUInt8> INST_statRedirect_full_virtual_reg_0;
  MOD_Reg<tUInt8> INST_statRedirect_full_virtual_reg_1;
  MOD_Reg<tUInt8> INST_statRedirect_full_virtual_reg_2;
  MOD_Wire<tUInt8> INST_statRedirect_full_wires_0;
  MOD_Wire<tUInt8> INST_statRedirect_full_wires_1;
  MOD_Wire<tUInt8> INST_statRedirect_full_wires_2;
  MOD_module_exec INST_instance_exec_1;
  MOD_module_decode INST_instance_decode_0;
 
 /* Constructor */
 public:
  MOD_mkProc(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
  tUInt8 PORT_RST_N;
 
 /* Port definitions */
 public:
  tUInt8 PORT_EN_dMemInit_request_put;
  tUInt8 PORT_EN_iMemInit_request_put;
  tUWide PORT_dMemInit_request_put;
  tUWide PORT_iMemInit_request_put;
 
 /* Publicly accessible definitions */
 public:
  tUInt8 DEF_WILL_FIRE_dMemInit_request_put;
  tUInt8 DEF_WILL_FIRE_iMemInit_request_put;
  tUInt8 DEF_dMem_RDY_init_request_put____d124;
  tUInt8 DEF_iMem_RDY_init_request_put____d123;
  tUInt8 DEF_x__h25460;
  tUInt8 DEF_exec_85_BIT_1___d386;
  tUInt8 DEF_d2e_deqP_virtual_reg_0_read____d279;
  tUInt8 DEF_x__h22232;
  tUInt8 DEF_f2d_deqP_virtual_reg_0_read____d190;
  tUWide DEF_exec___d385;
  tUWide DEF_SEL_ARR_d2e_data_0_74_BITS_139_TO_136_87_d2e_d_ETC___d364;
  tUInt32 DEF_rVal1__h25710;
  tUInt32 DEF_rVal2__h25711;
  tUInt32 DEF_pc__h24993;
  tUInt32 DEF_ppc__h24994;
  tUInt32 DEF_csrVal__h25712;
  tUWide DEF_d2e_data_1___d276;
  tUWide DEF_d2e_data_0___d274;
  tUWide DEF_f2d_data_1___d187;
  tUWide DEF_f2d_data_0___d185;
  tUInt32 DEF_x__h26495;
  tUInt32 DEF_idx__h26492;
  tUInt32 DEF_x__h26434;
  tUInt8 DEF_rindx__h26431;
  tUInt32 DEF_x__h26377;
  tUInt8 DEF_rindx__h26374;
  tUInt8 DEF_d2e_full_wires_0_whas____d117;
  tUInt8 DEF_d2e_full_wires_0_wget____d118;
  tUInt8 DEF_d2e_full_ehrReg__h17182;
  tUInt8 DEF_d2e_empty_ehrReg__h16059;
  tUInt8 DEF_d2e_deqP_virtual_reg_1_read____d262;
  tUInt8 DEF_f2d_full_wires_0_whas____d83;
  tUInt8 DEF_f2d_full_wires_0_wget____d84;
  tUInt8 DEF_f2d_full_ehrReg__h13119;
  tUInt8 DEF_f2d_empty_ehrReg__h11996;
  tUInt8 DEF_f2d_deqP_virtual_reg_1_read____d174;
  tUInt8 DEF_execRedirect_full_ehrReg__h9056;
  tUInt8 DEF_statRedirect_empty_wires_0_whas____d12;
  tUInt8 DEF_statRedirect_empty_wires_0_wget____d13;
  tUInt8 DEF_statRedirect_empty_ehrReg__h3520;
  tUInt8 DEF_eEpoch__h22488;
  tUInt8 DEF_csrf_started____d140;
  tUInt8 DEF_dMem_init_done____d128;
  tUInt8 DEF_iMem_init_done____d126;
  tUInt32 DEF_x__h26325;
  tUInt32 DEF__read_pc__h25686;
  tUInt32 DEF__read_ppc__h25687;
  tUInt32 DEF_x__h26328;
  tUInt32 DEF__read_pc__h25694;
  tUInt32 DEF__read_ppc__h25695;
  tUInt32 DEF_x__h26221;
  tUInt32 DEF_x__h26224;
  tUInt8 DEF_x__h25907;
  tUInt8 DEF_x__h26010;
  tUInt8 DEF_x__h26113;
  tUInt8 DEF_x__h25910;
  tUInt8 DEF_x__h26013;
  tUInt8 DEF_x__h26116;
  tUInt8 DEF_n__read__h25244;
  tUInt32 DEF_SEL_ARR_d2e_data_0_74_BITS_96_TO_65_56_d2e_dat_ETC___d359;
  tUInt8 DEF_n__read__h22016;
  tUInt32 DEF_SEL_ARR_d2e_data_0_74_BITS_109_TO_98_44_d2e_da_ETC___d347;
  tUInt8 DEF_SEL_ARR_d2e_data_0_74_BITS_115_TO_111_31_d2e_d_ETC___d334;
  tUInt8 DEF_SEL_ARR_d2e_data_0_74_BITS_127_TO_123_06_d2e_d_ETC___d309;
  tUInt8 DEF_SEL_ARR_d2e_data_0_74_BITS_121_TO_117_19_d2e_d_ETC___d322;
  tUInt8 DEF_SEL_ARR_d2e_data_0_74_BITS_139_TO_136_87_d2e_d_ETC___d290;
  tUInt8 DEF_SEL_ARR_d2e_data_0_74_BITS_135_TO_132_91_d2e_d_ETC___d294;
  tUInt8 DEF_SEL_ARR_d2e_data_0_74_BITS_131_TO_129_95_d2e_d_ETC___d298;
  tUInt8 DEF_SEL_ARR_NOT_d2e_data_0_74_BIT_97_49_50_NOT_d2e_ETC___d354;
  tUInt8 DEF_SEL_ARR_NOT_d2e_data_0_74_BIT_110_37_38_NOT_d2_ETC___d342;
  tUInt8 DEF_SEL_ARR_NOT_d2e_data_0_74_BIT_116_24_25_NOT_d2_ETC___d329;
  tUInt8 DEF_SEL_ARR_NOT_d2e_data_0_74_BIT_122_12_13_NOT_d2_ETC___d317;
  tUInt8 DEF_SEL_ARR_NOT_d2e_data_0_74_BIT_128_99_00_NOT_d2_ETC___d304;
  tUInt8 DEF_SEL_ARR_d2e_data_0_74_BIT_64_75_d2e_data_1_76__ETC___d283;
  tUInt8 DEF_SEL_ARR_f2d_data_0_85_BIT_0_86_f2d_data_1_87_B_ETC___d194;
  tUInt8 DEF_SEL_ARR_d2e_data_0_74_BIT_64_75_d2e_data_1_76__ETC___d284;
  tUInt8 DEF_SEL_ARR_f2d_data_0_85_BIT_0_86_f2d_data_1_87_B_ETC___d196;
  tUInt8 DEF_NOT_d2e_deqP_virtual_reg_1_read__62___d263;
  tUInt8 DEF_NOT_f2d_deqP_virtual_reg_1_read__74___d175;
  tUWide DEF_SEL_ARR_d2e_data_0_74_BITS_131_TO_129_95_d2e_d_ETC___d363;
 
 /* Local definitions */
 private:
  tUInt8 DEF_IF_d2e_deqP_wires_0_whas__8_THEN_d2e_deqP_wire_ETC___d101;
  tUInt8 DEF_IF_d2e_enqP_wires_0_whas__1_THEN_d2e_enqP_wire_ETC___d94;
  tUInt8 DEF_def__h24481;
  tUInt8 DEF_d2e_enqP_virtual_reg_0_read____d214;
  tUInt8 DEF_NOT_d2e_enqP_virtual_reg_1_read__12_13_AND_NOT_ETC___d217;
  tUInt8 DEF_IF_f2d_deqP_wires_0_whas__4_THEN_f2d_deqP_wire_ETC___d67;
  tUInt8 DEF_IF_f2d_enqP_wires_0_whas__7_THEN_f2d_enqP_wire_ETC___d60;
  tUInt8 DEF_def__h21239;
  tUInt8 DEF_f2d_enqP_virtual_reg_0_read____d161;
  tUInt8 DEF_NOT_f2d_enqP_virtual_reg_1_read__59_60_AND_NOT_ETC___d164;
  tUWide DEF_decode___d224;
  tUInt32 DEF_x__h26893;
  tUInt8 DEF_statRedirect_data_0_ehrReg__h1205;
  tUInt8 DEF_d2e_enqP_virtual_reg_1_read____d212;
  tUInt8 DEF_f2d_enqP_virtual_reg_1_read____d159;
  tUInt8 DEF_execRedirect_empty_wires_0_whas____d39;
  tUInt8 DEF_execRedirect_empty_wires_0_wget____d40;
  tUInt8 DEF_execRedirect_empty_ehrReg__h7933;
  tUInt8 DEF_statRedirect_full_ehrReg__h4643;
  tUInt32 DEF_IF_execRedirect_data_0_wires_0_whas__0_THEN_ex_ETC___d33;
  tUInt8 DEF_IF_statRedirect_data_0_wires_0_whas_THEN_statR_ETC___d6;
  tUInt8 DEF_IF_d2e_full_wires_0_whas__17_THEN_d2e_full_wir_ETC___d120;
  tUInt8 DEF_IF_d2e_empty_wires_0_whas__07_THEN_d2e_empty_w_ETC___d110;
  tUInt8 DEF_IF_f2d_full_wires_0_whas__3_THEN_f2d_full_wire_ETC___d86;
  tUInt8 DEF_IF_f2d_empty_wires_0_whas__3_THEN_f2d_empty_wi_ETC___d76;
  tUInt8 DEF_IF_execRedirect_full_wires_0_whas__9_THEN_exec_ETC___d52;
  tUInt8 DEF_IF_execRedirect_empty_wires_0_whas__9_THEN_exe_ETC___d42;
  tUInt8 DEF_IF_statRedirect_full_wires_0_whas__2_THEN_stat_ETC___d25;
  tUInt8 DEF_IF_statRedirect_empty_wires_0_whas__2_THEN_sta_ETC___d15;
  tUInt8 DEF_NOT_d2e_deqP_virtual_reg_1_read__62_63_AND_IF__ETC___d264;
  tUInt8 DEF_NOT_f2d_deqP_virtual_reg_1_read__74_75_AND_IF__ETC___d176;
  tUInt8 DEF_NOT_d2e_enqP_virtual_reg_1_read__12___d213;
  tUInt8 DEF_NOT_f2d_enqP_virtual_reg_1_read__59___d160;
  tUWide DEF_decode_24_BITS_74_TO_64_25_CONCAT_decode_24_BI_ETC___d255;
  tUWide DEF_iMem_req_pc_54_66_CONCAT_pc_54_CONCAT_pc_54_PL_ETC___d168;
  tUWide DEF__1_CONCAT_DONTCARE___d132;
  tUWide DEF_dMemInit_request_put_BIT_64_54_CONCAT_IF_dMemI_ETC___d457;
  tUWide DEF_NOT_exec_85_BITS_88_TO_85_07_EQ_2_08_12_CONCAT_ETC___d418;
  tUWide DEF_iMemInit_request_put_BIT_64_50_CONCAT_IF_iMemI_ETC___d453;
 
 /* Rules */
 public:
  void RL_statRedirect_data_0_canonicalize();
  void RL_statRedirect_empty_canonicalize();
  void RL_statRedirect_full_canonicalize();
  void RL_execRedirect_data_0_canonicalize();
  void RL_execRedirect_empty_canonicalize();
  void RL_execRedirect_full_canonicalize();
  void RL_f2d_enqP_canonicalize();
  void RL_f2d_deqP_canonicalize();
  void RL_f2d_empty_canonicalize();
  void RL_f2d_full_canonicalize();
  void RL_d2e_enqP_canonicalize();
  void RL_d2e_deqP_canonicalize();
  void RL_d2e_empty_canonicalize();
  void RL_d2e_full_canonicalize();
  void RL_test();
  void RL_doFetch();
  void RL_doDecode();
  void RL_doRest();
  void RL_upd_Stat();
 
 /* Methods */
 public:
  tUInt64 METH_cpuToHost();
  tUInt8 METH_RDY_cpuToHost();
  void METH_hostToCpu(tUInt32 ARG_hostToCpu_startpc);
  tUInt8 METH_RDY_hostToCpu();
  void METH_iMemInit_request_put(tUWide ARG_iMemInit_request_put);
  tUInt8 METH_RDY_iMemInit_request_put();
  tUInt8 METH_iMemInit_done();
  tUInt8 METH_RDY_iMemInit_done();
  void METH_dMemInit_request_put(tUWide ARG_dMemInit_request_put);
  tUInt8 METH_RDY_dMemInit_request_put();
  tUInt8 METH_dMemInit_done();
  tUInt8 METH_RDY_dMemInit_done();
 
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
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkProc &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkProc &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkProc &backing);
  void vcd_submodules(tVCDDumpType dt, unsigned int levels, MOD_mkProc &backing);
};

#endif /* ifndef __mkProc_h__ */
