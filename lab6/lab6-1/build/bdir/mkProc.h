/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Sun May 30 01:34:33 KST 2021
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
#include "mkBypassRFile.h"
#include "module_decode.h"
#include "module_exec.h"


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
  MOD_Reg<tUInt8> INST_dEpoch;
  MOD_mkDMemory INST_dMem;
  MOD_Reg<tUWide> INST_e2m_data_0;
  MOD_Wire<tUInt8> INST_e2m_deqP_ignored_wires_0;
  MOD_Wire<tUInt8> INST_e2m_deqP_ignored_wires_1;
  MOD_Reg<tUInt8> INST_e2m_deqP_virtual_reg_0;
  MOD_Reg<tUInt8> INST_e2m_deqP_virtual_reg_1;
  MOD_Wire<tUInt8> INST_e2m_deqP_wires_0;
  MOD_Wire<tUInt8> INST_e2m_deqP_wires_1;
  MOD_Reg<tUInt8> INST_e2m_empty_ehrReg;
  MOD_Wire<tUInt8> INST_e2m_empty_ignored_wires_0;
  MOD_Wire<tUInt8> INST_e2m_empty_ignored_wires_1;
  MOD_Wire<tUInt8> INST_e2m_empty_ignored_wires_2;
  MOD_Reg<tUInt8> INST_e2m_empty_virtual_reg_0;
  MOD_Reg<tUInt8> INST_e2m_empty_virtual_reg_1;
  MOD_Reg<tUInt8> INST_e2m_empty_virtual_reg_2;
  MOD_Wire<tUInt8> INST_e2m_empty_wires_0;
  MOD_Wire<tUInt8> INST_e2m_empty_wires_1;
  MOD_Wire<tUInt8> INST_e2m_empty_wires_2;
  MOD_Wire<tUInt8> INST_e2m_enqP_ignored_wires_0;
  MOD_Wire<tUInt8> INST_e2m_enqP_ignored_wires_1;
  MOD_Reg<tUInt8> INST_e2m_enqP_virtual_reg_0;
  MOD_Reg<tUInt8> INST_e2m_enqP_virtual_reg_1;
  MOD_Wire<tUInt8> INST_e2m_enqP_wires_0;
  MOD_Wire<tUInt8> INST_e2m_enqP_wires_1;
  MOD_Reg<tUInt8> INST_e2m_full_ehrReg;
  MOD_Wire<tUInt8> INST_e2m_full_ignored_wires_0;
  MOD_Wire<tUInt8> INST_e2m_full_ignored_wires_1;
  MOD_Wire<tUInt8> INST_e2m_full_ignored_wires_2;
  MOD_Reg<tUInt8> INST_e2m_full_virtual_reg_0;
  MOD_Reg<tUInt8> INST_e2m_full_virtual_reg_1;
  MOD_Reg<tUInt8> INST_e2m_full_virtual_reg_2;
  MOD_Wire<tUInt8> INST_e2m_full_wires_0;
  MOD_Wire<tUInt8> INST_e2m_full_wires_1;
  MOD_Wire<tUInt8> INST_e2m_full_wires_2;
  MOD_Reg<tUInt8> INST_eEpoch;
  MOD_Reg<tUInt32> INST_execRedirectToDecode_data_0_ehrReg;
  MOD_Wire<tUInt32> INST_execRedirectToDecode_data_0_ignored_wires_0;
  MOD_Wire<tUInt32> INST_execRedirectToDecode_data_0_ignored_wires_1;
  MOD_Reg<tUInt8> INST_execRedirectToDecode_data_0_virtual_reg_0;
  MOD_Reg<tUInt8> INST_execRedirectToDecode_data_0_virtual_reg_1;
  MOD_Wire<tUInt32> INST_execRedirectToDecode_data_0_wires_0;
  MOD_Wire<tUInt32> INST_execRedirectToDecode_data_0_wires_1;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_deqP_ignored_wires_0;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_deqP_ignored_wires_1;
  MOD_Reg<tUInt8> INST_execRedirectToDecode_deqP_virtual_reg_0;
  MOD_Reg<tUInt8> INST_execRedirectToDecode_deqP_virtual_reg_1;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_deqP_wires_0;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_deqP_wires_1;
  MOD_Reg<tUInt8> INST_execRedirectToDecode_empty_ehrReg;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_empty_ignored_wires_0;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_empty_ignored_wires_1;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_empty_ignored_wires_2;
  MOD_Reg<tUInt8> INST_execRedirectToDecode_empty_virtual_reg_0;
  MOD_Reg<tUInt8> INST_execRedirectToDecode_empty_virtual_reg_1;
  MOD_Reg<tUInt8> INST_execRedirectToDecode_empty_virtual_reg_2;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_empty_wires_0;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_empty_wires_1;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_empty_wires_2;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_enqP_ignored_wires_0;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_enqP_ignored_wires_1;
  MOD_Reg<tUInt8> INST_execRedirectToDecode_enqP_virtual_reg_0;
  MOD_Reg<tUInt8> INST_execRedirectToDecode_enqP_virtual_reg_1;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_enqP_wires_0;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_enqP_wires_1;
  MOD_Reg<tUInt8> INST_execRedirectToDecode_full_ehrReg;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_full_ignored_wires_0;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_full_ignored_wires_1;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_full_ignored_wires_2;
  MOD_Reg<tUInt8> INST_execRedirectToDecode_full_virtual_reg_0;
  MOD_Reg<tUInt8> INST_execRedirectToDecode_full_virtual_reg_1;
  MOD_Reg<tUInt8> INST_execRedirectToDecode_full_virtual_reg_2;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_full_wires_0;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_full_wires_1;
  MOD_Wire<tUInt8> INST_execRedirectToDecode_full_wires_2;
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
  MOD_Reg<tUWide> INST_m2w_data_0;
  MOD_Wire<tUInt8> INST_m2w_deqP_ignored_wires_0;
  MOD_Wire<tUInt8> INST_m2w_deqP_ignored_wires_1;
  MOD_Reg<tUInt8> INST_m2w_deqP_virtual_reg_0;
  MOD_Reg<tUInt8> INST_m2w_deqP_virtual_reg_1;
  MOD_Wire<tUInt8> INST_m2w_deqP_wires_0;
  MOD_Wire<tUInt8> INST_m2w_deqP_wires_1;
  MOD_Reg<tUInt8> INST_m2w_empty_ehrReg;
  MOD_Wire<tUInt8> INST_m2w_empty_ignored_wires_0;
  MOD_Wire<tUInt8> INST_m2w_empty_ignored_wires_1;
  MOD_Wire<tUInt8> INST_m2w_empty_ignored_wires_2;
  MOD_Reg<tUInt8> INST_m2w_empty_virtual_reg_0;
  MOD_Reg<tUInt8> INST_m2w_empty_virtual_reg_1;
  MOD_Reg<tUInt8> INST_m2w_empty_virtual_reg_2;
  MOD_Wire<tUInt8> INST_m2w_empty_wires_0;
  MOD_Wire<tUInt8> INST_m2w_empty_wires_1;
  MOD_Wire<tUInt8> INST_m2w_empty_wires_2;
  MOD_Wire<tUInt8> INST_m2w_enqP_ignored_wires_0;
  MOD_Wire<tUInt8> INST_m2w_enqP_ignored_wires_1;
  MOD_Reg<tUInt8> INST_m2w_enqP_virtual_reg_0;
  MOD_Reg<tUInt8> INST_m2w_enqP_virtual_reg_1;
  MOD_Wire<tUInt8> INST_m2w_enqP_wires_0;
  MOD_Wire<tUInt8> INST_m2w_enqP_wires_1;
  MOD_Reg<tUInt8> INST_m2w_full_ehrReg;
  MOD_Wire<tUInt8> INST_m2w_full_ignored_wires_0;
  MOD_Wire<tUInt8> INST_m2w_full_ignored_wires_1;
  MOD_Wire<tUInt8> INST_m2w_full_ignored_wires_2;
  MOD_Reg<tUInt8> INST_m2w_full_virtual_reg_0;
  MOD_Reg<tUInt8> INST_m2w_full_virtual_reg_1;
  MOD_Reg<tUInt8> INST_m2w_full_virtual_reg_2;
  MOD_Wire<tUInt8> INST_m2w_full_wires_0;
  MOD_Wire<tUInt8> INST_m2w_full_wires_1;
  MOD_Wire<tUInt8> INST_m2w_full_wires_2;
  MOD_Reg<tUInt32> INST_pc;
  MOD_mkBypassRFile INST_rf;
  MOD_Reg<tUInt8> INST_sb_f_data_0;
  MOD_Reg<tUInt8> INST_sb_f_data_1;
  MOD_Reg<tUInt8> INST_sb_f_data_2;
  MOD_Reg<tUInt8> INST_sb_f_data_3;
  MOD_Reg<tUInt8> INST_sb_f_deqP_ehrReg;
  MOD_Wire<tUInt8> INST_sb_f_deqP_ignored_wires_0;
  MOD_Wire<tUInt8> INST_sb_f_deqP_ignored_wires_1;
  MOD_Reg<tUInt8> INST_sb_f_deqP_virtual_reg_0;
  MOD_Reg<tUInt8> INST_sb_f_deqP_virtual_reg_1;
  MOD_Wire<tUInt8> INST_sb_f_deqP_wires_0;
  MOD_Wire<tUInt8> INST_sb_f_deqP_wires_1;
  MOD_Reg<tUInt8> INST_sb_f_empty_ehrReg;
  MOD_Wire<tUInt8> INST_sb_f_empty_ignored_wires_0;
  MOD_Wire<tUInt8> INST_sb_f_empty_ignored_wires_1;
  MOD_Wire<tUInt8> INST_sb_f_empty_ignored_wires_2;
  MOD_Reg<tUInt8> INST_sb_f_empty_virtual_reg_0;
  MOD_Reg<tUInt8> INST_sb_f_empty_virtual_reg_1;
  MOD_Reg<tUInt8> INST_sb_f_empty_virtual_reg_2;
  MOD_Wire<tUInt8> INST_sb_f_empty_wires_0;
  MOD_Wire<tUInt8> INST_sb_f_empty_wires_1;
  MOD_Wire<tUInt8> INST_sb_f_empty_wires_2;
  MOD_Reg<tUInt8> INST_sb_f_enqP_ehrReg;
  MOD_Wire<tUInt8> INST_sb_f_enqP_ignored_wires_0;
  MOD_Wire<tUInt8> INST_sb_f_enqP_ignored_wires_1;
  MOD_Reg<tUInt8> INST_sb_f_enqP_virtual_reg_0;
  MOD_Reg<tUInt8> INST_sb_f_enqP_virtual_reg_1;
  MOD_Wire<tUInt8> INST_sb_f_enqP_wires_0;
  MOD_Wire<tUInt8> INST_sb_f_enqP_wires_1;
  MOD_Reg<tUInt8> INST_sb_f_full_ehrReg;
  MOD_Wire<tUInt8> INST_sb_f_full_ignored_wires_0;
  MOD_Wire<tUInt8> INST_sb_f_full_ignored_wires_1;
  MOD_Wire<tUInt8> INST_sb_f_full_ignored_wires_2;
  MOD_Reg<tUInt8> INST_sb_f_full_virtual_reg_0;
  MOD_Reg<tUInt8> INST_sb_f_full_virtual_reg_1;
  MOD_Reg<tUInt8> INST_sb_f_full_virtual_reg_2;
  MOD_Wire<tUInt8> INST_sb_f_full_wires_0;
  MOD_Wire<tUInt8> INST_sb_f_full_wires_1;
  MOD_Wire<tUInt8> INST_sb_f_full_wires_2;
  MOD_module_decode INST_instance_decode_1;
  MOD_module_exec INST_instance_exec_0;
 
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
  tUWide PORT_dMemInit_request_put;
  tUWide PORT_iMemInit_request_put;
 
 /* Publicly accessible definitions */
 public:
  tUInt8 DEF_f2d_data_0_14_BIT_0_15_EQ_dEpoch_16___d217;
  tUInt8 DEF_x__h38372;
  tUInt8 DEF_NOT_f2d_data_0_14_BIT_0_15_EQ_dEpoch_16_17___d218;
  tUInt8 DEF_execRedirectToDecode_empty_virtual_reg_2_read__ETC___d206;
  tUInt8 DEF_NOT_sb_f_full_virtual_reg_2_read__19_20_AND_NO_ETC___d232;
  tUInt8 DEF_IF_sb_f_deqP_virtual_reg_1_read__29_THEN_0_ELS_ETC___d264;
  tUInt8 DEF_decode_36_BITS_83_TO_79_24_EQ_sb_f_data_0_92_B_ETC___d367;
  tUInt8 DEF_sb_f_data_0_92_BIT_5___d293;
  tUInt8 DEF_decode_36_BIT_84___d323;
  tUInt8 DEF_NOT_sb_f_full_virtual_reg_2_read__19_20_AND_NO_ETC___d251;
  tUInt8 DEF_NOT_sb_f_full_virtual_reg_2_read__19_20_AND_NO_ETC___d268;
  tUInt8 DEF_decode_36_BITS_89_TO_85_38_EQ_sb_f_data_0_92_B_ETC___d313;
  tUInt8 DEF_decode_36_BIT_90___d237;
  tUInt8 DEF_n__read__h33139;
  tUInt8 DEF_IF_sb_f_enqP_virtual_reg_1_read__25_OR_sb_f_en_ETC___d290;
  tUInt8 DEF_sb_f_full_virtual_reg_2_read__19_OR_sb_f_full__ETC___d281;
  tUInt8 DEF_sb_f_full_virtual_reg_2_read____d219;
  tUInt8 DEF_IF_sb_f_full_wires_0_whas__63_THEN_sb_f_full_w_ETC___d166;
  tUInt8 DEF_IF_sb_f_enqP_virtual_reg_1_read__25_OR_sb_f_en_ETC___d231;
  tUInt8 DEF_NOT_IF_sb_f_deqP_virtual_reg_1_read__29_THEN_0_ETC___d287;
  tUInt8 DEF_IF_sb_f_enqP_virtual_reg_1_read__25_OR_sb_f_en_ETC___d250;
  tUInt8 DEF_sb_f_full_virtual_reg_1_read____d221;
  tUInt8 DEF_NOT_IF_sb_f_enqP_virtual_reg_1_read__25_OR_sb__ETC___d267;
  tUInt8 DEF_NOT_sb_f_full_virtual_reg_2_read__19_20_AND_NO_ETC___d224;
  tUInt8 DEF_execRedirectToDecode_empty_virtual_reg_2_read____d200;
  tUInt8 DEF_execRedirectToDecode_empty_virtual_reg_1_read____d201;
  tUInt8 DEF_execRedirect_empty_virtual_reg_2_read__69_OR_e_ETC___d175;
  tUInt8 DEF_execRedirect_empty_virtual_reg_1_read____d170;
  tUInt8 DEF_execRedirect_empty_virtual_reg_2_read____d169;
  tUWide DEF_decode___d236;
  tUInt32 DEF_inst__h32252;
  tUWide DEF_exec___d568;
  tUWide DEF_d2e_data_0_25_BITS_268_TO_258_37_CONCAT_d2e_da_ETC___d562;
  tUInt32 DEF_rVal1__h40314;
  tUInt32 DEF_rVal2__h40315;
  tUInt32 DEF_pc__h40312;
  tUInt32 DEF_ppc__h40313;
  tUInt32 DEF_csrVal__h40311;
  tUWide DEF_d2e_data_0___d525;
  tUWide DEF_f2d_data_0___d214;
  tUWide DEF_e2m_data_0___d619;
  tUInt8 DEF_sb_f_data_3___d233;
  tUInt8 DEF_sb_f_data_2___d252;
  tUInt8 DEF_sb_f_data_1___d269;
  tUInt8 DEF_sb_f_data_0___d292;
  tUInt8 DEF_x__h49397;
  tUInt8 DEF_def__h39293;
  tUInt8 DEF_sb_f_full_wires_0_whas____d163;
  tUInt8 DEF_sb_f_full_wires_0_wget____d164;
  tUInt8 DEF_sb_f_full_ehrReg__h27466;
  tUInt8 DEF_sb_f_empty_ehrReg__h26343;
  tUInt8 DEF_sb_f_deqP_virtual_reg_1_read____d229;
  tUInt8 DEF_sb_f_enqP_virtual_reg_1_read____d225;
  tUInt8 DEF_m2w_full_wires_0_whas____d129;
  tUInt8 DEF_m2w_full_wires_0_wget____d130;
  tUInt8 DEF_m2w_full_ehrReg__h23287;
  tUInt8 DEF_m2w_empty_ehrReg__h22164;
  tUInt8 DEF_e2m_full_wires_0_whas____d109;
  tUInt8 DEF_e2m_full_wires_0_wget____d110;
  tUInt8 DEF_e2m_full_ehrReg__h19723;
  tUInt8 DEF_e2m_empty_ehrReg__h18600;
  tUInt8 DEF_d2e_full_wires_0_whas____d89;
  tUInt8 DEF_d2e_full_wires_0_wget____d90;
  tUInt8 DEF_d2e_full_ehrReg__h16159;
  tUInt8 DEF_d2e_empty_ehrReg__h15036;
  tUInt8 DEF_f2d_full_wires_0_whas____d69;
  tUInt8 DEF_f2d_full_wires_0_wget____d70;
  tUInt8 DEF_f2d_full_ehrReg__h12595;
  tUInt8 DEF_f2d_empty_ehrReg__h11472;
  tUInt8 DEF_execRedirectToDecode_full_ehrReg__h9031;
  tUInt8 DEF_execRedirectToDecode_empty_wires_0_whas____d39;
  tUInt8 DEF_execRedirectToDecode_empty_wires_0_wget____d40;
  tUInt8 DEF_execRedirectToDecode_empty_ehrReg__h7908;
  tUInt8 DEF_execRedirect_full_ehrReg__h4668;
  tUInt8 DEF_execRedirect_empty_wires_0_whas____d12;
  tUInt8 DEF_execRedirect_empty_wires_0_wget____d13;
  tUInt8 DEF_execRedirect_empty_ehrReg__h3545;
  tUInt8 DEF_eEpoch__h40293;
  tUInt8 DEF_dEpoch__h32448;
  tUInt8 DEF_csrf_started____d184;
  tUInt8 DEF_x__h33532;
  tUInt8 DEF_x__h33929;
  tUInt8 DEF_x__h33778;
  tUInt8 DEF_x__h33697;
  tUInt8 DEF_x__h33616;
  tUInt8 DEF_x__h33535;
  tUInt8 DEF_e2m_data_0_19_BITS_88_TO_85___d620;
  tUInt8 DEF_exec_68_BIT_1___d569;
  tUInt8 DEF_sb_f_data_1_69_BIT_5___d270;
  tUInt8 DEF_sb_f_data_2_52_BIT_5___d253;
  tUInt8 DEF_sb_f_data_3_33_BIT_5___d234;
  tUInt8 DEF_e2m_data_0_19_BITS_88_TO_85_20_EQ_2___d621;
  tUInt8 DEF_IF_sb_f_enqP_virtual_reg_1_read__25_OR_sb_f_en_ETC___d243;
  tUInt8 DEF_IF_sb_f_deqP_wires_0_whas__44_THEN_sb_f_deqP_w_ETC___d147;
  tUInt8 DEF_IF_sb_f_enqP_virtual_reg_1_read__25_OR_sb_f_en_ETC___d351;
  tUInt8 DEF_decode_36_BITS_83_TO_79_24_EQ_sb_f_data_1_69_B_ETC___d331;
  tUInt8 DEF_decode_36_BITS_83_TO_79_24_EQ_sb_f_data_3_33_B_ETC___d325;
  tUInt8 DEF_decode_36_BITS_83_TO_79_24_EQ_sb_f_data_2_52_B_ETC___d328;
  tUInt8 DEF_decode_36_BITS_89_TO_85_38_EQ_sb_f_data_1_69_B_ETC___d274;
  tUInt8 DEF_decode_36_BITS_89_TO_85_38_EQ_sb_f_data_3_33_B_ETC___d240;
  tUInt8 DEF_decode_36_BITS_89_TO_85_38_EQ_sb_f_data_2_52_B_ETC___d257;
  tUInt8 DEF_e2m_data_0_19_BITS_88_TO_85_20_EQ_3___d623;
  tUInt8 DEF_IF_sb_f_deqP_virtual_reg_1_read__29_THEN_0_ELS_ETC___d286;
  tUInt8 DEF_IF_sb_f_enqP_virtual_reg_1_read__25_OR_sb_f_en_ETC___d283;
  tUInt8 DEF_d2e_data_0_25_BIT_96_26_EQ_eEpoch_27___d528;
  tUInt8 DEF_sb_f_full_virtual_reg_2_read__19_OR_sb_f_full__ETC___d291;
  tUInt8 DEF_NOT_sb_f_data_0_92_BIT_5_93___d294;
  tUInt8 DEF_NOT_IF_sb_f_enqP_virtual_reg_1_read__25_OR_sb__ETC___d282;
  tUInt8 DEF_NOT_sb_f_data_1_69_BIT_5_70___d271;
  tUInt8 DEF_NOT_IF_sb_f_enqP_virtual_reg_1_read__25_OR_sb__ETC___d261;
  tUInt8 DEF_NOT_sb_f_data_2_52_BIT_5_53___d254;
  tUInt8 DEF_NOT_IF_sb_f_deqP_virtual_reg_1_read__29_THEN_0_ETC___d247;
  tUWide DEF_d2e_data_0_25_BIT_251_42_CONCAT_IF_d2e_data_0__ETC___d561;
  tUWide DEF_d2e_data_0_25_BIT_239_49_CONCAT_IF_d2e_data_0__ETC___d560;
 
 /* Local definitions */
 private:
  tUInt8 DEF_IF_execRedirectToDecode_empty_wires_0_whas__9__ETC___d42;
  tUInt8 DEF_IF_execRedirect_empty_wires_0_whas__2_THEN_exe_ETC___d15;
  tUWide DEF_m2w_data_0___d669;
  tUInt32 DEF_x__h44301;
  tUInt32 DEF_x__h43348;
  tUWide DEF_exec_68_BITS_65_TO_0___d599;
  tUWide DEF_f2d_data_0_14_BITS_64_TO_0___d496;
  tUInt32 DEF_IF_execRedirect_data_0_wires_0_whas_THEN_execR_ETC___d6;
  tUInt8 DEF_IF_sb_f_enqP_wires_0_whas__37_THEN_sb_f_enqP_w_ETC___d140;
  tUInt8 DEF_IF_sb_f_empty_wires_0_whas__53_THEN_sb_f_empty_ETC___d156;
  tUInt8 DEF_IF_m2w_full_wires_0_whas__29_THEN_m2w_full_wir_ETC___d132;
  tUInt8 DEF_IF_m2w_empty_wires_0_whas__19_THEN_m2w_empty_w_ETC___d122;
  tUInt8 DEF_IF_e2m_full_wires_0_whas__09_THEN_e2m_full_wir_ETC___d112;
  tUInt8 DEF_IF_e2m_empty_wires_0_whas__9_THEN_e2m_empty_wi_ETC___d102;
  tUInt8 DEF_IF_d2e_full_wires_0_whas__9_THEN_d2e_full_wire_ETC___d92;
  tUInt8 DEF_IF_d2e_empty_wires_0_whas__9_THEN_d2e_empty_wi_ETC___d82;
  tUInt8 DEF_IF_f2d_full_wires_0_whas__9_THEN_f2d_full_wire_ETC___d72;
  tUInt8 DEF_IF_f2d_empty_wires_0_whas__9_THEN_f2d_empty_wi_ETC___d62;
  tUInt8 DEF_IF_execRedirectToDecode_full_wires_0_whas__9_T_ETC___d52;
  tUInt8 DEF_IF_execRedirect_full_wires_0_whas__2_THEN_exec_ETC___d25;
  tUWide DEF_decode_36_BITS_107_TO_97_75_CONCAT_decode_36_B_ETC___d504;
  tUWide DEF_decode_36_BIT_96_76_CONCAT_IF_decode_36_BIT_96_ETC___d495;
  tUWide DEF_IF_decode_36_BIT_90_37_THEN_rf_rd1_IF_decode_3_ETC___d503;
  tUWide DEF_iMem_req_pc_88_97_CONCAT_pc_88_CONCAT_pc_88_PL_ETC___d199;
  tUWide DEF_decode_36_BIT_84_23_CONCAT_IF_decode_36_BIT_84_ETC___d494;
  tUWide DEF_exec_68_BITS_88_TO_85_91_CONCAT_exec_68_BIT_84_ETC___d601;
  tUWide DEF_exec_68_BIT_78_96_CONCAT_IF_exec_68_BIT_78_96__ETC___d600;
  tUWide DEF_e2m_data_0_19_BITS_88_TO_85_20_CONCAT_e2m_data_ETC___d650;
  tUWide DEF_e2m_data_0_19_BIT_78_44_CONCAT_IF_e2m_data_0_1_ETC___d649;
  tUWide DEF_decode_36_BIT_65_87_CONCAT_IF_decode_36_BIT_65_ETC___d493;
  tUWide DEF_dMemInit_request_put_BIT_64_97_CONCAT_IF_dMemI_ETC___d700;
  tUWide DEF_NOT_e2m_data_0_19_BITS_88_TO_85_20_EQ_2_21_31__ETC___d638;
  tUWide DEF_iMemInit_request_put_BIT_64_93_CONCAT_IF_iMemI_ETC___d696;
 
 /* Rules */
 public:
  void RL_execRedirect_data_0_canonicalize();
  void RL_execRedirect_empty_canonicalize();
  void RL_execRedirect_full_canonicalize();
  void RL_execRedirectToDecode_data_0_canonicalize();
  void RL_execRedirectToDecode_empty_canonicalize();
  void RL_execRedirectToDecode_full_canonicalize();
  void RL_f2d_empty_canonicalize();
  void RL_f2d_full_canonicalize();
  void RL_d2e_empty_canonicalize();
  void RL_d2e_full_canonicalize();
  void RL_e2m_empty_canonicalize();
  void RL_e2m_full_canonicalize();
  void RL_m2w_empty_canonicalize();
  void RL_m2w_full_canonicalize();
  void RL_sb_f_enqP_canonicalize();
  void RL_sb_f_deqP_canonicalize();
  void RL_sb_f_empty_canonicalize();
  void RL_sb_f_full_canonicalize();
  void RL_doFetch();
  void RL_doDecode();
  void RL_doExecute();
  void RL_doMemory();
  void RL_doWriteBack();
 
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
