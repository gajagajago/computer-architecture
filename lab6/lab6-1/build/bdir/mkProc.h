/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Wed Jul 21 23:35:59 KST 2021
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
  tUInt8 DEF_f2d_data_0_22_BIT_0_23_EQ_dEpoch_24___d425;
  tUInt8 DEF_x__h39171;
  tUInt8 DEF_NOT_f2d_data_0_22_BIT_0_23_EQ_dEpoch_24_25___d426;
  tUInt8 DEF_execRedirectToDecode_empty_virtual_reg_2_read__ETC___d414;
  tUInt8 DEF_NOT_sb_f_full_virtual_reg_2_read__27_28_AND_NO_ETC___d440;
  tUInt8 DEF_IF_sb_f_deqP_virtual_reg_1_read__37_THEN_0_ELS_ETC___d472;
  tUInt8 DEF_decode_44_BITS_83_TO_79_32_EQ_sb_f_data_0_00_B_ETC___d575;
  tUInt8 DEF_sb_f_data_0_00_BIT_5___d501;
  tUInt8 DEF_decode_44_BIT_84___d531;
  tUInt8 DEF_NOT_sb_f_full_virtual_reg_2_read__27_28_AND_NO_ETC___d459;
  tUInt8 DEF_NOT_sb_f_full_virtual_reg_2_read__27_28_AND_NO_ETC___d476;
  tUInt8 DEF_decode_44_BITS_89_TO_85_46_EQ_sb_f_data_0_00_B_ETC___d521;
  tUInt8 DEF_decode_44_BIT_90___d445;
  tUInt8 DEF_n__read__h33937;
  tUInt8 DEF_IF_sb_f_enqP_virtual_reg_1_read__33_OR_sb_f_en_ETC___d498;
  tUInt8 DEF_sb_f_full_virtual_reg_2_read__27_OR_sb_f_full__ETC___d489;
  tUInt8 DEF_sb_f_full_virtual_reg_2_read____d427;
  tUInt8 DEF_IF_sb_f_full_wires_0_whas__63_THEN_sb_f_full_w_ETC___d166;
  tUInt8 DEF_IF_sb_f_enqP_virtual_reg_1_read__33_OR_sb_f_en_ETC___d439;
  tUInt8 DEF_NOT_IF_sb_f_deqP_virtual_reg_1_read__37_THEN_0_ETC___d495;
  tUInt8 DEF_IF_sb_f_enqP_virtual_reg_1_read__33_OR_sb_f_en_ETC___d458;
  tUInt8 DEF_sb_f_full_virtual_reg_1_read____d429;
  tUInt8 DEF_NOT_IF_sb_f_enqP_virtual_reg_1_read__33_OR_sb__ETC___d475;
  tUInt8 DEF_NOT_sb_f_full_virtual_reg_2_read__27_28_AND_NO_ETC___d432;
  tUInt8 DEF_execRedirectToDecode_empty_virtual_reg_2_read____d408;
  tUInt8 DEF_execRedirectToDecode_empty_virtual_reg_1_read____d409;
  tUInt8 DEF_execRedirect_empty_virtual_reg_2_read__69_OR_e_ETC___d175;
  tUInt8 DEF_execRedirect_empty_virtual_reg_1_read____d170;
  tUInt8 DEF_execRedirect_empty_virtual_reg_2_read____d169;
  tUWide DEF_decode___d444;
  tUInt32 DEF_inst__h33050;
  tUWide DEF_exec___d776;
  tUWide DEF_d2e_data_0_33_BITS_268_TO_258_45_CONCAT_d2e_da_ETC___d770;
  tUInt32 DEF_rVal1__h41113;
  tUInt32 DEF_rVal2__h41114;
  tUInt32 DEF_pc__h41111;
  tUInt32 DEF_ppc__h41112;
  tUInt32 DEF_csrVal__h41110;
  tUWide DEF_d2e_data_0___d733;
  tUWide DEF_f2d_data_0___d422;
  tUWide DEF_e2m_data_0___d827;
  tUInt8 DEF_sb_f_data_3___d441;
  tUInt8 DEF_sb_f_data_2___d460;
  tUInt8 DEF_sb_f_data_1___d477;
  tUInt8 DEF_sb_f_data_0___d500;
  tUInt8 DEF_x__h50174;
  tUInt8 DEF_def__h40092;
  tUInt8 DEF_sb_f_full_wires_0_whas____d163;
  tUInt8 DEF_sb_f_full_wires_0_wget____d164;
  tUInt8 DEF_sb_f_full_ehrReg__h27466;
  tUInt8 DEF_sb_f_empty_ehrReg__h26343;
  tUInt8 DEF_sb_f_deqP_virtual_reg_1_read____d437;
  tUInt8 DEF_sb_f_enqP_virtual_reg_1_read____d433;
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
  tUInt8 DEF_eEpoch__h41092;
  tUInt8 DEF_dEpoch__h33246;
  tUInt8 DEF_csrf_started____d184;
  tUInt8 DEF_x__h34330;
  tUInt8 DEF_x__h34727;
  tUInt8 DEF_x__h34576;
  tUInt8 DEF_x__h34495;
  tUInt8 DEF_x__h34414;
  tUInt8 DEF_x__h34333;
  tUInt8 DEF_e2m_data_0_27_BITS_88_TO_85___d828;
  tUInt8 DEF_exec_76_BIT_1___d777;
  tUInt8 DEF_sb_f_data_1_77_BIT_5___d478;
  tUInt8 DEF_sb_f_data_2_60_BIT_5___d461;
  tUInt8 DEF_sb_f_data_3_41_BIT_5___d442;
  tUInt8 DEF_e2m_data_0_27_BITS_88_TO_85_28_EQ_2___d829;
  tUInt8 DEF_IF_sb_f_enqP_virtual_reg_1_read__33_OR_sb_f_en_ETC___d451;
  tUInt8 DEF_IF_sb_f_deqP_wires_0_whas__44_THEN_sb_f_deqP_w_ETC___d147;
  tUInt8 DEF_IF_sb_f_enqP_virtual_reg_1_read__33_OR_sb_f_en_ETC___d559;
  tUInt8 DEF_decode_44_BITS_83_TO_79_32_EQ_sb_f_data_1_77_B_ETC___d539;
  tUInt8 DEF_decode_44_BITS_83_TO_79_32_EQ_sb_f_data_3_41_B_ETC___d533;
  tUInt8 DEF_decode_44_BITS_83_TO_79_32_EQ_sb_f_data_2_60_B_ETC___d536;
  tUInt8 DEF_decode_44_BITS_89_TO_85_46_EQ_sb_f_data_1_77_B_ETC___d482;
  tUInt8 DEF_decode_44_BITS_89_TO_85_46_EQ_sb_f_data_3_41_B_ETC___d448;
  tUInt8 DEF_decode_44_BITS_89_TO_85_46_EQ_sb_f_data_2_60_B_ETC___d465;
  tUInt8 DEF_e2m_data_0_27_BITS_88_TO_85_28_EQ_3___d831;
  tUInt8 DEF_IF_sb_f_deqP_virtual_reg_1_read__37_THEN_0_ELS_ETC___d494;
  tUInt8 DEF_IF_sb_f_enqP_virtual_reg_1_read__33_OR_sb_f_en_ETC___d491;
  tUInt8 DEF_d2e_data_0_33_BIT_96_34_EQ_eEpoch_35___d736;
  tUInt8 DEF_sb_f_full_virtual_reg_2_read__27_OR_sb_f_full__ETC___d499;
  tUInt8 DEF_NOT_sb_f_data_0_00_BIT_5_01___d502;
  tUInt8 DEF_NOT_IF_sb_f_enqP_virtual_reg_1_read__33_OR_sb__ETC___d490;
  tUInt8 DEF_NOT_sb_f_data_1_77_BIT_5_78___d479;
  tUInt8 DEF_NOT_IF_sb_f_enqP_virtual_reg_1_read__33_OR_sb__ETC___d469;
  tUInt8 DEF_NOT_sb_f_data_2_60_BIT_5_61___d462;
  tUInt8 DEF_NOT_IF_sb_f_deqP_virtual_reg_1_read__37_THEN_0_ETC___d455;
  tUWide DEF_d2e_data_0_33_BIT_251_50_CONCAT_IF_d2e_data_0__ETC___d769;
  tUWide DEF_d2e_data_0_33_BIT_239_57_CONCAT_IF_d2e_data_0__ETC___d768;
 
 /* Local definitions */
 private:
  tUInt8 DEF_IF_execRedirectToDecode_empty_wires_0_whas__9__ETC___d42;
  tUInt8 DEF_IF_execRedirect_empty_wires_0_whas__2_THEN_exe_ETC___d15;
  tUWide DEF_m2w_data_0___d877;
  tUInt32 DEF_x__h45100;
  tUInt32 DEF_x__h44147;
  tUWide DEF_exec_76_BITS_65_TO_0___d807;
  tUWide DEF_f2d_data_0_22_BITS_64_TO_0___d704;
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
  tUWide DEF_decode_44_BITS_107_TO_97_83_CONCAT_decode_44_B_ETC___d712;
  tUWide DEF_decode_44_BIT_96_84_CONCAT_IF_decode_44_BIT_96_ETC___d703;
  tUWide DEF_IF_decode_44_BIT_90_45_THEN_rf_rd1_IF_decode_4_ETC___d711;
  tUWide DEF_iMem_req_pc_88_97_CONCAT_pc_88_CONCAT_pc_88_PL_ETC___d407;
  tUWide DEF_decode_44_BIT_84_31_CONCAT_IF_decode_44_BIT_84_ETC___d702;
  tUWide DEF_exec_76_BITS_88_TO_85_99_CONCAT_exec_76_BIT_84_ETC___d809;
  tUWide DEF_exec_76_BIT_78_04_CONCAT_IF_exec_76_BIT_78_04__ETC___d808;
  tUWide DEF_e2m_data_0_27_BITS_88_TO_85_28_CONCAT_e2m_data_ETC___d858;
  tUWide DEF_e2m_data_0_27_BIT_78_52_CONCAT_IF_e2m_data_0_2_ETC___d857;
  tUWide DEF_decode_44_BIT_65_95_CONCAT_IF_decode_44_BIT_65_ETC___d701;
  tUWide DEF_dMemInit_request_put_BIT_64_05_CONCAT_IF_dMemI_ETC___d908;
  tUWide DEF_NOT_e2m_data_0_27_BITS_88_TO_85_28_EQ_2_29_39__ETC___d846;
  tUWide DEF_iMemInit_request_put_BIT_64_01_CONCAT_IF_iMemI_ETC___d904;
 
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
