/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Tue Jun  1 22:49:00 KST 2021
 * 
 */

/* Generation options: */
#ifndef __mkBypassRFile_h__
#define __mkBypassRFile_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"


/* Class declaration for the mkBypassRFile module */
class MOD_mkBypassRFile : public Module {
 
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
  MOD_Reg<tUInt32> INST_rfile_0_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_0_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_0_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_0_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_0_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_0_wires_0;
  MOD_Wire<tUInt32> INST_rfile_0_wires_1;
  MOD_Reg<tUInt32> INST_rfile_10_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_10_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_10_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_10_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_10_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_10_wires_0;
  MOD_Wire<tUInt32> INST_rfile_10_wires_1;
  MOD_Reg<tUInt32> INST_rfile_11_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_11_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_11_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_11_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_11_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_11_wires_0;
  MOD_Wire<tUInt32> INST_rfile_11_wires_1;
  MOD_Reg<tUInt32> INST_rfile_12_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_12_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_12_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_12_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_12_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_12_wires_0;
  MOD_Wire<tUInt32> INST_rfile_12_wires_1;
  MOD_Reg<tUInt32> INST_rfile_13_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_13_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_13_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_13_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_13_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_13_wires_0;
  MOD_Wire<tUInt32> INST_rfile_13_wires_1;
  MOD_Reg<tUInt32> INST_rfile_14_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_14_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_14_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_14_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_14_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_14_wires_0;
  MOD_Wire<tUInt32> INST_rfile_14_wires_1;
  MOD_Reg<tUInt32> INST_rfile_15_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_15_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_15_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_15_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_15_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_15_wires_0;
  MOD_Wire<tUInt32> INST_rfile_15_wires_1;
  MOD_Reg<tUInt32> INST_rfile_16_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_16_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_16_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_16_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_16_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_16_wires_0;
  MOD_Wire<tUInt32> INST_rfile_16_wires_1;
  MOD_Reg<tUInt32> INST_rfile_17_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_17_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_17_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_17_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_17_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_17_wires_0;
  MOD_Wire<tUInt32> INST_rfile_17_wires_1;
  MOD_Reg<tUInt32> INST_rfile_18_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_18_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_18_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_18_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_18_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_18_wires_0;
  MOD_Wire<tUInt32> INST_rfile_18_wires_1;
  MOD_Reg<tUInt32> INST_rfile_19_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_19_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_19_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_19_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_19_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_19_wires_0;
  MOD_Wire<tUInt32> INST_rfile_19_wires_1;
  MOD_Reg<tUInt32> INST_rfile_1_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_1_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_1_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_1_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_1_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_1_wires_0;
  MOD_Wire<tUInt32> INST_rfile_1_wires_1;
  MOD_Reg<tUInt32> INST_rfile_20_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_20_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_20_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_20_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_20_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_20_wires_0;
  MOD_Wire<tUInt32> INST_rfile_20_wires_1;
  MOD_Reg<tUInt32> INST_rfile_21_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_21_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_21_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_21_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_21_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_21_wires_0;
  MOD_Wire<tUInt32> INST_rfile_21_wires_1;
  MOD_Reg<tUInt32> INST_rfile_22_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_22_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_22_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_22_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_22_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_22_wires_0;
  MOD_Wire<tUInt32> INST_rfile_22_wires_1;
  MOD_Reg<tUInt32> INST_rfile_23_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_23_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_23_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_23_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_23_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_23_wires_0;
  MOD_Wire<tUInt32> INST_rfile_23_wires_1;
  MOD_Reg<tUInt32> INST_rfile_24_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_24_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_24_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_24_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_24_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_24_wires_0;
  MOD_Wire<tUInt32> INST_rfile_24_wires_1;
  MOD_Reg<tUInt32> INST_rfile_25_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_25_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_25_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_25_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_25_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_25_wires_0;
  MOD_Wire<tUInt32> INST_rfile_25_wires_1;
  MOD_Reg<tUInt32> INST_rfile_26_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_26_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_26_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_26_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_26_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_26_wires_0;
  MOD_Wire<tUInt32> INST_rfile_26_wires_1;
  MOD_Reg<tUInt32> INST_rfile_27_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_27_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_27_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_27_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_27_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_27_wires_0;
  MOD_Wire<tUInt32> INST_rfile_27_wires_1;
  MOD_Reg<tUInt32> INST_rfile_28_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_28_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_28_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_28_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_28_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_28_wires_0;
  MOD_Wire<tUInt32> INST_rfile_28_wires_1;
  MOD_Reg<tUInt32> INST_rfile_29_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_29_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_29_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_29_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_29_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_29_wires_0;
  MOD_Wire<tUInt32> INST_rfile_29_wires_1;
  MOD_Reg<tUInt32> INST_rfile_2_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_2_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_2_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_2_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_2_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_2_wires_0;
  MOD_Wire<tUInt32> INST_rfile_2_wires_1;
  MOD_Reg<tUInt32> INST_rfile_30_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_30_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_30_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_30_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_30_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_30_wires_0;
  MOD_Wire<tUInt32> INST_rfile_30_wires_1;
  MOD_Reg<tUInt32> INST_rfile_31_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_31_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_31_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_31_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_31_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_31_wires_0;
  MOD_Wire<tUInt32> INST_rfile_31_wires_1;
  MOD_Reg<tUInt32> INST_rfile_3_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_3_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_3_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_3_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_3_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_3_wires_0;
  MOD_Wire<tUInt32> INST_rfile_3_wires_1;
  MOD_Reg<tUInt32> INST_rfile_4_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_4_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_4_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_4_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_4_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_4_wires_0;
  MOD_Wire<tUInt32> INST_rfile_4_wires_1;
  MOD_Reg<tUInt32> INST_rfile_5_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_5_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_5_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_5_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_5_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_5_wires_0;
  MOD_Wire<tUInt32> INST_rfile_5_wires_1;
  MOD_Reg<tUInt32> INST_rfile_6_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_6_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_6_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_6_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_6_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_6_wires_0;
  MOD_Wire<tUInt32> INST_rfile_6_wires_1;
  MOD_Reg<tUInt32> INST_rfile_7_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_7_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_7_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_7_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_7_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_7_wires_0;
  MOD_Wire<tUInt32> INST_rfile_7_wires_1;
  MOD_Reg<tUInt32> INST_rfile_8_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_8_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_8_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_8_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_8_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_8_wires_0;
  MOD_Wire<tUInt32> INST_rfile_8_wires_1;
  MOD_Reg<tUInt32> INST_rfile_9_ehrReg;
  MOD_Wire<tUInt32> INST_rfile_9_ignored_wires_0;
  MOD_Wire<tUInt32> INST_rfile_9_ignored_wires_1;
  MOD_Reg<tUInt8> INST_rfile_9_virtual_reg_0;
  MOD_Reg<tUInt8> INST_rfile_9_virtual_reg_1;
  MOD_Wire<tUInt32> INST_rfile_9_wires_0;
  MOD_Wire<tUInt32> INST_rfile_9_wires_1;
 
 /* Constructor */
 public:
  MOD_mkBypassRFile(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
  tUInt8 PORT_RST_N;
 
 /* Port definitions */
 public:
 
 /* Publicly accessible definitions */
 public:
 
 /* Local definitions */
 private:
  tUInt32 DEF_def__h40671;
  tUInt32 DEF_def__h40606;
  tUInt32 DEF_def__h40541;
  tUInt32 DEF_def__h40476;
  tUInt32 DEF_def__h40411;
  tUInt32 DEF_def__h40346;
  tUInt32 DEF_def__h40281;
  tUInt32 DEF_def__h40216;
  tUInt32 DEF_def__h40151;
  tUInt32 DEF_def__h40086;
  tUInt32 DEF_def__h40021;
  tUInt32 DEF_def__h39956;
  tUInt32 DEF_def__h39891;
  tUInt32 DEF_def__h39826;
  tUInt32 DEF_def__h39761;
  tUInt32 DEF_def__h39696;
  tUInt32 DEF_def__h39631;
  tUInt32 DEF_def__h39566;
  tUInt32 DEF_def__h39501;
  tUInt32 DEF_def__h39436;
  tUInt32 DEF_def__h39371;
  tUInt32 DEF_def__h39306;
  tUInt32 DEF_def__h39241;
  tUInt32 DEF_def__h39176;
  tUInt32 DEF_def__h39111;
  tUInt32 DEF_def__h39046;
  tUInt32 DEF_def__h38981;
  tUInt32 DEF_def__h38916;
  tUInt32 DEF_def__h38851;
  tUInt32 DEF_def__h38786;
  tUInt32 DEF_def__h38721;
  tUInt32 DEF_def__h38656;
  tUInt32 DEF_n__read__h38545;
  tUInt32 DEF_n__read__h38547;
  tUInt32 DEF_n__read__h38549;
  tUInt32 DEF_n__read__h38551;
  tUInt32 DEF_n__read__h38553;
  tUInt32 DEF_n__read__h38555;
  tUInt32 DEF_n__read__h38557;
  tUInt32 DEF_n__read__h38559;
  tUInt32 DEF_n__read__h38561;
  tUInt32 DEF_n__read__h38563;
  tUInt32 DEF_n__read__h38565;
  tUInt32 DEF_n__read__h38567;
  tUInt32 DEF_n__read__h38569;
  tUInt32 DEF_n__read__h38571;
  tUInt32 DEF_n__read__h38573;
  tUInt32 DEF_n__read__h38575;
  tUInt32 DEF_n__read__h38577;
  tUInt32 DEF_n__read__h38579;
  tUInt32 DEF_n__read__h38581;
  tUInt32 DEF_n__read__h38583;
  tUInt32 DEF_n__read__h38585;
  tUInt32 DEF_n__read__h38587;
  tUInt32 DEF_n__read__h38589;
  tUInt32 DEF_n__read__h38591;
  tUInt32 DEF_n__read__h38593;
  tUInt32 DEF_n__read__h38595;
  tUInt32 DEF_n__read__h38597;
  tUInt32 DEF_n__read__h38599;
  tUInt32 DEF_n__read__h38601;
  tUInt32 DEF_n__read__h38603;
  tUInt32 DEF_n__read__h38605;
  tUInt32 DEF_n__read__h38607;
  tUInt32 DEF_IF_rfile_31_wires_0_whas__20_THEN_rfile_31_wir_ETC___d223;
  tUInt32 DEF_IF_rfile_30_wires_0_whas__13_THEN_rfile_30_wir_ETC___d216;
  tUInt32 DEF_IF_rfile_29_wires_0_whas__06_THEN_rfile_29_wir_ETC___d209;
  tUInt32 DEF_IF_rfile_28_wires_0_whas__99_THEN_rfile_28_wir_ETC___d202;
  tUInt32 DEF_IF_rfile_27_wires_0_whas__92_THEN_rfile_27_wir_ETC___d195;
  tUInt32 DEF_IF_rfile_26_wires_0_whas__85_THEN_rfile_26_wir_ETC___d188;
  tUInt32 DEF_IF_rfile_25_wires_0_whas__78_THEN_rfile_25_wir_ETC___d181;
  tUInt32 DEF_IF_rfile_24_wires_0_whas__71_THEN_rfile_24_wir_ETC___d174;
  tUInt32 DEF_IF_rfile_23_wires_0_whas__64_THEN_rfile_23_wir_ETC___d167;
  tUInt32 DEF_IF_rfile_22_wires_0_whas__57_THEN_rfile_22_wir_ETC___d160;
  tUInt32 DEF_IF_rfile_21_wires_0_whas__50_THEN_rfile_21_wir_ETC___d153;
  tUInt32 DEF_IF_rfile_20_wires_0_whas__43_THEN_rfile_20_wir_ETC___d146;
  tUInt32 DEF_IF_rfile_19_wires_0_whas__36_THEN_rfile_19_wir_ETC___d139;
  tUInt32 DEF_IF_rfile_18_wires_0_whas__29_THEN_rfile_18_wir_ETC___d132;
  tUInt32 DEF_IF_rfile_17_wires_0_whas__22_THEN_rfile_17_wir_ETC___d125;
  tUInt32 DEF_IF_rfile_16_wires_0_whas__15_THEN_rfile_16_wir_ETC___d118;
  tUInt32 DEF_IF_rfile_15_wires_0_whas__08_THEN_rfile_15_wir_ETC___d111;
  tUInt32 DEF_IF_rfile_14_wires_0_whas__01_THEN_rfile_14_wir_ETC___d104;
  tUInt32 DEF_IF_rfile_13_wires_0_whas__4_THEN_rfile_13_wire_ETC___d97;
  tUInt32 DEF_IF_rfile_12_wires_0_whas__7_THEN_rfile_12_wire_ETC___d90;
  tUInt32 DEF_IF_rfile_11_wires_0_whas__0_THEN_rfile_11_wire_ETC___d83;
  tUInt32 DEF_IF_rfile_10_wires_0_whas__3_THEN_rfile_10_wire_ETC___d76;
  tUInt32 DEF_IF_rfile_9_wires_0_whas__6_THEN_rfile_9_wires__ETC___d69;
  tUInt32 DEF_IF_rfile_8_wires_0_whas__9_THEN_rfile_8_wires__ETC___d62;
  tUInt32 DEF_IF_rfile_7_wires_0_whas__2_THEN_rfile_7_wires__ETC___d55;
  tUInt32 DEF_IF_rfile_6_wires_0_whas__5_THEN_rfile_6_wires__ETC___d48;
  tUInt32 DEF_IF_rfile_5_wires_0_whas__8_THEN_rfile_5_wires__ETC___d41;
  tUInt32 DEF_IF_rfile_4_wires_0_whas__1_THEN_rfile_4_wires__ETC___d34;
  tUInt32 DEF_IF_rfile_3_wires_0_whas__4_THEN_rfile_3_wires__ETC___d27;
  tUInt32 DEF_IF_rfile_2_wires_0_whas__7_THEN_rfile_2_wires__ETC___d20;
  tUInt32 DEF_IF_rfile_1_wires_0_whas__0_THEN_rfile_1_wires__ETC___d13;
  tUInt32 DEF_IF_rfile_0_wires_0_whas_THEN_rfile_0_wires_0_w_ETC___d6;
 
 /* Rules */
 public:
  void RL_rfile_0_canonicalize();
  void RL_rfile_1_canonicalize();
  void RL_rfile_2_canonicalize();
  void RL_rfile_3_canonicalize();
  void RL_rfile_4_canonicalize();
  void RL_rfile_5_canonicalize();
  void RL_rfile_6_canonicalize();
  void RL_rfile_7_canonicalize();
  void RL_rfile_8_canonicalize();
  void RL_rfile_9_canonicalize();
  void RL_rfile_10_canonicalize();
  void RL_rfile_11_canonicalize();
  void RL_rfile_12_canonicalize();
  void RL_rfile_13_canonicalize();
  void RL_rfile_14_canonicalize();
  void RL_rfile_15_canonicalize();
  void RL_rfile_16_canonicalize();
  void RL_rfile_17_canonicalize();
  void RL_rfile_18_canonicalize();
  void RL_rfile_19_canonicalize();
  void RL_rfile_20_canonicalize();
  void RL_rfile_21_canonicalize();
  void RL_rfile_22_canonicalize();
  void RL_rfile_23_canonicalize();
  void RL_rfile_24_canonicalize();
  void RL_rfile_25_canonicalize();
  void RL_rfile_26_canonicalize();
  void RL_rfile_27_canonicalize();
  void RL_rfile_28_canonicalize();
  void RL_rfile_29_canonicalize();
  void RL_rfile_30_canonicalize();
  void RL_rfile_31_canonicalize();
 
 /* Methods */
 public:
  void METH_wr(tUInt8 ARG_wr_rindx, tUInt32 ARG_wr_data);
  tUInt8 METH_RDY_wr();
  tUInt32 METH_rd1(tUInt8 ARG_rd1_rindx);
  tUInt8 METH_RDY_rd1();
  tUInt32 METH_rd2(tUInt8 ARG_rd2_rindx);
  tUInt8 METH_RDY_rd2();
  tUInt32 METH_rd3(tUInt8 ARG_rd3_rindx);
  tUInt8 METH_RDY_rd3();
 
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
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkBypassRFile &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkBypassRFile &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkBypassRFile &backing);
};

#endif /* ifndef __mkBypassRFile_h__ */
