/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
<<<<<<< HEAD
 * On Tue Jun  1 13:00:47 KST 2021
=======
 * On Mon May 31 15:52:11 KST 2021
>>>>>>> 2c19ac96a682605b36ff1c51da4fcd661e83b5e6
 * 
 */
#include "bluesim_primitives.h"
#include "module_decode.h"


/* Constructor */
MOD_module_decode::MOD_module_decode(tSimStateHdl simHdl, char const *name, Module *parent)
  : Module(simHdl, name, parent),
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d186(100u),
    DEF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decode_i_ETC___d185(91u),
    DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011_6_CONCAT__ETC___d184(79u)
{
  PORT_decode.setSize(108u);
  PORT_decode.clear();
  symbol_count = 1u;
  symbols = new tSym[symbol_count];
  init_symbols_0();
}


/* Symbol init fns */

void MOD_module_decode::init_symbols_0()
{
  init_symbol(&symbols[0u], "decode", SYM_PORT, &PORT_decode, 108u);
}


/* Rule actions */


/* Methods */

tUWide MOD_module_decode::METH_decode(tUInt32 ARG_decode_inst)
{
  tUInt32 DEF_x__h1725;
  tUInt32 DEF_x__h1638;
  tUInt32 DEF_x__h1550;
  tUInt8 DEF_NOT_decode_inst_BITS_6_TO_0_EQ_0b1101111___d83;
  tUInt8 DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d67;
  tUInt8 DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_OR_decod_ETC___d64;
  tUInt8 DEF_decode_inst_BITS_14_TO_12_EQ_0b10___d10;
  tUInt8 DEF_decode_inst_BITS_11_TO_7_8_EQ_0___d19;
  tUInt8 DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b1_7_THEN_IF__ETC___d25;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011___d16;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b10111___d15;
  tUInt8 DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_IF__ETC___d58;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b110011___d3;
  tUInt8 DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d34;
  tUInt8 DEF_IF_decode_inst_BIT_30_9_THEN_8_ELSE_9___d40;
  tUInt8 DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_IF__ETC___d50;
  tUInt8 DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_0_E_ETC___d48;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b110111___d14;
  tUInt8 DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d123;
  tUInt32 DEF_immU__h28;
  tUInt32 DEF_immS__h26;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b100011___d12;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b11___d8;
  tUInt32 DEF_immB__h27;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b1100011___d7;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b1100111___d6;
  tUInt32 DEF_immJ__h29;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b1101111___d5;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b10011___d2;
  tUInt32 DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_SEX_ETC___d181;
  tUInt8 DEF_decode_inst_BIT_30___d39;
  tUInt8 DEF_decode_inst_BIT_31___d158;
  tUInt8 DEF_funct3__h20;
  tUInt8 DEF_decode_inst_BITS_11_TO_7___d18;
  tUInt8 DEF_rs1__h21;
  tUInt8 DEF_opcode__h18;
  tUInt32 DEF_x__h1475;
  tUInt32 DEF_immI__h25;
  tUInt32 DEF_immI_BITS_11_TO_0___h1495;
  DEF_x__h1475 = (tUInt32)(ARG_decode_inst >> 20u);
  DEF_immI__h25 = primSignExt32(32u, 12u, (tUInt32)(DEF_x__h1475));
  DEF_immI_BITS_11_TO_0___h1495 = (tUInt32)(4095u & DEF_immI__h25);
  DEF_opcode__h18 = (tUInt8)((tUInt8)127u & ARG_decode_inst);
  DEF_rs1__h21 = (tUInt8)((tUInt8)31u & (ARG_decode_inst >> 15u));
  DEF_decode_inst_BITS_11_TO_7___d18 = (tUInt8)((tUInt8)31u & (ARG_decode_inst >> 7u));
  DEF_funct3__h20 = (tUInt8)((tUInt8)7u & (ARG_decode_inst >> 12u));
  DEF_decode_inst_BIT_31___d158 = (tUInt8)(ARG_decode_inst >> 31u);
  DEF_decode_inst_BIT_30___d39 = (tUInt8)((tUInt8)1u & (ARG_decode_inst >> 30u));
  DEF_decode_inst_BITS_6_TO_0_EQ_0b10011___d2 = DEF_opcode__h18 == (tUInt8)19u;
  DEF_decode_inst_BITS_6_TO_0_EQ_0b1101111___d5 = DEF_opcode__h18 == (tUInt8)111u;
  DEF_decode_inst_BITS_6_TO_0_EQ_0b1100111___d6 = DEF_opcode__h18 == (tUInt8)103u;
  DEF_decode_inst_BITS_6_TO_0_EQ_0b1100011___d7 = DEF_opcode__h18 == (tUInt8)99u;
  DEF_decode_inst_BITS_6_TO_0_EQ_0b11___d8 = DEF_opcode__h18 == (tUInt8)3u;
  DEF_decode_inst_BITS_6_TO_0_EQ_0b100011___d12 = DEF_opcode__h18 == (tUInt8)35u;
  DEF_immU__h28 = ((tUInt32)(ARG_decode_inst >> 12u)) << 12u;
  switch (DEF_opcode__h18) {
  case (tUInt8)55u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d123 = (tUInt8)0u;
    break;
  default:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d123 = DEF_rs1__h21;
  }
  DEF_decode_inst_BITS_6_TO_0_EQ_0b110111___d14 = DEF_opcode__h18 == (tUInt8)55u;
  DEF_IF_decode_inst_BIT_30_9_THEN_8_ELSE_9___d40 = DEF_decode_inst_BIT_30___d39 ? (tUInt8)8u : (tUInt8)9u;
  switch (DEF_funct3__h20) {
  case (tUInt8)0u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_0_E_ETC___d48 = (tUInt8)0u;
    break;
  case (tUInt8)1u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_0_E_ETC___d48 = (tUInt8)7u;
    break;
  case (tUInt8)2u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_0_E_ETC___d48 = (tUInt8)5u;
    break;
  case (tUInt8)3u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_0_E_ETC___d48 = (tUInt8)6u;
    break;
  case (tUInt8)4u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_0_E_ETC___d48 = (tUInt8)4u;
    break;
  case (tUInt8)5u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_0_E_ETC___d48 = DEF_IF_decode_inst_BIT_30_9_THEN_8_ELSE_9___d40;
    break;
  case (tUInt8)6u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_0_E_ETC___d48 = (tUInt8)3u;
    break;
  default:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_0_E_ETC___d48 = (tUInt8)2u;
  }
  switch (DEF_funct3__h20) {
  case (tUInt8)0u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_IF__ETC___d50 = DEF_decode_inst_BIT_30___d39 ? (tUInt8)1u : (tUInt8)0u;
    break;
  case (tUInt8)1u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_IF__ETC___d50 = (tUInt8)7u;
    break;
  case (tUInt8)2u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_IF__ETC___d50 = (tUInt8)5u;
    break;
  case (tUInt8)3u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_IF__ETC___d50 = (tUInt8)6u;
    break;
  case (tUInt8)4u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_IF__ETC___d50 = (tUInt8)4u;
    break;
  case (tUInt8)5u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_IF__ETC___d50 = DEF_IF_decode_inst_BIT_30_9_THEN_8_ELSE_9___d40;
    break;
  case (tUInt8)6u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_IF__ETC___d50 = (tUInt8)3u;
    break;
  default:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_IF__ETC___d50 = (tUInt8)2u;
  }
  DEF_decode_inst_BITS_6_TO_0_EQ_0b110011___d3 = DEF_opcode__h18 == (tUInt8)51u;
  switch (DEF_opcode__h18) {
  case (tUInt8)19u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_IF__ETC___d58 = DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_0_E_ETC___d48;
    break;
  case (tUInt8)51u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_IF__ETC___d58 = DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_THEN_IF__ETC___d50;
    break;
  default:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_IF__ETC___d58 = (tUInt8)0u;
  }
  DEF_decode_inst_BITS_6_TO_0_EQ_0b10111___d15 = DEF_opcode__h18 == (tUInt8)23u;
  DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011___d16 = DEF_opcode__h18 == (tUInt8)115u;
  DEF_decode_inst_BITS_11_TO_7_8_EQ_0___d19 = DEF_decode_inst_BITS_11_TO_7___d18 == (tUInt8)0u;
  switch (DEF_funct3__h20) {
  case (tUInt8)1u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b1_7_THEN_IF__ETC___d25 = DEF_decode_inst_BITS_11_TO_7_8_EQ_0___d19 ? (tUInt8)8u : (tUInt8)0u;
    break;
  case (tUInt8)2u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b1_7_THEN_IF__ETC___d25 = DEF_rs1__h21 == (tUInt8)0u ? (tUInt8)7u : (tUInt8)0u;
    break;
  default:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b1_7_THEN_IF__ETC___d25 = (tUInt8)0u;
  }
  DEF_decode_inst_BITS_14_TO_12_EQ_0b10___d10 = DEF_funct3__h20 == (tUInt8)2u;
  switch (DEF_opcode__h18) {
  case (tUInt8)19u:
  case (tUInt8)51u:
  case (tUInt8)55u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d34 = (tUInt8)1u;
    break;
  case (tUInt8)111u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d34 = (tUInt8)4u;
    break;
  case (tUInt8)103u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d34 = (tUInt8)5u;
    break;
  case (tUInt8)99u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d34 = (tUInt8)6u;
    break;
  case (tUInt8)3u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d34 = DEF_decode_inst_BITS_14_TO_12_EQ_0b10___d10 ? (tUInt8)2u : (tUInt8)0u;
    break;
  case (tUInt8)35u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d34 = DEF_decode_inst_BITS_14_TO_12_EQ_0b10___d10 ? (tUInt8)3u : (tUInt8)0u;
    break;
  case (tUInt8)23u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d34 = (tUInt8)9u;
    break;
  case (tUInt8)115u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d34 = DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b1_7_THEN_IF__ETC___d25;
    break;
  default:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d34 = (tUInt8)0u;
  }
  switch (DEF_funct3__h20) {
  case (tUInt8)0u:
  case (tUInt8)1u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_OR_decod_ETC___d64 = DEF_funct3__h20;
    break;
  case (tUInt8)4u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_OR_decod_ETC___d64 = (tUInt8)2u;
    break;
  case (tUInt8)5u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_OR_decod_ETC___d64 = (tUInt8)4u;
    break;
  case (tUInt8)6u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_OR_decod_ETC___d64 = (tUInt8)3u;
    break;
  default:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_OR_decod_ETC___d64 = (tUInt8)5u;
  }
  switch (DEF_opcode__h18) {
  case (tUInt8)103u:
  case (tUInt8)111u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d67 = (tUInt8)6u;
    break;
  case (tUInt8)99u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d67 = DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_5_OR_decod_ETC___d64;
    break;
  default:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d67 = (tUInt8)7u;
  }
  DEF_NOT_decode_inst_BITS_6_TO_0_EQ_0b1101111___d83 = !DEF_decode_inst_BITS_6_TO_0_EQ_0b1101111___d5;
  DEF_x__h1550 = 2097151u & (((((((tUInt32)(DEF_decode_inst_BIT_31___d158)) << 20u) | (((tUInt32)((tUInt8)((tUInt8)255u & (ARG_decode_inst >> 12u)))) << 12u)) | (((tUInt32)((tUInt8)((tUInt8)1u & (ARG_decode_inst >> 20u)))) << 11u)) | (((tUInt32)(1023u & (ARG_decode_inst >> 21u))) << 1u)) | (tUInt32)((tUInt8)0u));
  DEF_immJ__h29 = primSignExt32(32u, 21u, (tUInt32)(DEF_x__h1550));
  DEF_x__h1638 = 8191u & (((((((tUInt32)(DEF_decode_inst_BIT_31___d158)) << 12u) | (((tUInt32)((tUInt8)((tUInt8)1u & (ARG_decode_inst >> 7u)))) << 11u)) | (((tUInt32)((tUInt8)((tUInt8)63u & (ARG_decode_inst >> 25u)))) << 5u)) | (((tUInt32)((tUInt8)((tUInt8)15u & (ARG_decode_inst >> 8u)))) << 1u)) | (tUInt32)((tUInt8)0u));
  DEF_immB__h27 = primSignExt32(32u, 13u, (tUInt32)(DEF_x__h1638));
  DEF_x__h1725 = 4095u & ((((tUInt32)((tUInt8)(ARG_decode_inst >> 25u))) << 5u) | (tUInt32)(DEF_decode_inst_BITS_11_TO_7___d18));
  DEF_immS__h26 = primSignExt32(32u, 12u, (tUInt32)(DEF_x__h1725));
  switch (DEF_opcode__h18) {
  case (tUInt8)3u:
  case (tUInt8)19u:
  case (tUInt8)103u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_SEX_ETC___d181 = DEF_immI__h25;
    break;
  case (tUInt8)111u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_SEX_ETC___d181 = DEF_immJ__h29;
    break;
  case (tUInt8)99u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_SEX_ETC___d181 = DEF_immB__h27;
    break;
  case (tUInt8)35u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_SEX_ETC___d181 = DEF_immS__h26;
    break;
  default:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_SEX_ETC___d181 = DEF_immU__h28;
  }
  DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011_6_CONCAT__ETC___d184.set_bits_in_word(32767u & ((((((tUInt32)(DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011___d16)) << 14u) | (DEF_immI_BITS_11_TO_0___h1495 << 2u)) | (((tUInt32)(DEF_decode_inst_BITS_6_TO_0_EQ_0b10011___d2 || (!DEF_decode_inst_BITS_6_TO_0_EQ_0b110011___d3 && (DEF_decode_inst_BITS_6_TO_0_EQ_0b1101111___d5 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b1100111___d6 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b1100011___d7 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b11___d8 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b100011___d12 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b110111___d14 || DEF_decode_inst_BITS_6_TO_0_EQ_0b10111___d15))))))))) << 1u)) | (tUInt32)((tUInt8)(DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_SEX_ETC___d181 >> 31u))),
										 2u,
										 0u,
										 15u).set_whole_word((((tUInt32)(2147483647u & DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_SEX_ETC___d181)) << 1u) | (tUInt32)((tUInt8)(2863311530llu >> 32u)),
												     1u).set_whole_word((tUInt32)(2863311530llu),
															0u);
  DEF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decode_i_ETC___d185.set_bits_in_word(134217727u & (((((((tUInt32)(DEF_decode_inst_BITS_6_TO_0_EQ_0b10011___d2 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b110011___d3 || (DEF_NOT_decode_inst_BITS_6_TO_0_EQ_0b1101111___d83 && (DEF_decode_inst_BITS_6_TO_0_EQ_0b1100111___d6 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b1100011___d7 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b11___d8 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b100011___d12 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b110111___d14 || DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011___d16))))))))) << 26u) | (((tUInt32)(DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d123)) << 21u)) | (((tUInt32)(!DEF_decode_inst_BITS_6_TO_0_EQ_0b10011___d2 && (DEF_decode_inst_BITS_6_TO_0_EQ_0b110011___d3 || (DEF_NOT_decode_inst_BITS_6_TO_0_EQ_0b1101111___d83 && (!DEF_decode_inst_BITS_6_TO_0_EQ_0b1100111___d6 && (DEF_decode_inst_BITS_6_TO_0_EQ_0b1100011___d7 || DEF_decode_inst_BITS_6_TO_0_EQ_0b100011___d12)))))) << 20u)) | (((tUInt32)((tUInt8)((tUInt8)31u & (ARG_decode_inst >> 20u)))) << 15u)) | DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011_6_CONCAT__ETC___d184.get_bits_in_word32(2u,
																																																																																																																																														      0u,
																																																																																																																																														      15u)),
										 2u,
										 0u,
										 27u).set_whole_word(DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011_6_CONCAT__ETC___d184.get_whole_word(1u),
												     1u).set_whole_word(DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011_6_CONCAT__ETC___d184.get_whole_word(0u),
															0u);
  DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d186.build_concat(68719476735llu & ((((((tUInt64)(DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d67)) << 33u) | (((tUInt64)(!DEF_decode_inst_BITS_11_TO_7_8_EQ_0___d19 && (DEF_decode_inst_BITS_6_TO_0_EQ_0b10011___d2 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b110011___d3 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b1101111___d5 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b1100111___d6 || (!DEF_decode_inst_BITS_6_TO_0_EQ_0b1100011___d7 && (DEF_decode_inst_BITS_6_TO_0_EQ_0b11___d8 || (!DEF_decode_inst_BITS_6_TO_0_EQ_0b100011___d12 && (DEF_decode_inst_BITS_6_TO_0_EQ_0b110111___d14 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b10111___d15 || DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011___d16))))))))))) << 32u)) | (((tUInt64)(DEF_decode_inst_BITS_11_TO_7___d18)) << 27u)) | (tUInt64)(DEF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decode_i_ETC___d185.get_bits_in_word32(2u,
																																																																																																																       0u,
																																																																																																																       27u))),
									     64u,
									     36u).set_whole_word(DEF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decode_i_ETC___d185.get_whole_word(1u),
												 1u).set_whole_word(DEF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decode_i_ETC___d185.get_whole_word(0u),
														    0u);
  PORT_decode.set_bits_in_word(4095u & (((((tUInt32)(DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d34)) << 8u) | (((tUInt32)(DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_IF__ETC___d58)) << 4u)) | (tUInt32)(DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d186.get_bits_in_word8(3u,
																																					    0u,
																																					    4u))),
			       3u,
			       0u,
			       12u).set_whole_word(DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d186.get_whole_word(2u),
						   2u).set_whole_word(DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d186.get_whole_word(1u),
								      1u).set_whole_word(DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d186.get_whole_word(0u),
											 0u);
  return PORT_decode;
}

tUInt8 MOD_module_decode::METH_RDY_decode()
{
  tUInt8 PORT_RDY_decode;
  tUInt8 DEF_CAN_FIRE_decode;
  DEF_CAN_FIRE_decode = (tUInt8)1u;
  PORT_RDY_decode = DEF_CAN_FIRE_decode;
  return PORT_RDY_decode;
}


/* Reset routines */


/* Static handles to reset routines */


/* Functions for the parent module to register its reset fns */


/* Functions to set the elaborated clock id */


/* State dumping routine */
void MOD_module_decode::dump_state(unsigned int indent)
{
}


/* VCD dumping routines */

unsigned int MOD_module_decode::dump_VCD_defs(unsigned int levels)
{
  vcd_write_scope_start(sim_hdl, inst_name);
  vcd_num = vcd_reserve_ids(sim_hdl, 4u);
  unsigned int num = vcd_num;
  for (unsigned int clk = 0u; clk < bk_num_clocks(sim_hdl); ++clk)
    vcd_add_clock_def(sim_hdl, this, bk_clock_name(sim_hdl, clk), bk_clock_vcd_num(sim_hdl, clk));
  vcd_write_def(sim_hdl, num++, "IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d186", 100u);
  vcd_write_def(sim_hdl, num++, "decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decode_i_ETC___d185", 91u);
  vcd_write_def(sim_hdl, num++, "decode_inst_BITS_6_TO_0_EQ_0b1110011_6_CONCAT__ETC___d184", 79u);
  vcd_write_def(sim_hdl, num++, "decode", 108u);
  vcd_write_scope_end(sim_hdl);
  return num;
}

void MOD_module_decode::dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_module_decode &backing)
{
  vcd_defs(dt, backing);
}

void MOD_module_decode::vcd_defs(tVCDDumpType dt, MOD_module_decode &backing)
{
  unsigned int num = vcd_num;
  if (dt == VCD_DUMP_XS)
  {
    vcd_write_x(sim_hdl, num++, 100u);
    vcd_write_x(sim_hdl, num++, 91u);
    vcd_write_x(sim_hdl, num++, 79u);
    vcd_write_x(sim_hdl, num++, 108u);
  }
  else
    if (dt == VCD_DUMP_CHANGES)
    {
      if ((backing.DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d186) != DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d186)
      {
	vcd_write_val(sim_hdl, num, DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d186, 100u);
	backing.DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d186 = DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d186;
      }
      ++num;
      if ((backing.DEF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decode_i_ETC___d185) != DEF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decode_i_ETC___d185)
      {
	vcd_write_val(sim_hdl, num, DEF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decode_i_ETC___d185, 91u);
	backing.DEF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decode_i_ETC___d185 = DEF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decode_i_ETC___d185;
      }
      ++num;
      if ((backing.DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011_6_CONCAT__ETC___d184) != DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011_6_CONCAT__ETC___d184)
      {
	vcd_write_val(sim_hdl, num, DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011_6_CONCAT__ETC___d184, 79u);
	backing.DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011_6_CONCAT__ETC___d184 = DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011_6_CONCAT__ETC___d184;
      }
      ++num;
      if ((backing.PORT_decode) != PORT_decode)
      {
	vcd_write_val(sim_hdl, num, PORT_decode, 108u);
	backing.PORT_decode = PORT_decode;
      }
      ++num;
    }
    else
    {
      vcd_write_val(sim_hdl, num++, DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d186, 100u);
      backing.DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d186 = DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d186;
      vcd_write_val(sim_hdl, num++, DEF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decode_i_ETC___d185, 91u);
      backing.DEF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decode_i_ETC___d185 = DEF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decode_i_ETC___d185;
      vcd_write_val(sim_hdl, num++, DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011_6_CONCAT__ETC___d184, 79u);
      backing.DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011_6_CONCAT__ETC___d184 = DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011_6_CONCAT__ETC___d184;
      vcd_write_val(sim_hdl, num++, PORT_decode, 108u);
      backing.PORT_decode = PORT_decode;
    }
}
