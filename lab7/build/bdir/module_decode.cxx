/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Sun Jun 13 19:38:05 KST 2021
 * 
 */
#include "bluesim_primitives.h"
#include "module_decode.h"


/* Constructor */
MOD_module_decode::MOD_module_decode(tSimStateHdl simHdl, char const *name, Module *parent)
  : Module(simHdl, name, parent), DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d210(67u)
{
  PORT_decode.setSize(75u);
  PORT_decode.clear();
  symbol_count = 1u;
  symbols = new tSym[symbol_count];
  init_symbols_0();
}


/* Symbol init fns */

void MOD_module_decode::init_symbols_0()
{
  init_symbol(&symbols[0u], "decode", SYM_PORT, &PORT_decode, 75u);
}


/* Rule actions */


/* Methods */

tUWide MOD_module_decode::METH_decode(tUInt32 ARG_decode_inst)
{
  tUInt32 DEF_x__h1705;
  tUInt32 DEF_x__h1618;
  tUInt32 DEF_x__h1530;
  tUInt8 DEF_NOT_decode_inst_BITS_6_TO_0_EQ_0b1101111_3___d110;
  tUInt8 DEF_NOT_decode_inst_BITS_6_TO_0_EQ_0b10111_2___d109;
  tUInt8 DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_OR_decode__ETC___d91;
  tUInt8 DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d94;
  tUInt8 DEF_decode_inst_BITS_14_TO_12_EQ_0b10___d6;
  tUInt8 DEF_decode_inst_BITS_11_TO_7_7_EQ_0___d38;
  tUInt8 DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b1_1_THEN_IF__ETC___d44;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011___d36;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b100011___d34;
  tUInt8 DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_OR_decode__ETC___d31;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b10111___d22;
  tUInt8 DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_IF_de_ETC___d72;
  tUInt8 DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_OR_decode__ETC___d20;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b110011___d3;
  tUInt8 DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_0_ELS_ETC___d63;
  tUInt8 DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_IF__ETC___d81;
  tUInt8 DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_1_E_ETC___d54;
  tUInt8 DEF_IF_decode_inst_BIT_30_5_THEN_8_ELSE_9___d56;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b110111___d21;
  tUInt8 DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d146;
  tUInt32 DEF_immS__h24;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b11___d32;
  tUInt32 DEF_immB__h25;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b1100011___d25;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b1100111___d24;
  tUInt32 DEF_immJ__h27;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b1101111___d23;
  tUInt8 DEF_decode_inst_BITS_6_TO_0_EQ_0b10011___d2;
  tUInt32 DEF_immU__h26;
  tUInt32 DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_SEX_ETC___d206;
  tUInt8 DEF_decode_inst_BIT_30___d55;
  tUInt8 DEF_decode_inst_BIT_31___d185;
  tUInt8 DEF_funct3__h19;
  tUInt8 DEF_decode_inst_BITS_11_TO_7___d37;
  tUInt8 DEF_rs1__h20;
  tUInt8 DEF_opcode__h17;
  tUInt32 DEF_x__h1433;
  tUInt32 DEF_immI__h23;
  tUInt32 DEF_immI_BITS_11_TO_0___h1453;
  DEF_x__h1433 = (tUInt32)(ARG_decode_inst >> 20u);
  DEF_immI__h23 = primSignExt32(32u, 12u, (tUInt32)(DEF_x__h1433));
  DEF_immI_BITS_11_TO_0___h1453 = (tUInt32)(4095u & DEF_immI__h23);
  DEF_opcode__h17 = (tUInt8)((tUInt8)127u & ARG_decode_inst);
  DEF_rs1__h20 = (tUInt8)((tUInt8)31u & (ARG_decode_inst >> 15u));
  DEF_decode_inst_BITS_11_TO_7___d37 = (tUInt8)((tUInt8)31u & (ARG_decode_inst >> 7u));
  DEF_funct3__h19 = (tUInt8)((tUInt8)7u & (ARG_decode_inst >> 12u));
  DEF_decode_inst_BIT_31___d185 = (tUInt8)(ARG_decode_inst >> 31u);
  DEF_decode_inst_BIT_30___d55 = (tUInt8)((tUInt8)1u & (ARG_decode_inst >> 30u));
  DEF_immU__h26 = ((tUInt32)(ARG_decode_inst >> 12u)) << 12u;
  DEF_decode_inst_BITS_6_TO_0_EQ_0b10011___d2 = DEF_opcode__h17 == (tUInt8)19u;
  DEF_decode_inst_BITS_6_TO_0_EQ_0b1101111___d23 = DEF_opcode__h17 == (tUInt8)111u;
  DEF_decode_inst_BITS_6_TO_0_EQ_0b1100111___d24 = DEF_opcode__h17 == (tUInt8)103u;
  DEF_decode_inst_BITS_6_TO_0_EQ_0b1100011___d25 = DEF_opcode__h17 == (tUInt8)99u;
  DEF_decode_inst_BITS_6_TO_0_EQ_0b11___d32 = DEF_opcode__h17 == (tUInt8)3u;
  switch (DEF_opcode__h17) {
  case (tUInt8)55u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d146 = (tUInt8)0u;
    break;
  default:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d146 = DEF_rs1__h20;
  }
  DEF_decode_inst_BITS_6_TO_0_EQ_0b110111___d21 = DEF_opcode__h17 == (tUInt8)55u;
  DEF_IF_decode_inst_BIT_30_5_THEN_8_ELSE_9___d56 = DEF_decode_inst_BIT_30___d55 ? (tUInt8)8u : (tUInt8)9u;
  switch (DEF_funct3__h19) {
  case (tUInt8)0u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_0_ELS_ETC___d63 = (tUInt8)0u;
    break;
  case (tUInt8)2u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_0_ELS_ETC___d63 = (tUInt8)5u;
    break;
  case (tUInt8)3u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_0_ELS_ETC___d63 = (tUInt8)6u;
    break;
  case (tUInt8)7u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_0_ELS_ETC___d63 = (tUInt8)2u;
    break;
  case (tUInt8)6u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_0_ELS_ETC___d63 = (tUInt8)3u;
    break;
  case (tUInt8)4u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_0_ELS_ETC___d63 = (tUInt8)4u;
    break;
  case (tUInt8)1u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_0_ELS_ETC___d63 = (tUInt8)7u;
    break;
  default:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_0_ELS_ETC___d63 = DEF_IF_decode_inst_BIT_30_5_THEN_8_ELSE_9___d56;
  }
  DEF_decode_inst_BITS_6_TO_0_EQ_0b110011___d3 = DEF_opcode__h17 == (tUInt8)51u;
  switch (DEF_funct3__h19) {
  case (tUInt8)0u:
  case (tUInt8)1u:
  case (tUInt8)2u:
  case (tUInt8)3u:
  case (tUInt8)4u:
  case (tUInt8)5u:
  case (tUInt8)6u:
  case (tUInt8)7u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_OR_decode__ETC___d20 = (tUInt8)1u;
    break;
  default:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_OR_decode__ETC___d20 = (tUInt8)0u;
  }
  switch (DEF_funct3__h19) {
  case (tUInt8)0u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_IF_de_ETC___d72 = DEF_decode_inst_BIT_30___d55 ? (tUInt8)1u : (tUInt8)0u;
    break;
  case (tUInt8)2u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_IF_de_ETC___d72 = (tUInt8)5u;
    break;
  case (tUInt8)3u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_IF_de_ETC___d72 = (tUInt8)6u;
    break;
  case (tUInt8)7u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_IF_de_ETC___d72 = (tUInt8)2u;
    break;
  case (tUInt8)6u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_IF_de_ETC___d72 = (tUInt8)3u;
    break;
  case (tUInt8)4u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_IF_de_ETC___d72 = (tUInt8)4u;
    break;
  case (tUInt8)1u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_IF_de_ETC___d72 = (tUInt8)7u;
    break;
  default:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_IF_de_ETC___d72 = DEF_IF_decode_inst_BIT_30_5_THEN_8_ELSE_9___d56;
  }
  switch (DEF_opcode__h17) {
  case (tUInt8)19u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_IF__ETC___d81 = DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_0_ELS_ETC___d63;
    break;
  case (tUInt8)51u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_IF__ETC___d81 = DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_THEN_IF_de_ETC___d72;
    break;
  default:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_IF__ETC___d81 = (tUInt8)0u;
  }
  DEF_decode_inst_BITS_6_TO_0_EQ_0b10111___d22 = DEF_opcode__h17 == (tUInt8)23u;
  switch (DEF_funct3__h19) {
  case (tUInt8)0u:
  case (tUInt8)1u:
  case (tUInt8)4u:
  case (tUInt8)5u:
  case (tUInt8)6u:
  case (tUInt8)7u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_OR_decode__ETC___d31 = (tUInt8)6u;
    break;
  default:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_OR_decode__ETC___d31 = (tUInt8)0u;
  }
  DEF_decode_inst_BITS_6_TO_0_EQ_0b100011___d34 = DEF_opcode__h17 == (tUInt8)35u;
  DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011___d36 = DEF_opcode__h17 == (tUInt8)115u;
  DEF_decode_inst_BITS_11_TO_7_7_EQ_0___d38 = DEF_decode_inst_BITS_11_TO_7___d37 == (tUInt8)0u;
  switch (DEF_funct3__h19) {
  case (tUInt8)1u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b1_1_THEN_IF__ETC___d44 = DEF_decode_inst_BITS_11_TO_7_7_EQ_0___d38 ? (tUInt8)8u : (tUInt8)0u;
    break;
  case (tUInt8)2u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b1_1_THEN_IF__ETC___d44 = DEF_rs1__h20 == (tUInt8)0u ? (tUInt8)7u : (tUInt8)0u;
    break;
  default:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b1_1_THEN_IF__ETC___d44 = (tUInt8)0u;
  }
  DEF_decode_inst_BITS_14_TO_12_EQ_0b10___d6 = DEF_funct3__h19 == (tUInt8)2u;
  switch (DEF_opcode__h17) {
  case (tUInt8)19u:
  case (tUInt8)55u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_1_E_ETC___d54 = (tUInt8)1u;
    break;
  case (tUInt8)51u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_1_E_ETC___d54 = DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_OR_decode__ETC___d20;
    break;
  case (tUInt8)23u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_1_E_ETC___d54 = (tUInt8)9u;
    break;
  case (tUInt8)111u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_1_E_ETC___d54 = (tUInt8)4u;
    break;
  case (tUInt8)103u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_1_E_ETC___d54 = (tUInt8)5u;
    break;
  case (tUInt8)99u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_1_E_ETC___d54 = DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_OR_decode__ETC___d31;
    break;
  case (tUInt8)3u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_1_E_ETC___d54 = DEF_decode_inst_BITS_14_TO_12_EQ_0b10___d6 ? (tUInt8)2u : (tUInt8)0u;
    break;
  case (tUInt8)35u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_1_E_ETC___d54 = DEF_decode_inst_BITS_14_TO_12_EQ_0b10___d6 ? (tUInt8)3u : (tUInt8)0u;
    break;
  case (tUInt8)115u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_1_E_ETC___d54 = DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b1_1_THEN_IF__ETC___d44;
    break;
  default:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_1_E_ETC___d54 = (tUInt8)0u;
  }
  switch (DEF_funct3__h19) {
  case (tUInt8)0u:
  case (tUInt8)1u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_OR_decode__ETC___d91 = DEF_funct3__h19;
    break;
  case (tUInt8)4u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_OR_decode__ETC___d91 = (tUInt8)2u;
    break;
  case (tUInt8)6u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_OR_decode__ETC___d91 = (tUInt8)3u;
    break;
  case (tUInt8)5u:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_OR_decode__ETC___d91 = (tUInt8)4u;
    break;
  default:
    DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_OR_decode__ETC___d91 = (tUInt8)5u;
  }
  switch (DEF_opcode__h17) {
  case (tUInt8)103u:
  case (tUInt8)111u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d94 = (tUInt8)6u;
    break;
  case (tUInt8)99u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d94 = DEF_IF_decode_inst_BITS_14_TO_12_EQ_0b0_OR_decode__ETC___d91;
    break;
  default:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d94 = (tUInt8)7u;
  }
  DEF_NOT_decode_inst_BITS_6_TO_0_EQ_0b10111_2___d109 = !DEF_decode_inst_BITS_6_TO_0_EQ_0b10111___d22;
  DEF_NOT_decode_inst_BITS_6_TO_0_EQ_0b1101111_3___d110 = !DEF_decode_inst_BITS_6_TO_0_EQ_0b1101111___d23;
  DEF_x__h1530 = 2097151u & (((((((tUInt32)(DEF_decode_inst_BIT_31___d185)) << 20u) | (((tUInt32)((tUInt8)((tUInt8)255u & (ARG_decode_inst >> 12u)))) << 12u)) | (((tUInt32)((tUInt8)((tUInt8)1u & (ARG_decode_inst >> 20u)))) << 11u)) | (((tUInt32)(1023u & (ARG_decode_inst >> 21u))) << 1u)) | (tUInt32)((tUInt8)0u));
  DEF_immJ__h27 = primSignExt32(32u, 21u, (tUInt32)(DEF_x__h1530));
  DEF_x__h1618 = 8191u & (((((((tUInt32)(DEF_decode_inst_BIT_31___d185)) << 12u) | (((tUInt32)((tUInt8)((tUInt8)1u & (ARG_decode_inst >> 7u)))) << 11u)) | (((tUInt32)((tUInt8)((tUInt8)63u & (ARG_decode_inst >> 25u)))) << 5u)) | (((tUInt32)((tUInt8)((tUInt8)15u & (ARG_decode_inst >> 8u)))) << 1u)) | (tUInt32)((tUInt8)0u));
  DEF_immB__h25 = primSignExt32(32u, 13u, (tUInt32)(DEF_x__h1618));
  DEF_x__h1705 = 4095u & ((((tUInt32)((tUInt8)(ARG_decode_inst >> 25u))) << 5u) | (tUInt32)(DEF_decode_inst_BITS_11_TO_7___d37));
  DEF_immS__h24 = primSignExt32(32u, 12u, (tUInt32)(DEF_x__h1705));
  switch (DEF_opcode__h17) {
  case (tUInt8)3u:
  case (tUInt8)19u:
  case (tUInt8)103u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_SEX_ETC___d206 = DEF_immI__h23;
    break;
  case (tUInt8)23u:
  case (tUInt8)55u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_SEX_ETC___d206 = DEF_immU__h26;
    break;
  case (tUInt8)111u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_SEX_ETC___d206 = DEF_immJ__h27;
    break;
  case (tUInt8)99u:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_SEX_ETC___d206 = DEF_immB__h25;
    break;
  default:
    DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_SEX_ETC___d206 = DEF_immS__h24;
  }
  DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d210.set_bits_in_word(DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d94,
										 2u,
										 0u,
										 3u).build_concat(!DEF_decode_inst_BITS_11_TO_7_7_EQ_0___d38 && (DEF_decode_inst_BITS_6_TO_0_EQ_0b10011___d2 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b110011___d3 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b110111___d21 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b10111___d22 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b1101111___d23 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b1100111___d24 || (!DEF_decode_inst_BITS_6_TO_0_EQ_0b1100011___d25 && (DEF_decode_inst_BITS_6_TO_0_EQ_0b11___d32 || DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011___d36)))))))),
												  63u,
												  1u).set_bits_in_word(DEF_decode_inst_BITS_11_TO_7___d37,
														       1u,
														       26u,
														       5u).set_bits_in_word(DEF_decode_inst_BITS_6_TO_0_EQ_0b10011___d2 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b110011___d3 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b110111___d21 || (DEF_NOT_decode_inst_BITS_6_TO_0_EQ_0b10111_2___d109 && (DEF_NOT_decode_inst_BITS_6_TO_0_EQ_0b1101111_3___d110 && (DEF_decode_inst_BITS_6_TO_0_EQ_0b1100111___d24 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b1100011___d25 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b11___d32 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b100011___d34 || DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011___d36)))))))),
																	    1u,
																	    25u,
																	    1u).set_bits_in_word(DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d146,
																				 1u,
																				 20u,
																				 5u).set_bits_in_word(!DEF_decode_inst_BITS_6_TO_0_EQ_0b10011___d2 && (DEF_decode_inst_BITS_6_TO_0_EQ_0b110011___d3 || (!DEF_decode_inst_BITS_6_TO_0_EQ_0b110111___d21 && (DEF_NOT_decode_inst_BITS_6_TO_0_EQ_0b10111_2___d109 && (DEF_NOT_decode_inst_BITS_6_TO_0_EQ_0b1101111_3___d110 && (!DEF_decode_inst_BITS_6_TO_0_EQ_0b1100111___d24 && (DEF_decode_inst_BITS_6_TO_0_EQ_0b1100011___d25 || DEF_decode_inst_BITS_6_TO_0_EQ_0b100011___d34)))))),
																						      1u,
																						      19u,
																						      1u).set_bits_in_word((tUInt8)((tUInt8)31u & (ARG_decode_inst >> 20u)),
																									   1u,
																									   14u,
																									   5u).set_bits_in_word(DEF_decode_inst_BITS_6_TO_0_EQ_0b1110011___d36,
																												1u,
																												13u,
																												1u).set_bits_in_word(DEF_immI_BITS_11_TO_0___h1453,
																														     1u,
																														     1u,
																														     12u).set_bits_in_word(DEF_decode_inst_BITS_6_TO_0_EQ_0b10011___d2 || (!DEF_decode_inst_BITS_6_TO_0_EQ_0b110011___d3 && ((DEF_decode_inst_BITS_6_TO_0_EQ_0b110111___d21 || DEF_decode_inst_BITS_6_TO_0_EQ_0b10111___d22) || (DEF_decode_inst_BITS_6_TO_0_EQ_0b1101111___d23 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b1100111___d24 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b1100011___d25 || (DEF_decode_inst_BITS_6_TO_0_EQ_0b11___d32 || DEF_decode_inst_BITS_6_TO_0_EQ_0b100011___d34)))))),
																																	   1u,
																																	   0u,
																																	   1u).set_whole_word(DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_SEX_ETC___d206,
																																			      0u);
  PORT_decode.set_bits_in_word(2047u & (((((tUInt32)(DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_1_E_ETC___d54)) << 7u) | (((tUInt32)(DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_THEN_IF__ETC___d81)) << 3u)) | (tUInt32)(DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d210.get_bits_in_word8(2u,
																																					    0u,
																																					    3u))),
			       2u,
			       0u,
			       11u).set_whole_word(DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d210.get_whole_word(1u),
						   1u).set_whole_word(DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d210.get_whole_word(0u),
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
  vcd_num = vcd_reserve_ids(sim_hdl, 2u);
  unsigned int num = vcd_num;
  for (unsigned int clk = 0u; clk < bk_num_clocks(sim_hdl); ++clk)
    vcd_add_clock_def(sim_hdl, this, bk_clock_name(sim_hdl, clk), bk_clock_vcd_num(sim_hdl, clk));
  vcd_write_def(sim_hdl, num++, "IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d210", 67u);
  vcd_write_def(sim_hdl, num++, "decode", 75u);
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
    vcd_write_x(sim_hdl, num++, 67u);
    vcd_write_x(sim_hdl, num++, 75u);
  }
  else
    if (dt == VCD_DUMP_CHANGES)
    {
      if ((backing.DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d210) != DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d210)
      {
	vcd_write_val(sim_hdl, num, DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d210, 67u);
	backing.DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d210 = DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d210;
      }
      ++num;
      if ((backing.PORT_decode) != PORT_decode)
      {
	vcd_write_val(sim_hdl, num, PORT_decode, 75u);
	backing.PORT_decode = PORT_decode;
      }
      ++num;
    }
    else
    {
      vcd_write_val(sim_hdl, num++, DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d210, 67u);
      backing.DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d210 = DEF_IF_decode_inst_BITS_6_TO_0_EQ_0b10011_OR_decod_ETC___d210;
      vcd_write_val(sim_hdl, num++, PORT_decode, 75u);
      backing.PORT_decode = PORT_decode;
    }
}
