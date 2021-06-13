/*
 * Generated by Bluespec Compiler (build ad73d8a)
 * 
 * On Sun Jun 13 01:07:20 KST 2021
 * 
 */
#include "bluesim_primitives.h"
#include "mkRFile.h"


/* Constructor */
MOD_mkRFile::MOD_mkRFile(tSimStateHdl simHdl, char const *name, Module *parent)
  : Module(simHdl, name, parent),
    __clk_handle_0(BAD_CLOCK_HANDLE),
    INST_rfile_0(simHdl, "rfile_0", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_1(simHdl, "rfile_1", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_10(simHdl, "rfile_10", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_11(simHdl, "rfile_11", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_12(simHdl, "rfile_12", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_13(simHdl, "rfile_13", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_14(simHdl, "rfile_14", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_15(simHdl, "rfile_15", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_16(simHdl, "rfile_16", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_17(simHdl, "rfile_17", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_18(simHdl, "rfile_18", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_19(simHdl, "rfile_19", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_2(simHdl, "rfile_2", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_20(simHdl, "rfile_20", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_21(simHdl, "rfile_21", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_22(simHdl, "rfile_22", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_23(simHdl, "rfile_23", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_24(simHdl, "rfile_24", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_25(simHdl, "rfile_25", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_26(simHdl, "rfile_26", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_27(simHdl, "rfile_27", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_28(simHdl, "rfile_28", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_29(simHdl, "rfile_29", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_3(simHdl, "rfile_3", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_30(simHdl, "rfile_30", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_31(simHdl, "rfile_31", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_4(simHdl, "rfile_4", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_5(simHdl, "rfile_5", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_6(simHdl, "rfile_6", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_7(simHdl, "rfile_7", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_8(simHdl, "rfile_8", this, 32u, 0u, (tUInt8)0u),
    INST_rfile_9(simHdl, "rfile_9", this, 32u, 0u, (tUInt8)0u),
    PORT_RST_N((tUInt8)1u)
{
  symbol_count = 32u;
  symbols = new tSym[symbol_count];
  init_symbols_0();
}


/* Symbol init fns */

void MOD_mkRFile::init_symbols_0()
{
  init_symbol(&symbols[0u], "rfile_0", SYM_MODULE, &INST_rfile_0);
  init_symbol(&symbols[1u], "rfile_1", SYM_MODULE, &INST_rfile_1);
  init_symbol(&symbols[2u], "rfile_10", SYM_MODULE, &INST_rfile_10);
  init_symbol(&symbols[3u], "rfile_11", SYM_MODULE, &INST_rfile_11);
  init_symbol(&symbols[4u], "rfile_12", SYM_MODULE, &INST_rfile_12);
  init_symbol(&symbols[5u], "rfile_13", SYM_MODULE, &INST_rfile_13);
  init_symbol(&symbols[6u], "rfile_14", SYM_MODULE, &INST_rfile_14);
  init_symbol(&symbols[7u], "rfile_15", SYM_MODULE, &INST_rfile_15);
  init_symbol(&symbols[8u], "rfile_16", SYM_MODULE, &INST_rfile_16);
  init_symbol(&symbols[9u], "rfile_17", SYM_MODULE, &INST_rfile_17);
  init_symbol(&symbols[10u], "rfile_18", SYM_MODULE, &INST_rfile_18);
  init_symbol(&symbols[11u], "rfile_19", SYM_MODULE, &INST_rfile_19);
  init_symbol(&symbols[12u], "rfile_2", SYM_MODULE, &INST_rfile_2);
  init_symbol(&symbols[13u], "rfile_20", SYM_MODULE, &INST_rfile_20);
  init_symbol(&symbols[14u], "rfile_21", SYM_MODULE, &INST_rfile_21);
  init_symbol(&symbols[15u], "rfile_22", SYM_MODULE, &INST_rfile_22);
  init_symbol(&symbols[16u], "rfile_23", SYM_MODULE, &INST_rfile_23);
  init_symbol(&symbols[17u], "rfile_24", SYM_MODULE, &INST_rfile_24);
  init_symbol(&symbols[18u], "rfile_25", SYM_MODULE, &INST_rfile_25);
  init_symbol(&symbols[19u], "rfile_26", SYM_MODULE, &INST_rfile_26);
  init_symbol(&symbols[20u], "rfile_27", SYM_MODULE, &INST_rfile_27);
  init_symbol(&symbols[21u], "rfile_28", SYM_MODULE, &INST_rfile_28);
  init_symbol(&symbols[22u], "rfile_29", SYM_MODULE, &INST_rfile_29);
  init_symbol(&symbols[23u], "rfile_3", SYM_MODULE, &INST_rfile_3);
  init_symbol(&symbols[24u], "rfile_30", SYM_MODULE, &INST_rfile_30);
  init_symbol(&symbols[25u], "rfile_31", SYM_MODULE, &INST_rfile_31);
  init_symbol(&symbols[26u], "rfile_4", SYM_MODULE, &INST_rfile_4);
  init_symbol(&symbols[27u], "rfile_5", SYM_MODULE, &INST_rfile_5);
  init_symbol(&symbols[28u], "rfile_6", SYM_MODULE, &INST_rfile_6);
  init_symbol(&symbols[29u], "rfile_7", SYM_MODULE, &INST_rfile_7);
  init_symbol(&symbols[30u], "rfile_8", SYM_MODULE, &INST_rfile_8);
  init_symbol(&symbols[31u], "rfile_9", SYM_MODULE, &INST_rfile_9);
}


/* Rule actions */


/* Methods */

void MOD_mkRFile::METH_wr(tUInt8 ARG_wr_rindx, tUInt32 ARG_wr_data)
{
  tUInt8 DEF_wr_rindx_EQ_1___d1;
  tUInt8 DEF_wr_rindx_EQ_2___d2;
  tUInt8 DEF_wr_rindx_EQ_3___d3;
  tUInt8 DEF_wr_rindx_EQ_4___d4;
  tUInt8 DEF_wr_rindx_EQ_5___d5;
  tUInt8 DEF_wr_rindx_EQ_6___d6;
  tUInt8 DEF_wr_rindx_EQ_7___d7;
  tUInt8 DEF_wr_rindx_EQ_8___d8;
  tUInt8 DEF_wr_rindx_EQ_9___d9;
  tUInt8 DEF_wr_rindx_EQ_10___d10;
  tUInt8 DEF_wr_rindx_EQ_11___d11;
  tUInt8 DEF_wr_rindx_EQ_12___d12;
  tUInt8 DEF_wr_rindx_EQ_13___d13;
  tUInt8 DEF_wr_rindx_EQ_14___d14;
  tUInt8 DEF_wr_rindx_EQ_15___d15;
  tUInt8 DEF_wr_rindx_EQ_16___d16;
  tUInt8 DEF_wr_rindx_EQ_17___d17;
  tUInt8 DEF_wr_rindx_EQ_18___d18;
  tUInt8 DEF_wr_rindx_EQ_19___d19;
  tUInt8 DEF_wr_rindx_EQ_20___d20;
  tUInt8 DEF_wr_rindx_EQ_21___d21;
  tUInt8 DEF_wr_rindx_EQ_22___d22;
  tUInt8 DEF_wr_rindx_EQ_23___d23;
  tUInt8 DEF_wr_rindx_EQ_24___d24;
  tUInt8 DEF_wr_rindx_EQ_25___d25;
  tUInt8 DEF_wr_rindx_EQ_26___d26;
  tUInt8 DEF_wr_rindx_EQ_27___d27;
  tUInt8 DEF_wr_rindx_EQ_28___d28;
  tUInt8 DEF_wr_rindx_EQ_29___d29;
  tUInt8 DEF_wr_rindx_EQ_30___d30;
  tUInt8 DEF_wr_rindx_EQ_31___d31;
  DEF_wr_rindx_EQ_31___d31 = ARG_wr_rindx == (tUInt8)31u;
  DEF_wr_rindx_EQ_30___d30 = ARG_wr_rindx == (tUInt8)30u;
  DEF_wr_rindx_EQ_29___d29 = ARG_wr_rindx == (tUInt8)29u;
  DEF_wr_rindx_EQ_28___d28 = ARG_wr_rindx == (tUInt8)28u;
  DEF_wr_rindx_EQ_27___d27 = ARG_wr_rindx == (tUInt8)27u;
  DEF_wr_rindx_EQ_26___d26 = ARG_wr_rindx == (tUInt8)26u;
  DEF_wr_rindx_EQ_25___d25 = ARG_wr_rindx == (tUInt8)25u;
  DEF_wr_rindx_EQ_24___d24 = ARG_wr_rindx == (tUInt8)24u;
  DEF_wr_rindx_EQ_23___d23 = ARG_wr_rindx == (tUInt8)23u;
  DEF_wr_rindx_EQ_22___d22 = ARG_wr_rindx == (tUInt8)22u;
  DEF_wr_rindx_EQ_21___d21 = ARG_wr_rindx == (tUInt8)21u;
  DEF_wr_rindx_EQ_20___d20 = ARG_wr_rindx == (tUInt8)20u;
  DEF_wr_rindx_EQ_19___d19 = ARG_wr_rindx == (tUInt8)19u;
  DEF_wr_rindx_EQ_18___d18 = ARG_wr_rindx == (tUInt8)18u;
  DEF_wr_rindx_EQ_17___d17 = ARG_wr_rindx == (tUInt8)17u;
  DEF_wr_rindx_EQ_16___d16 = ARG_wr_rindx == (tUInt8)16u;
  DEF_wr_rindx_EQ_15___d15 = ARG_wr_rindx == (tUInt8)15u;
  DEF_wr_rindx_EQ_14___d14 = ARG_wr_rindx == (tUInt8)14u;
  DEF_wr_rindx_EQ_13___d13 = ARG_wr_rindx == (tUInt8)13u;
  DEF_wr_rindx_EQ_12___d12 = ARG_wr_rindx == (tUInt8)12u;
  DEF_wr_rindx_EQ_11___d11 = ARG_wr_rindx == (tUInt8)11u;
  DEF_wr_rindx_EQ_10___d10 = ARG_wr_rindx == (tUInt8)10u;
  DEF_wr_rindx_EQ_9___d9 = ARG_wr_rindx == (tUInt8)9u;
  DEF_wr_rindx_EQ_8___d8 = ARG_wr_rindx == (tUInt8)8u;
  DEF_wr_rindx_EQ_7___d7 = ARG_wr_rindx == (tUInt8)7u;
  DEF_wr_rindx_EQ_6___d6 = ARG_wr_rindx == (tUInt8)6u;
  DEF_wr_rindx_EQ_5___d5 = ARG_wr_rindx == (tUInt8)5u;
  DEF_wr_rindx_EQ_4___d4 = ARG_wr_rindx == (tUInt8)4u;
  DEF_wr_rindx_EQ_3___d3 = ARG_wr_rindx == (tUInt8)3u;
  DEF_wr_rindx_EQ_2___d2 = ARG_wr_rindx == (tUInt8)2u;
  DEF_wr_rindx_EQ_1___d1 = ARG_wr_rindx == (tUInt8)1u;
  
  /* dead code removed here */
  ;
  if (DEF_wr_rindx_EQ_1___d1)
    INST_rfile_1.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_2___d2)
    INST_rfile_2.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_3___d3)
    INST_rfile_3.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_4___d4)
    INST_rfile_4.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_5___d5)
    INST_rfile_5.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_6___d6)
    INST_rfile_6.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_7___d7)
    INST_rfile_7.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_8___d8)
    INST_rfile_8.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_9___d9)
    INST_rfile_9.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_10___d10)
    INST_rfile_10.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_11___d11)
    INST_rfile_11.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_12___d12)
    INST_rfile_12.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_13___d13)
    INST_rfile_13.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_14___d14)
    INST_rfile_14.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_15___d15)
    INST_rfile_15.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_16___d16)
    INST_rfile_16.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_17___d17)
    INST_rfile_17.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_18___d18)
    INST_rfile_18.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_19___d19)
    INST_rfile_19.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_20___d20)
    INST_rfile_20.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_21___d21)
    INST_rfile_21.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_22___d22)
    INST_rfile_22.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_23___d23)
    INST_rfile_23.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_24___d24)
    INST_rfile_24.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_25___d25)
    INST_rfile_25.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_26___d26)
    INST_rfile_26.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_27___d27)
    INST_rfile_27.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_28___d28)
    INST_rfile_28.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_29___d29)
    INST_rfile_29.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_31___d31)
    INST_rfile_31.METH_write(ARG_wr_data);
  if (DEF_wr_rindx_EQ_30___d30)
    INST_rfile_30.METH_write(ARG_wr_data);
}

tUInt8 MOD_mkRFile::METH_RDY_wr()
{
  tUInt8 PORT_RDY_wr;
  tUInt8 DEF_CAN_FIRE_wr;
  DEF_CAN_FIRE_wr = (tUInt8)1u;
  PORT_RDY_wr = DEF_CAN_FIRE_wr;
  return PORT_RDY_wr;
}

tUInt32 MOD_mkRFile::METH_rd1(tUInt8 ARG_rd1_rindx)
{
  tUInt32 PORT_rd1;
  DEF__read__h1890 = INST_rfile_31.METH_read();
  DEF__read__h1859 = INST_rfile_30.METH_read();
  DEF__read__h1828 = INST_rfile_29.METH_read();
  DEF__read__h1797 = INST_rfile_28.METH_read();
  DEF__read__h1766 = INST_rfile_27.METH_read();
  DEF__read__h1735 = INST_rfile_26.METH_read();
  DEF__read__h1704 = INST_rfile_25.METH_read();
  DEF__read__h1673 = INST_rfile_24.METH_read();
  DEF__read__h1642 = INST_rfile_23.METH_read();
  DEF__read__h1611 = INST_rfile_22.METH_read();
  DEF__read__h1580 = INST_rfile_21.METH_read();
  DEF__read__h1549 = INST_rfile_20.METH_read();
  DEF__read__h1518 = INST_rfile_19.METH_read();
  DEF__read__h1487 = INST_rfile_18.METH_read();
  DEF__read__h1456 = INST_rfile_17.METH_read();
  DEF__read__h1425 = INST_rfile_16.METH_read();
  DEF__read__h1394 = INST_rfile_15.METH_read();
  DEF__read__h1363 = INST_rfile_14.METH_read();
  DEF__read__h1332 = INST_rfile_13.METH_read();
  DEF__read__h1301 = INST_rfile_12.METH_read();
  DEF__read__h1270 = INST_rfile_11.METH_read();
  DEF__read__h1239 = INST_rfile_10.METH_read();
  DEF__read__h1208 = INST_rfile_9.METH_read();
  DEF__read__h1177 = INST_rfile_8.METH_read();
  DEF__read__h1146 = INST_rfile_7.METH_read();
  DEF__read__h1115 = INST_rfile_6.METH_read();
  DEF__read__h1084 = INST_rfile_5.METH_read();
  DEF__read__h1053 = INST_rfile_4.METH_read();
  DEF__read__h1022 = INST_rfile_3.METH_read();
  DEF__read__h991 = INST_rfile_2.METH_read();
  DEF__read__h960 = INST_rfile_1.METH_read();
  DEF__read__h929 = INST_rfile_0.METH_read();
  switch (ARG_rd1_rindx) {
  case (tUInt8)0u:
    PORT_rd1 = DEF__read__h929;
    break;
  case (tUInt8)1u:
    PORT_rd1 = DEF__read__h960;
    break;
  case (tUInt8)2u:
    PORT_rd1 = DEF__read__h991;
    break;
  case (tUInt8)3u:
    PORT_rd1 = DEF__read__h1022;
    break;
  case (tUInt8)4u:
    PORT_rd1 = DEF__read__h1053;
    break;
  case (tUInt8)5u:
    PORT_rd1 = DEF__read__h1084;
    break;
  case (tUInt8)6u:
    PORT_rd1 = DEF__read__h1115;
    break;
  case (tUInt8)7u:
    PORT_rd1 = DEF__read__h1146;
    break;
  case (tUInt8)8u:
    PORT_rd1 = DEF__read__h1177;
    break;
  case (tUInt8)9u:
    PORT_rd1 = DEF__read__h1208;
    break;
  case (tUInt8)10u:
    PORT_rd1 = DEF__read__h1239;
    break;
  case (tUInt8)11u:
    PORT_rd1 = DEF__read__h1270;
    break;
  case (tUInt8)12u:
    PORT_rd1 = DEF__read__h1301;
    break;
  case (tUInt8)13u:
    PORT_rd1 = DEF__read__h1332;
    break;
  case (tUInt8)14u:
    PORT_rd1 = DEF__read__h1363;
    break;
  case (tUInt8)15u:
    PORT_rd1 = DEF__read__h1394;
    break;
  case (tUInt8)16u:
    PORT_rd1 = DEF__read__h1425;
    break;
  case (tUInt8)17u:
    PORT_rd1 = DEF__read__h1456;
    break;
  case (tUInt8)18u:
    PORT_rd1 = DEF__read__h1487;
    break;
  case (tUInt8)19u:
    PORT_rd1 = DEF__read__h1518;
    break;
  case (tUInt8)20u:
    PORT_rd1 = DEF__read__h1549;
    break;
  case (tUInt8)21u:
    PORT_rd1 = DEF__read__h1580;
    break;
  case (tUInt8)22u:
    PORT_rd1 = DEF__read__h1611;
    break;
  case (tUInt8)23u:
    PORT_rd1 = DEF__read__h1642;
    break;
  case (tUInt8)24u:
    PORT_rd1 = DEF__read__h1673;
    break;
  case (tUInt8)25u:
    PORT_rd1 = DEF__read__h1704;
    break;
  case (tUInt8)26u:
    PORT_rd1 = DEF__read__h1735;
    break;
  case (tUInt8)27u:
    PORT_rd1 = DEF__read__h1766;
    break;
  case (tUInt8)28u:
    PORT_rd1 = DEF__read__h1797;
    break;
  case (tUInt8)29u:
    PORT_rd1 = DEF__read__h1828;
    break;
  case (tUInt8)30u:
    PORT_rd1 = DEF__read__h1859;
    break;
  case (tUInt8)31u:
    PORT_rd1 = DEF__read__h1890;
    break;
  default:
    PORT_rd1 = 2863311530u;
  }
  return PORT_rd1;
}

tUInt8 MOD_mkRFile::METH_RDY_rd1()
{
  tUInt8 PORT_RDY_rd1;
  tUInt8 DEF_CAN_FIRE_rd1;
  DEF_CAN_FIRE_rd1 = (tUInt8)1u;
  PORT_RDY_rd1 = DEF_CAN_FIRE_rd1;
  return PORT_RDY_rd1;
}

tUInt32 MOD_mkRFile::METH_rd2(tUInt8 ARG_rd2_rindx)
{
  tUInt32 PORT_rd2;
  DEF__read__h1890 = INST_rfile_31.METH_read();
  DEF__read__h1859 = INST_rfile_30.METH_read();
  DEF__read__h1828 = INST_rfile_29.METH_read();
  DEF__read__h1797 = INST_rfile_28.METH_read();
  DEF__read__h1766 = INST_rfile_27.METH_read();
  DEF__read__h1735 = INST_rfile_26.METH_read();
  DEF__read__h1704 = INST_rfile_25.METH_read();
  DEF__read__h1673 = INST_rfile_24.METH_read();
  DEF__read__h1642 = INST_rfile_23.METH_read();
  DEF__read__h1611 = INST_rfile_22.METH_read();
  DEF__read__h1580 = INST_rfile_21.METH_read();
  DEF__read__h1549 = INST_rfile_20.METH_read();
  DEF__read__h1518 = INST_rfile_19.METH_read();
  DEF__read__h1487 = INST_rfile_18.METH_read();
  DEF__read__h1456 = INST_rfile_17.METH_read();
  DEF__read__h1425 = INST_rfile_16.METH_read();
  DEF__read__h1394 = INST_rfile_15.METH_read();
  DEF__read__h1363 = INST_rfile_14.METH_read();
  DEF__read__h1332 = INST_rfile_13.METH_read();
  DEF__read__h1301 = INST_rfile_12.METH_read();
  DEF__read__h1270 = INST_rfile_11.METH_read();
  DEF__read__h1239 = INST_rfile_10.METH_read();
  DEF__read__h1208 = INST_rfile_9.METH_read();
  DEF__read__h1177 = INST_rfile_8.METH_read();
  DEF__read__h1146 = INST_rfile_7.METH_read();
  DEF__read__h1115 = INST_rfile_6.METH_read();
  DEF__read__h1084 = INST_rfile_5.METH_read();
  DEF__read__h1053 = INST_rfile_4.METH_read();
  DEF__read__h1022 = INST_rfile_3.METH_read();
  DEF__read__h991 = INST_rfile_2.METH_read();
  DEF__read__h960 = INST_rfile_1.METH_read();
  DEF__read__h929 = INST_rfile_0.METH_read();
  switch (ARG_rd2_rindx) {
  case (tUInt8)0u:
    PORT_rd2 = DEF__read__h929;
    break;
  case (tUInt8)1u:
    PORT_rd2 = DEF__read__h960;
    break;
  case (tUInt8)2u:
    PORT_rd2 = DEF__read__h991;
    break;
  case (tUInt8)3u:
    PORT_rd2 = DEF__read__h1022;
    break;
  case (tUInt8)4u:
    PORT_rd2 = DEF__read__h1053;
    break;
  case (tUInt8)5u:
    PORT_rd2 = DEF__read__h1084;
    break;
  case (tUInt8)6u:
    PORT_rd2 = DEF__read__h1115;
    break;
  case (tUInt8)7u:
    PORT_rd2 = DEF__read__h1146;
    break;
  case (tUInt8)8u:
    PORT_rd2 = DEF__read__h1177;
    break;
  case (tUInt8)9u:
    PORT_rd2 = DEF__read__h1208;
    break;
  case (tUInt8)10u:
    PORT_rd2 = DEF__read__h1239;
    break;
  case (tUInt8)11u:
    PORT_rd2 = DEF__read__h1270;
    break;
  case (tUInt8)12u:
    PORT_rd2 = DEF__read__h1301;
    break;
  case (tUInt8)13u:
    PORT_rd2 = DEF__read__h1332;
    break;
  case (tUInt8)14u:
    PORT_rd2 = DEF__read__h1363;
    break;
  case (tUInt8)15u:
    PORT_rd2 = DEF__read__h1394;
    break;
  case (tUInt8)16u:
    PORT_rd2 = DEF__read__h1425;
    break;
  case (tUInt8)17u:
    PORT_rd2 = DEF__read__h1456;
    break;
  case (tUInt8)18u:
    PORT_rd2 = DEF__read__h1487;
    break;
  case (tUInt8)19u:
    PORT_rd2 = DEF__read__h1518;
    break;
  case (tUInt8)20u:
    PORT_rd2 = DEF__read__h1549;
    break;
  case (tUInt8)21u:
    PORT_rd2 = DEF__read__h1580;
    break;
  case (tUInt8)22u:
    PORT_rd2 = DEF__read__h1611;
    break;
  case (tUInt8)23u:
    PORT_rd2 = DEF__read__h1642;
    break;
  case (tUInt8)24u:
    PORT_rd2 = DEF__read__h1673;
    break;
  case (tUInt8)25u:
    PORT_rd2 = DEF__read__h1704;
    break;
  case (tUInt8)26u:
    PORT_rd2 = DEF__read__h1735;
    break;
  case (tUInt8)27u:
    PORT_rd2 = DEF__read__h1766;
    break;
  case (tUInt8)28u:
    PORT_rd2 = DEF__read__h1797;
    break;
  case (tUInt8)29u:
    PORT_rd2 = DEF__read__h1828;
    break;
  case (tUInt8)30u:
    PORT_rd2 = DEF__read__h1859;
    break;
  case (tUInt8)31u:
    PORT_rd2 = DEF__read__h1890;
    break;
  default:
    PORT_rd2 = 2863311530u;
  }
  return PORT_rd2;
}

tUInt8 MOD_mkRFile::METH_RDY_rd2()
{
  tUInt8 PORT_RDY_rd2;
  tUInt8 DEF_CAN_FIRE_rd2;
  DEF_CAN_FIRE_rd2 = (tUInt8)1u;
  PORT_RDY_rd2 = DEF_CAN_FIRE_rd2;
  return PORT_RDY_rd2;
}


/* Reset routines */

void MOD_mkRFile::reset_RST_N(tUInt8 ARG_rst_in)
{
  PORT_RST_N = ARG_rst_in;
  INST_rfile_9.reset_RST(ARG_rst_in);
  INST_rfile_8.reset_RST(ARG_rst_in);
  INST_rfile_7.reset_RST(ARG_rst_in);
  INST_rfile_6.reset_RST(ARG_rst_in);
  INST_rfile_5.reset_RST(ARG_rst_in);
  INST_rfile_4.reset_RST(ARG_rst_in);
  INST_rfile_31.reset_RST(ARG_rst_in);
  INST_rfile_30.reset_RST(ARG_rst_in);
  INST_rfile_3.reset_RST(ARG_rst_in);
  INST_rfile_29.reset_RST(ARG_rst_in);
  INST_rfile_28.reset_RST(ARG_rst_in);
  INST_rfile_27.reset_RST(ARG_rst_in);
  INST_rfile_26.reset_RST(ARG_rst_in);
  INST_rfile_25.reset_RST(ARG_rst_in);
  INST_rfile_24.reset_RST(ARG_rst_in);
  INST_rfile_23.reset_RST(ARG_rst_in);
  INST_rfile_22.reset_RST(ARG_rst_in);
  INST_rfile_21.reset_RST(ARG_rst_in);
  INST_rfile_20.reset_RST(ARG_rst_in);
  INST_rfile_2.reset_RST(ARG_rst_in);
  INST_rfile_19.reset_RST(ARG_rst_in);
  INST_rfile_18.reset_RST(ARG_rst_in);
  INST_rfile_17.reset_RST(ARG_rst_in);
  INST_rfile_16.reset_RST(ARG_rst_in);
  INST_rfile_15.reset_RST(ARG_rst_in);
  INST_rfile_14.reset_RST(ARG_rst_in);
  INST_rfile_13.reset_RST(ARG_rst_in);
  INST_rfile_12.reset_RST(ARG_rst_in);
  INST_rfile_11.reset_RST(ARG_rst_in);
  INST_rfile_10.reset_RST(ARG_rst_in);
  INST_rfile_1.reset_RST(ARG_rst_in);
  INST_rfile_0.reset_RST(ARG_rst_in);
}


/* Static handles to reset routines */


/* Functions for the parent module to register its reset fns */


/* Functions to set the elaborated clock id */

void MOD_mkRFile::set_clk_0(char const *s)
{
  __clk_handle_0 = bk_get_or_define_clock(sim_hdl, s);
}


/* State dumping routine */
void MOD_mkRFile::dump_state(unsigned int indent)
{
  printf("%*s%s:\n", indent, "", inst_name);
  INST_rfile_0.dump_state(indent + 2u);
  INST_rfile_1.dump_state(indent + 2u);
  INST_rfile_10.dump_state(indent + 2u);
  INST_rfile_11.dump_state(indent + 2u);
  INST_rfile_12.dump_state(indent + 2u);
  INST_rfile_13.dump_state(indent + 2u);
  INST_rfile_14.dump_state(indent + 2u);
  INST_rfile_15.dump_state(indent + 2u);
  INST_rfile_16.dump_state(indent + 2u);
  INST_rfile_17.dump_state(indent + 2u);
  INST_rfile_18.dump_state(indent + 2u);
  INST_rfile_19.dump_state(indent + 2u);
  INST_rfile_2.dump_state(indent + 2u);
  INST_rfile_20.dump_state(indent + 2u);
  INST_rfile_21.dump_state(indent + 2u);
  INST_rfile_22.dump_state(indent + 2u);
  INST_rfile_23.dump_state(indent + 2u);
  INST_rfile_24.dump_state(indent + 2u);
  INST_rfile_25.dump_state(indent + 2u);
  INST_rfile_26.dump_state(indent + 2u);
  INST_rfile_27.dump_state(indent + 2u);
  INST_rfile_28.dump_state(indent + 2u);
  INST_rfile_29.dump_state(indent + 2u);
  INST_rfile_3.dump_state(indent + 2u);
  INST_rfile_30.dump_state(indent + 2u);
  INST_rfile_31.dump_state(indent + 2u);
  INST_rfile_4.dump_state(indent + 2u);
  INST_rfile_5.dump_state(indent + 2u);
  INST_rfile_6.dump_state(indent + 2u);
  INST_rfile_7.dump_state(indent + 2u);
  INST_rfile_8.dump_state(indent + 2u);
  INST_rfile_9.dump_state(indent + 2u);
}


/* VCD dumping routines */

unsigned int MOD_mkRFile::dump_VCD_defs(unsigned int levels)
{
  vcd_write_scope_start(sim_hdl, inst_name);
  vcd_num = vcd_reserve_ids(sim_hdl, 65u);
  unsigned int num = vcd_num;
  for (unsigned int clk = 0u; clk < bk_num_clocks(sim_hdl); ++clk)
    vcd_add_clock_def(sim_hdl, this, bk_clock_name(sim_hdl, clk), bk_clock_vcd_num(sim_hdl, clk));
  vcd_write_def(sim_hdl, bk_clock_vcd_num(sim_hdl, __clk_handle_0), "CLK", 1u);
  vcd_write_def(sim_hdl, num++, "RST_N", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1022", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1053", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1084", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1115", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1146", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1177", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1208", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1239", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1270", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1301", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1332", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1363", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1394", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1425", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1456", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1487", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1518", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1549", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1580", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1611", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1642", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1673", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1704", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1735", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1766", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1797", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1828", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1859", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h1890", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h929", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h960", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_read__h991", 32u);
  num = INST_rfile_0.dump_VCD_defs(num);
  num = INST_rfile_1.dump_VCD_defs(num);
  num = INST_rfile_10.dump_VCD_defs(num);
  num = INST_rfile_11.dump_VCD_defs(num);
  num = INST_rfile_12.dump_VCD_defs(num);
  num = INST_rfile_13.dump_VCD_defs(num);
  num = INST_rfile_14.dump_VCD_defs(num);
  num = INST_rfile_15.dump_VCD_defs(num);
  num = INST_rfile_16.dump_VCD_defs(num);
  num = INST_rfile_17.dump_VCD_defs(num);
  num = INST_rfile_18.dump_VCD_defs(num);
  num = INST_rfile_19.dump_VCD_defs(num);
  num = INST_rfile_2.dump_VCD_defs(num);
  num = INST_rfile_20.dump_VCD_defs(num);
  num = INST_rfile_21.dump_VCD_defs(num);
  num = INST_rfile_22.dump_VCD_defs(num);
  num = INST_rfile_23.dump_VCD_defs(num);
  num = INST_rfile_24.dump_VCD_defs(num);
  num = INST_rfile_25.dump_VCD_defs(num);
  num = INST_rfile_26.dump_VCD_defs(num);
  num = INST_rfile_27.dump_VCD_defs(num);
  num = INST_rfile_28.dump_VCD_defs(num);
  num = INST_rfile_29.dump_VCD_defs(num);
  num = INST_rfile_3.dump_VCD_defs(num);
  num = INST_rfile_30.dump_VCD_defs(num);
  num = INST_rfile_31.dump_VCD_defs(num);
  num = INST_rfile_4.dump_VCD_defs(num);
  num = INST_rfile_5.dump_VCD_defs(num);
  num = INST_rfile_6.dump_VCD_defs(num);
  num = INST_rfile_7.dump_VCD_defs(num);
  num = INST_rfile_8.dump_VCD_defs(num);
  num = INST_rfile_9.dump_VCD_defs(num);
  vcd_write_scope_end(sim_hdl);
  return num;
}

void MOD_mkRFile::dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkRFile &backing)
{
  vcd_defs(dt, backing);
  vcd_prims(dt, backing);
}

void MOD_mkRFile::vcd_defs(tVCDDumpType dt, MOD_mkRFile &backing)
{
  unsigned int num = vcd_num;
  if (dt == VCD_DUMP_XS)
  {
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
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
      if ((backing.DEF__read__h1022) != DEF__read__h1022)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1022, 32u);
	backing.DEF__read__h1022 = DEF__read__h1022;
      }
      ++num;
      if ((backing.DEF__read__h1053) != DEF__read__h1053)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1053, 32u);
	backing.DEF__read__h1053 = DEF__read__h1053;
      }
      ++num;
      if ((backing.DEF__read__h1084) != DEF__read__h1084)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1084, 32u);
	backing.DEF__read__h1084 = DEF__read__h1084;
      }
      ++num;
      if ((backing.DEF__read__h1115) != DEF__read__h1115)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1115, 32u);
	backing.DEF__read__h1115 = DEF__read__h1115;
      }
      ++num;
      if ((backing.DEF__read__h1146) != DEF__read__h1146)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1146, 32u);
	backing.DEF__read__h1146 = DEF__read__h1146;
      }
      ++num;
      if ((backing.DEF__read__h1177) != DEF__read__h1177)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1177, 32u);
	backing.DEF__read__h1177 = DEF__read__h1177;
      }
      ++num;
      if ((backing.DEF__read__h1208) != DEF__read__h1208)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1208, 32u);
	backing.DEF__read__h1208 = DEF__read__h1208;
      }
      ++num;
      if ((backing.DEF__read__h1239) != DEF__read__h1239)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1239, 32u);
	backing.DEF__read__h1239 = DEF__read__h1239;
      }
      ++num;
      if ((backing.DEF__read__h1270) != DEF__read__h1270)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1270, 32u);
	backing.DEF__read__h1270 = DEF__read__h1270;
      }
      ++num;
      if ((backing.DEF__read__h1301) != DEF__read__h1301)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1301, 32u);
	backing.DEF__read__h1301 = DEF__read__h1301;
      }
      ++num;
      if ((backing.DEF__read__h1332) != DEF__read__h1332)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1332, 32u);
	backing.DEF__read__h1332 = DEF__read__h1332;
      }
      ++num;
      if ((backing.DEF__read__h1363) != DEF__read__h1363)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1363, 32u);
	backing.DEF__read__h1363 = DEF__read__h1363;
      }
      ++num;
      if ((backing.DEF__read__h1394) != DEF__read__h1394)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1394, 32u);
	backing.DEF__read__h1394 = DEF__read__h1394;
      }
      ++num;
      if ((backing.DEF__read__h1425) != DEF__read__h1425)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1425, 32u);
	backing.DEF__read__h1425 = DEF__read__h1425;
      }
      ++num;
      if ((backing.DEF__read__h1456) != DEF__read__h1456)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1456, 32u);
	backing.DEF__read__h1456 = DEF__read__h1456;
      }
      ++num;
      if ((backing.DEF__read__h1487) != DEF__read__h1487)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1487, 32u);
	backing.DEF__read__h1487 = DEF__read__h1487;
      }
      ++num;
      if ((backing.DEF__read__h1518) != DEF__read__h1518)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1518, 32u);
	backing.DEF__read__h1518 = DEF__read__h1518;
      }
      ++num;
      if ((backing.DEF__read__h1549) != DEF__read__h1549)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1549, 32u);
	backing.DEF__read__h1549 = DEF__read__h1549;
      }
      ++num;
      if ((backing.DEF__read__h1580) != DEF__read__h1580)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1580, 32u);
	backing.DEF__read__h1580 = DEF__read__h1580;
      }
      ++num;
      if ((backing.DEF__read__h1611) != DEF__read__h1611)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1611, 32u);
	backing.DEF__read__h1611 = DEF__read__h1611;
      }
      ++num;
      if ((backing.DEF__read__h1642) != DEF__read__h1642)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1642, 32u);
	backing.DEF__read__h1642 = DEF__read__h1642;
      }
      ++num;
      if ((backing.DEF__read__h1673) != DEF__read__h1673)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1673, 32u);
	backing.DEF__read__h1673 = DEF__read__h1673;
      }
      ++num;
      if ((backing.DEF__read__h1704) != DEF__read__h1704)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1704, 32u);
	backing.DEF__read__h1704 = DEF__read__h1704;
      }
      ++num;
      if ((backing.DEF__read__h1735) != DEF__read__h1735)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1735, 32u);
	backing.DEF__read__h1735 = DEF__read__h1735;
      }
      ++num;
      if ((backing.DEF__read__h1766) != DEF__read__h1766)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1766, 32u);
	backing.DEF__read__h1766 = DEF__read__h1766;
      }
      ++num;
      if ((backing.DEF__read__h1797) != DEF__read__h1797)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1797, 32u);
	backing.DEF__read__h1797 = DEF__read__h1797;
      }
      ++num;
      if ((backing.DEF__read__h1828) != DEF__read__h1828)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1828, 32u);
	backing.DEF__read__h1828 = DEF__read__h1828;
      }
      ++num;
      if ((backing.DEF__read__h1859) != DEF__read__h1859)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1859, 32u);
	backing.DEF__read__h1859 = DEF__read__h1859;
      }
      ++num;
      if ((backing.DEF__read__h1890) != DEF__read__h1890)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h1890, 32u);
	backing.DEF__read__h1890 = DEF__read__h1890;
      }
      ++num;
      if ((backing.DEF__read__h929) != DEF__read__h929)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h929, 32u);
	backing.DEF__read__h929 = DEF__read__h929;
      }
      ++num;
      if ((backing.DEF__read__h960) != DEF__read__h960)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h960, 32u);
	backing.DEF__read__h960 = DEF__read__h960;
      }
      ++num;
      if ((backing.DEF__read__h991) != DEF__read__h991)
      {
	vcd_write_val(sim_hdl, num, DEF__read__h991, 32u);
	backing.DEF__read__h991 = DEF__read__h991;
      }
      ++num;
    }
    else
    {
      vcd_write_val(sim_hdl, num++, PORT_RST_N, 1u);
      backing.PORT_RST_N = PORT_RST_N;
      vcd_write_val(sim_hdl, num++, DEF__read__h1022, 32u);
      backing.DEF__read__h1022 = DEF__read__h1022;
      vcd_write_val(sim_hdl, num++, DEF__read__h1053, 32u);
      backing.DEF__read__h1053 = DEF__read__h1053;
      vcd_write_val(sim_hdl, num++, DEF__read__h1084, 32u);
      backing.DEF__read__h1084 = DEF__read__h1084;
      vcd_write_val(sim_hdl, num++, DEF__read__h1115, 32u);
      backing.DEF__read__h1115 = DEF__read__h1115;
      vcd_write_val(sim_hdl, num++, DEF__read__h1146, 32u);
      backing.DEF__read__h1146 = DEF__read__h1146;
      vcd_write_val(sim_hdl, num++, DEF__read__h1177, 32u);
      backing.DEF__read__h1177 = DEF__read__h1177;
      vcd_write_val(sim_hdl, num++, DEF__read__h1208, 32u);
      backing.DEF__read__h1208 = DEF__read__h1208;
      vcd_write_val(sim_hdl, num++, DEF__read__h1239, 32u);
      backing.DEF__read__h1239 = DEF__read__h1239;
      vcd_write_val(sim_hdl, num++, DEF__read__h1270, 32u);
      backing.DEF__read__h1270 = DEF__read__h1270;
      vcd_write_val(sim_hdl, num++, DEF__read__h1301, 32u);
      backing.DEF__read__h1301 = DEF__read__h1301;
      vcd_write_val(sim_hdl, num++, DEF__read__h1332, 32u);
      backing.DEF__read__h1332 = DEF__read__h1332;
      vcd_write_val(sim_hdl, num++, DEF__read__h1363, 32u);
      backing.DEF__read__h1363 = DEF__read__h1363;
      vcd_write_val(sim_hdl, num++, DEF__read__h1394, 32u);
      backing.DEF__read__h1394 = DEF__read__h1394;
      vcd_write_val(sim_hdl, num++, DEF__read__h1425, 32u);
      backing.DEF__read__h1425 = DEF__read__h1425;
      vcd_write_val(sim_hdl, num++, DEF__read__h1456, 32u);
      backing.DEF__read__h1456 = DEF__read__h1456;
      vcd_write_val(sim_hdl, num++, DEF__read__h1487, 32u);
      backing.DEF__read__h1487 = DEF__read__h1487;
      vcd_write_val(sim_hdl, num++, DEF__read__h1518, 32u);
      backing.DEF__read__h1518 = DEF__read__h1518;
      vcd_write_val(sim_hdl, num++, DEF__read__h1549, 32u);
      backing.DEF__read__h1549 = DEF__read__h1549;
      vcd_write_val(sim_hdl, num++, DEF__read__h1580, 32u);
      backing.DEF__read__h1580 = DEF__read__h1580;
      vcd_write_val(sim_hdl, num++, DEF__read__h1611, 32u);
      backing.DEF__read__h1611 = DEF__read__h1611;
      vcd_write_val(sim_hdl, num++, DEF__read__h1642, 32u);
      backing.DEF__read__h1642 = DEF__read__h1642;
      vcd_write_val(sim_hdl, num++, DEF__read__h1673, 32u);
      backing.DEF__read__h1673 = DEF__read__h1673;
      vcd_write_val(sim_hdl, num++, DEF__read__h1704, 32u);
      backing.DEF__read__h1704 = DEF__read__h1704;
      vcd_write_val(sim_hdl, num++, DEF__read__h1735, 32u);
      backing.DEF__read__h1735 = DEF__read__h1735;
      vcd_write_val(sim_hdl, num++, DEF__read__h1766, 32u);
      backing.DEF__read__h1766 = DEF__read__h1766;
      vcd_write_val(sim_hdl, num++, DEF__read__h1797, 32u);
      backing.DEF__read__h1797 = DEF__read__h1797;
      vcd_write_val(sim_hdl, num++, DEF__read__h1828, 32u);
      backing.DEF__read__h1828 = DEF__read__h1828;
      vcd_write_val(sim_hdl, num++, DEF__read__h1859, 32u);
      backing.DEF__read__h1859 = DEF__read__h1859;
      vcd_write_val(sim_hdl, num++, DEF__read__h1890, 32u);
      backing.DEF__read__h1890 = DEF__read__h1890;
      vcd_write_val(sim_hdl, num++, DEF__read__h929, 32u);
      backing.DEF__read__h929 = DEF__read__h929;
      vcd_write_val(sim_hdl, num++, DEF__read__h960, 32u);
      backing.DEF__read__h960 = DEF__read__h960;
      vcd_write_val(sim_hdl, num++, DEF__read__h991, 32u);
      backing.DEF__read__h991 = DEF__read__h991;
    }
}

void MOD_mkRFile::vcd_prims(tVCDDumpType dt, MOD_mkRFile &backing)
{
  INST_rfile_0.dump_VCD(dt, backing.INST_rfile_0);
  INST_rfile_1.dump_VCD(dt, backing.INST_rfile_1);
  INST_rfile_10.dump_VCD(dt, backing.INST_rfile_10);
  INST_rfile_11.dump_VCD(dt, backing.INST_rfile_11);
  INST_rfile_12.dump_VCD(dt, backing.INST_rfile_12);
  INST_rfile_13.dump_VCD(dt, backing.INST_rfile_13);
  INST_rfile_14.dump_VCD(dt, backing.INST_rfile_14);
  INST_rfile_15.dump_VCD(dt, backing.INST_rfile_15);
  INST_rfile_16.dump_VCD(dt, backing.INST_rfile_16);
  INST_rfile_17.dump_VCD(dt, backing.INST_rfile_17);
  INST_rfile_18.dump_VCD(dt, backing.INST_rfile_18);
  INST_rfile_19.dump_VCD(dt, backing.INST_rfile_19);
  INST_rfile_2.dump_VCD(dt, backing.INST_rfile_2);
  INST_rfile_20.dump_VCD(dt, backing.INST_rfile_20);
  INST_rfile_21.dump_VCD(dt, backing.INST_rfile_21);
  INST_rfile_22.dump_VCD(dt, backing.INST_rfile_22);
  INST_rfile_23.dump_VCD(dt, backing.INST_rfile_23);
  INST_rfile_24.dump_VCD(dt, backing.INST_rfile_24);
  INST_rfile_25.dump_VCD(dt, backing.INST_rfile_25);
  INST_rfile_26.dump_VCD(dt, backing.INST_rfile_26);
  INST_rfile_27.dump_VCD(dt, backing.INST_rfile_27);
  INST_rfile_28.dump_VCD(dt, backing.INST_rfile_28);
  INST_rfile_29.dump_VCD(dt, backing.INST_rfile_29);
  INST_rfile_3.dump_VCD(dt, backing.INST_rfile_3);
  INST_rfile_30.dump_VCD(dt, backing.INST_rfile_30);
  INST_rfile_31.dump_VCD(dt, backing.INST_rfile_31);
  INST_rfile_4.dump_VCD(dt, backing.INST_rfile_4);
  INST_rfile_5.dump_VCD(dt, backing.INST_rfile_5);
  INST_rfile_6.dump_VCD(dt, backing.INST_rfile_6);
  INST_rfile_7.dump_VCD(dt, backing.INST_rfile_7);
  INST_rfile_8.dump_VCD(dt, backing.INST_rfile_8);
  INST_rfile_9.dump_VCD(dt, backing.INST_rfile_9);
}
