module CSR(
  input         clock,
  input         reset,
  input         io__stall, // @[src/main/scala/mini/CSR.scala 124:14]
  input  [2:0]  io__cmd, // @[src/main/scala/mini/CSR.scala 124:14]
  input  [31:0] io__in, // @[src/main/scala/mini/CSR.scala 124:14]
  output [31:0] io__out, // @[src/main/scala/mini/CSR.scala 124:14]
  input  [31:0] io__pc, // @[src/main/scala/mini/CSR.scala 124:14]
  input  [31:0] io__addr, // @[src/main/scala/mini/CSR.scala 124:14]
  input  [31:0] io__inst, // @[src/main/scala/mini/CSR.scala 124:14]
  input         io__illegal, // @[src/main/scala/mini/CSR.scala 124:14]
  input  [1:0]  io__st_type, // @[src/main/scala/mini/CSR.scala 124:14]
  input  [2:0]  io__ld_type, // @[src/main/scala/mini/CSR.scala 124:14]
  input         io__pc_check, // @[src/main/scala/mini/CSR.scala 124:14]
  output        io__expt, // @[src/main/scala/mini/CSR.scala 124:14]
  output [31:0] io__evec, // @[src/main/scala/mini/CSR.scala 124:14]
  output [31:0] io__epc, // @[src/main/scala/mini/CSR.scala 124:14]
  output        resultEventWire_0_valid,
  output [31:0] resultEventWire_0_intrNO,
  output [31:0] resultEventWire_0_cause,
  output [31:0] resultEventWire_0_exceptionPC,
  output [31:0] resultEventWire_0_exceptionInst,
  output        io_expt
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
  reg [31:0] _RAND_4;
  reg [31:0] _RAND_5;
  reg [31:0] _RAND_6;
  reg [31:0] _RAND_7;
  reg [31:0] _RAND_8;
  reg [31:0] _RAND_9;
  reg [31:0] _RAND_10;
  reg [31:0] _RAND_11;
  reg [31:0] _RAND_12;
  reg [31:0] _RAND_13;
  reg [31:0] _RAND_14;
  reg [31:0] _RAND_15;
  reg [31:0] _RAND_16;
  reg [31:0] _RAND_17;
  reg [31:0] _RAND_18;
  reg [31:0] _RAND_19;
  reg [31:0] _RAND_20;
`endif // RANDOMIZE_REG_INIT
  wire [11:0] csr_addr = io__inst[31:20]; // @[src/main/scala/mini/CSR.scala 126:25]
  wire [4:0] rs1_addr = io__inst[19:15]; // @[src/main/scala/mini/CSR.scala 127:25]
  reg [31:0] time_; // @[src/main/scala/mini/CSR.scala 130:21]
  reg [31:0] timeh; // @[src/main/scala/mini/CSR.scala 131:22]
  reg [31:0] cycle; // @[src/main/scala/mini/CSR.scala 132:22]
  reg [31:0] cycleh; // @[src/main/scala/mini/CSR.scala 133:23]
  reg [31:0] instret; // @[src/main/scala/mini/CSR.scala 134:24]
  reg [31:0] instreth; // @[src/main/scala/mini/CSR.scala 135:25]
  reg [1:0] PRV; // @[src/main/scala/mini/CSR.scala 147:20]
  reg [1:0] PRV1; // @[src/main/scala/mini/CSR.scala 148:21]
  reg  IE; // @[src/main/scala/mini/CSR.scala 151:19]
  reg  IE1; // @[src/main/scala/mini/CSR.scala 152:20]
  wire [31:0] mstatus = {22'h0,3'h0,1'h0,PRV1,IE1,PRV,IE}; // @[src/main/scala/mini/CSR.scala 163:20]
  reg  MTIP; // @[src/main/scala/mini/CSR.scala 168:21]
  reg  MTIE; // @[src/main/scala/mini/CSR.scala 171:21]
  reg  MSIP; // @[src/main/scala/mini/CSR.scala 174:21]
  reg  MSIE; // @[src/main/scala/mini/CSR.scala 177:21]
  wire [31:0] mip = {24'h0,MTIP,1'h0,2'h0,MSIP,1'h0,2'h0}; // @[src/main/scala/mini/CSR.scala 180:16]
  wire [31:0] mie = {24'h0,MTIE,1'h0,2'h0,MSIE,1'h0,2'h0}; // @[src/main/scala/mini/CSR.scala 181:16]
  reg [31:0] mtimecmp; // @[src/main/scala/mini/CSR.scala 183:21]
  reg [31:0] mscratch; // @[src/main/scala/mini/CSR.scala 185:21]
  reg [31:0] mepc; // @[src/main/scala/mini/CSR.scala 187:17]
  reg [31:0] mcause; // @[src/main/scala/mini/CSR.scala 188:19]
  reg [31:0] mbadaddr; // @[src/main/scala/mini/CSR.scala 189:21]
  reg [31:0] mtohost; // @[src/main/scala/mini/CSR.scala 191:24]
  reg [31:0] mfromhost; // @[src/main/scala/mini/CSR.scala 192:22]
  wire  _io_out_T_1 = 12'hc00 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_3 = 12'hc01 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_5 = 12'hc02 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_7 = 12'hc80 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_9 = 12'hc81 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_11 = 12'hc82 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_13 = 12'h900 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_15 = 12'h901 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_17 = 12'h902 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_19 = 12'h980 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_21 = 12'h981 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_23 = 12'h982 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_25 = 12'hf00 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_27 = 12'hf01 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_29 = 12'hf10 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_31 = 12'h301 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_33 = 12'h302 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_35 = 12'h304 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_37 = 12'h321 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_39 = 12'h701 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_41 = 12'h741 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_43 = 12'h340 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_45 = 12'h341 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_47 = 12'h342 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_49 = 12'h343 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_51 = 12'h344 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_53 = 12'h780 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_55 = 12'h781 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _io_out_T_57 = 12'h300 == csr_addr; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire [31:0] _io_out_T_58 = _io_out_T_57 ? mstatus : 32'h0; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_59 = _io_out_T_55 ? mfromhost : _io_out_T_58; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_60 = _io_out_T_53 ? mtohost : _io_out_T_59; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_61 = _io_out_T_51 ? mip : _io_out_T_60; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_62 = _io_out_T_49 ? mbadaddr : _io_out_T_61; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_63 = _io_out_T_47 ? mcause : _io_out_T_62; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_64 = _io_out_T_45 ? mepc : _io_out_T_63; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_65 = _io_out_T_43 ? mscratch : _io_out_T_64; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_66 = _io_out_T_41 ? timeh : _io_out_T_65; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_67 = _io_out_T_39 ? time_ : _io_out_T_66; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_68 = _io_out_T_37 ? mtimecmp : _io_out_T_67; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_69 = _io_out_T_35 ? mie : _io_out_T_68; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_70 = _io_out_T_33 ? 32'h0 : _io_out_T_69; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_71 = _io_out_T_31 ? 32'h100 : _io_out_T_70; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_72 = _io_out_T_29 ? 32'h0 : _io_out_T_71; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_73 = _io_out_T_27 ? 32'h0 : _io_out_T_72; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_74 = _io_out_T_25 ? 32'h100100 : _io_out_T_73; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_75 = _io_out_T_23 ? instreth : _io_out_T_74; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_76 = _io_out_T_21 ? timeh : _io_out_T_75; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_77 = _io_out_T_19 ? cycleh : _io_out_T_76; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_78 = _io_out_T_17 ? instret : _io_out_T_77; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_79 = _io_out_T_15 ? time_ : _io_out_T_78; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_80 = _io_out_T_13 ? cycle : _io_out_T_79; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_81 = _io_out_T_11 ? instreth : _io_out_T_80; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_82 = _io_out_T_9 ? timeh : _io_out_T_81; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_83 = _io_out_T_7 ? cycleh : _io_out_T_82; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_84 = _io_out_T_5 ? instret : _io_out_T_83; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [31:0] _io_out_T_85 = _io_out_T_3 ? time_ : _io_out_T_84; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  privValid = csr_addr[9:8] <= PRV; // @[src/main/scala/mini/CSR.scala 232:34]
  wire  privInst = io__cmd == 3'h4; // @[src/main/scala/mini/CSR.scala 233:25]
  wire  _isEcall_T_2 = privInst & ~csr_addr[0]; // @[src/main/scala/mini/CSR.scala 234:26]
  wire  _isEcall_T_4 = ~csr_addr[8]; // @[src/main/scala/mini/CSR.scala 234:45]
  wire  isEcall = privInst & ~csr_addr[0] & ~csr_addr[8]; // @[src/main/scala/mini/CSR.scala 234:42]
  wire  isEbreak = privInst & csr_addr[0] & _isEcall_T_4; // @[src/main/scala/mini/CSR.scala 235:42]
  wire  isEret = _isEcall_T_2 & csr_addr[8]; // @[src/main/scala/mini/CSR.scala 236:41]
  wire  csrValid = _io_out_T_1 | _io_out_T_3 | _io_out_T_5 | _io_out_T_7 | _io_out_T_9 | _io_out_T_11 | _io_out_T_13 |
    _io_out_T_15 | _io_out_T_17 | _io_out_T_19 | _io_out_T_21 | _io_out_T_23 | _io_out_T_25 | _io_out_T_27 |
    _io_out_T_29 | _io_out_T_31 | _io_out_T_33 | _io_out_T_35 | _io_out_T_37 | _io_out_T_39 | _io_out_T_41 |
    _io_out_T_43 | _io_out_T_45 | _io_out_T_47 | _io_out_T_49 | _io_out_T_51 | _io_out_T_53 | _io_out_T_55 |
    _io_out_T_57; // @[src/main/scala/mini/CSR.scala 237:58]
  wire  csrRO = &csr_addr[11:10] | csr_addr == 12'h301 | csr_addr == 12'h302; // @[src/main/scala/mini/CSR.scala 238:63]
  wire  wen = io__cmd == 3'h1 | io__cmd[1] & |rs1_addr; // @[src/main/scala/mini/CSR.scala 239:30]
  wire [31:0] _wdata_T = io__out | io__in; // @[src/main/scala/mini/CSR.scala 243:24]
  wire [31:0] _wdata_T_1 = ~io__in; // @[src/main/scala/mini/CSR.scala 244:26]
  wire [31:0] _wdata_T_2 = io__out & _wdata_T_1; // @[src/main/scala/mini/CSR.scala 244:24]
  wire [31:0] _wdata_T_4 = 3'h1 == io__cmd ? io__in : 32'h0; // @[src/main/scala/mini/CSR.scala 240:37]
  wire [31:0] _wdata_T_6 = 3'h2 == io__cmd ? _wdata_T : _wdata_T_4; // @[src/main/scala/mini/CSR.scala 240:37]
  wire [31:0] wdata = 3'h3 == io__cmd ? _wdata_T_2 : _wdata_T_6; // @[src/main/scala/mini/CSR.scala 240:37]
  wire  iaddrInvalid = io__pc_check & io__addr[1]; // @[src/main/scala/mini/CSR.scala 247:34]
  wire  _laddrInvalid_T_1 = |io__addr[1:0]; // @[src/main/scala/mini/CSR.scala 249:40]
  wire  _laddrInvalid_T_7 = 3'h2 == io__ld_type ? io__addr[0] : 3'h1 == io__ld_type & _laddrInvalid_T_1; // @[src/main/scala/mini/CSR.scala 248:52]
  wire  laddrInvalid = 3'h4 == io__ld_type ? io__addr[0] : _laddrInvalid_T_7; // @[src/main/scala/mini/CSR.scala 248:52]
  wire  saddrInvalid = 2'h2 == io__st_type ? io__addr[0] : 2'h1 == io__st_type & _laddrInvalid_T_1; // @[src/main/scala/mini/CSR.scala 252:35]
  wire  _io_expt_T_6 = ~privValid; // @[src/main/scala/mini/CSR.scala 254:39]
  wire  _io_expt_T_8 = |io__cmd[1:0] & (~csrValid | ~privValid); // @[src/main/scala/mini/CSR.scala 254:22]
  wire  _io_expt_T_9 = io__illegal | iaddrInvalid | laddrInvalid | saddrInvalid | _io_expt_T_8; // @[src/main/scala/mini/CSR.scala 253:73]
  wire  _io_expt_T_13 = privInst & _io_expt_T_6; // @[src/main/scala/mini/CSR.scala 255:15]
  wire  _io_expt_T_14 = _io_expt_T_9 | wen & csrRO | _io_expt_T_13; // @[src/main/scala/mini/CSR.scala 254:67]
  wire [7:0] _io_evec_T = {PRV, 6'h0}; // @[src/main/scala/mini/CSR.scala 256:27]
  wire [31:0] _GEN_260 = {{24'd0}, _io_evec_T}; // @[src/main/scala/mini/CSR.scala 256:20]
  wire [31:0] _time_T_1 = time_ + 32'h1; // @[src/main/scala/mini/CSR.scala 260:16]
  wire [31:0] _timeh_T_1 = timeh + 32'h1; // @[src/main/scala/mini/CSR.scala 261:36]
  wire [31:0] _GEN_1 = &time_ ? _timeh_T_1 : timeh; // @[src/main/scala/mini/CSR.scala 261:19 131:22 261:27]
  wire [31:0] _cycle_T_1 = cycle + 32'h1; // @[src/main/scala/mini/CSR.scala 262:18]
  wire [31:0] _cycleh_T_1 = cycleh + 32'h1; // @[src/main/scala/mini/CSR.scala 263:39]
  wire [31:0] _GEN_2 = &cycle ? _cycleh_T_1 : cycleh; // @[src/main/scala/mini/CSR.scala 263:20 133:23 263:29]
  wire  _isInstRet_T_5 = ~io__stall; // @[src/main/scala/mini/CSR.scala 264:88]
  wire  isInstRet = io__inst != 32'h13 & (~io__expt | isEcall | isEbreak) & ~io__stall; // @[src/main/scala/mini/CSR.scala 264:85]
  wire [31:0] _instret_T_1 = instret + 32'h1; // @[src/main/scala/mini/CSR.scala 265:40]
  wire [31:0] _GEN_3 = isInstRet ? _instret_T_1 : instret; // @[src/main/scala/mini/CSR.scala 265:19 134:24 265:29]
  wire [31:0] _instreth_T_1 = instreth + 32'h1; // @[src/main/scala/mini/CSR.scala 266:58]
  wire [31:0] _GEN_4 = isInstRet & &instret ? _instreth_T_1 : instreth; // @[src/main/scala/mini/CSR.scala 135:25 266:{35,46}]
  wire [3:0] _GEN_261 = {{2'd0}, PRV}; // @[src/main/scala/mini/CSR.scala 276:34]
  wire [3:0] _exception_case_T_1 = 4'h8 + _GEN_261; // @[src/main/scala/mini/CSR.scala 276:34]
  wire [1:0] _exception_case_T_2 = isEbreak ? 2'h3 : 2'h2; // @[src/main/scala/mini/CSR.scala 276:44]
  wire [3:0] _exception_case_T_3 = isEcall ? _exception_case_T_1 : {{2'd0}, _exception_case_T_2}; // @[src/main/scala/mini/CSR.scala 276:12]
  wire [3:0] _exception_case_T_4 = saddrInvalid ? 4'h6 : _exception_case_T_3; // @[src/main/scala/mini/CSR.scala 273:10]
  wire [3:0] _exception_case_T_5 = laddrInvalid ? 4'h4 : _exception_case_T_4; // @[src/main/scala/mini/CSR.scala 270:8]
  wire [3:0] exception_case = iaddrInvalid ? 4'h0 : _exception_case_T_5; // @[src/main/scala/mini/CSR.scala 267:27]
  wire [31:0] _mepc_T_1 = {io__pc[31:2], 2'h0}; // @[src/main/scala/mini/CSR.scala 284:26]
  wire [31:0] _mepc_T_2 = {{2'd0}, wdata[31:2]}; // @[src/main/scala/mini/CSR.scala 315:58]
  wire [33:0] _GEN_263 = {_mepc_T_2, 2'h0}; // @[src/main/scala/mini/CSR.scala 315:65]
  wire [34:0] _mepc_T_3 = {{1'd0}, _GEN_263}; // @[src/main/scala/mini/CSR.scala 315:65]
  wire [31:0] _mcause_T = wdata & 32'h8000000f; // @[src/main/scala/mini/CSR.scala 316:62]
  wire [31:0] _GEN_6 = csr_addr == 12'h982 ? wdata : _GEN_4; // @[src/main/scala/mini/CSR.scala 325:{47,58}]
  wire [31:0] _GEN_7 = csr_addr == 12'h981 ? wdata : _GEN_1; // @[src/main/scala/mini/CSR.scala 324:{44,52}]
  wire [31:0] _GEN_8 = csr_addr == 12'h981 ? _GEN_4 : _GEN_6; // @[src/main/scala/mini/CSR.scala 324:44]
  wire [31:0] _GEN_9 = csr_addr == 12'h980 ? wdata : _GEN_2; // @[src/main/scala/mini/CSR.scala 323:{45,54}]
  wire [31:0] _GEN_10 = csr_addr == 12'h980 ? _GEN_1 : _GEN_7; // @[src/main/scala/mini/CSR.scala 323:45]
  wire [31:0] _GEN_11 = csr_addr == 12'h980 ? _GEN_4 : _GEN_8; // @[src/main/scala/mini/CSR.scala 323:45]
  wire [31:0] _GEN_12 = csr_addr == 12'h902 ? wdata : _GEN_3; // @[src/main/scala/mini/CSR.scala 322:{46,56}]
  wire [31:0] _GEN_13 = csr_addr == 12'h902 ? _GEN_2 : _GEN_9; // @[src/main/scala/mini/CSR.scala 322:46]
  wire [31:0] _GEN_14 = csr_addr == 12'h902 ? _GEN_1 : _GEN_10; // @[src/main/scala/mini/CSR.scala 322:46]
  wire [31:0] _GEN_15 = csr_addr == 12'h902 ? _GEN_4 : _GEN_11; // @[src/main/scala/mini/CSR.scala 322:46]
  wire [31:0] _GEN_16 = csr_addr == 12'h901 ? wdata : _time_T_1; // @[src/main/scala/mini/CSR.scala 321:{43,50} 260:8]
  wire [31:0] _GEN_17 = csr_addr == 12'h901 ? _GEN_3 : _GEN_12; // @[src/main/scala/mini/CSR.scala 321:43]
  wire [31:0] _GEN_18 = csr_addr == 12'h901 ? _GEN_2 : _GEN_13; // @[src/main/scala/mini/CSR.scala 321:43]
  wire [31:0] _GEN_19 = csr_addr == 12'h901 ? _GEN_1 : _GEN_14; // @[src/main/scala/mini/CSR.scala 321:43]
  wire [31:0] _GEN_20 = csr_addr == 12'h901 ? _GEN_4 : _GEN_15; // @[src/main/scala/mini/CSR.scala 321:43]
  wire [31:0] _GEN_21 = csr_addr == 12'h900 ? wdata : _cycle_T_1; // @[src/main/scala/mini/CSR.scala 320:{44,52} 262:9]
  wire [31:0] _GEN_22 = csr_addr == 12'h900 ? _time_T_1 : _GEN_16; // @[src/main/scala/mini/CSR.scala 320:44 260:8]
  wire [31:0] _GEN_23 = csr_addr == 12'h900 ? _GEN_3 : _GEN_17; // @[src/main/scala/mini/CSR.scala 320:44]
  wire [31:0] _GEN_24 = csr_addr == 12'h900 ? _GEN_2 : _GEN_18; // @[src/main/scala/mini/CSR.scala 320:44]
  wire [31:0] _GEN_25 = csr_addr == 12'h900 ? _GEN_1 : _GEN_19; // @[src/main/scala/mini/CSR.scala 320:44]
  wire [31:0] _GEN_26 = csr_addr == 12'h900 ? _GEN_4 : _GEN_20; // @[src/main/scala/mini/CSR.scala 320:44]
  wire [31:0] _GEN_27 = csr_addr == 12'h781 ? wdata : mfromhost; // @[src/main/scala/mini/CSR.scala 319:{47,59}]
  wire [31:0] _GEN_28 = csr_addr == 12'h781 ? _cycle_T_1 : _GEN_21; // @[src/main/scala/mini/CSR.scala 319:47 262:9]
  wire [31:0] _GEN_29 = csr_addr == 12'h781 ? _time_T_1 : _GEN_22; // @[src/main/scala/mini/CSR.scala 319:47 260:8]
  wire [31:0] _GEN_30 = csr_addr == 12'h781 ? _GEN_3 : _GEN_23; // @[src/main/scala/mini/CSR.scala 319:47]
  wire [31:0] _GEN_31 = csr_addr == 12'h781 ? _GEN_2 : _GEN_24; // @[src/main/scala/mini/CSR.scala 319:47]
  wire [31:0] _GEN_32 = csr_addr == 12'h781 ? _GEN_1 : _GEN_25; // @[src/main/scala/mini/CSR.scala 319:47]
  wire [31:0] _GEN_33 = csr_addr == 12'h781 ? _GEN_4 : _GEN_26; // @[src/main/scala/mini/CSR.scala 319:47]
  wire [31:0] _GEN_34 = csr_addr == 12'h780 ? wdata : mtohost; // @[src/main/scala/mini/CSR.scala 191:24 318:{45,55}]
  wire [31:0] _GEN_35 = csr_addr == 12'h780 ? mfromhost : _GEN_27; // @[src/main/scala/mini/CSR.scala 318:45]
  wire [31:0] _GEN_36 = csr_addr == 12'h780 ? _cycle_T_1 : _GEN_28; // @[src/main/scala/mini/CSR.scala 318:45 262:9]
  wire [31:0] _GEN_37 = csr_addr == 12'h780 ? _time_T_1 : _GEN_29; // @[src/main/scala/mini/CSR.scala 318:45 260:8]
  wire [31:0] _GEN_38 = csr_addr == 12'h780 ? _GEN_3 : _GEN_30; // @[src/main/scala/mini/CSR.scala 318:45]
  wire [31:0] _GEN_39 = csr_addr == 12'h780 ? _GEN_2 : _GEN_31; // @[src/main/scala/mini/CSR.scala 318:45]
  wire [31:0] _GEN_40 = csr_addr == 12'h780 ? _GEN_1 : _GEN_32; // @[src/main/scala/mini/CSR.scala 318:45]
  wire [31:0] _GEN_41 = csr_addr == 12'h780 ? _GEN_4 : _GEN_33; // @[src/main/scala/mini/CSR.scala 318:45]
  wire [31:0] _GEN_42 = csr_addr == 12'h343 ? wdata : mbadaddr; // @[src/main/scala/mini/CSR.scala 189:21 317:{46,57}]
  wire [31:0] _GEN_43 = csr_addr == 12'h343 ? mtohost : _GEN_34; // @[src/main/scala/mini/CSR.scala 191:24 317:46]
  wire [31:0] _GEN_44 = csr_addr == 12'h343 ? mfromhost : _GEN_35; // @[src/main/scala/mini/CSR.scala 317:46]
  wire [31:0] _GEN_45 = csr_addr == 12'h343 ? _cycle_T_1 : _GEN_36; // @[src/main/scala/mini/CSR.scala 317:46 262:9]
  wire [31:0] _GEN_46 = csr_addr == 12'h343 ? _time_T_1 : _GEN_37; // @[src/main/scala/mini/CSR.scala 317:46 260:8]
  wire [31:0] _GEN_47 = csr_addr == 12'h343 ? _GEN_3 : _GEN_38; // @[src/main/scala/mini/CSR.scala 317:46]
  wire [31:0] _GEN_48 = csr_addr == 12'h343 ? _GEN_2 : _GEN_39; // @[src/main/scala/mini/CSR.scala 317:46]
  wire [31:0] _GEN_49 = csr_addr == 12'h343 ? _GEN_1 : _GEN_40; // @[src/main/scala/mini/CSR.scala 317:46]
  wire [31:0] _GEN_50 = csr_addr == 12'h343 ? _GEN_4 : _GEN_41; // @[src/main/scala/mini/CSR.scala 317:46]
  wire [31:0] _GEN_51 = csr_addr == 12'h342 ? _mcause_T : mcause; // @[src/main/scala/mini/CSR.scala 188:19 316:{44,53}]
  wire [31:0] _GEN_52 = csr_addr == 12'h342 ? mbadaddr : _GEN_42; // @[src/main/scala/mini/CSR.scala 189:21 316:44]
  wire [31:0] _GEN_53 = csr_addr == 12'h342 ? mtohost : _GEN_43; // @[src/main/scala/mini/CSR.scala 191:24 316:44]
  wire [31:0] _GEN_54 = csr_addr == 12'h342 ? mfromhost : _GEN_44; // @[src/main/scala/mini/CSR.scala 316:44]
  wire [31:0] _GEN_55 = csr_addr == 12'h342 ? _cycle_T_1 : _GEN_45; // @[src/main/scala/mini/CSR.scala 316:44 262:9]
  wire [31:0] _GEN_56 = csr_addr == 12'h342 ? _time_T_1 : _GEN_46; // @[src/main/scala/mini/CSR.scala 316:44 260:8]
  wire [31:0] _GEN_57 = csr_addr == 12'h342 ? _GEN_3 : _GEN_47; // @[src/main/scala/mini/CSR.scala 316:44]
  wire [31:0] _GEN_58 = csr_addr == 12'h342 ? _GEN_2 : _GEN_48; // @[src/main/scala/mini/CSR.scala 316:44]
  wire [31:0] _GEN_59 = csr_addr == 12'h342 ? _GEN_1 : _GEN_49; // @[src/main/scala/mini/CSR.scala 316:44]
  wire [31:0] _GEN_60 = csr_addr == 12'h342 ? _GEN_4 : _GEN_50; // @[src/main/scala/mini/CSR.scala 316:44]
  wire [34:0] _GEN_61 = csr_addr == 12'h341 ? _mepc_T_3 : {{3'd0}, mepc}; // @[src/main/scala/mini/CSR.scala 187:17 315:{42,49}]
  wire [31:0] _GEN_62 = csr_addr == 12'h341 ? mcause : _GEN_51; // @[src/main/scala/mini/CSR.scala 188:19 315:42]
  wire [31:0] _GEN_63 = csr_addr == 12'h341 ? mbadaddr : _GEN_52; // @[src/main/scala/mini/CSR.scala 189:21 315:42]
  wire [31:0] _GEN_64 = csr_addr == 12'h341 ? mtohost : _GEN_53; // @[src/main/scala/mini/CSR.scala 191:24 315:42]
  wire [31:0] _GEN_65 = csr_addr == 12'h341 ? mfromhost : _GEN_54; // @[src/main/scala/mini/CSR.scala 315:42]
  wire [31:0] _GEN_66 = csr_addr == 12'h341 ? _cycle_T_1 : _GEN_55; // @[src/main/scala/mini/CSR.scala 315:42 262:9]
  wire [31:0] _GEN_67 = csr_addr == 12'h341 ? _time_T_1 : _GEN_56; // @[src/main/scala/mini/CSR.scala 315:42 260:8]
  wire [31:0] _GEN_68 = csr_addr == 12'h341 ? _GEN_3 : _GEN_57; // @[src/main/scala/mini/CSR.scala 315:42]
  wire [31:0] _GEN_69 = csr_addr == 12'h341 ? _GEN_2 : _GEN_58; // @[src/main/scala/mini/CSR.scala 315:42]
  wire [31:0] _GEN_70 = csr_addr == 12'h341 ? _GEN_1 : _GEN_59; // @[src/main/scala/mini/CSR.scala 315:42]
  wire [31:0] _GEN_71 = csr_addr == 12'h341 ? _GEN_4 : _GEN_60; // @[src/main/scala/mini/CSR.scala 315:42]
  wire [31:0] _GEN_72 = csr_addr == 12'h340 ? wdata : mscratch; // @[src/main/scala/mini/CSR.scala 185:21 314:{46,57}]
  wire [34:0] _GEN_73 = csr_addr == 12'h340 ? {{3'd0}, mepc} : _GEN_61; // @[src/main/scala/mini/CSR.scala 187:17 314:46]
  wire [31:0] _GEN_74 = csr_addr == 12'h340 ? mcause : _GEN_62; // @[src/main/scala/mini/CSR.scala 188:19 314:46]
  wire [31:0] _GEN_75 = csr_addr == 12'h340 ? mbadaddr : _GEN_63; // @[src/main/scala/mini/CSR.scala 189:21 314:46]
  wire [31:0] _GEN_76 = csr_addr == 12'h340 ? mtohost : _GEN_64; // @[src/main/scala/mini/CSR.scala 191:24 314:46]
  wire [31:0] _GEN_77 = csr_addr == 12'h340 ? mfromhost : _GEN_65; // @[src/main/scala/mini/CSR.scala 314:46]
  wire [31:0] _GEN_78 = csr_addr == 12'h340 ? _cycle_T_1 : _GEN_66; // @[src/main/scala/mini/CSR.scala 314:46 262:9]
  wire [31:0] _GEN_79 = csr_addr == 12'h340 ? _time_T_1 : _GEN_67; // @[src/main/scala/mini/CSR.scala 314:46 260:8]
  wire [31:0] _GEN_80 = csr_addr == 12'h340 ? _GEN_3 : _GEN_68; // @[src/main/scala/mini/CSR.scala 314:46]
  wire [31:0] _GEN_81 = csr_addr == 12'h340 ? _GEN_2 : _GEN_69; // @[src/main/scala/mini/CSR.scala 314:46]
  wire [31:0] _GEN_82 = csr_addr == 12'h340 ? _GEN_1 : _GEN_70; // @[src/main/scala/mini/CSR.scala 314:46]
  wire [31:0] _GEN_83 = csr_addr == 12'h340 ? _GEN_4 : _GEN_71; // @[src/main/scala/mini/CSR.scala 314:46]
  wire [31:0] _GEN_84 = csr_addr == 12'h321 ? wdata : mtimecmp; // @[src/main/scala/mini/CSR.scala 183:21 313:{46,57}]
  wire [31:0] _GEN_85 = csr_addr == 12'h321 ? mscratch : _GEN_72; // @[src/main/scala/mini/CSR.scala 185:21 313:46]
  wire [34:0] _GEN_86 = csr_addr == 12'h321 ? {{3'd0}, mepc} : _GEN_73; // @[src/main/scala/mini/CSR.scala 187:17 313:46]
  wire [31:0] _GEN_87 = csr_addr == 12'h321 ? mcause : _GEN_74; // @[src/main/scala/mini/CSR.scala 188:19 313:46]
  wire [31:0] _GEN_88 = csr_addr == 12'h321 ? mbadaddr : _GEN_75; // @[src/main/scala/mini/CSR.scala 189:21 313:46]
  wire [31:0] _GEN_89 = csr_addr == 12'h321 ? mtohost : _GEN_76; // @[src/main/scala/mini/CSR.scala 191:24 313:46]
  wire [31:0] _GEN_90 = csr_addr == 12'h321 ? mfromhost : _GEN_77; // @[src/main/scala/mini/CSR.scala 313:46]
  wire [31:0] _GEN_91 = csr_addr == 12'h321 ? _cycle_T_1 : _GEN_78; // @[src/main/scala/mini/CSR.scala 313:46 262:9]
  wire [31:0] _GEN_92 = csr_addr == 12'h321 ? _time_T_1 : _GEN_79; // @[src/main/scala/mini/CSR.scala 313:46 260:8]
  wire [31:0] _GEN_93 = csr_addr == 12'h321 ? _GEN_3 : _GEN_80; // @[src/main/scala/mini/CSR.scala 313:46]
  wire [31:0] _GEN_94 = csr_addr == 12'h321 ? _GEN_2 : _GEN_81; // @[src/main/scala/mini/CSR.scala 313:46]
  wire [31:0] _GEN_95 = csr_addr == 12'h321 ? _GEN_1 : _GEN_82; // @[src/main/scala/mini/CSR.scala 313:46]
  wire [31:0] _GEN_96 = csr_addr == 12'h321 ? _GEN_4 : _GEN_83; // @[src/main/scala/mini/CSR.scala 313:46]
  wire [31:0] _GEN_97 = csr_addr == 12'h741 ? wdata : _GEN_95; // @[src/main/scala/mini/CSR.scala 312:{44,52}]
  wire [31:0] _GEN_98 = csr_addr == 12'h741 ? mtimecmp : _GEN_84; // @[src/main/scala/mini/CSR.scala 183:21 312:44]
  wire [31:0] _GEN_99 = csr_addr == 12'h741 ? mscratch : _GEN_85; // @[src/main/scala/mini/CSR.scala 185:21 312:44]
  wire [34:0] _GEN_100 = csr_addr == 12'h741 ? {{3'd0}, mepc} : _GEN_86; // @[src/main/scala/mini/CSR.scala 187:17 312:44]
  wire [31:0] _GEN_101 = csr_addr == 12'h741 ? mcause : _GEN_87; // @[src/main/scala/mini/CSR.scala 188:19 312:44]
  wire [31:0] _GEN_102 = csr_addr == 12'h741 ? mbadaddr : _GEN_88; // @[src/main/scala/mini/CSR.scala 189:21 312:44]
  wire [31:0] _GEN_103 = csr_addr == 12'h741 ? mtohost : _GEN_89; // @[src/main/scala/mini/CSR.scala 191:24 312:44]
  wire [31:0] _GEN_104 = csr_addr == 12'h741 ? mfromhost : _GEN_90; // @[src/main/scala/mini/CSR.scala 312:44]
  wire [31:0] _GEN_105 = csr_addr == 12'h741 ? _cycle_T_1 : _GEN_91; // @[src/main/scala/mini/CSR.scala 312:44 262:9]
  wire [31:0] _GEN_106 = csr_addr == 12'h741 ? _time_T_1 : _GEN_92; // @[src/main/scala/mini/CSR.scala 312:44 260:8]
  wire [31:0] _GEN_107 = csr_addr == 12'h741 ? _GEN_3 : _GEN_93; // @[src/main/scala/mini/CSR.scala 312:44]
  wire [31:0] _GEN_108 = csr_addr == 12'h741 ? _GEN_2 : _GEN_94; // @[src/main/scala/mini/CSR.scala 312:44]
  wire [31:0] _GEN_109 = csr_addr == 12'h741 ? _GEN_4 : _GEN_96; // @[src/main/scala/mini/CSR.scala 312:44]
  wire [31:0] _GEN_110 = csr_addr == 12'h701 ? wdata : _GEN_106; // @[src/main/scala/mini/CSR.scala 311:{43,50}]
  wire [31:0] _GEN_111 = csr_addr == 12'h701 ? _GEN_1 : _GEN_97; // @[src/main/scala/mini/CSR.scala 311:43]
  wire [31:0] _GEN_112 = csr_addr == 12'h701 ? mtimecmp : _GEN_98; // @[src/main/scala/mini/CSR.scala 183:21 311:43]
  wire [31:0] _GEN_113 = csr_addr == 12'h701 ? mscratch : _GEN_99; // @[src/main/scala/mini/CSR.scala 185:21 311:43]
  wire [34:0] _GEN_114 = csr_addr == 12'h701 ? {{3'd0}, mepc} : _GEN_100; // @[src/main/scala/mini/CSR.scala 187:17 311:43]
  wire [31:0] _GEN_115 = csr_addr == 12'h701 ? mcause : _GEN_101; // @[src/main/scala/mini/CSR.scala 188:19 311:43]
  wire [31:0] _GEN_116 = csr_addr == 12'h701 ? mbadaddr : _GEN_102; // @[src/main/scala/mini/CSR.scala 189:21 311:43]
  wire [31:0] _GEN_117 = csr_addr == 12'h701 ? mtohost : _GEN_103; // @[src/main/scala/mini/CSR.scala 191:24 311:43]
  wire [31:0] _GEN_118 = csr_addr == 12'h701 ? mfromhost : _GEN_104; // @[src/main/scala/mini/CSR.scala 311:43]
  wire [31:0] _GEN_119 = csr_addr == 12'h701 ? _cycle_T_1 : _GEN_105; // @[src/main/scala/mini/CSR.scala 311:43 262:9]
  wire [31:0] _GEN_120 = csr_addr == 12'h701 ? _GEN_3 : _GEN_107; // @[src/main/scala/mini/CSR.scala 311:43]
  wire [31:0] _GEN_121 = csr_addr == 12'h701 ? _GEN_2 : _GEN_108; // @[src/main/scala/mini/CSR.scala 311:43]
  wire [31:0] _GEN_122 = csr_addr == 12'h701 ? _GEN_4 : _GEN_109; // @[src/main/scala/mini/CSR.scala 311:43]
  wire  _GEN_123 = csr_addr == 12'h304 ? wdata[7] : MTIE; // @[src/main/scala/mini/CSR.scala 307:41 308:16 171:21]
  wire  _GEN_124 = csr_addr == 12'h304 ? wdata[3] : MSIE; // @[src/main/scala/mini/CSR.scala 307:41 309:16 177:21]
  wire [31:0] _GEN_125 = csr_addr == 12'h304 ? _time_T_1 : _GEN_110; // @[src/main/scala/mini/CSR.scala 307:41 260:8]
  wire [31:0] _GEN_126 = csr_addr == 12'h304 ? _GEN_1 : _GEN_111; // @[src/main/scala/mini/CSR.scala 307:41]
  wire [31:0] _GEN_127 = csr_addr == 12'h304 ? mtimecmp : _GEN_112; // @[src/main/scala/mini/CSR.scala 183:21 307:41]
  wire [31:0] _GEN_128 = csr_addr == 12'h304 ? mscratch : _GEN_113; // @[src/main/scala/mini/CSR.scala 185:21 307:41]
  wire [34:0] _GEN_129 = csr_addr == 12'h304 ? {{3'd0}, mepc} : _GEN_114; // @[src/main/scala/mini/CSR.scala 187:17 307:41]
  wire [31:0] _GEN_130 = csr_addr == 12'h304 ? mcause : _GEN_115; // @[src/main/scala/mini/CSR.scala 188:19 307:41]
  wire [31:0] _GEN_131 = csr_addr == 12'h304 ? mbadaddr : _GEN_116; // @[src/main/scala/mini/CSR.scala 189:21 307:41]
  wire [31:0] _GEN_132 = csr_addr == 12'h304 ? mtohost : _GEN_117; // @[src/main/scala/mini/CSR.scala 191:24 307:41]
  wire [31:0] _GEN_133 = csr_addr == 12'h304 ? mfromhost : _GEN_118; // @[src/main/scala/mini/CSR.scala 307:41]
  wire [31:0] _GEN_134 = csr_addr == 12'h304 ? _cycle_T_1 : _GEN_119; // @[src/main/scala/mini/CSR.scala 307:41 262:9]
  wire [31:0] _GEN_135 = csr_addr == 12'h304 ? _GEN_3 : _GEN_120; // @[src/main/scala/mini/CSR.scala 307:41]
  wire [31:0] _GEN_136 = csr_addr == 12'h304 ? _GEN_2 : _GEN_121; // @[src/main/scala/mini/CSR.scala 307:41]
  wire [31:0] _GEN_137 = csr_addr == 12'h304 ? _GEN_4 : _GEN_122; // @[src/main/scala/mini/CSR.scala 307:41]
  wire  _GEN_138 = csr_addr == 12'h344 ? wdata[7] : MTIP; // @[src/main/scala/mini/CSR.scala 303:41 304:16 168:21]
  wire  _GEN_139 = csr_addr == 12'h344 ? wdata[3] : MSIP; // @[src/main/scala/mini/CSR.scala 303:41 305:16 174:21]
  wire  _GEN_140 = csr_addr == 12'h344 ? MTIE : _GEN_123; // @[src/main/scala/mini/CSR.scala 171:21 303:41]
  wire  _GEN_141 = csr_addr == 12'h344 ? MSIE : _GEN_124; // @[src/main/scala/mini/CSR.scala 177:21 303:41]
  wire [31:0] _GEN_142 = csr_addr == 12'h344 ? _time_T_1 : _GEN_125; // @[src/main/scala/mini/CSR.scala 303:41 260:8]
  wire [31:0] _GEN_143 = csr_addr == 12'h344 ? _GEN_1 : _GEN_126; // @[src/main/scala/mini/CSR.scala 303:41]
  wire [31:0] _GEN_144 = csr_addr == 12'h344 ? mtimecmp : _GEN_127; // @[src/main/scala/mini/CSR.scala 183:21 303:41]
  wire [31:0] _GEN_145 = csr_addr == 12'h344 ? mscratch : _GEN_128; // @[src/main/scala/mini/CSR.scala 185:21 303:41]
  wire [34:0] _GEN_146 = csr_addr == 12'h344 ? {{3'd0}, mepc} : _GEN_129; // @[src/main/scala/mini/CSR.scala 187:17 303:41]
  wire [31:0] _GEN_147 = csr_addr == 12'h344 ? mcause : _GEN_130; // @[src/main/scala/mini/CSR.scala 188:19 303:41]
  wire [31:0] _GEN_148 = csr_addr == 12'h344 ? mbadaddr : _GEN_131; // @[src/main/scala/mini/CSR.scala 189:21 303:41]
  wire [31:0] _GEN_149 = csr_addr == 12'h344 ? mtohost : _GEN_132; // @[src/main/scala/mini/CSR.scala 191:24 303:41]
  wire [31:0] _GEN_150 = csr_addr == 12'h344 ? mfromhost : _GEN_133; // @[src/main/scala/mini/CSR.scala 303:41]
  wire [31:0] _GEN_151 = csr_addr == 12'h344 ? _cycle_T_1 : _GEN_134; // @[src/main/scala/mini/CSR.scala 303:41 262:9]
  wire [31:0] _GEN_152 = csr_addr == 12'h344 ? _GEN_3 : _GEN_135; // @[src/main/scala/mini/CSR.scala 303:41]
  wire [31:0] _GEN_153 = csr_addr == 12'h344 ? _GEN_2 : _GEN_136; // @[src/main/scala/mini/CSR.scala 303:41]
  wire [31:0] _GEN_154 = csr_addr == 12'h344 ? _GEN_4 : _GEN_137; // @[src/main/scala/mini/CSR.scala 303:41]
  wire [1:0] _GEN_155 = csr_addr == 12'h300 ? wdata[5:4] : PRV1; // @[src/main/scala/mini/CSR.scala 297:38 298:14 148:21]
  wire  _GEN_156 = csr_addr == 12'h300 ? wdata[3] : IE1; // @[src/main/scala/mini/CSR.scala 297:38 299:13 152:20]
  wire [1:0] _GEN_157 = csr_addr == 12'h300 ? wdata[2:1] : PRV; // @[src/main/scala/mini/CSR.scala 297:38 300:13 147:20]
  wire  _GEN_158 = csr_addr == 12'h300 ? wdata[0] : IE; // @[src/main/scala/mini/CSR.scala 297:38 301:12 151:19]
  wire  _GEN_159 = csr_addr == 12'h300 ? MTIP : _GEN_138; // @[src/main/scala/mini/CSR.scala 168:21 297:38]
  wire  _GEN_160 = csr_addr == 12'h300 ? MSIP : _GEN_139; // @[src/main/scala/mini/CSR.scala 174:21 297:38]
  wire  _GEN_161 = csr_addr == 12'h300 ? MTIE : _GEN_140; // @[src/main/scala/mini/CSR.scala 171:21 297:38]
  wire  _GEN_162 = csr_addr == 12'h300 ? MSIE : _GEN_141; // @[src/main/scala/mini/CSR.scala 177:21 297:38]
  wire [31:0] _GEN_163 = csr_addr == 12'h300 ? _time_T_1 : _GEN_142; // @[src/main/scala/mini/CSR.scala 297:38 260:8]
  wire [31:0] _GEN_164 = csr_addr == 12'h300 ? _GEN_1 : _GEN_143; // @[src/main/scala/mini/CSR.scala 297:38]
  wire [31:0] _GEN_165 = csr_addr == 12'h300 ? mtimecmp : _GEN_144; // @[src/main/scala/mini/CSR.scala 183:21 297:38]
  wire [31:0] _GEN_166 = csr_addr == 12'h300 ? mscratch : _GEN_145; // @[src/main/scala/mini/CSR.scala 185:21 297:38]
  wire [34:0] _GEN_167 = csr_addr == 12'h300 ? {{3'd0}, mepc} : _GEN_146; // @[src/main/scala/mini/CSR.scala 187:17 297:38]
  wire [31:0] _GEN_168 = csr_addr == 12'h300 ? mcause : _GEN_147; // @[src/main/scala/mini/CSR.scala 188:19 297:38]
  wire [31:0] _GEN_169 = csr_addr == 12'h300 ? mbadaddr : _GEN_148; // @[src/main/scala/mini/CSR.scala 189:21 297:38]
  wire [31:0] _GEN_170 = csr_addr == 12'h300 ? mtohost : _GEN_149; // @[src/main/scala/mini/CSR.scala 191:24 297:38]
  wire [31:0] _GEN_171 = csr_addr == 12'h300 ? mfromhost : _GEN_150; // @[src/main/scala/mini/CSR.scala 297:38]
  wire [31:0] _GEN_172 = csr_addr == 12'h300 ? _cycle_T_1 : _GEN_151; // @[src/main/scala/mini/CSR.scala 297:38 262:9]
  wire [31:0] _GEN_173 = csr_addr == 12'h300 ? _GEN_3 : _GEN_152; // @[src/main/scala/mini/CSR.scala 297:38]
  wire [31:0] _GEN_174 = csr_addr == 12'h300 ? _GEN_2 : _GEN_153; // @[src/main/scala/mini/CSR.scala 297:38]
  wire [31:0] _GEN_175 = csr_addr == 12'h300 ? _GEN_4 : _GEN_154; // @[src/main/scala/mini/CSR.scala 297:38]
  wire [1:0] _GEN_176 = wen ? _GEN_155 : PRV1; // @[src/main/scala/mini/CSR.scala 148:21 296:21]
  wire  _GEN_177 = wen ? _GEN_156 : IE1; // @[src/main/scala/mini/CSR.scala 152:20 296:21]
  wire [1:0] _GEN_178 = wen ? _GEN_157 : PRV; // @[src/main/scala/mini/CSR.scala 147:20 296:21]
  wire  _GEN_179 = wen ? _GEN_158 : IE; // @[src/main/scala/mini/CSR.scala 151:19 296:21]
  wire  _GEN_180 = wen ? _GEN_159 : MTIP; // @[src/main/scala/mini/CSR.scala 168:21 296:21]
  wire  _GEN_181 = wen ? _GEN_160 : MSIP; // @[src/main/scala/mini/CSR.scala 174:21 296:21]
  wire  _GEN_182 = wen ? _GEN_161 : MTIE; // @[src/main/scala/mini/CSR.scala 171:21 296:21]
  wire  _GEN_183 = wen ? _GEN_162 : MSIE; // @[src/main/scala/mini/CSR.scala 177:21 296:21]
  wire [31:0] _GEN_184 = wen ? _GEN_163 : _time_T_1; // @[src/main/scala/mini/CSR.scala 296:21 260:8]
  wire [31:0] _GEN_185 = wen ? _GEN_164 : _GEN_1; // @[src/main/scala/mini/CSR.scala 296:21]
  wire [34:0] _GEN_188 = wen ? _GEN_167 : {{3'd0}, mepc}; // @[src/main/scala/mini/CSR.scala 187:17 296:21]
  wire [31:0] _GEN_191 = wen ? _GEN_170 : mtohost; // @[src/main/scala/mini/CSR.scala 296:21 191:24]
  wire [31:0] _GEN_193 = wen ? _GEN_172 : _cycle_T_1; // @[src/main/scala/mini/CSR.scala 296:21 262:9]
  wire [31:0] _GEN_194 = wen ? _GEN_173 : _GEN_3; // @[src/main/scala/mini/CSR.scala 296:21]
  wire [31:0] _GEN_195 = wen ? _GEN_174 : _GEN_2; // @[src/main/scala/mini/CSR.scala 296:21]
  wire [31:0] _GEN_196 = wen ? _GEN_175 : _GEN_4; // @[src/main/scala/mini/CSR.scala 296:21]
  wire  _GEN_200 = isEret | _GEN_177; // @[src/main/scala/mini/CSR.scala 291:24 295:11]
  wire [34:0] _GEN_209 = isEret ? {{3'd0}, mepc} : _GEN_188; // @[src/main/scala/mini/CSR.scala 187:17 291:24]
  wire [34:0] _GEN_218 = io__expt ? {{3'd0}, _mepc_T_1} : _GEN_209; // @[src/main/scala/mini/CSR.scala 283:19 284:12]
  wire [34:0] _GEN_239 = _isInstRet_T_5 ? _GEN_218 : {{3'd0}, mepc}; // @[src/main/scala/mini/CSR.scala 187:17 280:19]
  wire  resultEventWire_valid = io__expt; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 75:21 src/main/scala/mini/CSR.scala 338:25]
  wire [31:0] resultEventWire_intrNO = 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 75:21 src/main/scala/mini/CSR.scala 339:26]
  wire [31:0] resultEventWire_cause = {{28'd0}, exception_case}; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 75:21 src/main/scala/mini/CSR.scala 340:25]
  wire [31:0] resultEventWire_exceptionPC = _mepc_T_1; // @[src/main/scala/mini/CSR.scala 341:45]
  wire [31:0] resultEventWire_exceptionInst = io__inst; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 75:21 src/main/scala/mini/CSR.scala 342:33]
  assign io__out = _io_out_T_1 ? cycle : _io_out_T_85; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  assign io__expt = _io_expt_T_14 | isEcall | isEbreak; // @[src/main/scala/mini/CSR.scala 255:41]
  assign io__evec = 32'h100 + _GEN_260; // @[src/main/scala/mini/CSR.scala 256:20]
  assign io__epc = mepc; // @[src/main/scala/mini/CSR.scala 257:10]
  assign resultEventWire_0_valid = resultEventWire_valid;
  assign resultEventWire_0_intrNO = resultEventWire_intrNO;
  assign resultEventWire_0_cause = resultEventWire_cause;
  assign resultEventWire_0_exceptionPC = resultEventWire_exceptionPC;
  assign resultEventWire_0_exceptionInst = resultEventWire_exceptionInst;
  assign io_expt = io__expt;
  always @(posedge clock) begin
    if (reset) begin // @[src/main/scala/mini/CSR.scala 130:21]
      time_ <= 32'h0; // @[src/main/scala/mini/CSR.scala 130:21]
    end else if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (io__expt) begin // @[src/main/scala/mini/CSR.scala 283:19]
        time_ <= _time_T_1; // @[src/main/scala/mini/CSR.scala 260:8]
      end else if (isEret) begin // @[src/main/scala/mini/CSR.scala 291:24]
        time_ <= _time_T_1; // @[src/main/scala/mini/CSR.scala 260:8]
      end else begin
        time_ <= _GEN_184;
      end
    end else begin
      time_ <= _time_T_1; // @[src/main/scala/mini/CSR.scala 260:8]
    end
    if (reset) begin // @[src/main/scala/mini/CSR.scala 131:22]
      timeh <= 32'h0; // @[src/main/scala/mini/CSR.scala 131:22]
    end else if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (io__expt) begin // @[src/main/scala/mini/CSR.scala 283:19]
        timeh <= _GEN_1;
      end else if (isEret) begin // @[src/main/scala/mini/CSR.scala 291:24]
        timeh <= _GEN_1;
      end else begin
        timeh <= _GEN_185;
      end
    end else begin
      timeh <= _GEN_1;
    end
    if (reset) begin // @[src/main/scala/mini/CSR.scala 132:22]
      cycle <= 32'h0; // @[src/main/scala/mini/CSR.scala 132:22]
    end else if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (io__expt) begin // @[src/main/scala/mini/CSR.scala 283:19]
        cycle <= _cycle_T_1; // @[src/main/scala/mini/CSR.scala 262:9]
      end else if (isEret) begin // @[src/main/scala/mini/CSR.scala 291:24]
        cycle <= _cycle_T_1; // @[src/main/scala/mini/CSR.scala 262:9]
      end else begin
        cycle <= _GEN_193;
      end
    end else begin
      cycle <= _cycle_T_1; // @[src/main/scala/mini/CSR.scala 262:9]
    end
    if (reset) begin // @[src/main/scala/mini/CSR.scala 133:23]
      cycleh <= 32'h0; // @[src/main/scala/mini/CSR.scala 133:23]
    end else if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (io__expt) begin // @[src/main/scala/mini/CSR.scala 283:19]
        cycleh <= _GEN_2;
      end else if (isEret) begin // @[src/main/scala/mini/CSR.scala 291:24]
        cycleh <= _GEN_2;
      end else begin
        cycleh <= _GEN_195;
      end
    end else begin
      cycleh <= _GEN_2;
    end
    if (reset) begin // @[src/main/scala/mini/CSR.scala 134:24]
      instret <= 32'h0; // @[src/main/scala/mini/CSR.scala 134:24]
    end else if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (io__expt) begin // @[src/main/scala/mini/CSR.scala 283:19]
        instret <= _GEN_3;
      end else if (isEret) begin // @[src/main/scala/mini/CSR.scala 291:24]
        instret <= _GEN_3;
      end else begin
        instret <= _GEN_194;
      end
    end else begin
      instret <= _GEN_3;
    end
    if (reset) begin // @[src/main/scala/mini/CSR.scala 135:25]
      instreth <= 32'h0; // @[src/main/scala/mini/CSR.scala 135:25]
    end else if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (io__expt) begin // @[src/main/scala/mini/CSR.scala 283:19]
        instreth <= _GEN_4;
      end else if (isEret) begin // @[src/main/scala/mini/CSR.scala 291:24]
        instreth <= _GEN_4;
      end else begin
        instreth <= _GEN_196;
      end
    end else begin
      instreth <= _GEN_4;
    end
    if (reset) begin // @[src/main/scala/mini/CSR.scala 147:20]
      PRV <= 2'h3; // @[src/main/scala/mini/CSR.scala 147:20]
    end else if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (io__expt) begin // @[src/main/scala/mini/CSR.scala 283:19]
        PRV <= 2'h3; // @[src/main/scala/mini/CSR.scala 286:11]
      end else if (isEret) begin // @[src/main/scala/mini/CSR.scala 291:24]
        PRV <= PRV1; // @[src/main/scala/mini/CSR.scala 292:11]
      end else begin
        PRV <= _GEN_178;
      end
    end
    if (reset) begin // @[src/main/scala/mini/CSR.scala 148:21]
      PRV1 <= 2'h3; // @[src/main/scala/mini/CSR.scala 148:21]
    end else if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (io__expt) begin // @[src/main/scala/mini/CSR.scala 283:19]
        PRV1 <= PRV; // @[src/main/scala/mini/CSR.scala 288:12]
      end else if (isEret) begin // @[src/main/scala/mini/CSR.scala 291:24]
        PRV1 <= 2'h0; // @[src/main/scala/mini/CSR.scala 294:12]
      end else begin
        PRV1 <= _GEN_176;
      end
    end
    if (reset) begin // @[src/main/scala/mini/CSR.scala 151:19]
      IE <= 1'h0; // @[src/main/scala/mini/CSR.scala 151:19]
    end else if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (io__expt) begin // @[src/main/scala/mini/CSR.scala 283:19]
        IE <= 1'h0; // @[src/main/scala/mini/CSR.scala 287:10]
      end else if (isEret) begin // @[src/main/scala/mini/CSR.scala 291:24]
        IE <= IE1; // @[src/main/scala/mini/CSR.scala 293:10]
      end else begin
        IE <= _GEN_179;
      end
    end
    if (reset) begin // @[src/main/scala/mini/CSR.scala 152:20]
      IE1 <= 1'h0; // @[src/main/scala/mini/CSR.scala 152:20]
    end else if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (io__expt) begin // @[src/main/scala/mini/CSR.scala 283:19]
        IE1 <= IE; // @[src/main/scala/mini/CSR.scala 289:11]
      end else begin
        IE1 <= _GEN_200;
      end
    end
    if (reset) begin // @[src/main/scala/mini/CSR.scala 168:21]
      MTIP <= 1'h0; // @[src/main/scala/mini/CSR.scala 168:21]
    end else if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (!(io__expt)) begin // @[src/main/scala/mini/CSR.scala 283:19]
        if (!(isEret)) begin // @[src/main/scala/mini/CSR.scala 291:24]
          MTIP <= _GEN_180;
        end
      end
    end
    if (reset) begin // @[src/main/scala/mini/CSR.scala 171:21]
      MTIE <= 1'h0; // @[src/main/scala/mini/CSR.scala 171:21]
    end else if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (!(io__expt)) begin // @[src/main/scala/mini/CSR.scala 283:19]
        if (!(isEret)) begin // @[src/main/scala/mini/CSR.scala 291:24]
          MTIE <= _GEN_182;
        end
      end
    end
    if (reset) begin // @[src/main/scala/mini/CSR.scala 174:21]
      MSIP <= 1'h0; // @[src/main/scala/mini/CSR.scala 174:21]
    end else if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (!(io__expt)) begin // @[src/main/scala/mini/CSR.scala 283:19]
        if (!(isEret)) begin // @[src/main/scala/mini/CSR.scala 291:24]
          MSIP <= _GEN_181;
        end
      end
    end
    if (reset) begin // @[src/main/scala/mini/CSR.scala 177:21]
      MSIE <= 1'h0; // @[src/main/scala/mini/CSR.scala 177:21]
    end else if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (!(io__expt)) begin // @[src/main/scala/mini/CSR.scala 283:19]
        if (!(isEret)) begin // @[src/main/scala/mini/CSR.scala 291:24]
          MSIE <= _GEN_183;
        end
      end
    end
    if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (!(io__expt)) begin // @[src/main/scala/mini/CSR.scala 283:19]
        if (!(isEret)) begin // @[src/main/scala/mini/CSR.scala 291:24]
          if (wen) begin // @[src/main/scala/mini/CSR.scala 296:21]
            mtimecmp <= _GEN_165;
          end
        end
      end
    end
    if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (!(io__expt)) begin // @[src/main/scala/mini/CSR.scala 283:19]
        if (!(isEret)) begin // @[src/main/scala/mini/CSR.scala 291:24]
          if (wen) begin // @[src/main/scala/mini/CSR.scala 296:21]
            mscratch <= _GEN_166;
          end
        end
      end
    end
    mepc <= _GEN_239[31:0];
    if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (io__expt) begin // @[src/main/scala/mini/CSR.scala 283:19]
        mcause <= {{28'd0}, exception_case}; // @[src/main/scala/mini/CSR.scala 285:14]
      end else if (!(isEret)) begin // @[src/main/scala/mini/CSR.scala 291:24]
        if (wen) begin // @[src/main/scala/mini/CSR.scala 296:21]
          mcause <= _GEN_168;
        end
      end
    end
    if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (io__expt) begin // @[src/main/scala/mini/CSR.scala 283:19]
        if (iaddrInvalid | laddrInvalid | saddrInvalid) begin // @[src/main/scala/mini/CSR.scala 290:58]
          mbadaddr <= io__addr; // @[src/main/scala/mini/CSR.scala 290:69]
        end
      end else if (!(isEret)) begin // @[src/main/scala/mini/CSR.scala 291:24]
        if (wen) begin // @[src/main/scala/mini/CSR.scala 296:21]
          mbadaddr <= _GEN_169;
        end
      end
    end
    if (reset) begin // @[src/main/scala/mini/CSR.scala 191:24]
      mtohost <= 32'h0; // @[src/main/scala/mini/CSR.scala 191:24]
    end else if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (!(io__expt)) begin // @[src/main/scala/mini/CSR.scala 283:19]
        if (!(isEret)) begin // @[src/main/scala/mini/CSR.scala 291:24]
          mtohost <= _GEN_191;
        end
      end
    end
    if (_isInstRet_T_5) begin // @[src/main/scala/mini/CSR.scala 280:19]
      if (!(io__expt)) begin // @[src/main/scala/mini/CSR.scala 283:19]
        if (!(isEret)) begin // @[src/main/scala/mini/CSR.scala 291:24]
          if (wen) begin // @[src/main/scala/mini/CSR.scala 296:21]
            mfromhost <= _GEN_171;
          end
        end
      end
    end
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_isInstRet_T_5 & ~reset) begin
          $fwrite(32'h80000002,"Exception:%d\n",io__expt); // @[src/main/scala/mini/CSR.scala 281:11]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  time_ = _RAND_0[31:0];
  _RAND_1 = {1{`RANDOM}};
  timeh = _RAND_1[31:0];
  _RAND_2 = {1{`RANDOM}};
  cycle = _RAND_2[31:0];
  _RAND_3 = {1{`RANDOM}};
  cycleh = _RAND_3[31:0];
  _RAND_4 = {1{`RANDOM}};
  instret = _RAND_4[31:0];
  _RAND_5 = {1{`RANDOM}};
  instreth = _RAND_5[31:0];
  _RAND_6 = {1{`RANDOM}};
  PRV = _RAND_6[1:0];
  _RAND_7 = {1{`RANDOM}};
  PRV1 = _RAND_7[1:0];
  _RAND_8 = {1{`RANDOM}};
  IE = _RAND_8[0:0];
  _RAND_9 = {1{`RANDOM}};
  IE1 = _RAND_9[0:0];
  _RAND_10 = {1{`RANDOM}};
  MTIP = _RAND_10[0:0];
  _RAND_11 = {1{`RANDOM}};
  MTIE = _RAND_11[0:0];
  _RAND_12 = {1{`RANDOM}};
  MSIP = _RAND_12[0:0];
  _RAND_13 = {1{`RANDOM}};
  MSIE = _RAND_13[0:0];
  _RAND_14 = {1{`RANDOM}};
  mtimecmp = _RAND_14[31:0];
  _RAND_15 = {1{`RANDOM}};
  mscratch = _RAND_15[31:0];
  _RAND_16 = {1{`RANDOM}};
  mepc = _RAND_16[31:0];
  _RAND_17 = {1{`RANDOM}};
  mcause = _RAND_17[31:0];
  _RAND_18 = {1{`RANDOM}};
  mbadaddr = _RAND_18[31:0];
  _RAND_19 = {1{`RANDOM}};
  mtohost = _RAND_19[31:0];
  _RAND_20 = {1{`RANDOM}};
  mfromhost = _RAND_20[31:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module RegFile(
  input         clock,
  input         reset,
  input  [4:0]  io_raddr1, // @[src/main/scala/mini/RegFile.scala 20:14]
  input  [4:0]  io_raddr2, // @[src/main/scala/mini/RegFile.scala 20:14]
  output [31:0] io_rdata1, // @[src/main/scala/mini/RegFile.scala 20:14]
  output [31:0] io_rdata2, // @[src/main/scala/mini/RegFile.scala 20:14]
  input         io_wen, // @[src/main/scala/mini/RegFile.scala 20:14]
  input  [4:0]  io_waddr, // @[src/main/scala/mini/RegFile.scala 20:14]
  input  [31:0] io_wdata, // @[src/main/scala/mini/RegFile.scala 20:14]
  input  [31:0] ArbitraryRegFile_0,
  input  [31:0] ArbitraryRegFile_1,
  input  [31:0] ArbitraryRegFile_2,
  input  [31:0] ArbitraryRegFile_3,
  input  [31:0] ArbitraryRegFile_4,
  input  [31:0] ArbitraryRegFile_5,
  input  [31:0] ArbitraryRegFile_6,
  input  [31:0] ArbitraryRegFile_7,
  input  [31:0] ArbitraryRegFile_8,
  input  [31:0] ArbitraryRegFile_9,
  input  [31:0] ArbitraryRegFile_10,
  input  [31:0] ArbitraryRegFile_11,
  input  [31:0] ArbitraryRegFile_12,
  input  [31:0] ArbitraryRegFile_13,
  input  [31:0] ArbitraryRegFile_14,
  input  [31:0] ArbitraryRegFile_15,
  input  [31:0] ArbitraryRegFile_16,
  input  [31:0] ArbitraryRegFile_17,
  input  [31:0] ArbitraryRegFile_18,
  input  [31:0] ArbitraryRegFile_19,
  input  [31:0] ArbitraryRegFile_20,
  input  [31:0] ArbitraryRegFile_21,
  input  [31:0] ArbitraryRegFile_22,
  input  [31:0] ArbitraryRegFile_23,
  input  [31:0] ArbitraryRegFile_24,
  input  [31:0] ArbitraryRegFile_25,
  input  [31:0] ArbitraryRegFile_26,
  input  [31:0] ArbitraryRegFile_27,
  input  [31:0] ArbitraryRegFile_28,
  input  [31:0] ArbitraryRegFile_29,
  input  [31:0] ArbitraryRegFile_30,
  input  [31:0] ArbitraryRegFile_31,
  output [31:0] resultRegWire_0_0,
  output [31:0] resultRegWire_0_1,
  output [31:0] resultRegWire_0_2,
  output [31:0] resultRegWire_0_3,
  output [31:0] resultRegWire_0_4,
  output [31:0] resultRegWire_0_5,
  output [31:0] resultRegWire_0_6,
  output [31:0] resultRegWire_0_7,
  output [31:0] resultRegWire_0_8,
  output [31:0] resultRegWire_0_9,
  output [31:0] resultRegWire_0_10,
  output [31:0] resultRegWire_0_11,
  output [31:0] resultRegWire_0_12,
  output [31:0] resultRegWire_0_13,
  output [31:0] resultRegWire_0_14,
  output [31:0] resultRegWire_0_15,
  output [31:0] resultRegWire_0_16,
  output [31:0] resultRegWire_0_17,
  output [31:0] resultRegWire_0_18,
  output [31:0] resultRegWire_0_19,
  output [31:0] resultRegWire_0_20,
  output [31:0] resultRegWire_0_21,
  output [31:0] resultRegWire_0_22,
  output [31:0] resultRegWire_0_23,
  output [31:0] resultRegWire_0_24,
  output [31:0] resultRegWire_0_25,
  output [31:0] resultRegWire_0_26,
  output [31:0] resultRegWire_0_27,
  output [31:0] resultRegWire_0_28,
  output [31:0] resultRegWire_0_29,
  output [31:0] resultRegWire_0_30,
  output [31:0] resultRegWire_0_31
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
  reg [31:0] _RAND_4;
  reg [31:0] _RAND_5;
  reg [31:0] _RAND_6;
  reg [31:0] _RAND_7;
  reg [31:0] _RAND_8;
  reg [31:0] _RAND_9;
  reg [31:0] _RAND_10;
  reg [31:0] _RAND_11;
  reg [31:0] _RAND_12;
  reg [31:0] _RAND_13;
  reg [31:0] _RAND_14;
  reg [31:0] _RAND_15;
  reg [31:0] _RAND_16;
  reg [31:0] _RAND_17;
  reg [31:0] _RAND_18;
  reg [31:0] _RAND_19;
  reg [31:0] _RAND_20;
  reg [31:0] _RAND_21;
  reg [31:0] _RAND_22;
  reg [31:0] _RAND_23;
  reg [31:0] _RAND_24;
  reg [31:0] _RAND_25;
  reg [31:0] _RAND_26;
  reg [31:0] _RAND_27;
  reg [31:0] _RAND_28;
  reg [31:0] _RAND_29;
  reg [31:0] _RAND_30;
  reg [31:0] _RAND_31;
`endif // RANDOMIZE_REG_INIT
  reg [31:0] regs_0; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_1; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_2; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_3; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_4; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_5; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_6; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_7; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_8; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_9; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_10; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_11; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_12; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_13; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_14; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_15; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_16; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_17; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_18; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_19; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_20; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_21; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_22; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_23; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_24; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_25; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_26; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_27; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_28; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_29; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_30; // @[src/main/scala/mini/RegFile.scala 24:21]
  reg [31:0] regs_31; // @[src/main/scala/mini/RegFile.scala 24:21]
  wire [31:0] _GEN_1 = 5'h1 == io_raddr1 ? regs_1 : regs_0; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_2 = 5'h2 == io_raddr1 ? regs_2 : _GEN_1; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_3 = 5'h3 == io_raddr1 ? regs_3 : _GEN_2; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_4 = 5'h4 == io_raddr1 ? regs_4 : _GEN_3; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_5 = 5'h5 == io_raddr1 ? regs_5 : _GEN_4; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_6 = 5'h6 == io_raddr1 ? regs_6 : _GEN_5; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_7 = 5'h7 == io_raddr1 ? regs_7 : _GEN_6; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_8 = 5'h8 == io_raddr1 ? regs_8 : _GEN_7; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_9 = 5'h9 == io_raddr1 ? regs_9 : _GEN_8; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_10 = 5'ha == io_raddr1 ? regs_10 : _GEN_9; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_11 = 5'hb == io_raddr1 ? regs_11 : _GEN_10; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_12 = 5'hc == io_raddr1 ? regs_12 : _GEN_11; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_13 = 5'hd == io_raddr1 ? regs_13 : _GEN_12; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_14 = 5'he == io_raddr1 ? regs_14 : _GEN_13; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_15 = 5'hf == io_raddr1 ? regs_15 : _GEN_14; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_16 = 5'h10 == io_raddr1 ? regs_16 : _GEN_15; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_17 = 5'h11 == io_raddr1 ? regs_17 : _GEN_16; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_18 = 5'h12 == io_raddr1 ? regs_18 : _GEN_17; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_19 = 5'h13 == io_raddr1 ? regs_19 : _GEN_18; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_20 = 5'h14 == io_raddr1 ? regs_20 : _GEN_19; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_21 = 5'h15 == io_raddr1 ? regs_21 : _GEN_20; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_22 = 5'h16 == io_raddr1 ? regs_22 : _GEN_21; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_23 = 5'h17 == io_raddr1 ? regs_23 : _GEN_22; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_24 = 5'h18 == io_raddr1 ? regs_24 : _GEN_23; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_25 = 5'h19 == io_raddr1 ? regs_25 : _GEN_24; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_26 = 5'h1a == io_raddr1 ? regs_26 : _GEN_25; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_27 = 5'h1b == io_raddr1 ? regs_27 : _GEN_26; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_28 = 5'h1c == io_raddr1 ? regs_28 : _GEN_27; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_29 = 5'h1d == io_raddr1 ? regs_29 : _GEN_28; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_30 = 5'h1e == io_raddr1 ? regs_30 : _GEN_29; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_31 = 5'h1f == io_raddr1 ? regs_31 : _GEN_30; // @[src/main/scala/mini/RegFile.scala 29:{19,19}]
  wire [31:0] _GEN_33 = 5'h1 == io_raddr2 ? regs_1 : regs_0; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_34 = 5'h2 == io_raddr2 ? regs_2 : _GEN_33; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_35 = 5'h3 == io_raddr2 ? regs_3 : _GEN_34; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_36 = 5'h4 == io_raddr2 ? regs_4 : _GEN_35; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_37 = 5'h5 == io_raddr2 ? regs_5 : _GEN_36; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_38 = 5'h6 == io_raddr2 ? regs_6 : _GEN_37; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_39 = 5'h7 == io_raddr2 ? regs_7 : _GEN_38; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_40 = 5'h8 == io_raddr2 ? regs_8 : _GEN_39; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_41 = 5'h9 == io_raddr2 ? regs_9 : _GEN_40; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_42 = 5'ha == io_raddr2 ? regs_10 : _GEN_41; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_43 = 5'hb == io_raddr2 ? regs_11 : _GEN_42; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_44 = 5'hc == io_raddr2 ? regs_12 : _GEN_43; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_45 = 5'hd == io_raddr2 ? regs_13 : _GEN_44; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_46 = 5'he == io_raddr2 ? regs_14 : _GEN_45; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_47 = 5'hf == io_raddr2 ? regs_15 : _GEN_46; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_48 = 5'h10 == io_raddr2 ? regs_16 : _GEN_47; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_49 = 5'h11 == io_raddr2 ? regs_17 : _GEN_48; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_50 = 5'h12 == io_raddr2 ? regs_18 : _GEN_49; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_51 = 5'h13 == io_raddr2 ? regs_19 : _GEN_50; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_52 = 5'h14 == io_raddr2 ? regs_20 : _GEN_51; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_53 = 5'h15 == io_raddr2 ? regs_21 : _GEN_52; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_54 = 5'h16 == io_raddr2 ? regs_22 : _GEN_53; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_55 = 5'h17 == io_raddr2 ? regs_23 : _GEN_54; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_56 = 5'h18 == io_raddr2 ? regs_24 : _GEN_55; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_57 = 5'h19 == io_raddr2 ? regs_25 : _GEN_56; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_58 = 5'h1a == io_raddr2 ? regs_26 : _GEN_57; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_59 = 5'h1b == io_raddr2 ? regs_27 : _GEN_58; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_60 = 5'h1c == io_raddr2 ? regs_28 : _GEN_59; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_61 = 5'h1d == io_raddr2 ? regs_29 : _GEN_60; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_62 = 5'h1e == io_raddr2 ? regs_30 : _GEN_61; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_63 = 5'h1f == io_raddr2 ? regs_31 : _GEN_62; // @[src/main/scala/mini/RegFile.scala 30:{19,19}]
  wire [31:0] _GEN_65 = 5'h1 == io_waddr ? io_wdata : regs_1; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_66 = 5'h2 == io_waddr ? io_wdata : regs_2; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_67 = 5'h3 == io_waddr ? io_wdata : regs_3; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_68 = 5'h4 == io_waddr ? io_wdata : regs_4; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_69 = 5'h5 == io_waddr ? io_wdata : regs_5; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_70 = 5'h6 == io_waddr ? io_wdata : regs_6; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_71 = 5'h7 == io_waddr ? io_wdata : regs_7; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_72 = 5'h8 == io_waddr ? io_wdata : regs_8; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_73 = 5'h9 == io_waddr ? io_wdata : regs_9; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_74 = 5'ha == io_waddr ? io_wdata : regs_10; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_75 = 5'hb == io_waddr ? io_wdata : regs_11; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_76 = 5'hc == io_waddr ? io_wdata : regs_12; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_77 = 5'hd == io_waddr ? io_wdata : regs_13; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_78 = 5'he == io_waddr ? io_wdata : regs_14; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_79 = 5'hf == io_waddr ? io_wdata : regs_15; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_80 = 5'h10 == io_waddr ? io_wdata : regs_16; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_81 = 5'h11 == io_waddr ? io_wdata : regs_17; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_82 = 5'h12 == io_waddr ? io_wdata : regs_18; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_83 = 5'h13 == io_waddr ? io_wdata : regs_19; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_84 = 5'h14 == io_waddr ? io_wdata : regs_20; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_85 = 5'h15 == io_waddr ? io_wdata : regs_21; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_86 = 5'h16 == io_waddr ? io_wdata : regs_22; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_87 = 5'h17 == io_waddr ? io_wdata : regs_23; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_88 = 5'h18 == io_waddr ? io_wdata : regs_24; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_89 = 5'h19 == io_waddr ? io_wdata : regs_25; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_90 = 5'h1a == io_waddr ? io_wdata : regs_26; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_91 = 5'h1b == io_waddr ? io_wdata : regs_27; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_92 = 5'h1c == io_waddr ? io_wdata : regs_28; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_93 = 5'h1d == io_waddr ? io_wdata : regs_29; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_94 = 5'h1e == io_waddr ? io_wdata : regs_30; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_95 = 5'h1f == io_waddr ? io_wdata : regs_31; // @[src/main/scala/mini/RegFile.scala 32:{20,20} 24:21]
  wire [31:0] _GEN_96 = 5'h0 == io_waddr ? io_wdata : 32'h0; // @[src/main/scala/mini/RegFile.scala 27:20 33:{29,29}]
  wire [31:0] _GEN_129 = io_wen & |io_waddr ? _GEN_65 : regs_1; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_130 = io_wen & |io_waddr ? _GEN_66 : regs_2; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_131 = io_wen & |io_waddr ? _GEN_67 : regs_3; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_132 = io_wen & |io_waddr ? _GEN_68 : regs_4; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_133 = io_wen & |io_waddr ? _GEN_69 : regs_5; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_134 = io_wen & |io_waddr ? _GEN_70 : regs_6; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_135 = io_wen & |io_waddr ? _GEN_71 : regs_7; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_136 = io_wen & |io_waddr ? _GEN_72 : regs_8; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_137 = io_wen & |io_waddr ? _GEN_73 : regs_9; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_138 = io_wen & |io_waddr ? _GEN_74 : regs_10; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_139 = io_wen & |io_waddr ? _GEN_75 : regs_11; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_140 = io_wen & |io_waddr ? _GEN_76 : regs_12; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_141 = io_wen & |io_waddr ? _GEN_77 : regs_13; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_142 = io_wen & |io_waddr ? _GEN_78 : regs_14; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_143 = io_wen & |io_waddr ? _GEN_79 : regs_15; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_144 = io_wen & |io_waddr ? _GEN_80 : regs_16; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_145 = io_wen & |io_waddr ? _GEN_81 : regs_17; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_146 = io_wen & |io_waddr ? _GEN_82 : regs_18; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_147 = io_wen & |io_waddr ? _GEN_83 : regs_19; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_148 = io_wen & |io_waddr ? _GEN_84 : regs_20; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_149 = io_wen & |io_waddr ? _GEN_85 : regs_21; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_150 = io_wen & |io_waddr ? _GEN_86 : regs_22; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_151 = io_wen & |io_waddr ? _GEN_87 : regs_23; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_152 = io_wen & |io_waddr ? _GEN_88 : regs_24; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_153 = io_wen & |io_waddr ? _GEN_89 : regs_25; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_154 = io_wen & |io_waddr ? _GEN_90 : regs_26; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_155 = io_wen & |io_waddr ? _GEN_91 : regs_27; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_156 = io_wen & |io_waddr ? _GEN_92 : regs_28; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_157 = io_wen & |io_waddr ? _GEN_93 : regs_29; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_158 = io_wen & |io_waddr ? _GEN_94 : regs_30; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] _GEN_159 = io_wen & |io_waddr ? _GEN_95 : regs_31; // @[src/main/scala/mini/RegFile.scala 24:21 31:31]
  wire [31:0] resultRegWire_0 = io_wen & |io_waddr ? _GEN_96 : 32'h0; // @[src/main/scala/mini/RegFile.scala 27:20 31:31]
  wire [31:0] resultRegWire_1 = _GEN_129; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_2 = _GEN_130; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_3 = _GEN_131; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_4 = _GEN_132; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_5 = _GEN_133; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_6 = _GEN_134; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_7 = _GEN_135; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_8 = _GEN_136; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_9 = _GEN_137; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_10 = _GEN_138; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_11 = _GEN_139; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_12 = _GEN_140; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_13 = _GEN_141; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_14 = _GEN_142; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_15 = _GEN_143; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_16 = _GEN_144; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_17 = _GEN_145; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_18 = _GEN_146; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_19 = _GEN_147; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_20 = _GEN_148; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_21 = _GEN_149; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_22 = _GEN_150; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_23 = _GEN_151; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_24 = _GEN_152; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_25 = _GEN_153; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_26 = _GEN_154; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_27 = _GEN_155; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_28 = _GEN_156; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_29 = _GEN_157; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_30 = _GEN_158; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  wire [31:0] resultRegWire_31 = _GEN_159; // @[src/main/scala/mini/RegFile.scala 26:17 31:31]
  assign io_rdata1 = |io_raddr1 ? _GEN_31 : 32'h0; // @[src/main/scala/mini/RegFile.scala 29:19]
  assign io_rdata2 = |io_raddr2 ? _GEN_63 : 32'h0; // @[src/main/scala/mini/RegFile.scala 30:19]
  assign resultRegWire_0_0 = resultRegWire_0;
  assign resultRegWire_0_1 = resultRegWire_1;
  assign resultRegWire_0_2 = resultRegWire_2;
  assign resultRegWire_0_3 = resultRegWire_3;
  assign resultRegWire_0_4 = resultRegWire_4;
  assign resultRegWire_0_5 = resultRegWire_5;
  assign resultRegWire_0_6 = resultRegWire_6;
  assign resultRegWire_0_7 = resultRegWire_7;
  assign resultRegWire_0_8 = resultRegWire_8;
  assign resultRegWire_0_9 = resultRegWire_9;
  assign resultRegWire_0_10 = resultRegWire_10;
  assign resultRegWire_0_11 = resultRegWire_11;
  assign resultRegWire_0_12 = resultRegWire_12;
  assign resultRegWire_0_13 = resultRegWire_13;
  assign resultRegWire_0_14 = resultRegWire_14;
  assign resultRegWire_0_15 = resultRegWire_15;
  assign resultRegWire_0_16 = resultRegWire_16;
  assign resultRegWire_0_17 = resultRegWire_17;
  assign resultRegWire_0_18 = resultRegWire_18;
  assign resultRegWire_0_19 = resultRegWire_19;
  assign resultRegWire_0_20 = resultRegWire_20;
  assign resultRegWire_0_21 = resultRegWire_21;
  assign resultRegWire_0_22 = resultRegWire_22;
  assign resultRegWire_0_23 = resultRegWire_23;
  assign resultRegWire_0_24 = resultRegWire_24;
  assign resultRegWire_0_25 = resultRegWire_25;
  assign resultRegWire_0_26 = resultRegWire_26;
  assign resultRegWire_0_27 = resultRegWire_27;
  assign resultRegWire_0_28 = resultRegWire_28;
  assign resultRegWire_0_29 = resultRegWire_29;
  assign resultRegWire_0_30 = resultRegWire_30;
  assign resultRegWire_0_31 = resultRegWire_31;
  always @(posedge clock) begin
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_0 <= ArbitraryRegFile_0; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h0 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_0 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_1 <= ArbitraryRegFile_1; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h1 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_1 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_2 <= ArbitraryRegFile_2; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h2 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_2 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_3 <= ArbitraryRegFile_3; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h3 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_3 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_4 <= ArbitraryRegFile_4; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h4 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_4 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_5 <= ArbitraryRegFile_5; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h5 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_5 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_6 <= ArbitraryRegFile_6; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h6 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_6 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_7 <= ArbitraryRegFile_7; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h7 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_7 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_8 <= ArbitraryRegFile_8; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h8 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_8 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_9 <= ArbitraryRegFile_9; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h9 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_9 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_10 <= ArbitraryRegFile_10; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'ha == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_10 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_11 <= ArbitraryRegFile_11; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'hb == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_11 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_12 <= ArbitraryRegFile_12; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'hc == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_12 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_13 <= ArbitraryRegFile_13; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'hd == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_13 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_14 <= ArbitraryRegFile_14; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'he == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_14 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_15 <= ArbitraryRegFile_15; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'hf == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_15 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_16 <= ArbitraryRegFile_16; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h10 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_16 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_17 <= ArbitraryRegFile_17; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h11 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_17 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_18 <= ArbitraryRegFile_18; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h12 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_18 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_19 <= ArbitraryRegFile_19; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h13 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_19 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_20 <= ArbitraryRegFile_20; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h14 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_20 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_21 <= ArbitraryRegFile_21; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h15 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_21 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_22 <= ArbitraryRegFile_22; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h16 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_22 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_23 <= ArbitraryRegFile_23; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h17 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_23 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_24 <= ArbitraryRegFile_24; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h18 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_24 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_25 <= ArbitraryRegFile_25; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h19 == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_25 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_26 <= ArbitraryRegFile_26; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h1a == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_26 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_27 <= ArbitraryRegFile_27; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h1b == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_27 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_28 <= ArbitraryRegFile_28; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h1c == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_28 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_29 <= ArbitraryRegFile_29; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h1d == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_29 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_30 <= ArbitraryRegFile_30; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h1e == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_30 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
    if (reset) begin // @[src/main/scala/mini/RegFile.scala 24:21]
      regs_31 <= ArbitraryRegFile_31; // @[src/main/scala/mini/RegFile.scala 24:21]
    end else if (io_wen & |io_waddr) begin // @[src/main/scala/mini/RegFile.scala 31:31]
      if (5'h1f == io_waddr) begin // @[src/main/scala/mini/RegFile.scala 32:20]
        regs_31 <= io_wdata; // @[src/main/scala/mini/RegFile.scala 32:20]
      end
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  regs_0 = _RAND_0[31:0];
  _RAND_1 = {1{`RANDOM}};
  regs_1 = _RAND_1[31:0];
  _RAND_2 = {1{`RANDOM}};
  regs_2 = _RAND_2[31:0];
  _RAND_3 = {1{`RANDOM}};
  regs_3 = _RAND_3[31:0];
  _RAND_4 = {1{`RANDOM}};
  regs_4 = _RAND_4[31:0];
  _RAND_5 = {1{`RANDOM}};
  regs_5 = _RAND_5[31:0];
  _RAND_6 = {1{`RANDOM}};
  regs_6 = _RAND_6[31:0];
  _RAND_7 = {1{`RANDOM}};
  regs_7 = _RAND_7[31:0];
  _RAND_8 = {1{`RANDOM}};
  regs_8 = _RAND_8[31:0];
  _RAND_9 = {1{`RANDOM}};
  regs_9 = _RAND_9[31:0];
  _RAND_10 = {1{`RANDOM}};
  regs_10 = _RAND_10[31:0];
  _RAND_11 = {1{`RANDOM}};
  regs_11 = _RAND_11[31:0];
  _RAND_12 = {1{`RANDOM}};
  regs_12 = _RAND_12[31:0];
  _RAND_13 = {1{`RANDOM}};
  regs_13 = _RAND_13[31:0];
  _RAND_14 = {1{`RANDOM}};
  regs_14 = _RAND_14[31:0];
  _RAND_15 = {1{`RANDOM}};
  regs_15 = _RAND_15[31:0];
  _RAND_16 = {1{`RANDOM}};
  regs_16 = _RAND_16[31:0];
  _RAND_17 = {1{`RANDOM}};
  regs_17 = _RAND_17[31:0];
  _RAND_18 = {1{`RANDOM}};
  regs_18 = _RAND_18[31:0];
  _RAND_19 = {1{`RANDOM}};
  regs_19 = _RAND_19[31:0];
  _RAND_20 = {1{`RANDOM}};
  regs_20 = _RAND_20[31:0];
  _RAND_21 = {1{`RANDOM}};
  regs_21 = _RAND_21[31:0];
  _RAND_22 = {1{`RANDOM}};
  regs_22 = _RAND_22[31:0];
  _RAND_23 = {1{`RANDOM}};
  regs_23 = _RAND_23[31:0];
  _RAND_24 = {1{`RANDOM}};
  regs_24 = _RAND_24[31:0];
  _RAND_25 = {1{`RANDOM}};
  regs_25 = _RAND_25[31:0];
  _RAND_26 = {1{`RANDOM}};
  regs_26 = _RAND_26[31:0];
  _RAND_27 = {1{`RANDOM}};
  regs_27 = _RAND_27[31:0];
  _RAND_28 = {1{`RANDOM}};
  regs_28 = _RAND_28[31:0];
  _RAND_29 = {1{`RANDOM}};
  regs_29 = _RAND_29[31:0];
  _RAND_30 = {1{`RANDOM}};
  regs_30 = _RAND_30[31:0];
  _RAND_31 = {1{`RANDOM}};
  regs_31 = _RAND_31[31:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module AluArea(
  input  [31:0] io_A, // @[src/main/scala/mini/Alu.scala 64:14]
  input  [31:0] io_B, // @[src/main/scala/mini/Alu.scala 64:14]
  input  [3:0]  io_alu_op, // @[src/main/scala/mini/Alu.scala 64:14]
  output [31:0] io_out, // @[src/main/scala/mini/Alu.scala 64:14]
  output [31:0] io_sum // @[src/main/scala/mini/Alu.scala 64:14]
);
  wire [31:0] _sum_T_2 = 32'h0 - io_B; // @[src/main/scala/mini/Alu.scala 65:38]
  wire [31:0] _sum_T_3 = io_alu_op[0] ? _sum_T_2 : io_B; // @[src/main/scala/mini/Alu.scala 65:23]
  wire [31:0] sum = io_A + _sum_T_3; // @[src/main/scala/mini/Alu.scala 65:18]
  wire  _cmp_T_7 = io_alu_op[1] ? io_B[31] : io_A[31]; // @[src/main/scala/mini/Alu.scala 67:65]
  wire  cmp = io_A[31] == io_B[31] ? sum[31] : _cmp_T_7; // @[src/main/scala/mini/Alu.scala 67:8]
  wire [4:0] shamt = io_B[4:0]; // @[src/main/scala/mini/Alu.scala 68:19]
  wire [31:0] _GEN_0 = {{16'd0}, io_A[31:16]}; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_4 = _GEN_0 & 32'hffff; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_6 = {io_A[15:0], 16'h0}; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_8 = _shin_T_6 & 32'hffff0000; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_9 = _shin_T_4 | _shin_T_8; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _GEN_1 = {{8'd0}, _shin_T_9[31:8]}; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_14 = _GEN_1 & 32'hff00ff; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_16 = {_shin_T_9[23:0], 8'h0}; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_18 = _shin_T_16 & 32'hff00ff00; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_19 = _shin_T_14 | _shin_T_18; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _GEN_2 = {{4'd0}, _shin_T_19[31:4]}; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_24 = _GEN_2 & 32'hf0f0f0f; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_26 = {_shin_T_19[27:0], 4'h0}; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_28 = _shin_T_26 & 32'hf0f0f0f0; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_29 = _shin_T_24 | _shin_T_28; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _GEN_3 = {{2'd0}, _shin_T_29[31:2]}; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_34 = _GEN_3 & 32'h33333333; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_36 = {_shin_T_29[29:0], 2'h0}; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_38 = _shin_T_36 & 32'hcccccccc; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_39 = _shin_T_34 | _shin_T_38; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _GEN_4 = {{1'd0}, _shin_T_39[31:1]}; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_44 = _GEN_4 & 32'h55555555; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_46 = {_shin_T_39[30:0], 1'h0}; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_48 = _shin_T_46 & 32'haaaaaaaa; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] _shin_T_49 = _shin_T_44 | _shin_T_48; // @[src/main/scala/mini/Alu.scala 69:45]
  wire [31:0] shin = io_alu_op[3] ? io_A : _shin_T_49; // @[src/main/scala/mini/Alu.scala 69:17]
  wire  _shiftr_T_2 = io_alu_op[0] & shin[31]; // @[src/main/scala/mini/Alu.scala 70:34]
  wire [32:0] _shiftr_T_4 = {_shiftr_T_2,shin}; // @[src/main/scala/mini/Alu.scala 70:60]
  wire [32:0] _shiftr_T_5 = $signed(_shiftr_T_4) >>> shamt; // @[src/main/scala/mini/Alu.scala 70:67]
  wire [31:0] shiftr = _shiftr_T_5[31:0]; // @[src/main/scala/mini/Alu.scala 70:76]
  wire [31:0] _GEN_5 = {{16'd0}, shiftr[31:16]}; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _shiftl_T_3 = _GEN_5 & 32'hffff; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _shiftl_T_5 = {shiftr[15:0], 16'h0}; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _shiftl_T_7 = _shiftl_T_5 & 32'hffff0000; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _shiftl_T_8 = _shiftl_T_3 | _shiftl_T_7; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _GEN_6 = {{8'd0}, _shiftl_T_8[31:8]}; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _shiftl_T_13 = _GEN_6 & 32'hff00ff; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _shiftl_T_15 = {_shiftl_T_8[23:0], 8'h0}; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _shiftl_T_17 = _shiftl_T_15 & 32'hff00ff00; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _shiftl_T_18 = _shiftl_T_13 | _shiftl_T_17; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _GEN_7 = {{4'd0}, _shiftl_T_18[31:4]}; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _shiftl_T_23 = _GEN_7 & 32'hf0f0f0f; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _shiftl_T_25 = {_shiftl_T_18[27:0], 4'h0}; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _shiftl_T_27 = _shiftl_T_25 & 32'hf0f0f0f0; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _shiftl_T_28 = _shiftl_T_23 | _shiftl_T_27; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _GEN_8 = {{2'd0}, _shiftl_T_28[31:2]}; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _shiftl_T_33 = _GEN_8 & 32'h33333333; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _shiftl_T_35 = {_shiftl_T_28[29:0], 2'h0}; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _shiftl_T_37 = _shiftl_T_35 & 32'hcccccccc; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _shiftl_T_38 = _shiftl_T_33 | _shiftl_T_37; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _GEN_9 = {{1'd0}, _shiftl_T_38[31:1]}; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _shiftl_T_43 = _GEN_9 & 32'h55555555; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _shiftl_T_45 = {_shiftl_T_38[30:0], 1'h0}; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] _shiftl_T_47 = _shiftl_T_45 & 32'haaaaaaaa; // @[src/main/scala/mini/Alu.scala 71:23]
  wire [31:0] shiftl = _shiftl_T_43 | _shiftl_T_47; // @[src/main/scala/mini/Alu.scala 71:23]
  wire  _out_T_2 = io_alu_op == 4'h0 | io_alu_op == 4'h1; // @[src/main/scala/mini/Alu.scala 75:29]
  wire  _out_T_5 = io_alu_op == 4'h5 | io_alu_op == 4'h7; // @[src/main/scala/mini/Alu.scala 78:31]
  wire  _out_T_8 = io_alu_op == 4'h9 | io_alu_op == 4'h8; // @[src/main/scala/mini/Alu.scala 81:33]
  wire  _out_T_9 = io_alu_op == 4'h6; // @[src/main/scala/mini/Alu.scala 84:23]
  wire  _out_T_10 = io_alu_op == 4'h2; // @[src/main/scala/mini/Alu.scala 87:25]
  wire [31:0] _out_T_11 = io_A & io_B; // @[src/main/scala/mini/Alu.scala 88:20]
  wire  _out_T_12 = io_alu_op == 4'h3; // @[src/main/scala/mini/Alu.scala 90:27]
  wire [31:0] _out_T_13 = io_A | io_B; // @[src/main/scala/mini/Alu.scala 91:22]
  wire [31:0] _out_T_15 = io_A ^ io_B; // @[src/main/scala/mini/Alu.scala 92:49]
  wire [31:0] _out_T_17 = io_alu_op == 4'ha ? io_A : io_B; // @[src/main/scala/mini/Alu.scala 92:60]
  wire [31:0] _out_T_18 = io_alu_op == 4'h4 ? _out_T_15 : _out_T_17; // @[src/main/scala/mini/Alu.scala 92:20]
  wire [31:0] _out_T_19 = _out_T_12 ? _out_T_13 : _out_T_18; // @[src/main/scala/mini/Alu.scala 89:18]
  wire [31:0] _out_T_20 = _out_T_10 ? _out_T_11 : _out_T_19; // @[src/main/scala/mini/Alu.scala 86:16]
  wire [31:0] _out_T_21 = _out_T_9 ? shiftl : _out_T_20; // @[src/main/scala/mini/Alu.scala 83:14]
  wire [31:0] _out_T_22 = _out_T_8 ? shiftr : _out_T_21; // @[src/main/scala/mini/Alu.scala 80:12]
  wire [31:0] _out_T_23 = _out_T_5 ? {{31'd0}, cmp} : _out_T_22; // @[src/main/scala/mini/Alu.scala 77:10]
  assign io_out = _out_T_2 ? sum : _out_T_23; // @[src/main/scala/mini/Alu.scala 74:8]
  assign io_sum = io_A + _sum_T_3; // @[src/main/scala/mini/Alu.scala 65:18]
endmodule
module ImmGenWire(
  input  [31:0] io_inst, // @[src/main/scala/mini/ImmGen.scala 21:14]
  input  [2:0]  io_sel, // @[src/main/scala/mini/ImmGen.scala 21:14]
  output [31:0] io_out // @[src/main/scala/mini/ImmGen.scala 21:14]
);
  wire [11:0] Iimm = io_inst[31:20]; // @[src/main/scala/mini/ImmGen.scala 22:30]
  wire [11:0] Simm = {io_inst[31:25],io_inst[11:7]}; // @[src/main/scala/mini/ImmGen.scala 23:51]
  wire [12:0] Bimm = {io_inst[31],io_inst[7],io_inst[30:25],io_inst[11:8],1'h0}; // @[src/main/scala/mini/ImmGen.scala 24:86]
  wire [31:0] Uimm = {io_inst[31:12],12'h0}; // @[src/main/scala/mini/ImmGen.scala 25:46]
  wire [20:0] Jimm = {io_inst[31],io_inst[19:12],io_inst[20],io_inst[30:25],io_inst[24:21],1'h0}; // @[src/main/scala/mini/ImmGen.scala 26:105]
  wire [5:0] Zimm = {1'b0,$signed(io_inst[19:15])}; // @[src/main/scala/mini/ImmGen.scala 27:30]
  wire [11:0] _io_out_T_1 = $signed(Iimm) & -12'sh2; // @[src/main/scala/mini/ImmGen.scala 29:36]
  wire [11:0] _io_out_T_3 = 3'h1 == io_sel ? $signed(Iimm) : $signed(_io_out_T_1); // @[src/main/scala/mini/ImmGen.scala 29:45]
  wire [11:0] _io_out_T_5 = 3'h2 == io_sel ? $signed(Simm) : $signed(_io_out_T_3); // @[src/main/scala/mini/ImmGen.scala 29:45]
  wire [12:0] _io_out_T_7 = 3'h5 == io_sel ? $signed(Bimm) : $signed({{1{_io_out_T_5[11]}},_io_out_T_5}); // @[src/main/scala/mini/ImmGen.scala 29:45]
  wire [31:0] _io_out_T_9 = 3'h3 == io_sel ? $signed(Uimm) : $signed({{19{_io_out_T_7[12]}},_io_out_T_7}); // @[src/main/scala/mini/ImmGen.scala 29:45]
  wire [31:0] _io_out_T_11 = 3'h4 == io_sel ? $signed({{11{Jimm[20]}},Jimm}) : $signed(_io_out_T_9); // @[src/main/scala/mini/ImmGen.scala 29:45]
  assign io_out = 3'h6 == io_sel ? $signed({{26{Zimm[5]}},Zimm}) : $signed(_io_out_T_11); // @[src/main/scala/mini/ImmGen.scala 31:5]
endmodule
module BrCondArea(
  input  [31:0] io_rs1, // @[src/main/scala/mini/BrCond.scala 38:14]
  input  [31:0] io_rs2, // @[src/main/scala/mini/BrCond.scala 38:14]
  input  [2:0]  io_br_type, // @[src/main/scala/mini/BrCond.scala 38:14]
  output        io_taken // @[src/main/scala/mini/BrCond.scala 38:14]
);
  wire [31:0] diff = io_rs1 - io_rs2; // @[src/main/scala/mini/BrCond.scala 39:21]
  wire  neq = |diff; // @[src/main/scala/mini/BrCond.scala 40:18]
  wire  eq = ~neq; // @[src/main/scala/mini/BrCond.scala 41:12]
  wire  isSameSign = io_rs1[31] == io_rs2[31]; // @[src/main/scala/mini/BrCond.scala 42:37]
  wire  lt = isSameSign ? diff[31] : io_rs1[31]; // @[src/main/scala/mini/BrCond.scala 43:15]
  wire  ltu = isSameSign ? diff[31] : io_rs2[31]; // @[src/main/scala/mini/BrCond.scala 44:16]
  wire  ge = ~lt; // @[src/main/scala/mini/BrCond.scala 45:12]
  wire  geu = ~ltu; // @[src/main/scala/mini/BrCond.scala 46:13]
  wire  _io_taken_T_3 = io_br_type == 3'h6 & neq; // @[src/main/scala/mini/BrCond.scala 49:31]
  wire  _io_taken_T_4 = io_br_type == 3'h3 & eq | _io_taken_T_3; // @[src/main/scala/mini/BrCond.scala 48:36]
  wire  _io_taken_T_6 = io_br_type == 3'h2 & lt; // @[src/main/scala/mini/BrCond.scala 50:31]
  wire  _io_taken_T_7 = _io_taken_T_4 | _io_taken_T_6; // @[src/main/scala/mini/BrCond.scala 49:39]
  wire  _io_taken_T_9 = io_br_type == 3'h5 & ge; // @[src/main/scala/mini/BrCond.scala 51:31]
  wire  _io_taken_T_10 = _io_taken_T_7 | _io_taken_T_9; // @[src/main/scala/mini/BrCond.scala 50:38]
  wire  _io_taken_T_12 = io_br_type == 3'h1 & ltu; // @[src/main/scala/mini/BrCond.scala 52:32]
  wire  _io_taken_T_13 = _io_taken_T_10 | _io_taken_T_12; // @[src/main/scala/mini/BrCond.scala 51:38]
  wire  _io_taken_T_15 = io_br_type == 3'h4 & geu; // @[src/main/scala/mini/BrCond.scala 53:32]
  assign io_taken = _io_taken_T_13 | _io_taken_T_15; // @[src/main/scala/mini/BrCond.scala 52:40]
endmodule
module RiscvTrans(
  input  [31:0] io_inst, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input         io_valid, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output        io_mem_read_valid, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_mem_read_addr, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [5:0]  io_mem_read_memWidth, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_mem_read_data, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output        io_mem_write_valid, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_mem_write_addr, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [5:0]  io_mem_write_memWidth, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_mem_write_data, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_0, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_1, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_2, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_3, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_4, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_5, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_6, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_7, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_8, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_9, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_10, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_11, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_12, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_13, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_14, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_15, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_16, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_17, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_18, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_19, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_20, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_21, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_22, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_23, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_24, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_25, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_26, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_27, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_28, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_29, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_30, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_reg_31, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_pc, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_privilege_csr_misa, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_privilege_csr_mtvec, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_privilege_csr_medeleg, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_privilege_csr_mcause, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_privilege_csr_scause, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [31:0] io_now_privilege_csr_stvec, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [7:0]  io_now_privilege_csr_MXLEN, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  input  [1:0]  io_now_privilege_internal_privilegeMode, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_0, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_1, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_2, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_3, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_4, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_5, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_6, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_7, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_8, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_9, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_10, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_11, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_12, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_13, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_14, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_15, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_16, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_17, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_18, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_19, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_20, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_21, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_22, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_23, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_24, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_25, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_26, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_27, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_28, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_29, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_30, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_reg_31, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_pc, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_privilege_csr_misa, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_privilege_csr_mtvec, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_privilege_csr_medeleg, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_privilege_csr_mcause, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_privilege_csr_scause, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_next_privilege_csr_stvec, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [7:0]  io_next_privilege_csr_MXLEN, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [1:0]  io_next_privilege_internal_privilegeMode, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output        io_event_valid, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_event_cause, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_event_exceptionPC, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
  output [31:0] io_event_exceptionInst // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 134:14]
);
  wire [31:0] inst = io_valid ? io_inst : 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 172:16 153:21]
  wire [31:0] _T_426 = inst & 32'h707f; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_427 = 32'h5003 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire [31:0] _T_587 = inst & 32'hfe00707f; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_588 = 32'h2007033 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_581 = 32'h2006033 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_574 = 32'h2005033 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_567 = 32'h2004033 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_560 = 32'h2003033 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_553 = 32'h2002033 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_546 = 32'h2001033 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_539 = 32'h2000033 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_533 = 32'hf == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_524 = 32'h73 == inst; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_518 = 32'h100073 == inst; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_494 = 32'h2023 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_470 = 32'h1023 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_447 = 32'h23 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_408 = 32'h4003 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_388 = 32'h2003 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_368 = 32'h1003 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_348 = 32'h3 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_321 = 32'h7063 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_292 = 32'h5063 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_265 = 32'h6063 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_236 = 32'h4063 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_209 = 32'h1063 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_182 = 32'h63 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_157 = 32'h67 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_126 = 32'h40005033 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_119 = 32'h40000033 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_112 = 32'h5033 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_105 = 32'h1033 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_98 = 32'h4033 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_91 = 32'h6033 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_84 = 32'h7033 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_77 = 32'h3033 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_70 = 32'h2033 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_63 = 32'h33 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_49 = 32'h40005013 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 29:12]
  wire  _T_43 = 32'h5013 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 29:12]
  wire  _T_37 = 32'h1013 == _T_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 29:12]
  wire  _T_31 = 32'h4013 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_25 = 32'h6013 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_19 = 32'h7013 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_13 = 32'h3013 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_7 = 32'h2013 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_1 = 32'h13 == _T_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire [4:0] _GEN_98 = _T_1 ? inst[19:15] : 5'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 19:24]
  wire [4:0] _GEN_174 = _T_7 ? inst[19:15] : _GEN_98; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_250 = _T_13 ? inst[19:15] : _GEN_174; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_326 = _T_19 ? inst[19:15] : _GEN_250; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_402 = _T_25 ? inst[19:15] : _GEN_326; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_478 = _T_31 ? inst[19:15] : _GEN_402; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_554 = _T_37 ? inst[19:15] : _GEN_478; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_630 = _T_43 ? inst[19:15] : _GEN_554; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_706 = _T_49 ? inst[19:15] : _GEN_630; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_959 = _T_63 ? inst[19:15] : _GEN_706; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1037 = _T_70 ? inst[19:15] : _GEN_959; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1115 = _T_77 ? inst[19:15] : _GEN_1037; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1193 = _T_84 ? inst[19:15] : _GEN_1115; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1271 = _T_91 ? inst[19:15] : _GEN_1193; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1349 = _T_98 ? inst[19:15] : _GEN_1271; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1427 = _T_105 ? inst[19:15] : _GEN_1349; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1505 = _T_112 ? inst[19:15] : _GEN_1427; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1583 = _T_119 ? inst[19:15] : _GEN_1505; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1661 = _T_126 ? inst[19:15] : _GEN_1583; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1898 = _T_157 ? inst[19:15] : _GEN_1661; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1959 = _T_182 ? inst[19:15] : _GEN_1898; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 213:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1988 = _T_209 ? inst[19:15] : _GEN_1959; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 225:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2017 = _T_236 ? inst[19:15] : _GEN_1988; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 238:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2046 = _T_265 ? inst[19:15] : _GEN_2017; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 250:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2075 = _T_292 ? inst[19:15] : _GEN_2046; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 263:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2104 = _T_321 ? inst[19:15] : _GEN_2075; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 275:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2190 = _T_348 ? inst[19:15] : _GEN_2104; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2308 = _T_368 ? inst[19:15] : _GEN_2190; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2426 = _T_388 ? inst[19:15] : _GEN_2308; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2509 = _T_408 ? inst[19:15] : _GEN_2426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2627 = _T_427 ? inst[19:15] : _GEN_2509; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2679 = _T_447 ? inst[19:15] : _GEN_2627; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 328:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2701 = _T_470 ? inst[19:15] : _GEN_2679; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 329:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2723 = _T_494 ? inst[19:15] : _GEN_2701; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 338:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2738 = _T_518 ? inst[19:15] : _GEN_2723; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 347:24 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2756 = _T_524 ? inst[19:15] : _GEN_2738; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 353:23 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2767 = _T_533 ? inst[19:15] : _GEN_2756; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 361:23 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2807 = _T_539 ? inst[19:15] : _GEN_2767; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2878 = _T_546 ? inst[19:15] : _GEN_2807; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2949 = _T_553 ? inst[19:15] : _GEN_2878; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_3020 = _T_560 ? inst[19:15] : _GEN_2949; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_3091 = _T_567 ? inst[19:15] : _GEN_3020; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_3162 = _T_574 ? inst[19:15] : _GEN_3091; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_3233 = _T_581 ? inst[19:15] : _GEN_3162; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_3304 = _T_588 ? inst[19:15] : _GEN_3233; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] rs1 = io_valid ? _GEN_3304 : 5'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 19:24]
  wire [31:0] _GEN_1 = 5'h1 == rs1 ? io_now_reg_1 : io_now_reg_0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_2 = 5'h2 == rs1 ? io_now_reg_2 : _GEN_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_3 = 5'h3 == rs1 ? io_now_reg_3 : _GEN_2; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_4 = 5'h4 == rs1 ? io_now_reg_4 : _GEN_3; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_5 = 5'h5 == rs1 ? io_now_reg_5 : _GEN_4; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_6 = 5'h6 == rs1 ? io_now_reg_6 : _GEN_5; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_7 = 5'h7 == rs1 ? io_now_reg_7 : _GEN_6; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_8 = 5'h8 == rs1 ? io_now_reg_8 : _GEN_7; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_9 = 5'h9 == rs1 ? io_now_reg_9 : _GEN_8; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_10 = 5'ha == rs1 ? io_now_reg_10 : _GEN_9; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_11 = 5'hb == rs1 ? io_now_reg_11 : _GEN_10; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_12 = 5'hc == rs1 ? io_now_reg_12 : _GEN_11; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_13 = 5'hd == rs1 ? io_now_reg_13 : _GEN_12; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_14 = 5'he == rs1 ? io_now_reg_14 : _GEN_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_15 = 5'hf == rs1 ? io_now_reg_15 : _GEN_14; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_16 = 5'h10 == rs1 ? io_now_reg_16 : _GEN_15; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_17 = 5'h11 == rs1 ? io_now_reg_17 : _GEN_16; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_18 = 5'h12 == rs1 ? io_now_reg_18 : _GEN_17; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_19 = 5'h13 == rs1 ? io_now_reg_19 : _GEN_18; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_20 = 5'h14 == rs1 ? io_now_reg_20 : _GEN_19; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_21 = 5'h15 == rs1 ? io_now_reg_21 : _GEN_20; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_22 = 5'h16 == rs1 ? io_now_reg_22 : _GEN_21; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_23 = 5'h17 == rs1 ? io_now_reg_23 : _GEN_22; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_24 = 5'h18 == rs1 ? io_now_reg_24 : _GEN_23; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_25 = 5'h19 == rs1 ? io_now_reg_25 : _GEN_24; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_26 = 5'h1a == rs1 ? io_now_reg_26 : _GEN_25; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_27 = 5'h1b == rs1 ? io_now_reg_27 : _GEN_26; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_28 = 5'h1c == rs1 ? io_now_reg_28 : _GEN_27; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_29 = 5'h1d == rs1 ? io_now_reg_29 : _GEN_28; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_30 = 5'h1e == rs1 ? io_now_reg_30 : _GEN_29; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [31:0] _GEN_31 = 5'h1f == rs1 ? io_now_reg_31 : _GEN_30; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{81,81}]
  wire [11:0] _GEN_97 = _T_1 ? inst[31:20] : 12'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 38:27]
  wire [11:0] _GEN_173 = _T_7 ? inst[31:20] : _GEN_97; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [11:0] _GEN_249 = _T_13 ? inst[31:20] : _GEN_173; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [11:0] _GEN_325 = _T_19 ? inst[31:20] : _GEN_249; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [11:0] _GEN_401 = _T_25 ? inst[31:20] : _GEN_325; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [11:0] _GEN_477 = _T_31 ? inst[31:20] : _GEN_401; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [11:0] _GEN_553 = _T_37 ? inst[31:20] : _GEN_477; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [11:0] _GEN_629 = _T_43 ? inst[31:20] : _GEN_553; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [11:0] _GEN_705 = _T_49 ? inst[31:20] : _GEN_629; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [11:0] _GEN_1897 = _T_157 ? inst[31:20] : _GEN_705; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [11:0] _GEN_2189 = _T_348 ? inst[31:20] : _GEN_1897; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [11:0] _GEN_2307 = _T_368 ? inst[31:20] : _GEN_2189; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [11:0] _GEN_2425 = _T_388 ? inst[31:20] : _GEN_2307; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [11:0] _GEN_2508 = _T_408 ? inst[31:20] : _GEN_2425; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [11:0] _GEN_2626 = _T_427 ? inst[31:20] : _GEN_2508; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [11:0] _GEN_2737 = _T_518 ? inst[31:20] : _GEN_2626; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 347:24 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [11:0] _GEN_2755 = _T_524 ? inst[31:20] : _GEN_2737; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 353:23 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [11:0] _GEN_2766 = _T_533 ? inst[31:20] : _GEN_2755; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 361:23 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [11:0] imm_11_0 = io_valid ? _GEN_2766 : 12'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 38:27]
  wire  imm_signBit_29 = imm_11_0[11]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 9:20]
  wire [19:0] _imm_T_96 = imm_signBit_29 ? 20'hfffff : 20'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 13:15]
  wire [31:0] _imm_T_97 = {_imm_T_96,imm_11_0}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 13:10]
  wire [6:0] _GEN_2677 = _T_447 ? inst[31:25] : 7'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 328:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 39:27]
  wire [6:0] _GEN_2699 = _T_470 ? inst[31:25] : _GEN_2677; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 329:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [6:0] _GEN_2721 = _T_494 ? inst[31:25] : _GEN_2699; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 338:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [6:0] imm_11_5 = io_valid ? _GEN_2721 : 7'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 39:27]
  wire [4:0] _GEN_2681 = _T_447 ? inst[11:7] : 5'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 328:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 39:62]
  wire [4:0] _GEN_2703 = _T_470 ? inst[11:7] : _GEN_2681; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 329:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2725 = _T_494 ? inst[11:7] : _GEN_2703; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 338:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] imm_4_0 = io_valid ? _GEN_2725 : 5'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 39:62]
  wire [11:0] _imm_T_85 = {imm_11_5,imm_4_0}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 49:141]
  wire  imm_signBit_26 = _imm_T_85[11]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 9:20]
  wire [19:0] _imm_T_87 = imm_signBit_26 ? 20'hfffff : 20'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 13:15]
  wire [31:0] _imm_T_88 = {_imm_T_87,imm_11_5,imm_4_0}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 13:10]
  wire  _GEN_1956 = _T_182 & inst[31]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 213:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 40:27]
  wire  _GEN_1985 = _T_209 ? inst[31] : _GEN_1956; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 225:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire  _GEN_2014 = _T_236 ? inst[31] : _GEN_1985; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 238:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire  _GEN_2043 = _T_265 ? inst[31] : _GEN_2014; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 250:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire  _GEN_2072 = _T_292 ? inst[31] : _GEN_2043; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 263:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire  _GEN_2101 = _T_321 ? inst[31] : _GEN_2072; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 275:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire  imm_12 = io_valid & _GEN_2101; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 40:27]
  wire [31:0] _T_132 = inst & 32'h7f; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_133 = 32'h6f == _T_132; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _GEN_1779 = _T_133 & inst[20]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 40:132]
  wire  _GEN_1962 = _T_182 ? inst[7] : _GEN_1779; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 213:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire  _GEN_1991 = _T_209 ? inst[7] : _GEN_1962; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 225:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire  _GEN_2020 = _T_236 ? inst[7] : _GEN_1991; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 238:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire  _GEN_2049 = _T_265 ? inst[7] : _GEN_2020; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 250:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire  _GEN_2078 = _T_292 ? inst[7] : _GEN_2049; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 263:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire  _GEN_2107 = _T_321 ? inst[7] : _GEN_2078; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 275:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire  imm_11 = io_valid & _GEN_2107; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 40:132]
  wire [5:0] _GEN_1957 = _T_182 ? inst[30:25] : 6'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 213:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 40:62]
  wire [5:0] _GEN_1986 = _T_209 ? inst[30:25] : _GEN_1957; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 225:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [5:0] _GEN_2015 = _T_236 ? inst[30:25] : _GEN_1986; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 238:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [5:0] _GEN_2044 = _T_265 ? inst[30:25] : _GEN_2015; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 250:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [5:0] _GEN_2073 = _T_292 ? inst[30:25] : _GEN_2044; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 263:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [5:0] _GEN_2102 = _T_321 ? inst[30:25] : _GEN_2073; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 275:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [5:0] imm_10_5 = io_valid ? _GEN_2102 : 6'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 40:62]
  wire [3:0] _GEN_1961 = _T_182 ? inst[11:8] : 4'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 213:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 40:99]
  wire [3:0] _GEN_1990 = _T_209 ? inst[11:8] : _GEN_1961; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 225:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [3:0] _GEN_2019 = _T_236 ? inst[11:8] : _GEN_1990; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 238:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [3:0] _GEN_2048 = _T_265 ? inst[11:8] : _GEN_2019; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 250:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [3:0] _GEN_2077 = _T_292 ? inst[11:8] : _GEN_2048; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 263:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [3:0] _GEN_2106 = _T_321 ? inst[11:8] : _GEN_2077; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 275:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [3:0] imm_4_1 = io_valid ? _GEN_2106 : 4'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 40:99]
  wire [12:0] _imm_T_58 = {imm_12,imm_11,imm_10_5,imm_4_1,1'h0}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 50:141]
  wire  imm_signBit_18 = _imm_T_58[12]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 9:20]
  wire [18:0] _imm_T_60 = imm_signBit_18 ? 19'h7ffff : 19'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 13:15]
  wire [31:0] _imm_T_61 = {_imm_T_60,imm_12,imm_11,imm_10_5,imm_4_1,1'h0}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 13:10]
  wire  _GEN_1777 = _T_133 & inst[31]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 42:27]
  wire  imm_20 = io_valid & _GEN_1777; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 42:27]
  wire [7:0] _GEN_1780 = _T_133 ? inst[19:12] : 8'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 42:99]
  wire [7:0] imm_19_12 = io_valid ? _GEN_1780 : 8'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 42:99]
  wire [9:0] _GEN_1778 = _T_133 ? inst[30:21] : 10'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 42:62]
  wire [9:0] imm_10_1 = io_valid ? _GEN_1778 : 10'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 42:62]
  wire [20:0] _imm_T_31 = {imm_20,imm_19_12,imm_11,imm_10_1,1'h0}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 52:141]
  wire  imm_signBit_11 = _imm_T_31[20]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 9:20]
  wire [10:0] _imm_T_33 = imm_signBit_11 ? 11'h7ff : 11'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 13:15]
  wire [31:0] _imm_T_34 = {_imm_T_33,imm_20,imm_19_12,imm_11,imm_10_1,1'h0}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 13:10]
  wire  _T_59 = 32'h17 == _T_132; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _T_55 = 32'h37 == _T_132; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire [19:0] _GEN_781 = _T_55 ? inst[31:12] : 20'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 41:27]
  wire [19:0] _GEN_853 = _T_59 ? inst[31:12] : _GEN_781; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [19:0] imm_31_12 = io_valid ? _GEN_853 : 20'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 41:27]
  wire [31:0] _imm_T_29 = {imm_31_12,12'h0}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 51:141]
  wire [31:0] _GEN_102 = _T_1 ? _imm_T_97 : 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 48:127 22:24]
  wire [31:0] _GEN_178 = _T_7 ? _imm_T_97 : _GEN_102; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 48:127]
  wire [31:0] _GEN_254 = _T_13 ? _imm_T_97 : _GEN_178; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 48:127]
  wire [31:0] _GEN_330 = _T_19 ? _imm_T_97 : _GEN_254; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 48:127]
  wire [31:0] _GEN_406 = _T_25 ? _imm_T_97 : _GEN_330; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 48:127]
  wire [31:0] _GEN_482 = _T_31 ? _imm_T_97 : _GEN_406; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 48:127]
  wire [31:0] _GEN_558 = _T_37 ? _imm_T_97 : _GEN_482; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 48:127]
  wire [31:0] _GEN_634 = _T_43 ? _imm_T_97 : _GEN_558; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 48:127]
  wire [31:0] _GEN_710 = _T_49 ? _imm_T_97 : _GEN_634; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 48:127]
  wire [31:0] _GEN_784 = _T_55 ? _imm_T_29 : _GEN_710; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 51:127]
  wire [31:0] _GEN_856 = _T_59 ? _imm_T_29 : _GEN_784; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 51:127]
  wire [31:0] _GEN_1783 = _T_133 ? _imm_T_34 : _GEN_856; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 52:127]
  wire [31:0] _GEN_1902 = _T_157 ? _imm_T_97 : _GEN_1783; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 48:127]
  wire [31:0] _GEN_1964 = _T_182 ? _imm_T_61 : _GEN_1902; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 213:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 50:127]
  wire [31:0] _GEN_1993 = _T_209 ? _imm_T_61 : _GEN_1964; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 225:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 50:127]
  wire [31:0] _GEN_2022 = _T_236 ? _imm_T_61 : _GEN_1993; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 238:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 50:127]
  wire [31:0] _GEN_2051 = _T_265 ? _imm_T_61 : _GEN_2022; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 250:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 50:127]
  wire [31:0] _GEN_2080 = _T_292 ? _imm_T_61 : _GEN_2051; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 263:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 50:127]
  wire [31:0] _GEN_2109 = _T_321 ? _imm_T_61 : _GEN_2080; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 275:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 50:127]
  wire [31:0] _GEN_2194 = _T_348 ? _imm_T_97 : _GEN_2109; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 48:127]
  wire [31:0] _GEN_2312 = _T_368 ? _imm_T_97 : _GEN_2194; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 48:127]
  wire [31:0] _GEN_2430 = _T_388 ? _imm_T_97 : _GEN_2312; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 48:127]
  wire [31:0] _GEN_2513 = _T_408 ? _imm_T_97 : _GEN_2430; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 48:127]
  wire [31:0] _GEN_2631 = _T_427 ? _imm_T_97 : _GEN_2513; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 48:127]
  wire [31:0] _GEN_2683 = _T_447 ? _imm_T_88 : _GEN_2631; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 328:20 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 49:127]
  wire [31:0] _GEN_2705 = _T_470 ? _imm_T_88 : _GEN_2683; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 329:20 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 49:127]
  wire [31:0] _GEN_2727 = _T_494 ? _imm_T_88 : _GEN_2705; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 338:20 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 49:127]
  wire [31:0] _GEN_2742 = _T_518 ? _imm_T_97 : _GEN_2727; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 347:24 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 48:127]
  wire [31:0] _GEN_2760 = _T_524 ? _imm_T_97 : _GEN_2742; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 353:23 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 48:127]
  wire [31:0] _GEN_2771 = _T_533 ? _imm_T_97 : _GEN_2760; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 361:23 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 48:127]
  wire [31:0] imm = io_valid ? _GEN_2771 : 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 22:24]
  wire [31:0] _T_433 = _GEN_31 + imm; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:47]
  wire  _T_437 = _T_433[1:0] == 2'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 125:32]
  wire  _T_435 = ~_T_433[0]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 124:29]
  wire [4:0] _GEN_958 = _T_63 ? inst[24:20] : 5'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 20:24]
  wire [4:0] _GEN_1036 = _T_70 ? inst[24:20] : _GEN_958; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1114 = _T_77 ? inst[24:20] : _GEN_1036; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1192 = _T_84 ? inst[24:20] : _GEN_1114; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1270 = _T_91 ? inst[24:20] : _GEN_1192; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1348 = _T_98 ? inst[24:20] : _GEN_1270; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1426 = _T_105 ? inst[24:20] : _GEN_1348; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1504 = _T_112 ? inst[24:20] : _GEN_1426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1582 = _T_119 ? inst[24:20] : _GEN_1504; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1660 = _T_126 ? inst[24:20] : _GEN_1582; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1958 = _T_182 ? inst[24:20] : _GEN_1660; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 213:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1987 = _T_209 ? inst[24:20] : _GEN_1958; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 225:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2016 = _T_236 ? inst[24:20] : _GEN_1987; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 238:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2045 = _T_265 ? inst[24:20] : _GEN_2016; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 250:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2074 = _T_292 ? inst[24:20] : _GEN_2045; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 263:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2103 = _T_321 ? inst[24:20] : _GEN_2074; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 275:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2678 = _T_447 ? inst[24:20] : _GEN_2103; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 328:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2700 = _T_470 ? inst[24:20] : _GEN_2678; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 329:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2722 = _T_494 ? inst[24:20] : _GEN_2700; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 338:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2806 = _T_539 ? inst[24:20] : _GEN_2722; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2877 = _T_546 ? inst[24:20] : _GEN_2806; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2948 = _T_553 ? inst[24:20] : _GEN_2877; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_3019 = _T_560 ? inst[24:20] : _GEN_2948; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_3090 = _T_567 ? inst[24:20] : _GEN_3019; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_3161 = _T_574 ? inst[24:20] : _GEN_3090; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_3232 = _T_581 ? inst[24:20] : _GEN_3161; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_3303 = _T_588 ? inst[24:20] : _GEN_3232; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] rs2 = io_valid ? _GEN_3303 : 5'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 20:24]
  wire  _GEN_2304 = _T_435 ? 1'h0 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 144:33]
  wire  _GEN_2350 = _T_368 & _GEN_2304; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire  _GEN_2422 = _T_437 ? _GEN_2350 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 144:33]
  wire  _GEN_2468 = _T_388 ? _GEN_2422 : _GEN_2350; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire  _GEN_2623 = _T_435 ? _GEN_2468 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 144:33]
  wire  _GEN_2669 = _T_427 ? _GEN_2623 : _GEN_2468; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire  exceptionVec_4 = io_valid & _GEN_2669; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 122:30]
  wire [4:0] _exceptionNO_T_4 = exceptionVec_4 ? 5'h4 : 5'h0; // @[src/main/scala/chisel3/util/Mux.scala 141:16]
  wire  _GEN_2712 = _T_470 & _GEN_2304; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 329:20]
  wire  _GEN_2718 = _T_437 ? _GEN_2712 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 340:55 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 144:33]
  wire  _GEN_2734 = _T_494 ? _GEN_2718 : _GEN_2712; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 338:20]
  wire  exceptionVec_6 = io_valid & _GEN_2734; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 122:30]
  wire [4:0] _exceptionNO_T_5 = exceptionVec_6 ? 5'h6 : _exceptionNO_T_4; // @[src/main/scala/chisel3/util/Mux.scala 141:16]
  wire  _T_529 = 2'h3 == io_now_privilege_internal_privilegeMode; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 355:52]
  wire  _GEN_2749 = 2'h1 == io_now_privilege_internal_privilegeMode ? 1'h0 : 2'h0 ==
    io_now_privilege_internal_privilegeMode; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 355:52 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 122:30]
  wire  _GEN_2753 = 2'h3 == io_now_privilege_internal_privilegeMode ? 1'h0 : _GEN_2749; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 355:52 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 122:30]
  wire  _GEN_2764 = _T_524 & _GEN_2753; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 353:23 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 122:30]
  wire  exceptionVec_8 = io_valid & _GEN_2764; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 122:30]
  wire [4:0] _exceptionNO_T_6 = exceptionVec_8 ? 5'h8 : _exceptionNO_T_5; // @[src/main/scala/chisel3/util/Mux.scala 141:16]
  wire  _GEN_2752 = 2'h3 == io_now_privilege_internal_privilegeMode ? 1'h0 : 2'h1 ==
    io_now_privilege_internal_privilegeMode; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 355:52 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 122:30]
  wire  _GEN_2763 = _T_524 & _GEN_2752; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 353:23 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 122:30]
  wire  exceptionVec_9 = io_valid & _GEN_2763; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 122:30]
  wire [4:0] _exceptionNO_T_7 = exceptionVec_9 ? 5'h9 : _exceptionNO_T_6; // @[src/main/scala/chisel3/util/Mux.scala 141:16]
  wire  _GEN_2761 = _T_524 & _T_529; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 353:23 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 122:30]
  wire  exceptionVec_11 = io_valid & _GEN_2761; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 122:30]
  wire [4:0] _exceptionNO_T_8 = exceptionVec_11 ? 5'hb : _exceptionNO_T_7; // @[src/main/scala/chisel3/util/Mux.scala 141:16]
  wire [31:0] _GEN_893 = 5'h1 == rs2 ? io_now_reg_1 : io_now_reg_0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_894 = 5'h2 == rs2 ? io_now_reg_2 : _GEN_893; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_895 = 5'h3 == rs2 ? io_now_reg_3 : _GEN_894; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_896 = 5'h4 == rs2 ? io_now_reg_4 : _GEN_895; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_897 = 5'h5 == rs2 ? io_now_reg_5 : _GEN_896; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_898 = 5'h6 == rs2 ? io_now_reg_6 : _GEN_897; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_899 = 5'h7 == rs2 ? io_now_reg_7 : _GEN_898; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_900 = 5'h8 == rs2 ? io_now_reg_8 : _GEN_899; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_901 = 5'h9 == rs2 ? io_now_reg_9 : _GEN_900; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_902 = 5'ha == rs2 ? io_now_reg_10 : _GEN_901; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_903 = 5'hb == rs2 ? io_now_reg_11 : _GEN_902; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_904 = 5'hc == rs2 ? io_now_reg_12 : _GEN_903; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_905 = 5'hd == rs2 ? io_now_reg_13 : _GEN_904; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_906 = 5'he == rs2 ? io_now_reg_14 : _GEN_905; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_907 = 5'hf == rs2 ? io_now_reg_15 : _GEN_906; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_908 = 5'h10 == rs2 ? io_now_reg_16 : _GEN_907; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_909 = 5'h11 == rs2 ? io_now_reg_17 : _GEN_908; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_910 = 5'h12 == rs2 ? io_now_reg_18 : _GEN_909; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_911 = 5'h13 == rs2 ? io_now_reg_19 : _GEN_910; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_912 = 5'h14 == rs2 ? io_now_reg_20 : _GEN_911; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_913 = 5'h15 == rs2 ? io_now_reg_21 : _GEN_912; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_914 = 5'h16 == rs2 ? io_now_reg_22 : _GEN_913; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_915 = 5'h17 == rs2 ? io_now_reg_23 : _GEN_914; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_916 = 5'h18 == rs2 ? io_now_reg_24 : _GEN_915; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_917 = 5'h19 == rs2 ? io_now_reg_25 : _GEN_916; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_918 = 5'h1a == rs2 ? io_now_reg_26 : _GEN_917; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_919 = 5'h1b == rs2 ? io_now_reg_27 : _GEN_918; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_920 = 5'h1c == rs2 ? io_now_reg_28 : _GEN_919; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_921 = 5'h1d == rs2 ? io_now_reg_29 : _GEN_920; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_922 = 5'h1e == rs2 ? io_now_reg_30 : _GEN_921; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [31:0] _GEN_923 = 5'h1f == rs2 ? io_now_reg_31 : _GEN_922; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{85,85}]
  wire [1:0] _T_332 = io_now_privilege_csr_misa[2] ? 2'h1 : 2'h2; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 132:72]
  wire [31:0] _T_334 = io_now_pc + imm; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 278:49]
  wire  _T_340 = _T_334[2:0] == 3'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 126:32]
  wire  _T_338 = _T_334[1:0] == 2'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 125:32]
  wire  _T_336 = ~_T_334[0]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 124:29]
  wire  _T_342 = 2'h1 == _T_332 ? _T_336 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 121:29]
  wire  _T_344 = 2'h2 == _T_332 ? _T_338 : _T_342; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 121:29]
  wire  _T_346 = 2'h3 == _T_332 ? _T_340 : _T_344; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 121:29]
  wire [31:0] _T_300 = 5'h1f == rs1 ? io_now_reg_31 : _GEN_30; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 265:25]
  wire [31:0] _T_301 = 5'h1f == rs2 ? io_now_reg_31 : _GEN_922; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 265:48]
  wire  _T_273 = _GEN_31 < _GEN_923; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 252:25]
  wire  _T_246 = $signed(_T_300) < $signed(_T_301); // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 240:32]
  wire [31:0] _T_168 = {_T_433[31:1],1'h0}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:43]
  wire  _T_174 = _T_168[2:0] == 3'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 126:32]
  wire  _T_172 = _T_168[1:0] == 2'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 125:32]
  wire  _T_170 = ~_T_168[0]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 124:29]
  wire  _T_176 = 2'h1 == _T_332 ? _T_170 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 121:29]
  wire  _T_178 = 2'h2 == _T_332 ? _T_172 : _T_176; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 121:29]
  wire  _T_180 = 2'h3 == _T_332 ? _T_174 : _T_178; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 121:29]
  wire  _GEN_1774 = _T_346 ? 1'h0 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 122:30 144:33]
  wire  _GEN_1822 = _T_133 & _GEN_1774; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 122:30]
  wire  _GEN_1894 = _T_180 ? _GEN_1822 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 144:33]
  wire  _GEN_1943 = _T_157 ? _GEN_1894 : _GEN_1822; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire  _GEN_1948 = _T_346 ? _GEN_1943 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 216:57 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 144:33]
  wire  _GEN_1953 = _GEN_31 == _GEN_923 ? _GEN_1948 : _GEN_1943; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 215:43]
  wire  _GEN_1972 = _T_182 ? _GEN_1953 : _GEN_1943; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 213:21]
  wire  _GEN_1977 = _T_346 ? _GEN_1972 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 228:57 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 144:33]
  wire  _GEN_1982 = _GEN_31 != _GEN_923 ? _GEN_1977 : _GEN_1972; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 227:43]
  wire  _GEN_2001 = _T_209 ? _GEN_1982 : _GEN_1972; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 225:21]
  wire  _GEN_2006 = _T_346 ? _GEN_2001 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 241:57 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 144:33]
  wire  _GEN_2011 = $signed(_T_300) < $signed(_T_301) ? _GEN_2006 : _GEN_2001; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 240:55]
  wire  _GEN_2030 = _T_236 ? _GEN_2011 : _GEN_2001; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 238:21]
  wire  _GEN_2035 = _T_346 ? _GEN_2030 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 253:57 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 144:33]
  wire  _GEN_2040 = _GEN_31 < _GEN_923 ? _GEN_2035 : _GEN_2030; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 252:41]
  wire  _GEN_2059 = _T_265 ? _GEN_2040 : _GEN_2030; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 250:22]
  wire  _GEN_2064 = _T_346 ? _GEN_2059 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 266:57 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 144:33]
  wire  _GEN_2069 = $signed(_T_300) >= $signed(_T_301) ? _GEN_2064 : _GEN_2059; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 265:56]
  wire  _GEN_2088 = _T_292 ? _GEN_2069 : _GEN_2059; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 263:21]
  wire  _GEN_2093 = _T_346 ? _GEN_2088 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 278:57 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 144:33]
  wire  _GEN_2098 = _GEN_31 >= _GEN_923 ? _GEN_2093 : _GEN_2088; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 277:42]
  wire  _GEN_2117 = _T_321 ? _GEN_2098 : _GEN_2088; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 275:22]
  wire  exceptionVec_0 = io_valid & _GEN_2117; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 122:30]
  wire [4:0] _exceptionNO_T_9 = exceptionVec_0 ? 5'h0 : _exceptionNO_T_8; // @[src/main/scala/chisel3/util/Mux.scala 141:16]
  wire  _GEN_96 = _T_1 ? 1'h0 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 125:24 128:24]
  wire  _GEN_172 = _T_7 ? 1'h0 : _GEN_96; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_248 = _T_13 ? 1'h0 : _GEN_172; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_324 = _T_19 ? 1'h0 : _GEN_248; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_400 = _T_25 ? 1'h0 : _GEN_324; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_476 = _T_31 ? 1'h0 : _GEN_400; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_552 = _T_37 ? 1'h0 : _GEN_476; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_628 = _T_43 ? 1'h0 : _GEN_552; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_704 = _T_49 ? 1'h0 : _GEN_628; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_780 = _T_55 ? 1'h0 : _GEN_704; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_852 = _T_59 ? 1'h0 : _GEN_780; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_956 = _T_63 ? 1'h0 : _GEN_852; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_1034 = _T_70 ? 1'h0 : _GEN_956; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_1112 = _T_77 ? 1'h0 : _GEN_1034; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_1190 = _T_84 ? 1'h0 : _GEN_1112; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_1268 = _T_91 ? 1'h0 : _GEN_1190; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_1346 = _T_98 ? 1'h0 : _GEN_1268; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_1424 = _T_105 ? 1'h0 : _GEN_1346; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_1502 = _T_112 ? 1'h0 : _GEN_1424; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_1580 = _T_119 ? 1'h0 : _GEN_1502; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_1658 = _T_126 ? 1'h0 : _GEN_1580; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_1776 = _T_133 ? 1'h0 : _GEN_1658; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_1896 = _T_157 ? 1'h0 : _GEN_1776; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_1955 = _T_182 ? 1'h0 : _GEN_1896; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 213:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_1984 = _T_209 ? 1'h0 : _GEN_1955; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 225:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_2013 = _T_236 ? 1'h0 : _GEN_1984; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 238:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_2042 = _T_265 ? 1'h0 : _GEN_2013; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 250:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_2071 = _T_292 ? 1'h0 : _GEN_2042; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 263:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_2100 = _T_321 ? 1'h0 : _GEN_2071; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 275:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_2188 = _T_348 ? 1'h0 : _GEN_2100; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_2306 = _T_368 ? 1'h0 : _GEN_2188; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_2424 = _T_388 ? 1'h0 : _GEN_2306; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_2507 = _T_408 ? 1'h0 : _GEN_2424; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_2625 = _T_427 ? 1'h0 : _GEN_2507; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_2676 = _T_447 ? 1'h0 : _GEN_2625; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 328:20 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_2698 = _T_470 ? 1'h0 : _GEN_2676; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 329:20 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_2720 = _T_494 ? 1'h0 : _GEN_2698; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 338:20 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_2736 = _T_518 ? 1'h0 : _GEN_2720; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 347:24 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_2754 = _T_524 ? 1'h0 : _GEN_2736; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 353:23 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_2765 = _T_533 ? 1'h0 : _GEN_2754; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 361:23 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_2804 = _T_539 ? 1'h0 : _GEN_2765; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_2875 = _T_546 ? 1'h0 : _GEN_2804; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_2946 = _T_553 ? 1'h0 : _GEN_2875; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_3017 = _T_560 ? 1'h0 : _GEN_2946; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_3088 = _T_567 ? 1'h0 : _GEN_3017; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_3159 = _T_574 ? 1'h0 : _GEN_3088; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_3230 = _T_581 ? 1'h0 : _GEN_3159; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  _GEN_3301 = _T_588 ? 1'h0 : _GEN_3230; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 128:24]
  wire  illegalInstruction = io_valid & _GEN_3301; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 119:36]
  wire  exceptionVec_2 = io_valid & illegalInstruction; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 122:30]
  wire [4:0] _exceptionNO_T_10 = exceptionVec_2 ? 5'h2 : _exceptionNO_T_9; // @[src/main/scala/chisel3/util/Mux.scala 141:16]
  wire  exceptionVec_3 = io_valid & _T_518; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 122:30]
  wire [4:0] exceptionNO = exceptionVec_3 ? 5'h3 : _exceptionNO_T_10; // @[src/main/scala/chisel3/util/Mux.scala 141:16]
  wire [4:0] _GEN_100 = _T_1 ? inst[11:7] : 5'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 17:24]
  wire [4:0] _GEN_176 = _T_7 ? inst[11:7] : _GEN_100; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_252 = _T_13 ? inst[11:7] : _GEN_176; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_328 = _T_19 ? inst[11:7] : _GEN_252; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_404 = _T_25 ? inst[11:7] : _GEN_328; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_480 = _T_31 ? inst[11:7] : _GEN_404; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_556 = _T_37 ? inst[11:7] : _GEN_480; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_632 = _T_43 ? inst[11:7] : _GEN_556; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_708 = _T_49 ? inst[11:7] : _GEN_632; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_782 = _T_55 ? inst[11:7] : _GEN_708; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_854 = _T_59 ? inst[11:7] : _GEN_782; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_961 = _T_63 ? inst[11:7] : _GEN_854; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1039 = _T_70 ? inst[11:7] : _GEN_961; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1117 = _T_77 ? inst[11:7] : _GEN_1039; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1195 = _T_84 ? inst[11:7] : _GEN_1117; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1273 = _T_91 ? inst[11:7] : _GEN_1195; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1351 = _T_98 ? inst[11:7] : _GEN_1273; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1429 = _T_105 ? inst[11:7] : _GEN_1351; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1507 = _T_112 ? inst[11:7] : _GEN_1429; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1585 = _T_119 ? inst[11:7] : _GEN_1507; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1663 = _T_126 ? inst[11:7] : _GEN_1585; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1781 = _T_133 ? inst[11:7] : _GEN_1663; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_1900 = _T_157 ? inst[11:7] : _GEN_1781; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2192 = _T_348 ? inst[11:7] : _GEN_1900; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2310 = _T_368 ? inst[11:7] : _GEN_2192; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2428 = _T_388 ? inst[11:7] : _GEN_2310; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2511 = _T_408 ? inst[11:7] : _GEN_2428; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2629 = _T_427 ? inst[11:7] : _GEN_2511; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2740 = _T_518 ? inst[11:7] : _GEN_2629; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 347:24 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2758 = _T_524 ? inst[11:7] : _GEN_2740; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 353:23 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2769 = _T_533 ? inst[11:7] : _GEN_2758; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 361:23 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2809 = _T_539 ? inst[11:7] : _GEN_2769; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2880 = _T_546 ? inst[11:7] : _GEN_2809; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_2951 = _T_553 ? inst[11:7] : _GEN_2880; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_3022 = _T_560 ? inst[11:7] : _GEN_2951; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_3093 = _T_567 ? inst[11:7] : _GEN_3022; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_3164 = _T_574 ? inst[11:7] : _GEN_3093; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_3235 = _T_581 ? inst[11:7] : _GEN_3164; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] _GEN_3306 = _T_588 ? inst[11:7] : _GEN_3235; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22 riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 31:14]
  wire [4:0] rd = io_valid ? _GEN_3306 : 5'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/CommonDecode.scala 17:24]
  wire [31:0] _GEN_33 = 5'h1 == rd ? _T_433 : io_now_reg_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_34 = 5'h2 == rd ? _T_433 : io_now_reg_2; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_35 = 5'h3 == rd ? _T_433 : io_now_reg_3; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_36 = 5'h4 == rd ? _T_433 : io_now_reg_4; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_37 = 5'h5 == rd ? _T_433 : io_now_reg_5; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_38 = 5'h6 == rd ? _T_433 : io_now_reg_6; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_39 = 5'h7 == rd ? _T_433 : io_now_reg_7; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_40 = 5'h8 == rd ? _T_433 : io_now_reg_8; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_41 = 5'h9 == rd ? _T_433 : io_now_reg_9; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_42 = 5'ha == rd ? _T_433 : io_now_reg_10; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_43 = 5'hb == rd ? _T_433 : io_now_reg_11; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_44 = 5'hc == rd ? _T_433 : io_now_reg_12; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_45 = 5'hd == rd ? _T_433 : io_now_reg_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_46 = 5'he == rd ? _T_433 : io_now_reg_14; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_47 = 5'hf == rd ? _T_433 : io_now_reg_15; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_48 = 5'h10 == rd ? _T_433 : io_now_reg_16; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_49 = 5'h11 == rd ? _T_433 : io_now_reg_17; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_50 = 5'h12 == rd ? _T_433 : io_now_reg_18; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_51 = 5'h13 == rd ? _T_433 : io_now_reg_19; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_52 = 5'h14 == rd ? _T_433 : io_now_reg_20; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_53 = 5'h15 == rd ? _T_433 : io_now_reg_21; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_54 = 5'h16 == rd ? _T_433 : io_now_reg_22; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_55 = 5'h17 == rd ? _T_433 : io_now_reg_23; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_56 = 5'h18 == rd ? _T_433 : io_now_reg_24; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_57 = 5'h19 == rd ? _T_433 : io_now_reg_25; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_58 = 5'h1a == rd ? _T_433 : io_now_reg_26; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_59 = 5'h1b == rd ? _T_433 : io_now_reg_27; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_60 = 5'h1c == rd ? _T_433 : io_now_reg_28; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_61 = 5'h1d == rd ? _T_433 : io_now_reg_29; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_62 = 5'h1e == rd ? _T_433 : io_now_reg_30; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_63 = 5'h1f == rd ? _T_433 : io_now_reg_31; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:{65,65} riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire  _next_reg_T_133 = _GEN_923 == 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 77:19]
  wire [31:0] _next_reg_T_132 = _GEN_31 % _GEN_923; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 75:15]
  wire [31:0] _next_reg_T_134 = _next_reg_T_133 ? _GEN_31 : _next_reg_T_132; // @[src/main/scala/chisel3/util/Mux.scala 141:16]
  wire [31:0] _next_reg_T_125 = 32'h0 - 32'h80000000; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 69:22]
  wire  _next_reg_T_129 = _GEN_31 == _next_reg_T_125 & _GEN_923 == 32'hffffffff; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 69:52]
  wire [31:0] _next_reg_T_121 = $signed(_T_300) % $signed(_T_301); // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 66:42]
  wire [31:0] _next_reg_T_130 = _next_reg_T_129 ? 32'h0 : _next_reg_T_121; // @[src/main/scala/chisel3/util/Mux.scala 141:16]
  wire [31:0] _next_reg_T_131 = _next_reg_T_133 ? _GEN_31 : _next_reg_T_130; // @[src/main/scala/chisel3/util/Mux.scala 141:16]
  wire [31:0] _next_reg_T_114 = _GEN_31 / _GEN_923; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 58:15]
  wire [31:0] _next_reg_T_117 = _next_reg_T_133 ? 32'hffffffff : _next_reg_T_114; // @[src/main/scala/chisel3/util/Mux.scala 141:16]
  wire [32:0] _next_reg_T_98 = $signed(_T_300) / $signed(_T_301); // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 49:23]
  wire [31:0] _next_reg_T_112 = _next_reg_T_129 ? _next_reg_T_125 : _next_reg_T_98[31:0]; // @[src/main/scala/chisel3/util/Mux.scala 141:16]
  wire [31:0] _next_reg_T_113 = _next_reg_T_133 ? 32'hffffffff : _next_reg_T_112; // @[src/main/scala/chisel3/util/Mux.scala 141:16]
  wire [63:0] _next_reg_T_94 = _GEN_31 * _GEN_923; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:65]
  wire [32:0] _next_reg_T_88 = {1'b0,$signed(_GEN_923)}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:72]
  wire [64:0] _next_reg_T_89 = $signed(_T_300) * $signed(_next_reg_T_88); // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:72]
  wire [63:0] _next_reg_T_92 = _next_reg_T_89[63:0]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:88]
  wire [63:0] _next_reg_T_85 = $signed(_T_300) * $signed(_T_301); // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:95]
  wire [4:0] next_reg_rOff_4 = {_T_433[1:0], 3'h0}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 50:48]
  wire [31:0] _next_reg_T_76 = io_mem_read_data >> next_reg_rOff_4; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 55:22]
  wire [63:0] _GEN_3508 = {{32'd0}, _next_reg_T_76}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 55:31]
  wire [63:0] _next_reg_T_77 = _GEN_3508 & 64'hffff; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 55:31]
  wire [31:0] _next_reg_T_79 = {16'h0,_next_reg_T_77[15:0]}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 21:10]
  wire [63:0] _next_reg_T_71 = _GEN_3508 & 64'hff; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 55:31]
  wire [31:0] _next_reg_T_73 = {24'h0,_next_reg_T_71[7:0]}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 21:10]
  wire [63:0] _next_reg_T_65 = _GEN_3508 & 64'hffffffff; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 55:31]
  wire  next_reg_signBit_1 = _next_reg_T_77[15]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 9:20]
  wire [15:0] _next_reg_T_60 = next_reg_signBit_1 ? 16'hffff : 16'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 13:15]
  wire [31:0] _next_reg_T_61 = {_next_reg_T_60,_next_reg_T_77[15:0]}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 13:10]
  wire  next_reg_signBit = _next_reg_T_71[7]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 9:20]
  wire [23:0] _next_reg_T_52 = next_reg_signBit ? 24'hffffff : 24'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 13:15]
  wire [31:0] _next_reg_T_53 = {_next_reg_T_52,_next_reg_T_71[7:0]}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 13:10]
  wire [31:0] _next_reg_T_45 = io_now_pc + 32'h4; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:37]
  wire [31:0] _next_reg_T_41 = $signed(_T_300) >>> _GEN_923[4:0]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:115]
  wire [31:0] _next_reg_T_37 = _GEN_31 - _GEN_923; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:84]
  wire [31:0] _next_reg_T_35 = _GEN_31 >> _GEN_923[4:0]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:84]
  wire [62:0] _GEN_0 = {{31'd0}, _GEN_31}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:84]
  wire [62:0] _next_reg_T_33 = _GEN_0 << _GEN_923[4:0]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:84]
  wire [31:0] _next_reg_T_31 = _GEN_31 ^ _GEN_923; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:84]
  wire [31:0] _next_reg_T_30 = _GEN_31 | _GEN_923; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:84]
  wire [31:0] _next_reg_T_29 = _GEN_31 & _GEN_923; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:84]
  wire [31:0] _next_reg_rd_25 = {{31'd0}, _T_273}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _next_reg_rd_23 = {{31'd0}, _T_246}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _next_reg_T_22 = _GEN_31 + _GEN_923; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:85]
  wire [31:0] _next_reg_T_18 = $signed(_T_300) >>> imm[4:0]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:102]
  wire [31:0] _next_reg_T_14 = _GEN_31 >> imm[4:0]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:80]
  wire [62:0] _GEN_32 = {{31'd0}, _GEN_31}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:80]
  wire [62:0] _next_reg_T_12 = _GEN_32 << imm[4:0]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:80]
  wire [31:0] _next_reg_T_10 = _GEN_31 ^ imm; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:80]
  wire [31:0] _next_reg_T_9 = _GEN_31 | imm; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:80]
  wire [31:0] _next_reg_T_8 = _GEN_31 & imm; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:80]
  wire [31:0] _next_reg_rd_3 = {{31'd0}, _GEN_31 < imm}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _next_reg_T_3 = io_valid ? _GEN_2771 : 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:98]
  wire [31:0] _next_reg_rd_1 = {{31'd0}, $signed(_T_300) < $signed(_next_reg_T_3)}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_106 = _T_1 ? _GEN_33 : io_now_reg_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_141 = 5'h1 == rd ? _next_reg_rd_1 : _GEN_106; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_182 = _T_7 ? _GEN_141 : _GEN_106; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_217 = 5'h1 == rd ? _next_reg_rd_3 : _GEN_182; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_258 = _T_13 ? _GEN_217 : _GEN_182; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_293 = 5'h1 == rd ? _next_reg_T_8 : _GEN_258; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_334 = _T_19 ? _GEN_293 : _GEN_258; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_369 = 5'h1 == rd ? _next_reg_T_9 : _GEN_334; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_410 = _T_25 ? _GEN_369 : _GEN_334; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_445 = 5'h1 == rd ? _next_reg_T_10 : _GEN_410; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_486 = _T_31 ? _GEN_445 : _GEN_410; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_521 = 5'h1 == rd ? _next_reg_T_12[31:0] : _GEN_486; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_562 = _T_37 ? _GEN_521 : _GEN_486; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_597 = 5'h1 == rd ? _next_reg_T_14 : _GEN_562; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_638 = _T_43 ? _GEN_597 : _GEN_562; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_673 = 5'h1 == rd ? _next_reg_T_18 : _GEN_638; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_714 = _T_49 ? _GEN_673 : _GEN_638; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_749 = 5'h1 == rd ? imm : _GEN_714; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_786 = _T_55 ? _GEN_749 : _GEN_714; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_821 = 5'h1 == rd ? _T_334 : _GEN_786; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_858 = _T_59 ? _GEN_821 : _GEN_786; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_925 = 5'h1 == rd ? _next_reg_T_22 : _GEN_858; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_968 = _T_63 ? _GEN_925 : _GEN_858; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1003 = 5'h1 == rd ? _next_reg_rd_23 : _GEN_968; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1046 = _T_70 ? _GEN_1003 : _GEN_968; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1081 = 5'h1 == rd ? _next_reg_rd_25 : _GEN_1046; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1124 = _T_77 ? _GEN_1081 : _GEN_1046; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1159 = 5'h1 == rd ? _next_reg_T_29 : _GEN_1124; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1202 = _T_84 ? _GEN_1159 : _GEN_1124; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1237 = 5'h1 == rd ? _next_reg_T_30 : _GEN_1202; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1280 = _T_91 ? _GEN_1237 : _GEN_1202; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1315 = 5'h1 == rd ? _next_reg_T_31 : _GEN_1280; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1358 = _T_98 ? _GEN_1315 : _GEN_1280; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1393 = 5'h1 == rd ? _next_reg_T_33[31:0] : _GEN_1358; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1436 = _T_105 ? _GEN_1393 : _GEN_1358; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1471 = 5'h1 == rd ? _next_reg_T_35 : _GEN_1436; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1514 = _T_112 ? _GEN_1471 : _GEN_1436; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1549 = 5'h1 == rd ? _next_reg_T_37 : _GEN_1514; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1592 = _T_119 ? _GEN_1549 : _GEN_1514; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1627 = 5'h1 == rd ? _next_reg_T_41 : _GEN_1592; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1670 = _T_126 ? _GEN_1627 : _GEN_1592; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1705 = 5'h1 == rd ? _next_reg_T_45 : _GEN_1670; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1739 = _T_346 ? _GEN_1705 : _GEN_1670; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1787 = _T_133 ? _GEN_1739 : _GEN_1670; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1825 = 5'h1 == rd ? _next_reg_T_45 : _GEN_1787; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1859 = _T_180 ? _GEN_1825 : _GEN_1787; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1908 = _T_157 ? _GEN_1859 : _GEN_1787; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2120 = 5'h1 == rd ? _next_reg_T_53 : _GEN_1908; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2201 = _T_348 ? _GEN_2120 : _GEN_1908; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2238 = 5'h1 == rd ? _next_reg_T_61 : _GEN_2201; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2273 = _T_435 ? _GEN_2238 : _GEN_2201; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2319 = _T_368 ? _GEN_2273 : _GEN_2201; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2356 = 5'h1 == rd ? _next_reg_T_65[31:0] : _GEN_2319; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2391 = _T_437 ? _GEN_2356 : _GEN_2319; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2437 = _T_388 ? _GEN_2391 : _GEN_2319; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2476 = 5'h1 == rd ? _next_reg_T_73 : _GEN_2437; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2522 = _T_408 ? _GEN_2476 : _GEN_2437; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2557 = 5'h1 == rd ? _next_reg_T_79 : _GEN_2522; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2592 = _T_435 ? _GEN_2557 : _GEN_2522; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2638 = _T_427 ? _GEN_2592 : _GEN_2522; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2773 = 5'h1 == rd ? _next_reg_T_94[31:0] : _GEN_2638; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2812 = _T_539 ? _GEN_2773 : _GEN_2638; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2844 = 5'h1 == rd ? _next_reg_T_85[63:32] : _GEN_2812; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2883 = _T_546 ? _GEN_2844 : _GEN_2812; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2915 = 5'h1 == rd ? _next_reg_T_92[63:32] : _GEN_2883; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2954 = _T_553 ? _GEN_2915 : _GEN_2883; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_2986 = 5'h1 == rd ? _next_reg_T_94[63:32] : _GEN_2954; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3025 = _T_560 ? _GEN_2986 : _GEN_2954; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3057 = 5'h1 == rd ? _next_reg_T_113 : _GEN_3025; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3096 = _T_567 ? _GEN_3057 : _GEN_3025; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3128 = 5'h1 == rd ? _next_reg_T_117 : _GEN_3096; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3167 = _T_574 ? _GEN_3128 : _GEN_3096; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3199 = 5'h1 == rd ? _next_reg_T_131 : _GEN_3167; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3238 = _T_581 ? _GEN_3199 : _GEN_3167; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3270 = 5'h1 == rd ? _next_reg_T_134 : _GEN_3238; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3309 = _T_588 ? _GEN_3270 : _GEN_3238; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_107 = _T_1 ? _GEN_34 : io_now_reg_2; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_142 = 5'h2 == rd ? _next_reg_rd_1 : _GEN_107; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_183 = _T_7 ? _GEN_142 : _GEN_107; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_218 = 5'h2 == rd ? _next_reg_rd_3 : _GEN_183; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_259 = _T_13 ? _GEN_218 : _GEN_183; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_294 = 5'h2 == rd ? _next_reg_T_8 : _GEN_259; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_335 = _T_19 ? _GEN_294 : _GEN_259; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_370 = 5'h2 == rd ? _next_reg_T_9 : _GEN_335; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_411 = _T_25 ? _GEN_370 : _GEN_335; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_446 = 5'h2 == rd ? _next_reg_T_10 : _GEN_411; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_487 = _T_31 ? _GEN_446 : _GEN_411; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_522 = 5'h2 == rd ? _next_reg_T_12[31:0] : _GEN_487; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_563 = _T_37 ? _GEN_522 : _GEN_487; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_598 = 5'h2 == rd ? _next_reg_T_14 : _GEN_563; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_639 = _T_43 ? _GEN_598 : _GEN_563; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_674 = 5'h2 == rd ? _next_reg_T_18 : _GEN_639; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_715 = _T_49 ? _GEN_674 : _GEN_639; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_750 = 5'h2 == rd ? imm : _GEN_715; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_787 = _T_55 ? _GEN_750 : _GEN_715; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_822 = 5'h2 == rd ? _T_334 : _GEN_787; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_859 = _T_59 ? _GEN_822 : _GEN_787; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_926 = 5'h2 == rd ? _next_reg_T_22 : _GEN_859; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_969 = _T_63 ? _GEN_926 : _GEN_859; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1004 = 5'h2 == rd ? _next_reg_rd_23 : _GEN_969; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1047 = _T_70 ? _GEN_1004 : _GEN_969; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1082 = 5'h2 == rd ? _next_reg_rd_25 : _GEN_1047; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1125 = _T_77 ? _GEN_1082 : _GEN_1047; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1160 = 5'h2 == rd ? _next_reg_T_29 : _GEN_1125; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1203 = _T_84 ? _GEN_1160 : _GEN_1125; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1238 = 5'h2 == rd ? _next_reg_T_30 : _GEN_1203; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1281 = _T_91 ? _GEN_1238 : _GEN_1203; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1316 = 5'h2 == rd ? _next_reg_T_31 : _GEN_1281; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1359 = _T_98 ? _GEN_1316 : _GEN_1281; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1394 = 5'h2 == rd ? _next_reg_T_33[31:0] : _GEN_1359; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1437 = _T_105 ? _GEN_1394 : _GEN_1359; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1472 = 5'h2 == rd ? _next_reg_T_35 : _GEN_1437; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1515 = _T_112 ? _GEN_1472 : _GEN_1437; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1550 = 5'h2 == rd ? _next_reg_T_37 : _GEN_1515; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1593 = _T_119 ? _GEN_1550 : _GEN_1515; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1628 = 5'h2 == rd ? _next_reg_T_41 : _GEN_1593; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1671 = _T_126 ? _GEN_1628 : _GEN_1593; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1706 = 5'h2 == rd ? _next_reg_T_45 : _GEN_1671; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1740 = _T_346 ? _GEN_1706 : _GEN_1671; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1788 = _T_133 ? _GEN_1740 : _GEN_1671; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1826 = 5'h2 == rd ? _next_reg_T_45 : _GEN_1788; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1860 = _T_180 ? _GEN_1826 : _GEN_1788; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1909 = _T_157 ? _GEN_1860 : _GEN_1788; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2121 = 5'h2 == rd ? _next_reg_T_53 : _GEN_1909; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2202 = _T_348 ? _GEN_2121 : _GEN_1909; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2239 = 5'h2 == rd ? _next_reg_T_61 : _GEN_2202; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2274 = _T_435 ? _GEN_2239 : _GEN_2202; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2320 = _T_368 ? _GEN_2274 : _GEN_2202; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2357 = 5'h2 == rd ? _next_reg_T_65[31:0] : _GEN_2320; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2392 = _T_437 ? _GEN_2357 : _GEN_2320; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2438 = _T_388 ? _GEN_2392 : _GEN_2320; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2477 = 5'h2 == rd ? _next_reg_T_73 : _GEN_2438; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2523 = _T_408 ? _GEN_2477 : _GEN_2438; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2558 = 5'h2 == rd ? _next_reg_T_79 : _GEN_2523; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2593 = _T_435 ? _GEN_2558 : _GEN_2523; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2639 = _T_427 ? _GEN_2593 : _GEN_2523; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2774 = 5'h2 == rd ? _next_reg_T_94[31:0] : _GEN_2639; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2813 = _T_539 ? _GEN_2774 : _GEN_2639; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2845 = 5'h2 == rd ? _next_reg_T_85[63:32] : _GEN_2813; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2884 = _T_546 ? _GEN_2845 : _GEN_2813; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2916 = 5'h2 == rd ? _next_reg_T_92[63:32] : _GEN_2884; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2955 = _T_553 ? _GEN_2916 : _GEN_2884; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_2987 = 5'h2 == rd ? _next_reg_T_94[63:32] : _GEN_2955; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3026 = _T_560 ? _GEN_2987 : _GEN_2955; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3058 = 5'h2 == rd ? _next_reg_T_113 : _GEN_3026; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3097 = _T_567 ? _GEN_3058 : _GEN_3026; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3129 = 5'h2 == rd ? _next_reg_T_117 : _GEN_3097; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3168 = _T_574 ? _GEN_3129 : _GEN_3097; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3200 = 5'h2 == rd ? _next_reg_T_131 : _GEN_3168; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3239 = _T_581 ? _GEN_3200 : _GEN_3168; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3271 = 5'h2 == rd ? _next_reg_T_134 : _GEN_3239; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3310 = _T_588 ? _GEN_3271 : _GEN_3239; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_108 = _T_1 ? _GEN_35 : io_now_reg_3; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_143 = 5'h3 == rd ? _next_reg_rd_1 : _GEN_108; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_184 = _T_7 ? _GEN_143 : _GEN_108; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_219 = 5'h3 == rd ? _next_reg_rd_3 : _GEN_184; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_260 = _T_13 ? _GEN_219 : _GEN_184; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_295 = 5'h3 == rd ? _next_reg_T_8 : _GEN_260; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_336 = _T_19 ? _GEN_295 : _GEN_260; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_371 = 5'h3 == rd ? _next_reg_T_9 : _GEN_336; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_412 = _T_25 ? _GEN_371 : _GEN_336; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_447 = 5'h3 == rd ? _next_reg_T_10 : _GEN_412; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_488 = _T_31 ? _GEN_447 : _GEN_412; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_523 = 5'h3 == rd ? _next_reg_T_12[31:0] : _GEN_488; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_564 = _T_37 ? _GEN_523 : _GEN_488; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_599 = 5'h3 == rd ? _next_reg_T_14 : _GEN_564; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_640 = _T_43 ? _GEN_599 : _GEN_564; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_675 = 5'h3 == rd ? _next_reg_T_18 : _GEN_640; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_716 = _T_49 ? _GEN_675 : _GEN_640; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_751 = 5'h3 == rd ? imm : _GEN_716; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_788 = _T_55 ? _GEN_751 : _GEN_716; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_823 = 5'h3 == rd ? _T_334 : _GEN_788; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_860 = _T_59 ? _GEN_823 : _GEN_788; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_927 = 5'h3 == rd ? _next_reg_T_22 : _GEN_860; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_970 = _T_63 ? _GEN_927 : _GEN_860; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1005 = 5'h3 == rd ? _next_reg_rd_23 : _GEN_970; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1048 = _T_70 ? _GEN_1005 : _GEN_970; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1083 = 5'h3 == rd ? _next_reg_rd_25 : _GEN_1048; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1126 = _T_77 ? _GEN_1083 : _GEN_1048; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1161 = 5'h3 == rd ? _next_reg_T_29 : _GEN_1126; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1204 = _T_84 ? _GEN_1161 : _GEN_1126; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1239 = 5'h3 == rd ? _next_reg_T_30 : _GEN_1204; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1282 = _T_91 ? _GEN_1239 : _GEN_1204; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1317 = 5'h3 == rd ? _next_reg_T_31 : _GEN_1282; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1360 = _T_98 ? _GEN_1317 : _GEN_1282; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1395 = 5'h3 == rd ? _next_reg_T_33[31:0] : _GEN_1360; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1438 = _T_105 ? _GEN_1395 : _GEN_1360; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1473 = 5'h3 == rd ? _next_reg_T_35 : _GEN_1438; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1516 = _T_112 ? _GEN_1473 : _GEN_1438; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1551 = 5'h3 == rd ? _next_reg_T_37 : _GEN_1516; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1594 = _T_119 ? _GEN_1551 : _GEN_1516; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1629 = 5'h3 == rd ? _next_reg_T_41 : _GEN_1594; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1672 = _T_126 ? _GEN_1629 : _GEN_1594; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1707 = 5'h3 == rd ? _next_reg_T_45 : _GEN_1672; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1741 = _T_346 ? _GEN_1707 : _GEN_1672; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1789 = _T_133 ? _GEN_1741 : _GEN_1672; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1827 = 5'h3 == rd ? _next_reg_T_45 : _GEN_1789; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1861 = _T_180 ? _GEN_1827 : _GEN_1789; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1910 = _T_157 ? _GEN_1861 : _GEN_1789; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2122 = 5'h3 == rd ? _next_reg_T_53 : _GEN_1910; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2203 = _T_348 ? _GEN_2122 : _GEN_1910; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2240 = 5'h3 == rd ? _next_reg_T_61 : _GEN_2203; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2275 = _T_435 ? _GEN_2240 : _GEN_2203; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2321 = _T_368 ? _GEN_2275 : _GEN_2203; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2358 = 5'h3 == rd ? _next_reg_T_65[31:0] : _GEN_2321; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2393 = _T_437 ? _GEN_2358 : _GEN_2321; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2439 = _T_388 ? _GEN_2393 : _GEN_2321; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2478 = 5'h3 == rd ? _next_reg_T_73 : _GEN_2439; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2524 = _T_408 ? _GEN_2478 : _GEN_2439; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2559 = 5'h3 == rd ? _next_reg_T_79 : _GEN_2524; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2594 = _T_435 ? _GEN_2559 : _GEN_2524; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2640 = _T_427 ? _GEN_2594 : _GEN_2524; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2775 = 5'h3 == rd ? _next_reg_T_94[31:0] : _GEN_2640; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2814 = _T_539 ? _GEN_2775 : _GEN_2640; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2846 = 5'h3 == rd ? _next_reg_T_85[63:32] : _GEN_2814; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2885 = _T_546 ? _GEN_2846 : _GEN_2814; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2917 = 5'h3 == rd ? _next_reg_T_92[63:32] : _GEN_2885; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2956 = _T_553 ? _GEN_2917 : _GEN_2885; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_2988 = 5'h3 == rd ? _next_reg_T_94[63:32] : _GEN_2956; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3027 = _T_560 ? _GEN_2988 : _GEN_2956; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3059 = 5'h3 == rd ? _next_reg_T_113 : _GEN_3027; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3098 = _T_567 ? _GEN_3059 : _GEN_3027; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3130 = 5'h3 == rd ? _next_reg_T_117 : _GEN_3098; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3169 = _T_574 ? _GEN_3130 : _GEN_3098; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3201 = 5'h3 == rd ? _next_reg_T_131 : _GEN_3169; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3240 = _T_581 ? _GEN_3201 : _GEN_3169; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3272 = 5'h3 == rd ? _next_reg_T_134 : _GEN_3240; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3311 = _T_588 ? _GEN_3272 : _GEN_3240; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_109 = _T_1 ? _GEN_36 : io_now_reg_4; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_144 = 5'h4 == rd ? _next_reg_rd_1 : _GEN_109; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_185 = _T_7 ? _GEN_144 : _GEN_109; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_220 = 5'h4 == rd ? _next_reg_rd_3 : _GEN_185; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_261 = _T_13 ? _GEN_220 : _GEN_185; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_296 = 5'h4 == rd ? _next_reg_T_8 : _GEN_261; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_337 = _T_19 ? _GEN_296 : _GEN_261; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_372 = 5'h4 == rd ? _next_reg_T_9 : _GEN_337; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_413 = _T_25 ? _GEN_372 : _GEN_337; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_448 = 5'h4 == rd ? _next_reg_T_10 : _GEN_413; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_489 = _T_31 ? _GEN_448 : _GEN_413; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_524 = 5'h4 == rd ? _next_reg_T_12[31:0] : _GEN_489; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_565 = _T_37 ? _GEN_524 : _GEN_489; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_600 = 5'h4 == rd ? _next_reg_T_14 : _GEN_565; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_641 = _T_43 ? _GEN_600 : _GEN_565; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_676 = 5'h4 == rd ? _next_reg_T_18 : _GEN_641; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_717 = _T_49 ? _GEN_676 : _GEN_641; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_752 = 5'h4 == rd ? imm : _GEN_717; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_789 = _T_55 ? _GEN_752 : _GEN_717; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_824 = 5'h4 == rd ? _T_334 : _GEN_789; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_861 = _T_59 ? _GEN_824 : _GEN_789; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_928 = 5'h4 == rd ? _next_reg_T_22 : _GEN_861; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_971 = _T_63 ? _GEN_928 : _GEN_861; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1006 = 5'h4 == rd ? _next_reg_rd_23 : _GEN_971; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1049 = _T_70 ? _GEN_1006 : _GEN_971; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1084 = 5'h4 == rd ? _next_reg_rd_25 : _GEN_1049; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1127 = _T_77 ? _GEN_1084 : _GEN_1049; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1162 = 5'h4 == rd ? _next_reg_T_29 : _GEN_1127; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1205 = _T_84 ? _GEN_1162 : _GEN_1127; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1240 = 5'h4 == rd ? _next_reg_T_30 : _GEN_1205; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1283 = _T_91 ? _GEN_1240 : _GEN_1205; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1318 = 5'h4 == rd ? _next_reg_T_31 : _GEN_1283; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1361 = _T_98 ? _GEN_1318 : _GEN_1283; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1396 = 5'h4 == rd ? _next_reg_T_33[31:0] : _GEN_1361; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1439 = _T_105 ? _GEN_1396 : _GEN_1361; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1474 = 5'h4 == rd ? _next_reg_T_35 : _GEN_1439; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1517 = _T_112 ? _GEN_1474 : _GEN_1439; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1552 = 5'h4 == rd ? _next_reg_T_37 : _GEN_1517; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1595 = _T_119 ? _GEN_1552 : _GEN_1517; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1630 = 5'h4 == rd ? _next_reg_T_41 : _GEN_1595; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1673 = _T_126 ? _GEN_1630 : _GEN_1595; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1708 = 5'h4 == rd ? _next_reg_T_45 : _GEN_1673; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1742 = _T_346 ? _GEN_1708 : _GEN_1673; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1790 = _T_133 ? _GEN_1742 : _GEN_1673; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1828 = 5'h4 == rd ? _next_reg_T_45 : _GEN_1790; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1862 = _T_180 ? _GEN_1828 : _GEN_1790; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1911 = _T_157 ? _GEN_1862 : _GEN_1790; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2123 = 5'h4 == rd ? _next_reg_T_53 : _GEN_1911; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2204 = _T_348 ? _GEN_2123 : _GEN_1911; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2241 = 5'h4 == rd ? _next_reg_T_61 : _GEN_2204; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2276 = _T_435 ? _GEN_2241 : _GEN_2204; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2322 = _T_368 ? _GEN_2276 : _GEN_2204; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2359 = 5'h4 == rd ? _next_reg_T_65[31:0] : _GEN_2322; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2394 = _T_437 ? _GEN_2359 : _GEN_2322; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2440 = _T_388 ? _GEN_2394 : _GEN_2322; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2479 = 5'h4 == rd ? _next_reg_T_73 : _GEN_2440; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2525 = _T_408 ? _GEN_2479 : _GEN_2440; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2560 = 5'h4 == rd ? _next_reg_T_79 : _GEN_2525; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2595 = _T_435 ? _GEN_2560 : _GEN_2525; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2641 = _T_427 ? _GEN_2595 : _GEN_2525; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2776 = 5'h4 == rd ? _next_reg_T_94[31:0] : _GEN_2641; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2815 = _T_539 ? _GEN_2776 : _GEN_2641; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2847 = 5'h4 == rd ? _next_reg_T_85[63:32] : _GEN_2815; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2886 = _T_546 ? _GEN_2847 : _GEN_2815; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2918 = 5'h4 == rd ? _next_reg_T_92[63:32] : _GEN_2886; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2957 = _T_553 ? _GEN_2918 : _GEN_2886; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_2989 = 5'h4 == rd ? _next_reg_T_94[63:32] : _GEN_2957; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3028 = _T_560 ? _GEN_2989 : _GEN_2957; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3060 = 5'h4 == rd ? _next_reg_T_113 : _GEN_3028; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3099 = _T_567 ? _GEN_3060 : _GEN_3028; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3131 = 5'h4 == rd ? _next_reg_T_117 : _GEN_3099; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3170 = _T_574 ? _GEN_3131 : _GEN_3099; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3202 = 5'h4 == rd ? _next_reg_T_131 : _GEN_3170; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3241 = _T_581 ? _GEN_3202 : _GEN_3170; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3273 = 5'h4 == rd ? _next_reg_T_134 : _GEN_3241; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3312 = _T_588 ? _GEN_3273 : _GEN_3241; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_110 = _T_1 ? _GEN_37 : io_now_reg_5; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_145 = 5'h5 == rd ? _next_reg_rd_1 : _GEN_110; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_186 = _T_7 ? _GEN_145 : _GEN_110; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_221 = 5'h5 == rd ? _next_reg_rd_3 : _GEN_186; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_262 = _T_13 ? _GEN_221 : _GEN_186; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_297 = 5'h5 == rd ? _next_reg_T_8 : _GEN_262; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_338 = _T_19 ? _GEN_297 : _GEN_262; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_373 = 5'h5 == rd ? _next_reg_T_9 : _GEN_338; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_414 = _T_25 ? _GEN_373 : _GEN_338; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_449 = 5'h5 == rd ? _next_reg_T_10 : _GEN_414; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_490 = _T_31 ? _GEN_449 : _GEN_414; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_525 = 5'h5 == rd ? _next_reg_T_12[31:0] : _GEN_490; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_566 = _T_37 ? _GEN_525 : _GEN_490; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_601 = 5'h5 == rd ? _next_reg_T_14 : _GEN_566; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_642 = _T_43 ? _GEN_601 : _GEN_566; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_677 = 5'h5 == rd ? _next_reg_T_18 : _GEN_642; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_718 = _T_49 ? _GEN_677 : _GEN_642; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_753 = 5'h5 == rd ? imm : _GEN_718; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_790 = _T_55 ? _GEN_753 : _GEN_718; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_825 = 5'h5 == rd ? _T_334 : _GEN_790; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_862 = _T_59 ? _GEN_825 : _GEN_790; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_929 = 5'h5 == rd ? _next_reg_T_22 : _GEN_862; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_972 = _T_63 ? _GEN_929 : _GEN_862; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1007 = 5'h5 == rd ? _next_reg_rd_23 : _GEN_972; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1050 = _T_70 ? _GEN_1007 : _GEN_972; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1085 = 5'h5 == rd ? _next_reg_rd_25 : _GEN_1050; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1128 = _T_77 ? _GEN_1085 : _GEN_1050; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1163 = 5'h5 == rd ? _next_reg_T_29 : _GEN_1128; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1206 = _T_84 ? _GEN_1163 : _GEN_1128; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1241 = 5'h5 == rd ? _next_reg_T_30 : _GEN_1206; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1284 = _T_91 ? _GEN_1241 : _GEN_1206; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1319 = 5'h5 == rd ? _next_reg_T_31 : _GEN_1284; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1362 = _T_98 ? _GEN_1319 : _GEN_1284; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1397 = 5'h5 == rd ? _next_reg_T_33[31:0] : _GEN_1362; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1440 = _T_105 ? _GEN_1397 : _GEN_1362; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1475 = 5'h5 == rd ? _next_reg_T_35 : _GEN_1440; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1518 = _T_112 ? _GEN_1475 : _GEN_1440; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1553 = 5'h5 == rd ? _next_reg_T_37 : _GEN_1518; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1596 = _T_119 ? _GEN_1553 : _GEN_1518; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1631 = 5'h5 == rd ? _next_reg_T_41 : _GEN_1596; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1674 = _T_126 ? _GEN_1631 : _GEN_1596; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1709 = 5'h5 == rd ? _next_reg_T_45 : _GEN_1674; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1743 = _T_346 ? _GEN_1709 : _GEN_1674; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1791 = _T_133 ? _GEN_1743 : _GEN_1674; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1829 = 5'h5 == rd ? _next_reg_T_45 : _GEN_1791; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1863 = _T_180 ? _GEN_1829 : _GEN_1791; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1912 = _T_157 ? _GEN_1863 : _GEN_1791; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2124 = 5'h5 == rd ? _next_reg_T_53 : _GEN_1912; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2205 = _T_348 ? _GEN_2124 : _GEN_1912; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2242 = 5'h5 == rd ? _next_reg_T_61 : _GEN_2205; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2277 = _T_435 ? _GEN_2242 : _GEN_2205; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2323 = _T_368 ? _GEN_2277 : _GEN_2205; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2360 = 5'h5 == rd ? _next_reg_T_65[31:0] : _GEN_2323; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2395 = _T_437 ? _GEN_2360 : _GEN_2323; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2441 = _T_388 ? _GEN_2395 : _GEN_2323; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2480 = 5'h5 == rd ? _next_reg_T_73 : _GEN_2441; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2526 = _T_408 ? _GEN_2480 : _GEN_2441; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2561 = 5'h5 == rd ? _next_reg_T_79 : _GEN_2526; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2596 = _T_435 ? _GEN_2561 : _GEN_2526; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2642 = _T_427 ? _GEN_2596 : _GEN_2526; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2777 = 5'h5 == rd ? _next_reg_T_94[31:0] : _GEN_2642; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2816 = _T_539 ? _GEN_2777 : _GEN_2642; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2848 = 5'h5 == rd ? _next_reg_T_85[63:32] : _GEN_2816; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2887 = _T_546 ? _GEN_2848 : _GEN_2816; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2919 = 5'h5 == rd ? _next_reg_T_92[63:32] : _GEN_2887; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2958 = _T_553 ? _GEN_2919 : _GEN_2887; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_2990 = 5'h5 == rd ? _next_reg_T_94[63:32] : _GEN_2958; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3029 = _T_560 ? _GEN_2990 : _GEN_2958; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3061 = 5'h5 == rd ? _next_reg_T_113 : _GEN_3029; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3100 = _T_567 ? _GEN_3061 : _GEN_3029; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3132 = 5'h5 == rd ? _next_reg_T_117 : _GEN_3100; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3171 = _T_574 ? _GEN_3132 : _GEN_3100; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3203 = 5'h5 == rd ? _next_reg_T_131 : _GEN_3171; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3242 = _T_581 ? _GEN_3203 : _GEN_3171; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3274 = 5'h5 == rd ? _next_reg_T_134 : _GEN_3242; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3313 = _T_588 ? _GEN_3274 : _GEN_3242; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_111 = _T_1 ? _GEN_38 : io_now_reg_6; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_146 = 5'h6 == rd ? _next_reg_rd_1 : _GEN_111; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_187 = _T_7 ? _GEN_146 : _GEN_111; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_222 = 5'h6 == rd ? _next_reg_rd_3 : _GEN_187; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_263 = _T_13 ? _GEN_222 : _GEN_187; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_298 = 5'h6 == rd ? _next_reg_T_8 : _GEN_263; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_339 = _T_19 ? _GEN_298 : _GEN_263; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_374 = 5'h6 == rd ? _next_reg_T_9 : _GEN_339; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_415 = _T_25 ? _GEN_374 : _GEN_339; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_450 = 5'h6 == rd ? _next_reg_T_10 : _GEN_415; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_491 = _T_31 ? _GEN_450 : _GEN_415; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_526 = 5'h6 == rd ? _next_reg_T_12[31:0] : _GEN_491; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_567 = _T_37 ? _GEN_526 : _GEN_491; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_602 = 5'h6 == rd ? _next_reg_T_14 : _GEN_567; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_643 = _T_43 ? _GEN_602 : _GEN_567; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_678 = 5'h6 == rd ? _next_reg_T_18 : _GEN_643; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_719 = _T_49 ? _GEN_678 : _GEN_643; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_754 = 5'h6 == rd ? imm : _GEN_719; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_791 = _T_55 ? _GEN_754 : _GEN_719; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_826 = 5'h6 == rd ? _T_334 : _GEN_791; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_863 = _T_59 ? _GEN_826 : _GEN_791; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_930 = 5'h6 == rd ? _next_reg_T_22 : _GEN_863; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_973 = _T_63 ? _GEN_930 : _GEN_863; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1008 = 5'h6 == rd ? _next_reg_rd_23 : _GEN_973; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1051 = _T_70 ? _GEN_1008 : _GEN_973; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1086 = 5'h6 == rd ? _next_reg_rd_25 : _GEN_1051; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1129 = _T_77 ? _GEN_1086 : _GEN_1051; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1164 = 5'h6 == rd ? _next_reg_T_29 : _GEN_1129; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1207 = _T_84 ? _GEN_1164 : _GEN_1129; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1242 = 5'h6 == rd ? _next_reg_T_30 : _GEN_1207; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1285 = _T_91 ? _GEN_1242 : _GEN_1207; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1320 = 5'h6 == rd ? _next_reg_T_31 : _GEN_1285; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1363 = _T_98 ? _GEN_1320 : _GEN_1285; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1398 = 5'h6 == rd ? _next_reg_T_33[31:0] : _GEN_1363; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1441 = _T_105 ? _GEN_1398 : _GEN_1363; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1476 = 5'h6 == rd ? _next_reg_T_35 : _GEN_1441; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1519 = _T_112 ? _GEN_1476 : _GEN_1441; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1554 = 5'h6 == rd ? _next_reg_T_37 : _GEN_1519; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1597 = _T_119 ? _GEN_1554 : _GEN_1519; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1632 = 5'h6 == rd ? _next_reg_T_41 : _GEN_1597; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1675 = _T_126 ? _GEN_1632 : _GEN_1597; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1710 = 5'h6 == rd ? _next_reg_T_45 : _GEN_1675; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1744 = _T_346 ? _GEN_1710 : _GEN_1675; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1792 = _T_133 ? _GEN_1744 : _GEN_1675; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1830 = 5'h6 == rd ? _next_reg_T_45 : _GEN_1792; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1864 = _T_180 ? _GEN_1830 : _GEN_1792; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1913 = _T_157 ? _GEN_1864 : _GEN_1792; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2125 = 5'h6 == rd ? _next_reg_T_53 : _GEN_1913; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2206 = _T_348 ? _GEN_2125 : _GEN_1913; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2243 = 5'h6 == rd ? _next_reg_T_61 : _GEN_2206; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2278 = _T_435 ? _GEN_2243 : _GEN_2206; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2324 = _T_368 ? _GEN_2278 : _GEN_2206; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2361 = 5'h6 == rd ? _next_reg_T_65[31:0] : _GEN_2324; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2396 = _T_437 ? _GEN_2361 : _GEN_2324; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2442 = _T_388 ? _GEN_2396 : _GEN_2324; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2481 = 5'h6 == rd ? _next_reg_T_73 : _GEN_2442; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2527 = _T_408 ? _GEN_2481 : _GEN_2442; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2562 = 5'h6 == rd ? _next_reg_T_79 : _GEN_2527; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2597 = _T_435 ? _GEN_2562 : _GEN_2527; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2643 = _T_427 ? _GEN_2597 : _GEN_2527; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2778 = 5'h6 == rd ? _next_reg_T_94[31:0] : _GEN_2643; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2817 = _T_539 ? _GEN_2778 : _GEN_2643; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2849 = 5'h6 == rd ? _next_reg_T_85[63:32] : _GEN_2817; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2888 = _T_546 ? _GEN_2849 : _GEN_2817; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2920 = 5'h6 == rd ? _next_reg_T_92[63:32] : _GEN_2888; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2959 = _T_553 ? _GEN_2920 : _GEN_2888; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_2991 = 5'h6 == rd ? _next_reg_T_94[63:32] : _GEN_2959; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3030 = _T_560 ? _GEN_2991 : _GEN_2959; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3062 = 5'h6 == rd ? _next_reg_T_113 : _GEN_3030; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3101 = _T_567 ? _GEN_3062 : _GEN_3030; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3133 = 5'h6 == rd ? _next_reg_T_117 : _GEN_3101; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3172 = _T_574 ? _GEN_3133 : _GEN_3101; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3204 = 5'h6 == rd ? _next_reg_T_131 : _GEN_3172; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3243 = _T_581 ? _GEN_3204 : _GEN_3172; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3275 = 5'h6 == rd ? _next_reg_T_134 : _GEN_3243; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3314 = _T_588 ? _GEN_3275 : _GEN_3243; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_112 = _T_1 ? _GEN_39 : io_now_reg_7; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_147 = 5'h7 == rd ? _next_reg_rd_1 : _GEN_112; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_188 = _T_7 ? _GEN_147 : _GEN_112; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_223 = 5'h7 == rd ? _next_reg_rd_3 : _GEN_188; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_264 = _T_13 ? _GEN_223 : _GEN_188; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_299 = 5'h7 == rd ? _next_reg_T_8 : _GEN_264; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_340 = _T_19 ? _GEN_299 : _GEN_264; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_375 = 5'h7 == rd ? _next_reg_T_9 : _GEN_340; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_416 = _T_25 ? _GEN_375 : _GEN_340; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_451 = 5'h7 == rd ? _next_reg_T_10 : _GEN_416; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_492 = _T_31 ? _GEN_451 : _GEN_416; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_527 = 5'h7 == rd ? _next_reg_T_12[31:0] : _GEN_492; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_568 = _T_37 ? _GEN_527 : _GEN_492; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_603 = 5'h7 == rd ? _next_reg_T_14 : _GEN_568; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_644 = _T_43 ? _GEN_603 : _GEN_568; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_679 = 5'h7 == rd ? _next_reg_T_18 : _GEN_644; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_720 = _T_49 ? _GEN_679 : _GEN_644; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_755 = 5'h7 == rd ? imm : _GEN_720; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_792 = _T_55 ? _GEN_755 : _GEN_720; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_827 = 5'h7 == rd ? _T_334 : _GEN_792; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_864 = _T_59 ? _GEN_827 : _GEN_792; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_931 = 5'h7 == rd ? _next_reg_T_22 : _GEN_864; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_974 = _T_63 ? _GEN_931 : _GEN_864; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1009 = 5'h7 == rd ? _next_reg_rd_23 : _GEN_974; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1052 = _T_70 ? _GEN_1009 : _GEN_974; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1087 = 5'h7 == rd ? _next_reg_rd_25 : _GEN_1052; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1130 = _T_77 ? _GEN_1087 : _GEN_1052; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1165 = 5'h7 == rd ? _next_reg_T_29 : _GEN_1130; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1208 = _T_84 ? _GEN_1165 : _GEN_1130; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1243 = 5'h7 == rd ? _next_reg_T_30 : _GEN_1208; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1286 = _T_91 ? _GEN_1243 : _GEN_1208; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1321 = 5'h7 == rd ? _next_reg_T_31 : _GEN_1286; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1364 = _T_98 ? _GEN_1321 : _GEN_1286; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1399 = 5'h7 == rd ? _next_reg_T_33[31:0] : _GEN_1364; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1442 = _T_105 ? _GEN_1399 : _GEN_1364; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1477 = 5'h7 == rd ? _next_reg_T_35 : _GEN_1442; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1520 = _T_112 ? _GEN_1477 : _GEN_1442; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1555 = 5'h7 == rd ? _next_reg_T_37 : _GEN_1520; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1598 = _T_119 ? _GEN_1555 : _GEN_1520; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1633 = 5'h7 == rd ? _next_reg_T_41 : _GEN_1598; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1676 = _T_126 ? _GEN_1633 : _GEN_1598; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1711 = 5'h7 == rd ? _next_reg_T_45 : _GEN_1676; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1745 = _T_346 ? _GEN_1711 : _GEN_1676; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1793 = _T_133 ? _GEN_1745 : _GEN_1676; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1831 = 5'h7 == rd ? _next_reg_T_45 : _GEN_1793; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1865 = _T_180 ? _GEN_1831 : _GEN_1793; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1914 = _T_157 ? _GEN_1865 : _GEN_1793; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2126 = 5'h7 == rd ? _next_reg_T_53 : _GEN_1914; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2207 = _T_348 ? _GEN_2126 : _GEN_1914; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2244 = 5'h7 == rd ? _next_reg_T_61 : _GEN_2207; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2279 = _T_435 ? _GEN_2244 : _GEN_2207; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2325 = _T_368 ? _GEN_2279 : _GEN_2207; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2362 = 5'h7 == rd ? _next_reg_T_65[31:0] : _GEN_2325; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2397 = _T_437 ? _GEN_2362 : _GEN_2325; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2443 = _T_388 ? _GEN_2397 : _GEN_2325; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2482 = 5'h7 == rd ? _next_reg_T_73 : _GEN_2443; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2528 = _T_408 ? _GEN_2482 : _GEN_2443; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2563 = 5'h7 == rd ? _next_reg_T_79 : _GEN_2528; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2598 = _T_435 ? _GEN_2563 : _GEN_2528; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2644 = _T_427 ? _GEN_2598 : _GEN_2528; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2779 = 5'h7 == rd ? _next_reg_T_94[31:0] : _GEN_2644; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2818 = _T_539 ? _GEN_2779 : _GEN_2644; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2850 = 5'h7 == rd ? _next_reg_T_85[63:32] : _GEN_2818; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2889 = _T_546 ? _GEN_2850 : _GEN_2818; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2921 = 5'h7 == rd ? _next_reg_T_92[63:32] : _GEN_2889; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2960 = _T_553 ? _GEN_2921 : _GEN_2889; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_2992 = 5'h7 == rd ? _next_reg_T_94[63:32] : _GEN_2960; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3031 = _T_560 ? _GEN_2992 : _GEN_2960; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3063 = 5'h7 == rd ? _next_reg_T_113 : _GEN_3031; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3102 = _T_567 ? _GEN_3063 : _GEN_3031; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3134 = 5'h7 == rd ? _next_reg_T_117 : _GEN_3102; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3173 = _T_574 ? _GEN_3134 : _GEN_3102; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3205 = 5'h7 == rd ? _next_reg_T_131 : _GEN_3173; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3244 = _T_581 ? _GEN_3205 : _GEN_3173; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3276 = 5'h7 == rd ? _next_reg_T_134 : _GEN_3244; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3315 = _T_588 ? _GEN_3276 : _GEN_3244; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_113 = _T_1 ? _GEN_40 : io_now_reg_8; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_148 = 5'h8 == rd ? _next_reg_rd_1 : _GEN_113; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_189 = _T_7 ? _GEN_148 : _GEN_113; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_224 = 5'h8 == rd ? _next_reg_rd_3 : _GEN_189; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_265 = _T_13 ? _GEN_224 : _GEN_189; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_300 = 5'h8 == rd ? _next_reg_T_8 : _GEN_265; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_341 = _T_19 ? _GEN_300 : _GEN_265; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_376 = 5'h8 == rd ? _next_reg_T_9 : _GEN_341; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_417 = _T_25 ? _GEN_376 : _GEN_341; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_452 = 5'h8 == rd ? _next_reg_T_10 : _GEN_417; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_493 = _T_31 ? _GEN_452 : _GEN_417; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_528 = 5'h8 == rd ? _next_reg_T_12[31:0] : _GEN_493; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_569 = _T_37 ? _GEN_528 : _GEN_493; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_604 = 5'h8 == rd ? _next_reg_T_14 : _GEN_569; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_645 = _T_43 ? _GEN_604 : _GEN_569; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_680 = 5'h8 == rd ? _next_reg_T_18 : _GEN_645; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_721 = _T_49 ? _GEN_680 : _GEN_645; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_756 = 5'h8 == rd ? imm : _GEN_721; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_793 = _T_55 ? _GEN_756 : _GEN_721; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_828 = 5'h8 == rd ? _T_334 : _GEN_793; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_865 = _T_59 ? _GEN_828 : _GEN_793; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_932 = 5'h8 == rd ? _next_reg_T_22 : _GEN_865; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_975 = _T_63 ? _GEN_932 : _GEN_865; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1010 = 5'h8 == rd ? _next_reg_rd_23 : _GEN_975; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1053 = _T_70 ? _GEN_1010 : _GEN_975; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1088 = 5'h8 == rd ? _next_reg_rd_25 : _GEN_1053; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1131 = _T_77 ? _GEN_1088 : _GEN_1053; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1166 = 5'h8 == rd ? _next_reg_T_29 : _GEN_1131; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1209 = _T_84 ? _GEN_1166 : _GEN_1131; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1244 = 5'h8 == rd ? _next_reg_T_30 : _GEN_1209; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1287 = _T_91 ? _GEN_1244 : _GEN_1209; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1322 = 5'h8 == rd ? _next_reg_T_31 : _GEN_1287; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1365 = _T_98 ? _GEN_1322 : _GEN_1287; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1400 = 5'h8 == rd ? _next_reg_T_33[31:0] : _GEN_1365; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1443 = _T_105 ? _GEN_1400 : _GEN_1365; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1478 = 5'h8 == rd ? _next_reg_T_35 : _GEN_1443; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1521 = _T_112 ? _GEN_1478 : _GEN_1443; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1556 = 5'h8 == rd ? _next_reg_T_37 : _GEN_1521; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1599 = _T_119 ? _GEN_1556 : _GEN_1521; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1634 = 5'h8 == rd ? _next_reg_T_41 : _GEN_1599; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1677 = _T_126 ? _GEN_1634 : _GEN_1599; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1712 = 5'h8 == rd ? _next_reg_T_45 : _GEN_1677; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1746 = _T_346 ? _GEN_1712 : _GEN_1677; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1794 = _T_133 ? _GEN_1746 : _GEN_1677; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1832 = 5'h8 == rd ? _next_reg_T_45 : _GEN_1794; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1866 = _T_180 ? _GEN_1832 : _GEN_1794; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1915 = _T_157 ? _GEN_1866 : _GEN_1794; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2127 = 5'h8 == rd ? _next_reg_T_53 : _GEN_1915; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2208 = _T_348 ? _GEN_2127 : _GEN_1915; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2245 = 5'h8 == rd ? _next_reg_T_61 : _GEN_2208; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2280 = _T_435 ? _GEN_2245 : _GEN_2208; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2326 = _T_368 ? _GEN_2280 : _GEN_2208; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2363 = 5'h8 == rd ? _next_reg_T_65[31:0] : _GEN_2326; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2398 = _T_437 ? _GEN_2363 : _GEN_2326; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2444 = _T_388 ? _GEN_2398 : _GEN_2326; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2483 = 5'h8 == rd ? _next_reg_T_73 : _GEN_2444; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2529 = _T_408 ? _GEN_2483 : _GEN_2444; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2564 = 5'h8 == rd ? _next_reg_T_79 : _GEN_2529; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2599 = _T_435 ? _GEN_2564 : _GEN_2529; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2645 = _T_427 ? _GEN_2599 : _GEN_2529; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2780 = 5'h8 == rd ? _next_reg_T_94[31:0] : _GEN_2645; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2819 = _T_539 ? _GEN_2780 : _GEN_2645; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2851 = 5'h8 == rd ? _next_reg_T_85[63:32] : _GEN_2819; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2890 = _T_546 ? _GEN_2851 : _GEN_2819; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2922 = 5'h8 == rd ? _next_reg_T_92[63:32] : _GEN_2890; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2961 = _T_553 ? _GEN_2922 : _GEN_2890; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_2993 = 5'h8 == rd ? _next_reg_T_94[63:32] : _GEN_2961; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3032 = _T_560 ? _GEN_2993 : _GEN_2961; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3064 = 5'h8 == rd ? _next_reg_T_113 : _GEN_3032; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3103 = _T_567 ? _GEN_3064 : _GEN_3032; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3135 = 5'h8 == rd ? _next_reg_T_117 : _GEN_3103; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3174 = _T_574 ? _GEN_3135 : _GEN_3103; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3206 = 5'h8 == rd ? _next_reg_T_131 : _GEN_3174; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3245 = _T_581 ? _GEN_3206 : _GEN_3174; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3277 = 5'h8 == rd ? _next_reg_T_134 : _GEN_3245; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3316 = _T_588 ? _GEN_3277 : _GEN_3245; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_114 = _T_1 ? _GEN_41 : io_now_reg_9; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_149 = 5'h9 == rd ? _next_reg_rd_1 : _GEN_114; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_190 = _T_7 ? _GEN_149 : _GEN_114; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_225 = 5'h9 == rd ? _next_reg_rd_3 : _GEN_190; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_266 = _T_13 ? _GEN_225 : _GEN_190; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_301 = 5'h9 == rd ? _next_reg_T_8 : _GEN_266; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_342 = _T_19 ? _GEN_301 : _GEN_266; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_377 = 5'h9 == rd ? _next_reg_T_9 : _GEN_342; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_418 = _T_25 ? _GEN_377 : _GEN_342; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_453 = 5'h9 == rd ? _next_reg_T_10 : _GEN_418; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_494 = _T_31 ? _GEN_453 : _GEN_418; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_529 = 5'h9 == rd ? _next_reg_T_12[31:0] : _GEN_494; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_570 = _T_37 ? _GEN_529 : _GEN_494; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_605 = 5'h9 == rd ? _next_reg_T_14 : _GEN_570; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_646 = _T_43 ? _GEN_605 : _GEN_570; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_681 = 5'h9 == rd ? _next_reg_T_18 : _GEN_646; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_722 = _T_49 ? _GEN_681 : _GEN_646; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_757 = 5'h9 == rd ? imm : _GEN_722; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_794 = _T_55 ? _GEN_757 : _GEN_722; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_829 = 5'h9 == rd ? _T_334 : _GEN_794; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_866 = _T_59 ? _GEN_829 : _GEN_794; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_933 = 5'h9 == rd ? _next_reg_T_22 : _GEN_866; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_976 = _T_63 ? _GEN_933 : _GEN_866; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1011 = 5'h9 == rd ? _next_reg_rd_23 : _GEN_976; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1054 = _T_70 ? _GEN_1011 : _GEN_976; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1089 = 5'h9 == rd ? _next_reg_rd_25 : _GEN_1054; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1132 = _T_77 ? _GEN_1089 : _GEN_1054; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1167 = 5'h9 == rd ? _next_reg_T_29 : _GEN_1132; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1210 = _T_84 ? _GEN_1167 : _GEN_1132; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1245 = 5'h9 == rd ? _next_reg_T_30 : _GEN_1210; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1288 = _T_91 ? _GEN_1245 : _GEN_1210; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1323 = 5'h9 == rd ? _next_reg_T_31 : _GEN_1288; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1366 = _T_98 ? _GEN_1323 : _GEN_1288; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1401 = 5'h9 == rd ? _next_reg_T_33[31:0] : _GEN_1366; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1444 = _T_105 ? _GEN_1401 : _GEN_1366; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1479 = 5'h9 == rd ? _next_reg_T_35 : _GEN_1444; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1522 = _T_112 ? _GEN_1479 : _GEN_1444; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1557 = 5'h9 == rd ? _next_reg_T_37 : _GEN_1522; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1600 = _T_119 ? _GEN_1557 : _GEN_1522; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1635 = 5'h9 == rd ? _next_reg_T_41 : _GEN_1600; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1678 = _T_126 ? _GEN_1635 : _GEN_1600; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1713 = 5'h9 == rd ? _next_reg_T_45 : _GEN_1678; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1747 = _T_346 ? _GEN_1713 : _GEN_1678; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1795 = _T_133 ? _GEN_1747 : _GEN_1678; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1833 = 5'h9 == rd ? _next_reg_T_45 : _GEN_1795; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1867 = _T_180 ? _GEN_1833 : _GEN_1795; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1916 = _T_157 ? _GEN_1867 : _GEN_1795; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2128 = 5'h9 == rd ? _next_reg_T_53 : _GEN_1916; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2209 = _T_348 ? _GEN_2128 : _GEN_1916; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2246 = 5'h9 == rd ? _next_reg_T_61 : _GEN_2209; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2281 = _T_435 ? _GEN_2246 : _GEN_2209; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2327 = _T_368 ? _GEN_2281 : _GEN_2209; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2364 = 5'h9 == rd ? _next_reg_T_65[31:0] : _GEN_2327; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2399 = _T_437 ? _GEN_2364 : _GEN_2327; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2445 = _T_388 ? _GEN_2399 : _GEN_2327; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2484 = 5'h9 == rd ? _next_reg_T_73 : _GEN_2445; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2530 = _T_408 ? _GEN_2484 : _GEN_2445; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2565 = 5'h9 == rd ? _next_reg_T_79 : _GEN_2530; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2600 = _T_435 ? _GEN_2565 : _GEN_2530; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2646 = _T_427 ? _GEN_2600 : _GEN_2530; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2781 = 5'h9 == rd ? _next_reg_T_94[31:0] : _GEN_2646; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2820 = _T_539 ? _GEN_2781 : _GEN_2646; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2852 = 5'h9 == rd ? _next_reg_T_85[63:32] : _GEN_2820; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2891 = _T_546 ? _GEN_2852 : _GEN_2820; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2923 = 5'h9 == rd ? _next_reg_T_92[63:32] : _GEN_2891; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2962 = _T_553 ? _GEN_2923 : _GEN_2891; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_2994 = 5'h9 == rd ? _next_reg_T_94[63:32] : _GEN_2962; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3033 = _T_560 ? _GEN_2994 : _GEN_2962; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3065 = 5'h9 == rd ? _next_reg_T_113 : _GEN_3033; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3104 = _T_567 ? _GEN_3065 : _GEN_3033; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3136 = 5'h9 == rd ? _next_reg_T_117 : _GEN_3104; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3175 = _T_574 ? _GEN_3136 : _GEN_3104; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3207 = 5'h9 == rd ? _next_reg_T_131 : _GEN_3175; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3246 = _T_581 ? _GEN_3207 : _GEN_3175; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3278 = 5'h9 == rd ? _next_reg_T_134 : _GEN_3246; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3317 = _T_588 ? _GEN_3278 : _GEN_3246; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_115 = _T_1 ? _GEN_42 : io_now_reg_10; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_150 = 5'ha == rd ? _next_reg_rd_1 : _GEN_115; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_191 = _T_7 ? _GEN_150 : _GEN_115; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_226 = 5'ha == rd ? _next_reg_rd_3 : _GEN_191; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_267 = _T_13 ? _GEN_226 : _GEN_191; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_302 = 5'ha == rd ? _next_reg_T_8 : _GEN_267; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_343 = _T_19 ? _GEN_302 : _GEN_267; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_378 = 5'ha == rd ? _next_reg_T_9 : _GEN_343; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_419 = _T_25 ? _GEN_378 : _GEN_343; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_454 = 5'ha == rd ? _next_reg_T_10 : _GEN_419; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_495 = _T_31 ? _GEN_454 : _GEN_419; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_530 = 5'ha == rd ? _next_reg_T_12[31:0] : _GEN_495; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_571 = _T_37 ? _GEN_530 : _GEN_495; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_606 = 5'ha == rd ? _next_reg_T_14 : _GEN_571; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_647 = _T_43 ? _GEN_606 : _GEN_571; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_682 = 5'ha == rd ? _next_reg_T_18 : _GEN_647; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_723 = _T_49 ? _GEN_682 : _GEN_647; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_758 = 5'ha == rd ? imm : _GEN_723; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_795 = _T_55 ? _GEN_758 : _GEN_723; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_830 = 5'ha == rd ? _T_334 : _GEN_795; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_867 = _T_59 ? _GEN_830 : _GEN_795; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_934 = 5'ha == rd ? _next_reg_T_22 : _GEN_867; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_977 = _T_63 ? _GEN_934 : _GEN_867; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1012 = 5'ha == rd ? _next_reg_rd_23 : _GEN_977; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1055 = _T_70 ? _GEN_1012 : _GEN_977; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1090 = 5'ha == rd ? _next_reg_rd_25 : _GEN_1055; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1133 = _T_77 ? _GEN_1090 : _GEN_1055; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1168 = 5'ha == rd ? _next_reg_T_29 : _GEN_1133; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1211 = _T_84 ? _GEN_1168 : _GEN_1133; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1246 = 5'ha == rd ? _next_reg_T_30 : _GEN_1211; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1289 = _T_91 ? _GEN_1246 : _GEN_1211; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1324 = 5'ha == rd ? _next_reg_T_31 : _GEN_1289; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1367 = _T_98 ? _GEN_1324 : _GEN_1289; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1402 = 5'ha == rd ? _next_reg_T_33[31:0] : _GEN_1367; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1445 = _T_105 ? _GEN_1402 : _GEN_1367; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1480 = 5'ha == rd ? _next_reg_T_35 : _GEN_1445; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1523 = _T_112 ? _GEN_1480 : _GEN_1445; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1558 = 5'ha == rd ? _next_reg_T_37 : _GEN_1523; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1601 = _T_119 ? _GEN_1558 : _GEN_1523; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1636 = 5'ha == rd ? _next_reg_T_41 : _GEN_1601; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1679 = _T_126 ? _GEN_1636 : _GEN_1601; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1714 = 5'ha == rd ? _next_reg_T_45 : _GEN_1679; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1748 = _T_346 ? _GEN_1714 : _GEN_1679; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1796 = _T_133 ? _GEN_1748 : _GEN_1679; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1834 = 5'ha == rd ? _next_reg_T_45 : _GEN_1796; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1868 = _T_180 ? _GEN_1834 : _GEN_1796; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1917 = _T_157 ? _GEN_1868 : _GEN_1796; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2129 = 5'ha == rd ? _next_reg_T_53 : _GEN_1917; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2210 = _T_348 ? _GEN_2129 : _GEN_1917; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2247 = 5'ha == rd ? _next_reg_T_61 : _GEN_2210; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2282 = _T_435 ? _GEN_2247 : _GEN_2210; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2328 = _T_368 ? _GEN_2282 : _GEN_2210; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2365 = 5'ha == rd ? _next_reg_T_65[31:0] : _GEN_2328; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2400 = _T_437 ? _GEN_2365 : _GEN_2328; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2446 = _T_388 ? _GEN_2400 : _GEN_2328; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2485 = 5'ha == rd ? _next_reg_T_73 : _GEN_2446; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2531 = _T_408 ? _GEN_2485 : _GEN_2446; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2566 = 5'ha == rd ? _next_reg_T_79 : _GEN_2531; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2601 = _T_435 ? _GEN_2566 : _GEN_2531; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2647 = _T_427 ? _GEN_2601 : _GEN_2531; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2782 = 5'ha == rd ? _next_reg_T_94[31:0] : _GEN_2647; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2821 = _T_539 ? _GEN_2782 : _GEN_2647; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2853 = 5'ha == rd ? _next_reg_T_85[63:32] : _GEN_2821; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2892 = _T_546 ? _GEN_2853 : _GEN_2821; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2924 = 5'ha == rd ? _next_reg_T_92[63:32] : _GEN_2892; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2963 = _T_553 ? _GEN_2924 : _GEN_2892; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_2995 = 5'ha == rd ? _next_reg_T_94[63:32] : _GEN_2963; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3034 = _T_560 ? _GEN_2995 : _GEN_2963; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3066 = 5'ha == rd ? _next_reg_T_113 : _GEN_3034; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3105 = _T_567 ? _GEN_3066 : _GEN_3034; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3137 = 5'ha == rd ? _next_reg_T_117 : _GEN_3105; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3176 = _T_574 ? _GEN_3137 : _GEN_3105; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3208 = 5'ha == rd ? _next_reg_T_131 : _GEN_3176; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3247 = _T_581 ? _GEN_3208 : _GEN_3176; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3279 = 5'ha == rd ? _next_reg_T_134 : _GEN_3247; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3318 = _T_588 ? _GEN_3279 : _GEN_3247; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_116 = _T_1 ? _GEN_43 : io_now_reg_11; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_151 = 5'hb == rd ? _next_reg_rd_1 : _GEN_116; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_192 = _T_7 ? _GEN_151 : _GEN_116; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_227 = 5'hb == rd ? _next_reg_rd_3 : _GEN_192; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_268 = _T_13 ? _GEN_227 : _GEN_192; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_303 = 5'hb == rd ? _next_reg_T_8 : _GEN_268; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_344 = _T_19 ? _GEN_303 : _GEN_268; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_379 = 5'hb == rd ? _next_reg_T_9 : _GEN_344; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_420 = _T_25 ? _GEN_379 : _GEN_344; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_455 = 5'hb == rd ? _next_reg_T_10 : _GEN_420; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_496 = _T_31 ? _GEN_455 : _GEN_420; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_531 = 5'hb == rd ? _next_reg_T_12[31:0] : _GEN_496; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_572 = _T_37 ? _GEN_531 : _GEN_496; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_607 = 5'hb == rd ? _next_reg_T_14 : _GEN_572; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_648 = _T_43 ? _GEN_607 : _GEN_572; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_683 = 5'hb == rd ? _next_reg_T_18 : _GEN_648; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_724 = _T_49 ? _GEN_683 : _GEN_648; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_759 = 5'hb == rd ? imm : _GEN_724; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_796 = _T_55 ? _GEN_759 : _GEN_724; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_831 = 5'hb == rd ? _T_334 : _GEN_796; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_868 = _T_59 ? _GEN_831 : _GEN_796; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_935 = 5'hb == rd ? _next_reg_T_22 : _GEN_868; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_978 = _T_63 ? _GEN_935 : _GEN_868; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1013 = 5'hb == rd ? _next_reg_rd_23 : _GEN_978; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1056 = _T_70 ? _GEN_1013 : _GEN_978; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1091 = 5'hb == rd ? _next_reg_rd_25 : _GEN_1056; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1134 = _T_77 ? _GEN_1091 : _GEN_1056; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1169 = 5'hb == rd ? _next_reg_T_29 : _GEN_1134; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1212 = _T_84 ? _GEN_1169 : _GEN_1134; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1247 = 5'hb == rd ? _next_reg_T_30 : _GEN_1212; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1290 = _T_91 ? _GEN_1247 : _GEN_1212; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1325 = 5'hb == rd ? _next_reg_T_31 : _GEN_1290; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1368 = _T_98 ? _GEN_1325 : _GEN_1290; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1403 = 5'hb == rd ? _next_reg_T_33[31:0] : _GEN_1368; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1446 = _T_105 ? _GEN_1403 : _GEN_1368; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1481 = 5'hb == rd ? _next_reg_T_35 : _GEN_1446; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1524 = _T_112 ? _GEN_1481 : _GEN_1446; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1559 = 5'hb == rd ? _next_reg_T_37 : _GEN_1524; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1602 = _T_119 ? _GEN_1559 : _GEN_1524; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1637 = 5'hb == rd ? _next_reg_T_41 : _GEN_1602; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1680 = _T_126 ? _GEN_1637 : _GEN_1602; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1715 = 5'hb == rd ? _next_reg_T_45 : _GEN_1680; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1749 = _T_346 ? _GEN_1715 : _GEN_1680; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1797 = _T_133 ? _GEN_1749 : _GEN_1680; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1835 = 5'hb == rd ? _next_reg_T_45 : _GEN_1797; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1869 = _T_180 ? _GEN_1835 : _GEN_1797; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1918 = _T_157 ? _GEN_1869 : _GEN_1797; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2130 = 5'hb == rd ? _next_reg_T_53 : _GEN_1918; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2211 = _T_348 ? _GEN_2130 : _GEN_1918; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2248 = 5'hb == rd ? _next_reg_T_61 : _GEN_2211; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2283 = _T_435 ? _GEN_2248 : _GEN_2211; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2329 = _T_368 ? _GEN_2283 : _GEN_2211; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2366 = 5'hb == rd ? _next_reg_T_65[31:0] : _GEN_2329; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2401 = _T_437 ? _GEN_2366 : _GEN_2329; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2447 = _T_388 ? _GEN_2401 : _GEN_2329; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2486 = 5'hb == rd ? _next_reg_T_73 : _GEN_2447; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2532 = _T_408 ? _GEN_2486 : _GEN_2447; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2567 = 5'hb == rd ? _next_reg_T_79 : _GEN_2532; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2602 = _T_435 ? _GEN_2567 : _GEN_2532; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2648 = _T_427 ? _GEN_2602 : _GEN_2532; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2783 = 5'hb == rd ? _next_reg_T_94[31:0] : _GEN_2648; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2822 = _T_539 ? _GEN_2783 : _GEN_2648; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2854 = 5'hb == rd ? _next_reg_T_85[63:32] : _GEN_2822; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2893 = _T_546 ? _GEN_2854 : _GEN_2822; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2925 = 5'hb == rd ? _next_reg_T_92[63:32] : _GEN_2893; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2964 = _T_553 ? _GEN_2925 : _GEN_2893; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_2996 = 5'hb == rd ? _next_reg_T_94[63:32] : _GEN_2964; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3035 = _T_560 ? _GEN_2996 : _GEN_2964; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3067 = 5'hb == rd ? _next_reg_T_113 : _GEN_3035; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3106 = _T_567 ? _GEN_3067 : _GEN_3035; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3138 = 5'hb == rd ? _next_reg_T_117 : _GEN_3106; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3177 = _T_574 ? _GEN_3138 : _GEN_3106; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3209 = 5'hb == rd ? _next_reg_T_131 : _GEN_3177; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3248 = _T_581 ? _GEN_3209 : _GEN_3177; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3280 = 5'hb == rd ? _next_reg_T_134 : _GEN_3248; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3319 = _T_588 ? _GEN_3280 : _GEN_3248; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_117 = _T_1 ? _GEN_44 : io_now_reg_12; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_152 = 5'hc == rd ? _next_reg_rd_1 : _GEN_117; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_193 = _T_7 ? _GEN_152 : _GEN_117; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_228 = 5'hc == rd ? _next_reg_rd_3 : _GEN_193; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_269 = _T_13 ? _GEN_228 : _GEN_193; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_304 = 5'hc == rd ? _next_reg_T_8 : _GEN_269; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_345 = _T_19 ? _GEN_304 : _GEN_269; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_380 = 5'hc == rd ? _next_reg_T_9 : _GEN_345; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_421 = _T_25 ? _GEN_380 : _GEN_345; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_456 = 5'hc == rd ? _next_reg_T_10 : _GEN_421; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_497 = _T_31 ? _GEN_456 : _GEN_421; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_532 = 5'hc == rd ? _next_reg_T_12[31:0] : _GEN_497; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_573 = _T_37 ? _GEN_532 : _GEN_497; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_608 = 5'hc == rd ? _next_reg_T_14 : _GEN_573; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_649 = _T_43 ? _GEN_608 : _GEN_573; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_684 = 5'hc == rd ? _next_reg_T_18 : _GEN_649; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_725 = _T_49 ? _GEN_684 : _GEN_649; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_760 = 5'hc == rd ? imm : _GEN_725; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_797 = _T_55 ? _GEN_760 : _GEN_725; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_832 = 5'hc == rd ? _T_334 : _GEN_797; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_869 = _T_59 ? _GEN_832 : _GEN_797; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_936 = 5'hc == rd ? _next_reg_T_22 : _GEN_869; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_979 = _T_63 ? _GEN_936 : _GEN_869; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1014 = 5'hc == rd ? _next_reg_rd_23 : _GEN_979; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1057 = _T_70 ? _GEN_1014 : _GEN_979; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1092 = 5'hc == rd ? _next_reg_rd_25 : _GEN_1057; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1135 = _T_77 ? _GEN_1092 : _GEN_1057; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1170 = 5'hc == rd ? _next_reg_T_29 : _GEN_1135; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1213 = _T_84 ? _GEN_1170 : _GEN_1135; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1248 = 5'hc == rd ? _next_reg_T_30 : _GEN_1213; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1291 = _T_91 ? _GEN_1248 : _GEN_1213; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1326 = 5'hc == rd ? _next_reg_T_31 : _GEN_1291; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1369 = _T_98 ? _GEN_1326 : _GEN_1291; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1404 = 5'hc == rd ? _next_reg_T_33[31:0] : _GEN_1369; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1447 = _T_105 ? _GEN_1404 : _GEN_1369; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1482 = 5'hc == rd ? _next_reg_T_35 : _GEN_1447; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1525 = _T_112 ? _GEN_1482 : _GEN_1447; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1560 = 5'hc == rd ? _next_reg_T_37 : _GEN_1525; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1603 = _T_119 ? _GEN_1560 : _GEN_1525; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1638 = 5'hc == rd ? _next_reg_T_41 : _GEN_1603; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1681 = _T_126 ? _GEN_1638 : _GEN_1603; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1716 = 5'hc == rd ? _next_reg_T_45 : _GEN_1681; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1750 = _T_346 ? _GEN_1716 : _GEN_1681; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1798 = _T_133 ? _GEN_1750 : _GEN_1681; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1836 = 5'hc == rd ? _next_reg_T_45 : _GEN_1798; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1870 = _T_180 ? _GEN_1836 : _GEN_1798; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1919 = _T_157 ? _GEN_1870 : _GEN_1798; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2131 = 5'hc == rd ? _next_reg_T_53 : _GEN_1919; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2212 = _T_348 ? _GEN_2131 : _GEN_1919; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2249 = 5'hc == rd ? _next_reg_T_61 : _GEN_2212; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2284 = _T_435 ? _GEN_2249 : _GEN_2212; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2330 = _T_368 ? _GEN_2284 : _GEN_2212; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2367 = 5'hc == rd ? _next_reg_T_65[31:0] : _GEN_2330; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2402 = _T_437 ? _GEN_2367 : _GEN_2330; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2448 = _T_388 ? _GEN_2402 : _GEN_2330; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2487 = 5'hc == rd ? _next_reg_T_73 : _GEN_2448; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2533 = _T_408 ? _GEN_2487 : _GEN_2448; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2568 = 5'hc == rd ? _next_reg_T_79 : _GEN_2533; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2603 = _T_435 ? _GEN_2568 : _GEN_2533; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2649 = _T_427 ? _GEN_2603 : _GEN_2533; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2784 = 5'hc == rd ? _next_reg_T_94[31:0] : _GEN_2649; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2823 = _T_539 ? _GEN_2784 : _GEN_2649; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2855 = 5'hc == rd ? _next_reg_T_85[63:32] : _GEN_2823; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2894 = _T_546 ? _GEN_2855 : _GEN_2823; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2926 = 5'hc == rd ? _next_reg_T_92[63:32] : _GEN_2894; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2965 = _T_553 ? _GEN_2926 : _GEN_2894; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_2997 = 5'hc == rd ? _next_reg_T_94[63:32] : _GEN_2965; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3036 = _T_560 ? _GEN_2997 : _GEN_2965; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3068 = 5'hc == rd ? _next_reg_T_113 : _GEN_3036; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3107 = _T_567 ? _GEN_3068 : _GEN_3036; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3139 = 5'hc == rd ? _next_reg_T_117 : _GEN_3107; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3178 = _T_574 ? _GEN_3139 : _GEN_3107; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3210 = 5'hc == rd ? _next_reg_T_131 : _GEN_3178; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3249 = _T_581 ? _GEN_3210 : _GEN_3178; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3281 = 5'hc == rd ? _next_reg_T_134 : _GEN_3249; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3320 = _T_588 ? _GEN_3281 : _GEN_3249; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_118 = _T_1 ? _GEN_45 : io_now_reg_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_153 = 5'hd == rd ? _next_reg_rd_1 : _GEN_118; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_194 = _T_7 ? _GEN_153 : _GEN_118; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_229 = 5'hd == rd ? _next_reg_rd_3 : _GEN_194; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_270 = _T_13 ? _GEN_229 : _GEN_194; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_305 = 5'hd == rd ? _next_reg_T_8 : _GEN_270; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_346 = _T_19 ? _GEN_305 : _GEN_270; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_381 = 5'hd == rd ? _next_reg_T_9 : _GEN_346; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_422 = _T_25 ? _GEN_381 : _GEN_346; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_457 = 5'hd == rd ? _next_reg_T_10 : _GEN_422; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_498 = _T_31 ? _GEN_457 : _GEN_422; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_533 = 5'hd == rd ? _next_reg_T_12[31:0] : _GEN_498; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_574 = _T_37 ? _GEN_533 : _GEN_498; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_609 = 5'hd == rd ? _next_reg_T_14 : _GEN_574; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_650 = _T_43 ? _GEN_609 : _GEN_574; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_685 = 5'hd == rd ? _next_reg_T_18 : _GEN_650; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_726 = _T_49 ? _GEN_685 : _GEN_650; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_761 = 5'hd == rd ? imm : _GEN_726; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_798 = _T_55 ? _GEN_761 : _GEN_726; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_833 = 5'hd == rd ? _T_334 : _GEN_798; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_870 = _T_59 ? _GEN_833 : _GEN_798; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_937 = 5'hd == rd ? _next_reg_T_22 : _GEN_870; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_980 = _T_63 ? _GEN_937 : _GEN_870; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1015 = 5'hd == rd ? _next_reg_rd_23 : _GEN_980; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1058 = _T_70 ? _GEN_1015 : _GEN_980; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1093 = 5'hd == rd ? _next_reg_rd_25 : _GEN_1058; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1136 = _T_77 ? _GEN_1093 : _GEN_1058; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1171 = 5'hd == rd ? _next_reg_T_29 : _GEN_1136; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1214 = _T_84 ? _GEN_1171 : _GEN_1136; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1249 = 5'hd == rd ? _next_reg_T_30 : _GEN_1214; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1292 = _T_91 ? _GEN_1249 : _GEN_1214; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1327 = 5'hd == rd ? _next_reg_T_31 : _GEN_1292; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1370 = _T_98 ? _GEN_1327 : _GEN_1292; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1405 = 5'hd == rd ? _next_reg_T_33[31:0] : _GEN_1370; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1448 = _T_105 ? _GEN_1405 : _GEN_1370; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1483 = 5'hd == rd ? _next_reg_T_35 : _GEN_1448; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1526 = _T_112 ? _GEN_1483 : _GEN_1448; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1561 = 5'hd == rd ? _next_reg_T_37 : _GEN_1526; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1604 = _T_119 ? _GEN_1561 : _GEN_1526; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1639 = 5'hd == rd ? _next_reg_T_41 : _GEN_1604; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1682 = _T_126 ? _GEN_1639 : _GEN_1604; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1717 = 5'hd == rd ? _next_reg_T_45 : _GEN_1682; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1751 = _T_346 ? _GEN_1717 : _GEN_1682; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1799 = _T_133 ? _GEN_1751 : _GEN_1682; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1837 = 5'hd == rd ? _next_reg_T_45 : _GEN_1799; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1871 = _T_180 ? _GEN_1837 : _GEN_1799; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1920 = _T_157 ? _GEN_1871 : _GEN_1799; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2132 = 5'hd == rd ? _next_reg_T_53 : _GEN_1920; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2213 = _T_348 ? _GEN_2132 : _GEN_1920; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2250 = 5'hd == rd ? _next_reg_T_61 : _GEN_2213; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2285 = _T_435 ? _GEN_2250 : _GEN_2213; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2331 = _T_368 ? _GEN_2285 : _GEN_2213; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2368 = 5'hd == rd ? _next_reg_T_65[31:0] : _GEN_2331; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2403 = _T_437 ? _GEN_2368 : _GEN_2331; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2449 = _T_388 ? _GEN_2403 : _GEN_2331; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2488 = 5'hd == rd ? _next_reg_T_73 : _GEN_2449; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2534 = _T_408 ? _GEN_2488 : _GEN_2449; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2569 = 5'hd == rd ? _next_reg_T_79 : _GEN_2534; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2604 = _T_435 ? _GEN_2569 : _GEN_2534; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2650 = _T_427 ? _GEN_2604 : _GEN_2534; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2785 = 5'hd == rd ? _next_reg_T_94[31:0] : _GEN_2650; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2824 = _T_539 ? _GEN_2785 : _GEN_2650; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2856 = 5'hd == rd ? _next_reg_T_85[63:32] : _GEN_2824; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2895 = _T_546 ? _GEN_2856 : _GEN_2824; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2927 = 5'hd == rd ? _next_reg_T_92[63:32] : _GEN_2895; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2966 = _T_553 ? _GEN_2927 : _GEN_2895; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_2998 = 5'hd == rd ? _next_reg_T_94[63:32] : _GEN_2966; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3037 = _T_560 ? _GEN_2998 : _GEN_2966; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3069 = 5'hd == rd ? _next_reg_T_113 : _GEN_3037; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3108 = _T_567 ? _GEN_3069 : _GEN_3037; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3140 = 5'hd == rd ? _next_reg_T_117 : _GEN_3108; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3179 = _T_574 ? _GEN_3140 : _GEN_3108; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3211 = 5'hd == rd ? _next_reg_T_131 : _GEN_3179; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3250 = _T_581 ? _GEN_3211 : _GEN_3179; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3282 = 5'hd == rd ? _next_reg_T_134 : _GEN_3250; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3321 = _T_588 ? _GEN_3282 : _GEN_3250; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_119 = _T_1 ? _GEN_46 : io_now_reg_14; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_154 = 5'he == rd ? _next_reg_rd_1 : _GEN_119; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_195 = _T_7 ? _GEN_154 : _GEN_119; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_230 = 5'he == rd ? _next_reg_rd_3 : _GEN_195; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_271 = _T_13 ? _GEN_230 : _GEN_195; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_306 = 5'he == rd ? _next_reg_T_8 : _GEN_271; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_347 = _T_19 ? _GEN_306 : _GEN_271; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_382 = 5'he == rd ? _next_reg_T_9 : _GEN_347; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_423 = _T_25 ? _GEN_382 : _GEN_347; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_458 = 5'he == rd ? _next_reg_T_10 : _GEN_423; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_499 = _T_31 ? _GEN_458 : _GEN_423; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_534 = 5'he == rd ? _next_reg_T_12[31:0] : _GEN_499; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_575 = _T_37 ? _GEN_534 : _GEN_499; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_610 = 5'he == rd ? _next_reg_T_14 : _GEN_575; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_651 = _T_43 ? _GEN_610 : _GEN_575; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_686 = 5'he == rd ? _next_reg_T_18 : _GEN_651; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_727 = _T_49 ? _GEN_686 : _GEN_651; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_762 = 5'he == rd ? imm : _GEN_727; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_799 = _T_55 ? _GEN_762 : _GEN_727; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_834 = 5'he == rd ? _T_334 : _GEN_799; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_871 = _T_59 ? _GEN_834 : _GEN_799; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_938 = 5'he == rd ? _next_reg_T_22 : _GEN_871; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_981 = _T_63 ? _GEN_938 : _GEN_871; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1016 = 5'he == rd ? _next_reg_rd_23 : _GEN_981; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1059 = _T_70 ? _GEN_1016 : _GEN_981; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1094 = 5'he == rd ? _next_reg_rd_25 : _GEN_1059; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1137 = _T_77 ? _GEN_1094 : _GEN_1059; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1172 = 5'he == rd ? _next_reg_T_29 : _GEN_1137; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1215 = _T_84 ? _GEN_1172 : _GEN_1137; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1250 = 5'he == rd ? _next_reg_T_30 : _GEN_1215; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1293 = _T_91 ? _GEN_1250 : _GEN_1215; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1328 = 5'he == rd ? _next_reg_T_31 : _GEN_1293; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1371 = _T_98 ? _GEN_1328 : _GEN_1293; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1406 = 5'he == rd ? _next_reg_T_33[31:0] : _GEN_1371; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1449 = _T_105 ? _GEN_1406 : _GEN_1371; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1484 = 5'he == rd ? _next_reg_T_35 : _GEN_1449; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1527 = _T_112 ? _GEN_1484 : _GEN_1449; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1562 = 5'he == rd ? _next_reg_T_37 : _GEN_1527; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1605 = _T_119 ? _GEN_1562 : _GEN_1527; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1640 = 5'he == rd ? _next_reg_T_41 : _GEN_1605; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1683 = _T_126 ? _GEN_1640 : _GEN_1605; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1718 = 5'he == rd ? _next_reg_T_45 : _GEN_1683; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1752 = _T_346 ? _GEN_1718 : _GEN_1683; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1800 = _T_133 ? _GEN_1752 : _GEN_1683; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1838 = 5'he == rd ? _next_reg_T_45 : _GEN_1800; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1872 = _T_180 ? _GEN_1838 : _GEN_1800; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1921 = _T_157 ? _GEN_1872 : _GEN_1800; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2133 = 5'he == rd ? _next_reg_T_53 : _GEN_1921; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2214 = _T_348 ? _GEN_2133 : _GEN_1921; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2251 = 5'he == rd ? _next_reg_T_61 : _GEN_2214; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2286 = _T_435 ? _GEN_2251 : _GEN_2214; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2332 = _T_368 ? _GEN_2286 : _GEN_2214; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2369 = 5'he == rd ? _next_reg_T_65[31:0] : _GEN_2332; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2404 = _T_437 ? _GEN_2369 : _GEN_2332; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2450 = _T_388 ? _GEN_2404 : _GEN_2332; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2489 = 5'he == rd ? _next_reg_T_73 : _GEN_2450; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2535 = _T_408 ? _GEN_2489 : _GEN_2450; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2570 = 5'he == rd ? _next_reg_T_79 : _GEN_2535; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2605 = _T_435 ? _GEN_2570 : _GEN_2535; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2651 = _T_427 ? _GEN_2605 : _GEN_2535; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2786 = 5'he == rd ? _next_reg_T_94[31:0] : _GEN_2651; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2825 = _T_539 ? _GEN_2786 : _GEN_2651; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2857 = 5'he == rd ? _next_reg_T_85[63:32] : _GEN_2825; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2896 = _T_546 ? _GEN_2857 : _GEN_2825; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2928 = 5'he == rd ? _next_reg_T_92[63:32] : _GEN_2896; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2967 = _T_553 ? _GEN_2928 : _GEN_2896; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_2999 = 5'he == rd ? _next_reg_T_94[63:32] : _GEN_2967; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3038 = _T_560 ? _GEN_2999 : _GEN_2967; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3070 = 5'he == rd ? _next_reg_T_113 : _GEN_3038; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3109 = _T_567 ? _GEN_3070 : _GEN_3038; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3141 = 5'he == rd ? _next_reg_T_117 : _GEN_3109; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3180 = _T_574 ? _GEN_3141 : _GEN_3109; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3212 = 5'he == rd ? _next_reg_T_131 : _GEN_3180; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3251 = _T_581 ? _GEN_3212 : _GEN_3180; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3283 = 5'he == rd ? _next_reg_T_134 : _GEN_3251; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3322 = _T_588 ? _GEN_3283 : _GEN_3251; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_120 = _T_1 ? _GEN_47 : io_now_reg_15; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_155 = 5'hf == rd ? _next_reg_rd_1 : _GEN_120; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_196 = _T_7 ? _GEN_155 : _GEN_120; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_231 = 5'hf == rd ? _next_reg_rd_3 : _GEN_196; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_272 = _T_13 ? _GEN_231 : _GEN_196; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_307 = 5'hf == rd ? _next_reg_T_8 : _GEN_272; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_348 = _T_19 ? _GEN_307 : _GEN_272; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_383 = 5'hf == rd ? _next_reg_T_9 : _GEN_348; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_424 = _T_25 ? _GEN_383 : _GEN_348; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_459 = 5'hf == rd ? _next_reg_T_10 : _GEN_424; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_500 = _T_31 ? _GEN_459 : _GEN_424; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_535 = 5'hf == rd ? _next_reg_T_12[31:0] : _GEN_500; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_576 = _T_37 ? _GEN_535 : _GEN_500; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_611 = 5'hf == rd ? _next_reg_T_14 : _GEN_576; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_652 = _T_43 ? _GEN_611 : _GEN_576; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_687 = 5'hf == rd ? _next_reg_T_18 : _GEN_652; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_728 = _T_49 ? _GEN_687 : _GEN_652; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_763 = 5'hf == rd ? imm : _GEN_728; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_800 = _T_55 ? _GEN_763 : _GEN_728; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_835 = 5'hf == rd ? _T_334 : _GEN_800; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_872 = _T_59 ? _GEN_835 : _GEN_800; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_939 = 5'hf == rd ? _next_reg_T_22 : _GEN_872; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_982 = _T_63 ? _GEN_939 : _GEN_872; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1017 = 5'hf == rd ? _next_reg_rd_23 : _GEN_982; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1060 = _T_70 ? _GEN_1017 : _GEN_982; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1095 = 5'hf == rd ? _next_reg_rd_25 : _GEN_1060; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1138 = _T_77 ? _GEN_1095 : _GEN_1060; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1173 = 5'hf == rd ? _next_reg_T_29 : _GEN_1138; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1216 = _T_84 ? _GEN_1173 : _GEN_1138; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1251 = 5'hf == rd ? _next_reg_T_30 : _GEN_1216; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1294 = _T_91 ? _GEN_1251 : _GEN_1216; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1329 = 5'hf == rd ? _next_reg_T_31 : _GEN_1294; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1372 = _T_98 ? _GEN_1329 : _GEN_1294; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1407 = 5'hf == rd ? _next_reg_T_33[31:0] : _GEN_1372; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1450 = _T_105 ? _GEN_1407 : _GEN_1372; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1485 = 5'hf == rd ? _next_reg_T_35 : _GEN_1450; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1528 = _T_112 ? _GEN_1485 : _GEN_1450; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1563 = 5'hf == rd ? _next_reg_T_37 : _GEN_1528; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1606 = _T_119 ? _GEN_1563 : _GEN_1528; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1641 = 5'hf == rd ? _next_reg_T_41 : _GEN_1606; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1684 = _T_126 ? _GEN_1641 : _GEN_1606; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1719 = 5'hf == rd ? _next_reg_T_45 : _GEN_1684; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1753 = _T_346 ? _GEN_1719 : _GEN_1684; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1801 = _T_133 ? _GEN_1753 : _GEN_1684; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1839 = 5'hf == rd ? _next_reg_T_45 : _GEN_1801; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1873 = _T_180 ? _GEN_1839 : _GEN_1801; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1922 = _T_157 ? _GEN_1873 : _GEN_1801; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2134 = 5'hf == rd ? _next_reg_T_53 : _GEN_1922; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2215 = _T_348 ? _GEN_2134 : _GEN_1922; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2252 = 5'hf == rd ? _next_reg_T_61 : _GEN_2215; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2287 = _T_435 ? _GEN_2252 : _GEN_2215; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2333 = _T_368 ? _GEN_2287 : _GEN_2215; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2370 = 5'hf == rd ? _next_reg_T_65[31:0] : _GEN_2333; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2405 = _T_437 ? _GEN_2370 : _GEN_2333; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2451 = _T_388 ? _GEN_2405 : _GEN_2333; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2490 = 5'hf == rd ? _next_reg_T_73 : _GEN_2451; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2536 = _T_408 ? _GEN_2490 : _GEN_2451; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2571 = 5'hf == rd ? _next_reg_T_79 : _GEN_2536; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2606 = _T_435 ? _GEN_2571 : _GEN_2536; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2652 = _T_427 ? _GEN_2606 : _GEN_2536; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2787 = 5'hf == rd ? _next_reg_T_94[31:0] : _GEN_2652; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2826 = _T_539 ? _GEN_2787 : _GEN_2652; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2858 = 5'hf == rd ? _next_reg_T_85[63:32] : _GEN_2826; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2897 = _T_546 ? _GEN_2858 : _GEN_2826; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2929 = 5'hf == rd ? _next_reg_T_92[63:32] : _GEN_2897; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2968 = _T_553 ? _GEN_2929 : _GEN_2897; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_3000 = 5'hf == rd ? _next_reg_T_94[63:32] : _GEN_2968; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3039 = _T_560 ? _GEN_3000 : _GEN_2968; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3071 = 5'hf == rd ? _next_reg_T_113 : _GEN_3039; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3110 = _T_567 ? _GEN_3071 : _GEN_3039; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3142 = 5'hf == rd ? _next_reg_T_117 : _GEN_3110; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3181 = _T_574 ? _GEN_3142 : _GEN_3110; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3213 = 5'hf == rd ? _next_reg_T_131 : _GEN_3181; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3252 = _T_581 ? _GEN_3213 : _GEN_3181; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3284 = 5'hf == rd ? _next_reg_T_134 : _GEN_3252; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3323 = _T_588 ? _GEN_3284 : _GEN_3252; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_121 = _T_1 ? _GEN_48 : io_now_reg_16; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_156 = 5'h10 == rd ? _next_reg_rd_1 : _GEN_121; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_197 = _T_7 ? _GEN_156 : _GEN_121; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_232 = 5'h10 == rd ? _next_reg_rd_3 : _GEN_197; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_273 = _T_13 ? _GEN_232 : _GEN_197; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_308 = 5'h10 == rd ? _next_reg_T_8 : _GEN_273; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_349 = _T_19 ? _GEN_308 : _GEN_273; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_384 = 5'h10 == rd ? _next_reg_T_9 : _GEN_349; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_425 = _T_25 ? _GEN_384 : _GEN_349; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_460 = 5'h10 == rd ? _next_reg_T_10 : _GEN_425; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_501 = _T_31 ? _GEN_460 : _GEN_425; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_536 = 5'h10 == rd ? _next_reg_T_12[31:0] : _GEN_501; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_577 = _T_37 ? _GEN_536 : _GEN_501; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_612 = 5'h10 == rd ? _next_reg_T_14 : _GEN_577; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_653 = _T_43 ? _GEN_612 : _GEN_577; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_688 = 5'h10 == rd ? _next_reg_T_18 : _GEN_653; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_729 = _T_49 ? _GEN_688 : _GEN_653; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_764 = 5'h10 == rd ? imm : _GEN_729; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_801 = _T_55 ? _GEN_764 : _GEN_729; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_836 = 5'h10 == rd ? _T_334 : _GEN_801; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_873 = _T_59 ? _GEN_836 : _GEN_801; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_940 = 5'h10 == rd ? _next_reg_T_22 : _GEN_873; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_983 = _T_63 ? _GEN_940 : _GEN_873; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1018 = 5'h10 == rd ? _next_reg_rd_23 : _GEN_983; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1061 = _T_70 ? _GEN_1018 : _GEN_983; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1096 = 5'h10 == rd ? _next_reg_rd_25 : _GEN_1061; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1139 = _T_77 ? _GEN_1096 : _GEN_1061; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1174 = 5'h10 == rd ? _next_reg_T_29 : _GEN_1139; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1217 = _T_84 ? _GEN_1174 : _GEN_1139; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1252 = 5'h10 == rd ? _next_reg_T_30 : _GEN_1217; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1295 = _T_91 ? _GEN_1252 : _GEN_1217; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1330 = 5'h10 == rd ? _next_reg_T_31 : _GEN_1295; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1373 = _T_98 ? _GEN_1330 : _GEN_1295; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1408 = 5'h10 == rd ? _next_reg_T_33[31:0] : _GEN_1373; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1451 = _T_105 ? _GEN_1408 : _GEN_1373; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1486 = 5'h10 == rd ? _next_reg_T_35 : _GEN_1451; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1529 = _T_112 ? _GEN_1486 : _GEN_1451; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1564 = 5'h10 == rd ? _next_reg_T_37 : _GEN_1529; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1607 = _T_119 ? _GEN_1564 : _GEN_1529; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1642 = 5'h10 == rd ? _next_reg_T_41 : _GEN_1607; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1685 = _T_126 ? _GEN_1642 : _GEN_1607; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1720 = 5'h10 == rd ? _next_reg_T_45 : _GEN_1685; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1754 = _T_346 ? _GEN_1720 : _GEN_1685; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1802 = _T_133 ? _GEN_1754 : _GEN_1685; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1840 = 5'h10 == rd ? _next_reg_T_45 : _GEN_1802; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1874 = _T_180 ? _GEN_1840 : _GEN_1802; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1923 = _T_157 ? _GEN_1874 : _GEN_1802; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2135 = 5'h10 == rd ? _next_reg_T_53 : _GEN_1923; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2216 = _T_348 ? _GEN_2135 : _GEN_1923; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2253 = 5'h10 == rd ? _next_reg_T_61 : _GEN_2216; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2288 = _T_435 ? _GEN_2253 : _GEN_2216; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2334 = _T_368 ? _GEN_2288 : _GEN_2216; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2371 = 5'h10 == rd ? _next_reg_T_65[31:0] : _GEN_2334; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2406 = _T_437 ? _GEN_2371 : _GEN_2334; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2452 = _T_388 ? _GEN_2406 : _GEN_2334; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2491 = 5'h10 == rd ? _next_reg_T_73 : _GEN_2452; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2537 = _T_408 ? _GEN_2491 : _GEN_2452; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2572 = 5'h10 == rd ? _next_reg_T_79 : _GEN_2537; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2607 = _T_435 ? _GEN_2572 : _GEN_2537; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2653 = _T_427 ? _GEN_2607 : _GEN_2537; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2788 = 5'h10 == rd ? _next_reg_T_94[31:0] : _GEN_2653; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2827 = _T_539 ? _GEN_2788 : _GEN_2653; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2859 = 5'h10 == rd ? _next_reg_T_85[63:32] : _GEN_2827; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2898 = _T_546 ? _GEN_2859 : _GEN_2827; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2930 = 5'h10 == rd ? _next_reg_T_92[63:32] : _GEN_2898; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2969 = _T_553 ? _GEN_2930 : _GEN_2898; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_3001 = 5'h10 == rd ? _next_reg_T_94[63:32] : _GEN_2969; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3040 = _T_560 ? _GEN_3001 : _GEN_2969; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3072 = 5'h10 == rd ? _next_reg_T_113 : _GEN_3040; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3111 = _T_567 ? _GEN_3072 : _GEN_3040; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3143 = 5'h10 == rd ? _next_reg_T_117 : _GEN_3111; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3182 = _T_574 ? _GEN_3143 : _GEN_3111; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3214 = 5'h10 == rd ? _next_reg_T_131 : _GEN_3182; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3253 = _T_581 ? _GEN_3214 : _GEN_3182; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3285 = 5'h10 == rd ? _next_reg_T_134 : _GEN_3253; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3324 = _T_588 ? _GEN_3285 : _GEN_3253; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_122 = _T_1 ? _GEN_49 : io_now_reg_17; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_157 = 5'h11 == rd ? _next_reg_rd_1 : _GEN_122; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_198 = _T_7 ? _GEN_157 : _GEN_122; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_233 = 5'h11 == rd ? _next_reg_rd_3 : _GEN_198; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_274 = _T_13 ? _GEN_233 : _GEN_198; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_309 = 5'h11 == rd ? _next_reg_T_8 : _GEN_274; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_350 = _T_19 ? _GEN_309 : _GEN_274; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_385 = 5'h11 == rd ? _next_reg_T_9 : _GEN_350; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_426 = _T_25 ? _GEN_385 : _GEN_350; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_461 = 5'h11 == rd ? _next_reg_T_10 : _GEN_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_502 = _T_31 ? _GEN_461 : _GEN_426; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_537 = 5'h11 == rd ? _next_reg_T_12[31:0] : _GEN_502; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_578 = _T_37 ? _GEN_537 : _GEN_502; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_613 = 5'h11 == rd ? _next_reg_T_14 : _GEN_578; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_654 = _T_43 ? _GEN_613 : _GEN_578; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_689 = 5'h11 == rd ? _next_reg_T_18 : _GEN_654; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_730 = _T_49 ? _GEN_689 : _GEN_654; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_765 = 5'h11 == rd ? imm : _GEN_730; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_802 = _T_55 ? _GEN_765 : _GEN_730; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_837 = 5'h11 == rd ? _T_334 : _GEN_802; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_874 = _T_59 ? _GEN_837 : _GEN_802; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_941 = 5'h11 == rd ? _next_reg_T_22 : _GEN_874; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_984 = _T_63 ? _GEN_941 : _GEN_874; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1019 = 5'h11 == rd ? _next_reg_rd_23 : _GEN_984; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1062 = _T_70 ? _GEN_1019 : _GEN_984; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1097 = 5'h11 == rd ? _next_reg_rd_25 : _GEN_1062; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1140 = _T_77 ? _GEN_1097 : _GEN_1062; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1175 = 5'h11 == rd ? _next_reg_T_29 : _GEN_1140; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1218 = _T_84 ? _GEN_1175 : _GEN_1140; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1253 = 5'h11 == rd ? _next_reg_T_30 : _GEN_1218; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1296 = _T_91 ? _GEN_1253 : _GEN_1218; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1331 = 5'h11 == rd ? _next_reg_T_31 : _GEN_1296; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1374 = _T_98 ? _GEN_1331 : _GEN_1296; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1409 = 5'h11 == rd ? _next_reg_T_33[31:0] : _GEN_1374; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1452 = _T_105 ? _GEN_1409 : _GEN_1374; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1487 = 5'h11 == rd ? _next_reg_T_35 : _GEN_1452; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1530 = _T_112 ? _GEN_1487 : _GEN_1452; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1565 = 5'h11 == rd ? _next_reg_T_37 : _GEN_1530; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1608 = _T_119 ? _GEN_1565 : _GEN_1530; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1643 = 5'h11 == rd ? _next_reg_T_41 : _GEN_1608; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1686 = _T_126 ? _GEN_1643 : _GEN_1608; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1721 = 5'h11 == rd ? _next_reg_T_45 : _GEN_1686; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1755 = _T_346 ? _GEN_1721 : _GEN_1686; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1803 = _T_133 ? _GEN_1755 : _GEN_1686; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1841 = 5'h11 == rd ? _next_reg_T_45 : _GEN_1803; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1875 = _T_180 ? _GEN_1841 : _GEN_1803; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1924 = _T_157 ? _GEN_1875 : _GEN_1803; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2136 = 5'h11 == rd ? _next_reg_T_53 : _GEN_1924; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2217 = _T_348 ? _GEN_2136 : _GEN_1924; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2254 = 5'h11 == rd ? _next_reg_T_61 : _GEN_2217; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2289 = _T_435 ? _GEN_2254 : _GEN_2217; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2335 = _T_368 ? _GEN_2289 : _GEN_2217; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2372 = 5'h11 == rd ? _next_reg_T_65[31:0] : _GEN_2335; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2407 = _T_437 ? _GEN_2372 : _GEN_2335; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2453 = _T_388 ? _GEN_2407 : _GEN_2335; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2492 = 5'h11 == rd ? _next_reg_T_73 : _GEN_2453; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2538 = _T_408 ? _GEN_2492 : _GEN_2453; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2573 = 5'h11 == rd ? _next_reg_T_79 : _GEN_2538; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2608 = _T_435 ? _GEN_2573 : _GEN_2538; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2654 = _T_427 ? _GEN_2608 : _GEN_2538; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2789 = 5'h11 == rd ? _next_reg_T_94[31:0] : _GEN_2654; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2828 = _T_539 ? _GEN_2789 : _GEN_2654; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2860 = 5'h11 == rd ? _next_reg_T_85[63:32] : _GEN_2828; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2899 = _T_546 ? _GEN_2860 : _GEN_2828; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2931 = 5'h11 == rd ? _next_reg_T_92[63:32] : _GEN_2899; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2970 = _T_553 ? _GEN_2931 : _GEN_2899; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_3002 = 5'h11 == rd ? _next_reg_T_94[63:32] : _GEN_2970; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3041 = _T_560 ? _GEN_3002 : _GEN_2970; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3073 = 5'h11 == rd ? _next_reg_T_113 : _GEN_3041; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3112 = _T_567 ? _GEN_3073 : _GEN_3041; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3144 = 5'h11 == rd ? _next_reg_T_117 : _GEN_3112; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3183 = _T_574 ? _GEN_3144 : _GEN_3112; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3215 = 5'h11 == rd ? _next_reg_T_131 : _GEN_3183; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3254 = _T_581 ? _GEN_3215 : _GEN_3183; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3286 = 5'h11 == rd ? _next_reg_T_134 : _GEN_3254; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3325 = _T_588 ? _GEN_3286 : _GEN_3254; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_123 = _T_1 ? _GEN_50 : io_now_reg_18; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_158 = 5'h12 == rd ? _next_reg_rd_1 : _GEN_123; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_199 = _T_7 ? _GEN_158 : _GEN_123; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_234 = 5'h12 == rd ? _next_reg_rd_3 : _GEN_199; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_275 = _T_13 ? _GEN_234 : _GEN_199; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_310 = 5'h12 == rd ? _next_reg_T_8 : _GEN_275; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_351 = _T_19 ? _GEN_310 : _GEN_275; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_386 = 5'h12 == rd ? _next_reg_T_9 : _GEN_351; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_427 = _T_25 ? _GEN_386 : _GEN_351; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_462 = 5'h12 == rd ? _next_reg_T_10 : _GEN_427; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_503 = _T_31 ? _GEN_462 : _GEN_427; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_538 = 5'h12 == rd ? _next_reg_T_12[31:0] : _GEN_503; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_579 = _T_37 ? _GEN_538 : _GEN_503; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_614 = 5'h12 == rd ? _next_reg_T_14 : _GEN_579; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_655 = _T_43 ? _GEN_614 : _GEN_579; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_690 = 5'h12 == rd ? _next_reg_T_18 : _GEN_655; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_731 = _T_49 ? _GEN_690 : _GEN_655; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_766 = 5'h12 == rd ? imm : _GEN_731; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_803 = _T_55 ? _GEN_766 : _GEN_731; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_838 = 5'h12 == rd ? _T_334 : _GEN_803; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_875 = _T_59 ? _GEN_838 : _GEN_803; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_942 = 5'h12 == rd ? _next_reg_T_22 : _GEN_875; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_985 = _T_63 ? _GEN_942 : _GEN_875; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1020 = 5'h12 == rd ? _next_reg_rd_23 : _GEN_985; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1063 = _T_70 ? _GEN_1020 : _GEN_985; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1098 = 5'h12 == rd ? _next_reg_rd_25 : _GEN_1063; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1141 = _T_77 ? _GEN_1098 : _GEN_1063; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1176 = 5'h12 == rd ? _next_reg_T_29 : _GEN_1141; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1219 = _T_84 ? _GEN_1176 : _GEN_1141; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1254 = 5'h12 == rd ? _next_reg_T_30 : _GEN_1219; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1297 = _T_91 ? _GEN_1254 : _GEN_1219; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1332 = 5'h12 == rd ? _next_reg_T_31 : _GEN_1297; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1375 = _T_98 ? _GEN_1332 : _GEN_1297; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1410 = 5'h12 == rd ? _next_reg_T_33[31:0] : _GEN_1375; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1453 = _T_105 ? _GEN_1410 : _GEN_1375; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1488 = 5'h12 == rd ? _next_reg_T_35 : _GEN_1453; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1531 = _T_112 ? _GEN_1488 : _GEN_1453; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1566 = 5'h12 == rd ? _next_reg_T_37 : _GEN_1531; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1609 = _T_119 ? _GEN_1566 : _GEN_1531; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1644 = 5'h12 == rd ? _next_reg_T_41 : _GEN_1609; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1687 = _T_126 ? _GEN_1644 : _GEN_1609; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1722 = 5'h12 == rd ? _next_reg_T_45 : _GEN_1687; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1756 = _T_346 ? _GEN_1722 : _GEN_1687; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1804 = _T_133 ? _GEN_1756 : _GEN_1687; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1842 = 5'h12 == rd ? _next_reg_T_45 : _GEN_1804; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1876 = _T_180 ? _GEN_1842 : _GEN_1804; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1925 = _T_157 ? _GEN_1876 : _GEN_1804; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2137 = 5'h12 == rd ? _next_reg_T_53 : _GEN_1925; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2218 = _T_348 ? _GEN_2137 : _GEN_1925; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2255 = 5'h12 == rd ? _next_reg_T_61 : _GEN_2218; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2290 = _T_435 ? _GEN_2255 : _GEN_2218; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2336 = _T_368 ? _GEN_2290 : _GEN_2218; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2373 = 5'h12 == rd ? _next_reg_T_65[31:0] : _GEN_2336; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2408 = _T_437 ? _GEN_2373 : _GEN_2336; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2454 = _T_388 ? _GEN_2408 : _GEN_2336; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2493 = 5'h12 == rd ? _next_reg_T_73 : _GEN_2454; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2539 = _T_408 ? _GEN_2493 : _GEN_2454; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2574 = 5'h12 == rd ? _next_reg_T_79 : _GEN_2539; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2609 = _T_435 ? _GEN_2574 : _GEN_2539; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2655 = _T_427 ? _GEN_2609 : _GEN_2539; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2790 = 5'h12 == rd ? _next_reg_T_94[31:0] : _GEN_2655; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2829 = _T_539 ? _GEN_2790 : _GEN_2655; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2861 = 5'h12 == rd ? _next_reg_T_85[63:32] : _GEN_2829; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2900 = _T_546 ? _GEN_2861 : _GEN_2829; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2932 = 5'h12 == rd ? _next_reg_T_92[63:32] : _GEN_2900; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2971 = _T_553 ? _GEN_2932 : _GEN_2900; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_3003 = 5'h12 == rd ? _next_reg_T_94[63:32] : _GEN_2971; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3042 = _T_560 ? _GEN_3003 : _GEN_2971; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3074 = 5'h12 == rd ? _next_reg_T_113 : _GEN_3042; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3113 = _T_567 ? _GEN_3074 : _GEN_3042; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3145 = 5'h12 == rd ? _next_reg_T_117 : _GEN_3113; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3184 = _T_574 ? _GEN_3145 : _GEN_3113; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3216 = 5'h12 == rd ? _next_reg_T_131 : _GEN_3184; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3255 = _T_581 ? _GEN_3216 : _GEN_3184; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3287 = 5'h12 == rd ? _next_reg_T_134 : _GEN_3255; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3326 = _T_588 ? _GEN_3287 : _GEN_3255; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_124 = _T_1 ? _GEN_51 : io_now_reg_19; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_159 = 5'h13 == rd ? _next_reg_rd_1 : _GEN_124; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_200 = _T_7 ? _GEN_159 : _GEN_124; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_235 = 5'h13 == rd ? _next_reg_rd_3 : _GEN_200; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_276 = _T_13 ? _GEN_235 : _GEN_200; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_311 = 5'h13 == rd ? _next_reg_T_8 : _GEN_276; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_352 = _T_19 ? _GEN_311 : _GEN_276; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_387 = 5'h13 == rd ? _next_reg_T_9 : _GEN_352; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_428 = _T_25 ? _GEN_387 : _GEN_352; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_463 = 5'h13 == rd ? _next_reg_T_10 : _GEN_428; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_504 = _T_31 ? _GEN_463 : _GEN_428; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_539 = 5'h13 == rd ? _next_reg_T_12[31:0] : _GEN_504; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_580 = _T_37 ? _GEN_539 : _GEN_504; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_615 = 5'h13 == rd ? _next_reg_T_14 : _GEN_580; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_656 = _T_43 ? _GEN_615 : _GEN_580; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_691 = 5'h13 == rd ? _next_reg_T_18 : _GEN_656; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_732 = _T_49 ? _GEN_691 : _GEN_656; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_767 = 5'h13 == rd ? imm : _GEN_732; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_804 = _T_55 ? _GEN_767 : _GEN_732; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_839 = 5'h13 == rd ? _T_334 : _GEN_804; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_876 = _T_59 ? _GEN_839 : _GEN_804; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_943 = 5'h13 == rd ? _next_reg_T_22 : _GEN_876; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_986 = _T_63 ? _GEN_943 : _GEN_876; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1021 = 5'h13 == rd ? _next_reg_rd_23 : _GEN_986; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1064 = _T_70 ? _GEN_1021 : _GEN_986; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1099 = 5'h13 == rd ? _next_reg_rd_25 : _GEN_1064; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1142 = _T_77 ? _GEN_1099 : _GEN_1064; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1177 = 5'h13 == rd ? _next_reg_T_29 : _GEN_1142; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1220 = _T_84 ? _GEN_1177 : _GEN_1142; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1255 = 5'h13 == rd ? _next_reg_T_30 : _GEN_1220; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1298 = _T_91 ? _GEN_1255 : _GEN_1220; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1333 = 5'h13 == rd ? _next_reg_T_31 : _GEN_1298; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1376 = _T_98 ? _GEN_1333 : _GEN_1298; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1411 = 5'h13 == rd ? _next_reg_T_33[31:0] : _GEN_1376; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1454 = _T_105 ? _GEN_1411 : _GEN_1376; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1489 = 5'h13 == rd ? _next_reg_T_35 : _GEN_1454; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1532 = _T_112 ? _GEN_1489 : _GEN_1454; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1567 = 5'h13 == rd ? _next_reg_T_37 : _GEN_1532; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1610 = _T_119 ? _GEN_1567 : _GEN_1532; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1645 = 5'h13 == rd ? _next_reg_T_41 : _GEN_1610; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1688 = _T_126 ? _GEN_1645 : _GEN_1610; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1723 = 5'h13 == rd ? _next_reg_T_45 : _GEN_1688; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1757 = _T_346 ? _GEN_1723 : _GEN_1688; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1805 = _T_133 ? _GEN_1757 : _GEN_1688; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1843 = 5'h13 == rd ? _next_reg_T_45 : _GEN_1805; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1877 = _T_180 ? _GEN_1843 : _GEN_1805; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1926 = _T_157 ? _GEN_1877 : _GEN_1805; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2138 = 5'h13 == rd ? _next_reg_T_53 : _GEN_1926; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2219 = _T_348 ? _GEN_2138 : _GEN_1926; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2256 = 5'h13 == rd ? _next_reg_T_61 : _GEN_2219; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2291 = _T_435 ? _GEN_2256 : _GEN_2219; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2337 = _T_368 ? _GEN_2291 : _GEN_2219; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2374 = 5'h13 == rd ? _next_reg_T_65[31:0] : _GEN_2337; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2409 = _T_437 ? _GEN_2374 : _GEN_2337; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2455 = _T_388 ? _GEN_2409 : _GEN_2337; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2494 = 5'h13 == rd ? _next_reg_T_73 : _GEN_2455; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2540 = _T_408 ? _GEN_2494 : _GEN_2455; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2575 = 5'h13 == rd ? _next_reg_T_79 : _GEN_2540; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2610 = _T_435 ? _GEN_2575 : _GEN_2540; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2656 = _T_427 ? _GEN_2610 : _GEN_2540; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2791 = 5'h13 == rd ? _next_reg_T_94[31:0] : _GEN_2656; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2830 = _T_539 ? _GEN_2791 : _GEN_2656; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2862 = 5'h13 == rd ? _next_reg_T_85[63:32] : _GEN_2830; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2901 = _T_546 ? _GEN_2862 : _GEN_2830; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2933 = 5'h13 == rd ? _next_reg_T_92[63:32] : _GEN_2901; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2972 = _T_553 ? _GEN_2933 : _GEN_2901; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_3004 = 5'h13 == rd ? _next_reg_T_94[63:32] : _GEN_2972; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3043 = _T_560 ? _GEN_3004 : _GEN_2972; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3075 = 5'h13 == rd ? _next_reg_T_113 : _GEN_3043; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3114 = _T_567 ? _GEN_3075 : _GEN_3043; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3146 = 5'h13 == rd ? _next_reg_T_117 : _GEN_3114; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3185 = _T_574 ? _GEN_3146 : _GEN_3114; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3217 = 5'h13 == rd ? _next_reg_T_131 : _GEN_3185; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3256 = _T_581 ? _GEN_3217 : _GEN_3185; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3288 = 5'h13 == rd ? _next_reg_T_134 : _GEN_3256; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3327 = _T_588 ? _GEN_3288 : _GEN_3256; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_125 = _T_1 ? _GEN_52 : io_now_reg_20; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_160 = 5'h14 == rd ? _next_reg_rd_1 : _GEN_125; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_201 = _T_7 ? _GEN_160 : _GEN_125; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_236 = 5'h14 == rd ? _next_reg_rd_3 : _GEN_201; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_277 = _T_13 ? _GEN_236 : _GEN_201; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_312 = 5'h14 == rd ? _next_reg_T_8 : _GEN_277; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_353 = _T_19 ? _GEN_312 : _GEN_277; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_388 = 5'h14 == rd ? _next_reg_T_9 : _GEN_353; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_429 = _T_25 ? _GEN_388 : _GEN_353; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_464 = 5'h14 == rd ? _next_reg_T_10 : _GEN_429; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_505 = _T_31 ? _GEN_464 : _GEN_429; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_540 = 5'h14 == rd ? _next_reg_T_12[31:0] : _GEN_505; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_581 = _T_37 ? _GEN_540 : _GEN_505; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_616 = 5'h14 == rd ? _next_reg_T_14 : _GEN_581; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_657 = _T_43 ? _GEN_616 : _GEN_581; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_692 = 5'h14 == rd ? _next_reg_T_18 : _GEN_657; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_733 = _T_49 ? _GEN_692 : _GEN_657; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_768 = 5'h14 == rd ? imm : _GEN_733; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_805 = _T_55 ? _GEN_768 : _GEN_733; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_840 = 5'h14 == rd ? _T_334 : _GEN_805; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_877 = _T_59 ? _GEN_840 : _GEN_805; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_944 = 5'h14 == rd ? _next_reg_T_22 : _GEN_877; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_987 = _T_63 ? _GEN_944 : _GEN_877; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1022 = 5'h14 == rd ? _next_reg_rd_23 : _GEN_987; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1065 = _T_70 ? _GEN_1022 : _GEN_987; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1100 = 5'h14 == rd ? _next_reg_rd_25 : _GEN_1065; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1143 = _T_77 ? _GEN_1100 : _GEN_1065; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1178 = 5'h14 == rd ? _next_reg_T_29 : _GEN_1143; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1221 = _T_84 ? _GEN_1178 : _GEN_1143; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1256 = 5'h14 == rd ? _next_reg_T_30 : _GEN_1221; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1299 = _T_91 ? _GEN_1256 : _GEN_1221; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1334 = 5'h14 == rd ? _next_reg_T_31 : _GEN_1299; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1377 = _T_98 ? _GEN_1334 : _GEN_1299; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1412 = 5'h14 == rd ? _next_reg_T_33[31:0] : _GEN_1377; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1455 = _T_105 ? _GEN_1412 : _GEN_1377; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1490 = 5'h14 == rd ? _next_reg_T_35 : _GEN_1455; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1533 = _T_112 ? _GEN_1490 : _GEN_1455; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1568 = 5'h14 == rd ? _next_reg_T_37 : _GEN_1533; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1611 = _T_119 ? _GEN_1568 : _GEN_1533; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1646 = 5'h14 == rd ? _next_reg_T_41 : _GEN_1611; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1689 = _T_126 ? _GEN_1646 : _GEN_1611; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1724 = 5'h14 == rd ? _next_reg_T_45 : _GEN_1689; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1758 = _T_346 ? _GEN_1724 : _GEN_1689; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1806 = _T_133 ? _GEN_1758 : _GEN_1689; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1844 = 5'h14 == rd ? _next_reg_T_45 : _GEN_1806; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1878 = _T_180 ? _GEN_1844 : _GEN_1806; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1927 = _T_157 ? _GEN_1878 : _GEN_1806; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2139 = 5'h14 == rd ? _next_reg_T_53 : _GEN_1927; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2220 = _T_348 ? _GEN_2139 : _GEN_1927; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2257 = 5'h14 == rd ? _next_reg_T_61 : _GEN_2220; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2292 = _T_435 ? _GEN_2257 : _GEN_2220; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2338 = _T_368 ? _GEN_2292 : _GEN_2220; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2375 = 5'h14 == rd ? _next_reg_T_65[31:0] : _GEN_2338; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2410 = _T_437 ? _GEN_2375 : _GEN_2338; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2456 = _T_388 ? _GEN_2410 : _GEN_2338; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2495 = 5'h14 == rd ? _next_reg_T_73 : _GEN_2456; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2541 = _T_408 ? _GEN_2495 : _GEN_2456; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2576 = 5'h14 == rd ? _next_reg_T_79 : _GEN_2541; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2611 = _T_435 ? _GEN_2576 : _GEN_2541; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2657 = _T_427 ? _GEN_2611 : _GEN_2541; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2792 = 5'h14 == rd ? _next_reg_T_94[31:0] : _GEN_2657; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2831 = _T_539 ? _GEN_2792 : _GEN_2657; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2863 = 5'h14 == rd ? _next_reg_T_85[63:32] : _GEN_2831; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2902 = _T_546 ? _GEN_2863 : _GEN_2831; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2934 = 5'h14 == rd ? _next_reg_T_92[63:32] : _GEN_2902; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2973 = _T_553 ? _GEN_2934 : _GEN_2902; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_3005 = 5'h14 == rd ? _next_reg_T_94[63:32] : _GEN_2973; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3044 = _T_560 ? _GEN_3005 : _GEN_2973; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3076 = 5'h14 == rd ? _next_reg_T_113 : _GEN_3044; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3115 = _T_567 ? _GEN_3076 : _GEN_3044; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3147 = 5'h14 == rd ? _next_reg_T_117 : _GEN_3115; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3186 = _T_574 ? _GEN_3147 : _GEN_3115; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3218 = 5'h14 == rd ? _next_reg_T_131 : _GEN_3186; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3257 = _T_581 ? _GEN_3218 : _GEN_3186; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3289 = 5'h14 == rd ? _next_reg_T_134 : _GEN_3257; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3328 = _T_588 ? _GEN_3289 : _GEN_3257; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_126 = _T_1 ? _GEN_53 : io_now_reg_21; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_161 = 5'h15 == rd ? _next_reg_rd_1 : _GEN_126; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_202 = _T_7 ? _GEN_161 : _GEN_126; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_237 = 5'h15 == rd ? _next_reg_rd_3 : _GEN_202; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_278 = _T_13 ? _GEN_237 : _GEN_202; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_313 = 5'h15 == rd ? _next_reg_T_8 : _GEN_278; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_354 = _T_19 ? _GEN_313 : _GEN_278; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_389 = 5'h15 == rd ? _next_reg_T_9 : _GEN_354; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_430 = _T_25 ? _GEN_389 : _GEN_354; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_465 = 5'h15 == rd ? _next_reg_T_10 : _GEN_430; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_506 = _T_31 ? _GEN_465 : _GEN_430; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_541 = 5'h15 == rd ? _next_reg_T_12[31:0] : _GEN_506; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_582 = _T_37 ? _GEN_541 : _GEN_506; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_617 = 5'h15 == rd ? _next_reg_T_14 : _GEN_582; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_658 = _T_43 ? _GEN_617 : _GEN_582; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_693 = 5'h15 == rd ? _next_reg_T_18 : _GEN_658; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_734 = _T_49 ? _GEN_693 : _GEN_658; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_769 = 5'h15 == rd ? imm : _GEN_734; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_806 = _T_55 ? _GEN_769 : _GEN_734; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_841 = 5'h15 == rd ? _T_334 : _GEN_806; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_878 = _T_59 ? _GEN_841 : _GEN_806; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_945 = 5'h15 == rd ? _next_reg_T_22 : _GEN_878; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_988 = _T_63 ? _GEN_945 : _GEN_878; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1023 = 5'h15 == rd ? _next_reg_rd_23 : _GEN_988; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1066 = _T_70 ? _GEN_1023 : _GEN_988; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1101 = 5'h15 == rd ? _next_reg_rd_25 : _GEN_1066; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1144 = _T_77 ? _GEN_1101 : _GEN_1066; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1179 = 5'h15 == rd ? _next_reg_T_29 : _GEN_1144; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1222 = _T_84 ? _GEN_1179 : _GEN_1144; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1257 = 5'h15 == rd ? _next_reg_T_30 : _GEN_1222; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1300 = _T_91 ? _GEN_1257 : _GEN_1222; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1335 = 5'h15 == rd ? _next_reg_T_31 : _GEN_1300; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1378 = _T_98 ? _GEN_1335 : _GEN_1300; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1413 = 5'h15 == rd ? _next_reg_T_33[31:0] : _GEN_1378; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1456 = _T_105 ? _GEN_1413 : _GEN_1378; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1491 = 5'h15 == rd ? _next_reg_T_35 : _GEN_1456; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1534 = _T_112 ? _GEN_1491 : _GEN_1456; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1569 = 5'h15 == rd ? _next_reg_T_37 : _GEN_1534; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1612 = _T_119 ? _GEN_1569 : _GEN_1534; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1647 = 5'h15 == rd ? _next_reg_T_41 : _GEN_1612; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1690 = _T_126 ? _GEN_1647 : _GEN_1612; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1725 = 5'h15 == rd ? _next_reg_T_45 : _GEN_1690; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1759 = _T_346 ? _GEN_1725 : _GEN_1690; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1807 = _T_133 ? _GEN_1759 : _GEN_1690; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1845 = 5'h15 == rd ? _next_reg_T_45 : _GEN_1807; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1879 = _T_180 ? _GEN_1845 : _GEN_1807; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1928 = _T_157 ? _GEN_1879 : _GEN_1807; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2140 = 5'h15 == rd ? _next_reg_T_53 : _GEN_1928; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2221 = _T_348 ? _GEN_2140 : _GEN_1928; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2258 = 5'h15 == rd ? _next_reg_T_61 : _GEN_2221; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2293 = _T_435 ? _GEN_2258 : _GEN_2221; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2339 = _T_368 ? _GEN_2293 : _GEN_2221; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2376 = 5'h15 == rd ? _next_reg_T_65[31:0] : _GEN_2339; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2411 = _T_437 ? _GEN_2376 : _GEN_2339; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2457 = _T_388 ? _GEN_2411 : _GEN_2339; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2496 = 5'h15 == rd ? _next_reg_T_73 : _GEN_2457; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2542 = _T_408 ? _GEN_2496 : _GEN_2457; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2577 = 5'h15 == rd ? _next_reg_T_79 : _GEN_2542; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2612 = _T_435 ? _GEN_2577 : _GEN_2542; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2658 = _T_427 ? _GEN_2612 : _GEN_2542; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2793 = 5'h15 == rd ? _next_reg_T_94[31:0] : _GEN_2658; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2832 = _T_539 ? _GEN_2793 : _GEN_2658; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2864 = 5'h15 == rd ? _next_reg_T_85[63:32] : _GEN_2832; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2903 = _T_546 ? _GEN_2864 : _GEN_2832; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2935 = 5'h15 == rd ? _next_reg_T_92[63:32] : _GEN_2903; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2974 = _T_553 ? _GEN_2935 : _GEN_2903; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_3006 = 5'h15 == rd ? _next_reg_T_94[63:32] : _GEN_2974; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3045 = _T_560 ? _GEN_3006 : _GEN_2974; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3077 = 5'h15 == rd ? _next_reg_T_113 : _GEN_3045; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3116 = _T_567 ? _GEN_3077 : _GEN_3045; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3148 = 5'h15 == rd ? _next_reg_T_117 : _GEN_3116; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3187 = _T_574 ? _GEN_3148 : _GEN_3116; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3219 = 5'h15 == rd ? _next_reg_T_131 : _GEN_3187; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3258 = _T_581 ? _GEN_3219 : _GEN_3187; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3290 = 5'h15 == rd ? _next_reg_T_134 : _GEN_3258; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3329 = _T_588 ? _GEN_3290 : _GEN_3258; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_127 = _T_1 ? _GEN_54 : io_now_reg_22; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_162 = 5'h16 == rd ? _next_reg_rd_1 : _GEN_127; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_203 = _T_7 ? _GEN_162 : _GEN_127; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_238 = 5'h16 == rd ? _next_reg_rd_3 : _GEN_203; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_279 = _T_13 ? _GEN_238 : _GEN_203; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_314 = 5'h16 == rd ? _next_reg_T_8 : _GEN_279; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_355 = _T_19 ? _GEN_314 : _GEN_279; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_390 = 5'h16 == rd ? _next_reg_T_9 : _GEN_355; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_431 = _T_25 ? _GEN_390 : _GEN_355; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_466 = 5'h16 == rd ? _next_reg_T_10 : _GEN_431; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_507 = _T_31 ? _GEN_466 : _GEN_431; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_542 = 5'h16 == rd ? _next_reg_T_12[31:0] : _GEN_507; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_583 = _T_37 ? _GEN_542 : _GEN_507; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_618 = 5'h16 == rd ? _next_reg_T_14 : _GEN_583; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_659 = _T_43 ? _GEN_618 : _GEN_583; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_694 = 5'h16 == rd ? _next_reg_T_18 : _GEN_659; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_735 = _T_49 ? _GEN_694 : _GEN_659; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_770 = 5'h16 == rd ? imm : _GEN_735; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_807 = _T_55 ? _GEN_770 : _GEN_735; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_842 = 5'h16 == rd ? _T_334 : _GEN_807; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_879 = _T_59 ? _GEN_842 : _GEN_807; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_946 = 5'h16 == rd ? _next_reg_T_22 : _GEN_879; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_989 = _T_63 ? _GEN_946 : _GEN_879; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1024 = 5'h16 == rd ? _next_reg_rd_23 : _GEN_989; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1067 = _T_70 ? _GEN_1024 : _GEN_989; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1102 = 5'h16 == rd ? _next_reg_rd_25 : _GEN_1067; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1145 = _T_77 ? _GEN_1102 : _GEN_1067; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1180 = 5'h16 == rd ? _next_reg_T_29 : _GEN_1145; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1223 = _T_84 ? _GEN_1180 : _GEN_1145; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1258 = 5'h16 == rd ? _next_reg_T_30 : _GEN_1223; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1301 = _T_91 ? _GEN_1258 : _GEN_1223; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1336 = 5'h16 == rd ? _next_reg_T_31 : _GEN_1301; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1379 = _T_98 ? _GEN_1336 : _GEN_1301; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1414 = 5'h16 == rd ? _next_reg_T_33[31:0] : _GEN_1379; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1457 = _T_105 ? _GEN_1414 : _GEN_1379; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1492 = 5'h16 == rd ? _next_reg_T_35 : _GEN_1457; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1535 = _T_112 ? _GEN_1492 : _GEN_1457; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1570 = 5'h16 == rd ? _next_reg_T_37 : _GEN_1535; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1613 = _T_119 ? _GEN_1570 : _GEN_1535; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1648 = 5'h16 == rd ? _next_reg_T_41 : _GEN_1613; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1691 = _T_126 ? _GEN_1648 : _GEN_1613; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1726 = 5'h16 == rd ? _next_reg_T_45 : _GEN_1691; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1760 = _T_346 ? _GEN_1726 : _GEN_1691; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1808 = _T_133 ? _GEN_1760 : _GEN_1691; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1846 = 5'h16 == rd ? _next_reg_T_45 : _GEN_1808; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1880 = _T_180 ? _GEN_1846 : _GEN_1808; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1929 = _T_157 ? _GEN_1880 : _GEN_1808; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2141 = 5'h16 == rd ? _next_reg_T_53 : _GEN_1929; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2222 = _T_348 ? _GEN_2141 : _GEN_1929; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2259 = 5'h16 == rd ? _next_reg_T_61 : _GEN_2222; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2294 = _T_435 ? _GEN_2259 : _GEN_2222; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2340 = _T_368 ? _GEN_2294 : _GEN_2222; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2377 = 5'h16 == rd ? _next_reg_T_65[31:0] : _GEN_2340; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2412 = _T_437 ? _GEN_2377 : _GEN_2340; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2458 = _T_388 ? _GEN_2412 : _GEN_2340; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2497 = 5'h16 == rd ? _next_reg_T_73 : _GEN_2458; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2543 = _T_408 ? _GEN_2497 : _GEN_2458; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2578 = 5'h16 == rd ? _next_reg_T_79 : _GEN_2543; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2613 = _T_435 ? _GEN_2578 : _GEN_2543; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2659 = _T_427 ? _GEN_2613 : _GEN_2543; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2794 = 5'h16 == rd ? _next_reg_T_94[31:0] : _GEN_2659; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2833 = _T_539 ? _GEN_2794 : _GEN_2659; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2865 = 5'h16 == rd ? _next_reg_T_85[63:32] : _GEN_2833; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2904 = _T_546 ? _GEN_2865 : _GEN_2833; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2936 = 5'h16 == rd ? _next_reg_T_92[63:32] : _GEN_2904; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2975 = _T_553 ? _GEN_2936 : _GEN_2904; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_3007 = 5'h16 == rd ? _next_reg_T_94[63:32] : _GEN_2975; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3046 = _T_560 ? _GEN_3007 : _GEN_2975; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3078 = 5'h16 == rd ? _next_reg_T_113 : _GEN_3046; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3117 = _T_567 ? _GEN_3078 : _GEN_3046; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3149 = 5'h16 == rd ? _next_reg_T_117 : _GEN_3117; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3188 = _T_574 ? _GEN_3149 : _GEN_3117; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3220 = 5'h16 == rd ? _next_reg_T_131 : _GEN_3188; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3259 = _T_581 ? _GEN_3220 : _GEN_3188; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3291 = 5'h16 == rd ? _next_reg_T_134 : _GEN_3259; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3330 = _T_588 ? _GEN_3291 : _GEN_3259; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_128 = _T_1 ? _GEN_55 : io_now_reg_23; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_163 = 5'h17 == rd ? _next_reg_rd_1 : _GEN_128; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_204 = _T_7 ? _GEN_163 : _GEN_128; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_239 = 5'h17 == rd ? _next_reg_rd_3 : _GEN_204; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_280 = _T_13 ? _GEN_239 : _GEN_204; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_315 = 5'h17 == rd ? _next_reg_T_8 : _GEN_280; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_356 = _T_19 ? _GEN_315 : _GEN_280; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_391 = 5'h17 == rd ? _next_reg_T_9 : _GEN_356; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_432 = _T_25 ? _GEN_391 : _GEN_356; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_467 = 5'h17 == rd ? _next_reg_T_10 : _GEN_432; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_508 = _T_31 ? _GEN_467 : _GEN_432; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_543 = 5'h17 == rd ? _next_reg_T_12[31:0] : _GEN_508; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_584 = _T_37 ? _GEN_543 : _GEN_508; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_619 = 5'h17 == rd ? _next_reg_T_14 : _GEN_584; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_660 = _T_43 ? _GEN_619 : _GEN_584; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_695 = 5'h17 == rd ? _next_reg_T_18 : _GEN_660; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_736 = _T_49 ? _GEN_695 : _GEN_660; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_771 = 5'h17 == rd ? imm : _GEN_736; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_808 = _T_55 ? _GEN_771 : _GEN_736; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_843 = 5'h17 == rd ? _T_334 : _GEN_808; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_880 = _T_59 ? _GEN_843 : _GEN_808; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_947 = 5'h17 == rd ? _next_reg_T_22 : _GEN_880; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_990 = _T_63 ? _GEN_947 : _GEN_880; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1025 = 5'h17 == rd ? _next_reg_rd_23 : _GEN_990; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1068 = _T_70 ? _GEN_1025 : _GEN_990; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1103 = 5'h17 == rd ? _next_reg_rd_25 : _GEN_1068; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1146 = _T_77 ? _GEN_1103 : _GEN_1068; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1181 = 5'h17 == rd ? _next_reg_T_29 : _GEN_1146; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1224 = _T_84 ? _GEN_1181 : _GEN_1146; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1259 = 5'h17 == rd ? _next_reg_T_30 : _GEN_1224; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1302 = _T_91 ? _GEN_1259 : _GEN_1224; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1337 = 5'h17 == rd ? _next_reg_T_31 : _GEN_1302; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1380 = _T_98 ? _GEN_1337 : _GEN_1302; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1415 = 5'h17 == rd ? _next_reg_T_33[31:0] : _GEN_1380; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1458 = _T_105 ? _GEN_1415 : _GEN_1380; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1493 = 5'h17 == rd ? _next_reg_T_35 : _GEN_1458; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1536 = _T_112 ? _GEN_1493 : _GEN_1458; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1571 = 5'h17 == rd ? _next_reg_T_37 : _GEN_1536; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1614 = _T_119 ? _GEN_1571 : _GEN_1536; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1649 = 5'h17 == rd ? _next_reg_T_41 : _GEN_1614; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1692 = _T_126 ? _GEN_1649 : _GEN_1614; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1727 = 5'h17 == rd ? _next_reg_T_45 : _GEN_1692; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1761 = _T_346 ? _GEN_1727 : _GEN_1692; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1809 = _T_133 ? _GEN_1761 : _GEN_1692; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1847 = 5'h17 == rd ? _next_reg_T_45 : _GEN_1809; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1881 = _T_180 ? _GEN_1847 : _GEN_1809; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1930 = _T_157 ? _GEN_1881 : _GEN_1809; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2142 = 5'h17 == rd ? _next_reg_T_53 : _GEN_1930; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2223 = _T_348 ? _GEN_2142 : _GEN_1930; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2260 = 5'h17 == rd ? _next_reg_T_61 : _GEN_2223; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2295 = _T_435 ? _GEN_2260 : _GEN_2223; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2341 = _T_368 ? _GEN_2295 : _GEN_2223; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2378 = 5'h17 == rd ? _next_reg_T_65[31:0] : _GEN_2341; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2413 = _T_437 ? _GEN_2378 : _GEN_2341; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2459 = _T_388 ? _GEN_2413 : _GEN_2341; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2498 = 5'h17 == rd ? _next_reg_T_73 : _GEN_2459; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2544 = _T_408 ? _GEN_2498 : _GEN_2459; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2579 = 5'h17 == rd ? _next_reg_T_79 : _GEN_2544; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2614 = _T_435 ? _GEN_2579 : _GEN_2544; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2660 = _T_427 ? _GEN_2614 : _GEN_2544; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2795 = 5'h17 == rd ? _next_reg_T_94[31:0] : _GEN_2660; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2834 = _T_539 ? _GEN_2795 : _GEN_2660; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2866 = 5'h17 == rd ? _next_reg_T_85[63:32] : _GEN_2834; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2905 = _T_546 ? _GEN_2866 : _GEN_2834; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2937 = 5'h17 == rd ? _next_reg_T_92[63:32] : _GEN_2905; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2976 = _T_553 ? _GEN_2937 : _GEN_2905; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_3008 = 5'h17 == rd ? _next_reg_T_94[63:32] : _GEN_2976; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3047 = _T_560 ? _GEN_3008 : _GEN_2976; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3079 = 5'h17 == rd ? _next_reg_T_113 : _GEN_3047; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3118 = _T_567 ? _GEN_3079 : _GEN_3047; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3150 = 5'h17 == rd ? _next_reg_T_117 : _GEN_3118; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3189 = _T_574 ? _GEN_3150 : _GEN_3118; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3221 = 5'h17 == rd ? _next_reg_T_131 : _GEN_3189; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3260 = _T_581 ? _GEN_3221 : _GEN_3189; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3292 = 5'h17 == rd ? _next_reg_T_134 : _GEN_3260; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3331 = _T_588 ? _GEN_3292 : _GEN_3260; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_129 = _T_1 ? _GEN_56 : io_now_reg_24; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_164 = 5'h18 == rd ? _next_reg_rd_1 : _GEN_129; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_205 = _T_7 ? _GEN_164 : _GEN_129; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_240 = 5'h18 == rd ? _next_reg_rd_3 : _GEN_205; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_281 = _T_13 ? _GEN_240 : _GEN_205; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_316 = 5'h18 == rd ? _next_reg_T_8 : _GEN_281; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_357 = _T_19 ? _GEN_316 : _GEN_281; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_392 = 5'h18 == rd ? _next_reg_T_9 : _GEN_357; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_433 = _T_25 ? _GEN_392 : _GEN_357; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_468 = 5'h18 == rd ? _next_reg_T_10 : _GEN_433; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_509 = _T_31 ? _GEN_468 : _GEN_433; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_544 = 5'h18 == rd ? _next_reg_T_12[31:0] : _GEN_509; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_585 = _T_37 ? _GEN_544 : _GEN_509; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_620 = 5'h18 == rd ? _next_reg_T_14 : _GEN_585; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_661 = _T_43 ? _GEN_620 : _GEN_585; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_696 = 5'h18 == rd ? _next_reg_T_18 : _GEN_661; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_737 = _T_49 ? _GEN_696 : _GEN_661; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_772 = 5'h18 == rd ? imm : _GEN_737; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_809 = _T_55 ? _GEN_772 : _GEN_737; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_844 = 5'h18 == rd ? _T_334 : _GEN_809; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_881 = _T_59 ? _GEN_844 : _GEN_809; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_948 = 5'h18 == rd ? _next_reg_T_22 : _GEN_881; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_991 = _T_63 ? _GEN_948 : _GEN_881; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1026 = 5'h18 == rd ? _next_reg_rd_23 : _GEN_991; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1069 = _T_70 ? _GEN_1026 : _GEN_991; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1104 = 5'h18 == rd ? _next_reg_rd_25 : _GEN_1069; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1147 = _T_77 ? _GEN_1104 : _GEN_1069; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1182 = 5'h18 == rd ? _next_reg_T_29 : _GEN_1147; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1225 = _T_84 ? _GEN_1182 : _GEN_1147; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1260 = 5'h18 == rd ? _next_reg_T_30 : _GEN_1225; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1303 = _T_91 ? _GEN_1260 : _GEN_1225; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1338 = 5'h18 == rd ? _next_reg_T_31 : _GEN_1303; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1381 = _T_98 ? _GEN_1338 : _GEN_1303; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1416 = 5'h18 == rd ? _next_reg_T_33[31:0] : _GEN_1381; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1459 = _T_105 ? _GEN_1416 : _GEN_1381; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1494 = 5'h18 == rd ? _next_reg_T_35 : _GEN_1459; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1537 = _T_112 ? _GEN_1494 : _GEN_1459; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1572 = 5'h18 == rd ? _next_reg_T_37 : _GEN_1537; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1615 = _T_119 ? _GEN_1572 : _GEN_1537; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1650 = 5'h18 == rd ? _next_reg_T_41 : _GEN_1615; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1693 = _T_126 ? _GEN_1650 : _GEN_1615; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1728 = 5'h18 == rd ? _next_reg_T_45 : _GEN_1693; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1762 = _T_346 ? _GEN_1728 : _GEN_1693; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1810 = _T_133 ? _GEN_1762 : _GEN_1693; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1848 = 5'h18 == rd ? _next_reg_T_45 : _GEN_1810; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1882 = _T_180 ? _GEN_1848 : _GEN_1810; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1931 = _T_157 ? _GEN_1882 : _GEN_1810; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2143 = 5'h18 == rd ? _next_reg_T_53 : _GEN_1931; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2224 = _T_348 ? _GEN_2143 : _GEN_1931; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2261 = 5'h18 == rd ? _next_reg_T_61 : _GEN_2224; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2296 = _T_435 ? _GEN_2261 : _GEN_2224; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2342 = _T_368 ? _GEN_2296 : _GEN_2224; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2379 = 5'h18 == rd ? _next_reg_T_65[31:0] : _GEN_2342; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2414 = _T_437 ? _GEN_2379 : _GEN_2342; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2460 = _T_388 ? _GEN_2414 : _GEN_2342; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2499 = 5'h18 == rd ? _next_reg_T_73 : _GEN_2460; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2545 = _T_408 ? _GEN_2499 : _GEN_2460; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2580 = 5'h18 == rd ? _next_reg_T_79 : _GEN_2545; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2615 = _T_435 ? _GEN_2580 : _GEN_2545; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2661 = _T_427 ? _GEN_2615 : _GEN_2545; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2796 = 5'h18 == rd ? _next_reg_T_94[31:0] : _GEN_2661; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2835 = _T_539 ? _GEN_2796 : _GEN_2661; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2867 = 5'h18 == rd ? _next_reg_T_85[63:32] : _GEN_2835; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2906 = _T_546 ? _GEN_2867 : _GEN_2835; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2938 = 5'h18 == rd ? _next_reg_T_92[63:32] : _GEN_2906; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2977 = _T_553 ? _GEN_2938 : _GEN_2906; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_3009 = 5'h18 == rd ? _next_reg_T_94[63:32] : _GEN_2977; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3048 = _T_560 ? _GEN_3009 : _GEN_2977; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3080 = 5'h18 == rd ? _next_reg_T_113 : _GEN_3048; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3119 = _T_567 ? _GEN_3080 : _GEN_3048; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3151 = 5'h18 == rd ? _next_reg_T_117 : _GEN_3119; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3190 = _T_574 ? _GEN_3151 : _GEN_3119; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3222 = 5'h18 == rd ? _next_reg_T_131 : _GEN_3190; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3261 = _T_581 ? _GEN_3222 : _GEN_3190; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3293 = 5'h18 == rd ? _next_reg_T_134 : _GEN_3261; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3332 = _T_588 ? _GEN_3293 : _GEN_3261; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_130 = _T_1 ? _GEN_57 : io_now_reg_25; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_165 = 5'h19 == rd ? _next_reg_rd_1 : _GEN_130; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_206 = _T_7 ? _GEN_165 : _GEN_130; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_241 = 5'h19 == rd ? _next_reg_rd_3 : _GEN_206; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_282 = _T_13 ? _GEN_241 : _GEN_206; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_317 = 5'h19 == rd ? _next_reg_T_8 : _GEN_282; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_358 = _T_19 ? _GEN_317 : _GEN_282; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_393 = 5'h19 == rd ? _next_reg_T_9 : _GEN_358; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_434 = _T_25 ? _GEN_393 : _GEN_358; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_469 = 5'h19 == rd ? _next_reg_T_10 : _GEN_434; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_510 = _T_31 ? _GEN_469 : _GEN_434; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_545 = 5'h19 == rd ? _next_reg_T_12[31:0] : _GEN_510; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_586 = _T_37 ? _GEN_545 : _GEN_510; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_621 = 5'h19 == rd ? _next_reg_T_14 : _GEN_586; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_662 = _T_43 ? _GEN_621 : _GEN_586; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_697 = 5'h19 == rd ? _next_reg_T_18 : _GEN_662; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_738 = _T_49 ? _GEN_697 : _GEN_662; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_773 = 5'h19 == rd ? imm : _GEN_738; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_810 = _T_55 ? _GEN_773 : _GEN_738; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_845 = 5'h19 == rd ? _T_334 : _GEN_810; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_882 = _T_59 ? _GEN_845 : _GEN_810; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_949 = 5'h19 == rd ? _next_reg_T_22 : _GEN_882; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_992 = _T_63 ? _GEN_949 : _GEN_882; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1027 = 5'h19 == rd ? _next_reg_rd_23 : _GEN_992; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1070 = _T_70 ? _GEN_1027 : _GEN_992; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1105 = 5'h19 == rd ? _next_reg_rd_25 : _GEN_1070; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1148 = _T_77 ? _GEN_1105 : _GEN_1070; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1183 = 5'h19 == rd ? _next_reg_T_29 : _GEN_1148; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1226 = _T_84 ? _GEN_1183 : _GEN_1148; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1261 = 5'h19 == rd ? _next_reg_T_30 : _GEN_1226; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1304 = _T_91 ? _GEN_1261 : _GEN_1226; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1339 = 5'h19 == rd ? _next_reg_T_31 : _GEN_1304; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1382 = _T_98 ? _GEN_1339 : _GEN_1304; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1417 = 5'h19 == rd ? _next_reg_T_33[31:0] : _GEN_1382; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1460 = _T_105 ? _GEN_1417 : _GEN_1382; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1495 = 5'h19 == rd ? _next_reg_T_35 : _GEN_1460; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1538 = _T_112 ? _GEN_1495 : _GEN_1460; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1573 = 5'h19 == rd ? _next_reg_T_37 : _GEN_1538; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1616 = _T_119 ? _GEN_1573 : _GEN_1538; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1651 = 5'h19 == rd ? _next_reg_T_41 : _GEN_1616; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1694 = _T_126 ? _GEN_1651 : _GEN_1616; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1729 = 5'h19 == rd ? _next_reg_T_45 : _GEN_1694; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1763 = _T_346 ? _GEN_1729 : _GEN_1694; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1811 = _T_133 ? _GEN_1763 : _GEN_1694; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1849 = 5'h19 == rd ? _next_reg_T_45 : _GEN_1811; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1883 = _T_180 ? _GEN_1849 : _GEN_1811; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1932 = _T_157 ? _GEN_1883 : _GEN_1811; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2144 = 5'h19 == rd ? _next_reg_T_53 : _GEN_1932; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2225 = _T_348 ? _GEN_2144 : _GEN_1932; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2262 = 5'h19 == rd ? _next_reg_T_61 : _GEN_2225; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2297 = _T_435 ? _GEN_2262 : _GEN_2225; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2343 = _T_368 ? _GEN_2297 : _GEN_2225; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2380 = 5'h19 == rd ? _next_reg_T_65[31:0] : _GEN_2343; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2415 = _T_437 ? _GEN_2380 : _GEN_2343; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2461 = _T_388 ? _GEN_2415 : _GEN_2343; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2500 = 5'h19 == rd ? _next_reg_T_73 : _GEN_2461; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2546 = _T_408 ? _GEN_2500 : _GEN_2461; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2581 = 5'h19 == rd ? _next_reg_T_79 : _GEN_2546; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2616 = _T_435 ? _GEN_2581 : _GEN_2546; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2662 = _T_427 ? _GEN_2616 : _GEN_2546; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2797 = 5'h19 == rd ? _next_reg_T_94[31:0] : _GEN_2662; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2836 = _T_539 ? _GEN_2797 : _GEN_2662; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2868 = 5'h19 == rd ? _next_reg_T_85[63:32] : _GEN_2836; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2907 = _T_546 ? _GEN_2868 : _GEN_2836; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2939 = 5'h19 == rd ? _next_reg_T_92[63:32] : _GEN_2907; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2978 = _T_553 ? _GEN_2939 : _GEN_2907; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_3010 = 5'h19 == rd ? _next_reg_T_94[63:32] : _GEN_2978; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3049 = _T_560 ? _GEN_3010 : _GEN_2978; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3081 = 5'h19 == rd ? _next_reg_T_113 : _GEN_3049; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3120 = _T_567 ? _GEN_3081 : _GEN_3049; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3152 = 5'h19 == rd ? _next_reg_T_117 : _GEN_3120; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3191 = _T_574 ? _GEN_3152 : _GEN_3120; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3223 = 5'h19 == rd ? _next_reg_T_131 : _GEN_3191; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3262 = _T_581 ? _GEN_3223 : _GEN_3191; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3294 = 5'h19 == rd ? _next_reg_T_134 : _GEN_3262; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3333 = _T_588 ? _GEN_3294 : _GEN_3262; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_131 = _T_1 ? _GEN_58 : io_now_reg_26; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_166 = 5'h1a == rd ? _next_reg_rd_1 : _GEN_131; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_207 = _T_7 ? _GEN_166 : _GEN_131; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_242 = 5'h1a == rd ? _next_reg_rd_3 : _GEN_207; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_283 = _T_13 ? _GEN_242 : _GEN_207; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_318 = 5'h1a == rd ? _next_reg_T_8 : _GEN_283; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_359 = _T_19 ? _GEN_318 : _GEN_283; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_394 = 5'h1a == rd ? _next_reg_T_9 : _GEN_359; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_435 = _T_25 ? _GEN_394 : _GEN_359; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_470 = 5'h1a == rd ? _next_reg_T_10 : _GEN_435; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_511 = _T_31 ? _GEN_470 : _GEN_435; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_546 = 5'h1a == rd ? _next_reg_T_12[31:0] : _GEN_511; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_587 = _T_37 ? _GEN_546 : _GEN_511; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_622 = 5'h1a == rd ? _next_reg_T_14 : _GEN_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_663 = _T_43 ? _GEN_622 : _GEN_587; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_698 = 5'h1a == rd ? _next_reg_T_18 : _GEN_663; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_739 = _T_49 ? _GEN_698 : _GEN_663; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_774 = 5'h1a == rd ? imm : _GEN_739; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_811 = _T_55 ? _GEN_774 : _GEN_739; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_846 = 5'h1a == rd ? _T_334 : _GEN_811; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_883 = _T_59 ? _GEN_846 : _GEN_811; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_950 = 5'h1a == rd ? _next_reg_T_22 : _GEN_883; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_993 = _T_63 ? _GEN_950 : _GEN_883; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1028 = 5'h1a == rd ? _next_reg_rd_23 : _GEN_993; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1071 = _T_70 ? _GEN_1028 : _GEN_993; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1106 = 5'h1a == rd ? _next_reg_rd_25 : _GEN_1071; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1149 = _T_77 ? _GEN_1106 : _GEN_1071; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1184 = 5'h1a == rd ? _next_reg_T_29 : _GEN_1149; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1227 = _T_84 ? _GEN_1184 : _GEN_1149; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1262 = 5'h1a == rd ? _next_reg_T_30 : _GEN_1227; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1305 = _T_91 ? _GEN_1262 : _GEN_1227; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1340 = 5'h1a == rd ? _next_reg_T_31 : _GEN_1305; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1383 = _T_98 ? _GEN_1340 : _GEN_1305; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1418 = 5'h1a == rd ? _next_reg_T_33[31:0] : _GEN_1383; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1461 = _T_105 ? _GEN_1418 : _GEN_1383; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1496 = 5'h1a == rd ? _next_reg_T_35 : _GEN_1461; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1539 = _T_112 ? _GEN_1496 : _GEN_1461; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1574 = 5'h1a == rd ? _next_reg_T_37 : _GEN_1539; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1617 = _T_119 ? _GEN_1574 : _GEN_1539; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1652 = 5'h1a == rd ? _next_reg_T_41 : _GEN_1617; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1695 = _T_126 ? _GEN_1652 : _GEN_1617; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1730 = 5'h1a == rd ? _next_reg_T_45 : _GEN_1695; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1764 = _T_346 ? _GEN_1730 : _GEN_1695; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1812 = _T_133 ? _GEN_1764 : _GEN_1695; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1850 = 5'h1a == rd ? _next_reg_T_45 : _GEN_1812; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1884 = _T_180 ? _GEN_1850 : _GEN_1812; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1933 = _T_157 ? _GEN_1884 : _GEN_1812; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2145 = 5'h1a == rd ? _next_reg_T_53 : _GEN_1933; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2226 = _T_348 ? _GEN_2145 : _GEN_1933; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2263 = 5'h1a == rd ? _next_reg_T_61 : _GEN_2226; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2298 = _T_435 ? _GEN_2263 : _GEN_2226; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2344 = _T_368 ? _GEN_2298 : _GEN_2226; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2381 = 5'h1a == rd ? _next_reg_T_65[31:0] : _GEN_2344; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2416 = _T_437 ? _GEN_2381 : _GEN_2344; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2462 = _T_388 ? _GEN_2416 : _GEN_2344; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2501 = 5'h1a == rd ? _next_reg_T_73 : _GEN_2462; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2547 = _T_408 ? _GEN_2501 : _GEN_2462; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2582 = 5'h1a == rd ? _next_reg_T_79 : _GEN_2547; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2617 = _T_435 ? _GEN_2582 : _GEN_2547; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2663 = _T_427 ? _GEN_2617 : _GEN_2547; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2798 = 5'h1a == rd ? _next_reg_T_94[31:0] : _GEN_2663; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2837 = _T_539 ? _GEN_2798 : _GEN_2663; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2869 = 5'h1a == rd ? _next_reg_T_85[63:32] : _GEN_2837; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2908 = _T_546 ? _GEN_2869 : _GEN_2837; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2940 = 5'h1a == rd ? _next_reg_T_92[63:32] : _GEN_2908; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2979 = _T_553 ? _GEN_2940 : _GEN_2908; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_3011 = 5'h1a == rd ? _next_reg_T_94[63:32] : _GEN_2979; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3050 = _T_560 ? _GEN_3011 : _GEN_2979; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3082 = 5'h1a == rd ? _next_reg_T_113 : _GEN_3050; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3121 = _T_567 ? _GEN_3082 : _GEN_3050; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3153 = 5'h1a == rd ? _next_reg_T_117 : _GEN_3121; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3192 = _T_574 ? _GEN_3153 : _GEN_3121; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3224 = 5'h1a == rd ? _next_reg_T_131 : _GEN_3192; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3263 = _T_581 ? _GEN_3224 : _GEN_3192; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3295 = 5'h1a == rd ? _next_reg_T_134 : _GEN_3263; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3334 = _T_588 ? _GEN_3295 : _GEN_3263; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_132 = _T_1 ? _GEN_59 : io_now_reg_27; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_167 = 5'h1b == rd ? _next_reg_rd_1 : _GEN_132; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_208 = _T_7 ? _GEN_167 : _GEN_132; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_243 = 5'h1b == rd ? _next_reg_rd_3 : _GEN_208; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_284 = _T_13 ? _GEN_243 : _GEN_208; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_319 = 5'h1b == rd ? _next_reg_T_8 : _GEN_284; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_360 = _T_19 ? _GEN_319 : _GEN_284; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_395 = 5'h1b == rd ? _next_reg_T_9 : _GEN_360; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_436 = _T_25 ? _GEN_395 : _GEN_360; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_471 = 5'h1b == rd ? _next_reg_T_10 : _GEN_436; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_512 = _T_31 ? _GEN_471 : _GEN_436; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_547 = 5'h1b == rd ? _next_reg_T_12[31:0] : _GEN_512; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_588 = _T_37 ? _GEN_547 : _GEN_512; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_623 = 5'h1b == rd ? _next_reg_T_14 : _GEN_588; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_664 = _T_43 ? _GEN_623 : _GEN_588; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_699 = 5'h1b == rd ? _next_reg_T_18 : _GEN_664; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_740 = _T_49 ? _GEN_699 : _GEN_664; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_775 = 5'h1b == rd ? imm : _GEN_740; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_812 = _T_55 ? _GEN_775 : _GEN_740; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_847 = 5'h1b == rd ? _T_334 : _GEN_812; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_884 = _T_59 ? _GEN_847 : _GEN_812; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_951 = 5'h1b == rd ? _next_reg_T_22 : _GEN_884; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_994 = _T_63 ? _GEN_951 : _GEN_884; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1029 = 5'h1b == rd ? _next_reg_rd_23 : _GEN_994; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1072 = _T_70 ? _GEN_1029 : _GEN_994; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1107 = 5'h1b == rd ? _next_reg_rd_25 : _GEN_1072; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1150 = _T_77 ? _GEN_1107 : _GEN_1072; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1185 = 5'h1b == rd ? _next_reg_T_29 : _GEN_1150; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1228 = _T_84 ? _GEN_1185 : _GEN_1150; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1263 = 5'h1b == rd ? _next_reg_T_30 : _GEN_1228; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1306 = _T_91 ? _GEN_1263 : _GEN_1228; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1341 = 5'h1b == rd ? _next_reg_T_31 : _GEN_1306; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1384 = _T_98 ? _GEN_1341 : _GEN_1306; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1419 = 5'h1b == rd ? _next_reg_T_33[31:0] : _GEN_1384; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1462 = _T_105 ? _GEN_1419 : _GEN_1384; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1497 = 5'h1b == rd ? _next_reg_T_35 : _GEN_1462; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1540 = _T_112 ? _GEN_1497 : _GEN_1462; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1575 = 5'h1b == rd ? _next_reg_T_37 : _GEN_1540; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1618 = _T_119 ? _GEN_1575 : _GEN_1540; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1653 = 5'h1b == rd ? _next_reg_T_41 : _GEN_1618; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1696 = _T_126 ? _GEN_1653 : _GEN_1618; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1731 = 5'h1b == rd ? _next_reg_T_45 : _GEN_1696; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1765 = _T_346 ? _GEN_1731 : _GEN_1696; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1813 = _T_133 ? _GEN_1765 : _GEN_1696; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1851 = 5'h1b == rd ? _next_reg_T_45 : _GEN_1813; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1885 = _T_180 ? _GEN_1851 : _GEN_1813; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1934 = _T_157 ? _GEN_1885 : _GEN_1813; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2146 = 5'h1b == rd ? _next_reg_T_53 : _GEN_1934; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2227 = _T_348 ? _GEN_2146 : _GEN_1934; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2264 = 5'h1b == rd ? _next_reg_T_61 : _GEN_2227; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2299 = _T_435 ? _GEN_2264 : _GEN_2227; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2345 = _T_368 ? _GEN_2299 : _GEN_2227; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2382 = 5'h1b == rd ? _next_reg_T_65[31:0] : _GEN_2345; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2417 = _T_437 ? _GEN_2382 : _GEN_2345; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2463 = _T_388 ? _GEN_2417 : _GEN_2345; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2502 = 5'h1b == rd ? _next_reg_T_73 : _GEN_2463; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2548 = _T_408 ? _GEN_2502 : _GEN_2463; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2583 = 5'h1b == rd ? _next_reg_T_79 : _GEN_2548; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2618 = _T_435 ? _GEN_2583 : _GEN_2548; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2664 = _T_427 ? _GEN_2618 : _GEN_2548; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2799 = 5'h1b == rd ? _next_reg_T_94[31:0] : _GEN_2664; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2838 = _T_539 ? _GEN_2799 : _GEN_2664; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2870 = 5'h1b == rd ? _next_reg_T_85[63:32] : _GEN_2838; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2909 = _T_546 ? _GEN_2870 : _GEN_2838; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2941 = 5'h1b == rd ? _next_reg_T_92[63:32] : _GEN_2909; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2980 = _T_553 ? _GEN_2941 : _GEN_2909; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_3012 = 5'h1b == rd ? _next_reg_T_94[63:32] : _GEN_2980; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3051 = _T_560 ? _GEN_3012 : _GEN_2980; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3083 = 5'h1b == rd ? _next_reg_T_113 : _GEN_3051; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3122 = _T_567 ? _GEN_3083 : _GEN_3051; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3154 = 5'h1b == rd ? _next_reg_T_117 : _GEN_3122; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3193 = _T_574 ? _GEN_3154 : _GEN_3122; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3225 = 5'h1b == rd ? _next_reg_T_131 : _GEN_3193; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3264 = _T_581 ? _GEN_3225 : _GEN_3193; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3296 = 5'h1b == rd ? _next_reg_T_134 : _GEN_3264; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3335 = _T_588 ? _GEN_3296 : _GEN_3264; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_133 = _T_1 ? _GEN_60 : io_now_reg_28; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_168 = 5'h1c == rd ? _next_reg_rd_1 : _GEN_133; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_209 = _T_7 ? _GEN_168 : _GEN_133; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_244 = 5'h1c == rd ? _next_reg_rd_3 : _GEN_209; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_285 = _T_13 ? _GEN_244 : _GEN_209; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_320 = 5'h1c == rd ? _next_reg_T_8 : _GEN_285; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_361 = _T_19 ? _GEN_320 : _GEN_285; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_396 = 5'h1c == rd ? _next_reg_T_9 : _GEN_361; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_437 = _T_25 ? _GEN_396 : _GEN_361; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_472 = 5'h1c == rd ? _next_reg_T_10 : _GEN_437; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_513 = _T_31 ? _GEN_472 : _GEN_437; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_548 = 5'h1c == rd ? _next_reg_T_12[31:0] : _GEN_513; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_589 = _T_37 ? _GEN_548 : _GEN_513; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_624 = 5'h1c == rd ? _next_reg_T_14 : _GEN_589; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_665 = _T_43 ? _GEN_624 : _GEN_589; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_700 = 5'h1c == rd ? _next_reg_T_18 : _GEN_665; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_741 = _T_49 ? _GEN_700 : _GEN_665; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_776 = 5'h1c == rd ? imm : _GEN_741; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_813 = _T_55 ? _GEN_776 : _GEN_741; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_848 = 5'h1c == rd ? _T_334 : _GEN_813; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_885 = _T_59 ? _GEN_848 : _GEN_813; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_952 = 5'h1c == rd ? _next_reg_T_22 : _GEN_885; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_995 = _T_63 ? _GEN_952 : _GEN_885; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1030 = 5'h1c == rd ? _next_reg_rd_23 : _GEN_995; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1073 = _T_70 ? _GEN_1030 : _GEN_995; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1108 = 5'h1c == rd ? _next_reg_rd_25 : _GEN_1073; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1151 = _T_77 ? _GEN_1108 : _GEN_1073; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1186 = 5'h1c == rd ? _next_reg_T_29 : _GEN_1151; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1229 = _T_84 ? _GEN_1186 : _GEN_1151; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1264 = 5'h1c == rd ? _next_reg_T_30 : _GEN_1229; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1307 = _T_91 ? _GEN_1264 : _GEN_1229; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1342 = 5'h1c == rd ? _next_reg_T_31 : _GEN_1307; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1385 = _T_98 ? _GEN_1342 : _GEN_1307; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1420 = 5'h1c == rd ? _next_reg_T_33[31:0] : _GEN_1385; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1463 = _T_105 ? _GEN_1420 : _GEN_1385; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1498 = 5'h1c == rd ? _next_reg_T_35 : _GEN_1463; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1541 = _T_112 ? _GEN_1498 : _GEN_1463; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1576 = 5'h1c == rd ? _next_reg_T_37 : _GEN_1541; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1619 = _T_119 ? _GEN_1576 : _GEN_1541; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1654 = 5'h1c == rd ? _next_reg_T_41 : _GEN_1619; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1697 = _T_126 ? _GEN_1654 : _GEN_1619; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1732 = 5'h1c == rd ? _next_reg_T_45 : _GEN_1697; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1766 = _T_346 ? _GEN_1732 : _GEN_1697; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1814 = _T_133 ? _GEN_1766 : _GEN_1697; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1852 = 5'h1c == rd ? _next_reg_T_45 : _GEN_1814; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1886 = _T_180 ? _GEN_1852 : _GEN_1814; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1935 = _T_157 ? _GEN_1886 : _GEN_1814; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2147 = 5'h1c == rd ? _next_reg_T_53 : _GEN_1935; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2228 = _T_348 ? _GEN_2147 : _GEN_1935; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2265 = 5'h1c == rd ? _next_reg_T_61 : _GEN_2228; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2300 = _T_435 ? _GEN_2265 : _GEN_2228; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2346 = _T_368 ? _GEN_2300 : _GEN_2228; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2383 = 5'h1c == rd ? _next_reg_T_65[31:0] : _GEN_2346; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2418 = _T_437 ? _GEN_2383 : _GEN_2346; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2464 = _T_388 ? _GEN_2418 : _GEN_2346; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2503 = 5'h1c == rd ? _next_reg_T_73 : _GEN_2464; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2549 = _T_408 ? _GEN_2503 : _GEN_2464; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2584 = 5'h1c == rd ? _next_reg_T_79 : _GEN_2549; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2619 = _T_435 ? _GEN_2584 : _GEN_2549; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2665 = _T_427 ? _GEN_2619 : _GEN_2549; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2800 = 5'h1c == rd ? _next_reg_T_94[31:0] : _GEN_2665; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2839 = _T_539 ? _GEN_2800 : _GEN_2665; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2871 = 5'h1c == rd ? _next_reg_T_85[63:32] : _GEN_2839; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2910 = _T_546 ? _GEN_2871 : _GEN_2839; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2942 = 5'h1c == rd ? _next_reg_T_92[63:32] : _GEN_2910; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2981 = _T_553 ? _GEN_2942 : _GEN_2910; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_3013 = 5'h1c == rd ? _next_reg_T_94[63:32] : _GEN_2981; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3052 = _T_560 ? _GEN_3013 : _GEN_2981; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3084 = 5'h1c == rd ? _next_reg_T_113 : _GEN_3052; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3123 = _T_567 ? _GEN_3084 : _GEN_3052; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3155 = 5'h1c == rd ? _next_reg_T_117 : _GEN_3123; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3194 = _T_574 ? _GEN_3155 : _GEN_3123; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3226 = 5'h1c == rd ? _next_reg_T_131 : _GEN_3194; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3265 = _T_581 ? _GEN_3226 : _GEN_3194; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3297 = 5'h1c == rd ? _next_reg_T_134 : _GEN_3265; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3336 = _T_588 ? _GEN_3297 : _GEN_3265; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_134 = _T_1 ? _GEN_61 : io_now_reg_29; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_169 = 5'h1d == rd ? _next_reg_rd_1 : _GEN_134; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_210 = _T_7 ? _GEN_169 : _GEN_134; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_245 = 5'h1d == rd ? _next_reg_rd_3 : _GEN_210; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_286 = _T_13 ? _GEN_245 : _GEN_210; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_321 = 5'h1d == rd ? _next_reg_T_8 : _GEN_286; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_362 = _T_19 ? _GEN_321 : _GEN_286; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_397 = 5'h1d == rd ? _next_reg_T_9 : _GEN_362; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_438 = _T_25 ? _GEN_397 : _GEN_362; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_473 = 5'h1d == rd ? _next_reg_T_10 : _GEN_438; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_514 = _T_31 ? _GEN_473 : _GEN_438; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_549 = 5'h1d == rd ? _next_reg_T_12[31:0] : _GEN_514; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_590 = _T_37 ? _GEN_549 : _GEN_514; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_625 = 5'h1d == rd ? _next_reg_T_14 : _GEN_590; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_666 = _T_43 ? _GEN_625 : _GEN_590; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_701 = 5'h1d == rd ? _next_reg_T_18 : _GEN_666; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_742 = _T_49 ? _GEN_701 : _GEN_666; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_777 = 5'h1d == rd ? imm : _GEN_742; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_814 = _T_55 ? _GEN_777 : _GEN_742; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_849 = 5'h1d == rd ? _T_334 : _GEN_814; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_886 = _T_59 ? _GEN_849 : _GEN_814; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_953 = 5'h1d == rd ? _next_reg_T_22 : _GEN_886; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_996 = _T_63 ? _GEN_953 : _GEN_886; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1031 = 5'h1d == rd ? _next_reg_rd_23 : _GEN_996; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1074 = _T_70 ? _GEN_1031 : _GEN_996; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1109 = 5'h1d == rd ? _next_reg_rd_25 : _GEN_1074; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1152 = _T_77 ? _GEN_1109 : _GEN_1074; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1187 = 5'h1d == rd ? _next_reg_T_29 : _GEN_1152; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1230 = _T_84 ? _GEN_1187 : _GEN_1152; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1265 = 5'h1d == rd ? _next_reg_T_30 : _GEN_1230; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1308 = _T_91 ? _GEN_1265 : _GEN_1230; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1343 = 5'h1d == rd ? _next_reg_T_31 : _GEN_1308; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1386 = _T_98 ? _GEN_1343 : _GEN_1308; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1421 = 5'h1d == rd ? _next_reg_T_33[31:0] : _GEN_1386; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1464 = _T_105 ? _GEN_1421 : _GEN_1386; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1499 = 5'h1d == rd ? _next_reg_T_35 : _GEN_1464; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1542 = _T_112 ? _GEN_1499 : _GEN_1464; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1577 = 5'h1d == rd ? _next_reg_T_37 : _GEN_1542; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1620 = _T_119 ? _GEN_1577 : _GEN_1542; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1655 = 5'h1d == rd ? _next_reg_T_41 : _GEN_1620; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1698 = _T_126 ? _GEN_1655 : _GEN_1620; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1733 = 5'h1d == rd ? _next_reg_T_45 : _GEN_1698; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1767 = _T_346 ? _GEN_1733 : _GEN_1698; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1815 = _T_133 ? _GEN_1767 : _GEN_1698; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1853 = 5'h1d == rd ? _next_reg_T_45 : _GEN_1815; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1887 = _T_180 ? _GEN_1853 : _GEN_1815; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1936 = _T_157 ? _GEN_1887 : _GEN_1815; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2148 = 5'h1d == rd ? _next_reg_T_53 : _GEN_1936; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2229 = _T_348 ? _GEN_2148 : _GEN_1936; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2266 = 5'h1d == rd ? _next_reg_T_61 : _GEN_2229; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2301 = _T_435 ? _GEN_2266 : _GEN_2229; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2347 = _T_368 ? _GEN_2301 : _GEN_2229; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2384 = 5'h1d == rd ? _next_reg_T_65[31:0] : _GEN_2347; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2419 = _T_437 ? _GEN_2384 : _GEN_2347; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2465 = _T_388 ? _GEN_2419 : _GEN_2347; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2504 = 5'h1d == rd ? _next_reg_T_73 : _GEN_2465; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2550 = _T_408 ? _GEN_2504 : _GEN_2465; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2585 = 5'h1d == rd ? _next_reg_T_79 : _GEN_2550; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2620 = _T_435 ? _GEN_2585 : _GEN_2550; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2666 = _T_427 ? _GEN_2620 : _GEN_2550; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2801 = 5'h1d == rd ? _next_reg_T_94[31:0] : _GEN_2666; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2840 = _T_539 ? _GEN_2801 : _GEN_2666; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2872 = 5'h1d == rd ? _next_reg_T_85[63:32] : _GEN_2840; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2911 = _T_546 ? _GEN_2872 : _GEN_2840; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2943 = 5'h1d == rd ? _next_reg_T_92[63:32] : _GEN_2911; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2982 = _T_553 ? _GEN_2943 : _GEN_2911; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_3014 = 5'h1d == rd ? _next_reg_T_94[63:32] : _GEN_2982; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3053 = _T_560 ? _GEN_3014 : _GEN_2982; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3085 = 5'h1d == rd ? _next_reg_T_113 : _GEN_3053; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3124 = _T_567 ? _GEN_3085 : _GEN_3053; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3156 = 5'h1d == rd ? _next_reg_T_117 : _GEN_3124; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3195 = _T_574 ? _GEN_3156 : _GEN_3124; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3227 = 5'h1d == rd ? _next_reg_T_131 : _GEN_3195; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3266 = _T_581 ? _GEN_3227 : _GEN_3195; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3298 = 5'h1d == rd ? _next_reg_T_134 : _GEN_3266; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3337 = _T_588 ? _GEN_3298 : _GEN_3266; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_135 = _T_1 ? _GEN_62 : io_now_reg_30; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_170 = 5'h1e == rd ? _next_reg_rd_1 : _GEN_135; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_211 = _T_7 ? _GEN_170 : _GEN_135; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_246 = 5'h1e == rd ? _next_reg_rd_3 : _GEN_211; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_287 = _T_13 ? _GEN_246 : _GEN_211; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_322 = 5'h1e == rd ? _next_reg_T_8 : _GEN_287; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_363 = _T_19 ? _GEN_322 : _GEN_287; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_398 = 5'h1e == rd ? _next_reg_T_9 : _GEN_363; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_439 = _T_25 ? _GEN_398 : _GEN_363; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_474 = 5'h1e == rd ? _next_reg_T_10 : _GEN_439; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_515 = _T_31 ? _GEN_474 : _GEN_439; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_550 = 5'h1e == rd ? _next_reg_T_12[31:0] : _GEN_515; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_591 = _T_37 ? _GEN_550 : _GEN_515; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_626 = 5'h1e == rd ? _next_reg_T_14 : _GEN_591; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_667 = _T_43 ? _GEN_626 : _GEN_591; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_702 = 5'h1e == rd ? _next_reg_T_18 : _GEN_667; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_743 = _T_49 ? _GEN_702 : _GEN_667; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_778 = 5'h1e == rd ? imm : _GEN_743; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_815 = _T_55 ? _GEN_778 : _GEN_743; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_850 = 5'h1e == rd ? _T_334 : _GEN_815; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_887 = _T_59 ? _GEN_850 : _GEN_815; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_954 = 5'h1e == rd ? _next_reg_T_22 : _GEN_887; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_997 = _T_63 ? _GEN_954 : _GEN_887; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1032 = 5'h1e == rd ? _next_reg_rd_23 : _GEN_997; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1075 = _T_70 ? _GEN_1032 : _GEN_997; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1110 = 5'h1e == rd ? _next_reg_rd_25 : _GEN_1075; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1153 = _T_77 ? _GEN_1110 : _GEN_1075; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1188 = 5'h1e == rd ? _next_reg_T_29 : _GEN_1153; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1231 = _T_84 ? _GEN_1188 : _GEN_1153; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1266 = 5'h1e == rd ? _next_reg_T_30 : _GEN_1231; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1309 = _T_91 ? _GEN_1266 : _GEN_1231; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1344 = 5'h1e == rd ? _next_reg_T_31 : _GEN_1309; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1387 = _T_98 ? _GEN_1344 : _GEN_1309; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1422 = 5'h1e == rd ? _next_reg_T_33[31:0] : _GEN_1387; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1465 = _T_105 ? _GEN_1422 : _GEN_1387; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1500 = 5'h1e == rd ? _next_reg_T_35 : _GEN_1465; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1543 = _T_112 ? _GEN_1500 : _GEN_1465; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1578 = 5'h1e == rd ? _next_reg_T_37 : _GEN_1543; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1621 = _T_119 ? _GEN_1578 : _GEN_1543; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1656 = 5'h1e == rd ? _next_reg_T_41 : _GEN_1621; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1699 = _T_126 ? _GEN_1656 : _GEN_1621; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1734 = 5'h1e == rd ? _next_reg_T_45 : _GEN_1699; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1768 = _T_346 ? _GEN_1734 : _GEN_1699; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1816 = _T_133 ? _GEN_1768 : _GEN_1699; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1854 = 5'h1e == rd ? _next_reg_T_45 : _GEN_1816; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1888 = _T_180 ? _GEN_1854 : _GEN_1816; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1937 = _T_157 ? _GEN_1888 : _GEN_1816; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2149 = 5'h1e == rd ? _next_reg_T_53 : _GEN_1937; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2230 = _T_348 ? _GEN_2149 : _GEN_1937; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2267 = 5'h1e == rd ? _next_reg_T_61 : _GEN_2230; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2302 = _T_435 ? _GEN_2267 : _GEN_2230; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2348 = _T_368 ? _GEN_2302 : _GEN_2230; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2385 = 5'h1e == rd ? _next_reg_T_65[31:0] : _GEN_2348; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2420 = _T_437 ? _GEN_2385 : _GEN_2348; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2466 = _T_388 ? _GEN_2420 : _GEN_2348; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2505 = 5'h1e == rd ? _next_reg_T_73 : _GEN_2466; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2551 = _T_408 ? _GEN_2505 : _GEN_2466; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2586 = 5'h1e == rd ? _next_reg_T_79 : _GEN_2551; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2621 = _T_435 ? _GEN_2586 : _GEN_2551; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2667 = _T_427 ? _GEN_2621 : _GEN_2551; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2802 = 5'h1e == rd ? _next_reg_T_94[31:0] : _GEN_2667; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2841 = _T_539 ? _GEN_2802 : _GEN_2667; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2873 = 5'h1e == rd ? _next_reg_T_85[63:32] : _GEN_2841; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2912 = _T_546 ? _GEN_2873 : _GEN_2841; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2944 = 5'h1e == rd ? _next_reg_T_92[63:32] : _GEN_2912; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2983 = _T_553 ? _GEN_2944 : _GEN_2912; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_3015 = 5'h1e == rd ? _next_reg_T_94[63:32] : _GEN_2983; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3054 = _T_560 ? _GEN_3015 : _GEN_2983; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3086 = 5'h1e == rd ? _next_reg_T_113 : _GEN_3054; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3125 = _T_567 ? _GEN_3086 : _GEN_3054; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3157 = 5'h1e == rd ? _next_reg_T_117 : _GEN_3125; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3196 = _T_574 ? _GEN_3157 : _GEN_3125; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3228 = 5'h1e == rd ? _next_reg_T_131 : _GEN_3196; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3267 = _T_581 ? _GEN_3228 : _GEN_3196; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3299 = 5'h1e == rd ? _next_reg_T_134 : _GEN_3267; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3338 = _T_588 ? _GEN_3299 : _GEN_3267; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_136 = _T_1 ? _GEN_63 : io_now_reg_31; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 149:23 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_171 = 5'h1f == rd ? _next_reg_rd_1 : _GEN_136; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:{65,65}]
  wire [31:0] _GEN_212 = _T_7 ? _GEN_171 : _GEN_136; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 150:23]
  wire [31:0] _GEN_247 = 5'h1f == rd ? _next_reg_rd_3 : _GEN_212; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:{65,65}]
  wire [31:0] _GEN_288 = _T_13 ? _GEN_247 : _GEN_212; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 151:23]
  wire [31:0] _GEN_323 = 5'h1f == rd ? _next_reg_T_8 : _GEN_288; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:{64,64}]
  wire [31:0] _GEN_364 = _T_19 ? _GEN_323 : _GEN_288; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 153:22]
  wire [31:0] _GEN_399 = 5'h1f == rd ? _next_reg_T_9 : _GEN_364; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:{64,64}]
  wire [31:0] _GEN_440 = _T_25 ? _GEN_399 : _GEN_364; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 154:22]
  wire [31:0] _GEN_475 = 5'h1f == rd ? _next_reg_T_10 : _GEN_440; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:{64,64}]
  wire [31:0] _GEN_516 = _T_31 ? _GEN_475 : _GEN_440; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 155:22]
  wire [31:0] _GEN_551 = 5'h1f == rd ? _next_reg_T_12[31:0] : _GEN_516; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:{64,64}]
  wire [31:0] _GEN_592 = _T_37 ? _GEN_551 : _GEN_516; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 157:22]
  wire [31:0] _GEN_627 = 5'h1f == rd ? _next_reg_T_14 : _GEN_592; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:{64,64}]
  wire [31:0] _GEN_668 = _T_43 ? _GEN_627 : _GEN_592; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 158:22]
  wire [31:0] _GEN_703 = 5'h1f == rd ? _next_reg_T_18 : _GEN_668; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:{64,64}]
  wire [31:0] _GEN_744 = _T_49 ? _GEN_703 : _GEN_668; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 159:22]
  wire [31:0] _GEN_779 = 5'h1f == rd ? imm : _GEN_744; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:{45,45}]
  wire [31:0] _GEN_816 = _T_55 ? _GEN_779 : _GEN_744; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 161:21]
  wire [31:0] _GEN_851 = 5'h1f == rd ? _T_334 : _GEN_816; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:{47,47}]
  wire [31:0] _GEN_888 = _T_59 ? _GEN_851 : _GEN_816; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 163:23]
  wire [31:0] _GEN_955 = 5'h1f == rd ? _next_reg_T_22 : _GEN_888; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:{69,69}]
  wire [31:0] _GEN_998 = _T_63 ? _GEN_955 : _GEN_888; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 166:22]
  wire [31:0] _GEN_1033 = 5'h1f == rd ? _next_reg_rd_23 : _GEN_998; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:{69,69}]
  wire [31:0] _GEN_1076 = _T_70 ? _GEN_1033 : _GEN_998; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 167:22]
  wire [31:0] _GEN_1111 = 5'h1f == rd ? _next_reg_rd_25 : _GEN_1076; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:{69,69}]
  wire [31:0] _GEN_1154 = _T_77 ? _GEN_1111 : _GEN_1076; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 168:22]
  wire [31:0] _GEN_1189 = 5'h1f == rd ? _next_reg_T_29 : _GEN_1154; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:{68,68}]
  wire [31:0] _GEN_1232 = _T_84 ? _GEN_1189 : _GEN_1154; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 170:21]
  wire [31:0] _GEN_1267 = 5'h1f == rd ? _next_reg_T_30 : _GEN_1232; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:{68,68}]
  wire [31:0] _GEN_1310 = _T_91 ? _GEN_1267 : _GEN_1232; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 171:21]
  wire [31:0] _GEN_1345 = 5'h1f == rd ? _next_reg_T_31 : _GEN_1310; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:{68,68}]
  wire [31:0] _GEN_1388 = _T_98 ? _GEN_1345 : _GEN_1310; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 172:21]
  wire [31:0] _GEN_1423 = 5'h1f == rd ? _next_reg_T_33[31:0] : _GEN_1388; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:{68,68}]
  wire [31:0] _GEN_1466 = _T_105 ? _GEN_1423 : _GEN_1388; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 174:21]
  wire [31:0] _GEN_1501 = 5'h1f == rd ? _next_reg_T_35 : _GEN_1466; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:{68,68}]
  wire [31:0] _GEN_1544 = _T_112 ? _GEN_1501 : _GEN_1466; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 175:21]
  wire [31:0] _GEN_1579 = 5'h1f == rd ? _next_reg_T_37 : _GEN_1544; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:{68,68}]
  wire [31:0] _GEN_1622 = _T_119 ? _GEN_1579 : _GEN_1544; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 177:21]
  wire [31:0] _GEN_1657 = 5'h1f == rd ? _next_reg_T_41 : _GEN_1622; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:{68,68}]
  wire [31:0] _GEN_1700 = _T_126 ? _GEN_1657 : _GEN_1622; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 178:21]
  wire [31:0] _GEN_1735 = 5'h1f == rd ? _next_reg_T_45 : _GEN_1700; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 190:{27,27}]
  wire [31:0] _GEN_1769 = _T_346 ? _GEN_1735 : _GEN_1700; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55]
  wire [31:0] _GEN_1817 = _T_133 ? _GEN_1769 : _GEN_1700; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1855 = 5'h1f == rd ? _next_reg_T_45 : _GEN_1817; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 203:{27,27}]
  wire [31:0] _GEN_1889 = _T_180 ? _GEN_1855 : _GEN_1817; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91]
  wire [31:0] _GEN_1938 = _T_157 ? _GEN_1889 : _GEN_1817; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_2150 = 5'h1f == rd ? _next_reg_T_53 : _GEN_1938; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 292:{22,22}]
  wire [31:0] _GEN_2231 = _T_348 ? _GEN_2150 : _GEN_1938; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20]
  wire [31:0] _GEN_2268 = 5'h1f == rd ? _next_reg_T_61 : _GEN_2231; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 302:{22,22}]
  wire [31:0] _GEN_2303 = _T_435 ? _GEN_2268 : _GEN_2231; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55]
  wire [31:0] _GEN_2349 = _T_368 ? _GEN_2303 : _GEN_2231; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2386 = 5'h1f == rd ? _next_reg_T_65[31:0] : _GEN_2349; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 311:{22,22}]
  wire [31:0] _GEN_2421 = _T_437 ? _GEN_2386 : _GEN_2349; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55]
  wire [31:0] _GEN_2467 = _T_388 ? _GEN_2421 : _GEN_2349; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2506 = 5'h1f == rd ? _next_reg_T_73 : _GEN_2467; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:{104,104}]
  wire [31:0] _GEN_2552 = _T_408 ? _GEN_2506 : _GEN_2467; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21]
  wire [31:0] _GEN_2587 = 5'h1f == rd ? _next_reg_T_79 : _GEN_2552; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 321:{22,22}]
  wire [31:0] _GEN_2622 = _T_435 ? _GEN_2587 : _GEN_2552; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55]
  wire [31:0] _GEN_2668 = _T_427 ? _GEN_2622 : _GEN_2552; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2803 = 5'h1f == rd ? _next_reg_T_94[31:0] : _GEN_2668; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:{48,48}]
  wire [31:0] _GEN_2842 = _T_539 ? _GEN_2803 : _GEN_2668; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 85:24]
  wire [31:0] _GEN_2874 = 5'h1f == rd ? _next_reg_T_85[63:32] : _GEN_2842; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:{48,48}]
  wire [31:0] _GEN_2913 = _T_546 ? _GEN_2874 : _GEN_2842; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 86:24]
  wire [31:0] _GEN_2945 = 5'h1f == rd ? _next_reg_T_92[63:32] : _GEN_2913; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:{48,48}]
  wire [31:0] _GEN_2984 = _T_553 ? _GEN_2945 : _GEN_2913; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 87:24]
  wire [31:0] _GEN_3016 = 5'h1f == rd ? _next_reg_T_94[63:32] : _GEN_2984; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:{48,48}]
  wire [31:0] _GEN_3055 = _T_560 ? _GEN_3016 : _GEN_2984; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 88:24]
  wire [31:0] _GEN_3087 = 5'h1f == rd ? _next_reg_T_113 : _GEN_3055; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:{46,46}]
  wire [31:0] _GEN_3126 = _T_567 ? _GEN_3087 : _GEN_3055; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 91:22]
  wire [31:0] _GEN_3158 = 5'h1f == rd ? _next_reg_T_117 : _GEN_3126; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:{46,46}]
  wire [31:0] _GEN_3197 = _T_574 ? _GEN_3158 : _GEN_3126; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 92:22]
  wire [31:0] _GEN_3229 = 5'h1f == rd ? _next_reg_T_131 : _GEN_3197; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:{46,46}]
  wire [31:0] _GEN_3268 = _T_581 ? _GEN_3229 : _GEN_3197; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 93:22]
  wire [31:0] _GEN_3300 = 5'h1f == rd ? _next_reg_T_134 : _GEN_3268; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:{46,46}]
  wire [31:0] _GEN_3339 = _T_588 ? _GEN_3300 : _GEN_3268; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/MExtension.scala 94:22]
  wire [31:0] _GEN_1737 = _T_346 ? _T_334 : io_now_pc; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 187:55 189:27]
  wire  _GEN_1784 = _T_133 & _T_346; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 154:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire [31:0] _GEN_1785 = _T_133 ? _GEN_1737 : io_now_pc; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 185:21]
  wire  _GEN_1856 = _T_180 | _GEN_1784; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91 201:27]
  wire [31:0] _GEN_1857 = _T_180 ? _T_168 : _GEN_1785; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 200:91 202:27]
  wire  _GEN_1905 = _T_157 ? _GEN_1856 : _GEN_1784; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire [31:0] _GEN_1906 = _T_157 ? _GEN_1857 : _GEN_1785; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 198:22]
  wire  _GEN_1945 = _T_346 | _GEN_1905; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 216:57 217:29]
  wire [31:0] _GEN_1946 = _T_346 ? _T_334 : _GEN_1906; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 216:57 218:29]
  wire  _GEN_1950 = _GEN_31 == _GEN_923 ? _GEN_1945 : _GEN_1905; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 215:43]
  wire [31:0] _GEN_1951 = _GEN_31 == _GEN_923 ? _GEN_1946 : _GEN_1906; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 215:43]
  wire  _GEN_1969 = _T_182 ? _GEN_1950 : _GEN_1905; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 213:21]
  wire [31:0] _GEN_1970 = _T_182 ? _GEN_1951 : _GEN_1906; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 213:21]
  wire  _GEN_1974 = _T_346 | _GEN_1969; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 228:57 229:29]
  wire [31:0] _GEN_1975 = _T_346 ? _T_334 : _GEN_1970; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 228:57 230:29]
  wire  _GEN_1979 = _GEN_31 != _GEN_923 ? _GEN_1974 : _GEN_1969; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 227:43]
  wire [31:0] _GEN_1980 = _GEN_31 != _GEN_923 ? _GEN_1975 : _GEN_1970; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 227:43]
  wire  _GEN_1998 = _T_209 ? _GEN_1979 : _GEN_1969; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 225:21]
  wire [31:0] _GEN_1999 = _T_209 ? _GEN_1980 : _GEN_1970; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 225:21]
  wire  _GEN_2003 = _T_346 | _GEN_1998; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 241:57 242:29]
  wire [31:0] _GEN_2004 = _T_346 ? _T_334 : _GEN_1999; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 241:57 243:29]
  wire  _GEN_2008 = $signed(_T_300) < $signed(_T_301) ? _GEN_2003 : _GEN_1998; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 240:55]
  wire [31:0] _GEN_2009 = $signed(_T_300) < $signed(_T_301) ? _GEN_2004 : _GEN_1999; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 240:55]
  wire  _GEN_2027 = _T_236 ? _GEN_2008 : _GEN_1998; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 238:21]
  wire [31:0] _GEN_2028 = _T_236 ? _GEN_2009 : _GEN_1999; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 238:21]
  wire  _GEN_2032 = _T_346 | _GEN_2027; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 253:57 254:29]
  wire [31:0] _GEN_2033 = _T_346 ? _T_334 : _GEN_2028; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 253:57 255:29]
  wire  _GEN_2037 = _GEN_31 < _GEN_923 ? _GEN_2032 : _GEN_2027; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 252:41]
  wire [31:0] _GEN_2038 = _GEN_31 < _GEN_923 ? _GEN_2033 : _GEN_2028; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 252:41]
  wire  _GEN_2056 = _T_265 ? _GEN_2037 : _GEN_2027; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 250:22]
  wire [31:0] _GEN_2057 = _T_265 ? _GEN_2038 : _GEN_2028; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 250:22]
  wire  _GEN_2061 = _T_346 | _GEN_2056; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 266:57 267:29]
  wire [31:0] _GEN_2062 = _T_346 ? _T_334 : _GEN_2057; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 266:57 268:29]
  wire  _GEN_2066 = $signed(_T_300) >= $signed(_T_301) ? _GEN_2061 : _GEN_2056; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 265:56]
  wire [31:0] _GEN_2067 = $signed(_T_300) >= $signed(_T_301) ? _GEN_2062 : _GEN_2057; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 265:56]
  wire  _GEN_2085 = _T_292 ? _GEN_2066 : _GEN_2056; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 263:21]
  wire [31:0] _GEN_2086 = _T_292 ? _GEN_2067 : _GEN_2057; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 263:21]
  wire  _GEN_2090 = _T_346 | _GEN_2085; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 278:57 279:29]
  wire [31:0] _GEN_2091 = _T_346 ? _T_334 : _GEN_2086; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 278:57 280:29]
  wire  _GEN_2095 = _GEN_31 >= _GEN_923 ? _GEN_2090 : _GEN_2085; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 277:42]
  wire [31:0] _GEN_2096 = _GEN_31 >= _GEN_923 ? _GEN_2091 : _GEN_2086; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 277:42]
  wire  _GEN_2114 = _T_321 ? _GEN_2095 : _GEN_2085; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 275:22]
  wire [31:0] _GEN_2115 = _T_321 ? _GEN_2096 : _GEN_2086; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 275:22]
  wire [31:0] _GEN_2198 = _T_348 ? _T_433 : 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 161:7]
  wire [5:0] _GEN_2199 = _T_348 ? 6'h8 : 6'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 289:20 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 161:7]
  wire  _GEN_2269 = _T_435 | _T_348; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 52:25]
  wire [31:0] _GEN_2270 = _T_435 ? _T_433 : _T_433; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55 304:23 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 53:25]
  wire [5:0] _GEN_2271 = _T_435 ? 6'h10 : _GEN_2199; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 54:25]
  wire  _GEN_2305 = _T_435 ? _GEN_2117 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 301:55 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 145:33]
  wire  _GEN_2315 = _T_368 ? _GEN_2269 : _T_348; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [31:0] _GEN_2316 = _T_368 ? _GEN_2270 : _GEN_2198; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire [5:0] _GEN_2317 = _T_368 ? _GEN_2271 : _GEN_2199; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire  _GEN_2351 = _T_368 ? _GEN_2305 : _GEN_2117; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 299:20]
  wire  _GEN_2387 = _T_437 | _GEN_2315; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 52:25]
  wire [31:0] _GEN_2388 = _T_437 ? _T_433 : _T_433; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55 313:23 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 53:25]
  wire [5:0] _GEN_2389 = _T_437 ? 6'h20 : _GEN_2317; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 54:25]
  wire  _GEN_2423 = _T_437 ? _GEN_2351 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 310:55 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 145:33]
  wire  _GEN_2433 = _T_388 ? _GEN_2387 : _GEN_2315; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [31:0] _GEN_2434 = _T_388 ? _GEN_2388 : _GEN_2316; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire [5:0] _GEN_2435 = _T_388 ? _GEN_2389 : _GEN_2317; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire  _GEN_2469 = _T_388 ? _GEN_2423 : _GEN_2351; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 308:20]
  wire  _GEN_2518 = _T_408 | _GEN_2433; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 52:25]
  wire [31:0] _GEN_2519 = _T_408 ? _T_433 : _GEN_2434; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 53:25]
  wire [5:0] _GEN_2520 = _T_408 ? 6'h8 : _GEN_2435; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 317:21 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 54:25]
  wire  _GEN_2588 = _T_435 | _GEN_2518; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 52:25]
  wire [5:0] _GEN_2590 = _T_435 ? 6'h10 : _GEN_2520; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 54:25]
  wire  _GEN_2624 = _T_435 ? _GEN_2469 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 320:55 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 145:33]
  wire  _GEN_2634 = _T_427 ? _GEN_2588 : _GEN_2518; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2635 = _T_427 ? _GEN_2270 : _GEN_2519; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [5:0] _GEN_2636 = _T_427 ? _GEN_2590 : _GEN_2520; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire  _GEN_2670 = _T_427 ? _GEN_2624 : _GEN_2469; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 318:21]
  wire [31:0] _GEN_2689 = _T_447 ? _T_433 : 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 328:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 84:26 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 161:7]
  wire [5:0] _GEN_2690 = _T_447 ? 6'h8 : 6'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 328:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 85:26 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 161:7]
  wire [31:0] _GEN_2691 = _T_447 ? {{24'd0}, _GEN_923[7:0]} : 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 328:20 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 86:26 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 161:7]
  wire  _GEN_2692 = _T_435 | _T_447; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 331:55 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 83:26]
  wire [5:0] _GEN_2694 = _T_435 ? 6'h10 : _GEN_2690; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 331:55 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 85:26]
  wire [31:0] _GEN_2695 = _T_435 ? {{16'd0}, _GEN_923[15:0]} : _GEN_2691; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 331:55 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 86:26]
  wire  _GEN_2697 = _T_435 ? _GEN_2670 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 331:55 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 145:33]
  wire  _GEN_2708 = _T_470 ? _GEN_2692 : _T_447; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 329:20]
  wire [31:0] _GEN_2709 = _T_470 ? _GEN_2270 : _GEN_2689; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 329:20]
  wire [5:0] _GEN_2710 = _T_470 ? _GEN_2694 : _GEN_2690; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 329:20]
  wire [31:0] _GEN_2711 = _T_470 ? _GEN_2695 : _GEN_2691; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 329:20]
  wire  _GEN_2713 = _T_470 ? _GEN_2697 : _GEN_2670; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 329:20]
  wire  _GEN_2714 = _T_437 | _GEN_2708; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 340:55 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 83:26]
  wire [5:0] _GEN_2716 = _T_437 ? 6'h20 : _GEN_2710; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 340:55 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 85:26]
  wire [31:0] _GEN_2717 = _T_437 ? _GEN_923 : _GEN_2711; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 340:55 riscv-spec-core/src/main/scala/rvspeccore/core/tool/LoadStore.scala 86:26]
  wire  _GEN_2719 = _T_437 ? _GEN_2713 : 1'h1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 340:55 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 145:33]
  wire  _GEN_2730 = _T_494 ? _GEN_2714 : _GEN_2708; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 338:20]
  wire [31:0] _GEN_2731 = _T_494 ? _GEN_2388 : _GEN_2709; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 338:20]
  wire [5:0] _GEN_2732 = _T_494 ? _GEN_2716 : _GEN_2710; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 338:20]
  wire [31:0] _GEN_2733 = _T_494 ? _GEN_2717 : _GEN_2711; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 338:20]
  wire  _GEN_2735 = _T_494 ? _GEN_2719 : _GEN_2713; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 338:20]
  wire  _GEN_2744 = _T_518 | _GEN_2735; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 347:24 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 145:33]
  wire  _GEN_2751 = 2'h3 == io_now_privilege_internal_privilegeMode | (2'h1 == io_now_privilege_internal_privilegeMode
     | (2'h0 == io_now_privilege_internal_privilegeMode | _GEN_2744)); // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 355:52 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 145:33]
  wire  _GEN_2762 = _T_524 ? _GEN_2751 : _GEN_2744; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/IBase.scala 353:23]
  wire  _GEN_3342 = illegalInstruction | _GEN_2762; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 133:30 145:33]
  wire  raiseExceptionIntr = io_valid & _GEN_3342; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 118:36]
  wire [31:0] _delegS_T = io_now_privilege_csr_medeleg >> exceptionNO; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 155:24]
  wire  delegS = _delegS_T[0] & io_now_privilege_internal_privilegeMode < 2'h3; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 155:39]
  wire  _T_595 = 8'h20 == io_now_privilege_csr_MXLEN; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 166:39]
  wire  _GEN_3352 = 2'h1 == io_now_privilege_csr_stvec[1:0] | _GEN_2114; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 308:45 316:29]
  wire  _GEN_3354 = 2'h0 == io_now_privilege_csr_stvec[1:0] | _GEN_3352; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 308:45 311:29]
  wire  _GEN_3364 = 8'h20 == io_now_privilege_csr_MXLEN ? _GEN_3354 : _GEN_2114; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 166:39]
  wire  _GEN_3372 = 2'h1 == io_now_privilege_csr_mtvec[1:0] | _GEN_2114; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 234:45 242:29]
  wire  _GEN_3374 = 2'h0 == io_now_privilege_csr_mtvec[1:0] | _GEN_3372; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 234:45 237:29]
  wire  _GEN_3384 = _T_595 ? _GEN_3374 : _GEN_2114; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 176:39]
  wire  _GEN_3395 = delegS ? _GEN_3364 : _GEN_3384; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 160:18]
  wire  _GEN_3412 = raiseExceptionIntr ? _GEN_3395 : _GEN_2114; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 136:30]
  wire  global_data_setpc = io_valid & _GEN_3412; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 154:21]
  wire [31:0] _GEN_3340 = ~global_data_setpc ? _next_reg_T_45 : _GEN_2115; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 192:30 198:17]
  wire [31:0] _next_privilege_csr_scause_T_1 = {1'h0,26'h0,exceptionNO}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 252:39]
  wire [31:0] _next_pc_T_21 = {io_now_privilege_csr_stvec[31:2], 2'h0}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 312:72]
  wire [31:0] _next_pc_T_23 = {27'h0,exceptionNO}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/tool/BitTool.scala 21:10]
  wire [31:0] _GEN_3513 = {{2'd0}, io_now_privilege_csr_stvec[31:2]}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 317:70]
  wire [31:0] _next_pc_T_25 = _GEN_3513 + _next_pc_T_23; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 317:70]
  wire [33:0] _next_pc_T_26 = {_next_pc_T_25, 2'h0}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 317:102]
  wire [33:0] _GEN_3353 = 2'h1 == io_now_privilege_csr_stvec[1:0] ? _next_pc_T_26 : {{2'd0}, _GEN_3340}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 308:45 317:29]
  wire [33:0] _GEN_3355 = 2'h0 == io_now_privilege_csr_stvec[1:0] ? {{2'd0}, _next_pc_T_21} : _GEN_3353; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 308:45 312:29]
  wire [31:0] _GEN_3356 = 8'h20 == io_now_privilege_csr_MXLEN ? _next_privilege_csr_scause_T_1 :
    io_now_privilege_csr_scause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 166:39 252:33]
  wire [33:0] _GEN_3365 = 8'h20 == io_now_privilege_csr_MXLEN ? _GEN_3355 : {{2'd0}, _GEN_3340}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 166:39]
  wire [31:0] _next_pc_T_28 = {io_now_privilege_csr_mtvec[31:2], 2'h0}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 238:72]
  wire [31:0] _GEN_3514 = {{2'd0}, io_now_privilege_csr_mtvec[31:2]}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 243:70]
  wire [31:0] _next_pc_T_32 = _GEN_3514 + _next_pc_T_23; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 243:70]
  wire [33:0] _next_pc_T_33 = {_next_pc_T_32, 2'h0}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 243:102]
  wire [33:0] _GEN_3373 = 2'h1 == io_now_privilege_csr_mtvec[1:0] ? _next_pc_T_33 : {{2'd0}, _GEN_3340}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 234:45 243:29]
  wire [33:0] _GEN_3375 = 2'h0 == io_now_privilege_csr_mtvec[1:0] ? {{2'd0}, _next_pc_T_28} : _GEN_3373; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 234:45 238:29]
  wire [31:0] _GEN_3376 = _T_595 ? _next_privilege_csr_scause_T_1 : io_now_privilege_csr_mcause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21 riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 176:39 185:45]
  wire [33:0] _GEN_3385 = _T_595 ? _GEN_3375 : {{2'd0}, _GEN_3340}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 176:39]
  wire [31:0] _GEN_3391 = delegS ? _GEN_3356 : io_now_privilege_csr_scause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 160:18 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_3408 = raiseExceptionIntr ? _GEN_3391 : io_now_privilege_csr_scause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 136:30 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] next_privilege_csr_scause = io_valid ? _GEN_3408 : io_now_privilege_csr_scause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  wire [31:0] _GEN_3400 = delegS ? io_now_privilege_csr_mcause : _GEN_3376; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 160:18 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] _GEN_3414 = raiseExceptionIntr ? _GEN_3400 : io_now_privilege_csr_mcause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 136:30 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [31:0] next_privilege_csr_mcause = io_valid ? _GEN_3414 : io_now_privilege_csr_mcause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  wire [31:0] _GEN_3386 = delegS ? next_privilege_csr_scause : next_privilege_csr_mcause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 160:18 161:45 171:45]
  wire [1:0] _GEN_3390 = delegS ? 2'h1 : 2'h3; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 160:18]
  wire [33:0] _GEN_3396 = delegS ? _GEN_3365 : _GEN_3385; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 160:18]
  wire [31:0] _GEN_3404 = raiseExceptionIntr ? _GEN_3386 : 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 136:30 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 155:21]
  wire [31:0] _GEN_3405 = raiseExceptionIntr ? io_now_pc : 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 136:30 151:25 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 155:21]
  wire [31:0] _GEN_3406 = raiseExceptionIntr ? inst : 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 136:30 152:25 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 155:21]
  wire [1:0] _GEN_3407 = raiseExceptionIntr ? _GEN_3390 : io_now_privilege_internal_privilegeMode; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 136:30 riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 152:21]
  wire [33:0] _GEN_3413 = raiseExceptionIntr ? _GEN_3396 : {{2'd0}, _GEN_3340}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/instset/csr/ExceptionSupport.scala 136:30]
  wire [33:0] _GEN_3474 = io_valid ? _GEN_3413 : {{2'd0}, io_now_pc}; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_mem_read_valid = io_valid & _GEN_2634; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 161:7]
  assign io_mem_read_addr = io_valid ? _GEN_2635 : 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 161:7]
  assign io_mem_read_memWidth = io_valid ? _GEN_2636 : 6'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 161:7]
  assign io_mem_write_valid = io_valid & _GEN_2730; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 161:7]
  assign io_mem_write_addr = io_valid ? _GEN_2731 : 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 161:7]
  assign io_mem_write_memWidth = io_valid ? _GEN_2732 : 6'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 161:7]
  assign io_mem_write_data = io_valid ? _GEN_2733 : 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 161:7]
  assign io_next_reg_0 = io_valid ? 32'h0 : io_now_reg_0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 190:17 152:21]
  assign io_next_reg_1 = io_valid ? _GEN_3309 : io_now_reg_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_2 = io_valid ? _GEN_3310 : io_now_reg_2; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_3 = io_valid ? _GEN_3311 : io_now_reg_3; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_4 = io_valid ? _GEN_3312 : io_now_reg_4; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_5 = io_valid ? _GEN_3313 : io_now_reg_5; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_6 = io_valid ? _GEN_3314 : io_now_reg_6; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_7 = io_valid ? _GEN_3315 : io_now_reg_7; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_8 = io_valid ? _GEN_3316 : io_now_reg_8; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_9 = io_valid ? _GEN_3317 : io_now_reg_9; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_10 = io_valid ? _GEN_3318 : io_now_reg_10; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_11 = io_valid ? _GEN_3319 : io_now_reg_11; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_12 = io_valid ? _GEN_3320 : io_now_reg_12; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_13 = io_valid ? _GEN_3321 : io_now_reg_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_14 = io_valid ? _GEN_3322 : io_now_reg_14; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_15 = io_valid ? _GEN_3323 : io_now_reg_15; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_16 = io_valid ? _GEN_3324 : io_now_reg_16; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_17 = io_valid ? _GEN_3325 : io_now_reg_17; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_18 = io_valid ? _GEN_3326 : io_now_reg_18; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_19 = io_valid ? _GEN_3327 : io_now_reg_19; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_20 = io_valid ? _GEN_3328 : io_now_reg_20; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_21 = io_valid ? _GEN_3329 : io_now_reg_21; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_22 = io_valid ? _GEN_3330 : io_now_reg_22; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_23 = io_valid ? _GEN_3331 : io_now_reg_23; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_24 = io_valid ? _GEN_3332 : io_now_reg_24; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_25 = io_valid ? _GEN_3333 : io_now_reg_25; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_26 = io_valid ? _GEN_3334 : io_now_reg_26; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_27 = io_valid ? _GEN_3335 : io_now_reg_27; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_28 = io_valid ? _GEN_3336 : io_now_reg_28; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_29 = io_valid ? _GEN_3337 : io_now_reg_29; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_30 = io_valid ? _GEN_3338 : io_now_reg_30; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_reg_31 = io_valid ? _GEN_3339 : io_now_reg_31; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_pc = _GEN_3474[31:0]; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 14:18]
  assign io_next_privilege_csr_misa = io_now_privilege_csr_misa; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 13:18 150:7]
  assign io_next_privilege_csr_mtvec = io_now_privilege_csr_mtvec; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 13:18 150:7]
  assign io_next_privilege_csr_medeleg = io_now_privilege_csr_medeleg; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 13:18 150:7]
  assign io_next_privilege_csr_mcause = io_valid ? _GEN_3414 : io_now_privilege_csr_mcause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_privilege_csr_scause = io_valid ? _GEN_3408 : io_now_privilege_csr_scause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_next_privilege_csr_stvec = io_now_privilege_csr_stvec; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 13:18 150:7]
  assign io_next_privilege_csr_MXLEN = io_now_privilege_csr_MXLEN; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 13:18 150:7]
  assign io_next_privilege_internal_privilegeMode = io_valid ? _GEN_3407 : io_now_privilege_internal_privilegeMode; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 152:21]
  assign io_event_valid = io_valid & raiseExceptionIntr; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 155:21]
  assign io_event_cause = io_valid ? _GEN_3404 : 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 155:21]
  assign io_event_exceptionPC = io_valid ? _GEN_3405 : 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 155:21]
  assign io_event_exceptionInst = io_valid ? _GEN_3406 : 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 165:18 155:21]
endmodule
module RiscvCore(
  input         clock,
  input         reset,
  input  [31:0] io_inst, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  input         io_valid, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output        io_mem_read_valid, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_mem_read_addr, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [5:0]  io_mem_read_memWidth, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  input  [31:0] io_mem_read_data, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output        io_mem_write_valid, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_mem_write_addr, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [5:0]  io_mem_write_memWidth, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_mem_write_data, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_now_pc, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_0, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_1, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_2, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_3, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_4, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_5, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_6, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_7, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_8, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_9, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_10, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_11, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_12, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_13, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_14, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_15, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_16, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_17, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_18, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_19, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_20, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_21, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_22, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_23, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_24, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_25, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_26, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_27, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_28, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_29, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_30, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_next_reg_31, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output        io_event_valid, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_event_cause, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_event_exceptionPC, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  output [31:0] io_event_exceptionInst, // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 217:14]
  input  [31:0] ArbitraryRegFile_0,
  input  [31:0] ArbitraryRegFile_1,
  input  [31:0] ArbitraryRegFile_2,
  input  [31:0] ArbitraryRegFile_3,
  input  [31:0] ArbitraryRegFile_4,
  input  [31:0] ArbitraryRegFile_5,
  input  [31:0] ArbitraryRegFile_6,
  input  [31:0] ArbitraryRegFile_7,
  input  [31:0] ArbitraryRegFile_8,
  input  [31:0] ArbitraryRegFile_9,
  input  [31:0] ArbitraryRegFile_10,
  input  [31:0] ArbitraryRegFile_11,
  input  [31:0] ArbitraryRegFile_12,
  input  [31:0] ArbitraryRegFile_13,
  input  [31:0] ArbitraryRegFile_14,
  input  [31:0] ArbitraryRegFile_15,
  input  [31:0] ArbitraryRegFile_16,
  input  [31:0] ArbitraryRegFile_17,
  input  [31:0] ArbitraryRegFile_18,
  input  [31:0] ArbitraryRegFile_19,
  input  [31:0] ArbitraryRegFile_20,
  input  [31:0] ArbitraryRegFile_21,
  input  [31:0] ArbitraryRegFile_22,
  input  [31:0] ArbitraryRegFile_23,
  input  [31:0] ArbitraryRegFile_24,
  input  [31:0] ArbitraryRegFile_25,
  input  [31:0] ArbitraryRegFile_26,
  input  [31:0] ArbitraryRegFile_27,
  input  [31:0] ArbitraryRegFile_28,
  input  [31:0] ArbitraryRegFile_29,
  input  [31:0] ArbitraryRegFile_30,
  input  [31:0] ArbitraryRegFile_31
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
  reg [31:0] _RAND_4;
  reg [31:0] _RAND_5;
  reg [31:0] _RAND_6;
  reg [31:0] _RAND_7;
  reg [31:0] _RAND_8;
  reg [31:0] _RAND_9;
  reg [31:0] _RAND_10;
  reg [31:0] _RAND_11;
  reg [31:0] _RAND_12;
  reg [31:0] _RAND_13;
  reg [31:0] _RAND_14;
  reg [31:0] _RAND_15;
  reg [31:0] _RAND_16;
  reg [31:0] _RAND_17;
  reg [31:0] _RAND_18;
  reg [31:0] _RAND_19;
  reg [31:0] _RAND_20;
  reg [31:0] _RAND_21;
  reg [31:0] _RAND_22;
  reg [31:0] _RAND_23;
  reg [31:0] _RAND_24;
  reg [31:0] _RAND_25;
  reg [31:0] _RAND_26;
  reg [31:0] _RAND_27;
  reg [31:0] _RAND_28;
  reg [31:0] _RAND_29;
  reg [31:0] _RAND_30;
  reg [31:0] _RAND_31;
  reg [31:0] _RAND_32;
  reg [31:0] _RAND_33;
  reg [31:0] _RAND_34;
  reg [31:0] _RAND_35;
  reg [31:0] _RAND_36;
  reg [31:0] _RAND_37;
  reg [31:0] _RAND_38;
  reg [31:0] _RAND_39;
  reg [31:0] _RAND_40;
`endif // RANDOMIZE_REG_INIT
  wire [31:0] trans_io_inst; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire  trans_io_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire  trans_io_mem_read_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_mem_read_addr; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [5:0] trans_io_mem_read_memWidth; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_mem_read_data; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire  trans_io_mem_write_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_mem_write_addr; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [5:0] trans_io_mem_write_memWidth; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_mem_write_data; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_2; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_3; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_4; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_5; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_6; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_7; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_8; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_9; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_10; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_11; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_12; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_14; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_15; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_16; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_17; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_18; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_19; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_20; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_21; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_22; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_23; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_24; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_25; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_26; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_27; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_28; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_29; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_30; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_reg_31; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_pc; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_privilege_csr_misa; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_privilege_csr_mtvec; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_privilege_csr_medeleg; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_privilege_csr_mcause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_privilege_csr_scause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_now_privilege_csr_stvec; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [7:0] trans_io_now_privilege_csr_MXLEN; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [1:0] trans_io_now_privilege_internal_privilegeMode; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_2; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_3; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_4; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_5; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_6; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_7; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_8; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_9; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_10; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_11; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_12; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_14; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_15; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_16; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_17; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_18; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_19; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_20; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_21; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_22; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_23; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_24; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_25; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_26; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_27; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_28; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_29; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_30; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_reg_31; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_pc; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_privilege_csr_misa; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_privilege_csr_mtvec; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_privilege_csr_medeleg; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_privilege_csr_mcause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_privilege_csr_scause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_next_privilege_csr_stvec; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [7:0] trans_io_next_privilege_csr_MXLEN; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [1:0] trans_io_next_privilege_internal_privilegeMode; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire  trans_io_event_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_event_cause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_event_exceptionPC; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  wire [31:0] trans_io_event_exceptionInst; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
  reg [31:0] state_reg_0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_2; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_3; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_4; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_5; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_6; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_7; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_8; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_9; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_10; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_11; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_12; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_14; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_15; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_16; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_17; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_18; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_19; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_20; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_21; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_22; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_23; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_24; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_25; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_26; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_27; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_28; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_29; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_30; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_reg_31; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_pc; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_privilege_csr_misa; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_privilege_csr_mtvec; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_privilege_csr_medeleg; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_privilege_csr_mcause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_privilege_csr_scause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [31:0] state_privilege_csr_stvec; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [7:0] state_privilege_csr_MXLEN; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  reg [1:0] state_privilege_internal_privilegeMode; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
  RiscvTrans trans ( // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 232:21]
    .io_inst(trans_io_inst),
    .io_valid(trans_io_valid),
    .io_mem_read_valid(trans_io_mem_read_valid),
    .io_mem_read_addr(trans_io_mem_read_addr),
    .io_mem_read_memWidth(trans_io_mem_read_memWidth),
    .io_mem_read_data(trans_io_mem_read_data),
    .io_mem_write_valid(trans_io_mem_write_valid),
    .io_mem_write_addr(trans_io_mem_write_addr),
    .io_mem_write_memWidth(trans_io_mem_write_memWidth),
    .io_mem_write_data(trans_io_mem_write_data),
    .io_now_reg_0(trans_io_now_reg_0),
    .io_now_reg_1(trans_io_now_reg_1),
    .io_now_reg_2(trans_io_now_reg_2),
    .io_now_reg_3(trans_io_now_reg_3),
    .io_now_reg_4(trans_io_now_reg_4),
    .io_now_reg_5(trans_io_now_reg_5),
    .io_now_reg_6(trans_io_now_reg_6),
    .io_now_reg_7(trans_io_now_reg_7),
    .io_now_reg_8(trans_io_now_reg_8),
    .io_now_reg_9(trans_io_now_reg_9),
    .io_now_reg_10(trans_io_now_reg_10),
    .io_now_reg_11(trans_io_now_reg_11),
    .io_now_reg_12(trans_io_now_reg_12),
    .io_now_reg_13(trans_io_now_reg_13),
    .io_now_reg_14(trans_io_now_reg_14),
    .io_now_reg_15(trans_io_now_reg_15),
    .io_now_reg_16(trans_io_now_reg_16),
    .io_now_reg_17(trans_io_now_reg_17),
    .io_now_reg_18(trans_io_now_reg_18),
    .io_now_reg_19(trans_io_now_reg_19),
    .io_now_reg_20(trans_io_now_reg_20),
    .io_now_reg_21(trans_io_now_reg_21),
    .io_now_reg_22(trans_io_now_reg_22),
    .io_now_reg_23(trans_io_now_reg_23),
    .io_now_reg_24(trans_io_now_reg_24),
    .io_now_reg_25(trans_io_now_reg_25),
    .io_now_reg_26(trans_io_now_reg_26),
    .io_now_reg_27(trans_io_now_reg_27),
    .io_now_reg_28(trans_io_now_reg_28),
    .io_now_reg_29(trans_io_now_reg_29),
    .io_now_reg_30(trans_io_now_reg_30),
    .io_now_reg_31(trans_io_now_reg_31),
    .io_now_pc(trans_io_now_pc),
    .io_now_privilege_csr_misa(trans_io_now_privilege_csr_misa),
    .io_now_privilege_csr_mtvec(trans_io_now_privilege_csr_mtvec),
    .io_now_privilege_csr_medeleg(trans_io_now_privilege_csr_medeleg),
    .io_now_privilege_csr_mcause(trans_io_now_privilege_csr_mcause),
    .io_now_privilege_csr_scause(trans_io_now_privilege_csr_scause),
    .io_now_privilege_csr_stvec(trans_io_now_privilege_csr_stvec),
    .io_now_privilege_csr_MXLEN(trans_io_now_privilege_csr_MXLEN),
    .io_now_privilege_internal_privilegeMode(trans_io_now_privilege_internal_privilegeMode),
    .io_next_reg_0(trans_io_next_reg_0),
    .io_next_reg_1(trans_io_next_reg_1),
    .io_next_reg_2(trans_io_next_reg_2),
    .io_next_reg_3(trans_io_next_reg_3),
    .io_next_reg_4(trans_io_next_reg_4),
    .io_next_reg_5(trans_io_next_reg_5),
    .io_next_reg_6(trans_io_next_reg_6),
    .io_next_reg_7(trans_io_next_reg_7),
    .io_next_reg_8(trans_io_next_reg_8),
    .io_next_reg_9(trans_io_next_reg_9),
    .io_next_reg_10(trans_io_next_reg_10),
    .io_next_reg_11(trans_io_next_reg_11),
    .io_next_reg_12(trans_io_next_reg_12),
    .io_next_reg_13(trans_io_next_reg_13),
    .io_next_reg_14(trans_io_next_reg_14),
    .io_next_reg_15(trans_io_next_reg_15),
    .io_next_reg_16(trans_io_next_reg_16),
    .io_next_reg_17(trans_io_next_reg_17),
    .io_next_reg_18(trans_io_next_reg_18),
    .io_next_reg_19(trans_io_next_reg_19),
    .io_next_reg_20(trans_io_next_reg_20),
    .io_next_reg_21(trans_io_next_reg_21),
    .io_next_reg_22(trans_io_next_reg_22),
    .io_next_reg_23(trans_io_next_reg_23),
    .io_next_reg_24(trans_io_next_reg_24),
    .io_next_reg_25(trans_io_next_reg_25),
    .io_next_reg_26(trans_io_next_reg_26),
    .io_next_reg_27(trans_io_next_reg_27),
    .io_next_reg_28(trans_io_next_reg_28),
    .io_next_reg_29(trans_io_next_reg_29),
    .io_next_reg_30(trans_io_next_reg_30),
    .io_next_reg_31(trans_io_next_reg_31),
    .io_next_pc(trans_io_next_pc),
    .io_next_privilege_csr_misa(trans_io_next_privilege_csr_misa),
    .io_next_privilege_csr_mtvec(trans_io_next_privilege_csr_mtvec),
    .io_next_privilege_csr_medeleg(trans_io_next_privilege_csr_medeleg),
    .io_next_privilege_csr_mcause(trans_io_next_privilege_csr_mcause),
    .io_next_privilege_csr_scause(trans_io_next_privilege_csr_scause),
    .io_next_privilege_csr_stvec(trans_io_next_privilege_csr_stvec),
    .io_next_privilege_csr_MXLEN(trans_io_next_privilege_csr_MXLEN),
    .io_next_privilege_internal_privilegeMode(trans_io_next_privilege_internal_privilegeMode),
    .io_event_valid(trans_io_event_valid),
    .io_event_cause(trans_io_event_cause),
    .io_event_exceptionPC(trans_io_event_exceptionPC),
    .io_event_exceptionInst(trans_io_event_exceptionInst)
  );
  assign io_mem_read_valid = trans_io_mem_read_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 236:16]
  assign io_mem_read_addr = trans_io_mem_read_addr; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 236:16]
  assign io_mem_read_memWidth = trans_io_mem_read_memWidth; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 236:16]
  assign io_mem_write_valid = trans_io_mem_write_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 236:16]
  assign io_mem_write_addr = trans_io_mem_write_addr; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 236:16]
  assign io_mem_write_memWidth = trans_io_mem_write_memWidth; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 236:16]
  assign io_mem_write_data = trans_io_mem_write_data; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 236:16]
  assign io_now_pc = state_pc; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 242:15]
  assign io_next_reg_0 = trans_io_next_reg_0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_1 = trans_io_next_reg_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_2 = trans_io_next_reg_2; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_3 = trans_io_next_reg_3; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_4 = trans_io_next_reg_4; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_5 = trans_io_next_reg_5; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_6 = trans_io_next_reg_6; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_7 = trans_io_next_reg_7; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_8 = trans_io_next_reg_8; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_9 = trans_io_next_reg_9; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_10 = trans_io_next_reg_10; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_11 = trans_io_next_reg_11; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_12 = trans_io_next_reg_12; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_13 = trans_io_next_reg_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_14 = trans_io_next_reg_14; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_15 = trans_io_next_reg_15; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_16 = trans_io_next_reg_16; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_17 = trans_io_next_reg_17; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_18 = trans_io_next_reg_18; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_19 = trans_io_next_reg_19; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_20 = trans_io_next_reg_20; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_21 = trans_io_next_reg_21; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_22 = trans_io_next_reg_22; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_23 = trans_io_next_reg_23; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_24 = trans_io_next_reg_24; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_25 = trans_io_next_reg_25; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_26 = trans_io_next_reg_26; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_27 = trans_io_next_reg_27; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_28 = trans_io_next_reg_28; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_29 = trans_io_next_reg_29; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_30 = trans_io_next_reg_30; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_next_reg_31 = trans_io_next_reg_31; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 243:15]
  assign io_event_valid = trans_io_event_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 244:15]
  assign io_event_cause = trans_io_event_cause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 244:15]
  assign io_event_exceptionPC = trans_io_event_exceptionPC; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 244:15]
  assign io_event_exceptionInst = trans_io_event_exceptionInst; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 244:15]
  assign trans_io_inst = io_inst; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 234:18]
  assign trans_io_valid = io_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 235:18]
  assign trans_io_mem_read_data = io_mem_read_data; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 236:16]
  assign trans_io_now_reg_0 = state_reg_0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_1 = state_reg_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_2 = state_reg_2; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_3 = state_reg_3; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_4 = state_reg_4; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_5 = state_reg_5; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_6 = state_reg_6; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_7 = state_reg_7; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_8 = state_reg_8; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_9 = state_reg_9; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_10 = state_reg_10; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_11 = state_reg_11; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_12 = state_reg_12; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_13 = state_reg_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_14 = state_reg_14; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_15 = state_reg_15; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_16 = state_reg_16; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_17 = state_reg_17; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_18 = state_reg_18; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_19 = state_reg_19; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_20 = state_reg_20; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_21 = state_reg_21; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_22 = state_reg_22; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_23 = state_reg_23; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_24 = state_reg_24; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_25 = state_reg_25; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_26 = state_reg_26; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_27 = state_reg_27; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_28 = state_reg_28; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_29 = state_reg_29; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_30 = state_reg_30; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_reg_31 = state_reg_31; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_pc = state_pc; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_privilege_csr_misa = state_privilege_csr_misa; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_privilege_csr_mtvec = state_privilege_csr_mtvec; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_privilege_csr_medeleg = state_privilege_csr_medeleg; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_privilege_csr_mcause = state_privilege_csr_mcause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_privilege_csr_scause = state_privilege_csr_scause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_privilege_csr_stvec = state_privilege_csr_stvec; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_privilege_csr_MXLEN = state_privilege_csr_MXLEN; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  assign trans_io_now_privilege_internal_privilegeMode = state_privilege_internal_privilegeMode; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 239:16]
  always @(posedge clock) begin
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_0 <= ArbitraryRegFile_0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_0 <= trans_io_next_reg_0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_1 <= ArbitraryRegFile_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_1 <= trans_io_next_reg_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_2 <= ArbitraryRegFile_2; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_2 <= trans_io_next_reg_2; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_3 <= ArbitraryRegFile_3; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_3 <= trans_io_next_reg_3; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_4 <= ArbitraryRegFile_4; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_4 <= trans_io_next_reg_4; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_5 <= ArbitraryRegFile_5; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_5 <= trans_io_next_reg_5; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_6 <= ArbitraryRegFile_6; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_6 <= trans_io_next_reg_6; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_7 <= ArbitraryRegFile_7; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_7 <= trans_io_next_reg_7; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_8 <= ArbitraryRegFile_8; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_8 <= trans_io_next_reg_8; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_9 <= ArbitraryRegFile_9; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_9 <= trans_io_next_reg_9; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_10 <= ArbitraryRegFile_10; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_10 <= trans_io_next_reg_10; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_11 <= ArbitraryRegFile_11; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_11 <= trans_io_next_reg_11; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_12 <= ArbitraryRegFile_12; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_12 <= trans_io_next_reg_12; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_13 <= ArbitraryRegFile_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_13 <= trans_io_next_reg_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_14 <= ArbitraryRegFile_14; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_14 <= trans_io_next_reg_14; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_15 <= ArbitraryRegFile_15; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_15 <= trans_io_next_reg_15; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_16 <= ArbitraryRegFile_16; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_16 <= trans_io_next_reg_16; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_17 <= ArbitraryRegFile_17; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_17 <= trans_io_next_reg_17; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_18 <= ArbitraryRegFile_18; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_18 <= trans_io_next_reg_18; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_19 <= ArbitraryRegFile_19; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_19 <= trans_io_next_reg_19; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_20 <= ArbitraryRegFile_20; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_20 <= trans_io_next_reg_20; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_21 <= ArbitraryRegFile_21; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_21 <= trans_io_next_reg_21; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_22 <= ArbitraryRegFile_22; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_22 <= trans_io_next_reg_22; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_23 <= ArbitraryRegFile_23; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_23 <= trans_io_next_reg_23; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_24 <= ArbitraryRegFile_24; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_24 <= trans_io_next_reg_24; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_25 <= ArbitraryRegFile_25; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_25 <= trans_io_next_reg_25; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_26 <= ArbitraryRegFile_26; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_26 <= trans_io_next_reg_26; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_27 <= ArbitraryRegFile_27; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_27 <= trans_io_next_reg_27; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_28 <= ArbitraryRegFile_28; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_28 <= trans_io_next_reg_28; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_29 <= ArbitraryRegFile_29; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_29 <= trans_io_next_reg_29; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_30 <= ArbitraryRegFile_30; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_30 <= trans_io_next_reg_30; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_reg_31 <= ArbitraryRegFile_31; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_reg_31 <= trans_io_next_reg_31; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_pc <= 32'h200; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_pc <= trans_io_next_pc; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_privilege_csr_misa <= 32'h40001100; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_privilege_csr_misa <= trans_io_next_privilege_csr_misa; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_privilege_csr_mtvec <= 32'h1c0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_privilege_csr_mtvec <= trans_io_next_privilege_csr_mtvec; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_privilege_csr_medeleg <= 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_privilege_csr_medeleg <= trans_io_next_privilege_csr_medeleg; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_privilege_csr_mcause <= 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_privilege_csr_mcause <= trans_io_next_privilege_csr_mcause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_privilege_csr_scause <= 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_privilege_csr_scause <= trans_io_next_privilege_csr_scause; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_privilege_csr_stvec <= 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_privilege_csr_stvec <= trans_io_next_privilege_csr_stvec; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_privilege_csr_MXLEN <= 8'h20; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_privilege_csr_MXLEN <= trans_io_next_privilege_csr_MXLEN; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
    if (reset) begin // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
      state_privilege_internal_privilegeMode <= 2'h3; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 231:22]
    end else begin
      state_privilege_internal_privilegeMode <= trans_io_next_privilege_internal_privilegeMode; // @[riscv-spec-core/src/main/scala/rvspeccore/core/RiscvCore.scala 240:16]
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  state_reg_0 = _RAND_0[31:0];
  _RAND_1 = {1{`RANDOM}};
  state_reg_1 = _RAND_1[31:0];
  _RAND_2 = {1{`RANDOM}};
  state_reg_2 = _RAND_2[31:0];
  _RAND_3 = {1{`RANDOM}};
  state_reg_3 = _RAND_3[31:0];
  _RAND_4 = {1{`RANDOM}};
  state_reg_4 = _RAND_4[31:0];
  _RAND_5 = {1{`RANDOM}};
  state_reg_5 = _RAND_5[31:0];
  _RAND_6 = {1{`RANDOM}};
  state_reg_6 = _RAND_6[31:0];
  _RAND_7 = {1{`RANDOM}};
  state_reg_7 = _RAND_7[31:0];
  _RAND_8 = {1{`RANDOM}};
  state_reg_8 = _RAND_8[31:0];
  _RAND_9 = {1{`RANDOM}};
  state_reg_9 = _RAND_9[31:0];
  _RAND_10 = {1{`RANDOM}};
  state_reg_10 = _RAND_10[31:0];
  _RAND_11 = {1{`RANDOM}};
  state_reg_11 = _RAND_11[31:0];
  _RAND_12 = {1{`RANDOM}};
  state_reg_12 = _RAND_12[31:0];
  _RAND_13 = {1{`RANDOM}};
  state_reg_13 = _RAND_13[31:0];
  _RAND_14 = {1{`RANDOM}};
  state_reg_14 = _RAND_14[31:0];
  _RAND_15 = {1{`RANDOM}};
  state_reg_15 = _RAND_15[31:0];
  _RAND_16 = {1{`RANDOM}};
  state_reg_16 = _RAND_16[31:0];
  _RAND_17 = {1{`RANDOM}};
  state_reg_17 = _RAND_17[31:0];
  _RAND_18 = {1{`RANDOM}};
  state_reg_18 = _RAND_18[31:0];
  _RAND_19 = {1{`RANDOM}};
  state_reg_19 = _RAND_19[31:0];
  _RAND_20 = {1{`RANDOM}};
  state_reg_20 = _RAND_20[31:0];
  _RAND_21 = {1{`RANDOM}};
  state_reg_21 = _RAND_21[31:0];
  _RAND_22 = {1{`RANDOM}};
  state_reg_22 = _RAND_22[31:0];
  _RAND_23 = {1{`RANDOM}};
  state_reg_23 = _RAND_23[31:0];
  _RAND_24 = {1{`RANDOM}};
  state_reg_24 = _RAND_24[31:0];
  _RAND_25 = {1{`RANDOM}};
  state_reg_25 = _RAND_25[31:0];
  _RAND_26 = {1{`RANDOM}};
  state_reg_26 = _RAND_26[31:0];
  _RAND_27 = {1{`RANDOM}};
  state_reg_27 = _RAND_27[31:0];
  _RAND_28 = {1{`RANDOM}};
  state_reg_28 = _RAND_28[31:0];
  _RAND_29 = {1{`RANDOM}};
  state_reg_29 = _RAND_29[31:0];
  _RAND_30 = {1{`RANDOM}};
  state_reg_30 = _RAND_30[31:0];
  _RAND_31 = {1{`RANDOM}};
  state_reg_31 = _RAND_31[31:0];
  _RAND_32 = {1{`RANDOM}};
  state_pc = _RAND_32[31:0];
  _RAND_33 = {1{`RANDOM}};
  state_privilege_csr_misa = _RAND_33[31:0];
  _RAND_34 = {1{`RANDOM}};
  state_privilege_csr_mtvec = _RAND_34[31:0];
  _RAND_35 = {1{`RANDOM}};
  state_privilege_csr_medeleg = _RAND_35[31:0];
  _RAND_36 = {1{`RANDOM}};
  state_privilege_csr_mcause = _RAND_36[31:0];
  _RAND_37 = {1{`RANDOM}};
  state_privilege_csr_scause = _RAND_37[31:0];
  _RAND_38 = {1{`RANDOM}};
  state_privilege_csr_stvec = _RAND_38[31:0];
  _RAND_39 = {1{`RANDOM}};
  state_privilege_csr_MXLEN = _RAND_39[7:0];
  _RAND_40 = {1{`RANDOM}};
  state_privilege_internal_privilegeMode = _RAND_40[1:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module CheckerWithResult(
  input         clock,
  input         reset,
  input         io_instCommit_valid, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_instCommit_inst, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_instCommit_pc, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_0, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_1, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_2, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_3, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_4, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_5, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_6, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_7, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_8, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_9, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_10, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_11, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_12, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_13, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_14, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_15, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_16, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_17, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_18, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_19, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_20, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_21, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_22, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_23, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_24, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_25, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_26, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_27, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_28, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_29, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_30, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_result_reg_31, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input         io_event_valid, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_event_intrNO, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_event_cause, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_event_exceptionPC, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_event_exceptionInst, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input         io_mem_read_valid, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_mem_read_addr, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [5:0]  io_mem_read_memWidth, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_mem_read_data, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input         io_mem_write_valid, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_mem_write_addr, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [5:0]  io_mem_write_memWidth, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] io_mem_write_data, // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 61:14]
  input  [31:0] rf_0,
  input  [31:0] rf_1,
  input  [31:0] rf_2,
  input  [31:0] rf_3,
  input  [31:0] rf_4,
  input  [31:0] rf_5,
  input  [31:0] rf_6,
  input  [31:0] rf_7,
  input  [31:0] rf_8,
  input  [31:0] rf_9,
  input  [31:0] rf_10,
  input  [31:0] rf_11,
  input  [31:0] rf_12,
  input  [31:0] rf_13,
  input  [31:0] rf_14,
  input  [31:0] rf_15,
  input  [31:0] rf_16,
  input  [31:0] rf_17,
  input  [31:0] rf_18,
  input  [31:0] rf_19,
  input  [31:0] rf_20,
  input  [31:0] rf_21,
  input  [31:0] rf_22,
  input  [31:0] rf_23,
  input  [31:0] rf_24,
  input  [31:0] rf_25,
  input  [31:0] rf_26,
  input  [31:0] rf_27,
  input  [31:0] rf_28,
  input  [31:0] rf_29,
  input  [31:0] rf_30,
  input  [31:0] rf_31
);
  wire  specCore_clock; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire  specCore_reset; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_inst; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire  specCore_io_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire  specCore_io_mem_read_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_mem_read_addr; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [5:0] specCore_io_mem_read_memWidth; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_mem_read_data; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire  specCore_io_mem_write_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_mem_write_addr; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [5:0] specCore_io_mem_write_memWidth; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_mem_write_data; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_now_pc; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_0; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_1; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_2; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_3; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_4; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_5; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_6; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_7; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_8; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_9; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_10; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_11; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_12; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_13; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_14; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_15; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_16; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_17; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_18; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_19; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_20; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_21; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_22; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_23; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_24; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_25; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_26; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_27; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_28; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_29; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_30; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_next_reg_31; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire  specCore_io_event_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_event_cause; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_event_exceptionPC; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_io_event_exceptionInst; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_0; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_1; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_2; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_3; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_4; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_5; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_6; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_7; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_8; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_9; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_10; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_11; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_12; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_13; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_14; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_15; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_16; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_17; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_18; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_19; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_20; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_21; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_22; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_23; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_24; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_25; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_26; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_27; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_28; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_29; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_30; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire [31:0] specCore_ArbitraryRegFile_31; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
  wire  _T_2 = ~reset; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 96:13]
  wire  _T_4 = io_mem_read_valid | specCore_io_mem_read_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 97:43]
  wire  _T_17 = io_mem_write_valid | specCore_io_mem_write_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 102:44]
  wire  _T_162 = io_event_valid | specCore_io_event_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 208:33]
  wire  _T_163 = io_event_valid == specCore_io_event_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 210:32]
  wire  _GEN_0 = _T_4 & _T_2; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 98:15]
  wire  _GEN_2 = _T_17 & _T_2; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 103:15]
  wire  _GEN_5 = io_instCommit_valid & _T_2; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 192:11]
  wire  _GEN_38 = _T_162 & _T_2; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 209:11]
  RiscvCore specCore ( // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 81:24]
    .clock(specCore_clock),
    .reset(specCore_reset),
    .io_inst(specCore_io_inst),
    .io_valid(specCore_io_valid),
    .io_mem_read_valid(specCore_io_mem_read_valid),
    .io_mem_read_addr(specCore_io_mem_read_addr),
    .io_mem_read_memWidth(specCore_io_mem_read_memWidth),
    .io_mem_read_data(specCore_io_mem_read_data),
    .io_mem_write_valid(specCore_io_mem_write_valid),
    .io_mem_write_addr(specCore_io_mem_write_addr),
    .io_mem_write_memWidth(specCore_io_mem_write_memWidth),
    .io_mem_write_data(specCore_io_mem_write_data),
    .io_now_pc(specCore_io_now_pc),
    .io_next_reg_0(specCore_io_next_reg_0),
    .io_next_reg_1(specCore_io_next_reg_1),
    .io_next_reg_2(specCore_io_next_reg_2),
    .io_next_reg_3(specCore_io_next_reg_3),
    .io_next_reg_4(specCore_io_next_reg_4),
    .io_next_reg_5(specCore_io_next_reg_5),
    .io_next_reg_6(specCore_io_next_reg_6),
    .io_next_reg_7(specCore_io_next_reg_7),
    .io_next_reg_8(specCore_io_next_reg_8),
    .io_next_reg_9(specCore_io_next_reg_9),
    .io_next_reg_10(specCore_io_next_reg_10),
    .io_next_reg_11(specCore_io_next_reg_11),
    .io_next_reg_12(specCore_io_next_reg_12),
    .io_next_reg_13(specCore_io_next_reg_13),
    .io_next_reg_14(specCore_io_next_reg_14),
    .io_next_reg_15(specCore_io_next_reg_15),
    .io_next_reg_16(specCore_io_next_reg_16),
    .io_next_reg_17(specCore_io_next_reg_17),
    .io_next_reg_18(specCore_io_next_reg_18),
    .io_next_reg_19(specCore_io_next_reg_19),
    .io_next_reg_20(specCore_io_next_reg_20),
    .io_next_reg_21(specCore_io_next_reg_21),
    .io_next_reg_22(specCore_io_next_reg_22),
    .io_next_reg_23(specCore_io_next_reg_23),
    .io_next_reg_24(specCore_io_next_reg_24),
    .io_next_reg_25(specCore_io_next_reg_25),
    .io_next_reg_26(specCore_io_next_reg_26),
    .io_next_reg_27(specCore_io_next_reg_27),
    .io_next_reg_28(specCore_io_next_reg_28),
    .io_next_reg_29(specCore_io_next_reg_29),
    .io_next_reg_30(specCore_io_next_reg_30),
    .io_next_reg_31(specCore_io_next_reg_31),
    .io_event_valid(specCore_io_event_valid),
    .io_event_cause(specCore_io_event_cause),
    .io_event_exceptionPC(specCore_io_event_exceptionPC),
    .io_event_exceptionInst(specCore_io_event_exceptionInst),
    .ArbitraryRegFile_0(specCore_ArbitraryRegFile_0),
    .ArbitraryRegFile_1(specCore_ArbitraryRegFile_1),
    .ArbitraryRegFile_2(specCore_ArbitraryRegFile_2),
    .ArbitraryRegFile_3(specCore_ArbitraryRegFile_3),
    .ArbitraryRegFile_4(specCore_ArbitraryRegFile_4),
    .ArbitraryRegFile_5(specCore_ArbitraryRegFile_5),
    .ArbitraryRegFile_6(specCore_ArbitraryRegFile_6),
    .ArbitraryRegFile_7(specCore_ArbitraryRegFile_7),
    .ArbitraryRegFile_8(specCore_ArbitraryRegFile_8),
    .ArbitraryRegFile_9(specCore_ArbitraryRegFile_9),
    .ArbitraryRegFile_10(specCore_ArbitraryRegFile_10),
    .ArbitraryRegFile_11(specCore_ArbitraryRegFile_11),
    .ArbitraryRegFile_12(specCore_ArbitraryRegFile_12),
    .ArbitraryRegFile_13(specCore_ArbitraryRegFile_13),
    .ArbitraryRegFile_14(specCore_ArbitraryRegFile_14),
    .ArbitraryRegFile_15(specCore_ArbitraryRegFile_15),
    .ArbitraryRegFile_16(specCore_ArbitraryRegFile_16),
    .ArbitraryRegFile_17(specCore_ArbitraryRegFile_17),
    .ArbitraryRegFile_18(specCore_ArbitraryRegFile_18),
    .ArbitraryRegFile_19(specCore_ArbitraryRegFile_19),
    .ArbitraryRegFile_20(specCore_ArbitraryRegFile_20),
    .ArbitraryRegFile_21(specCore_ArbitraryRegFile_21),
    .ArbitraryRegFile_22(specCore_ArbitraryRegFile_22),
    .ArbitraryRegFile_23(specCore_ArbitraryRegFile_23),
    .ArbitraryRegFile_24(specCore_ArbitraryRegFile_24),
    .ArbitraryRegFile_25(specCore_ArbitraryRegFile_25),
    .ArbitraryRegFile_26(specCore_ArbitraryRegFile_26),
    .ArbitraryRegFile_27(specCore_ArbitraryRegFile_27),
    .ArbitraryRegFile_28(specCore_ArbitraryRegFile_28),
    .ArbitraryRegFile_29(specCore_ArbitraryRegFile_29),
    .ArbitraryRegFile_30(specCore_ArbitraryRegFile_30),
    .ArbitraryRegFile_31(specCore_ArbitraryRegFile_31)
  );
  assign specCore_clock = clock;
  assign specCore_reset = reset;
  assign specCore_io_inst = io_instCommit_inst; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 83:21]
  assign specCore_io_valid = io_instCommit_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 82:21]
  assign specCore_io_mem_read_data = io_mem_read_data; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 107:33]
  assign specCore_ArbitraryRegFile_0 = rf_0;
  assign specCore_ArbitraryRegFile_1 = rf_1;
  assign specCore_ArbitraryRegFile_2 = rf_2;
  assign specCore_ArbitraryRegFile_3 = rf_3;
  assign specCore_ArbitraryRegFile_4 = rf_4;
  assign specCore_ArbitraryRegFile_5 = rf_5;
  assign specCore_ArbitraryRegFile_6 = rf_6;
  assign specCore_ArbitraryRegFile_7 = rf_7;
  assign specCore_ArbitraryRegFile_8 = rf_8;
  assign specCore_ArbitraryRegFile_9 = rf_9;
  assign specCore_ArbitraryRegFile_10 = rf_10;
  assign specCore_ArbitraryRegFile_11 = rf_11;
  assign specCore_ArbitraryRegFile_12 = rf_12;
  assign specCore_ArbitraryRegFile_13 = rf_13;
  assign specCore_ArbitraryRegFile_14 = rf_14;
  assign specCore_ArbitraryRegFile_15 = rf_15;
  assign specCore_ArbitraryRegFile_16 = rf_16;
  assign specCore_ArbitraryRegFile_17 = rf_17;
  assign specCore_ArbitraryRegFile_18 = rf_18;
  assign specCore_ArbitraryRegFile_19 = rf_19;
  assign specCore_ArbitraryRegFile_20 = rf_20;
  assign specCore_ArbitraryRegFile_21 = rf_21;
  assign specCore_ArbitraryRegFile_22 = rf_22;
  assign specCore_ArbitraryRegFile_23 = rf_23;
  assign specCore_ArbitraryRegFile_24 = rf_24;
  assign specCore_ArbitraryRegFile_25 = rf_25;
  assign specCore_ArbitraryRegFile_26 = rf_26;
  assign specCore_ArbitraryRegFile_27 = rf_27;
  assign specCore_ArbitraryRegFile_28 = rf_28;
  assign specCore_ArbitraryRegFile_29 = rf_29;
  assign specCore_ArbitraryRegFile_30 = rf_30;
  assign specCore_ArbitraryRegFile_31 = rf_31;
  always @(posedge clock) begin
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (~reset & ~(io_mem_read_valid == specCore_io_mem_read_valid)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:96 assert(regDelay(io.mem.get.read.valid) === regDelay(specCore.io.mem.read.valid))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 96:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_T_4 & _T_2 & ~(io_mem_read_addr == specCore_io_mem_read_addr)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:98 assert(regDelay(io.mem.get.read.addr) === regDelay(specCore.io.mem.read.addr))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 98:15]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_0 & ~(io_mem_read_memWidth == specCore_io_mem_read_memWidth)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:99 assert(regDelay(io.mem.get.read.memWidth) === regDelay(specCore.io.mem.read.memWidth))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 99:15]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_T_2 & ~(io_mem_write_valid == specCore_io_mem_write_valid)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:101 assert(regDelay(io.mem.get.write.valid) === regDelay(specCore.io.mem.write.valid))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 101:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_T_17 & _T_2 & ~(io_mem_write_addr == specCore_io_mem_write_addr)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:103 assert(regDelay(io.mem.get.write.addr) === regDelay(specCore.io.mem.write.addr))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 103:15]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_2 & ~(io_mem_write_data == specCore_io_mem_write_data)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:104 assert(regDelay(io.mem.get.write.data) === regDelay(specCore.io.mem.write.data))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 104:15]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_2 & ~(io_mem_write_memWidth == specCore_io_mem_write_memWidth)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:105 assert(regDelay(io.mem.get.write.memWidth) === regDelay(specCore.io.mem.write.memWidth))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 105:15]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (io_instCommit_valid & _T_2 & ~(io_instCommit_pc == specCore_io_now_pc)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:192 assert(regDelay(io.instCommit.pc) === regDelay(specCore.io.now.pc))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 192:11]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_0 == specCore_io_next_reg_0)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_1 == specCore_io_next_reg_1)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_2 == specCore_io_next_reg_2)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_3 == specCore_io_next_reg_3)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_4 == specCore_io_next_reg_4)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_5 == specCore_io_next_reg_5)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_6 == specCore_io_next_reg_6)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_7 == specCore_io_next_reg_7)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_8 == specCore_io_next_reg_8)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_9 == specCore_io_next_reg_9)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_10 == specCore_io_next_reg_10)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_11 == specCore_io_next_reg_11)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_12 == specCore_io_next_reg_12)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_13 == specCore_io_next_reg_13)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_14 == specCore_io_next_reg_14)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_15 == specCore_io_next_reg_15)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_16 == specCore_io_next_reg_16)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_17 == specCore_io_next_reg_17)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_18 == specCore_io_next_reg_18)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_19 == specCore_io_next_reg_19)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_20 == specCore_io_next_reg_20)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_21 == specCore_io_next_reg_21)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_22 == specCore_io_next_reg_22)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_23 == specCore_io_next_reg_23)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_24 == specCore_io_next_reg_24)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_25 == specCore_io_next_reg_25)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_26 == specCore_io_next_reg_26)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_27 == specCore_io_next_reg_27)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_28 == specCore_io_next_reg_28)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_29 == specCore_io_next_reg_29)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_30 == specCore_io_next_reg_30)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_5 & ~(io_result_reg_31 == specCore_io_next_reg_31)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:204 assert(regDelay(io.result.reg(i.U)) === regDelay(specCore.io.next.reg(i.U)))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_T_162 & _T_2 & ~_T_163) begin
          $fwrite(32'h80000002,"Assertion failed\n    at Checker.scala:209 assert(\n"); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 209:11]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_38 & ~(io_event_intrNO == 32'h0)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:212 assert(regDelay(io.event.intrNO) === regDelay(specCore.io.event.intrNO))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 212:11]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_38 & ~(io_event_cause == specCore_io_event_cause)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:213 assert(regDelay(io.event.cause) === regDelay(specCore.io.event.cause))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 213:11]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_38 & ~(io_event_exceptionPC == specCore_io_event_exceptionPC)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:214 assert(regDelay(io.event.exceptionPC) === regDelay(specCore.io.event.exceptionPC))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 214:11]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_38 & ~(io_event_exceptionInst == specCore_io_event_exceptionInst)) begin
          $fwrite(32'h80000002,
            "Assertion failed\n    at Checker.scala:215 assert(regDelay(io.event.exceptionInst) === regDelay(specCore.io.event.exceptionInst))\n"
            ); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 215:11]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
  end
  always @(posedge clock) begin
    //
    if (~reset) begin
      assert(io_mem_read_valid == specCore_io_mem_read_valid); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 96:13]
    end
    //
    if (_T_4 & _T_2) begin
      assert(io_mem_read_addr == specCore_io_mem_read_addr); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 98:15]
    end
    //
    if (_T_4 & _T_2) begin
      assert(io_mem_read_memWidth == specCore_io_mem_read_memWidth); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 99:15]
    end
    //
    if (_T_2) begin
      assert(io_mem_write_valid == specCore_io_mem_write_valid); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 101:13]
    end
    //
    if (_T_17 & _T_2) begin
      assert(io_mem_write_addr == specCore_io_mem_write_addr); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 103:15]
    end
    //
    if (_T_17 & _T_2) begin
      assert(io_mem_write_data == specCore_io_mem_write_data); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 104:15]
    end
    //
    if (_T_17 & _T_2) begin
      assert(io_mem_write_memWidth == specCore_io_mem_write_memWidth); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 105:15]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_instCommit_pc == specCore_io_now_pc); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 192:11]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_0 == specCore_io_next_reg_0); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_1 == specCore_io_next_reg_1); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_2 == specCore_io_next_reg_2); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_3 == specCore_io_next_reg_3); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_4 == specCore_io_next_reg_4); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_5 == specCore_io_next_reg_5); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_6 == specCore_io_next_reg_6); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_7 == specCore_io_next_reg_7); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_8 == specCore_io_next_reg_8); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_9 == specCore_io_next_reg_9); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_10 == specCore_io_next_reg_10); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_11 == specCore_io_next_reg_11); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_12 == specCore_io_next_reg_12); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_13 == specCore_io_next_reg_13); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_14 == specCore_io_next_reg_14); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_15 == specCore_io_next_reg_15); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_16 == specCore_io_next_reg_16); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_17 == specCore_io_next_reg_17); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_18 == specCore_io_next_reg_18); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_19 == specCore_io_next_reg_19); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_20 == specCore_io_next_reg_20); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_21 == specCore_io_next_reg_21); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_22 == specCore_io_next_reg_22); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_23 == specCore_io_next_reg_23); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_24 == specCore_io_next_reg_24); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_25 == specCore_io_next_reg_25); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_26 == specCore_io_next_reg_26); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_27 == specCore_io_next_reg_27); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_28 == specCore_io_next_reg_28); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_29 == specCore_io_next_reg_29); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_30 == specCore_io_next_reg_30); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (io_instCommit_valid & _T_2) begin
      assert(io_result_reg_31 == specCore_io_next_reg_31); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 204:13]
    end
    //
    if (_T_162 & _T_2) begin
      assert(_T_163); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 209:11]
    end
    //
    if (_T_162 & _T_2) begin
      assert(io_event_intrNO == 32'h0); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 212:11]
    end
    //
    if (_T_162 & _T_2) begin
      assert(io_event_cause == specCore_io_event_cause); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 213:11]
    end
    //
    if (_T_162 & _T_2) begin
      assert(io_event_exceptionPC == specCore_io_event_exceptionPC); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 214:11]
    end
    //
    if (_T_162 & _T_2) begin
      assert(io_event_exceptionInst == specCore_io_event_exceptionInst); // @[riscv-spec-core/src/main/scala/rvspeccore/checker/Checker.scala 215:11]
    end
  end
endmodule
module Datapath(
  input         clock,
  input         reset,
  output [31:0] io__icache_req_bits_addr, // @[src/main/scala/mini/Datapath.scala 37:14]
  input         io__icache_resp_valid, // @[src/main/scala/mini/Datapath.scala 37:14]
  input  [31:0] io__icache_resp_bits_data, // @[src/main/scala/mini/Datapath.scala 37:14]
  output        io__dcache_req_valid, // @[src/main/scala/mini/Datapath.scala 37:14]
  output [31:0] io__dcache_req_bits_addr, // @[src/main/scala/mini/Datapath.scala 37:14]
  output [31:0] io__dcache_req_bits_data, // @[src/main/scala/mini/Datapath.scala 37:14]
  output [3:0]  io__dcache_req_bits_mask, // @[src/main/scala/mini/Datapath.scala 37:14]
  input         io__dcache_resp_valid, // @[src/main/scala/mini/Datapath.scala 37:14]
  input  [31:0] io__dcache_resp_bits_data, // @[src/main/scala/mini/Datapath.scala 37:14]
  output [31:0] io__ctrl_inst, // @[src/main/scala/mini/Datapath.scala 37:14]
  input  [1:0]  io__ctrl_pc_sel, // @[src/main/scala/mini/Datapath.scala 37:14]
  input         io__ctrl_inst_kill, // @[src/main/scala/mini/Datapath.scala 37:14]
  input         io__ctrl_A_sel, // @[src/main/scala/mini/Datapath.scala 37:14]
  input         io__ctrl_B_sel, // @[src/main/scala/mini/Datapath.scala 37:14]
  input  [2:0]  io__ctrl_imm_sel, // @[src/main/scala/mini/Datapath.scala 37:14]
  input  [3:0]  io__ctrl_alu_op, // @[src/main/scala/mini/Datapath.scala 37:14]
  input  [2:0]  io__ctrl_br_type, // @[src/main/scala/mini/Datapath.scala 37:14]
  input  [1:0]  io__ctrl_st_type, // @[src/main/scala/mini/Datapath.scala 37:14]
  input  [2:0]  io__ctrl_ld_type, // @[src/main/scala/mini/Datapath.scala 37:14]
  input  [1:0]  io__ctrl_wb_sel, // @[src/main/scala/mini/Datapath.scala 37:14]
  input         io__ctrl_wb_en, // @[src/main/scala/mini/Datapath.scala 37:14]
  input  [2:0]  io__ctrl_csr_cmd, // @[src/main/scala/mini/Datapath.scala 37:14]
  input         io__ctrl_illegal, // @[src/main/scala/mini/Datapath.scala 37:14]
  output [4:0]  _T_18_0,
  output [32:0] _T_9_0,
  output [31:0] REG_2_0,
  output        io_expt,
  output [31:0] ew_reg_inst,
  output [34:0] REG_4_0,
  output [4:0]  flywire_rs2_addr_0,
  output [31:0] ew_reg_pc,
  output        _T_8_0,
  output [63:0] instOrder_0,
  output [31:0] io_dcache_resp_bits_data,
  output [4:0]  load_mask_0,
  output [31:0] mem_wdata_0,
  output [4:0]  flywire_rs1_addr_0,
  output        instCommit_0,
  output [31:0] REG_3_0,
  output [32:0] regWrite_0,
  output [3:0]  mem_wmask_0
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
  reg [31:0] _RAND_4;
  reg [31:0] _RAND_5;
  reg [31:0] _RAND_6;
  reg [31:0] _RAND_7;
  reg [31:0] _RAND_8;
  reg [31:0] _RAND_9;
  reg [31:0] _RAND_10;
  reg [31:0] _RAND_11;
  reg [31:0] _RAND_12;
  reg [31:0] _RAND_13;
  reg [63:0] _RAND_14;
  reg [31:0] _RAND_15;
  reg [63:0] _RAND_16;
  reg [31:0] _RAND_17;
  reg [31:0] _RAND_18;
  reg [31:0] _RAND_19;
  reg [31:0] _RAND_20;
  reg [31:0] _RAND_21;
  reg [63:0] _RAND_22;
  reg [31:0] _RAND_23;
  reg [31:0] _RAND_24;
  reg [31:0] _RAND_25;
  reg [31:0] _RAND_26;
  reg [31:0] _RAND_27;
  reg [31:0] _RAND_28;
  reg [31:0] _RAND_29;
  reg [63:0] _RAND_30;
  reg [31:0] _RAND_31;
  reg [31:0] _RAND_32;
  reg [31:0] _RAND_33;
  reg [31:0] _RAND_34;
  reg [31:0] _RAND_35;
`endif // RANDOMIZE_REG_INIT
  wire  csr_clock; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire  csr_reset; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire  csr_io__stall; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire [2:0] csr_io__cmd; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire [31:0] csr_io__in; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire [31:0] csr_io__out; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire [31:0] csr_io__pc; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire [31:0] csr_io__addr; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire [31:0] csr_io__inst; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire  csr_io__illegal; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire [1:0] csr_io__st_type; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire [2:0] csr_io__ld_type; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire  csr_io__pc_check; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire  csr_io__expt; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire [31:0] csr_io__evec; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire [31:0] csr_io__epc; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire  csr_resultEventWire_0_valid; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire [31:0] csr_resultEventWire_0_intrNO; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire [31:0] csr_resultEventWire_0_cause; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire [31:0] csr_resultEventWire_0_exceptionPC; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire [31:0] csr_resultEventWire_0_exceptionInst; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire  csr_io_expt; // @[src/main/scala/mini/Datapath.scala 38:19]
  wire  regFile_clock; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire  regFile_reset; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [4:0] regFile_io_raddr1; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [4:0] regFile_io_raddr2; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_io_rdata1; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_io_rdata2; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire  regFile_io_wen; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [4:0] regFile_io_waddr; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_io_wdata; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_0; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_1; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_2; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_3; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_4; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_5; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_6; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_7; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_8; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_9; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_10; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_11; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_12; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_13; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_14; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_15; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_16; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_17; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_18; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_19; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_20; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_21; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_22; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_23; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_24; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_25; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_26; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_27; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_28; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_29; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_30; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_ArbitraryRegFile_31; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_0; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_1; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_2; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_3; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_4; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_5; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_6; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_7; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_8; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_9; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_10; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_11; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_12; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_13; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_14; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_15; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_16; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_17; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_18; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_19; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_20; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_21; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_22; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_23; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_24; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_25; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_26; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_27; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_28; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_29; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_30; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] regFile_resultRegWire_0_31; // @[src/main/scala/mini/Datapath.scala 39:23]
  wire [31:0] alu_io_A; // @[src/main/scala/mini/Datapath.scala 40:19]
  wire [31:0] alu_io_B; // @[src/main/scala/mini/Datapath.scala 40:19]
  wire [3:0] alu_io_alu_op; // @[src/main/scala/mini/Datapath.scala 40:19]
  wire [31:0] alu_io_out; // @[src/main/scala/mini/Datapath.scala 40:19]
  wire [31:0] alu_io_sum; // @[src/main/scala/mini/Datapath.scala 40:19]
  wire [31:0] immGen_io_inst; // @[src/main/scala/mini/Datapath.scala 41:22]
  wire [2:0] immGen_io_sel; // @[src/main/scala/mini/Datapath.scala 41:22]
  wire [31:0] immGen_io_out; // @[src/main/scala/mini/Datapath.scala 41:22]
  wire [31:0] brCond_io_rs1; // @[src/main/scala/mini/Datapath.scala 42:22]
  wire [31:0] brCond_io_rs2; // @[src/main/scala/mini/Datapath.scala 42:22]
  wire [2:0] brCond_io_br_type; // @[src/main/scala/mini/Datapath.scala 42:22]
  wire  brCond_io_taken; // @[src/main/scala/mini/Datapath.scala 42:22]
  wire  checker__clock; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire  checker__reset; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire  checker__io_instCommit_valid; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_instCommit_inst; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_instCommit_pc; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_0; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_1; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_2; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_3; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_4; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_5; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_6; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_7; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_8; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_9; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_10; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_11; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_12; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_13; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_14; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_15; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_16; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_17; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_18; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_19; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_20; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_21; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_22; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_23; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_24; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_25; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_26; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_27; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_28; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_29; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_30; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_result_reg_31; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire  checker__io_event_valid; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_event_intrNO; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_event_cause; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_event_exceptionPC; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_event_exceptionInst; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire  checker__io_mem_read_valid; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_mem_read_addr; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [5:0] checker__io_mem_read_memWidth; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_mem_read_data; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire  checker__io_mem_write_valid; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_mem_write_addr; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [5:0] checker__io_mem_write_memWidth; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__io_mem_write_data; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_0; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_1; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_2; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_3; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_4; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_5; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_6; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_7; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_8; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_9; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_10; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_11; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_12; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_13; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_14; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_15; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_16; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_17; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_18; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_19; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_20; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_21; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_22; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_23; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_24; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_25; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_26; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_27; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_28; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_29; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_30; // @[src/main/scala/mini/Datapath.scala 311:23]
  wire [31:0] checker__rf_31; // @[src/main/scala/mini/Datapath.scala 311:23]
  reg [31:0] fe_reg_inst; // @[src/main/scala/mini/Datapath.scala 50:23]
  reg [31:0] fe_reg_pc; // @[src/main/scala/mini/Datapath.scala 50:23]
  reg [31:0] ew_reg__inst; // @[src/main/scala/mini/Datapath.scala 59:23]
  reg [31:0] ew_reg__pc; // @[src/main/scala/mini/Datapath.scala 59:23]
  reg [31:0] ew_reg__alu; // @[src/main/scala/mini/Datapath.scala 59:23]
  reg [31:0] ew_reg__csr_in; // @[src/main/scala/mini/Datapath.scala 59:23]
  reg [1:0] st_type; // @[src/main/scala/mini/Datapath.scala 70:20]
  reg [2:0] ld_type; // @[src/main/scala/mini/Datapath.scala 71:20]
  reg [1:0] wb_sel; // @[src/main/scala/mini/Datapath.scala 72:19]
  reg  wb_en; // @[src/main/scala/mini/Datapath.scala 73:18]
  reg [2:0] csr_cmd; // @[src/main/scala/mini/Datapath.scala 74:20]
  reg  illegal; // @[src/main/scala/mini/Datapath.scala 75:20]
  reg  pc_check; // @[src/main/scala/mini/Datapath.scala 76:21]
  reg  started; // @[src/main/scala/mini/Datapath.scala 80:24]
  wire  stall = ~io__icache_resp_valid | ~io__dcache_resp_valid; // @[src/main/scala/mini/Datapath.scala 81:37]
  wire [31:0] _pc_T_1 = 32'h200 - 32'h4; // @[src/main/scala/mini/Datapath.scala 82:50]
  reg [32:0] pc; // @[src/main/scala/mini/Datapath.scala 82:19]
  wire [32:0] _next_pc_T_1 = pc + 33'h4; // @[src/main/scala/mini/Datapath.scala 85:8]
  wire  _next_pc_T_2 = io__ctrl_pc_sel == 2'h3; // @[src/main/scala/mini/Datapath.scala 89:23]
  wire  _next_pc_T_3 = io__ctrl_pc_sel == 2'h1; // @[src/main/scala/mini/Datapath.scala 90:24]
  wire  _next_pc_T_4 = io__ctrl_pc_sel == 2'h1 | brCond_io_taken; // @[src/main/scala/mini/Datapath.scala 90:36]
  wire [31:0] _next_pc_T_5 = {{1'd0}, alu_io_sum[31:1]}; // @[src/main/scala/mini/Datapath.scala 90:73]
  wire [32:0] _next_pc_T_6 = {_next_pc_T_5, 1'h0}; // @[src/main/scala/mini/Datapath.scala 90:80]
  wire  _next_pc_T_7 = io__ctrl_pc_sel == 2'h2; // @[src/main/scala/mini/Datapath.scala 91:23]
  wire [32:0] _next_pc_T_8 = _next_pc_T_7 ? pc : _next_pc_T_1; // @[src/main/scala/chisel3/util/Mux.scala 141:16]
  wire [32:0] _next_pc_T_9 = _next_pc_T_4 ? _next_pc_T_6 : _next_pc_T_8; // @[src/main/scala/chisel3/util/Mux.scala 141:16]
  wire [32:0] _next_pc_T_10 = _next_pc_T_2 ? {{1'd0}, csr_io__epc} : _next_pc_T_9; // @[src/main/scala/chisel3/util/Mux.scala 141:16]
  wire [32:0] _next_pc_T_11 = csr_io__expt ? {{1'd0}, csr_io__evec} : _next_pc_T_10; // @[src/main/scala/chisel3/util/Mux.scala 141:16]
  wire [32:0] next_pc = stall ? pc : _next_pc_T_11; // @[src/main/scala/chisel3/util/Mux.scala 141:16]
  reg  REG; // @[src/main/scala/mini/Datapath.scala 94:58]
  wire  _T_8 = csr_io__expt | REG; // @[src/main/scala/mini/Datapath.scala 94:48]
  reg [32:0] REG_1; // @[src/main/scala/mini/Datapath.scala 95:58]
  wire [32:0] _T_9 = csr_io__expt ? next_pc : REG_1; // @[src/main/scala/mini/Datapath.scala 95:28]
  wire  _inst_T_2 = started | io__ctrl_inst_kill | brCond_io_taken | csr_io__expt; // @[src/main/scala/mini/Datapath.scala 98:57]
  wire  _io_icache_req_valid_T = ~stall; // @[src/main/scala/mini/Datapath.scala 105:26]
  wire [32:0] _GEN_0 = _io_icache_req_valid_T ? pc : {{1'd0}, fe_reg_pc}; // @[src/main/scala/mini/Datapath.scala 109:16 110:15 50:23]
  wire [4:0] rs1_addr = fe_reg_inst[19:15]; // @[src/main/scala/mini/Datapath.scala 120:29]
  wire [4:0] rs2_addr = fe_reg_inst[24:20]; // @[src/main/scala/mini/Datapath.scala 121:29]
  wire [4:0] wb_rd_addr = ew_reg__inst[11:7]; // @[src/main/scala/mini/Datapath.scala 130:31]
  wire  rs1hazard = wb_en & |rs1_addr & rs1_addr == wb_rd_addr; // @[src/main/scala/mini/Datapath.scala 131:41]
  wire  rs2hazard = wb_en & |rs2_addr & rs2_addr == wb_rd_addr; // @[src/main/scala/mini/Datapath.scala 132:41]
  wire  _rs1_T = wb_sel == 2'h0; // @[src/main/scala/mini/Datapath.scala 133:24]
  wire [31:0] rs1 = wb_sel == 2'h0 & rs1hazard ? ew_reg__alu : regFile_io_rdata1; // @[src/main/scala/mini/Datapath.scala 133:16]
  wire [31:0] rs2 = _rs1_T & rs2hazard ? ew_reg__alu : regFile_io_rdata2; // @[src/main/scala/mini/Datapath.scala 134:16]
  wire [31:0] _daddr_T = stall ? ew_reg__alu : alu_io_sum; // @[src/main/scala/mini/Datapath.scala 147:18]
  wire [31:0] _daddr_T_1 = {{2'd0}, _daddr_T[31:2]}; // @[src/main/scala/mini/Datapath.scala 147:50]
  wire [33:0] _GEN_27 = {_daddr_T_1, 2'h0}; // @[src/main/scala/mini/Datapath.scala 147:57]
  wire [34:0] daddr = {{1'd0}, _GEN_27}; // @[src/main/scala/mini/Datapath.scala 147:57]
  wire [4:0] _GEN_28 = {alu_io_sum[1], 4'h0}; // @[src/main/scala/mini/Datapath.scala 148:32]
  wire [7:0] _woffset_T_1 = {{3'd0}, _GEN_28}; // @[src/main/scala/mini/Datapath.scala 148:32]
  wire [3:0] _woffset_T_3 = {alu_io_sum[0], 3'h0}; // @[src/main/scala/mini/Datapath.scala 148:64]
  wire [7:0] _GEN_29 = {{4'd0}, _woffset_T_3}; // @[src/main/scala/mini/Datapath.scala 148:47]
  wire [7:0] woffset = _woffset_T_1 | _GEN_29; // @[src/main/scala/mini/Datapath.scala 148:47]
  wire [286:0] _GEN_13 = {{255'd0}, rs2}; // @[src/main/scala/mini/Datapath.scala 151:34]
  wire [286:0] _io_dcache_req_bits_data_T = _GEN_13 << woffset; // @[src/main/scala/mini/Datapath.scala 151:34]
  wire [1:0] _io_dcache_req_bits_mask_T = stall ? st_type : io__ctrl_st_type; // @[src/main/scala/mini/Datapath.scala 152:43]
  wire [4:0] _io_dcache_req_bits_mask_T_2 = 5'h3 << alu_io_sum[1:0]; // @[src/main/scala/mini/Datapath.scala 153:47]
  wire [3:0] _io_dcache_req_bits_mask_T_4 = 4'h1 << alu_io_sum[1:0]; // @[src/main/scala/mini/Datapath.scala 153:86]
  wire  _io_dcache_req_bits_mask_T_5 = 2'h1 == _io_dcache_req_bits_mask_T; // @[src/main/scala/mini/Datapath.scala 152:88]
  wire [3:0] _io_dcache_req_bits_mask_T_6 = 2'h1 == _io_dcache_req_bits_mask_T ? 4'hf : 4'h0; // @[src/main/scala/mini/Datapath.scala 152:88]
  wire  _io_dcache_req_bits_mask_T_7 = 2'h2 == _io_dcache_req_bits_mask_T; // @[src/main/scala/mini/Datapath.scala 152:88]
  wire [4:0] _io_dcache_req_bits_mask_T_8 = 2'h2 == _io_dcache_req_bits_mask_T ? _io_dcache_req_bits_mask_T_2 : {{1
    'd0}, _io_dcache_req_bits_mask_T_6}; // @[src/main/scala/mini/Datapath.scala 152:88]
  wire  _io_dcache_req_bits_mask_T_9 = 2'h3 == _io_dcache_req_bits_mask_T; // @[src/main/scala/mini/Datapath.scala 152:88]
  wire [4:0] _io_dcache_req_bits_mask_T_10 = 2'h3 == _io_dcache_req_bits_mask_T ? {{1'd0}, _io_dcache_req_bits_mask_T_4}
     : _io_dcache_req_bits_mask_T_8; // @[src/main/scala/mini/Datapath.scala 152:88]
  wire  _T_16 = ~csr_io__expt; // @[src/main/scala/mini/Datapath.scala 164:24]
  wire [4:0] _GEN_30 = {ew_reg__alu[1], 4'h0}; // @[src/main/scala/mini/Datapath.scala 179:32]
  wire [7:0] _loffset_T_1 = {{3'd0}, _GEN_30}; // @[src/main/scala/mini/Datapath.scala 179:32]
  wire [3:0] _loffset_T_3 = {ew_reg__alu[0], 3'h0}; // @[src/main/scala/mini/Datapath.scala 179:64]
  wire [7:0] _GEN_31 = {{4'd0}, _loffset_T_3}; // @[src/main/scala/mini/Datapath.scala 179:47]
  wire [7:0] loffset = _loffset_T_1 | _GEN_31; // @[src/main/scala/mini/Datapath.scala 179:47]
  wire [31:0] lshift = io__dcache_resp_bits_data >> loffset; // @[src/main/scala/mini/Datapath.scala 180:41]
  wire [32:0] _load_T = {1'b0,$signed(io__dcache_resp_bits_data)}; // @[src/main/scala/mini/Datapath.scala 181:58]
  wire [15:0] _load_T_2 = lshift[15:0]; // @[src/main/scala/mini/Datapath.scala 183:30]
  wire [7:0] _load_T_4 = lshift[7:0]; // @[src/main/scala/mini/Datapath.scala 184:29]
  wire [16:0] _load_T_6 = {1'b0,$signed(lshift[15:0])}; // @[src/main/scala/mini/Datapath.scala 185:31]
  wire [8:0] _load_T_8 = {1'b0,$signed(lshift[7:0])}; // @[src/main/scala/mini/Datapath.scala 186:30]
  wire  _load_T_9 = 3'h2 == ld_type; // @[src/main/scala/mini/Datapath.scala 181:63]
  wire [32:0] _load_T_10 = 3'h2 == ld_type ? $signed({{17{_load_T_2[15]}},_load_T_2}) : $signed(_load_T); // @[src/main/scala/mini/Datapath.scala 181:63]
  wire  _load_T_11 = 3'h3 == ld_type; // @[src/main/scala/mini/Datapath.scala 181:63]
  wire [32:0] _load_T_12 = 3'h3 == ld_type ? $signed({{25{_load_T_4[7]}},_load_T_4}) : $signed(_load_T_10); // @[src/main/scala/mini/Datapath.scala 181:63]
  wire  _load_T_13 = 3'h4 == ld_type; // @[src/main/scala/mini/Datapath.scala 181:63]
  wire [32:0] _load_T_14 = 3'h4 == ld_type ? $signed({{16{_load_T_6[16]}},_load_T_6}) : $signed(_load_T_12); // @[src/main/scala/mini/Datapath.scala 181:63]
  wire  _load_T_15 = 3'h5 == ld_type; // @[src/main/scala/mini/Datapath.scala 181:63]
  wire [32:0] load = 3'h5 == ld_type ? $signed({{24{_load_T_8[8]}},_load_T_8}) : $signed(_load_T_14); // @[src/main/scala/mini/Datapath.scala 181:63]
  reg [31:0] lw_addr; // @[src/main/scala/mini/Datapath.scala 193:24]
  wire [4:0] _load_mask_T_1 = 5'h3 << lw_addr[1:0]; // @[src/main/scala/mini/Datapath.scala 197:28]
  wire [3:0] _load_mask_T_3 = 4'h1 << lw_addr[1:0]; // @[src/main/scala/mini/Datapath.scala 198:28]
  wire [3:0] _load_mask_T_9 = 3'h0 == ld_type ? 4'h0 : 4'hf; // @[src/main/scala/mini/Datapath.scala 194:48]
  wire [4:0] _load_mask_T_11 = _load_T_9 ? _load_mask_T_1 : {{1'd0}, _load_mask_T_9}; // @[src/main/scala/mini/Datapath.scala 194:48]
  wire [4:0] _load_mask_T_13 = _load_T_11 ? {{1'd0}, _load_mask_T_3} : _load_mask_T_11; // @[src/main/scala/mini/Datapath.scala 194:48]
  wire [4:0] _load_mask_T_15 = _load_T_13 ? _load_mask_T_1 : _load_mask_T_13; // @[src/main/scala/mini/Datapath.scala 194:48]
  wire [4:0] load_mask = _load_T_15 ? {{1'd0}, _load_mask_T_3} : _load_mask_T_15; // @[src/main/scala/mini/Datapath.scala 194:48]
  reg [3:0] mem_wmask; // @[src/main/scala/mini/Datapath.scala 207:26]
  reg [31:0] mem_wdata; // @[src/main/scala/mini/Datapath.scala 208:26]
  wire [32:0] _regWrite_T = {1'b0,$signed(ew_reg__alu)}; // @[src/main/scala/mini/Datapath.scala 226:34]
  wire [31:0] _regWrite_T_2 = ew_reg__pc + 32'h4; // @[src/main/scala/mini/Datapath.scala 227:48]
  wire [32:0] _regWrite_T_3 = {1'b0,$signed(_regWrite_T_2)}; // @[src/main/scala/mini/Datapath.scala 227:55]
  wire [32:0] _regWrite_T_4 = {1'b0,$signed(csr_io__out)}; // @[src/main/scala/mini/Datapath.scala 227:82]
  wire [32:0] _regWrite_T_6 = 2'h1 == wb_sel ? $signed(load) : $signed(_regWrite_T); // @[src/main/scala/mini/Datapath.scala 226:39]
  wire [32:0] _regWrite_T_8 = 2'h2 == wb_sel ? $signed(_regWrite_T_3) : $signed(_regWrite_T_6); // @[src/main/scala/mini/Datapath.scala 226:39]
  wire [32:0] regWrite = 2'h3 == wb_sel ? $signed(_regWrite_T_4) : $signed(_regWrite_T_8); // @[src/main/scala/mini/Datapath.scala 228:7]
  reg  instCommit_predit_REG; // @[src/main/scala/mini/Datapath.scala 250:40]
  reg  instCommit_predit_REG_1; // @[src/main/scala/mini/Datapath.scala 250:32]
  wire  instCommit_predit = instCommit_predit_REG_1 | stall | reset; // @[src/main/scala/mini/Datapath.scala 250:148]
  reg [63:0] instOrder; // @[src/main/scala/mini/Datapath.scala 273:26]
  wire [63:0] _instOrder_T_1 = instOrder + 64'h1; // @[src/main/scala/mini/Datapath.scala 276:28]
  wire  instCommit = ~instCommit_predit; // @[src/main/scala/mini/Datapath.scala 251:17]
  reg  flywire_rs1_addr_REG; // @[src/main/scala/mini/Datapath.scala 288:34]
  wire  _flywire_rs1_addr_T_2 = io__ctrl_br_type != 3'h0; // @[src/main/scala/mini/Datapath.scala 288:99]
  reg  flywire_rs1_addr_REG_1; // @[src/main/scala/mini/Datapath.scala 288:82]
  reg  flywire_rs2_addr_REG; // @[src/main/scala/mini/Datapath.scala 289:34]
  reg  flywire_rs2_addr_REG_1; // @[src/main/scala/mini/Datapath.scala 289:82]
  reg  flywire_rs2_addr_REG_2; // @[src/main/scala/mini/Datapath.scala 289:130]
  reg [31:0] REG_2; // @[src/main/scala/mini/Datapath.scala 294:32]
  reg [31:0] REG_3; // @[src/main/scala/mini/Datapath.scala 295:32]
  wire [4:0] _T_18 = regFile_io_wen ? wb_rd_addr : 5'h0; // @[src/main/scala/mini/Datapath.scala 296:28]
  reg [34:0] REG_4; // @[src/main/scala/mini/Datapath.scala 300:32]
  wire [5:0] _load_width_T_3 = _load_T_11 ? 6'h8 : 6'h0; // @[src/main/scala/mini/Datapath.scala 320:49]
  wire [5:0] _load_width_T_5 = _load_T_9 ? 6'h10 : _load_width_T_3; // @[src/main/scala/mini/Datapath.scala 320:49]
  wire [5:0] _load_width_T_7 = 3'h1 == ld_type ? 6'h20 : _load_width_T_5; // @[src/main/scala/mini/Datapath.scala 320:49]
  wire [5:0] _load_width_T_9 = _load_T_15 ? 6'h8 : _load_width_T_7; // @[src/main/scala/mini/Datapath.scala 320:49]
  wire [5:0] load_width = _load_T_13 ? 6'h10 : _load_width_T_9; // @[src/main/scala/mini/Datapath.scala 320:49]
  wire [5:0] _store_width_T_2 = _io_dcache_req_bits_mask_T_9 ? 6'h8 : 6'h0; // @[src/main/scala/mini/Datapath.scala 330:79]
  wire [5:0] _store_width_T_4 = _io_dcache_req_bits_mask_T_7 ? 6'h10 : _store_width_T_2; // @[src/main/scala/mini/Datapath.scala 330:79]
  wire [5:0] store_width = _io_dcache_req_bits_mask_T_5 ? 6'h20 : _store_width_T_4; // @[src/main/scala/mini/Datapath.scala 330:79]
  reg [31:0] req_addr; // @[src/main/scala/mini/Datapath.scala 338:25]
  wire [7:0] wmask_seq_0 = mem_wmask[0] ? 8'hff : 8'h0; // @[src/main/scala/mini/Datapath.scala 346:8]
  wire [7:0] wmask_seq_1 = mem_wmask[1] ? 8'hff : 8'h0; // @[src/main/scala/mini/Datapath.scala 346:8]
  wire [7:0] wmask_seq_2 = mem_wmask[2] ? 8'hff : 8'h0; // @[src/main/scala/mini/Datapath.scala 346:8]
  wire [7:0] wmask_seq_3 = mem_wmask[3] ? 8'hff : 8'h0; // @[src/main/scala/mini/Datapath.scala 346:8]
  wire [31:0] wmask_32bits = {wmask_seq_3,wmask_seq_2,wmask_seq_1,wmask_seq_0}; // @[src/main/scala/mini/Datapath.scala 348:25]
  wire  _T_19 = store_width > 6'h0; // @[src/main/scala/mini/Datapath.scala 349:158]
  reg  REG_5; // @[src/main/scala/mini/Datapath.scala 349:145]
  reg [5:0] REG_6; // @[src/main/scala/mini/Datapath.scala 349:251]
  reg  mem_write_valid_REG; // @[src/main/scala/mini/Datapath.scala 350:29]
  wire [3:0] _shiftAmount_T_2 = 2'h1 == req_addr[1:0] ? 4'h8 : 4'h0; // @[src/main/scala/chisel3/util/Mux.scala 77:13]
  wire [4:0] _shiftAmount_T_4 = 2'h2 == req_addr[1:0] ? 5'h10 : {{1'd0}, _shiftAmount_T_2}; // @[src/main/scala/chisel3/util/Mux.scala 77:13]
  wire [4:0] shiftAmount = 2'h3 == req_addr[1:0] ? 5'h18 : _shiftAmount_T_4; // @[src/main/scala/chisel3/util/Mux.scala 77:13]
  wire [31:0] _mem_write_data_T = mem_wdata & wmask_32bits; // @[src/main/scala/mini/Datapath.scala 357:33]
  reg [5:0] mem_write_memWidth_REG; // @[src/main/scala/mini/Datapath.scala 358:32]
  wire  mem_1_read_valid = load_width > 6'h0 & _T_16; // @[src/main/scala/mini/Datapath.scala 339:40]
  wire [31:0] mem_1_read_addr = req_addr; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 45:19 src/main/scala/mini/Datapath.scala 340:18]
  wire [5:0] mem_1_read_memWidth = load_width; // @[src/main/scala/mini/Datapath.scala 320:49]
  wire [31:0] mem_1_read_data = io__dcache_resp_bits_data; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 45:19 src/main/scala/mini/Datapath.scala 341:18]
  wire  mem_1_write_valid = mem_write_valid_REG & _T_16; // @[src/main/scala/mini/Datapath.scala 350:58]
  wire [31:0] mem_1_write_addr = req_addr; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 45:19 src/main/scala/mini/Datapath.scala 351:19]
  wire [5:0] mem_1_write_memWidth = mem_write_memWidth_REG; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 45:19 src/main/scala/mini/Datapath.scala 358:22]
  wire [31:0] mem_1_write_data = _mem_write_data_T >> shiftAmount; // @[src/main/scala/mini/Datapath.scala 357:49]
  wire [31:0] rf__0 = 32'h0; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ArbitraryGenerater.scala 16:18 18:11]
  wire [31:0] rf__1 = 32'h0;
  wire [31:0] rf__2 = 32'h0;
  wire [31:0] rf__3 = 32'h0;
  wire [31:0] rf__4 = 32'h0;
  wire [31:0] rf__5 = 32'h0;
  wire [31:0] rf__6 = 32'h0;
  wire [31:0] rf__7 = 32'h0;
  wire [31:0] rf__8 = 32'h0;
  wire [31:0] rf__9 = 32'h0;
  wire [31:0] rf__10 = 32'h0;
  wire [31:0] rf__11 = 32'h0;
  wire [31:0] rf__12 = 32'h0;
  wire [31:0] rf__13 = 32'h0;
  wire [31:0] rf__14 = 32'h0;
  wire [31:0] rf__15 = 32'h0;
  wire [31:0] rf__16 = 32'h0;
  wire [31:0] rf__17 = 32'h0;
  wire [31:0] rf__18 = 32'h0;
  wire [31:0] rf__19 = 32'h0;
  wire [31:0] rf__20 = 32'h0;
  wire [31:0] rf__21 = 32'h0;
  wire [31:0] rf__22 = 32'h0;
  wire [31:0] rf__23 = 32'h0;
  wire [31:0] rf__24 = 32'h0;
  wire [31:0] rf__25 = 32'h0;
  wire [31:0] rf__26 = 32'h0;
  wire [31:0] rf__27 = 32'h0;
  wire [31:0] rf__28 = 32'h0;
  wire [31:0] rf__29 = 32'h0;
  wire [31:0] rf__30 = 32'h0;
  wire [31:0] rf__31 = 32'h0;
  wire [4:0] flywire_rs1_addr = flywire_rs1_addr_REG | flywire_rs1_addr_REG_1 ? ew_reg__inst[19:15] : 5'h0; // @[src/main/scala/mini/Datapath.scala 288:26]
  wire [4:0] flywire_rs2_addr = flywire_rs2_addr_REG | flywire_rs2_addr_REG_1 | flywire_rs2_addr_REG_2 ? ew_reg__inst[24
    :20] : 5'h0; // @[src/main/scala/mini/Datapath.scala 289:26]
  wire [32:0] _GEN_32 = reset ? 33'h0 : _GEN_0; // @[src/main/scala/mini/Datapath.scala 50:{23,23}]
  CSR csr ( // @[src/main/scala/mini/Datapath.scala 38:19]
    .clock(csr_clock),
    .reset(csr_reset),
    .io__stall(csr_io__stall),
    .io__cmd(csr_io__cmd),
    .io__in(csr_io__in),
    .io__out(csr_io__out),
    .io__pc(csr_io__pc),
    .io__addr(csr_io__addr),
    .io__inst(csr_io__inst),
    .io__illegal(csr_io__illegal),
    .io__st_type(csr_io__st_type),
    .io__ld_type(csr_io__ld_type),
    .io__pc_check(csr_io__pc_check),
    .io__expt(csr_io__expt),
    .io__evec(csr_io__evec),
    .io__epc(csr_io__epc),
    .resultEventWire_0_valid(csr_resultEventWire_0_valid),
    .resultEventWire_0_intrNO(csr_resultEventWire_0_intrNO),
    .resultEventWire_0_cause(csr_resultEventWire_0_cause),
    .resultEventWire_0_exceptionPC(csr_resultEventWire_0_exceptionPC),
    .resultEventWire_0_exceptionInst(csr_resultEventWire_0_exceptionInst),
    .io_expt(csr_io_expt)
  );
  RegFile regFile ( // @[src/main/scala/mini/Datapath.scala 39:23]
    .clock(regFile_clock),
    .reset(regFile_reset),
    .io_raddr1(regFile_io_raddr1),
    .io_raddr2(regFile_io_raddr2),
    .io_rdata1(regFile_io_rdata1),
    .io_rdata2(regFile_io_rdata2),
    .io_wen(regFile_io_wen),
    .io_waddr(regFile_io_waddr),
    .io_wdata(regFile_io_wdata),
    .ArbitraryRegFile_0(regFile_ArbitraryRegFile_0),
    .ArbitraryRegFile_1(regFile_ArbitraryRegFile_1),
    .ArbitraryRegFile_2(regFile_ArbitraryRegFile_2),
    .ArbitraryRegFile_3(regFile_ArbitraryRegFile_3),
    .ArbitraryRegFile_4(regFile_ArbitraryRegFile_4),
    .ArbitraryRegFile_5(regFile_ArbitraryRegFile_5),
    .ArbitraryRegFile_6(regFile_ArbitraryRegFile_6),
    .ArbitraryRegFile_7(regFile_ArbitraryRegFile_7),
    .ArbitraryRegFile_8(regFile_ArbitraryRegFile_8),
    .ArbitraryRegFile_9(regFile_ArbitraryRegFile_9),
    .ArbitraryRegFile_10(regFile_ArbitraryRegFile_10),
    .ArbitraryRegFile_11(regFile_ArbitraryRegFile_11),
    .ArbitraryRegFile_12(regFile_ArbitraryRegFile_12),
    .ArbitraryRegFile_13(regFile_ArbitraryRegFile_13),
    .ArbitraryRegFile_14(regFile_ArbitraryRegFile_14),
    .ArbitraryRegFile_15(regFile_ArbitraryRegFile_15),
    .ArbitraryRegFile_16(regFile_ArbitraryRegFile_16),
    .ArbitraryRegFile_17(regFile_ArbitraryRegFile_17),
    .ArbitraryRegFile_18(regFile_ArbitraryRegFile_18),
    .ArbitraryRegFile_19(regFile_ArbitraryRegFile_19),
    .ArbitraryRegFile_20(regFile_ArbitraryRegFile_20),
    .ArbitraryRegFile_21(regFile_ArbitraryRegFile_21),
    .ArbitraryRegFile_22(regFile_ArbitraryRegFile_22),
    .ArbitraryRegFile_23(regFile_ArbitraryRegFile_23),
    .ArbitraryRegFile_24(regFile_ArbitraryRegFile_24),
    .ArbitraryRegFile_25(regFile_ArbitraryRegFile_25),
    .ArbitraryRegFile_26(regFile_ArbitraryRegFile_26),
    .ArbitraryRegFile_27(regFile_ArbitraryRegFile_27),
    .ArbitraryRegFile_28(regFile_ArbitraryRegFile_28),
    .ArbitraryRegFile_29(regFile_ArbitraryRegFile_29),
    .ArbitraryRegFile_30(regFile_ArbitraryRegFile_30),
    .ArbitraryRegFile_31(regFile_ArbitraryRegFile_31),
    .resultRegWire_0_0(regFile_resultRegWire_0_0),
    .resultRegWire_0_1(regFile_resultRegWire_0_1),
    .resultRegWire_0_2(regFile_resultRegWire_0_2),
    .resultRegWire_0_3(regFile_resultRegWire_0_3),
    .resultRegWire_0_4(regFile_resultRegWire_0_4),
    .resultRegWire_0_5(regFile_resultRegWire_0_5),
    .resultRegWire_0_6(regFile_resultRegWire_0_6),
    .resultRegWire_0_7(regFile_resultRegWire_0_7),
    .resultRegWire_0_8(regFile_resultRegWire_0_8),
    .resultRegWire_0_9(regFile_resultRegWire_0_9),
    .resultRegWire_0_10(regFile_resultRegWire_0_10),
    .resultRegWire_0_11(regFile_resultRegWire_0_11),
    .resultRegWire_0_12(regFile_resultRegWire_0_12),
    .resultRegWire_0_13(regFile_resultRegWire_0_13),
    .resultRegWire_0_14(regFile_resultRegWire_0_14),
    .resultRegWire_0_15(regFile_resultRegWire_0_15),
    .resultRegWire_0_16(regFile_resultRegWire_0_16),
    .resultRegWire_0_17(regFile_resultRegWire_0_17),
    .resultRegWire_0_18(regFile_resultRegWire_0_18),
    .resultRegWire_0_19(regFile_resultRegWire_0_19),
    .resultRegWire_0_20(regFile_resultRegWire_0_20),
    .resultRegWire_0_21(regFile_resultRegWire_0_21),
    .resultRegWire_0_22(regFile_resultRegWire_0_22),
    .resultRegWire_0_23(regFile_resultRegWire_0_23),
    .resultRegWire_0_24(regFile_resultRegWire_0_24),
    .resultRegWire_0_25(regFile_resultRegWire_0_25),
    .resultRegWire_0_26(regFile_resultRegWire_0_26),
    .resultRegWire_0_27(regFile_resultRegWire_0_27),
    .resultRegWire_0_28(regFile_resultRegWire_0_28),
    .resultRegWire_0_29(regFile_resultRegWire_0_29),
    .resultRegWire_0_30(regFile_resultRegWire_0_30),
    .resultRegWire_0_31(regFile_resultRegWire_0_31)
  );
  AluArea alu ( // @[src/main/scala/mini/Datapath.scala 40:19]
    .io_A(alu_io_A),
    .io_B(alu_io_B),
    .io_alu_op(alu_io_alu_op),
    .io_out(alu_io_out),
    .io_sum(alu_io_sum)
  );
  ImmGenWire immGen ( // @[src/main/scala/mini/Datapath.scala 41:22]
    .io_inst(immGen_io_inst),
    .io_sel(immGen_io_sel),
    .io_out(immGen_io_out)
  );
  BrCondArea brCond ( // @[src/main/scala/mini/Datapath.scala 42:22]
    .io_rs1(brCond_io_rs1),
    .io_rs2(brCond_io_rs2),
    .io_br_type(brCond_io_br_type),
    .io_taken(brCond_io_taken)
  );
  CheckerWithResult checker_ ( // @[src/main/scala/mini/Datapath.scala 311:23]
    .clock(checker__clock),
    .reset(checker__reset),
    .io_instCommit_valid(checker__io_instCommit_valid),
    .io_instCommit_inst(checker__io_instCommit_inst),
    .io_instCommit_pc(checker__io_instCommit_pc),
    .io_result_reg_0(checker__io_result_reg_0),
    .io_result_reg_1(checker__io_result_reg_1),
    .io_result_reg_2(checker__io_result_reg_2),
    .io_result_reg_3(checker__io_result_reg_3),
    .io_result_reg_4(checker__io_result_reg_4),
    .io_result_reg_5(checker__io_result_reg_5),
    .io_result_reg_6(checker__io_result_reg_6),
    .io_result_reg_7(checker__io_result_reg_7),
    .io_result_reg_8(checker__io_result_reg_8),
    .io_result_reg_9(checker__io_result_reg_9),
    .io_result_reg_10(checker__io_result_reg_10),
    .io_result_reg_11(checker__io_result_reg_11),
    .io_result_reg_12(checker__io_result_reg_12),
    .io_result_reg_13(checker__io_result_reg_13),
    .io_result_reg_14(checker__io_result_reg_14),
    .io_result_reg_15(checker__io_result_reg_15),
    .io_result_reg_16(checker__io_result_reg_16),
    .io_result_reg_17(checker__io_result_reg_17),
    .io_result_reg_18(checker__io_result_reg_18),
    .io_result_reg_19(checker__io_result_reg_19),
    .io_result_reg_20(checker__io_result_reg_20),
    .io_result_reg_21(checker__io_result_reg_21),
    .io_result_reg_22(checker__io_result_reg_22),
    .io_result_reg_23(checker__io_result_reg_23),
    .io_result_reg_24(checker__io_result_reg_24),
    .io_result_reg_25(checker__io_result_reg_25),
    .io_result_reg_26(checker__io_result_reg_26),
    .io_result_reg_27(checker__io_result_reg_27),
    .io_result_reg_28(checker__io_result_reg_28),
    .io_result_reg_29(checker__io_result_reg_29),
    .io_result_reg_30(checker__io_result_reg_30),
    .io_result_reg_31(checker__io_result_reg_31),
    .io_event_valid(checker__io_event_valid),
    .io_event_intrNO(checker__io_event_intrNO),
    .io_event_cause(checker__io_event_cause),
    .io_event_exceptionPC(checker__io_event_exceptionPC),
    .io_event_exceptionInst(checker__io_event_exceptionInst),
    .io_mem_read_valid(checker__io_mem_read_valid),
    .io_mem_read_addr(checker__io_mem_read_addr),
    .io_mem_read_memWidth(checker__io_mem_read_memWidth),
    .io_mem_read_data(checker__io_mem_read_data),
    .io_mem_write_valid(checker__io_mem_write_valid),
    .io_mem_write_addr(checker__io_mem_write_addr),
    .io_mem_write_memWidth(checker__io_mem_write_memWidth),
    .io_mem_write_data(checker__io_mem_write_data),
    .rf_0(checker__rf_0),
    .rf_1(checker__rf_1),
    .rf_2(checker__rf_2),
    .rf_3(checker__rf_3),
    .rf_4(checker__rf_4),
    .rf_5(checker__rf_5),
    .rf_6(checker__rf_6),
    .rf_7(checker__rf_7),
    .rf_8(checker__rf_8),
    .rf_9(checker__rf_9),
    .rf_10(checker__rf_10),
    .rf_11(checker__rf_11),
    .rf_12(checker__rf_12),
    .rf_13(checker__rf_13),
    .rf_14(checker__rf_14),
    .rf_15(checker__rf_15),
    .rf_16(checker__rf_16),
    .rf_17(checker__rf_17),
    .rf_18(checker__rf_18),
    .rf_19(checker__rf_19),
    .rf_20(checker__rf_20),
    .rf_21(checker__rf_21),
    .rf_22(checker__rf_22),
    .rf_23(checker__rf_23),
    .rf_24(checker__rf_24),
    .rf_25(checker__rf_25),
    .rf_26(checker__rf_26),
    .rf_27(checker__rf_27),
    .rf_28(checker__rf_28),
    .rf_29(checker__rf_29),
    .rf_30(checker__rf_30),
    .rf_31(checker__rf_31)
  );
  assign io__icache_req_bits_addr = next_pc[31:0]; // @[src/main/scala/mini/Datapath.scala 102:27]
  assign io__dcache_req_valid = _io_icache_req_valid_T & (|io__ctrl_st_type | |io__ctrl_ld_type); // @[src/main/scala/mini/Datapath.scala 149:33]
  assign io__dcache_req_bits_addr = daddr[31:0]; // @[src/main/scala/mini/Datapath.scala 150:27]
  assign io__dcache_req_bits_data = _io_dcache_req_bits_data_T[31:0]; // @[src/main/scala/mini/Datapath.scala 151:27]
  assign io__dcache_req_bits_mask = _io_dcache_req_bits_mask_T_10[3:0]; // @[src/main/scala/mini/Datapath.scala 152:27]
  assign io__ctrl_inst = fe_reg_inst; // @[src/main/scala/mini/Datapath.scala 116:16]
  assign _T_18_0 = _T_18;
  assign _T_9_0 = _T_9;
  assign REG_2_0 = REG_2;
  assign io_expt = csr_io_expt;
  assign ew_reg_inst = ew_reg__inst;
  assign REG_4_0 = REG_4;
  assign flywire_rs2_addr_0 = flywire_rs2_addr;
  assign ew_reg_pc = ew_reg__pc;
  assign _T_8_0 = _T_8;
  assign instOrder_0 = instOrder;
  assign io_dcache_resp_bits_data = io__dcache_resp_bits_data;
  assign load_mask_0 = load_mask;
  assign mem_wdata_0 = mem_wdata;
  assign flywire_rs1_addr_0 = flywire_rs1_addr;
  assign instCommit_0 = instCommit;
  assign REG_3_0 = REG_3;
  assign regWrite_0 = regWrite;
  assign mem_wmask_0 = mem_wmask;
  assign csr_clock = clock;
  assign csr_reset = reset;
  assign csr_io__stall = ~io__icache_resp_valid | ~io__dcache_resp_valid; // @[src/main/scala/mini/Datapath.scala 81:37]
  assign csr_io__cmd = csr_cmd; // @[src/main/scala/mini/Datapath.scala 214:14]
  assign csr_io__in = ew_reg__csr_in; // @[src/main/scala/mini/Datapath.scala 213:13]
  assign csr_io__pc = ew_reg__pc; // @[src/main/scala/mini/Datapath.scala 216:13]
  assign csr_io__addr = ew_reg__alu; // @[src/main/scala/mini/Datapath.scala 217:15]
  assign csr_io__inst = ew_reg__inst; // @[src/main/scala/mini/Datapath.scala 215:15]
  assign csr_io__illegal = illegal; // @[src/main/scala/mini/Datapath.scala 218:18]
  assign csr_io__st_type = st_type; // @[src/main/scala/mini/Datapath.scala 221:18]
  assign csr_io__ld_type = ld_type; // @[src/main/scala/mini/Datapath.scala 220:18]
  assign csr_io__pc_check = pc_check; // @[src/main/scala/mini/Datapath.scala 219:19]
  assign regFile_clock = clock;
  assign regFile_reset = reset;
  assign regFile_io_raddr1 = fe_reg_inst[19:15]; // @[src/main/scala/mini/Datapath.scala 120:29]
  assign regFile_io_raddr2 = fe_reg_inst[24:20]; // @[src/main/scala/mini/Datapath.scala 121:29]
  assign regFile_io_wen = wb_en & _io_icache_req_valid_T & _T_16; // @[src/main/scala/mini/Datapath.scala 230:37]
  assign regFile_io_waddr = ew_reg__inst[11:7]; // @[src/main/scala/mini/Datapath.scala 130:31]
  assign regFile_io_wdata = regWrite[31:0]; // @[src/main/scala/mini/Datapath.scala 232:20]
  assign regFile_ArbitraryRegFile_0 = rf__0;
  assign regFile_ArbitraryRegFile_1 = rf__0;
  assign regFile_ArbitraryRegFile_2 = rf__0;
  assign regFile_ArbitraryRegFile_3 = rf__0;
  assign regFile_ArbitraryRegFile_4 = rf__0;
  assign regFile_ArbitraryRegFile_5 = rf__0;
  assign regFile_ArbitraryRegFile_6 = rf__0;
  assign regFile_ArbitraryRegFile_7 = rf__0;
  assign regFile_ArbitraryRegFile_8 = rf__0;
  assign regFile_ArbitraryRegFile_9 = rf__0;
  assign regFile_ArbitraryRegFile_10 = rf__0;
  assign regFile_ArbitraryRegFile_11 = rf__0;
  assign regFile_ArbitraryRegFile_12 = rf__0;
  assign regFile_ArbitraryRegFile_13 = rf__0;
  assign regFile_ArbitraryRegFile_14 = rf__0;
  assign regFile_ArbitraryRegFile_15 = rf__0;
  assign regFile_ArbitraryRegFile_16 = rf__0;
  assign regFile_ArbitraryRegFile_17 = rf__0;
  assign regFile_ArbitraryRegFile_18 = rf__0;
  assign regFile_ArbitraryRegFile_19 = rf__0;
  assign regFile_ArbitraryRegFile_20 = rf__0;
  assign regFile_ArbitraryRegFile_21 = rf__0;
  assign regFile_ArbitraryRegFile_22 = rf__0;
  assign regFile_ArbitraryRegFile_23 = rf__0;
  assign regFile_ArbitraryRegFile_24 = rf__0;
  assign regFile_ArbitraryRegFile_25 = rf__0;
  assign regFile_ArbitraryRegFile_26 = rf__0;
  assign regFile_ArbitraryRegFile_27 = rf__0;
  assign regFile_ArbitraryRegFile_28 = rf__0;
  assign regFile_ArbitraryRegFile_29 = rf__0;
  assign regFile_ArbitraryRegFile_30 = rf__0;
  assign regFile_ArbitraryRegFile_31 = rf__0;
  assign alu_io_A = io__ctrl_A_sel ? rs1 : fe_reg_pc; // @[src/main/scala/mini/Datapath.scala 137:18]
  assign alu_io_B = io__ctrl_B_sel ? rs2 : immGen_io_out; // @[src/main/scala/mini/Datapath.scala 138:18]
  assign alu_io_alu_op = io__ctrl_alu_op; // @[src/main/scala/mini/Datapath.scala 139:17]
  assign immGen_io_inst = fe_reg_inst; // @[src/main/scala/mini/Datapath.scala 126:18]
  assign immGen_io_sel = io__ctrl_imm_sel; // @[src/main/scala/mini/Datapath.scala 127:17]
  assign brCond_io_rs1 = wb_sel == 2'h0 & rs1hazard ? ew_reg__alu : regFile_io_rdata1; // @[src/main/scala/mini/Datapath.scala 133:16]
  assign brCond_io_rs2 = _rs1_T & rs2hazard ? ew_reg__alu : regFile_io_rdata2; // @[src/main/scala/mini/Datapath.scala 134:16]
  assign brCond_io_br_type = io__ctrl_br_type; // @[src/main/scala/mini/Datapath.scala 144:21]
  assign checker__clock = clock;
  assign checker__reset = reset;
  assign checker__io_instCommit_valid = instCommit; // @[src/main/scala/mini/Datapath.scala 312:31]
  assign checker__io_instCommit_inst = ew_reg__inst; // @[src/main/scala/mini/Datapath.scala 313:31]
  assign checker__io_instCommit_pc = ew_reg__pc; // @[src/main/scala/mini/Datapath.scala 314:31]
  assign checker__io_result_reg_0 = regFile_resultRegWire_0_0; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_1 = regFile_resultRegWire_0_1; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_2 = regFile_resultRegWire_0_2; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_3 = regFile_resultRegWire_0_3; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_4 = regFile_resultRegWire_0_4; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_5 = regFile_resultRegWire_0_5; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_6 = regFile_resultRegWire_0_6; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_7 = regFile_resultRegWire_0_7; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_8 = regFile_resultRegWire_0_8; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_9 = regFile_resultRegWire_0_9; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_10 = regFile_resultRegWire_0_10; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_11 = regFile_resultRegWire_0_11; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_12 = regFile_resultRegWire_0_12; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_13 = regFile_resultRegWire_0_13; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_14 = regFile_resultRegWire_0_14; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_15 = regFile_resultRegWire_0_15; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_16 = regFile_resultRegWire_0_16; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_17 = regFile_resultRegWire_0_17; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_18 = regFile_resultRegWire_0_18; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_19 = regFile_resultRegWire_0_19; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_20 = regFile_resultRegWire_0_20; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_21 = regFile_resultRegWire_0_21; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_22 = regFile_resultRegWire_0_22; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_23 = regFile_resultRegWire_0_23; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_24 = regFile_resultRegWire_0_24; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_25 = regFile_resultRegWire_0_25; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_26 = regFile_resultRegWire_0_26; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_27 = regFile_resultRegWire_0_27; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_28 = regFile_resultRegWire_0_28; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_29 = regFile_resultRegWire_0_29; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_30 = regFile_resultRegWire_0_30; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_result_reg_31 = regFile_resultRegWire_0_31; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 88:22]
  assign checker__io_event_valid = csr_resultEventWire_0_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 124:21]
  assign checker__io_event_intrNO = csr_resultEventWire_0_intrNO; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 124:21]
  assign checker__io_event_cause = csr_resultEventWire_0_cause; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 124:21]
  assign checker__io_event_exceptionPC = csr_resultEventWire_0_exceptionPC; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 124:21]
  assign checker__io_event_exceptionInst = csr_resultEventWire_0_exceptionInst; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 124:21]
  assign checker__io_mem_read_valid = mem_1_read_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 98:21]
  assign checker__io_mem_read_addr = mem_1_read_addr; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 98:21]
  assign checker__io_mem_read_memWidth = mem_1_read_memWidth; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 98:21]
  assign checker__io_mem_read_data = mem_1_read_data; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 98:21]
  assign checker__io_mem_write_valid = mem_1_write_valid; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 98:21]
  assign checker__io_mem_write_addr = mem_1_read_addr; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 98:21]
  assign checker__io_mem_write_memWidth = mem_1_write_memWidth; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 98:21]
  assign checker__io_mem_write_data = mem_1_write_data; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/ConnectHelper.scala 98:21]
  assign checker__rf_0 = rf__0;
  assign checker__rf_1 = rf__0;
  assign checker__rf_2 = rf__0;
  assign checker__rf_3 = rf__0;
  assign checker__rf_4 = rf__0;
  assign checker__rf_5 = rf__0;
  assign checker__rf_6 = rf__0;
  assign checker__rf_7 = rf__0;
  assign checker__rf_8 = rf__0;
  assign checker__rf_9 = rf__0;
  assign checker__rf_10 = rf__0;
  assign checker__rf_11 = rf__0;
  assign checker__rf_12 = rf__0;
  assign checker__rf_13 = rf__0;
  assign checker__rf_14 = rf__0;
  assign checker__rf_15 = rf__0;
  assign checker__rf_16 = rf__0;
  assign checker__rf_17 = rf__0;
  assign checker__rf_18 = rf__0;
  assign checker__rf_19 = rf__0;
  assign checker__rf_20 = rf__0;
  assign checker__rf_21 = rf__0;
  assign checker__rf_22 = rf__0;
  assign checker__rf_23 = rf__0;
  assign checker__rf_24 = rf__0;
  assign checker__rf_25 = rf__0;
  assign checker__rf_26 = rf__0;
  assign checker__rf_27 = rf__0;
  assign checker__rf_28 = rf__0;
  assign checker__rf_29 = rf__0;
  assign checker__rf_30 = rf__0;
  assign checker__rf_31 = rf__0;
  always @(posedge clock) begin
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 50:23]
      fe_reg_inst <= 32'h13; // @[src/main/scala/mini/Datapath.scala 50:23]
    end else if (_io_icache_req_valid_T) begin // @[src/main/scala/mini/Datapath.scala 109:16]
      if (started | io__ctrl_inst_kill | brCond_io_taken | csr_io__expt) begin // @[src/main/scala/mini/Datapath.scala 98:8]
        fe_reg_inst <= 32'h13;
      end else begin
        fe_reg_inst <= io__icache_resp_bits_data;
      end
    end
    fe_reg_pc <= _GEN_32[31:0]; // @[src/main/scala/mini/Datapath.scala 50:{23,23}]
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 59:23]
      ew_reg__inst <= 32'h13; // @[src/main/scala/mini/Datapath.scala 59:23]
    end else if (!(reset | _io_icache_req_valid_T & csr_io__expt)) begin // @[src/main/scala/mini/Datapath.scala 157:47]
      if (_io_icache_req_valid_T & ~csr_io__expt) begin // @[src/main/scala/mini/Datapath.scala 164:38]
        ew_reg__inst <= fe_reg_inst; // @[src/main/scala/mini/Datapath.scala 166:17]
      end
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 59:23]
      ew_reg__pc <= 32'h0; // @[src/main/scala/mini/Datapath.scala 59:23]
    end else if (!(reset | _io_icache_req_valid_T & csr_io__expt)) begin // @[src/main/scala/mini/Datapath.scala 157:47]
      if (_io_icache_req_valid_T & ~csr_io__expt) begin // @[src/main/scala/mini/Datapath.scala 164:38]
        ew_reg__pc <= fe_reg_pc; // @[src/main/scala/mini/Datapath.scala 165:15]
      end
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 59:23]
      ew_reg__alu <= 32'h0; // @[src/main/scala/mini/Datapath.scala 59:23]
    end else if (!(reset | _io_icache_req_valid_T & csr_io__expt)) begin // @[src/main/scala/mini/Datapath.scala 157:47]
      if (_io_icache_req_valid_T & ~csr_io__expt) begin // @[src/main/scala/mini/Datapath.scala 164:38]
        ew_reg__alu <= alu_io_out; // @[src/main/scala/mini/Datapath.scala 167:16]
      end
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 59:23]
      ew_reg__csr_in <= 32'h0; // @[src/main/scala/mini/Datapath.scala 59:23]
    end else if (!(reset | _io_icache_req_valid_T & csr_io__expt)) begin // @[src/main/scala/mini/Datapath.scala 157:47]
      if (_io_icache_req_valid_T & ~csr_io__expt) begin // @[src/main/scala/mini/Datapath.scala 164:38]
        if (io__ctrl_imm_sel == 3'h6) begin // @[src/main/scala/mini/Datapath.scala 168:25]
          ew_reg__csr_in <= immGen_io_out;
        end else begin
          ew_reg__csr_in <= rs1;
        end
      end
    end
    if (reset | _io_icache_req_valid_T & csr_io__expt) begin // @[src/main/scala/mini/Datapath.scala 157:47]
      st_type <= 2'h0; // @[src/main/scala/mini/Datapath.scala 158:13]
    end else if (_io_icache_req_valid_T & ~csr_io__expt) begin // @[src/main/scala/mini/Datapath.scala 164:38]
      st_type <= io__ctrl_st_type; // @[src/main/scala/mini/Datapath.scala 169:13]
    end
    if (reset | _io_icache_req_valid_T & csr_io__expt) begin // @[src/main/scala/mini/Datapath.scala 157:47]
      ld_type <= 3'h0; // @[src/main/scala/mini/Datapath.scala 159:13]
    end else if (_io_icache_req_valid_T & ~csr_io__expt) begin // @[src/main/scala/mini/Datapath.scala 164:38]
      ld_type <= io__ctrl_ld_type; // @[src/main/scala/mini/Datapath.scala 170:13]
    end
    if (!(reset | _io_icache_req_valid_T & csr_io__expt)) begin // @[src/main/scala/mini/Datapath.scala 157:47]
      if (_io_icache_req_valid_T & ~csr_io__expt) begin // @[src/main/scala/mini/Datapath.scala 164:38]
        wb_sel <= io__ctrl_wb_sel; // @[src/main/scala/mini/Datapath.scala 171:12]
      end
    end
    if (reset | _io_icache_req_valid_T & csr_io__expt) begin // @[src/main/scala/mini/Datapath.scala 157:47]
      wb_en <= 1'h0; // @[src/main/scala/mini/Datapath.scala 160:11]
    end else if (_io_icache_req_valid_T & ~csr_io__expt) begin // @[src/main/scala/mini/Datapath.scala 164:38]
      wb_en <= io__ctrl_wb_en; // @[src/main/scala/mini/Datapath.scala 172:11]
    end
    if (reset | _io_icache_req_valid_T & csr_io__expt) begin // @[src/main/scala/mini/Datapath.scala 157:47]
      csr_cmd <= 3'h0; // @[src/main/scala/mini/Datapath.scala 161:13]
    end else if (_io_icache_req_valid_T & ~csr_io__expt) begin // @[src/main/scala/mini/Datapath.scala 164:38]
      csr_cmd <= io__ctrl_csr_cmd; // @[src/main/scala/mini/Datapath.scala 173:13]
    end
    if (reset | _io_icache_req_valid_T & csr_io__expt) begin // @[src/main/scala/mini/Datapath.scala 157:47]
      illegal <= 1'h0; // @[src/main/scala/mini/Datapath.scala 162:13]
    end else if (_io_icache_req_valid_T & ~csr_io__expt) begin // @[src/main/scala/mini/Datapath.scala 164:38]
      illegal <= io__ctrl_illegal; // @[src/main/scala/mini/Datapath.scala 174:13]
    end
    if (reset | _io_icache_req_valid_T & csr_io__expt) begin // @[src/main/scala/mini/Datapath.scala 157:47]
      pc_check <= 1'h0; // @[src/main/scala/mini/Datapath.scala 163:14]
    end else if (_io_icache_req_valid_T & ~csr_io__expt) begin // @[src/main/scala/mini/Datapath.scala 164:38]
      pc_check <= _next_pc_T_4; // @[src/main/scala/mini/Datapath.scala 175:14]
    end
    started <= reset; // @[src/main/scala/mini/Datapath.scala 80:31]
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 82:19]
      pc <= {{1'd0}, _pc_T_1}; // @[src/main/scala/mini/Datapath.scala 82:19]
    end else if (!(stall)) begin // @[src/main/scala/chisel3/util/Mux.scala 141:16]
      if (csr_io__expt) begin // @[src/main/scala/chisel3/util/Mux.scala 141:16]
        pc <= {{1'd0}, csr_io__evec};
      end else if (_next_pc_T_2) begin // @[src/main/scala/chisel3/util/Mux.scala 141:16]
        pc <= {{1'd0}, csr_io__epc};
      end else begin
        pc <= _next_pc_T_9;
      end
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 94:58]
      REG <= 1'h0; // @[src/main/scala/mini/Datapath.scala 94:58]
    end else begin
      REG <= stall | _next_pc_T_2 | _next_pc_T_3 | brCond_io_taken | _next_pc_T_7; // @[src/main/scala/mini/Datapath.scala 94:58]
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 95:58]
      REG_1 <= 33'h0; // @[src/main/scala/mini/Datapath.scala 95:58]
    end else if (stall) begin // @[src/main/scala/chisel3/util/Mux.scala 141:16]
      REG_1 <= pc;
    end else if (csr_io__expt) begin // @[src/main/scala/chisel3/util/Mux.scala 141:16]
      REG_1 <= {{1'd0}, csr_io__evec};
    end else if (_next_pc_T_2) begin // @[src/main/scala/chisel3/util/Mux.scala 141:16]
      REG_1 <= {{1'd0}, csr_io__epc};
    end else begin
      REG_1 <= _next_pc_T_9;
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 193:24]
      lw_addr <= 32'h0; // @[src/main/scala/mini/Datapath.scala 193:24]
    end else begin
      lw_addr <= alu_io_sum; // @[src/main/scala/mini/Datapath.scala 193:24]
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 207:26]
      mem_wmask <= 4'h0; // @[src/main/scala/mini/Datapath.scala 207:26]
    end else begin
      mem_wmask <= io__dcache_req_bits_mask; // @[src/main/scala/mini/Datapath.scala 207:26]
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 208:26]
      mem_wdata <= 32'h0; // @[src/main/scala/mini/Datapath.scala 208:26]
    end else begin
      mem_wdata <= io__dcache_req_bits_data; // @[src/main/scala/mini/Datapath.scala 208:26]
    end
    instCommit_predit_REG <= reset | _inst_T_2; // @[src/main/scala/mini/Datapath.scala 250:{40,40,40}]
    instCommit_predit_REG_1 <= reset | (instCommit_predit_REG | csr_io__expt); // @[src/main/scala/mini/Datapath.scala 250:{32,32,32}]
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 273:26]
      instOrder <= 64'h0; // @[src/main/scala/mini/Datapath.scala 273:26]
    end else if (instCommit) begin // @[src/main/scala/mini/Datapath.scala 275:19]
      instOrder <= _instOrder_T_1; // @[src/main/scala/mini/Datapath.scala 276:15]
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 288:34]
      flywire_rs1_addr_REG <= 1'h0; // @[src/main/scala/mini/Datapath.scala 288:34]
    end else begin
      flywire_rs1_addr_REG <= io__ctrl_A_sel; // @[src/main/scala/mini/Datapath.scala 288:34]
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 288:82]
      flywire_rs1_addr_REG_1 <= 1'h0; // @[src/main/scala/mini/Datapath.scala 288:82]
    end else begin
      flywire_rs1_addr_REG_1 <= io__ctrl_br_type != 3'h0; // @[src/main/scala/mini/Datapath.scala 288:82]
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 289:34]
      flywire_rs2_addr_REG <= 1'h0; // @[src/main/scala/mini/Datapath.scala 289:34]
    end else begin
      flywire_rs2_addr_REG <= io__ctrl_B_sel; // @[src/main/scala/mini/Datapath.scala 289:34]
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 289:82]
      flywire_rs2_addr_REG_1 <= 1'h0; // @[src/main/scala/mini/Datapath.scala 289:82]
    end else begin
      flywire_rs2_addr_REG_1 <= io__ctrl_st_type != 2'h0; // @[src/main/scala/mini/Datapath.scala 289:82]
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 289:130]
      flywire_rs2_addr_REG_2 <= 1'h0; // @[src/main/scala/mini/Datapath.scala 289:130]
    end else begin
      flywire_rs2_addr_REG_2 <= _flywire_rs1_addr_T_2; // @[src/main/scala/mini/Datapath.scala 289:130]
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 294:32]
      REG_2 <= 32'h0; // @[src/main/scala/mini/Datapath.scala 294:32]
    end else if (wb_sel == 2'h0 & rs1hazard) begin // @[src/main/scala/mini/Datapath.scala 133:16]
      REG_2 <= ew_reg__alu;
    end else begin
      REG_2 <= regFile_io_rdata1;
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 295:32]
      REG_3 <= 32'h0; // @[src/main/scala/mini/Datapath.scala 295:32]
    end else if (_rs1_T & rs2hazard) begin // @[src/main/scala/mini/Datapath.scala 134:16]
      REG_3 <= ew_reg__alu;
    end else begin
      REG_3 <= regFile_io_rdata2;
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 300:32]
      REG_4 <= 35'h0; // @[src/main/scala/mini/Datapath.scala 300:32]
    end else begin
      REG_4 <= daddr; // @[src/main/scala/mini/Datapath.scala 300:32]
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 338:25]
      req_addr <= 32'h0; // @[src/main/scala/mini/Datapath.scala 338:25]
    end else if (stall) begin // @[src/main/scala/mini/Datapath.scala 147:18]
      req_addr <= ew_reg__alu;
    end else begin
      req_addr <= alu_io_sum;
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 349:145]
      REG_5 <= 1'h0; // @[src/main/scala/mini/Datapath.scala 349:145]
    end else begin
      REG_5 <= store_width > 6'h0; // @[src/main/scala/mini/Datapath.scala 349:145]
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 349:251]
      REG_6 <= 6'h0; // @[src/main/scala/mini/Datapath.scala 349:251]
    end else if (_io_dcache_req_bits_mask_T_5) begin // @[src/main/scala/mini/Datapath.scala 330:79]
      REG_6 <= 6'h20;
    end else if (_io_dcache_req_bits_mask_T_7) begin // @[src/main/scala/mini/Datapath.scala 330:79]
      REG_6 <= 6'h10;
    end else if (_io_dcache_req_bits_mask_T_9) begin // @[src/main/scala/mini/Datapath.scala 330:79]
      REG_6 <= 6'h8;
    end else begin
      REG_6 <= 6'h0;
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 350:29]
      mem_write_valid_REG <= 1'h0; // @[src/main/scala/mini/Datapath.scala 350:29]
    end else begin
      mem_write_valid_REG <= _T_19; // @[src/main/scala/mini/Datapath.scala 350:29]
    end
    if (reset) begin // @[src/main/scala/mini/Datapath.scala 358:32]
      mem_write_memWidth_REG <= 6'h0; // @[src/main/scala/mini/Datapath.scala 358:32]
    end else if (_io_dcache_req_bits_mask_T_5) begin // @[src/main/scala/mini/Datapath.scala 330:79]
      mem_write_memWidth_REG <= 6'h20;
    end else if (_io_dcache_req_bits_mask_T_7) begin // @[src/main/scala/mini/Datapath.scala 330:79]
      mem_write_memWidth_REG <= 6'h10;
    end else if (_io_dcache_req_bits_mask_T_9) begin // @[src/main/scala/mini/Datapath.scala 330:79]
      mem_write_memWidth_REG <= 6'h8;
    end else begin
      mem_write_memWidth_REG <= 6'h0;
    end
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (~reset) begin
          $fwrite(32'h80000002,
            "FlyWireMemDebug[mask: %x, %x][write valid:%d, expt:%d]:  Width>0:%d, addr:%x, data:%x, memWidth:%x \n",
            mem_wmask,wmask_32bits,REG_5 & _T_16,csr_io__expt,store_width > 6'h0,req_addr,mem_wdata,REG_6); // @[src/main/scala/mini/Datapath.scala 349:7]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  fe_reg_inst = _RAND_0[31:0];
  _RAND_1 = {1{`RANDOM}};
  fe_reg_pc = _RAND_1[31:0];
  _RAND_2 = {1{`RANDOM}};
  ew_reg__inst = _RAND_2[31:0];
  _RAND_3 = {1{`RANDOM}};
  ew_reg__pc = _RAND_3[31:0];
  _RAND_4 = {1{`RANDOM}};
  ew_reg__alu = _RAND_4[31:0];
  _RAND_5 = {1{`RANDOM}};
  ew_reg__csr_in = _RAND_5[31:0];
  _RAND_6 = {1{`RANDOM}};
  st_type = _RAND_6[1:0];
  _RAND_7 = {1{`RANDOM}};
  ld_type = _RAND_7[2:0];
  _RAND_8 = {1{`RANDOM}};
  wb_sel = _RAND_8[1:0];
  _RAND_9 = {1{`RANDOM}};
  wb_en = _RAND_9[0:0];
  _RAND_10 = {1{`RANDOM}};
  csr_cmd = _RAND_10[2:0];
  _RAND_11 = {1{`RANDOM}};
  illegal = _RAND_11[0:0];
  _RAND_12 = {1{`RANDOM}};
  pc_check = _RAND_12[0:0];
  _RAND_13 = {1{`RANDOM}};
  started = _RAND_13[0:0];
  _RAND_14 = {2{`RANDOM}};
  pc = _RAND_14[32:0];
  _RAND_15 = {1{`RANDOM}};
  REG = _RAND_15[0:0];
  _RAND_16 = {2{`RANDOM}};
  REG_1 = _RAND_16[32:0];
  _RAND_17 = {1{`RANDOM}};
  lw_addr = _RAND_17[31:0];
  _RAND_18 = {1{`RANDOM}};
  mem_wmask = _RAND_18[3:0];
  _RAND_19 = {1{`RANDOM}};
  mem_wdata = _RAND_19[31:0];
  _RAND_20 = {1{`RANDOM}};
  instCommit_predit_REG = _RAND_20[0:0];
  _RAND_21 = {1{`RANDOM}};
  instCommit_predit_REG_1 = _RAND_21[0:0];
  _RAND_22 = {2{`RANDOM}};
  instOrder = _RAND_22[63:0];
  _RAND_23 = {1{`RANDOM}};
  flywire_rs1_addr_REG = _RAND_23[0:0];
  _RAND_24 = {1{`RANDOM}};
  flywire_rs1_addr_REG_1 = _RAND_24[0:0];
  _RAND_25 = {1{`RANDOM}};
  flywire_rs2_addr_REG = _RAND_25[0:0];
  _RAND_26 = {1{`RANDOM}};
  flywire_rs2_addr_REG_1 = _RAND_26[0:0];
  _RAND_27 = {1{`RANDOM}};
  flywire_rs2_addr_REG_2 = _RAND_27[0:0];
  _RAND_28 = {1{`RANDOM}};
  REG_2 = _RAND_28[31:0];
  _RAND_29 = {1{`RANDOM}};
  REG_3 = _RAND_29[31:0];
  _RAND_30 = {2{`RANDOM}};
  REG_4 = _RAND_30[34:0];
  _RAND_31 = {1{`RANDOM}};
  req_addr = _RAND_31[31:0];
  _RAND_32 = {1{`RANDOM}};
  REG_5 = _RAND_32[0:0];
  _RAND_33 = {1{`RANDOM}};
  REG_6 = _RAND_33[5:0];
  _RAND_34 = {1{`RANDOM}};
  mem_write_valid_REG = _RAND_34[0:0];
  _RAND_35 = {1{`RANDOM}};
  mem_write_memWidth_REG = _RAND_35[5:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module Control(
  input  [31:0] io_inst, // @[src/main/scala/mini/Control.scala 146:14]
  output [1:0]  io_pc_sel, // @[src/main/scala/mini/Control.scala 146:14]
  output        io_inst_kill, // @[src/main/scala/mini/Control.scala 146:14]
  output        io_A_sel, // @[src/main/scala/mini/Control.scala 146:14]
  output        io_B_sel, // @[src/main/scala/mini/Control.scala 146:14]
  output [2:0]  io_imm_sel, // @[src/main/scala/mini/Control.scala 146:14]
  output [3:0]  io_alu_op, // @[src/main/scala/mini/Control.scala 146:14]
  output [2:0]  io_br_type, // @[src/main/scala/mini/Control.scala 146:14]
  output [1:0]  io_st_type, // @[src/main/scala/mini/Control.scala 146:14]
  output [2:0]  io_ld_type, // @[src/main/scala/mini/Control.scala 146:14]
  output [1:0]  io_wb_sel, // @[src/main/scala/mini/Control.scala 146:14]
  output        io_wb_en, // @[src/main/scala/mini/Control.scala 146:14]
  output [2:0]  io_csr_cmd, // @[src/main/scala/mini/Control.scala 146:14]
  output        io_illegal // @[src/main/scala/mini/Control.scala 146:14]
);
  wire [31:0] _ctrlSignals_T = io_inst & 32'h7f; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_1 = 32'h37 == _ctrlSignals_T; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_3 = 32'h17 == _ctrlSignals_T; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_5 = 32'h6f == _ctrlSignals_T; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire [31:0] _ctrlSignals_T_6 = io_inst & 32'h707f; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_7 = 32'h67 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_9 = 32'h63 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_11 = 32'h1063 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_13 = 32'h4063 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_15 = 32'h5063 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_17 = 32'h6063 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_19 = 32'h7063 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_21 = 32'h3 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_23 = 32'h1003 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_25 = 32'h2003 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_27 = 32'h4003 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_29 = 32'h5003 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_31 = 32'h23 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_33 = 32'h1023 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_35 = 32'h2023 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_37 = 32'h13 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_39 = 32'h2013 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_41 = 32'h3013 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_43 = 32'h4013 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_45 = 32'h6013 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_47 = 32'h7013 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire [31:0] _ctrlSignals_T_48 = io_inst & 32'hfe00707f; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_49 = 32'h1013 == _ctrlSignals_T_48; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_51 = 32'h5013 == _ctrlSignals_T_48; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_53 = 32'h40005013 == _ctrlSignals_T_48; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_55 = 32'h33 == _ctrlSignals_T_48; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_57 = 32'h40000033 == _ctrlSignals_T_48; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_59 = 32'h1033 == _ctrlSignals_T_48; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_61 = 32'h2033 == _ctrlSignals_T_48; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_63 = 32'h3033 == _ctrlSignals_T_48; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_65 = 32'h4033 == _ctrlSignals_T_48; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_67 = 32'h5033 == _ctrlSignals_T_48; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_69 = 32'h40005033 == _ctrlSignals_T_48; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_71 = 32'h6033 == _ctrlSignals_T_48; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_73 = 32'h7033 == _ctrlSignals_T_48; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire [31:0] _ctrlSignals_T_74 = io_inst & 32'hf00fffff; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_75 = 32'hf == _ctrlSignals_T_74; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_77 = 32'h100f == io_inst; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_79 = 32'h1073 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_81 = 32'h2073 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_83 = 32'h3073 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_85 = 32'h5073 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_87 = 32'h6073 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_89 = 32'h7073 == _ctrlSignals_T_6; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_91 = 32'h73 == io_inst; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_93 = 32'h100073 == io_inst; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_95 = 32'h10000073 == io_inst; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire  _ctrlSignals_T_97 = 32'h10200073 == io_inst; // @[src/main/scala/chisel3/util/Lookup.scala 31:38]
  wire [1:0] _ctrlSignals_T_99 = _ctrlSignals_T_95 ? 2'h3 : 2'h0; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_100 = _ctrlSignals_T_93 ? 2'h0 : _ctrlSignals_T_99; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_101 = _ctrlSignals_T_91 ? 2'h0 : _ctrlSignals_T_100; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_102 = _ctrlSignals_T_89 ? 2'h2 : _ctrlSignals_T_101; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_103 = _ctrlSignals_T_87 ? 2'h2 : _ctrlSignals_T_102; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_104 = _ctrlSignals_T_85 ? 2'h2 : _ctrlSignals_T_103; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_105 = _ctrlSignals_T_83 ? 2'h2 : _ctrlSignals_T_104; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_106 = _ctrlSignals_T_81 ? 2'h2 : _ctrlSignals_T_105; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_107 = _ctrlSignals_T_79 ? 2'h2 : _ctrlSignals_T_106; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_108 = _ctrlSignals_T_77 ? 2'h2 : _ctrlSignals_T_107; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_109 = _ctrlSignals_T_75 ? 2'h0 : _ctrlSignals_T_108; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_110 = _ctrlSignals_T_73 ? 2'h0 : _ctrlSignals_T_109; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_111 = _ctrlSignals_T_71 ? 2'h0 : _ctrlSignals_T_110; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_112 = _ctrlSignals_T_69 ? 2'h0 : _ctrlSignals_T_111; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_113 = _ctrlSignals_T_67 ? 2'h0 : _ctrlSignals_T_112; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_114 = _ctrlSignals_T_65 ? 2'h0 : _ctrlSignals_T_113; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_115 = _ctrlSignals_T_63 ? 2'h0 : _ctrlSignals_T_114; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_116 = _ctrlSignals_T_61 ? 2'h0 : _ctrlSignals_T_115; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_117 = _ctrlSignals_T_59 ? 2'h0 : _ctrlSignals_T_116; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_118 = _ctrlSignals_T_57 ? 2'h0 : _ctrlSignals_T_117; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_119 = _ctrlSignals_T_55 ? 2'h0 : _ctrlSignals_T_118; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_120 = _ctrlSignals_T_53 ? 2'h0 : _ctrlSignals_T_119; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_121 = _ctrlSignals_T_51 ? 2'h0 : _ctrlSignals_T_120; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_122 = _ctrlSignals_T_49 ? 2'h0 : _ctrlSignals_T_121; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_123 = _ctrlSignals_T_47 ? 2'h0 : _ctrlSignals_T_122; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_124 = _ctrlSignals_T_45 ? 2'h0 : _ctrlSignals_T_123; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_125 = _ctrlSignals_T_43 ? 2'h0 : _ctrlSignals_T_124; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_126 = _ctrlSignals_T_41 ? 2'h0 : _ctrlSignals_T_125; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_127 = _ctrlSignals_T_39 ? 2'h0 : _ctrlSignals_T_126; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_128 = _ctrlSignals_T_37 ? 2'h0 : _ctrlSignals_T_127; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_129 = _ctrlSignals_T_35 ? 2'h0 : _ctrlSignals_T_128; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_130 = _ctrlSignals_T_33 ? 2'h0 : _ctrlSignals_T_129; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_131 = _ctrlSignals_T_31 ? 2'h0 : _ctrlSignals_T_130; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_132 = _ctrlSignals_T_29 ? 2'h2 : _ctrlSignals_T_131; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_133 = _ctrlSignals_T_27 ? 2'h2 : _ctrlSignals_T_132; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_134 = _ctrlSignals_T_25 ? 2'h2 : _ctrlSignals_T_133; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_135 = _ctrlSignals_T_23 ? 2'h2 : _ctrlSignals_T_134; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_136 = _ctrlSignals_T_21 ? 2'h2 : _ctrlSignals_T_135; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_137 = _ctrlSignals_T_19 ? 2'h0 : _ctrlSignals_T_136; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_138 = _ctrlSignals_T_17 ? 2'h0 : _ctrlSignals_T_137; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_139 = _ctrlSignals_T_15 ? 2'h0 : _ctrlSignals_T_138; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_140 = _ctrlSignals_T_13 ? 2'h0 : _ctrlSignals_T_139; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_141 = _ctrlSignals_T_11 ? 2'h0 : _ctrlSignals_T_140; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_142 = _ctrlSignals_T_9 ? 2'h0 : _ctrlSignals_T_141; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_143 = _ctrlSignals_T_7 ? 2'h1 : _ctrlSignals_T_142; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_144 = _ctrlSignals_T_5 ? 2'h1 : _ctrlSignals_T_143; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_145 = _ctrlSignals_T_3 ? 2'h0 : _ctrlSignals_T_144; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_156 = _ctrlSignals_T_77 ? 1'h0 : _ctrlSignals_T_79 | (_ctrlSignals_T_81 | _ctrlSignals_T_83); // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_157 = _ctrlSignals_T_75 ? 1'h0 : _ctrlSignals_T_156; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_185 = _ctrlSignals_T_19 ? 1'h0 : _ctrlSignals_T_21 | (_ctrlSignals_T_23 | (_ctrlSignals_T_25 | (
    _ctrlSignals_T_27 | (_ctrlSignals_T_29 | (_ctrlSignals_T_31 | (_ctrlSignals_T_33 | (_ctrlSignals_T_35 | (
    _ctrlSignals_T_37 | (_ctrlSignals_T_39 | (_ctrlSignals_T_41 | (_ctrlSignals_T_43 | (_ctrlSignals_T_45 | (
    _ctrlSignals_T_47 | (_ctrlSignals_T_49 | (_ctrlSignals_T_51 | (_ctrlSignals_T_53 | (_ctrlSignals_T_55 | (
    _ctrlSignals_T_57 | (_ctrlSignals_T_59 | (_ctrlSignals_T_61 | (_ctrlSignals_T_63 | (_ctrlSignals_T_65 | (
    _ctrlSignals_T_67 | (_ctrlSignals_T_69 | (_ctrlSignals_T_71 | (_ctrlSignals_T_73 | _ctrlSignals_T_157)))))))))))))))
    ))))))))))); // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_186 = _ctrlSignals_T_17 ? 1'h0 : _ctrlSignals_T_185; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_187 = _ctrlSignals_T_15 ? 1'h0 : _ctrlSignals_T_186; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_188 = _ctrlSignals_T_13 ? 1'h0 : _ctrlSignals_T_187; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_189 = _ctrlSignals_T_11 ? 1'h0 : _ctrlSignals_T_188; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_190 = _ctrlSignals_T_9 ? 1'h0 : _ctrlSignals_T_189; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_192 = _ctrlSignals_T_5 ? 1'h0 : _ctrlSignals_T_7 | _ctrlSignals_T_190; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_193 = _ctrlSignals_T_3 ? 1'h0 : _ctrlSignals_T_192; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_216 = _ctrlSignals_T_53 ? 1'h0 : _ctrlSignals_T_55 | (_ctrlSignals_T_57 | (_ctrlSignals_T_59 | (
    _ctrlSignals_T_61 | (_ctrlSignals_T_63 | (_ctrlSignals_T_65 | (_ctrlSignals_T_67 | (_ctrlSignals_T_69 | (
    _ctrlSignals_T_71 | _ctrlSignals_T_73)))))))); // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_217 = _ctrlSignals_T_51 ? 1'h0 : _ctrlSignals_T_216; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_218 = _ctrlSignals_T_49 ? 1'h0 : _ctrlSignals_T_217; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_219 = _ctrlSignals_T_47 ? 1'h0 : _ctrlSignals_T_218; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_220 = _ctrlSignals_T_45 ? 1'h0 : _ctrlSignals_T_219; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_221 = _ctrlSignals_T_43 ? 1'h0 : _ctrlSignals_T_220; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_222 = _ctrlSignals_T_41 ? 1'h0 : _ctrlSignals_T_221; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_223 = _ctrlSignals_T_39 ? 1'h0 : _ctrlSignals_T_222; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_224 = _ctrlSignals_T_37 ? 1'h0 : _ctrlSignals_T_223; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_225 = _ctrlSignals_T_35 ? 1'h0 : _ctrlSignals_T_224; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_226 = _ctrlSignals_T_33 ? 1'h0 : _ctrlSignals_T_225; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_227 = _ctrlSignals_T_31 ? 1'h0 : _ctrlSignals_T_226; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_228 = _ctrlSignals_T_29 ? 1'h0 : _ctrlSignals_T_227; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_229 = _ctrlSignals_T_27 ? 1'h0 : _ctrlSignals_T_228; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_230 = _ctrlSignals_T_25 ? 1'h0 : _ctrlSignals_T_229; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_231 = _ctrlSignals_T_23 ? 1'h0 : _ctrlSignals_T_230; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_232 = _ctrlSignals_T_21 ? 1'h0 : _ctrlSignals_T_231; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_233 = _ctrlSignals_T_19 ? 1'h0 : _ctrlSignals_T_232; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_234 = _ctrlSignals_T_17 ? 1'h0 : _ctrlSignals_T_233; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_235 = _ctrlSignals_T_15 ? 1'h0 : _ctrlSignals_T_234; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_236 = _ctrlSignals_T_13 ? 1'h0 : _ctrlSignals_T_235; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_237 = _ctrlSignals_T_11 ? 1'h0 : _ctrlSignals_T_236; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_238 = _ctrlSignals_T_9 ? 1'h0 : _ctrlSignals_T_237; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_239 = _ctrlSignals_T_7 ? 1'h0 : _ctrlSignals_T_238; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_240 = _ctrlSignals_T_5 ? 1'h0 : _ctrlSignals_T_239; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_241 = _ctrlSignals_T_3 ? 1'h0 : _ctrlSignals_T_240; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_246 = _ctrlSignals_T_89 ? 3'h6 : 3'h0; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_247 = _ctrlSignals_T_87 ? 3'h6 : _ctrlSignals_T_246; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_248 = _ctrlSignals_T_85 ? 3'h6 : _ctrlSignals_T_247; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_249 = _ctrlSignals_T_83 ? 3'h0 : _ctrlSignals_T_248; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_250 = _ctrlSignals_T_81 ? 3'h0 : _ctrlSignals_T_249; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_251 = _ctrlSignals_T_79 ? 3'h0 : _ctrlSignals_T_250; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_252 = _ctrlSignals_T_77 ? 3'h0 : _ctrlSignals_T_251; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_253 = _ctrlSignals_T_75 ? 3'h0 : _ctrlSignals_T_252; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_254 = _ctrlSignals_T_73 ? 3'h0 : _ctrlSignals_T_253; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_255 = _ctrlSignals_T_71 ? 3'h0 : _ctrlSignals_T_254; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_256 = _ctrlSignals_T_69 ? 3'h0 : _ctrlSignals_T_255; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_257 = _ctrlSignals_T_67 ? 3'h0 : _ctrlSignals_T_256; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_258 = _ctrlSignals_T_65 ? 3'h0 : _ctrlSignals_T_257; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_259 = _ctrlSignals_T_63 ? 3'h0 : _ctrlSignals_T_258; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_260 = _ctrlSignals_T_61 ? 3'h0 : _ctrlSignals_T_259; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_261 = _ctrlSignals_T_59 ? 3'h0 : _ctrlSignals_T_260; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_262 = _ctrlSignals_T_57 ? 3'h0 : _ctrlSignals_T_261; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_263 = _ctrlSignals_T_55 ? 3'h0 : _ctrlSignals_T_262; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_264 = _ctrlSignals_T_53 ? 3'h1 : _ctrlSignals_T_263; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_265 = _ctrlSignals_T_51 ? 3'h1 : _ctrlSignals_T_264; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_266 = _ctrlSignals_T_49 ? 3'h1 : _ctrlSignals_T_265; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_267 = _ctrlSignals_T_47 ? 3'h1 : _ctrlSignals_T_266; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_268 = _ctrlSignals_T_45 ? 3'h1 : _ctrlSignals_T_267; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_269 = _ctrlSignals_T_43 ? 3'h1 : _ctrlSignals_T_268; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_270 = _ctrlSignals_T_41 ? 3'h1 : _ctrlSignals_T_269; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_271 = _ctrlSignals_T_39 ? 3'h1 : _ctrlSignals_T_270; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_272 = _ctrlSignals_T_37 ? 3'h1 : _ctrlSignals_T_271; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_273 = _ctrlSignals_T_35 ? 3'h2 : _ctrlSignals_T_272; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_274 = _ctrlSignals_T_33 ? 3'h2 : _ctrlSignals_T_273; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_275 = _ctrlSignals_T_31 ? 3'h2 : _ctrlSignals_T_274; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_276 = _ctrlSignals_T_29 ? 3'h1 : _ctrlSignals_T_275; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_277 = _ctrlSignals_T_27 ? 3'h1 : _ctrlSignals_T_276; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_278 = _ctrlSignals_T_25 ? 3'h1 : _ctrlSignals_T_277; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_279 = _ctrlSignals_T_23 ? 3'h1 : _ctrlSignals_T_278; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_280 = _ctrlSignals_T_21 ? 3'h1 : _ctrlSignals_T_279; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_281 = _ctrlSignals_T_19 ? 3'h5 : _ctrlSignals_T_280; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_282 = _ctrlSignals_T_17 ? 3'h5 : _ctrlSignals_T_281; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_283 = _ctrlSignals_T_15 ? 3'h5 : _ctrlSignals_T_282; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_284 = _ctrlSignals_T_13 ? 3'h5 : _ctrlSignals_T_283; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_285 = _ctrlSignals_T_11 ? 3'h5 : _ctrlSignals_T_284; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_286 = _ctrlSignals_T_9 ? 3'h5 : _ctrlSignals_T_285; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_287 = _ctrlSignals_T_7 ? 3'h1 : _ctrlSignals_T_286; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_288 = _ctrlSignals_T_5 ? 3'h4 : _ctrlSignals_T_287; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_289 = _ctrlSignals_T_3 ? 3'h3 : _ctrlSignals_T_288; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_297 = _ctrlSignals_T_83 ? 4'ha : 4'hf; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_298 = _ctrlSignals_T_81 ? 4'ha : _ctrlSignals_T_297; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_299 = _ctrlSignals_T_79 ? 4'ha : _ctrlSignals_T_298; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_300 = _ctrlSignals_T_77 ? 4'hf : _ctrlSignals_T_299; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_301 = _ctrlSignals_T_75 ? 4'hf : _ctrlSignals_T_300; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_302 = _ctrlSignals_T_73 ? 4'h2 : _ctrlSignals_T_301; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_303 = _ctrlSignals_T_71 ? 4'h3 : _ctrlSignals_T_302; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_304 = _ctrlSignals_T_69 ? 4'h9 : _ctrlSignals_T_303; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_305 = _ctrlSignals_T_67 ? 4'h8 : _ctrlSignals_T_304; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_306 = _ctrlSignals_T_65 ? 4'h4 : _ctrlSignals_T_305; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_307 = _ctrlSignals_T_63 ? 4'h7 : _ctrlSignals_T_306; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_308 = _ctrlSignals_T_61 ? 4'h5 : _ctrlSignals_T_307; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_309 = _ctrlSignals_T_59 ? 4'h6 : _ctrlSignals_T_308; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_310 = _ctrlSignals_T_57 ? 4'h1 : _ctrlSignals_T_309; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_311 = _ctrlSignals_T_55 ? 4'h0 : _ctrlSignals_T_310; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_312 = _ctrlSignals_T_53 ? 4'h9 : _ctrlSignals_T_311; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_313 = _ctrlSignals_T_51 ? 4'h8 : _ctrlSignals_T_312; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_314 = _ctrlSignals_T_49 ? 4'h6 : _ctrlSignals_T_313; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_315 = _ctrlSignals_T_47 ? 4'h2 : _ctrlSignals_T_314; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_316 = _ctrlSignals_T_45 ? 4'h3 : _ctrlSignals_T_315; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_317 = _ctrlSignals_T_43 ? 4'h4 : _ctrlSignals_T_316; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_318 = _ctrlSignals_T_41 ? 4'h7 : _ctrlSignals_T_317; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_319 = _ctrlSignals_T_39 ? 4'h5 : _ctrlSignals_T_318; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_320 = _ctrlSignals_T_37 ? 4'h0 : _ctrlSignals_T_319; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_321 = _ctrlSignals_T_35 ? 4'h0 : _ctrlSignals_T_320; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_322 = _ctrlSignals_T_33 ? 4'h0 : _ctrlSignals_T_321; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_323 = _ctrlSignals_T_31 ? 4'h0 : _ctrlSignals_T_322; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_324 = _ctrlSignals_T_29 ? 4'h0 : _ctrlSignals_T_323; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_325 = _ctrlSignals_T_27 ? 4'h0 : _ctrlSignals_T_324; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_326 = _ctrlSignals_T_25 ? 4'h0 : _ctrlSignals_T_325; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_327 = _ctrlSignals_T_23 ? 4'h0 : _ctrlSignals_T_326; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_328 = _ctrlSignals_T_21 ? 4'h0 : _ctrlSignals_T_327; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_329 = _ctrlSignals_T_19 ? 4'h0 : _ctrlSignals_T_328; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_330 = _ctrlSignals_T_17 ? 4'h0 : _ctrlSignals_T_329; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_331 = _ctrlSignals_T_15 ? 4'h0 : _ctrlSignals_T_330; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_332 = _ctrlSignals_T_13 ? 4'h0 : _ctrlSignals_T_331; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_333 = _ctrlSignals_T_11 ? 4'h0 : _ctrlSignals_T_332; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_334 = _ctrlSignals_T_9 ? 4'h0 : _ctrlSignals_T_333; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_335 = _ctrlSignals_T_7 ? 4'h0 : _ctrlSignals_T_334; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_336 = _ctrlSignals_T_5 ? 4'h0 : _ctrlSignals_T_335; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_337 = _ctrlSignals_T_3 ? 4'h0 : _ctrlSignals_T_336; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_377 = _ctrlSignals_T_19 ? 3'h4 : 3'h0; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_378 = _ctrlSignals_T_17 ? 3'h1 : _ctrlSignals_T_377; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_379 = _ctrlSignals_T_15 ? 3'h5 : _ctrlSignals_T_378; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_380 = _ctrlSignals_T_13 ? 3'h2 : _ctrlSignals_T_379; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_381 = _ctrlSignals_T_11 ? 3'h6 : _ctrlSignals_T_380; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_382 = _ctrlSignals_T_9 ? 3'h3 : _ctrlSignals_T_381; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_383 = _ctrlSignals_T_7 ? 3'h0 : _ctrlSignals_T_382; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_384 = _ctrlSignals_T_5 ? 3'h0 : _ctrlSignals_T_383; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_385 = _ctrlSignals_T_3 ? 3'h0 : _ctrlSignals_T_384; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_388 = _ctrlSignals_T_93 ? 1'h0 : _ctrlSignals_T_95; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_389 = _ctrlSignals_T_91 ? 1'h0 : _ctrlSignals_T_388; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_397 = _ctrlSignals_T_75 ? 1'h0 : _ctrlSignals_T_77 | (_ctrlSignals_T_79 | (_ctrlSignals_T_81 | (
    _ctrlSignals_T_83 | (_ctrlSignals_T_85 | (_ctrlSignals_T_87 | (_ctrlSignals_T_89 | _ctrlSignals_T_389)))))); // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_398 = _ctrlSignals_T_73 ? 1'h0 : _ctrlSignals_T_397; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_399 = _ctrlSignals_T_71 ? 1'h0 : _ctrlSignals_T_398; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_400 = _ctrlSignals_T_69 ? 1'h0 : _ctrlSignals_T_399; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_401 = _ctrlSignals_T_67 ? 1'h0 : _ctrlSignals_T_400; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_402 = _ctrlSignals_T_65 ? 1'h0 : _ctrlSignals_T_401; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_403 = _ctrlSignals_T_63 ? 1'h0 : _ctrlSignals_T_402; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_404 = _ctrlSignals_T_61 ? 1'h0 : _ctrlSignals_T_403; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_405 = _ctrlSignals_T_59 ? 1'h0 : _ctrlSignals_T_404; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_406 = _ctrlSignals_T_57 ? 1'h0 : _ctrlSignals_T_405; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_407 = _ctrlSignals_T_55 ? 1'h0 : _ctrlSignals_T_406; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_408 = _ctrlSignals_T_53 ? 1'h0 : _ctrlSignals_T_407; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_409 = _ctrlSignals_T_51 ? 1'h0 : _ctrlSignals_T_408; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_410 = _ctrlSignals_T_49 ? 1'h0 : _ctrlSignals_T_409; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_411 = _ctrlSignals_T_47 ? 1'h0 : _ctrlSignals_T_410; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_412 = _ctrlSignals_T_45 ? 1'h0 : _ctrlSignals_T_411; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_413 = _ctrlSignals_T_43 ? 1'h0 : _ctrlSignals_T_412; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_414 = _ctrlSignals_T_41 ? 1'h0 : _ctrlSignals_T_413; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_415 = _ctrlSignals_T_39 ? 1'h0 : _ctrlSignals_T_414; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_416 = _ctrlSignals_T_37 ? 1'h0 : _ctrlSignals_T_415; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_417 = _ctrlSignals_T_35 ? 1'h0 : _ctrlSignals_T_416; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_418 = _ctrlSignals_T_33 ? 1'h0 : _ctrlSignals_T_417; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_419 = _ctrlSignals_T_31 ? 1'h0 : _ctrlSignals_T_418; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_425 = _ctrlSignals_T_19 ? 1'h0 : _ctrlSignals_T_21 | (_ctrlSignals_T_23 | (_ctrlSignals_T_25 | (
    _ctrlSignals_T_27 | (_ctrlSignals_T_29 | _ctrlSignals_T_419)))); // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_426 = _ctrlSignals_T_17 ? 1'h0 : _ctrlSignals_T_425; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_427 = _ctrlSignals_T_15 ? 1'h0 : _ctrlSignals_T_426; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_428 = _ctrlSignals_T_13 ? 1'h0 : _ctrlSignals_T_427; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_429 = _ctrlSignals_T_11 ? 1'h0 : _ctrlSignals_T_428; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_430 = _ctrlSignals_T_9 ? 1'h0 : _ctrlSignals_T_429; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_433 = _ctrlSignals_T_3 ? 1'h0 : _ctrlSignals_T_5 | (_ctrlSignals_T_7 | _ctrlSignals_T_430); // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_465 = _ctrlSignals_T_35 ? 2'h1 : 2'h0; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_466 = _ctrlSignals_T_33 ? 2'h2 : _ctrlSignals_T_465; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_467 = _ctrlSignals_T_31 ? 2'h3 : _ctrlSignals_T_466; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_468 = _ctrlSignals_T_29 ? 2'h0 : _ctrlSignals_T_467; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_469 = _ctrlSignals_T_27 ? 2'h0 : _ctrlSignals_T_468; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_470 = _ctrlSignals_T_25 ? 2'h0 : _ctrlSignals_T_469; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_471 = _ctrlSignals_T_23 ? 2'h0 : _ctrlSignals_T_470; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_472 = _ctrlSignals_T_21 ? 2'h0 : _ctrlSignals_T_471; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_473 = _ctrlSignals_T_19 ? 2'h0 : _ctrlSignals_T_472; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_474 = _ctrlSignals_T_17 ? 2'h0 : _ctrlSignals_T_473; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_475 = _ctrlSignals_T_15 ? 2'h0 : _ctrlSignals_T_474; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_476 = _ctrlSignals_T_13 ? 2'h0 : _ctrlSignals_T_475; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_477 = _ctrlSignals_T_11 ? 2'h0 : _ctrlSignals_T_476; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_478 = _ctrlSignals_T_9 ? 2'h0 : _ctrlSignals_T_477; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_479 = _ctrlSignals_T_7 ? 2'h0 : _ctrlSignals_T_478; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_480 = _ctrlSignals_T_5 ? 2'h0 : _ctrlSignals_T_479; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_481 = _ctrlSignals_T_3 ? 2'h0 : _ctrlSignals_T_480; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_516 = _ctrlSignals_T_29 ? 3'h4 : 3'h0; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_517 = _ctrlSignals_T_27 ? 3'h5 : _ctrlSignals_T_516; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_518 = _ctrlSignals_T_25 ? 3'h1 : _ctrlSignals_T_517; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_519 = _ctrlSignals_T_23 ? 3'h2 : _ctrlSignals_T_518; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_520 = _ctrlSignals_T_21 ? 3'h3 : _ctrlSignals_T_519; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_521 = _ctrlSignals_T_19 ? 3'h0 : _ctrlSignals_T_520; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_522 = _ctrlSignals_T_17 ? 3'h0 : _ctrlSignals_T_521; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_523 = _ctrlSignals_T_15 ? 3'h0 : _ctrlSignals_T_522; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_524 = _ctrlSignals_T_13 ? 3'h0 : _ctrlSignals_T_523; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_525 = _ctrlSignals_T_11 ? 3'h0 : _ctrlSignals_T_524; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_526 = _ctrlSignals_T_9 ? 3'h0 : _ctrlSignals_T_525; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_527 = _ctrlSignals_T_7 ? 3'h0 : _ctrlSignals_T_526; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_528 = _ctrlSignals_T_5 ? 3'h0 : _ctrlSignals_T_527; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_529 = _ctrlSignals_T_3 ? 3'h0 : _ctrlSignals_T_528; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_532 = _ctrlSignals_T_93 ? 2'h3 : _ctrlSignals_T_99; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_533 = _ctrlSignals_T_91 ? 2'h3 : _ctrlSignals_T_532; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_534 = _ctrlSignals_T_89 ? 2'h3 : _ctrlSignals_T_533; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_535 = _ctrlSignals_T_87 ? 2'h3 : _ctrlSignals_T_534; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_536 = _ctrlSignals_T_85 ? 2'h3 : _ctrlSignals_T_535; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_537 = _ctrlSignals_T_83 ? 2'h3 : _ctrlSignals_T_536; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_538 = _ctrlSignals_T_81 ? 2'h3 : _ctrlSignals_T_537; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_539 = _ctrlSignals_T_79 ? 2'h3 : _ctrlSignals_T_538; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_540 = _ctrlSignals_T_77 ? 2'h0 : _ctrlSignals_T_539; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_541 = _ctrlSignals_T_75 ? 2'h0 : _ctrlSignals_T_540; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_542 = _ctrlSignals_T_73 ? 2'h0 : _ctrlSignals_T_541; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_543 = _ctrlSignals_T_71 ? 2'h0 : _ctrlSignals_T_542; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_544 = _ctrlSignals_T_69 ? 2'h0 : _ctrlSignals_T_543; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_545 = _ctrlSignals_T_67 ? 2'h0 : _ctrlSignals_T_544; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_546 = _ctrlSignals_T_65 ? 2'h0 : _ctrlSignals_T_545; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_547 = _ctrlSignals_T_63 ? 2'h0 : _ctrlSignals_T_546; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_548 = _ctrlSignals_T_61 ? 2'h0 : _ctrlSignals_T_547; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_549 = _ctrlSignals_T_59 ? 2'h0 : _ctrlSignals_T_548; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_550 = _ctrlSignals_T_57 ? 2'h0 : _ctrlSignals_T_549; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_551 = _ctrlSignals_T_55 ? 2'h0 : _ctrlSignals_T_550; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_552 = _ctrlSignals_T_53 ? 2'h0 : _ctrlSignals_T_551; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_553 = _ctrlSignals_T_51 ? 2'h0 : _ctrlSignals_T_552; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_554 = _ctrlSignals_T_49 ? 2'h0 : _ctrlSignals_T_553; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_555 = _ctrlSignals_T_47 ? 2'h0 : _ctrlSignals_T_554; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_556 = _ctrlSignals_T_45 ? 2'h0 : _ctrlSignals_T_555; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_557 = _ctrlSignals_T_43 ? 2'h0 : _ctrlSignals_T_556; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_558 = _ctrlSignals_T_41 ? 2'h0 : _ctrlSignals_T_557; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_559 = _ctrlSignals_T_39 ? 2'h0 : _ctrlSignals_T_558; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_560 = _ctrlSignals_T_37 ? 2'h0 : _ctrlSignals_T_559; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_561 = _ctrlSignals_T_35 ? 2'h0 : _ctrlSignals_T_560; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_562 = _ctrlSignals_T_33 ? 2'h0 : _ctrlSignals_T_561; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_563 = _ctrlSignals_T_31 ? 2'h0 : _ctrlSignals_T_562; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_564 = _ctrlSignals_T_29 ? 2'h1 : _ctrlSignals_T_563; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_565 = _ctrlSignals_T_27 ? 2'h1 : _ctrlSignals_T_564; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_566 = _ctrlSignals_T_25 ? 2'h1 : _ctrlSignals_T_565; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_567 = _ctrlSignals_T_23 ? 2'h1 : _ctrlSignals_T_566; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_568 = _ctrlSignals_T_21 ? 2'h1 : _ctrlSignals_T_567; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_569 = _ctrlSignals_T_19 ? 2'h0 : _ctrlSignals_T_568; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_570 = _ctrlSignals_T_17 ? 2'h0 : _ctrlSignals_T_569; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_571 = _ctrlSignals_T_15 ? 2'h0 : _ctrlSignals_T_570; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_572 = _ctrlSignals_T_13 ? 2'h0 : _ctrlSignals_T_571; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_573 = _ctrlSignals_T_11 ? 2'h0 : _ctrlSignals_T_572; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_574 = _ctrlSignals_T_9 ? 2'h0 : _ctrlSignals_T_573; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_575 = _ctrlSignals_T_7 ? 2'h2 : _ctrlSignals_T_574; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_576 = _ctrlSignals_T_5 ? 2'h2 : _ctrlSignals_T_575; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_577 = _ctrlSignals_T_3 ? 2'h0 : _ctrlSignals_T_576; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_588 = _ctrlSignals_T_77 ? 1'h0 : _ctrlSignals_T_79 | (_ctrlSignals_T_81 | (_ctrlSignals_T_83 | (
    _ctrlSignals_T_85 | (_ctrlSignals_T_87 | _ctrlSignals_T_89)))); // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_589 = _ctrlSignals_T_75 ? 1'h0 : _ctrlSignals_T_588; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_609 = _ctrlSignals_T_35 ? 1'h0 : _ctrlSignals_T_37 | (_ctrlSignals_T_39 | (_ctrlSignals_T_41 | (
    _ctrlSignals_T_43 | (_ctrlSignals_T_45 | (_ctrlSignals_T_47 | (_ctrlSignals_T_49 | (_ctrlSignals_T_51 | (
    _ctrlSignals_T_53 | (_ctrlSignals_T_55 | (_ctrlSignals_T_57 | (_ctrlSignals_T_59 | (_ctrlSignals_T_61 | (
    _ctrlSignals_T_63 | (_ctrlSignals_T_65 | (_ctrlSignals_T_67 | (_ctrlSignals_T_69 | (_ctrlSignals_T_71 | (
    _ctrlSignals_T_73 | _ctrlSignals_T_589)))))))))))))))))); // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_610 = _ctrlSignals_T_33 ? 1'h0 : _ctrlSignals_T_609; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_611 = _ctrlSignals_T_31 ? 1'h0 : _ctrlSignals_T_610; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_617 = _ctrlSignals_T_19 ? 1'h0 : _ctrlSignals_T_21 | (_ctrlSignals_T_23 | (_ctrlSignals_T_25 | (
    _ctrlSignals_T_27 | (_ctrlSignals_T_29 | _ctrlSignals_T_611)))); // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_618 = _ctrlSignals_T_17 ? 1'h0 : _ctrlSignals_T_617; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_619 = _ctrlSignals_T_15 ? 1'h0 : _ctrlSignals_T_618; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_620 = _ctrlSignals_T_13 ? 1'h0 : _ctrlSignals_T_619; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_621 = _ctrlSignals_T_11 ? 1'h0 : _ctrlSignals_T_620; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_622 = _ctrlSignals_T_9 ? 1'h0 : _ctrlSignals_T_621; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_627 = _ctrlSignals_T_95 ? 3'h4 : 3'h0; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_628 = _ctrlSignals_T_93 ? 3'h4 : _ctrlSignals_T_627; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_629 = _ctrlSignals_T_91 ? 3'h4 : _ctrlSignals_T_628; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_630 = _ctrlSignals_T_89 ? 3'h3 : _ctrlSignals_T_629; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_631 = _ctrlSignals_T_87 ? 3'h2 : _ctrlSignals_T_630; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_632 = _ctrlSignals_T_85 ? 3'h1 : _ctrlSignals_T_631; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_633 = _ctrlSignals_T_83 ? 3'h3 : _ctrlSignals_T_632; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_634 = _ctrlSignals_T_81 ? 3'h2 : _ctrlSignals_T_633; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_635 = _ctrlSignals_T_79 ? 3'h1 : _ctrlSignals_T_634; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_636 = _ctrlSignals_T_77 ? 3'h0 : _ctrlSignals_T_635; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_637 = _ctrlSignals_T_75 ? 3'h0 : _ctrlSignals_T_636; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_638 = _ctrlSignals_T_73 ? 3'h0 : _ctrlSignals_T_637; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_639 = _ctrlSignals_T_71 ? 3'h0 : _ctrlSignals_T_638; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_640 = _ctrlSignals_T_69 ? 3'h0 : _ctrlSignals_T_639; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_641 = _ctrlSignals_T_67 ? 3'h0 : _ctrlSignals_T_640; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_642 = _ctrlSignals_T_65 ? 3'h0 : _ctrlSignals_T_641; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_643 = _ctrlSignals_T_63 ? 3'h0 : _ctrlSignals_T_642; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_644 = _ctrlSignals_T_61 ? 3'h0 : _ctrlSignals_T_643; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_645 = _ctrlSignals_T_59 ? 3'h0 : _ctrlSignals_T_644; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_646 = _ctrlSignals_T_57 ? 3'h0 : _ctrlSignals_T_645; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_647 = _ctrlSignals_T_55 ? 3'h0 : _ctrlSignals_T_646; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_648 = _ctrlSignals_T_53 ? 3'h0 : _ctrlSignals_T_647; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_649 = _ctrlSignals_T_51 ? 3'h0 : _ctrlSignals_T_648; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_650 = _ctrlSignals_T_49 ? 3'h0 : _ctrlSignals_T_649; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_651 = _ctrlSignals_T_47 ? 3'h0 : _ctrlSignals_T_650; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_652 = _ctrlSignals_T_45 ? 3'h0 : _ctrlSignals_T_651; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_653 = _ctrlSignals_T_43 ? 3'h0 : _ctrlSignals_T_652; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_654 = _ctrlSignals_T_41 ? 3'h0 : _ctrlSignals_T_653; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_655 = _ctrlSignals_T_39 ? 3'h0 : _ctrlSignals_T_654; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_656 = _ctrlSignals_T_37 ? 3'h0 : _ctrlSignals_T_655; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_657 = _ctrlSignals_T_35 ? 3'h0 : _ctrlSignals_T_656; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_658 = _ctrlSignals_T_33 ? 3'h0 : _ctrlSignals_T_657; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_659 = _ctrlSignals_T_31 ? 3'h0 : _ctrlSignals_T_658; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_660 = _ctrlSignals_T_29 ? 3'h0 : _ctrlSignals_T_659; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_661 = _ctrlSignals_T_27 ? 3'h0 : _ctrlSignals_T_660; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_662 = _ctrlSignals_T_25 ? 3'h0 : _ctrlSignals_T_661; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_663 = _ctrlSignals_T_23 ? 3'h0 : _ctrlSignals_T_662; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_664 = _ctrlSignals_T_21 ? 3'h0 : _ctrlSignals_T_663; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_665 = _ctrlSignals_T_19 ? 3'h0 : _ctrlSignals_T_664; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_666 = _ctrlSignals_T_17 ? 3'h0 : _ctrlSignals_T_665; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_667 = _ctrlSignals_T_15 ? 3'h0 : _ctrlSignals_T_666; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_668 = _ctrlSignals_T_13 ? 3'h0 : _ctrlSignals_T_667; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_669 = _ctrlSignals_T_11 ? 3'h0 : _ctrlSignals_T_668; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_670 = _ctrlSignals_T_9 ? 3'h0 : _ctrlSignals_T_669; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_671 = _ctrlSignals_T_7 ? 3'h0 : _ctrlSignals_T_670; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_672 = _ctrlSignals_T_5 ? 3'h0 : _ctrlSignals_T_671; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_673 = _ctrlSignals_T_3 ? 3'h0 : _ctrlSignals_T_672; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_674 = _ctrlSignals_T_97 ? 1'h0 : 1'h1; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_675 = _ctrlSignals_T_95 ? 1'h0 : _ctrlSignals_T_674; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_676 = _ctrlSignals_T_93 ? 1'h0 : _ctrlSignals_T_675; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_677 = _ctrlSignals_T_91 ? 1'h0 : _ctrlSignals_T_676; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_678 = _ctrlSignals_T_89 ? 1'h0 : _ctrlSignals_T_677; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_679 = _ctrlSignals_T_87 ? 1'h0 : _ctrlSignals_T_678; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_680 = _ctrlSignals_T_85 ? 1'h0 : _ctrlSignals_T_679; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_681 = _ctrlSignals_T_83 ? 1'h0 : _ctrlSignals_T_680; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_682 = _ctrlSignals_T_81 ? 1'h0 : _ctrlSignals_T_681; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_683 = _ctrlSignals_T_79 ? 1'h0 : _ctrlSignals_T_682; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_684 = _ctrlSignals_T_77 ? 1'h0 : _ctrlSignals_T_683; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_685 = _ctrlSignals_T_75 ? 1'h0 : _ctrlSignals_T_684; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_686 = _ctrlSignals_T_73 ? 1'h0 : _ctrlSignals_T_685; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_687 = _ctrlSignals_T_71 ? 1'h0 : _ctrlSignals_T_686; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_688 = _ctrlSignals_T_69 ? 1'h0 : _ctrlSignals_T_687; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_689 = _ctrlSignals_T_67 ? 1'h0 : _ctrlSignals_T_688; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_690 = _ctrlSignals_T_65 ? 1'h0 : _ctrlSignals_T_689; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_691 = _ctrlSignals_T_63 ? 1'h0 : _ctrlSignals_T_690; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_692 = _ctrlSignals_T_61 ? 1'h0 : _ctrlSignals_T_691; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_693 = _ctrlSignals_T_59 ? 1'h0 : _ctrlSignals_T_692; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_694 = _ctrlSignals_T_57 ? 1'h0 : _ctrlSignals_T_693; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_695 = _ctrlSignals_T_55 ? 1'h0 : _ctrlSignals_T_694; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_696 = _ctrlSignals_T_53 ? 1'h0 : _ctrlSignals_T_695; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_697 = _ctrlSignals_T_51 ? 1'h0 : _ctrlSignals_T_696; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_698 = _ctrlSignals_T_49 ? 1'h0 : _ctrlSignals_T_697; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_699 = _ctrlSignals_T_47 ? 1'h0 : _ctrlSignals_T_698; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_700 = _ctrlSignals_T_45 ? 1'h0 : _ctrlSignals_T_699; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_701 = _ctrlSignals_T_43 ? 1'h0 : _ctrlSignals_T_700; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_702 = _ctrlSignals_T_41 ? 1'h0 : _ctrlSignals_T_701; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_703 = _ctrlSignals_T_39 ? 1'h0 : _ctrlSignals_T_702; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_704 = _ctrlSignals_T_37 ? 1'h0 : _ctrlSignals_T_703; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_705 = _ctrlSignals_T_35 ? 1'h0 : _ctrlSignals_T_704; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_706 = _ctrlSignals_T_33 ? 1'h0 : _ctrlSignals_T_705; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_707 = _ctrlSignals_T_31 ? 1'h0 : _ctrlSignals_T_706; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_708 = _ctrlSignals_T_29 ? 1'h0 : _ctrlSignals_T_707; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_709 = _ctrlSignals_T_27 ? 1'h0 : _ctrlSignals_T_708; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_710 = _ctrlSignals_T_25 ? 1'h0 : _ctrlSignals_T_709; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_711 = _ctrlSignals_T_23 ? 1'h0 : _ctrlSignals_T_710; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_712 = _ctrlSignals_T_21 ? 1'h0 : _ctrlSignals_T_711; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_713 = _ctrlSignals_T_19 ? 1'h0 : _ctrlSignals_T_712; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_714 = _ctrlSignals_T_17 ? 1'h0 : _ctrlSignals_T_713; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_715 = _ctrlSignals_T_15 ? 1'h0 : _ctrlSignals_T_714; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_716 = _ctrlSignals_T_13 ? 1'h0 : _ctrlSignals_T_715; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_717 = _ctrlSignals_T_11 ? 1'h0 : _ctrlSignals_T_716; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_718 = _ctrlSignals_T_9 ? 1'h0 : _ctrlSignals_T_717; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_719 = _ctrlSignals_T_7 ? 1'h0 : _ctrlSignals_T_718; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_720 = _ctrlSignals_T_5 ? 1'h0 : _ctrlSignals_T_719; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  wire  _ctrlSignals_T_721 = _ctrlSignals_T_3 ? 1'h0 : _ctrlSignals_T_720; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  assign io_pc_sel = _ctrlSignals_T_1 ? 2'h0 : _ctrlSignals_T_145; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  assign io_inst_kill = _ctrlSignals_T_1 ? 1'h0 : _ctrlSignals_T_433; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  assign io_A_sel = _ctrlSignals_T_1 ? 1'h0 : _ctrlSignals_T_193; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  assign io_B_sel = _ctrlSignals_T_1 ? 1'h0 : _ctrlSignals_T_241; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  assign io_imm_sel = _ctrlSignals_T_1 ? 3'h3 : _ctrlSignals_T_289; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  assign io_alu_op = _ctrlSignals_T_1 ? 4'hb : _ctrlSignals_T_337; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  assign io_br_type = _ctrlSignals_T_1 ? 3'h0 : _ctrlSignals_T_385; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  assign io_st_type = _ctrlSignals_T_1 ? 2'h0 : _ctrlSignals_T_481; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  assign io_ld_type = _ctrlSignals_T_1 ? 3'h0 : _ctrlSignals_T_529; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  assign io_wb_sel = _ctrlSignals_T_1 ? 2'h0 : _ctrlSignals_T_577; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  assign io_wb_en = _ctrlSignals_T_1 | (_ctrlSignals_T_3 | (_ctrlSignals_T_5 | (_ctrlSignals_T_7 | _ctrlSignals_T_622)))
    ; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  assign io_csr_cmd = _ctrlSignals_T_1 ? 3'h0 : _ctrlSignals_T_673; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
  assign io_illegal = _ctrlSignals_T_1 ? 1'h0 : _ctrlSignals_T_721; // @[src/main/scala/chisel3/util/Lookup.scala 34:39]
endmodule
module Core(
  input         clock,
  input         reset,
  output [31:0] io_icache_req_bits_addr, // @[src/main/scala/mini/Core.scala 97:14]
  input         io_icache_resp_valid, // @[src/main/scala/mini/Core.scala 97:14]
  input  [31:0] io_icache_resp_bits_data, // @[src/main/scala/mini/Core.scala 97:14]
  output        io_dcache_req_valid, // @[src/main/scala/mini/Core.scala 97:14]
  output [31:0] io_dcache_req_bits_addr, // @[src/main/scala/mini/Core.scala 97:14]
  output [31:0] io_dcache_req_bits_data, // @[src/main/scala/mini/Core.scala 97:14]
  output [3:0]  io_dcache_req_bits_mask, // @[src/main/scala/mini/Core.scala 97:14]
  input         io_dcache_resp_valid, // @[src/main/scala/mini/Core.scala 97:14]
  input  [31:0] io_dcache_resp_bits_data, // @[src/main/scala/mini/Core.scala 97:14]
  output        rvfi_valid, // @[src/main/scala/mini/Core.scala 100:18]
  output [63:0] rvfi_order, // @[src/main/scala/mini/Core.scala 100:18]
  output [31:0] rvfi_insn, // @[src/main/scala/mini/Core.scala 100:18]
  output        rvfi_trap, // @[src/main/scala/mini/Core.scala 100:18]
  output [4:0]  rvfi_rs1_addr, // @[src/main/scala/mini/Core.scala 100:18]
  output [4:0]  rvfi_rs2_addr, // @[src/main/scala/mini/Core.scala 100:18]
  output [31:0] rvfi_rs1_rdata, // @[src/main/scala/mini/Core.scala 100:18]
  output [31:0] rvfi_rs2_rdata, // @[src/main/scala/mini/Core.scala 100:18]
  output [4:0]  rvfi_rd_addr, // @[src/main/scala/mini/Core.scala 100:18]
  output [31:0] rvfi_rd_wdata, // @[src/main/scala/mini/Core.scala 100:18]
  output [31:0] rvfi_pc_rdata, // @[src/main/scala/mini/Core.scala 100:18]
  output [31:0] rvfi_pc_wdata, // @[src/main/scala/mini/Core.scala 100:18]
  output [31:0] rvfi_mem_addr, // @[src/main/scala/mini/Core.scala 100:18]
  output [3:0]  rvfi_mem_rmask, // @[src/main/scala/mini/Core.scala 100:18]
  output [3:0]  rvfi_mem_wmask, // @[src/main/scala/mini/Core.scala 100:18]
  output [31:0] rvfi_mem_rdata, // @[src/main/scala/mini/Core.scala 100:18]
  output [31:0] rvfi_mem_wdata, // @[src/main/scala/mini/Core.scala 100:18]
  output        _T_4_0
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
`endif // RANDOMIZE_REG_INIT
  wire  dpath_clock; // @[src/main/scala/mini/Core.scala 98:21]
  wire  dpath_reset; // @[src/main/scala/mini/Core.scala 98:21]
  wire [31:0] dpath_io__icache_req_bits_addr; // @[src/main/scala/mini/Core.scala 98:21]
  wire  dpath_io__icache_resp_valid; // @[src/main/scala/mini/Core.scala 98:21]
  wire [31:0] dpath_io__icache_resp_bits_data; // @[src/main/scala/mini/Core.scala 98:21]
  wire  dpath_io__dcache_req_valid; // @[src/main/scala/mini/Core.scala 98:21]
  wire [31:0] dpath_io__dcache_req_bits_addr; // @[src/main/scala/mini/Core.scala 98:21]
  wire [31:0] dpath_io__dcache_req_bits_data; // @[src/main/scala/mini/Core.scala 98:21]
  wire [3:0] dpath_io__dcache_req_bits_mask; // @[src/main/scala/mini/Core.scala 98:21]
  wire  dpath_io__dcache_resp_valid; // @[src/main/scala/mini/Core.scala 98:21]
  wire [31:0] dpath_io__dcache_resp_bits_data; // @[src/main/scala/mini/Core.scala 98:21]
  wire [31:0] dpath_io__ctrl_inst; // @[src/main/scala/mini/Core.scala 98:21]
  wire [1:0] dpath_io__ctrl_pc_sel; // @[src/main/scala/mini/Core.scala 98:21]
  wire  dpath_io__ctrl_inst_kill; // @[src/main/scala/mini/Core.scala 98:21]
  wire  dpath_io__ctrl_A_sel; // @[src/main/scala/mini/Core.scala 98:21]
  wire  dpath_io__ctrl_B_sel; // @[src/main/scala/mini/Core.scala 98:21]
  wire [2:0] dpath_io__ctrl_imm_sel; // @[src/main/scala/mini/Core.scala 98:21]
  wire [3:0] dpath_io__ctrl_alu_op; // @[src/main/scala/mini/Core.scala 98:21]
  wire [2:0] dpath_io__ctrl_br_type; // @[src/main/scala/mini/Core.scala 98:21]
  wire [1:0] dpath_io__ctrl_st_type; // @[src/main/scala/mini/Core.scala 98:21]
  wire [2:0] dpath_io__ctrl_ld_type; // @[src/main/scala/mini/Core.scala 98:21]
  wire [1:0] dpath_io__ctrl_wb_sel; // @[src/main/scala/mini/Core.scala 98:21]
  wire  dpath_io__ctrl_wb_en; // @[src/main/scala/mini/Core.scala 98:21]
  wire [2:0] dpath_io__ctrl_csr_cmd; // @[src/main/scala/mini/Core.scala 98:21]
  wire  dpath_io__ctrl_illegal; // @[src/main/scala/mini/Core.scala 98:21]
  wire [4:0] dpath__T_18_0; // @[src/main/scala/mini/Core.scala 98:21]
  wire [32:0] dpath__T_9_0; // @[src/main/scala/mini/Core.scala 98:21]
  wire [31:0] dpath_REG_2_0; // @[src/main/scala/mini/Core.scala 98:21]
  wire  dpath_io_expt; // @[src/main/scala/mini/Core.scala 98:21]
  wire [31:0] dpath_ew_reg_inst; // @[src/main/scala/mini/Core.scala 98:21]
  wire [34:0] dpath_REG_4_0; // @[src/main/scala/mini/Core.scala 98:21]
  wire [4:0] dpath_flywire_rs2_addr_0; // @[src/main/scala/mini/Core.scala 98:21]
  wire [31:0] dpath_ew_reg_pc; // @[src/main/scala/mini/Core.scala 98:21]
  wire  dpath__T_8_0; // @[src/main/scala/mini/Core.scala 98:21]
  wire [63:0] dpath_instOrder_0; // @[src/main/scala/mini/Core.scala 98:21]
  wire [31:0] dpath_io_dcache_resp_bits_data; // @[src/main/scala/mini/Core.scala 98:21]
  wire [4:0] dpath_load_mask_0; // @[src/main/scala/mini/Core.scala 98:21]
  wire [31:0] dpath_mem_wdata_0; // @[src/main/scala/mini/Core.scala 98:21]
  wire [4:0] dpath_flywire_rs1_addr_0; // @[src/main/scala/mini/Core.scala 98:21]
  wire  dpath_instCommit_0; // @[src/main/scala/mini/Core.scala 98:21]
  wire [31:0] dpath_REG_3_0; // @[src/main/scala/mini/Core.scala 98:21]
  wire [32:0] dpath_regWrite_0; // @[src/main/scala/mini/Core.scala 98:21]
  wire [3:0] dpath_mem_wmask_0; // @[src/main/scala/mini/Core.scala 98:21]
  wire [31:0] ctrl_io_inst; // @[src/main/scala/mini/Core.scala 99:20]
  wire [1:0] ctrl_io_pc_sel; // @[src/main/scala/mini/Core.scala 99:20]
  wire  ctrl_io_inst_kill; // @[src/main/scala/mini/Core.scala 99:20]
  wire  ctrl_io_A_sel; // @[src/main/scala/mini/Core.scala 99:20]
  wire  ctrl_io_B_sel; // @[src/main/scala/mini/Core.scala 99:20]
  wire [2:0] ctrl_io_imm_sel; // @[src/main/scala/mini/Core.scala 99:20]
  wire [3:0] ctrl_io_alu_op; // @[src/main/scala/mini/Core.scala 99:20]
  wire [2:0] ctrl_io_br_type; // @[src/main/scala/mini/Core.scala 99:20]
  wire [1:0] ctrl_io_st_type; // @[src/main/scala/mini/Core.scala 99:20]
  wire [2:0] ctrl_io_ld_type; // @[src/main/scala/mini/Core.scala 99:20]
  wire [1:0] ctrl_io_wb_sel; // @[src/main/scala/mini/Core.scala 99:20]
  wire  ctrl_io_wb_en; // @[src/main/scala/mini/Core.scala 99:20]
  wire [2:0] ctrl_io_csr_cmd; // @[src/main/scala/mini/Core.scala 99:20]
  wire  ctrl_io_illegal; // @[src/main/scala/mini/Core.scala 99:20]
  wire [4:0] rvfiio_rs1_addr = dpath_flywire_rs1_addr_0;
  wire [31:0] rvfiio_rs1_rdata = dpath_REG_2_0;
  wire [4:0] rvfiio_rs2_addr = dpath_flywire_rs2_addr_0;
  wire [31:0] rvfiio_rs2_rdata = dpath_REG_3_0;
  wire [4:0] rvfiio_rd_addr = dpath__T_18_0;
  wire [32:0] rvfiio_rd_wdata = dpath_regWrite_0;
  wire [31:0] rd_wdata_ssd = rvfiio_rd_wdata[31:0]; // @[src/main/scala/mini/Core.scala 132:27]
  wire [31:0] _rvfi_pc_wdata_T_1 = rvfi_pc_rdata + 32'h4; // @[src/main/scala/mini/Core.scala 203:36]
  wire  Jumpornot = dpath__T_8_0;
  wire [32:0] rvfiio_pc_jump_data = dpath__T_9_0;
  wire [31:0] pc_wdata_test = rvfiio_pc_jump_data[31:0]; // @[src/main/scala/mini/Core.scala 191:27]
  wire [31:0] _tmpAssume_T_1 = rvfi_insn & 32'h707f; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_2 = 32'h13 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_4 = 32'h2013 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_6 = 32'h3013 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_8 = 32'h7013 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_10 = 32'h6013 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_12 = 32'h4013 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire [31:0] _tmpAssume_T_13 = rvfi_insn & 32'hfe00707f; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 29:12]
  wire  _tmpAssume_T_14 = 32'h1013 == _tmpAssume_T_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 29:12]
  wire  _tmpAssume_T_16 = 32'h5013 == _tmpAssume_T_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 29:12]
  wire  _tmpAssume_T_18 = 32'h40005013 == _tmpAssume_T_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 29:12]
  wire [31:0] _tmpAssume_T_19 = rvfi_insn & 32'h7f; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_20 = 32'h37 == _tmpAssume_T_19; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_22 = 32'h17 == _tmpAssume_T_19; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_32 = _tmpAssume_T_2 | _tmpAssume_T_4 | _tmpAssume_T_6 | _tmpAssume_T_8 | _tmpAssume_T_10 |
    _tmpAssume_T_12 | _tmpAssume_T_14 | _tmpAssume_T_16 | _tmpAssume_T_18 | _tmpAssume_T_20 | _tmpAssume_T_22; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/AssumeHelper.scala 30:53]
  wire  _tmpAssume_T_34 = 32'h33 == _tmpAssume_T_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_36 = 32'h2033 == _tmpAssume_T_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_38 = 32'h3033 == _tmpAssume_T_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_40 = 32'h7033 == _tmpAssume_T_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_42 = 32'h6033 == _tmpAssume_T_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_44 = 32'h4033 == _tmpAssume_T_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_46 = 32'h1033 == _tmpAssume_T_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_48 = 32'h5033 == _tmpAssume_T_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_50 = 32'h40000033 == _tmpAssume_T_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_52 = 32'h40005033 == _tmpAssume_T_13; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_61 = _tmpAssume_T_34 | _tmpAssume_T_36 | _tmpAssume_T_38 | _tmpAssume_T_40 | _tmpAssume_T_42 |
    _tmpAssume_T_44 | _tmpAssume_T_46 | _tmpAssume_T_48 | _tmpAssume_T_50 | _tmpAssume_T_52; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/AssumeHelper.scala 30:53]
  wire  _tmpAssume_T_62 = _tmpAssume_T_32 | _tmpAssume_T_61; // @[src/main/scala/mini/Core.scala 208:7]
  wire  _tmpAssume_T_64 = 32'h6f == _tmpAssume_T_19; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_66 = 32'h67 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_68 = 32'h63 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_70 = 32'h1063 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_72 = 32'h4063 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_74 = 32'h6063 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_76 = 32'h5063 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_78 = 32'h7063 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_85 = _tmpAssume_T_64 | _tmpAssume_T_66 | _tmpAssume_T_68 | _tmpAssume_T_70 | _tmpAssume_T_72 |
    _tmpAssume_T_74 | _tmpAssume_T_76 | _tmpAssume_T_78; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/AssumeHelper.scala 30:53]
  wire  _tmpAssume_T_86 = _tmpAssume_T_62 | _tmpAssume_T_85; // @[src/main/scala/mini/Core.scala 209:7]
  wire  _tmpAssume_T_88 = 32'h3 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_90 = 32'h1003 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_92 = 32'h2003 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_94 = 32'h4003 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_96 = 32'h5003 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_98 = 32'h23 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_100 = 32'h1023 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_102 = 32'h2023 == _tmpAssume_T_1; // @[riscv-spec-core/src/main/scala/rvspeccore/core/spec/RVInsts.scala 12:39]
  wire  _tmpAssume_T_109 = _tmpAssume_T_88 | _tmpAssume_T_90 | _tmpAssume_T_92 | _tmpAssume_T_94 | _tmpAssume_T_96 |
    _tmpAssume_T_98 | _tmpAssume_T_100 | _tmpAssume_T_102; // @[riscv-spec-core/src/main/scala/rvspeccore/checker/AssumeHelper.scala 30:53]
  wire  _tmpAssume_T_110 = _tmpAssume_T_86 | _tmpAssume_T_109; // @[src/main/scala/mini/Core.scala 210:7]
  wire  tmpAssume = ~rvfi_valid | _tmpAssume_T_110; // @[src/main/scala/mini/Core.scala 206:31]
  wire  _T_4 = tmpAssume & ~rvfi_trap; // @[src/main/scala/mini/Core.scala 213:35]
  reg  REG; // @[src/main/scala/mini/Core.scala 223:14]
  wire [4:0] rvfiio_mem_rmask = dpath_load_mask_0;
  wire [31:0] rvfiio_mem_rdata = dpath_io_dcache_resp_bits_data;
  wire [34:0] rvfiio_mem_addr = dpath_REG_4_0;
  Datapath dpath ( // @[src/main/scala/mini/Core.scala 98:21]
    .clock(dpath_clock),
    .reset(dpath_reset),
    .io__icache_req_bits_addr(dpath_io__icache_req_bits_addr),
    .io__icache_resp_valid(dpath_io__icache_resp_valid),
    .io__icache_resp_bits_data(dpath_io__icache_resp_bits_data),
    .io__dcache_req_valid(dpath_io__dcache_req_valid),
    .io__dcache_req_bits_addr(dpath_io__dcache_req_bits_addr),
    .io__dcache_req_bits_data(dpath_io__dcache_req_bits_data),
    .io__dcache_req_bits_mask(dpath_io__dcache_req_bits_mask),
    .io__dcache_resp_valid(dpath_io__dcache_resp_valid),
    .io__dcache_resp_bits_data(dpath_io__dcache_resp_bits_data),
    .io__ctrl_inst(dpath_io__ctrl_inst),
    .io__ctrl_pc_sel(dpath_io__ctrl_pc_sel),
    .io__ctrl_inst_kill(dpath_io__ctrl_inst_kill),
    .io__ctrl_A_sel(dpath_io__ctrl_A_sel),
    .io__ctrl_B_sel(dpath_io__ctrl_B_sel),
    .io__ctrl_imm_sel(dpath_io__ctrl_imm_sel),
    .io__ctrl_alu_op(dpath_io__ctrl_alu_op),
    .io__ctrl_br_type(dpath_io__ctrl_br_type),
    .io__ctrl_st_type(dpath_io__ctrl_st_type),
    .io__ctrl_ld_type(dpath_io__ctrl_ld_type),
    .io__ctrl_wb_sel(dpath_io__ctrl_wb_sel),
    .io__ctrl_wb_en(dpath_io__ctrl_wb_en),
    .io__ctrl_csr_cmd(dpath_io__ctrl_csr_cmd),
    .io__ctrl_illegal(dpath_io__ctrl_illegal),
    ._T_18_0(dpath__T_18_0),
    ._T_9_0(dpath__T_9_0),
    .REG_2_0(dpath_REG_2_0),
    .io_expt(dpath_io_expt),
    .ew_reg_inst(dpath_ew_reg_inst),
    .REG_4_0(dpath_REG_4_0),
    .flywire_rs2_addr_0(dpath_flywire_rs2_addr_0),
    .ew_reg_pc(dpath_ew_reg_pc),
    ._T_8_0(dpath__T_8_0),
    .instOrder_0(dpath_instOrder_0),
    .io_dcache_resp_bits_data(dpath_io_dcache_resp_bits_data),
    .load_mask_0(dpath_load_mask_0),
    .mem_wdata_0(dpath_mem_wdata_0),
    .flywire_rs1_addr_0(dpath_flywire_rs1_addr_0),
    .instCommit_0(dpath_instCommit_0),
    .REG_3_0(dpath_REG_3_0),
    .regWrite_0(dpath_regWrite_0),
    .mem_wmask_0(dpath_mem_wmask_0)
  );
  Control ctrl ( // @[src/main/scala/mini/Core.scala 99:20]
    .io_inst(ctrl_io_inst),
    .io_pc_sel(ctrl_io_pc_sel),
    .io_inst_kill(ctrl_io_inst_kill),
    .io_A_sel(ctrl_io_A_sel),
    .io_B_sel(ctrl_io_B_sel),
    .io_imm_sel(ctrl_io_imm_sel),
    .io_alu_op(ctrl_io_alu_op),
    .io_br_type(ctrl_io_br_type),
    .io_st_type(ctrl_io_st_type),
    .io_ld_type(ctrl_io_ld_type),
    .io_wb_sel(ctrl_io_wb_sel),
    .io_wb_en(ctrl_io_wb_en),
    .io_csr_cmd(ctrl_io_csr_cmd),
    .io_illegal(ctrl_io_illegal)
  );
  assign io_icache_req_bits_addr = dpath_io__icache_req_bits_addr; // @[src/main/scala/mini/Core.scala 103:19]
  assign io_dcache_req_valid = dpath_io__dcache_req_valid; // @[src/main/scala/mini/Core.scala 104:19]
  assign io_dcache_req_bits_addr = dpath_io__dcache_req_bits_addr; // @[src/main/scala/mini/Core.scala 104:19]
  assign io_dcache_req_bits_data = dpath_io__dcache_req_bits_data; // @[src/main/scala/mini/Core.scala 104:19]
  assign io_dcache_req_bits_mask = dpath_io__dcache_req_bits_mask; // @[src/main/scala/mini/Core.scala 104:19]
  assign rvfi_valid = dpath_instCommit_0; // @[src/main/scala/mini/Core.scala 108:22]
  assign rvfi_order = dpath_instOrder_0; // @[src/main/scala/mini/Core.scala 108:22]
  assign rvfi_insn = dpath_ew_reg_inst; // @[src/main/scala/mini/Core.scala 110:25]
  assign rvfi_trap = dpath_io_expt; // @[src/main/scala/mini/Core.scala 108:22]
  assign rvfi_rs1_addr = dpath_flywire_rs1_addr_0; // @[src/main/scala/mini/Core.scala 127:35]
  assign rvfi_rs2_addr = dpath_flywire_rs2_addr_0; // @[src/main/scala/mini/Core.scala 128:35]
  assign rvfi_rs1_rdata = rvfiio_rs1_addr == 5'h0 ? 32'h0 : rvfiio_rs1_rdata; // @[src/main/scala/mini/Core.scala 149:38 150:24 152:24]
  assign rvfi_rs2_rdata = rvfiio_rs2_addr == 5'h0 ? 32'h0 : rvfiio_rs2_rdata; // @[src/main/scala/mini/Core.scala 154:38 155:24 157:24]
  assign rvfi_rd_addr = dpath__T_18_0; // @[src/main/scala/mini/Core.scala 108:22]
  assign rvfi_rd_wdata = rvfiio_rd_addr == 5'h0 ? 32'h0 : rd_wdata_ssd; // @[src/main/scala/mini/Core.scala 160:37 161:23 163:23]
  assign rvfi_pc_rdata = dpath_ew_reg_pc; // @[src/main/scala/mini/Core.scala 108:22]
  assign rvfi_pc_wdata = Jumpornot ? pc_wdata_test : _rvfi_pc_wdata_T_1; // @[src/main/scala/mini/Core.scala 200:16 201:19 203:19]
  assign rvfi_mem_addr = rvfiio_mem_addr[31:0]; // @[src/main/scala/mini/Core.scala 108:22]
  assign rvfi_mem_rmask = rvfiio_mem_rmask[3:0]; // @[src/main/scala/mini/Core.scala 108:22]
  assign rvfi_mem_wmask = dpath_mem_wmask_0; // @[src/main/scala/mini/Core.scala 108:22]
  assign rvfi_mem_rdata = dpath_io_dcache_resp_bits_data; // @[src/main/scala/mini/Core.scala 108:22]
  assign rvfi_mem_wdata = dpath_mem_wdata_0; // @[src/main/scala/mini/Core.scala 108:22]
  assign _T_4_0 = _T_4;
  assign dpath_clock = clock;
  assign dpath_reset = reset;
  assign dpath_io__icache_resp_valid = io_icache_resp_valid; // @[src/main/scala/mini/Core.scala 103:19]
  assign dpath_io__icache_resp_bits_data = io_icache_resp_bits_data; // @[src/main/scala/mini/Core.scala 103:19]
  assign dpath_io__dcache_resp_valid = io_dcache_resp_valid; // @[src/main/scala/mini/Core.scala 104:19]
  assign dpath_io__dcache_resp_bits_data = io_dcache_resp_bits_data; // @[src/main/scala/mini/Core.scala 104:19]
  assign dpath_io__ctrl_pc_sel = ctrl_io_pc_sel; // @[src/main/scala/mini/Core.scala 105:17]
  assign dpath_io__ctrl_inst_kill = ctrl_io_inst_kill; // @[src/main/scala/mini/Core.scala 105:17]
  assign dpath_io__ctrl_A_sel = ctrl_io_A_sel; // @[src/main/scala/mini/Core.scala 105:17]
  assign dpath_io__ctrl_B_sel = ctrl_io_B_sel; // @[src/main/scala/mini/Core.scala 105:17]
  assign dpath_io__ctrl_imm_sel = ctrl_io_imm_sel; // @[src/main/scala/mini/Core.scala 105:17]
  assign dpath_io__ctrl_alu_op = ctrl_io_alu_op; // @[src/main/scala/mini/Core.scala 105:17]
  assign dpath_io__ctrl_br_type = ctrl_io_br_type; // @[src/main/scala/mini/Core.scala 105:17]
  assign dpath_io__ctrl_st_type = ctrl_io_st_type; // @[src/main/scala/mini/Core.scala 105:17]
  assign dpath_io__ctrl_ld_type = ctrl_io_ld_type; // @[src/main/scala/mini/Core.scala 105:17]
  assign dpath_io__ctrl_wb_sel = ctrl_io_wb_sel; // @[src/main/scala/mini/Core.scala 105:17]
  assign dpath_io__ctrl_wb_en = ctrl_io_wb_en; // @[src/main/scala/mini/Core.scala 105:17]
  assign dpath_io__ctrl_csr_cmd = ctrl_io_csr_cmd; // @[src/main/scala/mini/Core.scala 105:17]
  assign dpath_io__ctrl_illegal = ctrl_io_illegal; // @[src/main/scala/mini/Core.scala 105:17]
  assign ctrl_io_inst = dpath_io__ctrl_inst; // @[src/main/scala/mini/Core.scala 105:17]
  always @(posedge clock) begin
    if (reset) begin // @[src/main/scala/mini/Core.scala 223:14]
      REG <= 1'h0; // @[src/main/scala/mini/Core.scala 223:14]
    end else begin
      REG <= rvfi_trap; // @[src/main/scala/mini/Core.scala 223:14]
    end
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (rvfi_valid & ~reset) begin
          $fwrite(32'h80000002,
            "[RVFI Print%x][trapnext:%x][Expt:%x]Mem_rmask:%x, ADDR:%x Core: valid=%d order=%x insn=%x Jump:%d JumpTarget: %x rd_addr=%d rd_data=%x rs1_addr=%d rs1_data=%x rs2_addr=%d rs2_data=%x PCr=%x, PCw=%x\n"
            ,rvfiio_mem_rdata,REG,rvfi_trap,rvfi_mem_rmask,rvfi_mem_addr,rvfi_valid,rvfi_order,rvfi_insn,Jumpornot,
            rvfi_pc_wdata,rvfi_rd_addr,rvfi_rd_wdata,rvfi_rs1_addr,rvfi_rs1_rdata,rvfi_rs2_addr,rvfi_rs2_rdata,
            rvfi_pc_rdata,rvfi_pc_wdata); // @[src/main/scala/mini/Core.scala 218:11]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  REG = _RAND_0[0:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module CoreSoc(
  input         clock,
  input         reset,
  output        rvfi_valid, // @[src/main/scala/mini/Core.scala 45:18]
  output [63:0] rvfi_order, // @[src/main/scala/mini/Core.scala 45:18]
  output [31:0] rvfi_insn, // @[src/main/scala/mini/Core.scala 45:18]
  output        rvfi_trap, // @[src/main/scala/mini/Core.scala 45:18]
  output        rvfi_halt, // @[src/main/scala/mini/Core.scala 45:18]
  output        rvfi_intr, // @[src/main/scala/mini/Core.scala 45:18]
  output [1:0]  rvfi_mode, // @[src/main/scala/mini/Core.scala 45:18]
  output [1:0]  rvfi_ixl, // @[src/main/scala/mini/Core.scala 45:18]
  output [4:0]  rvfi_rs1_addr, // @[src/main/scala/mini/Core.scala 45:18]
  output [4:0]  rvfi_rs2_addr, // @[src/main/scala/mini/Core.scala 45:18]
  output [31:0] rvfi_rs1_rdata, // @[src/main/scala/mini/Core.scala 45:18]
  output [31:0] rvfi_rs2_rdata, // @[src/main/scala/mini/Core.scala 45:18]
  output [4:0]  rvfi_rd_addr, // @[src/main/scala/mini/Core.scala 45:18]
  output [31:0] rvfi_rd_wdata, // @[src/main/scala/mini/Core.scala 45:18]
  output [31:0] rvfi_pc_rdata, // @[src/main/scala/mini/Core.scala 45:18]
  output [31:0] rvfi_pc_wdata, // @[src/main/scala/mini/Core.scala 45:18]
  output [31:0] rvfi_mem_addr, // @[src/main/scala/mini/Core.scala 45:18]
  output [3:0]  rvfi_mem_rmask, // @[src/main/scala/mini/Core.scala 45:18]
  output [3:0]  rvfi_mem_wmask, // @[src/main/scala/mini/Core.scala 45:18]
  output [31:0] rvfi_mem_rdata, // @[src/main/scala/mini/Core.scala 45:18]
  output [31:0] rvfi_mem_wdata // @[src/main/scala/mini/Core.scala 45:18]
);
`ifdef RANDOMIZE_MEM_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
`endif // RANDOMIZE_MEM_INIT
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
`endif // RANDOMIZE_REG_INIT
  wire  dut_clock; // @[src/main/scala/mini/Core.scala 46:19]
  wire  dut_reset; // @[src/main/scala/mini/Core.scala 46:19]
  wire [31:0] dut_io_icache_req_bits_addr; // @[src/main/scala/mini/Core.scala 46:19]
  wire  dut_io_icache_resp_valid; // @[src/main/scala/mini/Core.scala 46:19]
  wire [31:0] dut_io_icache_resp_bits_data; // @[src/main/scala/mini/Core.scala 46:19]
  wire  dut_io_dcache_req_valid; // @[src/main/scala/mini/Core.scala 46:19]
  wire [31:0] dut_io_dcache_req_bits_addr; // @[src/main/scala/mini/Core.scala 46:19]
  wire [31:0] dut_io_dcache_req_bits_data; // @[src/main/scala/mini/Core.scala 46:19]
  wire [3:0] dut_io_dcache_req_bits_mask; // @[src/main/scala/mini/Core.scala 46:19]
  wire  dut_io_dcache_resp_valid; // @[src/main/scala/mini/Core.scala 46:19]
  wire [31:0] dut_io_dcache_resp_bits_data; // @[src/main/scala/mini/Core.scala 46:19]
  wire  dut_rvfi_valid; // @[src/main/scala/mini/Core.scala 46:19]
  wire [63:0] dut_rvfi_order; // @[src/main/scala/mini/Core.scala 46:19]
  wire [31:0] dut_rvfi_insn; // @[src/main/scala/mini/Core.scala 46:19]
  wire  dut_rvfi_trap; // @[src/main/scala/mini/Core.scala 46:19]
  wire [4:0] dut_rvfi_rs1_addr; // @[src/main/scala/mini/Core.scala 46:19]
  wire [4:0] dut_rvfi_rs2_addr; // @[src/main/scala/mini/Core.scala 46:19]
  wire [31:0] dut_rvfi_rs1_rdata; // @[src/main/scala/mini/Core.scala 46:19]
  wire [31:0] dut_rvfi_rs2_rdata; // @[src/main/scala/mini/Core.scala 46:19]
  wire [4:0] dut_rvfi_rd_addr; // @[src/main/scala/mini/Core.scala 46:19]
  wire [31:0] dut_rvfi_rd_wdata; // @[src/main/scala/mini/Core.scala 46:19]
  wire [31:0] dut_rvfi_pc_rdata; // @[src/main/scala/mini/Core.scala 46:19]
  wire [31:0] dut_rvfi_pc_wdata; // @[src/main/scala/mini/Core.scala 46:19]
  wire [31:0] dut_rvfi_mem_addr; // @[src/main/scala/mini/Core.scala 46:19]
  wire [3:0] dut_rvfi_mem_rmask; // @[src/main/scala/mini/Core.scala 46:19]
  wire [3:0] dut_rvfi_mem_wmask; // @[src/main/scala/mini/Core.scala 46:19]
  wire [31:0] dut_rvfi_mem_rdata; // @[src/main/scala/mini/Core.scala 46:19]
  wire [31:0] dut_rvfi_mem_wdata; // @[src/main/scala/mini/Core.scala 46:19]
  wire  dut__T_4_0; // @[src/main/scala/mini/Core.scala 46:19]
  reg [31:0] imem [0:15]; // @[src/main/scala/mini/Core.scala 51:17]
  wire  imem_dut_io_icache_resp_bits_data_MPORT_en; // @[src/main/scala/mini/Core.scala 51:17]
  wire [3:0] imem_dut_io_icache_resp_bits_data_MPORT_addr; // @[src/main/scala/mini/Core.scala 51:17]
  wire [31:0] imem_dut_io_icache_resp_bits_data_MPORT_data; // @[src/main/scala/mini/Core.scala 51:17]
  reg [31:0] dmem [0:15]; // @[src/main/scala/mini/Core.scala 52:17]
  wire  dmem_write_MPORT_en; // @[src/main/scala/mini/Core.scala 52:17]
  wire [3:0] dmem_write_MPORT_addr; // @[src/main/scala/mini/Core.scala 52:17]
  wire [31:0] dmem_write_MPORT_data; // @[src/main/scala/mini/Core.scala 52:17]
  wire  dmem_write_MPORT_1_en; // @[src/main/scala/mini/Core.scala 52:17]
  wire [3:0] dmem_write_MPORT_1_addr; // @[src/main/scala/mini/Core.scala 52:17]
  wire [31:0] dmem_write_MPORT_1_data; // @[src/main/scala/mini/Core.scala 52:17]
  wire  dmem_write_MPORT_2_en; // @[src/main/scala/mini/Core.scala 52:17]
  wire [3:0] dmem_write_MPORT_2_addr; // @[src/main/scala/mini/Core.scala 52:17]
  wire [31:0] dmem_write_MPORT_2_data; // @[src/main/scala/mini/Core.scala 52:17]
  wire  dmem_write_MPORT_3_en; // @[src/main/scala/mini/Core.scala 52:17]
  wire [3:0] dmem_write_MPORT_3_addr; // @[src/main/scala/mini/Core.scala 52:17]
  wire [31:0] dmem_write_MPORT_3_data; // @[src/main/scala/mini/Core.scala 52:17]
  wire  dmem_dut_io_dcache_resp_bits_data_MPORT_en; // @[src/main/scala/mini/Core.scala 52:17]
  wire [3:0] dmem_dut_io_dcache_resp_bits_data_MPORT_addr; // @[src/main/scala/mini/Core.scala 52:17]
  wire [31:0] dmem_dut_io_dcache_resp_bits_data_MPORT_data; // @[src/main/scala/mini/Core.scala 52:17]
  wire [31:0] dmem_MPORT_data; // @[src/main/scala/mini/Core.scala 52:17]
  wire [3:0] dmem_MPORT_addr; // @[src/main/scala/mini/Core.scala 52:17]
  wire  dmem_MPORT_mask; // @[src/main/scala/mini/Core.scala 52:17]
  wire  dmem_MPORT_en; // @[src/main/scala/mini/Core.scala 52:17]
  wire [31:0] iaddr = dut_io_icache_req_bits_addr / 3'h4; // @[src/main/scala/mini/Core.scala 54:43]
  wire [31:0] daddr = dut_io_dcache_req_bits_addr / 3'h4; // @[src/main/scala/mini/Core.scala 55:43]
  wire  _write_T_1 = dut_io_dcache_req_valid & dut_io_dcache_req_bits_mask[0]; // @[src/main/scala/mini/Core.scala 59:34]
  wire [31:0] _write_T_4 = _write_T_1 ? dut_io_dcache_req_bits_data : dmem_write_MPORT_data; // @[src/main/scala/mini/Core.scala 58:11]
  wire [8:0] _write_T_6 = {{1'd0}, _write_T_4[7:0]}; // @[src/main/scala/mini/Core.scala 62:33]
  wire [31:0] _write_T_7 = {{23'd0}, _write_T_6}; // @[src/main/scala/mini/Core.scala 57:11]
  wire  _write_T_9 = dut_io_dcache_req_valid & dut_io_dcache_req_bits_mask[1]; // @[src/main/scala/mini/Core.scala 59:34]
  wire [31:0] _write_T_12 = _write_T_9 ? dut_io_dcache_req_bits_data : dmem_write_MPORT_1_data; // @[src/main/scala/mini/Core.scala 58:11]
  wire [15:0] _GEN_10 = {_write_T_12[15:8], 8'h0}; // @[src/main/scala/mini/Core.scala 62:33]
  wire [22:0] _write_T_14 = {{7'd0}, _GEN_10}; // @[src/main/scala/mini/Core.scala 62:33]
  wire [31:0] _GEN_11 = {{9'd0}, _write_T_14}; // @[src/main/scala/mini/Core.scala 57:11]
  wire [31:0] _write_T_15 = _write_T_7 | _GEN_11; // @[src/main/scala/mini/Core.scala 57:11]
  wire  _write_T_17 = dut_io_dcache_req_valid & dut_io_dcache_req_bits_mask[2]; // @[src/main/scala/mini/Core.scala 59:34]
  wire [31:0] _write_T_20 = _write_T_17 ? dut_io_dcache_req_bits_data : dmem_write_MPORT_2_data; // @[src/main/scala/mini/Core.scala 58:11]
  wire [23:0] _GEN_12 = {_write_T_20[23:16], 16'h0}; // @[src/main/scala/mini/Core.scala 62:33]
  wire [38:0] _write_T_22 = {{15'd0}, _GEN_12}; // @[src/main/scala/mini/Core.scala 62:33]
  wire [38:0] _GEN_13 = {{7'd0}, _write_T_15}; // @[src/main/scala/mini/Core.scala 57:11]
  wire [38:0] _write_T_23 = _GEN_13 | _write_T_22; // @[src/main/scala/mini/Core.scala 57:11]
  wire  _write_T_25 = dut_io_dcache_req_valid & dut_io_dcache_req_bits_mask[3]; // @[src/main/scala/mini/Core.scala 59:34]
  wire [31:0] _write_T_28 = _write_T_25 ? dut_io_dcache_req_bits_data : dmem_write_MPORT_3_data; // @[src/main/scala/mini/Core.scala 58:11]
  wire [31:0] _GEN_14 = {_write_T_28[31:24], 24'h0}; // @[src/main/scala/mini/Core.scala 62:33]
  wire [38:0] _write_T_30 = {{7'd0}, _GEN_14}; // @[src/main/scala/mini/Core.scala 62:33]
  wire [38:0] write = _write_T_23 | _write_T_30; // @[src/main/scala/mini/Core.scala 57:11]
  wire  _dut_io_icache_resp_valid_T_1 = ~reset; // @[src/main/scala/mini/Core.scala 64:31]
  reg [31:0] dut_io_icache_resp_bits_data_REG; // @[src/main/scala/mini/Core.scala 66:42]
  reg [31:0] dut_io_dcache_resp_bits_data_REG; // @[src/main/scala/mini/Core.scala 67:42]
  wire  _T = |dut_io_dcache_req_bits_mask; // @[src/main/scala/mini/Core.scala 69:38]
  wire  someassumeid = dut__T_4_0;
  Core dut ( // @[src/main/scala/mini/Core.scala 46:19]
    .clock(dut_clock),
    .reset(dut_reset),
    .io_icache_req_bits_addr(dut_io_icache_req_bits_addr),
    .io_icache_resp_valid(dut_io_icache_resp_valid),
    .io_icache_resp_bits_data(dut_io_icache_resp_bits_data),
    .io_dcache_req_valid(dut_io_dcache_req_valid),
    .io_dcache_req_bits_addr(dut_io_dcache_req_bits_addr),
    .io_dcache_req_bits_data(dut_io_dcache_req_bits_data),
    .io_dcache_req_bits_mask(dut_io_dcache_req_bits_mask),
    .io_dcache_resp_valid(dut_io_dcache_resp_valid),
    .io_dcache_resp_bits_data(dut_io_dcache_resp_bits_data),
    .rvfi_valid(dut_rvfi_valid),
    .rvfi_order(dut_rvfi_order),
    .rvfi_insn(dut_rvfi_insn),
    .rvfi_trap(dut_rvfi_trap),
    .rvfi_rs1_addr(dut_rvfi_rs1_addr),
    .rvfi_rs2_addr(dut_rvfi_rs2_addr),
    .rvfi_rs1_rdata(dut_rvfi_rs1_rdata),
    .rvfi_rs2_rdata(dut_rvfi_rs2_rdata),
    .rvfi_rd_addr(dut_rvfi_rd_addr),
    .rvfi_rd_wdata(dut_rvfi_rd_wdata),
    .rvfi_pc_rdata(dut_rvfi_pc_rdata),
    .rvfi_pc_wdata(dut_rvfi_pc_wdata),
    .rvfi_mem_addr(dut_rvfi_mem_addr),
    .rvfi_mem_rmask(dut_rvfi_mem_rmask),
    .rvfi_mem_wmask(dut_rvfi_mem_wmask),
    .rvfi_mem_rdata(dut_rvfi_mem_rdata),
    .rvfi_mem_wdata(dut_rvfi_mem_wdata),
    ._T_4_0(dut__T_4_0)
  );
  assign imem_dut_io_icache_resp_bits_data_MPORT_en = 1'h1;
  assign imem_dut_io_icache_resp_bits_data_MPORT_addr = iaddr[3:0];
  assign imem_dut_io_icache_resp_bits_data_MPORT_data = imem[imem_dut_io_icache_resp_bits_data_MPORT_addr]; // @[src/main/scala/mini/Core.scala 51:17]
  assign dmem_write_MPORT_en = 1'h1;
  assign dmem_write_MPORT_addr = daddr[3:0];
  assign dmem_write_MPORT_data = dmem[dmem_write_MPORT_addr]; // @[src/main/scala/mini/Core.scala 52:17]
  assign dmem_write_MPORT_1_en = 1'h1;
  assign dmem_write_MPORT_1_addr = daddr[3:0];
  assign dmem_write_MPORT_1_data = dmem[dmem_write_MPORT_1_addr]; // @[src/main/scala/mini/Core.scala 52:17]
  assign dmem_write_MPORT_2_en = 1'h1;
  assign dmem_write_MPORT_2_addr = daddr[3:0];
  assign dmem_write_MPORT_2_data = dmem[dmem_write_MPORT_2_addr]; // @[src/main/scala/mini/Core.scala 52:17]
  assign dmem_write_MPORT_3_en = 1'h1;
  assign dmem_write_MPORT_3_addr = daddr[3:0];
  assign dmem_write_MPORT_3_data = dmem[dmem_write_MPORT_3_addr]; // @[src/main/scala/mini/Core.scala 52:17]
  assign dmem_dut_io_dcache_resp_bits_data_MPORT_en = 1'h1;
  assign dmem_dut_io_dcache_resp_bits_data_MPORT_addr = daddr[3:0];
  assign dmem_dut_io_dcache_resp_bits_data_MPORT_data = dmem[dmem_dut_io_dcache_resp_bits_data_MPORT_addr]; // @[src/main/scala/mini/Core.scala 52:17]
  assign dmem_MPORT_data = write[31:0];
  assign dmem_MPORT_addr = daddr[3:0];
  assign dmem_MPORT_mask = 1'h1;
  assign dmem_MPORT_en = dut_io_dcache_req_valid & _T;
  assign rvfi_valid = dut_rvfi_valid; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_order = dut_rvfi_order; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_insn = dut_rvfi_insn; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_trap = dut_rvfi_trap; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_halt = 1'h0; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_intr = 1'h0; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_mode = 2'h3; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_ixl = 2'h1; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_rs1_addr = dut_rvfi_rs1_addr; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_rs2_addr = dut_rvfi_rs2_addr; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_rs1_rdata = dut_rvfi_rs1_rdata; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_rs2_rdata = dut_rvfi_rs2_rdata; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_rd_addr = dut_rvfi_rd_addr; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_rd_wdata = dut_rvfi_rd_wdata; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_pc_rdata = dut_rvfi_pc_rdata; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_pc_wdata = dut_rvfi_pc_wdata; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_mem_addr = dut_rvfi_mem_addr; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_mem_rmask = dut_rvfi_mem_rmask; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_mem_wmask = dut_rvfi_mem_wmask; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_mem_rdata = dut_rvfi_mem_rdata; // @[src/main/scala/mini/Core.scala 50:8]
  assign rvfi_mem_wdata = dut_rvfi_mem_wdata; // @[src/main/scala/mini/Core.scala 50:8]
  assign dut_clock = clock;
  assign dut_reset = reset;
  assign dut_io_icache_resp_valid = ~reset; // @[src/main/scala/mini/Core.scala 64:31]
  assign dut_io_icache_resp_bits_data = dut_io_icache_resp_bits_data_REG; // @[src/main/scala/mini/Core.scala 66:32]
  assign dut_io_dcache_resp_valid = ~reset; // @[src/main/scala/mini/Core.scala 65:31]
  assign dut_io_dcache_resp_bits_data = dut_io_dcache_resp_bits_data_REG; // @[src/main/scala/mini/Core.scala 67:32]
  always @(posedge clock) begin
    if (dmem_MPORT_en & dmem_MPORT_mask) begin
      dmem[dmem_MPORT_addr] <= dmem_MPORT_data; // @[src/main/scala/mini/Core.scala 52:17]
    end
    dut_io_icache_resp_bits_data_REG <= imem_dut_io_icache_resp_bits_data_MPORT_data; // @[src/main/scala/mini/Core.scala 66:42]
    dut_io_dcache_resp_bits_data_REG <= dmem_dut_io_dcache_resp_bits_data_MPORT_data; // @[src/main/scala/mini/Core.scala 67:42]
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_dut_io_icache_resp_valid_T_1 & ~someassumeid) begin
          $fwrite(32'h80000002,"Assumption failed\n    at Core.scala:77 assume(someAssume)\n"); // @[src/main/scala/mini/Core.scala 77:9]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_MEM_INIT
  _RAND_0 = {1{`RANDOM}};
  for (initvar = 0; initvar < 16; initvar = initvar+1)
    imem[initvar] = _RAND_0[31:0];
  _RAND_1 = {1{`RANDOM}};
  for (initvar = 0; initvar < 16; initvar = initvar+1)
    dmem[initvar] = _RAND_1[31:0];
`endif // RANDOMIZE_MEM_INIT
`ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  dut_io_icache_resp_bits_data_REG = _RAND_2[31:0];
  _RAND_3 = {1{`RANDOM}};
  dut_io_dcache_resp_bits_data_REG = _RAND_3[31:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
  always @(posedge clock) begin
    //
    if (_dut_io_icache_resp_valid_T_1) begin
      assume(someassumeid); // @[src/main/scala/mini/Core.scala 77:9]
    end
  end
endmodule
