module VerifyTop (
  input         clock,
  input         reset,
  output        rvfi_valid,
  output [63:0] rvfi_order,
  output [31:0] rvfi_insn,
  output        rvfi_trap,
  output        rvfi_halt,
  output        rvfi_intr,
  output [1:0]  rvfi_mode,
  output [1:0]  rvfi_ixl,
  output [4:0]  rvfi_rs1_addr,
  output [4:0]  rvfi_rs2_addr,
  output [31:0] rvfi_rs1_rdata,
  output [31:0] rvfi_rs2_rdata,
  output [4:0]  rvfi_rd_addr,
  output [31:0] rvfi_rd_wdata,
  output [31:0] rvfi_pc_rdata,
  output [31:0] rvfi_pc_wdata,
  output [31:0] rvfi_mem_addr,
  output [3:0]  rvfi_mem_rmask,
  output [3:0]  rvfi_mem_wmask,
  output [31:0] rvfi_mem_rdata,
  output [31:0] rvfi_mem_wdata
);

    reg  core_reset = 1'b1;

    always @(posedge clock) begin
        core_reset <= 1'b0; 
    end

    CoreSoc uut(
        .clock(clock),
        .reset(core_reset),
        .rvfi_valid(rvfi_valid),
        .rvfi_order(rvfi_order),
        .rvfi_insn(rvfi_insn),
        .rvfi_trap(rvfi_trap),
        .rvfi_halt(rvfi_halt),
        .rvfi_intr(rvfi_intr),
        .rvfi_mode(rvfi_mode),
        .rvfi_ixl(rvfi_ixl),
        .rvfi_rs1_addr(rvfi_rs1_addr),
        .rvfi_rs2_addr(rvfi_rs2_addr),
        .rvfi_rs1_rdata(rvfi_rs1_rdata),
        .rvfi_rs2_rdata(rvfi_rs2_rdata),
        .rvfi_rd_addr(rvfi_rd_addr),
        .rvfi_rd_wdata(rvfi_rd_wdata),
        .rvfi_pc_rdata(rvfi_pc_rdata),
        .rvfi_pc_wdata(rvfi_pc_wdata),
        .rvfi_mem_addr(rvfi_mem_addr),
        .rvfi_mem_rmask(rvfi_mem_rmask),
        .rvfi_mem_wmask(rvfi_mem_wmask),
        .rvfi_mem_rdata(rvfi_mem_rdata),
        .rvfi_mem_wdata(rvfi_mem_wdata)
    );

endmodule