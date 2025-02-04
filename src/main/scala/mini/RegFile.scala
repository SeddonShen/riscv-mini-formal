// See LICENSE for license details.

package mini

import chisel3._
import rvspeccore.checker.ConnectCheckerResult

class RegFileIO(xlen: Int) extends Bundle {
  val raddr1 = Input(UInt(5.W))
  val raddr2 = Input(UInt(5.W))
  val rdata1 = Output(UInt(xlen.W))
  val rdata2 = Output(UInt(xlen.W))
  val wen = Input(Bool())
  val waddr = Input(UInt(5.W))
  val wdata = Input(UInt(xlen.W))
}

class RegFile(xlen: Int) extends Module {
  val io = IO(new RegFileIO(xlen))
  val regs = Mem(32, UInt(xlen.W))
  val rf = RegInit(VecInit(Seq.fill(32)(0.U(xlen.W))))
  val resultRegWire = Wire(Vec(32, UInt(xlen.W)))
  // for (i <- 0 until 32) {
  //   resultRegWire(i) := regs.read(i.U)
  // }
  resultRegWire := rf
  resultRegWire(0) := 0.U
  ConnectCheckerResult.setRegSource(resultRegWire)

  io.rdata1 := Mux(io.raddr1.orR, rf(io.raddr1), 0.U)
  io.rdata2 := Mux(io.raddr2.orR, rf(io.raddr2), 0.U)
  when(io.wen & io.waddr.orR) {
    rf(io.waddr) := io.wdata
  }
}
