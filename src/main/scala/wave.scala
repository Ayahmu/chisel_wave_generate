import chisel3._
import chisel3.experimental.FixedPoint

import config.FixedConfig._

class fp_add extends Module {
  val io = IO(new Bundle {
    val a = Input(FixedPoint(width.W, fracBits.BP))
    val b = Input(FixedPoint(width.W, fracBits.BP))
    val out = Output(FixedPoint(width.W, fracBits.BP))
  })

  io.out := io.a + io.b
}