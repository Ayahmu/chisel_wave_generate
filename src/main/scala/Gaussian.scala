import chisel3._
import chisel3.experimental.FixedPoint

import complex.ComplexFP 
import config.FixedConfig._

class Gaussian extends Module {
    val io = IO(new Bundle {
        val in = Input(new ComplexFP)
        val len = Input(UInt(32.W))
        val t0 = Input(FixedPoint(width.W, fracBits.BP))
        val w = Input(FixedPoint(width.W, fracBits.BP))
        val amp = Input(FixedPoint(width.W, fracBits.BP))
        val start = Input(Bool())
        val out = Output(FixedPoint(width.W, fracBits.BP))
        val done = Output(Bool())
    })

}