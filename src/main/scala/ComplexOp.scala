package complex

import chisel3._
import chisel3.experimental.FixedPoint

import config.FixedConfig._

class ComplexFP extends Bundle {
  val real = FixedPoint(width.W, fracBits.BP)
  val imag = FixedPoint(width.W, fracBits.BP)

  def +(that: ComplexFP): ComplexFP = {
    val result = Wire(new ComplexFP)
    result.real := this.real + that.real
    result.imag := this.imag + that.imag
    result
  }

  def -(that: ComplexFP): ComplexFP = {
    val result = Wire(new ComplexFP)
    result.real := this.real - that.real
    result.imag := this.imag - that.imag
    result
  }

  def *(that: ComplexFP): ComplexFP = {
    val result = Wire(new ComplexFP)
    result.real := this.real * that.real - this.imag * that.imag
    result.imag := this.real * that.imag + this.imag * that.real
    result
  }

  def /(that: ComplexFP): ComplexFP = {
    val denom = that.real * that.real + that.imag * that.imag
    val result = Wire(new ComplexFP)
    result.real := (this.real * that.real + this.imag * that.imag) / denom
    result.imag := (this.imag * that.real - this.real * that.imag) / denom
    result
  }
}

class cpl_add extends Module {
  val io = IO(new Bundle {
    val a = Input(new ComplexFP)
    val b = Input(new ComplexFP)
    val out = Output(new ComplexFP)
  })

  io.out := io.a + io.b
}

class cpl_sub extends Module {
  val io = IO(new Bundle {
    val a = Input(new ComplexFP)
    val b = Input(new ComplexFP)
    val out = Output(new ComplexFP)
  })

  io.out := io.a - io.b
}

class cpl_mul extends Module {
  val io = IO(new Bundle {
    val a = Input(new ComplexFP)
    val b = Input(new ComplexFP)
    val out = Output(new ComplexFP)
  })

  io.out := io.a * io.b
}

class cpl_div extends Module {
  val io = IO(new Bundle {
    val a = Input(new ComplexFP)
    val b = Input(new ComplexFP)
    val out = Output(new ComplexFP)
  })

  io.out := io.a / io.b
}

class cpl_times_real extends Module {
  val io = IO(new Bundle {
    val a = Input(new ComplexFP)
    val b = Input(FixedPoint(width.W, fracBits.BP))
    val out = Output(new ComplexFP)
  })

  io.out.real := io.a.real * io.b
  io.out.imag := io.a.imag * io.b
}

class real_trans_cpl extends Module {
  val io = IO(new Bundle {
    val in = Input(FixedPoint(width.W, fracBits.BP))
    val out = Output(new ComplexFP)
  })

  io.out.real := io.in
  io.out.imag := 0.F(width.W, fracBits.BP)
}