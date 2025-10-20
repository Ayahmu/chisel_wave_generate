import chisel3._
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

import config.FixedConfig._

class WaveTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "add two fixed-point numbers" in {
    test(new cpl_add(width, fracBits))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) {
        dut =>
          dut.io.a.real.poke(1.25.F(width.W, fracBits.BP))
          dut.io.a.imag.poke(0.75.F(width.W, fracBits.BP))
          dut.io.b.real.poke(2.5.F(width.W, fracBits.BP))
          dut.io.b.imag.poke(1.25.F(width.W, fracBits.BP))

          dut.clock.step(1)

          dut.io.out.real.expect(3.75.F(width.W, fracBits.BP))
          dut.io.out.imag.expect(2.0.F(width.W, fracBits.BP))

          val scale = 1 << fracBits
          println(
            s"Result: ${dut.io.out.real.peek().litValue.toDouble / scale}"
          )
      }
  }

  "Pow" should "compute x^n correctly" in {
    test(new Pow(width, fracBits))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) {
        dut =>
          dut.io.x.poke(2.0.F(width.W, fracBits.BP))
          dut.io.n.poke(3.U)
          dut.io.start.poke(true.B)
          dut.clock.step(1)
          dut.io.start.poke(false.B)

          while (!dut.io.done.peek().litToBoolean) {
            dut.clock.step(1)
          }

          dut.io.res.expect(8.0.F(width.W, fracBits.BP))
      }
  }

  "Exp" should "compute e^x correctly" in {
    test(new Exp(width, fracBits))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) {
        dut =>
          dut.io.x.poke(1.0.F(width.W, fracBits.BP))
          dut.io.start.poke(true.B)
          dut.clock.step(1)
          dut.io.start.poke(false.B)

          while (!dut.io.done.peek().litToBoolean) {
            dut.clock.step(1)
          }
          dut.clock.step(1)

          val scale = 1L << fracBits
          println(s"e^1 = ${dut.io.res.peek().litValue.toDouble / scale}")
      }
  }

  "SearchErc" should "look up e^(-x) correctly" in {
    test(new SearchErc())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) {
        dut =>
          dut.io.x.poke(3.0.F(width.W, fracBits.BP))
          dut.clock.step(1)

          val scale = 1L << fracBits
          println(s"erf = ${dut.io.out.peek().litValue.toDouble / scale}")
      }
  }

  "Cordic" should "compute sine and cosine correctly" in {
    test(new Cordic)
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) {
        dut =>
          dut.io.theta.poke(
            (math.Pi / 6.0).F(width.W, fracBits.BP)
          ) // 30 degrees
          dut.io.start.poke(true.B)
          dut.clock.step(1)
          dut.io.start.poke(false.B)

          while (!dut.io.done.peek().litToBoolean) {
            dut.clock.step(1)
          }
          dut.clock.step(1)

          val scale = 1L << fracBits
          println(
            s"cos(30) = ${dut.io.cosOut.peek().litValue.toDouble / scale}, sin(30) = ${dut.io.sinOut.peek().litValue.toDouble / scale}"
          )
      }
  }
}
