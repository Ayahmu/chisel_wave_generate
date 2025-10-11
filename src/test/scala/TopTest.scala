import chisel3._
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

import config.FixedConfig._

class WaveTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "add two fixed-point numbers" in {
    test(new fp_add)
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
        dut.clock.setTimeout(0)
        dut.io.a.poke((1.24).F(width.W, fracBits.BP))
        dut.io.b.poke((0.73).F(width.W, fracBits.BP))
        dut.clock.step()

        val scale = 1 << fracBits
        println(s"Result: ${dut.io.out.peek().litValue.toDouble / scale}")
      }
  }
}
