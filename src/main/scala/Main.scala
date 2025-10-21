import chisel3._

import config.FixedConfig._
import complex._
import basicop._

object Main extends App {
  println("Generating the Top hardware...")
  
  (new chisel3.stage.ChiselStage).emitVerilog(
    new SearchErc,
    Array("--target-dir", "generated")
  )
}
