package config

object FixedConfig {
  val width = 64
  val fracBits = 32
  // val thetaWidth = 32
  // val thetaFracBits = 16
  // val cossinFracBits = 8
}

object CordicConst {
  val numIter = 16
  val K = 0.607253 // 迭代收敛因子
  val pi = math.Pi

  def invPowerOfTwoTableScala: Seq[Double] = 
    (0 until numIter).map(i => 1.0 / (1 << i))

  def cordicPhaseScala: Seq[Double] = 
    (0 until numIter).map(i => math.atan(1.0 / (1 << i)) / math.Pi * 180.0)
}
