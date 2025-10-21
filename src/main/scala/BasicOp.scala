package basicop

import chisel3._
import chisel3.experimental.FixedPoint

import complex.ComplexFP
import config.FixedConfig._

class Pow extends Module {
  val io = IO(new Bundle {
    val x = Input(FixedPoint(width.W, fracBits.BP))
    val n = Input(UInt(32.W))
    val start = Input(Bool())
    val res = Output(FixedPoint(width.W, fracBits.BP))
    val done = Output(Bool())
  })

  val resReg = RegInit(1.F(width.W, fracBits.BP))
  val counter = RegInit(0.U(32.W))
  val busy = RegInit(false.B)

  io.res := resReg
  io.done := !busy

  when(io.start && !busy) {
    resReg := 1.F(width.W, fracBits.BP)
    counter := 0.U
    busy := true.B
  }.elsewhen(busy) {
    when(counter === io.n) {
      busy := false.B
    }.otherwise {
      resReg := resReg * io.x
      counter := counter + 1.U
    }
  }
}

class Exp extends Module {
  val io = IO(new Bundle {
    val x = Input(FixedPoint(width.W, fracBits.BP))
    val start = Input(Bool())
    val res = Output(FixedPoint(width.W, fracBits.BP))
    val done = Output(Bool())
  })

  val pow = Module(new Pow)
  val sum = RegInit(0.F(width.W, fracBits.BP))
  val busy = RegInit(false.B)
  val powStart = RegInit(false.B)
  val doneReg = RegInit(false.B)

  val MAX_ITER = 80
  val invIter = (1.0 / MAX_ITER).F(width.W, fracBits.BP)
  val base = Wire(FixedPoint(width.W, fracBits.BP))
  base := io.x * invIter + 1.F(width.W, fracBits.BP)

  io.res := sum
  io.done := doneReg

  pow.io.x := base
  pow.io.n := MAX_ITER.U
  pow.io.start := powStart

  when(io.start && !busy) {
    when(io.x < -MAX_ITER.F(width.W, fracBits.BP)) {
      sum := 0.F(width.W, fracBits.BP)
      busy := false.B
      doneReg := true.B
    }.otherwise {
      powStart := true.B
      busy := true.B
      doneReg := false.B
    }
  }.elsewhen(powStart) {
    powStart := false.B
  }.elsewhen(busy && pow.io.done) {
    sum := pow.io.res
    busy := false.B
    doneReg := true.B
  }
}

class SearchErc extends Module {
  val io = IO(new Bundle {
    val x = Input(FixedPoint(width.W, fracBits.BP))
    val out = Output(FixedPoint(width.W, fracBits.BP))
  })

  val upper = 3.05.F(width.W, fracBits.BP)
  val lower = (-3.05).F(width.W, fracBits.BP)
  val one = 1.0.F(width.W, fracBits.BP)
  val negOne = (-1.0).F(width.W, fracBits.BP)

  val ErcSearchTable = VecInit(
    Seq(
      0.000000, 0.011283, 0.022565, 0.033841, 0.045111, 0.056372, 0.067622,
      0.078858, 0.090078, 0.101281, 0.112463, 0.123623, 0.134758, 0.145867,
      0.156947, 0.167996, 0.179012, 0.189992, 0.200936, 0.211840, 0.222703,
      0.233522, 0.244296, 0.255023, 0.265700, 0.276326, 0.286900, 0.297418,
      0.307880, 0.318284, 0.328627, 0.338908, 0.349126, 0.359279, 0.369365,
      0.379382, 0.389330, 0.399206, 0.409009, 0.418739, 0.428392, 0.437969,
      0.447468, 0.456887, 0.466225, 0.475482, 0.484655, 0.493745, 0.502750,
      0.511668, 0.520500, 0.529243, 0.537898, 0.546464, 0.554939, 0.563323,
      0.571616, 0.579816, 0.587923, 0.595936, 0.603856, 0.611681, 0.619411,
      0.627046, 0.634586, 0.642029, 0.649376, 0.656627, 0.663782, 0.670840,
      0.677801, 0.684665, 0.691433, 0.698104, 0.704678, 0.711155, 0.717537,
      0.723821, 0.730010, 0.736103, 0.742101, 0.748003, 0.753810, 0.759524,
      0.765142, 0.770668, 0.776100, 0.781440, 0.786687, 0.791843, 0.796908,
      0.801883, 0.806767, 0.811563, 0.816271, 0.820891, 0.825423, 0.829870,
      0.834231, 0.838508, 0.842701, 0.846810, 0.850838, 0.854784, 0.858650,
      0.862436, 0.866143, 0.869773, 0.873326, 0.876803, 0.880205, 0.883533,
      0.886788, 0.889970, 0.893082, 0.896124, 0.899096, 0.902000, 0.904837,
      0.907608, 0.910314, 0.912955, 0.915534, 0.918050, 0.920505, 0.922900,
      0.925236, 0.927513, 0.929734, 0.931898, 0.934008, 0.936063, 0.938065,
      0.940015, 0.941914, 0.943762, 0.945561, 0.947312, 0.949016, 0.950673,
      0.952285, 0.953852, 0.955376, 0.956857, 0.958296, 0.959695, 0.961053,
      0.962373, 0.963654, 0.964898, 0.966105, 0.967277, 0.968413, 0.969516,
      0.970586, 0.971623, 0.972628, 0.973603, 0.974547, 0.975462, 0.976348,
      0.977207, 0.978038, 0.978843, 0.979622, 0.980375, 0.981105, 0.981810,
      0.982493, 0.983153, 0.983790, 0.984407, 0.985003, 0.985578, 0.986135,
      0.986672, 0.987190, 0.987691, 0.988174, 0.988640, 0.989090, 0.989524,
      0.989943, 0.990347, 0.990736, 0.991111, 0.991472, 0.991821, 0.992156,
      0.992479, 0.992790, 0.993090, 0.993378, 0.993656, 0.993923, 0.994179,
      0.994426, 0.994664, 0.994892, 0.995111, 0.995322, 0.995525, 0.995719,
      0.995906, 0.996086, 0.996258, 0.996423, 0.996582, 0.996734, 0.996880,
      0.997020, 0.997155, 0.997284, 0.997407, 0.997525, 0.997639, 0.997747,
      0.997851, 0.997951, 0.998046, 0.998137, 0.998224, 0.998308, 0.998388,
      0.998464, 0.998537, 0.998607, 0.998674, 0.998738, 0.998799, 0.998857,
      0.998912, 0.998966, 0.999016, 0.999065, 0.999111, 0.999155, 0.999197,
      0.999237, 0.999275, 0.999312, 0.999346, 0.999379, 0.999411, 0.999441,
      0.999469, 0.999497, 0.999523, 0.999547, 0.999571, 0.999593, 0.999614,
      0.999635, 0.999654, 0.999672, 0.999689, 0.999706, 0.999722, 0.999736,
      0.999751, 0.999764, 0.999777, 0.999789, 0.999800, 0.999811, 0.999821,
      0.999831, 0.999841, 0.999849, 0.999858, 0.999866, 0.999873, 0.999880,
      0.999887, 0.999893, 0.999899, 0.999905, 0.999910, 0.999916, 0.999920,
      0.999925, 0.999929, 0.999933, 0.999937, 0.999941, 0.999944, 0.999948,
      0.999951, 0.999954, 0.999956, 0.999959, 0.999961, 0.999964, 0.999966,
      0.999968, 0.999970, 0.999972, 0.999973, 0.999975, 0.999976, 0.999978,
      0.999979, 0.999981, 0.999982, 0.999983, 0.999984
    ).map(_.F(width.W, fracBits.BP))
  )

  val absx = Mux(io.x < 0.F(width.W, fracBits.BP), -io.x, io.x)
  val y_fp =
    ((absx * 100.F(width.W, fracBits.BP)) + 0.5.F(width.W, fracBits.BP))
  val y = (y_fp.asUInt >> y_fp.binaryPoint.get.U)
  val idx = Mux(y > 409.U, 409.U, y)
  val valFromTable = ErcSearchTable(idx)

  val result = Wire(FixedPoint(width.W, fracBits.BP))
  result := 0.F(width.W, fracBits.BP)

  when(io.x > upper) {
    result := one
  }.elsewhen(io.x < lower) {
    result := negOne
  }.otherwise {
    result := Mux(io.x < 0.F(width.W, fracBits.BP), -valFromTable, valFromTable)
  }

  io.out := result
}

class Cordic extends Module {
  import config.CordicConst._

  val io = IO(new Bundle {
    val theta = Input(FixedPoint(thetaWidth.W, thetaFracBits.BP))
    val start = Input(Bool())
    val cosOut = Output(FixedPoint(cossinWidth.W, cossinFracBits.BP))
    val sinOut = Output(FixedPoint(cossinWidth.W, cossinFracBits.BP))
    val done = Output(Bool())
  })

  val current_cos = RegInit((K).F(cossinWidth.W, cossinFracBits.BP))
  val current_sin = RegInit(0.F(cossinWidth.W, cossinFracBits.BP))
  val theta_reg = Reg(FixedPoint(thetaWidth.W, thetaFracBits.BP))
  val iter = RegInit(0.U(6.W))
  val busy = RegInit(false.B)

  val invPowerOfTwoTable =
    VecInit(invPowerOfTwoTableScala.map(_.F(cossinWidth.W, cossinFracBits.BP)))
  val cordicPhaseTable =
    VecInit(cordicPhaseScala.map(_.F(thetaWidth.W, thetaFracBits.BP)))

  io.done := !busy

  when(io.start && !busy) {
    busy := true.B
    iter := 0.U
    current_cos := (K).F(cossinWidth.W, cossinFracBits.BP)
    current_sin := 0.F(cossinWidth.W, cossinFracBits.BP)
    theta_reg := io.theta * (180.0 / pi).F(thetaWidth.W, thetaFracBits.BP)
  }

  when(busy) {
    val sigma =
      Mux(
        theta_reg < 0.F(thetaWidth.W, thetaFracBits.BP),
        (-1).F(thetaWidth.W, thetaFracBits.BP),
        (1).F(thetaWidth.W, thetaFracBits.BP)
      )

    val factor = invPowerOfTwoTable(iter)
    val temp_cos = current_cos

    current_cos := current_cos - (current_sin * sigma * factor)
    current_sin := temp_cos * sigma * factor + current_sin

    val phase = cordicPhaseTable(iter)
    theta_reg := theta_reg - sigma * phase

    iter := iter + 1.U

    when(iter === (numIter - 1).U) {
      busy := false.B
    }
  }

  io.cosOut := current_cos
  io.sinOut := current_sin
}
