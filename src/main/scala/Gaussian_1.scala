import chisel3._
import chisel3.experimental.FixedPoint
import complex._
import config.FixedConfig._
import basicop._
class env_gaussian  extends Module {
  import config.CordicConst._
  val io = IO(new Bundle {
    val t = Input(FixedPoint(width.W, fracBits.BP)) 
    val t0 = Input(FixedPoint(width.W, fracBits.BP))
    val w = Input(FixedPoint(width.W, fracBits.BP))
    val amp = Input(FixedPoint(width.W, fracBits.BP))
    val phase = Input(FixedPoint(width.W, fracBits.BP))

    val start = Input(Bool()) 
    val done = Output(Bool()) 
    val res = Output(new ComplexFP(width, fracBits)) 
  })
  //标准差
  val SIGMA = 2.3548200.F(width.W, fracBits.BP)
  val TWO_SIGMA_SQ = 2.F(width.W, fracBits.BP) * SIGMA * SIGMA
  //状态机
  val s_idle :: s_cordic_start :: s_cordic_wait :: s_exp_start :: s_exp_wait :: s_calculate :: Nil = Enum(6)
  val state = RegInit(s_idle)
  //实例化
  val exp_mod = Module(new Exp(width, fracBits))
  val cordic_mod = Module(new Cordic)
  val cpl_mul_mod = Module(new cpl_times_real(width, fracBits)) 
  //寄存器
  val sigma_sq_reg = Reg(FixedPoint(width.W, fracBits.BP)) 
  val amp_reg = Reg(FixedPoint(width.W, fracBits.BP))
  val temp_exp_res = Reg(FixedPoint(width.W, fracBits.BP)) 
  val temp_cos = Reg(FixedPoint(width.W, fracBits.BP))
  val temp_sin = Reg(FixedPoint(width.W, fracBits.BP))
  //默认值
  cordic_mod.io.start := false.B
  exp_mod.io.start := false.B
  io.done := false.B
  io.res := 0.F(width.W, fracBits.BP).asTypeOf(new ComplexFP(width, fracBits)) 
  //
  switch(state) {
    is(s_idle) {
      when(io.start) {
        sigma_sq_reg := (io.w / SIGMA) * (io.w / SIGMA) 
        amp_reg := io.amp
        cordic_mod.io.theta := io.phase
        cordic_mod.io.start := true.B
        state := s_cordic_wait
      }
    }
    is(s_cordic_wait) {
      when(cordic_mod.io.done) {
        temp_cos := cordic_mod.io.cosOut
        temp_sin := cordic_mod.io.sinOut
        state := s_exp_start
      }
    }
    is(s_exp_start) {
      // 计算指数项的参数：(t - t0)^2 / (-2 * sigma^2)
      val t_diff = io.t - io.t0
      val t_diff_sq = t_diff * t_diff
      val exponent = t_diff_sq / (-TWO_SIGMA_SQ) // C++ 中的 pow_res / (-2 * sigma * sigma)

      exp_mod.io.x := exponent
      exp_mod.io.start := true.B
      state := s_exp_wait
    }

    is(s_exp_wait) {
      when(exp_mod.io.done) {
        temp_exp_res := exp_mod.io.res
        state := s_calculate
      }
    }

    is(s_calculate) {
      val temp = amp_reg * temp_exp_res
      cpl_mul_mod.io.a.real := temp_cos
      cpl_mul_mod.io.a.imag := temp_sin
      cpl_mul_mod.io.b := temp
      
      io.res := cpl_mul_mod.io.out
      io.done := true.B
      state := s_idle 
    }
  }

  when(state === s_cordic_wait) {
    cordic_mod.io.start := false.B
  }
  when(state === s_exp_wait) {
    exp_mod.io.start := false.B
  }
}
class EnvMix extends Module {
  import config.CordicConst._
  val io = IO(new Bundle {
    val waveLen = Input(UInt(32.W)) 
    val df = Input(FixedPoint(width.W, fracBits.BP)) 
    
    val dphase = Input(FixedPoint(width.W, fracBits.BP)) 
    val tIn = Input(FixedPoint(width.W, fracBits.BP)) 
    val resIn = Input(new ComplexFP) 
    val start = Input(Bool()) // 启动处理序列
    val resOut = Output(new ComplexFP) 
    val done = Output(Bool()) // 所有 waveLen 个元素处理完成
    val outValid = Output(Bool()) // 当前 resOut 信号有效
  })
  val counter = RegInit(0.U(32.W)) 
  val busy = RegInit(false.B) 
  val cordic = Module(new Cordic)
  val waveLenReg = Reg(UInt(32.W))
  val dfReg = Reg(FixedPoint(width.W, fracBits.BP))
  val dphaseReg = Reg(FixedPoint(width.W, fracBits.BP))
  val sIdle :: sCalculateConst :: sCordic :: sMultiply :: sDone :: Nil = Enum(5)
  val state = RegInit(sIdle)
  val const_1_fp = Reg(FixedPoint(width.W, fracBits.BP)) 
  val cordic_theta_in = Wire(FixedPoint(thetaWidth.W, thetaFracBits.BP))
  
  val resInReg = Reg(new ComplexFP) 
  val tmpReg = Reg(new ComplexFP) 

  val TWO_PI_FP = (2.0 * math.Pi).F(width.W, fracBits.BP)
  val NEG_ONE_FP = (-1.0).F(width.W, fracBits.BP)
  
  cordic_theta_in := const_1_fp.asTypeOf(FixedPoint(thetaWidth.W, thetaFracBits.BP))
  cordic.io.theta := cordic_theta_in
  
  val cordicStart = RegInit(false.B)
  cordic.io.start := cordicStart

  // 复数乘法
  val cplMul = Module(new cpl_mul)
  cplMul.io.a := resInReg
  cplMul.io.b := tmpReg

  // 默认输出 
  io.resOut := 0.F.asTypeOf(new ComplexFP)
  io.done := (state === sDone)
  io.outValid := (state === sMultiply) 

  // 状态机逻辑 
  switch(state) {
    is(sIdle) {
      io.outValid := false.B
      
      when(io.start && !busy) {
        // 启动，锁定输入参数
        waveLenReg := io.waveLen
        dfReg := io.df
        dphaseReg := io.dphase
        busy := true.B
        
        // 第一次计算，转到计算状态
        state := sCalculateConst
      }
    }

    is(sCalculateConst) {
      // const_1 = - data_f(2 * pi) * df * tlist[i] - dphase;
      val term1 = NEG_ONE_FP * TWO_PI_FP * dfReg * io.tIn
      val const_1_next = term1 - dphaseReg 

      // 存储结果和缓存输入
      const_1_fp := const_1_next
      resInReg := io.resIn 
      
      cordicStart := true.B // 启动 Cordic
      state := sCordic
    }

    is(sCordic) {
      cordicStart := false.B // 仅启动一拍
      when(cordic.io.done) {
        //  tmp.real = cos_const_1; tmp.img = sin_const_1;
        // Cordic 完成，得到 cos/sin，构成 tmp。
        // Cordic 输出 (cossinWidth, cossinFracBits) 转换回 data_f format (width, fracBits)
        tmpReg.real := cordic.io.cosOut.asTypeOf(FixedPoint(width.W, fracBits.BP))
        tmpReg.imag := cordic.io.sinOut.asTypeOf(FixedPoint(width.W, fracBits.BP))       
        //  ++i (计数器递增)
        counter := counter + 1.U 
        
        state := sMultiply
      }
    }

    is(sMultiply) {
      // 乘法在这一拍完成
      //  res[i] = data_w2_mul(res[i], tmp);
      io.resOut := cplMul.io.out 
      io.outValid := true.B
      // 检查是否完成所有 waveLen 个元素
      when(counter === waveLenReg) {
        state := sDone // 所有元素处理完毕
      }.otherwise {
        // 准备处理下一个元素
        state := sCalculateConst 
      }
    }
    is(sDone) {
      io.outValid := false.B
      busy := false.B
      when(io.start) {
        // 重新启动
        waveLenReg := io.waveLen
        dfReg := io.df
        dphaseReg := io.dphase
        counter := 0.U
        busy := true.B
        state := sCalculateConst
      }.otherwise {
        // 保持 Done 状态直到重新启动
        state := sDone
      }
    }
  }
}

