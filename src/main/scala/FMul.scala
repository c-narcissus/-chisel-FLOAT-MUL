
import chisel3._
import chisel3.util._
class FloatingPoint extends Bundle {
  val sign = UInt(1.W)
  val exp = UInt(8.W)
  val significand = UInt(23.W)

  def isZero(): Bool = exp === 0.U && significand === 0.U
  def isInf(): Bool = exp === 255.U && significand === 0.U
  def isNaN(): Bool = exp === 255.U && significand =/= 0.U
}
class SignCalculator extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(1.W))
    val b = Input(UInt(1.W))
    val valid = Input(Bool())
    val out = Output(UInt(1.W))
    val ready = Output(Bool())
  })

  // 默认值
  io.out := 0.U
  io.ready := false.B

  when(io.valid) {
    io.out := io.a ^ io.b  // 符号的异或运算
    io.ready := true.B
  }
}

class ExpAdder extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(8.W))
    val b = Input(UInt(8.W))
    val valid = Input(Bool())
    val out = Output(UInt(9.W))  // 9位宽以处理可能的进位
    val ready = Output(Bool())
  })

  // 默认值
  io.out := 0.U
  io.ready := false.B

  when(io.valid) {
    val bias = 127.U(9.W)  // 使用9位宽以处理进位
    // 将输入扩展到9位并进行计算
    val extendedA = (0.U(1.W) ## io.a - bias)
    val extendedB = (0.U(1.W) ## io.b - bias)
    io.out := extendedA + extendedB + bias  // 正确处理进位
    io.ready := true.B
  }
}

class SigMultiplier extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(23.W))
    val b = Input(UInt(23.W))
    val valid = Input(Bool())
    val out = Output(UInt(48.W))
    val ready = Output(Bool())
  })

  // 默认值
  // 默认值设置
   io.out := 0.U
  io.ready := false.B


  when(io.valid) {
    // 将23位尾数转换为24位，通过在最高位添加1
    val aExtended = Cat(1.U(1.W), io.a)
    val bExtended = Cat(1.U(1.W), io.b)
    // 计算24位尾数的乘积
    io.out := aExtended * bExtended
    io.ready := true.B
  }
}  

class FMul extends Module {
  val io = IO(new Bundle {
    val a = Input(new FloatingPoint)
    val b = Input(new FloatingPoint)
    val valid = Input(Bool())
    val ack = Input(Bool())
    val idle = Output(Bool())
    val out = Output(new FloatingPoint)
    val ready = Output(Bool())
  })

  val sWait :: sCompute :: sNormalize :: sFinish :: Nil = Enum(4)
  val state = RegInit(sWait)

  val signCalculator = Module(new SignCalculator)
  val expAdder = Module(new ExpAdder)
  val sigMultiplier = Module(new SigMultiplier)

  val expReg = RegInit(0.U(9.W))
  val productReg = RegInit(0.U(48.W))
  
  signCalculator.io.a := io.a.sign
  signCalculator.io.b := io.b.sign
  signCalculator.io.valid := (state === sCompute)

  expAdder.io.a := io.a.exp
  expAdder.io.b := io.b.exp
  expAdder.io.valid := (state === sCompute)

  sigMultiplier.io.a := io.a.significand
  sigMultiplier.io.b := io.b.significand
  sigMultiplier.io.valid := (state === sCompute)


  io.ready := false.B
  io.idle := false.B
  val signOutReg = RegInit(0.U(1.W))
  val expOutReg = RegInit(0.U(8.W))
  val significandOutReg =  RegInit(0.U(23.W))
  io.out.sign := signOutReg
  io.out.exp := expOutReg
  io.out.significand := significandOutReg
  switch(state) {
    is(sWait) {
      signOutReg := 0.U
      expOutReg := 0.U
      significandOutReg := 0.U
      io.idle := true.B
      when(io.valid) {
        state := sCompute
        io.idle := false.B
      }
    }
    is(sCompute) {
       // Add checks for zeros, NaNs, and infinities
        when(io.a.isZero || io.b.isZero) {
          state := sFinish
          signOutReg := 0.U
          expOutReg := 0.U
          significandOutReg := 0.U
        }.elsewhen(io.a.isNaN || io.b.isNaN) {
          state := sFinish
          signOutReg := 0.U
          expOutReg := 255.U
          significandOutReg := "b00000000000000000000001".U // example for signaling NaN
        }.elsewhen(io.a.isInf || io.b.isInf) {
          state := sFinish
          signOutReg := 0.U
          expOutReg := 255.U
          significandOutReg := "b00000000000000000000000".U // example for signaling NaN
        }.otherwise {
          when(signCalculator.io.ready && expAdder.io.ready && sigMultiplier.io.ready) {
            signOutReg := signCalculator.io.out // Always set the sign
            productReg := sigMultiplier.io.out
            expReg := expAdder.io.out
            state := sNormalize
          }
        }
    }
    is(sNormalize) {
    
      when(productReg(47)) { // Normalization needed
        val normalizedSignificand = productReg(46, 24)
        val incrementedExp = expReg(8, 0) + 1.U
        when(incrementedExp >= 255.U) {
          expOutReg := 255.U
          significandOutReg := 0.U
        }.otherwise {
          significandOutReg := normalizedSignificand
          expOutReg := incrementedExp(7, 0)
        }
      }.otherwise {
        significandOutReg := productReg(45, 23)
        expOutReg := expReg(7, 0)
      }
      state := sFinish
    }
    is(sFinish) {
      io.ready := true.B
      when(io.ack) { // Wait for ack to clear ready and reset state
        io.ready := false.B
        state := sWait
      }
    }
  }
}


object FMul extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new FMul(), Array("--target-dir","verilog"))
}