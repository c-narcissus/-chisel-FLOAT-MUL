
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class FMulTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FMul"

  it should "correctly multiply two floating-point numbers" in {
    test(new FMul) { c =>
      // Initialize the input
      c.io.valid.poke(true.B)
      c.io.a.sign.poke(0.U)  // positive number
      c.io.a.exp.poke("b01111110".U) // exponent = 126
      c.io.a.significand.poke("b00000000000000000000000".U)

      c.io.b.sign.poke(0.U)  // positive number
      c.io.b.exp.poke("b01111110".U) // exponent = 126
      c.io.b.significand.poke("b00000000000000000000000".U)

      println(s"Input A: sign = ${c.io.a.sign.peek().litValue}, exp = ${c.io.a.exp.peek().litValue}, significand = ${c.io.a.significand.peek().litValue}")
      println(s"Input B: sign = ${c.io.b.sign.peek().litValue}, exp = ${c.io.b.exp.peek().litValue}, significand = ${c.io.b.significand.peek().litValue}")

      // Wait until the module is ready
      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.clock.step(1) // additional step to ensure stability of output

      // Check the outputs after the computation is ready
      println(s"Output: sign = ${c.io.out.sign.peek().litValue}, exp = ${c.io.out.exp.peek().litValue}, significand = ${c.io.out.significand.peek().litValue}")
      c.io.out.sign.expect(0.U) // Result should be positive
      c.io.out.significand.expect("b00000000000000000000000".U) // Expected significand
      c.io.out.exp.expect("b01111101".U) // Expected exponent = 125
      c.io.ready.expect(true.B) // Ensure that the operation is indeed completed
      c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }
  it should "handle multiplication with zero" in {
  test(new FMul) { c =>
    c.io.valid.poke(true.B)
    c.io.a.sign.poke(0.U)
    c.io.a.exp.poke("b00000000".U) // Zero
    c.io.a.significand.poke("b00000000000000000000000".U)

    c.io.b.sign.poke(0.U)
    c.io.b.exp.poke("b01111110".U) // Non-zero
    c.io.b.significand.poke("b00000000000000000000000".U)

          // Wait until the module is ready
      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.clock.step(1) // additional step to ensure stability of output

    c.io.out.sign.expect(0.U)
    c.io.out.exp.expect("b00000000".U) // Result should be zero
    c.io.out.significand.expect("b00000000000000000000000".U)
    c.io.ready.expect(true.B)
       c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
  }
}

it should "handle NaN scenarios" in {
  test(new FMul) { c =>
    c.io.valid.poke(true.B)
    c.io.a.sign.poke(0.U)
    c.io.a.exp.poke("b11111111".U) // NaN
    c.io.a.significand.poke("b10000000000000000000000".U) // Non-zero significand for NaN

    c.io.b.sign.poke(0.U)
    c.io.b.exp.poke("b01111110".U)
    c.io.b.significand.poke("b00000000000000000000000".U)

          // Wait until the module is ready
      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.clock.step(1) // additional step to ensure stability of output

    c.io.out.sign.expect(0.U)
    c.io.out.exp.expect("b11111111".U) // NaN
    c.io.out.significand.expect("b00000000000000000000001".U) // Non-zero significand
    c.io.ready.expect(true.B)
          c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
  }
}

it should "handle infinity" in {
  test(new FMul) { c =>
    c.io.valid.poke(true.B)
    c.io.a.sign.poke(0.U)
    c.io.a.exp.poke("b11111111".U) // Infinity
    c.io.a.significand.poke("b00000000000000000000000".U)

    c.io.b.sign.poke(0.U)
    c.io.b.exp.poke("b01111110".U)
    c.io.b.significand.poke("b00000000000000000000000".U)

         // Wait until the module is ready
      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.clock.step(1) // additional step to ensure stability of output

    c.io.out.sign.expect(0.U)
    c.io.out.exp.expect("b11111111".U) // Infinity
    c.io.out.significand.expect("b00000000000000000000000".U)
    c.io.ready.expect(true.B)
          c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)

  }
}
 it should "correctly multiply a positive and a negative number" in {
    test(new FMul) { c =>
      // Test input: +1.5 * -1.25
      // +1.5 = 0 01111111 10000000000000000000000
      // -1.25 = 1 01111111 01000000000000000000000
      c.io.valid.poke(true.B)
      c.io.a.sign.poke(0.U)  // positive
      c.io.a.exp.poke("b01111111".U)  // exponent = 127
      c.io.a.significand.poke("b100_0000_0000_0000_0000_0000".U)  // significand for 1.5

      c.io.b.sign.poke(1.U)  // negative
      c.io.b.exp.poke("b01111111".U)  // exponent = 127
      c.io.b.significand.poke("b010_0000_0000_0000_0000_0000".U)  // significand for 1.25

              // Wait until the module is ready
      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.clock.step(1) // additional step to ensure stability of output

      c.io.out.sign.expect(1.U)  // Result should be negative
      c.io.out.exp.expect("b01111111".U)  // Adjusted exponent
      c.io.out.significand.expect("b111_0000_0000_0000_0000_0000".U)  // Result significand
      c.io.ready.expect(true.B)
            c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }

  it should "correctly multiply two negative numbers" in {
    test(new FMul) { c =>
      // Test input: -1.5 * -2.5
      // -1.5 = 1 01111111 10000000000000000000000
      // -2.5 = 1 10000000 01000000000000000000000
      c.io.valid.poke(true.B)
      c.io.a.sign.poke(1.U)  // negative
      c.io.a.exp.poke("b01111111".U)  // exponent = 127
      c.io.a.significand.poke("b10000000000000000000000".U)  // significand for 1.5

      c.io.b.sign.poke(1.U)  // negative
      c.io.b.exp.poke("b10000000".U)  // exponent = 128
      c.io.b.significand.poke("b01000000000000000000000".U)  // significand for 2.5

              // Wait until the module is ready
      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.clock.step(1) // additional step to ensure stability of output

      c.io.out.sign.expect(0.U)  // Result should be positive
      c.io.out.exp.expect("b10000000".U)  // Adjusted exponent
      c.io.out.significand.expect("b11100000000000000000000".U)  // Result significand
      c.io.ready.expect(true.B)
            c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }

  it should "correctly multiply two positive numbers with normalization" in {
    test(new FMul) { c =>
      // Test input: +2.5 * +3.5
      // +2.5 = 0 10000000 01000000000000000000000
      // +3.5 = 0 10000000 11000000000000000000000
      c.io.valid.poke(true.B)
      c.io.a.sign.poke(0.U)  // positive
      c.io.a.exp.poke("b10000000".U)  // exponent = 128
      c.io.a.significand.poke("b01000000000000000000000".U)  // significand for 2.5

      c.io.b.sign.poke(0.U)  // positive
      c.io.b.exp.poke("b10000000".U)  // exponent = 128
      c.io.b.significand.poke("b11000000000000000000000".U)  // significand for 3.5

               // Wait until the module is ready
      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.clock.step(1) // additional step to ensure stability of output

      c.io.out.sign.expect(0.U)  // Result should be positive
      c.io.out.exp.expect("b10000010".U)  // Adjusted exponent should be higher after normalization
      c.io.out.significand.expect("b00011000000000000000000".U)  // Normalized result significand
      c.io.ready.expect(true.B)
            c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }

}

