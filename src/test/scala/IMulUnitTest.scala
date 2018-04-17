package boom.common

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester, TesterOptionsManager, SteppedHWIOTester}
import org.scalatest.{Matchers, FlatSpec}
import boom.exu.Imul


class IMulUnitTester(c: IMul) extends PeekPokeTester(c) {

   private val imul = c

   poke(imul.io.valid, 1)
   poke(imul.io.fn, 0)
   poke(imul.io.dw, 0)
   poke(imul.io.in0, 0)
   poke(imul.io.in1, 0)

   step(1)
   poke(imul.io.in0, 1)
   poke(imul.io.in1, 2)
   step(1)
   step(1)
   expect(imul.io.out, 0)
   step(1)
   expect(imul.io.out, 2)
}

// Invoke test with:
//    $ sbt 'test-only boom.IMulSpec'
//
class IMulSpec extends FlatSpec with Matchers {
   behavior of "IMulSpec"

   val manager = new TesterOptionsManager {
      testerOptions = testerOptions.copy(backendName = "verilator") // firrtl or verilator
   }

   it should "compute imul excellently" in {
      chisel3.iotesters.Driver(() => new IMul(imul_stages = 3)) { c =>
         new IMulUnitTester(c)
      } should be(true)
   }
}


class IMulHWIOUnitTester extends SteppedHWIOTester {

   val device_under_test = Module(new IMul(imul_stages = 3))
   enable_all_debug = true
   enable_scala_debug = true
   rnd.setSeed(0L)

   val imul = device_under_test

   imul.io.valid := 1.U
   imul.io.fn    := 0.U
   imul.io.dw    := 0.U
   imul.io.in0   := 0.U
   imul.io.in1   := 0.U

   step(1)
   poke(imul.io.in0, 1)
   poke(imul.io.in1, 2)
   step(1)
   step(1)
   expect(imul.io.out, 0)
   step(1)
   expect(imul.io.out, 2)
   step(1)
   step(1)
   step(1)

}

// Invoke test with:
//    $ sbt 'test-only boom.IMulTester'
//
class IMulTester extends ChiselFlatSpec {
  "IMul" should "compile and run without incident" in {
    assertTesterPasses { new IMulHWIOUnitTester }
  }
}



