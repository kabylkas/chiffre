package chiffre.inject

import chisel3._
import chisel3.util._

import chiffre.{ChiffreInjector, InjectorInfo, ScanField}

case class DurationHigh(width: Int) extends ScanField
case class DurationLow(width: Int) extends ScanField

case class CongestorInfo(duration_high: Int, duration_low: Int) extends InjectorInfo {
  val fields = Seq(DurationHigh(duration_high), DurationLow(duration_low))
}

class Congestor(bitWidth: Int, duration_high: Int, duration_low: Int) extends Injector(1) {
  val sIdle :: sSetCounter :: sCongest :: sSetPassCounter :: sPass :: Nil = Enum(5)
  val state = RegInit(sIdle)
  val counter = RegInit(0.U(16.W))

  lazy val info = CongestorInfo(duration_high, duration_low)

  val fire = io.scan.en & (state === sCongest)
  io.out := Mux(fire, 1.U, io.in)
  io.scan.out := 0.U

  switch (state) {
    is(sIdle) {
      state := sSetCounter
    }

    is(sSetCounter) {
      counter := duration_high.U
      state := sCongest
    }

    is(sCongest) {
      counter := counter - 1.U
      when (counter === 0.U) {
        state := sSetPassCounter
      }
    }

    is (sSetPassCounter) {
      counter := duration_low.U
      state := sPass
    }

    is (sPass) {
      counter := counter - 1.U
      when (counter === 0.U) {
        state := sIdle
      }
    }
  }
}

class Congestor64_64(bitWidth: Int, val scanId: String) extends Congestor(bitWidth, 64, 64) with ChiffreInjector
// Period: 1024 cycles
class Congestor1024_20duty(bitWidth: Int, val scanId: String) extends Congestor(bitWidth, 204, 820) with ChiffreInjector
class Congestor1024_50duty(bitWidth: Int, val scanId: String) extends Congestor(bitWidth, 512, 512) with ChiffreInjector
class Congestor1024_80duty(bitWidth: Int, val scanId: String) extends Congestor(bitWidth, 820, 204) with ChiffreInjector
// Period: 2048 cycles
class Congestor2048_20duty(bitWidth: Int, val scanId: String) extends Congestor(bitWidth, 410, 1638) with ChiffreInjector
class Congestor2048_50duty(bitWidth: Int, val scanId: String) extends Congestor(bitWidth, 1024, 1024) with ChiffreInjector
class Congestor2048_80duty(bitWidth: Int, val scanId: String) extends Congestor(bitWidth, 1638, 410) with ChiffreInjector
// Period: 4096 cycles
class Congestor4096_20duty(bitWidth: Int, val scanId: String) extends Congestor(bitWidth, 820, 3276) with ChiffreInjector
class Congestor4096_50duty(bitWidth: Int, val scanId: String) extends Congestor(bitWidth, 2048, 2048) with ChiffreInjector
class Congestor4096_80duty(bitWidth: Int, val scanId: String) extends Congestor(bitWidth, 3276, 820) with ChiffreInjector


