package chiffre.inject

import chisel3._
import chisel3.util._

import chiffre.{ChiffreInjector, InjectorInfo, ScanField}

case class DurationHigh(width: Int) extends ScanField
case class DurationLow(width: Int) extends ScanField

case class CongestorInfo(duration_high: Int, duration_low: Int) extends InjectorInfo {
  val fields = Seq(DurationHigh(duration_high), DurationLow(duration_low))
}

class CongestorWide(bitWidth: Int) extends Injector(bitWidth) {
  val random_width = bitWidth
  val mask = (1 << random_width) - 1
  println(mask)
  lazy val info = CongestorInfo(10, 10)

  // pseudo random generator
  val random_data = RegInit(0.U(random_width.W))
  val random_wire = Wire(UInt(random_width.W))
  val xor = Wire(Bool())

  xor := random_data(0.U) ^ random_data(((random_width).toInt).U) ^ random_data((random_width-1).U)
  random_wire := ((random_data << 1.U) | (xor)) & mask.U
  random_data := random_wire

  io.out := random_data
  io.scan.out := 0.U
}

class Congestor(bitWidth: Int, 
                duration_high: Int, 
                duration_low: Int, 
                random: Boolean = false, 
                seed: Int = 1,
                force_val: Int = 1,
                random_width: Int = 6,
                passthrough: Boolean = false) extends Injector(1) {
  val mask = (1 << random_width) - 1
  println(mask)
  // pseudo random generator
  val random_data = RegInit(0.U(random_width.W))
  val random_wire = Wire(UInt(random_width.W))
  val xor = Wire(Bool())

  xor := random_data(0.U) ^ random_data(((random_width).toInt).U) ^ random_data((random_width-1).U)
  random_wire := ((random_data << 1.U) | (xor)) & mask.U
  random_data := random_wire

  // state machine
  val sIdle :: sSetCounter :: sCongest :: sSetPassCounter :: sPass :: Nil = Enum(5)
  val state = RegInit(sIdle)
  val counter = RegInit(0.U(random_width.W))

  lazy val info = CongestorInfo(duration_high, duration_low)

  // congestor logic
  val fire = Wire(Bool())
  fire := (state === sCongest)
  /*
  if (passthrough) {
    fire := 1.U
  } else {
    fire := io.scan.en & (state === sCongest)
  }
  */

  io.out := Mux(fire, force_val.U, io.in)
  io.scan.out := 0.U

  switch (state) {
    is(sIdle) {
      state := sSetCounter
    }

    is(sSetCounter) {
      if (random) {
        counter := random_data
      } else {
        counter := duration_high.U
      }
      state := sCongest
    }

    is(sCongest) {
      counter := counter - 1.U
      when (counter === 0.U) {
        state := sSetPassCounter
      }
    }

    is (sSetPassCounter) {
      if (random) {
        counter := random_data
      } else {
        counter := duration_low.U
      }

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

// Forcing low or high
class CongestorRandom_force_low(bitWidth: Int, val scanId: String) extends Congestor(bitWidth, 10, 10, random = true, seed = 4, force_val = 0, passthrough = false, random_width = 6) with ChiffreInjector
class CongestorRandom_force_high(bitWidth: Int, val scanId: String) extends Congestor(bitWidth, 10, 10, random = true, seed = 4, force_val = 0, passthrough = false, random_width = 6) with ChiffreInjector

class CongestorRandom_force_low_short(bitWidth: Int, val scanId: String) extends Congestor(bitWidth, 10, 10, random = true, seed = 4, force_val = 0, passthrough = false, random_width = 4) with ChiffreInjector
class CongestorRandom_force_high_short(bitWidth: Int, val scanId: String) extends Congestor(bitWidth, 10, 10, random = true, seed = 4, force_val = 0, passthrough = false, random_width = 4) with ChiffreInjector

class CongestorRandomWide(bitWidth: Int, val scanId: String) extends CongestorWide(bitWidth) with ChiffreInjector
