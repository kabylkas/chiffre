package chiffre.inject

import chisel3._
import chisel3.util._

import chiffre.{ChiffreInjector, InjectorInfo, ScanField}

case class Duration(width: Int) extends ScanField

case class CongestorInfo(duration: Int) extends InjectorInfo {
  val fields = Seq(Duration(duration))
}

class Congestor(bitWidth: Int, duration: Int) extends Injector(1) {
  val sIdle :: sSetCounter :: sCongest :: sSetPassCounter :: sPass :: Nil = Enum(5)
  val state = RegInit(sIdle)
  val counter = RegInit(0.U(10.W))

  lazy val info = CongestorInfo(duration)

  val fire = io.scan.en & (state === sCongest)
  io.out := Mux(fire, 1.U, io.in)
  io.scan.out := 0.U

  switch (state) {
    is(sIdle) {
      state := sSetCounter
    }

    is(sSetCounter) {
      counter := duration.U
      state := sCongest
    }

    is(sCongest) {
      counter := counter - 1.U
      when (counter === 0.U) {
        state := sSetPassCounter
      }
    }

    is (sSetPassCounter) {
      counter := 511.U
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

class Congestor64(bitWidth: Int, val scanId: String) extends Congestor(bitWidth, 64) with ChiffreInjector