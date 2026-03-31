package it.unibo.pps.polyglot.a05b

import scala.util.Random
import it.unibo.pps.util.Sequences.*
import it.unibo.pps.util.Sequences.Sequence
import it.unibo.pps.util.Sequences.Sequence.*

class LogicsImpl(private val size: Int) extends Logics:

  require(size >= 3)

  private val random = Random()

  private val centerX = 1 + random.nextInt(size - 2)
  private val centerY = 1 + random.nextInt(size - 2)

  private var distance = 1
  private var over = false
  private var status: Sequence[(Int, Int)] = Sequence((centerX, centerY))

  override def tick(): Unit =
    if canDraw(distance) then
      status = status.concat(nextRing(distance))
      distance = distance + 1
    else
      over = true

  override def isOver(): Boolean =
    over

  override def hasElement(x: Int, y: Int): Boolean =
    status.contains((x, y))

  private def canDraw(d: Int): Boolean =
    centerX - d >= 0 &&
      centerX + d < size &&
      centerY - d >= 0 &&
      centerY + d < size

  private def nextRing(d: Int): Sequence[(Int, Int)] =
    Sequence(
      (centerX - d, centerY),
      (centerX + d, centerY),
      (centerX, centerY - d),
      (centerX, centerY + d),
      (centerX - d, centerY - d),
      (centerX - d, centerY + d),
      (centerX + d, centerY - d),
      (centerX + d, centerY + d)
    )

object LogicsImpl:
  def apply(size: Int): LogicsImpl = new LogicsImpl(size)