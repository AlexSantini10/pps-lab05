package it.unibo.pps.polyglot.a01a

import it.unibo.pps.polyglot.a01a.Logics
import it.unibo.pps.polyglot.a01a.Logics.Result
import it.unibo.pps.util.Sequences.Sequence
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
class LogicsImpl(private val size: Int, private val boat: Int) extends Logics:

  private val maxMisses = 5
  require(size > 0, "size must be > 0")
  require(boat > 0 && boat <= size, "boat length must be in [1, size]")

  private val random = Random()
  private val boatRow = random.nextInt(size)
  private val boatStartCol = random.nextInt(size - boat + 1)
  private val boatCells: Sequence[(Int, Int)] = buildBoatCells(0)

  private var misses = 0
  private var hitCount = 0
  private var hits: Sequence[(Int, Int)] = Sequence.empty
  private var fired: Sequence[(Int, Int)] = Sequence.empty

  // Debug richiesto dal testo: stampa posizione nave all'avvio.
  println(s"Boat at row=$boatRow, startCol=$boatStartCol, length=$boat -> cells=$boatCells")

  private def buildBoatCells(offset: Int): Sequence[(Int, Int)] =
    if offset >= boat then Sequence.empty
    else Sequence.Cons((boatRow, boatStartCol + offset), buildBoatCells(offset + 1))

  private def addIfAbsent(sequence: Sequence[(Int, Int)], cell: (Int, Int)): (Sequence[(Int, Int)], Boolean) =
    if sequence.contains(cell) then (sequence, false)
    else (Sequence.Cons(cell, sequence), true)

  override def hit(row: Int, col: Int): Result =
    val cell = (row, col)
    val (updatedFired, isNewShot) = addIfAbsent(fired, cell)
    fired = updatedFired

    val result =
      if boatCells.contains(cell) then
        if isNewShot then
          val (updatedHits, isNewHit) = addIfAbsent(hits, cell)
          hits = updatedHits
          if isNewHit then
            hitCount += 1
        if hitCount == boat then Result.WON else Result.HIT
      else
        if isNewShot then
          misses += 1
        if misses >= maxMisses then Result.LOST else Result.MISS

    println(s"Shot ($row,$col) -> $result | hits=$hitCount/$boat, misses=$misses/$maxMisses")
    result
