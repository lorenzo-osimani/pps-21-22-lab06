package u06lab.code

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test
import u06lab.code.ConnectThree._
import u06lab.code.ConnectThree.Player._

class ConnectThreeTests:
  @Test
  def findTest(): Unit =
    assertEquals(Some(X), find(List(Disk(0, 0, X)), 0, 0))
    assertEquals(Some(O), find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1))
    assertEquals(None, find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1))

  @Test
  def firstAvailableRowTest(): Unit =
    assertEquals(Some(0), firstAvailableRow(List(), 0)) // Some(0)
    assertEquals(Some(1), firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
    assertEquals(Some(2), firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
    assertEquals(Some(3), firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
    assertEquals(None, firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None)

  @Test
  def placeAnyTest(): Unit =
    assertEquals(List(List(Disk(3, 0, X)), List(Disk(2, 0, X)), List(Disk(1, 0, X)), List(Disk(0, 0, X))), placeAnyDisk(List(), X))
    printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
    assertEquals(List(List(Disk(0, 0, O), Disk(3, 0, X)), List(Disk(0, 0, O), Disk(2, 0, X)), List(Disk(0, 0, O), Disk(1, 0, X)), List(Disk(0, 0, O), Disk(0, 1, X))), placeAnyDisk(List(Disk(0, 0, O)), X))

  @Test
  def checkRowsTest(): Unit =
    assertTrue(checkRows(List(Disk(0, 0, X), Disk(1, 0, X), Disk(2, 0, X))))
    assertFalse(checkRows(List(Disk(0, 0, X), Disk(1, 0, X), Disk(2, 0, O), Disk(3, 0, X))))


  @Test
  def checkColumnsTest(): Unit =
    assertTrue(checkColumns(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X))))
    assertFalse(checkColumns(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, O), Disk(0, 3, X))))

  @Test
  def checkDiagonalsTest(): Unit =
    assertTrue(checkDiagonals(List(Disk(0, 0, X), Disk(1, 1, X), Disk(2, 2, X))))
    assertTrue(checkDiagonals(List(Disk(3, 0, X), Disk(2, 1, X), Disk(1, 2, X))))
    assertFalse(checkDiagonals(List(Disk(0, 0, X), Disk(1, 1, X), Disk(2, 2, O), Disk(3, 3, X))))


