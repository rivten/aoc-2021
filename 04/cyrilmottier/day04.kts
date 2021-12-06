import Day04.Board
import java.io.File

class Cell(
    inValue: String,
) {
    val value: Int = inValue.toInt()
    var isTagged: Boolean = false
}

typealias Board = List<List<Cell>>

val data = File("input.txt").readLines()
var numbers = data[0].split(",")
val boards = data.drop(2)
    .windowed(size = 5, step = 6)
    .map { boardData ->
        boardData.map { boardLine ->
            boardLine.trim().split("\\s+".toRegex()).map { Cell(it) }
        }
    }

fun Board.tag(number: Int) {
    forEach { line ->
        line.forEach { cell ->
            if (cell.value == number) {
                cell.isTagged = true
            }
        }
    }
}

fun Board.completeSum(): Int {
    val hasFinishedRow = any { row ->
        row.all { it.isTagged }
    }

    val hasCompleteColumn = (0..4).map { col ->
        (0..4).map { row ->
            this[row][col]
        }
    }.any { col ->
       col.all { it.isTagged }
    }

    return if (hasFinishedRow || hasCompleteColumn) {
        sumOf { row ->
            row.sumOf { if (!it.isTagged) it.value else 0 }
        }
    } else {
        0
    }
}

numbers.forEach { numberString ->
    val number = numberString.toInt()
    boards.forEach { board ->
        board.tag(number)
        val completeSum = board.completeSum()
        if (completeSum > 0) {
            println(completeSum)
            println(number)
            println(completeSum * number)
            System.exit(1)
        }
    }
}