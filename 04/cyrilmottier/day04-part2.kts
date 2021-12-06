import java.io.File

class Cell(
    inValue: String,
) {
    val value: Int = inValue.toInt()
    var isTagged: Boolean = false
}

typealias Board = List<List<Cell>>

fun Board.dump() {
    val ansiReset = "\u001B[0m";
    val ansiRed = "\u001B[31m";

    val output = buildString {
        this@dump.forEach { line ->
            line.forEach { cell ->
                if (cell.isTagged) {
                    append(ansiRed)
                }
                if (cell.value < 10) {
                    append(" ")
                }
                append(cell.value)
                if (cell.isTagged) {
                    append(ansiReset)
                }
                append(" ")
            }
            append("\n")
        }
    }

    print(output)
}

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

var finishedBoards = mutableSetOf<Board>()

numbers.forEach { numberString ->
    val number = numberString.toInt()
    println("Tagging $number")
    boards.forEach { board ->
        board.tag(number)
        val completeSum = board.completeSum()
        if (completeSum > 0) {
            finishedBoards.add(board)
            if (finishedBoards.size == boards.size) {
                println(completeSum)
                println(number)
                println(completeSum * number)
                System.exit(1)
            }
        }
    }
}