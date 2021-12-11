import java.io.File

class OctopusGrid(rawData: List<String>) {

    private val data = rawData.map { line ->
        line.toList()
            .map { it.digitToInt() }
            .toMutableList()
    }.toMutableList()

    private val rowCount = data.size
    private val colCount = data[0].size

    fun next(): Int {
        var count = 0
        val cellsToFlash = mutableListOf<Pair<Int, Int>>()

        for (row in 0 until data.size) {
            for (col in 0 until data[row].size) {
                data[row][col] += 1
                if (data[row][col] > 9) {
                    cellsToFlash.add(row to col)
                    data[row][col] = 0
                    count++
                }
            }
        }

        while (cellsToFlash.isNotEmpty()) {
            val (cellRow, cellCol) = cellsToFlash.removeFirst()

            // Increase adjacent cells
            for (adjacentRow in ((cellRow - 1)..(cellRow + 1)).bound(0, rowCount - 1)) {
                for (adjacentCol in ((cellCol - 1)..(cellCol + 1)).bound(0, colCount - 1)) {
                    if (!(cellRow == adjacentRow && cellCol == adjacentCol)) {
                        if (data[adjacentRow][adjacentCol] != 0) {
                            data[adjacentRow][adjacentCol] += 1
                            if (data[adjacentRow][adjacentCol] > 9) {
                                data[adjacentRow][adjacentCol] = 0
                                count++
                                cellsToFlash.add(adjacentRow to adjacentCol)
                            }
                        }
                    }
                }
            }
        }

        return count
    }

    fun isSimultaneous(): Boolean {
        return data.all { line -> line.all { it == 0 } }
    }

    override fun toString(): String {
        return buildString {
            data.forEach { line ->
                line.forEach {
                    if (it == 0) {
                        append(ANSI_GREEN)
                        append('0')
                        append(ANSI_RESET)
                    } else {
                        append(it)
                    }
                }
                append("\n")
            }
        }
    }

    private fun IntRange.bound(lower: Int, upper: Int): IntRange {
        return start.coerceAtLeast(lower)..endInclusive.coerceAtMost(upper)
    }

    companion object {
        const val ANSI_RESET = "\u001B[0m"
        const val ANSI_GREEN = "\u001B[32m"
    }
}

val grid = OctopusGrid(File("input.txt").readLines())
val resultPart1 = (1..100).fold(0) { acc, _ ->
    val count = grid.next()
    acc + count
}

val grid2 = OctopusGrid(File("input.txt").readLines())
var resultPart2 = generateSequence(0) { step ->
    grid2.next()
    step + 1
}.first { grid2.isSimultaneous() }

println(resultPart1)
println(resultPart2)

