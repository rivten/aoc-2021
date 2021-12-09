import java.io.File

val heightMap = File("input.txt").readLines()
    .map { line ->
        line.map { it.digitToInt() }
    }

var lowHeights = mutableListOf<Pair<Int, Int>>()
for (row in 0 until heightMap.size) {
    for (col in 0 until heightMap[row].size) {
        val currentHeight = heightMap[row][col]

        val adjacentHeights = mutableListOf<Int>()
        if (col - 1 >= 0) adjacentHeights.add(heightMap[row][col - 1])
        if (row - 1 >= 0) adjacentHeights.add(heightMap[row - 1][col])
        if (col + 1 < heightMap[row].size) adjacentHeights.add(heightMap[row][col + 1])
        if (row + 1 < heightMap.size) adjacentHeights.add(heightMap[row + 1][col])

        if (adjacentHeights.all { currentHeight < it }) {
            lowHeights.add(row to col)
        }
    }
}

println(lowHeights)

val basins = lowHeights.map { pit ->
    val cellsToExplore = mutableListOf<Pair<Int, Int>>().apply {
        add(pit)
    }
    val exploredCells = mutableSetOf<Pair<Int, Int>>()
    while (cellsToExplore.isNotEmpty()) {
        val exploredCell = cellsToExplore.removeLast()
        exploredCells.add(exploredCell)

        val (row, col) = exploredCell
        if (col - 1 >= 0 && heightMap[row][col - 1] < 9) { // West
            val cell = row to (col - 1)
            if (cell !in exploredCells) {
                cellsToExplore.add(cell)
            }
        }
        if (row - 1 >= 0 && heightMap[row-1][col] < 9) { // North
            val cell = (row - 1) to col
            if (cell !in exploredCells) {
                cellsToExplore.add(cell)
            }
        }
        if (col + 1 < heightMap[row].size && heightMap[row][col + 1] < 9) { // East
            val cell = row to (col + 1)
            if (cell !in exploredCells) {
                cellsToExplore.add(cell)
            }
        }
        if (row + 1 < heightMap.size && heightMap[row + 1][col] < 9) { // South
            val cell = (row + 1) to col
            if (cell !in exploredCells) {
                cellsToExplore.add(cell)
            }
        }
    }
    exploredCells
}

println(
    basins.map { it.size}
        .sorted()
        .reversed()
        .take(3)
        .fold(1) { acc, item -> acc * item }
)