import java.io.File

val heightMap = File("input.txt").readLines()
    .map { line ->
        line.map { it.digitToInt() }
    }

var lowHeights = mutableListOf<Int>()
for (row in 0 until heightMap.size) {
    for (col in 0 until heightMap[row].size) {
        val currentHeight = heightMap[row][col]

        val adjacentHeights = mutableListOf<Int>()
        if (col - 1 >= 0) adjacentHeights.add(heightMap[row][col - 1])
        if (row - 1 >= 0) adjacentHeights.add(heightMap[row - 1][col])
        if (col + 1 < heightMap[row].size) adjacentHeights.add(heightMap[row][col + 1])
        if (row + 1 < heightMap.size) adjacentHeights.add(heightMap[row + 1][col])

        if (adjacentHeights.all { currentHeight < it }) {
            lowHeights.add(currentHeight)
        }
    }
}

println(lowHeights.sumOf { it + 1 })