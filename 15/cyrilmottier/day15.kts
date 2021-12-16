import java.io.File
import kotlin.math.max

fun dijkstra(graph: List<List<Int>>, isPart2: Boolean = false): Pair<IntArray, IntArray> {

    val originalWidth = graph[0].size
    val originalHeight = graph.size

    val multipler = if (isPart2) 5 else 1

    val width = originalWidth * multipler
    val height = originalHeight * multipler

    val end = width * height - 1

    val queue = mutableSetOf<Int>()
    val dist = IntArray(width * height)
    val prev = IntArray(width * height)

    (0 until (width * height)).forEach {
        dist[it] = Int.MAX_VALUE
        queue.add(it)
    }

    dist[0] = 0

    fun graphValue(row: Int, col: Int): Int {
        return if (isPart2) {
            val originalRow = row % originalHeight
            val originalCol = col % originalWidth
            val originalValue = graph[originalRow][originalCol]
            val r = originalValue + row / originalHeight + col / originalWidth
            if (r <= 9) r else r % 9
        } else {
            graph[row][col]
        }
    }

    while (queue.isNotEmpty()) {
        var minDist = Int.MAX_VALUE
        var minU = 0
        queue.forEach {
            if (dist[it] < minDist) {
                minDist = dist[it]
                minU = it
            }
        }
        queue.remove(minU)

        if (minU == end) {
            // We're done
            return dist to prev
        }

        val colOfU = minU % width
        val rowOfU = minU / width

        if (rowOfU > 0) {
            val adjacentRow = rowOfU - 1
            val adjacentCol = colOfU
            val alt = dist[minU] + graphValue(adjacentRow, adjacentCol)
            val index = adjacentRow * width + adjacentCol
            if (alt < dist[index]) {
                dist[index] = alt
                prev[index] = minU
            }
        }
        if (rowOfU + 1 <= height - 1) {
            val adjacentRow = rowOfU + 1
            val adjacentCol = colOfU
            val alt = dist[minU] + graphValue(adjacentRow, adjacentCol)
            val index = adjacentRow * width + adjacentCol
            if (alt < dist[index]) {
                dist[index] = alt
                prev[index] = minU
            }
        }
        if (colOfU > 0) {
            val adjacentRow = rowOfU
            val adjacentCol = colOfU - 1
            val alt = dist[minU] + graphValue(adjacentRow, adjacentCol)
            val index = adjacentRow * width + adjacentCol
            if (alt < dist[index]) {
                dist[index] = alt
                prev[index] = minU
            }
        }
        if (colOfU + 1 <= width - 1) {
            val adjacentRow = rowOfU
            val adjacentCol = colOfU + 1
            val alt = dist[minU] + graphValue(adjacentRow, adjacentCol)
            val index = adjacentRow * width + adjacentCol
            if (alt < dist[index]) {
                dist[index] = alt
                prev[index] = minU
            }
        }
    }

    return dist to prev
}


val input = File("input.txt").readLines().map { line ->
    line.map { it.digitToInt() }
}

repeat(1) {
    println("Starting part1…")
    val now = System.currentTimeMillis()

    val (dist, _) = dijkstra(input)

    println("dist: ${dist[dist.size - 1]}")
    println("in ${System.currentTimeMillis() - now}ms")
}

repeat(1) {
    println("Starting part2…")
    val now = System.currentTimeMillis()

    val (dist, _) = dijkstra(input, true)

    println("dist: ${dist[dist.size - 1]}")
    println("in ${System.currentTimeMillis() - now}ms")
}
