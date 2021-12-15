import java.io.File
import kotlin.time.measureTime

data class Graph<T>(
    val vertices: Set<T>,
    val edges: Map<T, Set<T>>,
    val weights: Map<Pair<T, T>, Int>
)

fun <T> dijkstra(graph: Graph<T>, start: T): Map<T, T?> {
    val S: MutableSet<T> = mutableSetOf() // a subset of vertices, for which we know the true distance

    val delta = graph.vertices.map { it to Int.MAX_VALUE }.toMap().toMutableMap()
    delta[start] = 0

    val previous: MutableMap<T, T?> = graph.vertices.map { it to null }.toMap().toMutableMap()

    while (S != graph.vertices) {
        val v: T = delta
            .filter { !S.contains(it.key) }
            .minByOrNull { it.value }!!
            .key

        graph.edges.getValue(v).minus(S).forEach { neighbor ->
            val newPath = delta.getValue(v) + graph.weights.getValue(Pair(v, neighbor))

            if (newPath < delta.getValue(neighbor)) {
                delta[neighbor] = newPath
                previous[neighbor] = v
            }
        }

        S.add(v)
    }

    return previous.toMap()
}

fun <T> shortestPath(shortestPathTree: Map<T, T?>, start: T, end: T): List<T> {
    fun pathTo(start: T, end: T): List<T> {
        if (shortestPathTree[end] == null) return listOf(end)
        return listOf(pathTo(start, shortestPathTree[end]!!), listOf(end)).flatten()
    }

    return pathTo(start, end)
}

val rawInput = File("input.txt").readLines()
val boardWidth = rawInput[0].length
val boardHeight = rawInput.size

val vertices = (0 until boardHeight).flatMap { line ->
    (0 until boardWidth).map {
        line to it
    }
}
    .toSet()

val edges = (0 until boardHeight).flatMap { row ->
    (0 until boardWidth).map { col ->
        val adjacents = mutableSetOf<Pair<Int, Int>>()
        if (row > 0) {
            adjacents.add((row - 1) to col)
        }
        if (row + 1 <= boardHeight - 1) {
            adjacents.add((row + 1) to col)
        }
        if (col > 0) {
            adjacents.add(row to (col - 1))
        }
        if (col + 1 <= boardWidth - 1) {
            adjacents.add(row to (col + 1))
        }
        (row to col) to adjacents
    }
}.toMap()

val weights = (0 until boardHeight).flatMap { row ->
    (0 until boardWidth).flatMap { col ->
        val result = mutableListOf<Pair<Pair<Pair<Int, Int>, Pair<Int, Int>>, Int>>()
        if (row > 0) {
            result.add(((row to col) to ((row - 1) to col)) to rawInput[row - 1][col].digitToInt())
        }
        if (row + 1 <= boardHeight - 1) {
            result.add(((row to col) to ((row + 1) to col)) to rawInput[row + 1][col].digitToInt())
        }
        if (col > 0) {
            result.add(((row to col) to (row to (col - 1))) to rawInput[row][col - 1].digitToInt())
        }
        if (col + 1 <= boardWidth - 1) {
            result.add(((row to col) to (row to (col + 1))) to rawInput[row][col + 1].digitToInt())
        }
        result
    }
}.toMap()

val start = 0 to 0
val end = (boardHeight - 1) to (boardWidth - 1)
val graph = Graph(
    vertices,
    edges,
    weights
)

val now = System.currentTimeMillis()

    val shortestPathTree = dijkstra(graph, start)
    val shortestPath = shortestPath(shortestPathTree, start, end)

    val count = shortestPath.drop(1)
        .fold(0) { acc, (row, col) ->
            acc + rawInput[row][col].digitToInt()
        }

    println(shortestPath)
    println(count)

println("in ${System.currentTimeMillis() - now}ms")

