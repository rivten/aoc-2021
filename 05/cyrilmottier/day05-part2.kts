import java.io.File
import kotlin.math.abs
import kotlin.math.max
import kotlin.math.sign

val result = File("input.txt").readLines()
    .map { line ->
        line.split(" -> ").map { points ->
            points.split(",").map { it.toInt() }
        }
    }
    .map { (start, end) ->
        val (x1, y1) = start
        val (x2, y2) = end

        val delta = max(abs(x2 - x1), abs(y2 - y1))
        val signX = (x2 - x1).sign
        val signY = (y2 - y1).sign
        (0..delta).map { (x1 + it * signX) to (y1 + it * signY) }
    }
    .fold(mapOf<Pair<Int, Int>, Int>()) { space, line ->
        buildMap {
            putAll(space)
            line.forEach { point ->
                put(point, space.getOrDefault(point, 0) + 1)
            }
        }
    }
    .count { it.value > 1 }

println(result)
