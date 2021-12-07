import java.io.File
import kotlin.math.abs

val positions = File("input.txt").readLines()[0]
    .split(",")
    .map { it.toInt() }

val maxPosition = positions.maxOf { it }

val result = (0..maxPosition).map { position ->
    positions.fold(0) { acc, origin ->
        acc + abs(origin - position)
    }
}
    .minOf { it }

println(result)

