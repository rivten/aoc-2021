import java.io.File

val largerCount = File("input.txt").readLines()
    .windowed(size = 3, step = 1)
    .map {
        it.fold(0) { acc, item -> acc + item.toInt() }
    }
    .zipWithNext()
    .map { it.second.toInt() - it.first.toInt() }
    .count { it > 0 }

println("Measurements larger that the previous measurement $largerCount")