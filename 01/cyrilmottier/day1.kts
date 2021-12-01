import java.io.File

val largerCount = File("input.txt").readLines()
    .filter { it.length > 0 }
    .zipWithNext()
    .map { it.second.toInt() - it.first.toInt() }
    .count { it > 0 }

println("Measurements larger that the previous measurement $largerCount")