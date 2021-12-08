import java.io.File

val count1478 = File("input.txt").readLines()
    .map { it.split(" | ")[1].split(" ") }
    .map { values ->
        values.filter { it.length in arrayOf(2, 3, 4, 7) }
    }
    .sumOf { it.size }

println(count1478)