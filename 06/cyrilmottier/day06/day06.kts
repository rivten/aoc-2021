import java.io.File

val lanternfishes = File("input.txt").readLines()[0]
    .split(",")
    .map { it.toInt() }

val result = (1..80).fold(lanternfishes) { acc, _ ->
    acc.flatMap {
        when (it) {
            0 -> listOf(6, 8)
            else -> listOf(it - 1)
        }
    }
}

println(result.size)