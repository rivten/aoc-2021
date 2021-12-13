import java.io.File

val lines = File("input.txt").readLines()
val splitLineIndex = lines.indexOf("")

val dots = lines.take(splitLineIndex)
    .map { line ->
        line.split(",").map { it.toInt() }
    }

val instructions = lines.drop(splitLineIndex + 1)
    .map { line ->
        val parts = line.split("=")
        parts[0].last() to parts[1].toInt()
    }

fun fold(instructionSet: List<Pair<Char, Int>>): List<List<Int>> {
    return instructionSet.fold(dots.toSet()) { acc, (axis, position) ->
        when (axis) {
            'x' -> acc.map {
                val newX = if (it[0] <= position) it[0] else 2 * position - it[0]
                listOf(newX, it[1])
            }
            'y' -> acc.map {
                val newY = if (it[1] <= position) it[1] else 2 * position - it[1]
                listOf(it[0], newY)
            }
            else -> throw IllegalArgumentException()
        }.toSet()
    }
        .distinct()
}

fun List<List<Int>>.dump(): String {
    val height = maxOf { it[1] } + 1
    val width = maxOf { it[0] } + 1
    return buildString {
        repeat(height) {
            repeat(width) {
                append('.')
            }
            append("\n")
        }
        this@dump.forEach {
            setCharAt(it[1] * (width + "\n".length) + it[0], '#')
        }
    }
}

val resultPart1 = fold(instructions.take(1)).size
val resultPart2 = fold(instructions).dump()

println(resultPart1)
println(resultPart2)
