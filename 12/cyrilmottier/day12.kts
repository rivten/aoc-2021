import java.io.File

val graph = File("input.txt").readLines()
    .map { it.split("-") }
    .flatMap {
        listOf(
            listOf(it[0], it[1]),
            listOf(it[1], it[0]),
        )
    }
    .groupBy(
        keySelector = { it[0] },
        valueTransform = { it[1] }
    )

fun pathsSequence(isPart1: Boolean) = generateSequence(emptyList<List<String>>() to listOf(listOf("start"))) { (_, present) ->
    val next = present.flatMap { path ->
        val lastCave = path.last()
        if (lastCave == "end") {
            listOf(path)
        } else {
            val nextCaves = graph[lastCave]
            if (nextCaves == null) {
                listOf(path)
            } else {
                nextCaves.mapNotNull { nextCave ->
                    when {
                        nextCave == "start" -> null
                        nextCave[0].isLowerCase() -> {
                            val smallCaves = path
                                .filter { it[0].isLowerCase() }
                                .groupBy { it }
                            if (smallCaves[nextCave] == null) {
                                path + nextCave
                            } else if (!isPart1 && smallCaves.all { it.value.size < 2 }) {
                                path + nextCave
                            } else {
                                null
                            }
                        }
                        else -> path + nextCave
                    }
                }
            }
        }
    }
    present to next
}

val resultPart1 = pathsSequence(true).first { it.first == it.second }.second.size
val resultPart2 = pathsSequence(false).first { it.first == it.second }.second.size

println("part1: $resultPart1")
println("part2: $resultPart2")