import java.io.File

val lines = File("input.txt").readLines()
val splitLineIndex = lines.indexOf("")

val originalPolymer = lines.take(splitLineIndex)[0].toList()

val rules = lines.drop(splitLineIndex + 1)
    .fold(emptyMap<String, Char>()) { acc, line ->
        val parts = line.split(" -> ")
        val newAcc = acc.toMutableMap()
        newAcc.put(parts[0], parts[1][0])
        newAcc
    }

data class State(
    val polymerInfo: Map<String, Long> = emptyMap(),
    val characterCount: Map<Char, Long> = emptyMap()
)

val initialPolymerInfo = originalPolymer.zipWithNext()
    .map { "${it.first}${it.second}" }
    .groupBy(
        keySelector = { it },
    ).mapValues { 1L }

val initialCharacterCount = originalPolymer.toList()
    .groupBy { it }
    .mapValues { it.value.size.toLong() }

fun compute(times: Int): Long {
    val state = (1..times).fold(
        State(
            initialPolymerInfo,
            initialCharacterCount
        )
    ) { acc, _ ->
        val polymerInfoResult = mutableMapOf<String, Long>()
        val characterCountResult = acc.characterCount.toMutableMap()
        acc.polymerInfo.forEach { (pair, count) ->
            val inserted = rules[pair]!!
            val left = "${pair[0]}$inserted"
            val right = "$inserted${pair[1]}"
            polymerInfoResult.put(left, count + polymerInfoResult.getOrDefault(left, 0L))
            polymerInfoResult.put(right, count + polymerInfoResult.getOrDefault(right, 0L))

            val charCount = characterCountResult.getOrDefault(inserted, 0L)
            characterCountResult[inserted] = charCount + count
        }
        State(
            polymerInfoResult,
            characterCountResult
        )
    }

    val max = state.characterCount.maxOf { it.value }
    val min = state.characterCount.minOf { it.value }

    return max - min
}

println(compute(10))
println(compute(40))