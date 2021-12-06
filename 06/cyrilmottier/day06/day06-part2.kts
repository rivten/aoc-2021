import java.io.File

val origin = File("input.txt").readLines()[0]
    .split(",")
    .map { it.toInt() }

val generations = origin.groupBy { it }
    .let { lanternfishesByGeneration ->
        (0..8).map {
            lanternfishesByGeneration.getOrElse(it) { emptyList<Long>() }.size.toLong()
        }
    }

val result = (1..256).fold(generations) { acc, day ->
    println("day: $day, count: $acc")
    buildList {
        addAll(acc.drop(1))
        val endOfLifeCount = acc[0]
        add(endOfLifeCount)
        this[6] += endOfLifeCount
    }
}

println(result.sum())