import java.io.File


val lines = File("input.txt").readLines()
val splitLineIndex = lines.indexOf("")

val originalPolymer = lines.take(splitLineIndex)[0].toList()

val rules = lines.drop(splitLineIndex + 1)
    .fold(emptyMap<List<Char>, Char>()) { acc, line ->
        val parts = line.split(" -> ")
        val newAcc = acc.toMutableMap()
        newAcc.put(parts[0].toList(), parts[1][0])
        newAcc
    }

val polymer = (1..40).fold(originalPolymer) { acc, _ ->
    acc.zipWithNext().flatMap { (first, second) ->
        val list = listOf(first, second)
        val middle = rules[list]
        if (middle != null) {
            listOf(first, middle)
        } else {
            listOf(first)
        }
    } + acc.last()
}

val atoms = polymer.groupBy { it }
val max = atoms.maxOf { it.value.size }
val min = atoms.minOf { it.value.size }

println(max - min)