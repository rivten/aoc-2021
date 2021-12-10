import java.io.File
import java.util.*

val PAIRS = mapOf(
    '>' to '<',
    ']' to '[',
    ')' to '(',
    '}' to '{',
)
val REVERSE_PAIRS = PAIRS
    .map { it.value to it.key }
    .toMap()

val SCORE = mapOf(
    ')' to 3,
    ']' to 57,
    '}' to 1197,
    '>' to 25137,
)

val SCORE_PART2 = mapOf(
    ')' to 1,
    ']' to 2,
    '}' to 3,
    '>' to 4,
)

sealed class Validity
object Valid : Validity()
class Corrupted(
    val expected: Char,
    val found: Char
    ) : Validity()
class Incomplete(
    private val stack: List<Char>
) : Validity() {

    val missingChars = stack.reversed()
        .map { // Should be "it -> REVERSE_PAIRS[it]!!" but I have a Kotlin compiler error :s
            when(it) {
                '<' -> '>'
                '[' -> ']'
                '(' -> ')'
                '{' -> '}'
                else -> throw IllegalStateException()
            }
        }

}

fun computeValidity(input: String): Validity {
    val stack = mutableListOf<Char>()
    input.forEach {
        when {
            it in PAIRS.values -> stack.add(it)
            it in PAIRS.keys -> {
                if (stack.isEmpty()) {
                    throw IllegalStateException() // Not defined in the exercise
                }
                if (stack[stack.size - 1] != PAIRS[it]) {
                    return Corrupted(REVERSE_PAIRS[stack[stack.size - 1]]!!, it)
                }
                stack.removeLast()
            }
        }
    }
    return if (!stack.isEmpty()) Incomplete(stack) else Valid
}

val validities = File("input.txt").readLines()
    .map { computeValidity(it) }

val resultPart1 = validities
    .mapNotNull { if (it is Corrupted) it else null }
    .map { SCORE[it.found]!! }
    .sum()

val resultPart2 = validities
    .mapNotNull { if (it is Incomplete) it else null }
    .map { it.missingChars }
    .map { line ->
        line.fold(0L) { acc, item -> acc * 5 + SCORE_PART2[item]!! }
    }
    .sorted()
    .let {
        it[it.size / 2]
    }


println("part1: $resultPart1")
println("part2: $resultPart2")
