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


sealed class Validity
object Valid : Validity()
class Corrupted(val expected: Char, val found: Char) : Validity()
object Incomplete : Validity()

fun computeValidity(input: String): Validity {
    val stack = Stack<Char>()
    input.forEach {
        when {
            it in PAIRS.values -> stack.push(it)
            it in PAIRS.keys -> {
                if (stack.isEmpty()) {
                    throw IllegalStateException() // Not defined in the exercise
                }
                if (stack.peek() != PAIRS[it]) {
                    return Corrupted(REVERSE_PAIRS[stack.peek()]!!, it)
                }
                stack.pop()
            }
        }
    }
    return if (!stack.isEmpty()) Incomplete else Valid
}

val resultPart1 = File("input.txt").readLines()
    .map { computeValidity(it) }
    .mapNotNull { if (it is Corrupted) it else null }
    .map { SCORE[it.found]!! }
    .sum()

println("part1: resultPart1")