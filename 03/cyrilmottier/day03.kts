import java.io.File
import kotlin.math.pow

val lines = File("input.txt").readLines()
val wordSize = lines[0].length

val delta = lines.fold(List(wordSize) { 0L }) { wordAcc, word ->
    wordAcc.mapIndexed { index, bitAcc ->
        bitAcc + if (word[index] == '1') 1 else -1
    }
}

// The fun stuff in this exercise if there's nothing indicating
// what happens if there's as much 1's as 0's. But gamma rate is the
// inverse of epsilon so it doesn't matter.
val gamma = delta
    .map { if (it > 0) "1" else "0" }
    .joinToString("")
    .toInt(2)

// Let's have fun with bits!
val epsilon = gamma.inv() and (2.0.pow(wordSize).toInt() - 1)

println("gamma: ${Integer.toBinaryString(gamma)} ($gamma)")
println("epsilon: ${Integer.toBinaryString(epsilon)} ($epsilon)")
println("gamma * epsilon: ${gamma * epsilon}")
