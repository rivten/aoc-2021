import java.io.File

val lines = File("input.txt").readLines()
val wordSize = lines[0].length

fun doTheMath(
    compare: (ones: List<String>, zeros: List<String>) -> List<String>
) = (0 until wordSize)
    .fold(lines) { acc, index ->
        if (acc.size == 1) {
            acc
        } else {
            val ones = acc.filter { it[index] == '1' }
            val zeros = acc.filter { it[index] == '0' }
            compare(ones, zeros)
        }
    }
    .joinToString("")
    .toInt(2)

val oxygen = doTheMath { ones, zeros -> if (ones.size >= zeros.size) ones else zeros }
val co2 = doTheMath { ones, zeros -> if (zeros.size <= ones.size) zeros else ones }

println("oxygen: ${oxygen}")
println("co2: ${co2}")
println("oxygen * co2: ${oxygen * co2}")
