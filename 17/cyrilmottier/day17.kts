import java.io.File
import kotlin.math.max

fun solve(instructions: String): Pair<Int, Int> {

    val regex = """target area: x=(\d+)..(\d+), y=(-\d+)..(-\d+)""".toRegex()
    val (startX, endX, startY, endY) = regex.find(instructions)!!.destructured.toList().map { it.toInt() }

    var best = 0
    var count = 0

    for (pvx in 0..1000) {
        for (pvy in -1000..1000) {
            var vx = pvx
            var vy = pvy

            var x = 0
            var y = 0
            var yMax = 0

            for (step in 0..1000) {
                x += vx
                y += vy
                yMax = max(yMax, y)
                if (x in startX..endX && y in startY..endY) {
                    best = max(best, yMax)
                    count++
                    break;
                }
                if (vx > 0) vx-- else if (vx < 0) vx++
                vy--
            }
        }
    }
    return best to count
}

val solution = solve(File("input.txt").readText())

println("resultPart1: ${solution.first}")
println("resultPart2: ${solution.second}")