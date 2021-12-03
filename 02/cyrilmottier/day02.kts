import java.io.File

val (position, depth) = File("input.txt").readLines()
    .fold(0 to 0) { (p, d), item ->
        val (command, quantity) = item.split(" ")
        when (command) {
            "forward" -> ((p + quantity.toInt()) to d)
            "down" -> (p to (d + quantity.toInt()))
            "up" -> (p to (d - quantity.toInt()))
            else -> throw IllegalArgumentException()
        }
    }

println("Horizontal position: $position, depth: $depth, result: ${position * depth}")