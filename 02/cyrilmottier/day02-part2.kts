import java.io.File

val lines = File("input.txt").readLines()

var aim = 0
var position = 0
var depth = 0

lines.forEach {
    val components = it.split(" ")
    val command = components[0]
    val quantity = components[1].toInt()
    when (command) {
        "forward" -> {
            position += quantity
            depth += aim * quantity
        }
        "down" -> aim += quantity
        "up" -> aim -= quantity
    }
}

println("Horizontal position: $position, depth: $depth, result: ${position * depth}")