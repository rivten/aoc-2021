private val REPORT = """199
200
208
210
200
207
240
269
260
263"""

val largerCount = REPORT.lines()
    .zipWithNext()
    .map { it.second.toInt() - it.first.toInt() }
    .count { it > 0 }

println("Measurements larger that the previous measurement $largerCount")