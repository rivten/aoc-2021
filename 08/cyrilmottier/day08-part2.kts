import java.io.File

// "After some careful analysis" at the segments, I noticed
// Numbers can be defined as follows based on the number on on segments:
//
// if segmentCount = 2 then possibleDigits are 1
// if segmentCount = 3 then possibleDigits are 7
// if segmentCount = 4 then possibleDigits are 4
// if segmentCount = 5 then possibleDigits are 2,3,5
// if segmentCount = 6 then possibleDigits are 0,6,9
// if segmentCount = 7 then possibleDigits are 8
//
// Using the segmentCount gives us unicity for digits 1, 4, 7 and 8
// For the other digits, I noticed:
//
// 3 has segmentCount=5 and only one "on" segment from digit 1

// 9 has segmentCount=6 and the segments(9) + segments(4) = segments(9)
// 6 has segmentCount=6 and the segments(6) + segments(1) = segments(8)
// 0 is the remaining digit with segmentCount=7

// 5 has segmentCount=5 and segments(5) + segments(7) = segments(9)
// 2 is the remaining digit with segmentCount=5

fun List<String>.mapping(): Map<Set<Char>, Int> {
    return map { it.toSet() }
        .groupBy { it.size }
        .let { bySegmentCount ->

            val digitToChars = mutableMapOf<Int, Set<Char>>()

            digitToChars.put(1, bySegmentCount[2]!![0])
            digitToChars.put(4, bySegmentCount[4]!![0])
            digitToChars.put(7, bySegmentCount[3]!![0])
            digitToChars.put(8, bySegmentCount[7]!![0])

            digitToChars.put(3, bySegmentCount[5]!!.first { digitToChars[1]?.intersect(it)?.size == 2 })

            digitToChars.put(9, bySegmentCount[6]!!.first { digitToChars[4]?.union(it) == it })
            digitToChars.put(6, bySegmentCount[6]!!.first { digitToChars[1]?.union(it) == digitToChars[8] })
            digitToChars.put(0, bySegmentCount[6]!!.first { it !in listOf(digitToChars[9], digitToChars[6]) })

            digitToChars.put(5, bySegmentCount[5]!!.first { digitToChars[7]?.union(it) == digitToChars[9] })
            digitToChars.put(2, bySegmentCount[5]!!.first { it !in listOf(digitToChars[3], digitToChars[5]) })

            digitToChars.map { it.value to it.key }.toMap()
        }
}

val sum = File("input.txt").readLines()
    .map { line ->
        line.split(" | ").map { it.split(" ") }
    }
    .map { (digits, output) ->
        val mapping = digits.mapping()
        output
            .map {
                mapping[it.toSet()]!!
            }
            .fold(0) { acc, item ->
                acc * 10 + item
            }
    }
    .sum()

println(sum)