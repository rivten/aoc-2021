import java.io.File

fun String.readBits(startBit: Int, bitCount: Int): Int {
    require(bitCount > 0)
    val hexa = subSequence(startBit / 4, (startBit + bitCount - 1) / 4 + 1).toString()
    val decimal = Integer.parseInt(hexa, 16)
    val mask = (1 shl bitCount) - 1
    val rightShift = 3 - ((startBit + bitCount - 1) % 4)
    return mask and (decimal shr rightShift)
}

sealed class Packet(
    open val version: Int, open val packetTypeId: Int
) {

    var bitCount = 6

    abstract fun parse(
        input: String, startBit: Int
    )

    companion object {
        fun parse(
            input: String,
            startBit: Int,
        ): Packet {

            val version = input.readBits(startBit, 3)
            val type = input.readBits(startBit + 3, 3)

            val packet = when (type) {
                4 -> LiteralPacket(version)
                else -> OperatorPacket(version, type)
            }

            packet.parse(input, startBit + 6)

            return packet
        }
    }

}

class LiteralPacket(
    override val version: Int,
) : Packet(version, 4) {

    var value = 0L

    override fun parse(input: String, startBit: Int) {
        var cursor = startBit
        var continuationBit: Int
        do {
            continuationBit = input.readBits(cursor, 1)
            value = (value shl 4) or input.readBits(cursor + 1, 4).toLong()
            cursor += 5

        } while (continuationBit == 1)

        bitCount += (cursor - startBit)
    }

    override fun toString(): String {
        return """
            LiteralPacket(
                version=$version,
                packetTypeId=4,
                bitCount=$bitCount
                value=$value
            )
            """.trimIndent()
    }
}

class OperatorPacket(
    override val version: Int,
    override val packetTypeId: Int,
) : Packet(version, packetTypeId) {

    var lengthTypeId: Int = 0
    val subPackets = mutableListOf<Packet>()

    override fun parse(input: String, startBit: Int) {
        var cursor = startBit
        lengthTypeId = input.readBits(cursor++, 1)
        when (lengthTypeId) {
            0 -> {
                val toRead = input.readBits(cursor, 15)
                cursor += 15
                var read = 0
                while (read < toRead) {
                    val packet = Packet.parse(input, cursor)
                    read += packet.bitCount
                    cursor += packet.bitCount
                    bitCount += packet.bitCount
                    subPackets.add(packet)
                }
                bitCount += 16
            }
            1 -> {
                val subPacketCount = input.readBits(cursor, 11)
                cursor += 11
                var readCount = 0
                while (readCount < subPacketCount) {
                    val packet = Packet.parse(input, cursor)
                    readCount++
                    cursor += packet.bitCount
                    bitCount += packet.bitCount
                    subPackets.add(packet)
                }
                bitCount += 12
            }
            else -> throw IllegalStateException()
        }
    }

    override fun toString(): String {
        return """
            OperatorPacket(
                version=$version,
                packetTypeId=$packetTypeId,
                lengthTypeId=$lengthTypeId,
                bitCount=$bitCount,
                subPackets=$subPackets,
            )
            """.trimIndent()
    }
}

val hexadecimal = File("input.txt").readText()
val rootPacket = Packet.parse(hexadecimal, 0)

fun Packet.versionSum(): Int {
    return when (this) {
        is LiteralPacket -> version
        is OperatorPacket -> version + subPackets.sumOf { it.versionSum() }
        else -> throw IllegalStateException()
    }
}

fun Packet.compute(): Long {
    return when (this) {
        is LiteralPacket -> value
        is OperatorPacket -> {
            when (packetTypeId) {
                0 -> subPackets.sumOf { it.compute() }
                1 -> subPackets.fold(1L) { acc, item -> acc * item.compute() }
                2 -> subPackets.minOf { it.compute() }
                3 -> subPackets.maxOf { it.compute() }
                5 -> if (subPackets[0].compute() > subPackets[1].compute()) 1L else 0L
                6 -> if (subPackets[0].compute() < subPackets[1].compute()) 1L else 0L
                7 -> if (subPackets[0].compute() == subPackets[1].compute()) 1L else 0L
                else -> throw IllegalStateException()
            }
        }
        else -> throw IllegalStateException()
    }
}

println(rootPacket)
println(rootPacket.versionSum())
println(rootPacket.compute())

