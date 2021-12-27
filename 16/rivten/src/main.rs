use bitstream_io::BitRead;
use bitstream_io::ByteWrite;

fn char_to_hex(c: &u8) -> u8 {
    let c = *c;
    if c as char >= '0' && c as char <= '9' {
        c - '0' as u8
    } else if c as char >= 'A' && c as char <= 'F' {
        (c - 'A' as u8) + 10
    } else {
        panic!()
    }
}

#[derive(Debug)]
enum PacketData {
    Operator(OperatorPacket),
    LiteralValue(LiteralValuePacket),
}

#[derive(Debug)]
struct PacketHeader {
    version: u8,
}

#[derive(Debug)]
struct OperatorPacket {
    header: PacketHeader,
    //length_type_id: u8,
    subpackets: Vec<Box<PacketData>>,
}

#[derive(Debug)]
struct LiteralValuePacket {
    header: PacketHeader,
    value: u32,
}

type BR<'a> = bitstream_io::read::BitReader<&'a [u8], bitstream_io::BigEndian>;

fn parse_packet_header(bit_reader: &mut BR) -> (u32, PacketHeader) {
    (
        3,
        PacketHeader {
            version: bit_reader.read::<u8>(3).unwrap(),
        },
    )
}

fn parse_literal_value_packet(
    bit_reader: &mut BR,
    header: PacketHeader,
) -> (u32, LiteralValuePacket) {
    let mut value = 0;
    let mut bits_parsed = 0;
    while bit_reader.read::<u8>(1).unwrap() == 1 {
        value <<= 4;
        value += bit_reader.read::<u32>(4).unwrap();
        bits_parsed += 5;
    }

    // NOTE: we need to do it once more after we see a 0
    value <<= 4;
    value += bit_reader.read::<u32>(4).unwrap();
    bits_parsed += 5;

    (bits_parsed, LiteralValuePacket { header, value })
}

fn parse_operator_packet(bit_reader: &mut BR, header: PacketHeader) -> (u32, OperatorPacket) {
    let mut bits_parsed = 0;

    let length_type_id = bit_reader.read::<u8>(1).unwrap();
    bits_parsed += 1;

    let mut subpackets = Vec::new();
    if length_type_id == 0 {
        let subpackets_length_in_bits = bit_reader.read::<u16>(15).unwrap();
        bits_parsed += 15;
        //println!("{}", subpackets_length_in_bits);

        let mut bits_parsed_for_subpackets = 0;
        while (bits_parsed_for_subpackets as u16) < subpackets_length_in_bits {
            let (bits_parsed_for_packet, packet) = parse_packet_data(bit_reader);
            bits_parsed_for_subpackets += bits_parsed_for_packet;
            bits_parsed += bits_parsed_for_packet;

            subpackets.push(Box::new(packet));
        }
    } else if length_type_id == 1 {
        let number_of_subpackets = bit_reader.read::<u16>(11).unwrap();
        bits_parsed += 11;
        //println!("{}", number_of_subpackets);
        for _ in 0..number_of_subpackets {
            let (bits_parsed_for_packet, packet) = parse_packet_data(bit_reader);
            bits_parsed += bits_parsed_for_packet;

            subpackets.push(Box::new(packet));
        }
    } else {
        panic!();
    }

    (
        bits_parsed,
        OperatorPacket {
            header,
            //length_type_id,
            subpackets,
        },
    )
}

fn parse_packet_data(bit_reader: &mut BR) -> (u32, PacketData) {
    let (bits_parsed_for_header, header) = parse_packet_header(bit_reader);
    let mut bits_parsed = bits_parsed_for_header;
    //println!("{:?}", header);
    let packet_type = bit_reader.read::<u8>(3).unwrap();
    bits_parsed += 3;
    match packet_type {
        4 => {
            let (bits_parsed_for_packet, lit_value) =
                parse_literal_value_packet(bit_reader, header);
            (
                bits_parsed + bits_parsed_for_packet,
                PacketData::LiteralValue(lit_value),
            )
        }
        _ => {
            let (bits_parsed_for_packet, op) = parse_operator_packet(bit_reader, header);
            (
                bits_parsed + bits_parsed_for_packet,
                PacketData::Operator(op),
            )
        }
    }
}

fn sum_of_version(packet: PacketData) -> u32 {
    match packet {
        PacketData::Operator(op) => {
            let mut sum = op.header.version as u32;
            for p in op.subpackets {
                sum += sum_of_version(*p);
            }
            sum
        }
        PacketData::LiteralValue(lit) => lit.header.version as u32,
    }
}

fn main() {
    let file_content = std::fs::read_to_string("input.txt").unwrap();
    let file_content = file_content.trim();

    let bytes = file_content
        .as_bytes()
        .chunks(2)
        .map(|chunk| chunk.iter().map(char_to_hex).fold(0, |acc, x| 16 * acc + x))
        .collect::<Vec<u8>>();

    let mut byte_writer = bitstream_io::write::ByteWriter::<
        std::io::Cursor<Vec<u8>>,
        bitstream_io::BigEndian,
    >::new(std::io::Cursor::<Vec<u8>>::new(Vec::new()));
    for byte in bytes {
        byte_writer.write(byte).unwrap();
    }

    let mut bit_reader = bitstream_io::read::BitReader::<&[u8], bitstream_io::BigEndian>::new(
        byte_writer.writer().get_ref(),
    );

    let (_, packet) = parse_packet_data(&mut bit_reader);
    println!("{:?}", packet);
    println!("{}", sum_of_version(packet));
}
