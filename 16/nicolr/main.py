from functools import reduce

eval_func ={
	0:sum,
	1:lambda z: reduce(lambda x,y: x*y, z),
	2:min,
	3:max,
	5:lambda x: int(x[0] > x[1]),
	6:lambda x: int(x[0] < x[1]),
	7:lambda x: int(x[0] == x[1])
}

def binary(num, length):
    return format(num, '#0{}b'.format(length + 2))

class Packet:
	def __init__(self, bin_packet):
		self.bin_packet = bin_packet
		self.code = int(self.bin_packet, 2)
		self.bits_to_read = len(self)

		self.version = self.read_next_n_bits(n=3)
		self.type_id = self.read_next_n_bits(n=3)
		self.content = self.parse_content()

		self.bin_packet = self.bin_packet[:(2 + len(self) - self.bits_to_read)]
		self.code = self.code >> self.bits_to_read

	def __len__(self):
		return len(self.bin_packet) - 2

	def __repr__(self):
		sep = ',' if self.packet_type =="literal" else "\n\t"
		return f"Version: {self.version}, Type ID: {self.type_id}{sep} Content: {self.content} \n\t\t"

	@property
	def packet_type(self):
		return "literal" if self.type_id == 4 else "operator"

	def literal_parser(self):
		literal_code = 0
		while self.read_next_n_bits(1):
			literal_code |= self.read_next_n_bits(4)
			literal_code <<= 4
		literal_code |= self.read_next_n_bits(4)
		return [literal_code]

	def operator_parser(self):
		subpackets = []
		if (length_type_id:= self.read_next_n_bits(1)):		
			n_subpackets = self.read_next_n_bits(11)
			subpackets = []
			for _ in range(n_subpackets):
				bin_code = binary(self.read_next_n_bits(self.bits_to_read, move_cursor=False), length=self.bits_to_read)
				subpackets.append(new_packet:=Packet(bin_code))
				self.bits_to_read -= len(new_packet)
		else:
			bits_lengths = self.read_next_n_bits(15)
			while bits_lengths != 0:
				bin_code = binary(self.read_next_n_bits(bits_lengths, move_cursor=False), length=bits_lengths)
				subpackets.append(new_packet:=Packet(bin_code))
				bits_lengths -= len(new_packet)
				self.bits_to_read -= len(new_packet)
		return subpackets

	def parse_content(self):
		if self.packet_type =="literal":
			content = self.literal_parser()
		else:
			content = self.operator_parser()
		return content

	def read_next_n_bits(self, n, move_cursor=True):
		code = self.code >> (self.bits_to_read - n) & (2**n - 1)
		if move_cursor:
			self.bits_to_read -= n
		return code

	def sum_version(self):
		value = self.version
		if self.packet_type == "operator":
			value += sum(v.sum_version() for v in self.content)
		return value 

	@classmethod
	def from_hex_packet(cls, hex_packet):
		code = int(hex_packet, 16)
		bin_packet = binary(code, length=len(hex_packet) * 4)
		return Packet(bin_packet)

	def eval(self):
		if self.packet_type == "literal":
			return self.content[0]
		else:
			evaluation = list(map(lambda p: p.eval(), self.content))
			return eval_func[self.type_id](evaluation)

if __name__ =="__main__":
	with open('input.txt') as f:input_file = f.read().strip()
	p = Packet.from_hex_packet(input_file)
	print(p)
	print(f"Part1: {p.sum_version()}")
	print(f"Part2: {p.eval()}")

	literal = 'D2FE28'
	p = Packet.from_hex_packet(literal)
	print(p, p.sum_version())

	operator1 = '38006F45291200'
	p = Packet.from_hex_packet(operator1)
	print(p, p.sum_version())

	operator2 = "EE00D40C823060"
	p = Packet.from_hex_packet(operator2)
	print(p, p.sum_version())

	list_of_code = ["8A004A801A8002F478", "620080001611562C8802118E34", "C0015000016115A2E0802F182340", "A0016C880162017C3686B18A3D4780"]
	print(list(map(lambda p: p.sum_version(), map(Packet.from_hex_packet, list_of_code))))

	list_of_code = ["C200B40A82", "04005AC33890", "880086C3E88112", "CE00C43D881120", "D8005AC2A8F0", "F600BC2D8F", "9C005AC2F8F0", "9C0141080250320F1802104A08"] 
	for c in list_of_code:
		print(c, Packet.from_hex_packet(c).eval())
