import 'dart:ffi';
import 'dart:io';

import '../../01/achp/dart/KdartExtension.dart';


List<List<T>> transposeMatrix<T>(List<List<T>> matrix){
	var byteLength = matrix[0].length;
  return [for (var i = 0; i < byteLength; ++i) matrix.map((bits) => bits[i]).toList()];
}

String unsignedBitInverse(String str){
	return (~(int
			.parse(str, radix: 2)
			.toUnsigned(str.length)))
			.toUnsigned(str.length)
			.toRadixString(2);
}

void main() async {
	var instructionList = (await File('./input.txt').readAsLines())
			.map((line) => line.split(''))
			.toList();

	var transposedMatrix = transposeMatrix(instructionList);
	var gamma = transposedMatrix.map((bits) => (bits.count((bit) => bit == "1") > (bits.length/2)) ? 1 : 0).join();
	var epsilon = unsignedBitInverse(gamma);
	var result = int.parse(gamma, radix: 2) * int.parse(epsilon, radix: 2);

	print("gamma $gamma, epsilon $epsilon, result: $result");
}
