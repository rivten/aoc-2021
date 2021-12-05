import 'dart:io';

import '../../01/achp/dart/KdartExtension.dart';

List<List<T>> transposeMatrix<T>(List<List<T>> matrix) {
  var byteLength = matrix[0].length;
  return [
    for (var i = 0; i < byteLength; ++i) matrix.map((bits) => bits[i]).toList()
  ];
}

String unsignedBitInverse(String str) {
  return (~(int.parse(str, radix: 2).toUnsigned(str.length)))
      .toUnsigned(str.length)
      .toRadixString(2);
}

void main() async {
  var bits = (await File('./input.txt').readAsLines())
      .map((line) => line.split(''))
      .toList();

  /**
   * Part 1
   */
  var gamma = transposeMatrix(bits)
      .map((bits) =>
          (bits.count((bit) => bit == "1") > (bits.length / 2)) ? 1 : 0)
      .join();
  var epsilon = unsignedBitInverse(gamma);

  print("""
  day 3 part 1:
  gamma $gamma, epsilon $epsilon,
  result: ${int.parse(gamma, radix: 2) * int.parse(epsilon, radix: 2)}
  """);

  /**
   * Part 2
   */
  List<String> computeRec(List<List<String>> arr, int pos, bool predicate(int ones, int total)) {
    if (arr.length == 1 || pos > arr[0].length) return arr[0];

    var strongBit = predicate(
            arr.map((bits) => bits[pos]).toList().count((item) => item == "1"),
            arr.length)
        ? "1"
        : "0";
    var filtered = arr.where((bits) => bits[pos] == strongBit).toList();
    return computeRec(filtered, pos + 1, predicate);
  }

  var oxygenBits = computeRec(bits, 0, (ones, total) => ones >= (total / 2)).join('');
  var co2Bits = computeRec(bits, 0, (ones, total) => ones < (total / 2)).join('');
  var oxygen = int.parse(oxygenBits, radix: 2);
  var co2 = int.parse(co2Bits, radix: 2);

  print("""
  day 3 part 2:
  oxygen: $oxygen ($oxygenBits) $co2 ($co2Bits), result: ${oxygen * co2}
  """);
}
