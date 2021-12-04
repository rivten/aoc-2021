import 'dart:io';

import 'KdartExtension.dart';

void main() async {
  var intList = (await File('./input.txt').readAsLines())
      .map((e) => int.tryParse(e) ?? 0)
      .toList();

  /**
   * Part 1
   */
  var partOneResult = intList
      .zipWithNext(transform: (a, b) => (b-a))
      .count((item) => item > 0);

  print("Day 1/part 1 result: $partOneResult");

  /**
   * Part 2
   */
  var partTwoResult = intList
      .windowed(size: 3, step: 1)
      .map((list) => list.sum())
      .toList()
      .zipWithNext(transform: (a, b) => (b-a))
      .count((item) => item > 0);

  print("Day 1/part 2 result: $partTwoResult");
}
