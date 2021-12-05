import 'dart:io';

/**
 * Going full OOP style for this second day
 */

/**
 * Simple Position ( part 1 )
 */
class Position {
  late int x;
  late int y;

  Position(int x, int y) {
    this.x = x;
    this.y = y;
  }

  Position move(Instruction instruction) {
    if (instruction.direction == Direction.up)
      return Position(this.x, this.y - instruction.value);
    if (instruction.direction == Direction.down)
      return Position(this.x, this.y + instruction.value);
    return Position(x + instruction.value, this.y);
  }

  String toString() {
    return "($x, $y)";
  }
}

/**
 * Simple Position ( part 2 )
 */
class PositionWithAim extends Position {
  late int aim;

  PositionWithAim(int x, int y, int aim) : super(x, y) {
      this.aim = aim;
  }

  @override
  PositionWithAim move(Instruction instruction) {
    if (instruction.direction == Direction.up)
      return PositionWithAim(this.x, this.y, this.aim - instruction.value);
    if (instruction.direction == Direction.down)
      return PositionWithAim(this.x, this.y, this.aim + instruction.value);
    return PositionWithAim(x + instruction.value, this.y + instruction.value*this.aim, this.aim);
  }

  @override
  String toString(){
    return "($x, $y, $aim)";
  }
}

enum Direction {
  forward,
  up,
  down,
}

class Instruction {
  late Direction direction;
  late int value;

  Instruction.fromString(String str) {
    var strArr = str.split(' ');
    value = int.tryParse(strArr[1]) ?? 0;
    direction = ((directionStr) {
      if (directionStr == 'up') return Direction.up;
      if (directionStr == 'down') return Direction.down;
      return Direction.forward;
    })(strArr[0]);
  }

  String toString() {
    return "($direction, $value)";
  }
}

/**
 * MAIN
 */
void main() async {
  var instructionList = (await File('./input.txt').readAsLines())
      .map((instructionStr) => Instruction.fromString(instructionStr))
      .toList();

  Position initPoint = Position(0, 0);
  var result = instructionList.fold(
      initPoint, (Position prevPoint, instruction) => prevPoint.move(instruction));

  print("Day 2 part 1: point $result, result ${result.x * result.y}");

  PositionWithAim initPoint2 = PositionWithAim(0, 0, 0);
  var resultPart2 = instructionList.fold(
      initPoint2, (PositionWithAim prevPoint, instruction) => prevPoint.move(instruction));

  print("Day 2 part 2: point $resultPart2, result ${resultPart2.x * resultPart2.y}");
}
