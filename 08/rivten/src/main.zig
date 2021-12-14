const input = @embedFile("../input.txt");
//const input = @embedFile("../sample.txt");
const std = @import("std");

fn same(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;

    for (a) |aLetter| {
        var isLetterInB = false;
        for (b) |bLetter| {
            if (aLetter == bLetter) {
                isLetterInB = true;
                break;
            }
        }
        if (!isLetterInB) {
            return false;
        }
    }
    return true;
}

pub fn main() anyerror!void {
    var result: usize = 0;
    var line_iterator = std.mem.tokenize(input, "\n");
    while (line_iterator.next()) |line| {
        var pipe_iterator = std.mem.tokenize(line, "|");
        var left_of_pipe = pipe_iterator.next().?;
        var right_of_pipe = pipe_iterator.rest();

        var left_words = std.ArrayList([]const u8).init(std.heap.page_allocator);
        var left_words_iterator = std.mem.tokenize(left_of_pipe, " ");
        while (left_words_iterator.next()) |word| {
            try left_words.append(word);
        }

        var right_words = std.ArrayList([]const u8).init(std.heap.page_allocator);
        var right_words_iterator = std.mem.tokenize(right_of_pipe, " ");
        while (right_words_iterator.next()) |word| {
            try right_words.append(word);
        }

        //  0:      1:      2:      3:      4:
        // aaaa    ....    aaaa    aaaa    ....
        //b    c  .    c  .    c  .    c  b    c
        //b    c  .    c  .    c  .    c  b    c
        // ....    ....    dddd    dddd    dddd
        //e    f  .    f  e    .  .    f  .    f
        //e    f  .    f  e    .  .    f  .    f
        // gggg    ....    gggg    gggg    ....
        //
        //  5:      6:      7:      8:      9:
        // aaaa    aaaa    aaaa    aaaa    aaaa
        //b    .  b    .  .    c  b    c  b    c
        //b    .  b    .  .    c  b    c  b    c
        // dddd    dddd    ....    dddd    dddd
        //.    f  e    f  .    f  e    f  .    f
        //.    f  e    f  .    f  e    f  .    f
        // gggg    gggg    ....    gggg    gggg

        var one: []const u8 = undefined;
        var seven: []const u8 = undefined;
        var four: []const u8 = undefined;
        var eight: []const u8 = undefined;
        var twoThreeFiveLen: usize = 0;
        var twoThreeFive: [3][]const u8 = undefined;
        var zeroSixNineLen: usize = 0;
        var zeroSixNine: [3][]const u8 = undefined;

        for (left_words.items) |w| {
            if (w.len == 2) {
                one = w;
            } else if (w.len == 3) {
                seven = w;
            } else if (w.len == 4) {
                four = w;
            } else if (w.len == 5) {
                twoThreeFive[twoThreeFiveLen] = w;
                twoThreeFiveLen += 1;
            } else if (w.len == 6) {
                zeroSixNine[zeroSixNineLen] = w;
                zeroSixNineLen += 1;
            } else if (w.len == 7) {
                eight = w;
            }
        }

        std.log.info("one: {s}", .{one});
        std.log.info("seven: {s}", .{seven});
        std.log.info("four: {s}", .{four});
        std.log.info("eight: {s}", .{eight});
        //std.log.info("two/three/five: {s} {s} {s}", .{ twoThreeFive[0], twoThreeFive[1], twoThreeFive[2] });
        //std.log.info("zero/six/nine: {s} {s} {s}", .{ zeroSixNine[0], zeroSixNine[1], zeroSixNine[2] });
        var wireToA: u8 = undefined;
        for (seven) |sevenLetter| {
            var found = false;
            for (one) |oneLetter| {
                if (oneLetter == sevenLetter) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                wireToA = sevenLetter;
                break;
            }
        }
        //std.log.info("a <- {c}", .{wireToA});

        var three: []const u8 = undefined;
        var twoFiveLen: usize = 0;
        var twoFive: [2][]const u8 = undefined;
        for (twoThreeFive) |oneNumber| {
            var hasOneInIt = true;
            for (one) |oneLetter| {
                var found = false;
                for (oneNumber) |oneNumberLetter| {
                    if (oneLetter == oneNumberLetter) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    hasOneInIt = false;
                    break;
                }
            }
            if (hasOneInIt) {
                three = oneNumber;
            } else {
                twoFive[twoFiveLen] = oneNumber;
                twoFiveLen += 1;
            }
        }
        std.log.info("three: {s}", .{three});
        //std.log.info("two/five: {s} {s}", .{ twoFive[0], twoFive[1] });

        var nine: []const u8 = undefined;
        var zeroSixLen: usize = 0;
        var zeroSix: [2][]const u8 = undefined;
        for (zeroSixNine) |oneNumber| {
            var hasThreeInIt = true;
            for (three) |threeLetter| {
                var found = false;
                for (oneNumber) |oneNumberLetter| {
                    if (threeLetter == oneNumberLetter) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    hasThreeInIt = false;
                    break;
                }
            }
            if (hasThreeInIt) {
                nine = oneNumber;
            } else {
                zeroSix[zeroSixLen] = oneNumber;
                zeroSixLen += 1;
            }
        }
        std.log.info("nine: {s}", .{nine});
        //std.log.info("zero/six: {s} {s}", .{ zeroSix[0], zeroSix[1] });

        var wireToE: u8 = undefined;
        for (eight) |eightLetter| {
            var found = false;
            for (nine) |nineLetter| {
                if (nineLetter == eightLetter) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                wireToE = eightLetter;
                break;
            }
        }
        //std.log.info("e <- {c}", .{wireToE});

        var two: []const u8 = undefined;
        var five: []const u8 = undefined;
        for (twoFive) |number| {
            var hasWireToE = false;
            for (number) |letter| {
                if (letter == wireToE) {
                    hasWireToE = true;
                    break;
                }
            }
            if (hasWireToE) {
                two = number;
            } else {
                five = number;
            }
        }

        std.log.info("two: {s}", .{two});
        std.log.info("five: {s}", .{five});

        var wireToC: u8 = undefined;
        for (one) |oneLetter| {
            var found = false;
            for (two) |twoLetter| {
                if (oneLetter == twoLetter) {
                    found = true;
                    break;
                }
            }
            if (found) {
                wireToC = oneLetter;
                break;
            }
        }
        //std.log.info("c <- {c}", .{wireToC});
        var zero: []const u8 = undefined;
        var six: []const u8 = undefined;
        for (zeroSix) |number| {
            var hasWireToC = false;
            for (number) |letter| {
                if (letter == wireToC) {
                    hasWireToC = true;
                    break;
                }
            }
            if (hasWireToC) {
                zero = number;
            } else {
                six = number;
            }
        }

        std.log.info("zero: {s}", .{zero});
        std.log.info("six: {s}", .{six});
        var value: usize = 0;
        for (right_words.items) |w| {
            value *= 10;
            if (same(w, zero)) {
                value += 0;
            } else if (same(w, one)) {
                value += 1;
            } else if (same(w, two)) {
                value += 2;
            } else if (same(w, three)) {
                value += 3;
            } else if (same(w, four)) {
                value += 4;
            } else if (same(w, five)) {
                value += 5;
            } else if (same(w, six)) {
                value += 6;
            } else if (same(w, seven)) {
                value += 7;
            } else if (same(w, eight)) {
                value += 8;
            } else if (same(w, nine)) {
                value += 9;
            } else {
                unreachable;
            }
        }
        std.log.info("{}", .{value});
        result += value;
    }
    std.log.info("{}", .{result});
}
