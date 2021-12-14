const input = @embedFile("../input.txt");
const sample = @embedFile("../sample.txt");
const std = @import("std");

const Rule = struct {
    firstChar: u8,
    secondChar: u8,
    result: u8,
};

const ParsedInput = struct {
    chain: []const u8,
    rules: std.ArrayList(Rule),
};

fn buildRuleListFromInput(input_data: []const u8) !ParsedInput {
    var result = std.ArrayList(Rule).init(std.heap.page_allocator);

    var paragraphIt = std.mem.tokenize(input_data, "\n\n");
    const firstPart = paragraphIt.next().?;
    const secondPart = paragraphIt.rest();
    var rulesIt = std.mem.tokenize(secondPart, "\n");
    while (rulesIt.next()) |line| {
        var ruleIt = std.mem.tokenize(line, "->");
        var leftRule = ruleIt.next().?;
        var rightRule = ruleIt.rest();
        var rule = Rule{
            .firstChar = leftRule[0],
            .secondChar = leftRule[1],
            .result = rightRule[1],
        };
        try result.append(rule);
    }

    return ParsedInput{
        .chain = firstPart,
        .rules = result,
    };
}

fn doOneSim(template: []const u8, rules: []const Rule) !std.ArrayList(u8) {
    var result = std.ArrayList(u8).init(std.heap.page_allocator);

    for (template) |c, index| {
        try result.append(c);
        if (index + 1 < template.len) {
            const nextChar = template[index + 1];
            var foundRule: ?Rule = null;
            for (rules) |rule| {
                if (rule.firstChar == c and rule.secondChar == nextChar) {
                    foundRule = rule;
                    break;
                }
            }
            if (foundRule) |rule| {
                try result.append(rule.result);
            }
        }
    }

    return result;
}

fn computeTemplateValue(template: []const u8) !usize {
    var h = std.AutoHashMap(u8, usize).init(std.heap.page_allocator);
    defer h.deinit();
    for (template) |c| {
        if (h.get(c)) |countC| {
            try h.put(c, countC + 1);
        } else {
            try h.put(c, 1);
        }
    }
    var maxValue: usize = std.math.minInt(usize);
    var minValue: usize = std.math.maxInt(usize);

    var it = h.valueIterator();
    while (it.next()) |v| {
        if (v.* > maxValue) {
            maxValue = v.*;
        }
        if (v.* < minValue) {
            minValue = v.*;
        }
    }
    return maxValue - minValue;
}

pub fn main() anyerror!void {
    const parsedInput = try buildRuleListFromInput(input);
    for (parsedInput.rules.items) |rule| {
        std.log.info("{} {} -> {}", .{ rule.firstChar, rule.secondChar, rule.result });
    }

    var oldTemplate = std.ArrayList(u8).init(std.heap.page_allocator);
    for (parsedInput.chain) |c| {
        try oldTemplate.append(c);
    }
    var iterIndex: usize = 0;
    while (iterIndex < 10) : (iterIndex += 1) {
        var newTemplate = try doOneSim(oldTemplate.items, parsedInput.rules.items);
        oldTemplate.deinit();
        std.log.info("After step {}: {s} {}", .{ iterIndex + 1, newTemplate.items, newTemplate.items.len });
        std.log.info("{}", .{computeTemplateValue(newTemplate.items)});
        oldTemplate = newTemplate;
    }
}
