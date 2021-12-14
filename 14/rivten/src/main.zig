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

fn doOneSim(template: Template, rules: []const Rule) !Template {
    var result = Template{
        .bi = try template.bi.clone(),
        .elemCount = try template.elemCount.clone(),
    };

    for (rules) |rule| {
        var bi = [2]u8{ rule.firstChar, rule.secondChar };
        if (template.bi.get(bi)) |count| {
            var biA = [2]u8{ rule.firstChar, rule.result };
            var biB = [2]u8{ rule.result, rule.secondChar };
            var getOrPutResultA = try result.bi.getOrPut(biA);
            if (getOrPutResultA.found_existing) {
                getOrPutResultA.value_ptr.* += count;
            } else {
                getOrPutResultA.value_ptr.* = count;
            }
            var getOrPutResultB = try result.bi.getOrPut(biB);
            if (getOrPutResultB.found_existing) {
                getOrPutResultB.value_ptr.* += count;
            } else {
                getOrPutResultB.value_ptr.* = count;
            }

            var getOrPutResultBi = try result.bi.getOrPut(bi);
            if (getOrPutResultBi.found_existing) {
                getOrPutResultBi.value_ptr.* -= count;
            }

            var gopr = try result.elemCount.getOrPut(rule.result);
            if (gopr.found_existing) {
                gopr.value_ptr.* += count;
            } else {
                gopr.value_ptr.* = count;
            }
        }
    }

    return result;
}

const Template = struct {
    bi: std.AutoHashMap([2]u8, usize),
    elemCount: std.AutoHashMap(u8, usize),
};

fn computeTemplateValue(h: std.AutoHashMap(u8, usize)) !usize {
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

fn buildTemplateFromInput(i: []const u8) !Template {
    var oldTemplate = Template{
        .bi = std.AutoHashMap([2]u8, usize).init(std.heap.page_allocator),
        .elemCount = std.AutoHashMap(u8, usize).init(std.heap.page_allocator),
    };
    for (i) |c, cIndex| {
        if (cIndex + 1 < i.len) {
            var bi = [2]u8{ c, i[cIndex + 1] };
            var getOrPutResult = try oldTemplate.bi.getOrPut(bi);
            if (getOrPutResult.found_existing) {
                getOrPutResult.value_ptr.* += 1;
            } else {
                getOrPutResult.value_ptr.* = 1;
            }
        }

        var gopr = try oldTemplate.elemCount.getOrPut(c);
        if (gopr.found_existing) {
            gopr.value_ptr.* += 1;
        } else {
            gopr.value_ptr.* = 1;
        }
    }
    return oldTemplate;
}

pub fn main() anyerror!void {
    const parsedInput = try buildRuleListFromInput(input);
    for (parsedInput.rules.items) |rule| {
        std.log.info("{} {} -> {}", .{ rule.firstChar, rule.secondChar, rule.result });
    }

    var oldTemplate = try buildTemplateFromInput(parsedInput.chain);
    var iterIndex: usize = 0;
    while (iterIndex < 40) : (iterIndex += 1) {
        var newTemplate = try doOneSim(oldTemplate, parsedInput.rules.items);
        oldTemplate.bi.deinit();
        oldTemplate.elemCount.deinit();
        std.log.info("{} {}", .{ iterIndex + 1, computeTemplateValue(newTemplate.elemCount) });
        oldTemplate = newTemplate;
    }
}
