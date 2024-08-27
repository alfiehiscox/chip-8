//                   A different tact
// ---------------------------------------------------------
// Using zig's composite structures to extract out the chip8
// emulator into a library so that it is more modular and
// testable.

const std = @import("std");
const testing = std.testing;

// Original Chip8 Screen was 64x32 Pixels.
const SCREEN_WIDTH = 64;
const SCREEN_HEIGHT = 32;
const DIMS = SCREEN_WIDTH * SCREEN_HEIGHT;

const FONT_START = 0x050;

const FONT: [80]u8 = [80]u8{
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80, // F
};

// There are a couple different versions of Chip8.
const EmulatorType = enum { COSMAC_VIP, SUPER_CHIP, CHIP_48 };

// Errors
const EmulatorError = error{
    OUT_OF_MEMORY,
    UNKNOWN_INSTRUCTION,
    UNKNOWN_REGISTER,
    NO_KEY_PRESSED,
    UNKNOWN_FONT_CHARACTER,
    STACK_ERROR,
};

// Options
pub const ChipOptions = struct {
    emulatorType: ?EmulatorType = null,
};

// The main emulator structure.
pub const Chip8 = struct {
    allocator: std.mem.Allocator,
    emulatorType: EmulatorType,

    screen: []u8,
    memory: []u8,

    pc: u16,
    i: u16,

    registers: []u8,

    stack: std.ArrayList(u16),

    // TODO: We need a renderer interface

    const This = @This();

    pub fn init(allocator: std.mem.Allocator, comptime options: ChipOptions) !This {
        const screen = try allocator.alloc(u8, DIMS);
        errdefer allocator.free(screen);

        const memory = try allocator.alloc(u8, 4096);
        errdefer allocator.free(memory);

        // Load the font into memory
        @memcpy(memory[FONT_START .. FONT_START + 80], &FONT);

        const registers = try allocator.alloc(u8, 16);
        @memset(registers, 0);
        errdefer allocator.free(registers);

        const stack = std.ArrayList(u16).init(allocator);
        errdefer stack.deinit();

        return .{
            .allocator = allocator,
            .emulatorType = options.emulatorType orelse EmulatorType.COSMAC_VIP,

            .screen = screen,
            .memory = memory,

            .pc = 0x200,
            .i = 0,

            .registers = registers,

            .stack = stack,
        };
    }

    pub fn run(self: *This, program: []u8) !void {
        self.load(program);
    }

    fn load(self: *This, program: []u8) void {
        self.pc = 0x200; // Programs always start at 0x200
        @memcpy(self.memory[self.pc .. self.pc + program.len], program);
    }

    // Fetch the next instruction
    fn fetchInstruction(self: *This) EmulatorError!Instruction {
        if (self.pc + 2 > self.memory.len) return EmulatorError.OUT_OF_MEMORY;

        // Get a 2 byte slice of MEM at PC
        const slice = self.memory[self.pc..(self.pc + 2)];
        self.pc += 2;

        // Convert into a u16 for easy masking
        const opcode = @as(u16, @intCast(slice[0])) << 8 | @as(u16, @intCast(slice[1]));

        // Mask off the first nibble and switch on it
        const category: u16 = opcode & 0xF000;

        // Depending on the above parse the rest of the instruction.
        const instruction: Instruction = switch (category) {
            0x0000 => switch (opcode) {
                0x00E0 => Instruction{ .CLEAR_SCREEN = {} },
                0x00EE => Instruction{ .RETURN = {} },
                else => {
                    std.debug.print("Unknown instruction '{X:4}'\n", .{opcode});
                    return EmulatorError.UNKNOWN_INSTRUCTION;
                },
            },
            0x1000 => Instruction{ .JUMP = opcode & 0x0FFF },
            0x2000 => Instruction{ .JUMP_SUBROUTINE = opcode & 0x0FFF },
            0x3000 => Instruction{ .EQUAL_TO = .{
                @as(u8, @intCast((opcode & 0x0F00) >> 8)),
                @as(u8, @intCast(opcode & 0x00FF)),
            } },
            0x4000 => Instruction{ .NOT_EQUAL_TO = .{
                @as(u8, @intCast((opcode & 0x0F00) >> 8)),
                @as(u8, @intCast(opcode & 0x00FF)),
            } },
            0x5000 => Instruction{ .EQUAL_REGISTERS = .{
                @as(u8, @intCast((opcode & 0x0F00) >> 8)),
                @as(u8, @intCast((opcode & 0x00F0) >> 4)),
            } },
            0x6000 => Instruction{ .SET_REGISTER = .{
                @as(u8, @intCast((opcode & 0x0F00) >> 8)),
                @as(u8, @intCast(opcode & 0x00FF)),
            } },
            0x7000 => Instruction{ .ADD_REGISTER = .{
                @as(u8, @intCast((opcode & 0x0F00) >> 8)),
                @as(u8, @intCast(opcode & 0x00FF)),
            } },
            0x8000 => try parse_08(opcode),
            0x9000 => Instruction{ .NOT_EQUAL_REGISTERS = .{
                @as(u8, @intCast((opcode & 0x0F00) >> 8)),
                @as(u8, @intCast((opcode & 0x00F0) >> 4)),
            } },
            0xA000 => Instruction{ .SET_INDEX = opcode & 0x0FFF },
            0xB000 => Instruction{ .JUMP_WITH_OFFSET = opcode & 0x0FFF },
            0xC000 => Instruction{ .RANDOM = .{
                @as(u8, @intCast((opcode & 0x0F00) >> 8)),
                @as(u8, @intCast(opcode & 0x00FF)),
            } },
            0xD000 => Instruction{ .DRAW = .{
                @as(u8, @intCast((opcode & 0x0F00) >> 8)),
                @as(u8, @intCast((opcode & 0x00F0) >> 4)),
                @as(u8, @intCast(opcode & 0x000F)),
            } },
            0xE000 => switch (opcode & 0x00FF) {
                0x009E => Instruction{ .SKIP_IF_KEY = @as(u8, @intCast((opcode & 0x0F00) >> 8)) },
                0x00A1 => Instruction{ .SKIP_IF_NOT_KEY = @as(u8, @intCast((opcode & 0x0F00) >> 8)) },
                else => return EmulatorError.UNKNOWN_INSTRUCTION,
            },
            0xF000 => try parse_0F(opcode),
            else => {
                std.debug.print("Unknown instruction '{X}'\n", .{opcode});
                return EmulatorError.UNKNOWN_INSTRUCTION;
            },
        };

        return instruction;
    }

    inline fn parse_08(opcode: u16) EmulatorError!Instruction {
        const subcategory = @as(u8, @intCast(opcode & 0x000F));
        return switch (subcategory) {
            0x00 => Instruction{ .SET_X_Y = .{
                @as(u8, @intCast((opcode & 0x0F00) >> 8)),
                @as(u8, @intCast((opcode & 0x00F0) >> 4)),
            } },
            0x01 => Instruction{ .OR_X_Y = .{
                @as(u8, @intCast((opcode & 0x0F00) >> 8)),
                @as(u8, @intCast((opcode & 0x00F0) >> 4)),
            } },
            0x02 => Instruction{ .AND_X_Y = .{
                @as(u8, @intCast((opcode & 0x0F00) >> 8)),
                @as(u8, @intCast((opcode & 0x00F0) >> 4)),
            } },
            0x03 => Instruction{ .XOR_X_Y = .{
                @as(u8, @intCast((opcode & 0x0F00) >> 8)),
                @as(u8, @intCast((opcode & 0x00F0) >> 4)),
            } },
            0x04 => Instruction{ .ADD_X_Y = .{
                @as(u8, @intCast((opcode & 0x0F00) >> 8)),
                @as(u8, @intCast((opcode & 0x00F0) >> 4)),
            } },
            0x05 => Instruction{ .SUB_X_Y = .{
                @as(u8, @intCast((opcode & 0x0F00) >> 8)),
                @as(u8, @intCast((opcode & 0x00F0) >> 4)),
            } },
            0x06 => Instruction{ .SHIFT_R_X_Y = .{
                @as(u8, @intCast((opcode & 0x0F00) >> 8)),
                @as(u8, @intCast((opcode & 0x00F0) >> 4)),
            } },
            0x07 => Instruction{ .SUB_Y_X = .{
                @as(u8, @intCast((opcode & 0x0F00) >> 8)),
                @as(u8, @intCast((opcode & 0x00F0) >> 4)),
            } },
            0x0E => Instruction{ .SHIFT_L_X_Y = .{
                @as(u8, @intCast((opcode & 0x0F00) >> 8)),
                @as(u8, @intCast((opcode & 0x00F0) >> 4)),
            } },
            else => {
                std.debug.print("Unknown instruction '{X}'\n", .{opcode});
                return EmulatorError.UNKNOWN_INSTRUCTION;
            },
        };
    }

    inline fn parse_0F(opcode: u16) EmulatorError!Instruction {
        return switch (opcode & 0x00FF) {
            0x0007 => Instruction{ .SET_REGISTER_FROM_DELAY = @as(u8, @intCast((opcode & 0x0F00) >> 8)) },
            0x0015 => Instruction{ .SET_DELAY_FROM_REGISTER = @as(u8, @intCast((opcode & 0x0F00) >> 8)) },
            0x0018 => Instruction{ .SET_SOUND_FROM_REGISTER = @as(u8, @intCast((opcode & 0x0F00) >> 8)) },
            0x001E => Instruction{ .ADD_INDEX = @as(u8, @intCast((opcode & 0x0F00) >> 8)) },
            0x000A => Instruction{ .BLOCK_KEY_PRESS = @as(u8, @intCast((opcode & 0x0F00) >> 8)) },
            0x0029 => Instruction{ .FONT_CHARACTER = @as(u8, @intCast((opcode & 0x0F00) >> 8)) },
            0x0033 => Instruction{ .BINARY_CODED_CONV = @as(u8, @intCast((opcode & 0x0F00) >> 8)) },
            0x0055 => Instruction{ .STORE = @as(u8, @intCast((opcode & 0x0F00) >> 8)) },
            0x0065 => Instruction{ .LOAD = @as(u8, @intCast((opcode & 0x0F00) >> 8)) },
            else => return EmulatorError.UNKNOWN_INSTRUCTION,
        };
    }

    fn executeInstruction(self: *This, instruction: Instruction) EmulatorError!void {
        switch (instruction) {
            Instruction.CLEAR_SCREEN => @memset(self.screen, 0),
            Instruction.RETURN => |_| self.pc = self.stack.popOrNull() orelse return EmulatorError.STACK_ERROR,
            Instruction.JUMP => |addr| self.pc = addr,
            Instruction.JUMP_SUBROUTINE => |addr| {
                self.stack.append(self.pc) catch return EmulatorError.STACK_ERROR;
                self.pc = addr;
            },
            else => return EmulatorError.UNKNOWN_INSTRUCTION,
        }
    }

    fn setRegisterValue(self: *This, register: u8, value: u8) EmulatorError!void {
        if (register < 0x00 or register > 0x0F) return EmulatorError.UNKNOWN_REGISTER;
        self.registers[register] = value;
    }

    fn getRegisterValue(self: *This, register: u8) EmulatorError!u8 {
        if (register < 0x00 or register > 0x0F) return EmulatorError.UNKNOWN_REGISTER;
        return self.registers[register];
    }

    pub fn deinit(self: This) void {
        self.allocator.free(self.memory);
        self.allocator.free(self.screen);
        self.allocator.free(self.registers);
        self.stack.deinit();
    }
};

// Instructions
const Instruction = union(enum) {
    CLEAR_SCREEN: void, // 0x00E0
    RETURN: void, // 0x00EE
    JUMP: u16, // 0x1NNN
    JUMP_SUBROUTINE: u16, // 0x2NNN
    EQUAL_TO: struct { u8, u8 }, // 0x3XNN
    NOT_EQUAL_TO: struct { u8, u8 }, // 0x4XNN
    EQUAL_REGISTERS: struct { u8, u8 }, // 0x5XY0
    NOT_EQUAL_REGISTERS: struct { u8, u8 }, // 0x9XY0
    SET_REGISTER: struct { u8, u8 }, // 0x6XNN
    ADD_REGISTER: struct { u8, u8 }, // 0x7XNN
    SET_X_Y: struct { u8, u8 }, // 0x8XY0
    OR_X_Y: struct { u8, u8 }, // 0x8XY1
    AND_X_Y: struct { u8, u8 }, // 0x8XY2
    XOR_X_Y: struct { u8, u8 }, // 0x8XY3
    ADD_X_Y: struct { u8, u8 }, // 0x8XY4
    SUB_X_Y: struct { u8, u8 }, // 0x8XY5
    SHIFT_R_X_Y: struct { u8, u8 }, //0x8XY6
    SUB_Y_X: struct { u8, u8 }, // 0x8XY7
    SHIFT_L_X_Y: struct { u8, u8 }, //0x8XYE
    SET_INDEX: u16, // 0xANNN
    JUMP_WITH_OFFSET: u16, // 0xBNNN or 0xBXNN
    RANDOM: struct { u8, u8 }, // 0xCXNN
    DRAW: struct { u8, u8, u8 }, // 0xDXYN
    SKIP_IF_KEY: u8, // 0xEX9E
    SKIP_IF_NOT_KEY: u8, // 0xEXA1
    SET_REGISTER_FROM_DELAY: u8, // 0xFX07
    SET_DELAY_FROM_REGISTER: u8, // 0xFX15
    SET_SOUND_FROM_REGISTER: u8, // 0xFX18
    ADD_INDEX: u8, // 0xFX1E
    BLOCK_KEY_PRESS: u8, // 0xFX0A
    FONT_CHARACTER: u8, // 0xFX29
    BINARY_CODED_CONV: u8, // 0xFX33
    STORE: u8, // 0xFX55
    LOAD: u8, // 0xFX65
};

// ======== TESTING ==========
test "Chip 8 Init" {
    const chip8 = try Chip8.init(testing.allocator, .{});
    defer chip8.deinit();
}

test "Fetch Instruction" {
    // All opcodes in order of Instruction Enum
    var opcodes: [68]u8 = [_]u8{
        0x00, 0xE0, 0x00, 0xEE, 0x11, 0x23, 0x21, 0x23,
        0x3A, 0xBC, 0x4A, 0xBC, 0x5A, 0xBC, 0x9A, 0xBC,
        0x6A, 0xBC, 0x7A, 0xBC, 0x8A, 0xB0, 0x8A, 0xB1,
        0x8A, 0xB2, 0x8A, 0xB3, 0x8A, 0xB4, 0x8A, 0xB5,
        0x8A, 0xB6, 0x8A, 0xB7, 0x8A, 0xBE, 0xA1, 0x23,
        0xBA, 0xBC, 0xCA, 0xBC, 0xDA, 0xB4, 0xE1, 0x9E,
        0xE1, 0xA1, 0xF1, 0x07, 0xF1, 0x15, 0xF1, 0x18,
        0xF1, 0x1E, 0xF1, 0x0A, 0xF1, 0x29, 0xF1, 0x33,
        0xF1, 0x55, 0xF1, 0x65,
    };

    // All instructions as matching the above
    const instructions: [34]Instruction = [_]Instruction{
        Instruction{ .CLEAR_SCREEN = {} },
        Instruction{ .RETURN = {} },
        Instruction{ .JUMP = 0x123 },
        Instruction{ .JUMP_SUBROUTINE = 0x123 },
        Instruction{ .EQUAL_TO = .{ 0x0A, 0xBC } },
        Instruction{ .NOT_EQUAL_TO = .{ 0x0A, 0xBC } },
        Instruction{ .EQUAL_REGISTERS = .{ 0x0A, 0x0B } },
        Instruction{ .NOT_EQUAL_REGISTERS = .{ 0x0A, 0x0B } },
        Instruction{ .SET_REGISTER = .{ 0x0A, 0xBC } },
        Instruction{ .ADD_REGISTER = .{ 0x0A, 0xBC } },
        Instruction{ .SET_X_Y = .{ 0x0A, 0x0B } },
        Instruction{ .OR_X_Y = .{ 0x0A, 0x0B } },
        Instruction{ .AND_X_Y = .{ 0x0A, 0x0B } },
        Instruction{ .XOR_X_Y = .{ 0x0A, 0x0B } },
        Instruction{ .ADD_X_Y = .{ 0x0A, 0x0B } },
        Instruction{ .SUB_X_Y = .{ 0x0A, 0x0B } },
        Instruction{ .SHIFT_R_X_Y = .{ 0x0A, 0x0B } },
        Instruction{ .SUB_Y_X = .{ 0x0A, 0x0B } },
        Instruction{ .SHIFT_L_X_Y = .{ 0x0A, 0x0B } },
        Instruction{ .SET_INDEX = 0x123 },
        Instruction{ .JUMP_WITH_OFFSET = 0x0ABC },
        Instruction{ .RANDOM = .{ 0x0A, 0xBC } },
        Instruction{ .DRAW = .{ 0x0A, 0x0B, 0x04 } },
        Instruction{ .SKIP_IF_KEY = 0x01 },
        Instruction{ .SKIP_IF_NOT_KEY = 0x01 },
        Instruction{ .SET_REGISTER_FROM_DELAY = 0x01 },
        Instruction{ .SET_DELAY_FROM_REGISTER = 0x01 },
        Instruction{ .SET_SOUND_FROM_REGISTER = 0x01 },
        Instruction{ .ADD_INDEX = 0x01 },
        Instruction{ .BLOCK_KEY_PRESS = 0x01 },
        Instruction{ .FONT_CHARACTER = 0x01 },
        Instruction{ .BINARY_CODED_CONV = 0x01 },
        Instruction{ .STORE = 0x01 },
        Instruction{ .LOAD = 0x01 },
    };

    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.load(&opcodes);

    for (instructions) |expected| {
        // std.debug.print("{}\n", .{expected});
        const actual = try emulator.fetchInstruction();
        try testing.expectEqualDeep(expected, actual);
    }
}

test "Execute Clear Screen (0x00E0)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    // Fill Screen with Junk
    for (emulator.screen) |*pixel| {
        pixel.* = 1;
    }

    try emulator.executeInstruction(Instruction{ .CLEAR_SCREEN = {} });

    for (emulator.screen) |pixel| {
        try testing.expectEqual(0, pixel);
    }
}

test "Execute Return (0x00EE)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    // Push a value onto the stack
    try emulator.stack.append(0x025);

    try emulator.executeInstruction(Instruction{ .RETURN = {} });

    try testing.expectEqual(0x025, emulator.pc);
}

test "Execute Jump (0x1NNN)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    var opcode: [2]u8 = .{ 0x11, 0x23 };
    emulator.load(&opcode);
    const instruction = try emulator.fetchInstruction();
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x123, emulator.pc);
}

test "Execute Jump With Subroutine (0x2NNN)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    var opcode: [2]u8 = .{ 0x21, 0x23 };
    emulator.load(&opcode);
    const instruction = try emulator.fetchInstruction();
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x123, emulator.pc);
    try testing.expectEqual(emulator.stack.items.len, 1);
    try testing.expectEqual(emulator.stack.items[0], 0x202);
}
