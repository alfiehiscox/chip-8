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
pub const EmulatorOptions = struct {
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

    delay: u8,
    sound: u8,

    // TODO: We need a renderer interface

    const This = @This();

    pub fn init(allocator: std.mem.Allocator, comptime options: EmulatorOptions) !This {
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

            .delay = 0,
            .sound = 0,
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
            Instruction.EQUAL_TO => |v| self.pc += if (try self.getRegisterValue(v[0]) == v[1]) 2 else 0,
            Instruction.NOT_EQUAL_TO => |v| self.pc += if (try self.getRegisterValue(v[0]) != v[1]) 2 else 0,
            Instruction.EQUAL_REGISTERS => |v| self.pc += if (try self.getRegisterValue(v[0]) == try self.getRegisterValue(v[1])) 2 else 0,
            Instruction.NOT_EQUAL_REGISTERS => |v| self.pc += if (try self.getRegisterValue(v[0]) != try self.getRegisterValue(v[1])) 2 else 0,
            Instruction.SET_REGISTER => |v| try self.setRegisterValue(v[0], v[1]),
            Instruction.ADD_REGISTER => |v| try self.setRegisterValue(v[0], try self.getRegisterValue(v[0]) +% v[1]),
            Instruction.SET_X_Y => |v| try self.setRegisterValue(v[0], try self.getRegisterValue(v[1])),
            Instruction.OR_X_Y => |v| try self.setRegisterValue(v[0], try self.getRegisterValue(v[0]) | try self.getRegisterValue(v[1])),
            Instruction.AND_X_Y => |v| try self.setRegisterValue(v[0], try self.getRegisterValue(v[0]) & try self.getRegisterValue(v[1])),
            Instruction.XOR_X_Y => |v| try self.setRegisterValue(v[0], try self.getRegisterValue(v[0]) ^ try self.getRegisterValue(v[1])),
            Instruction.ADD_X_Y => |v| {
                const x = try self.getRegisterValue(v[0]);
                const y = try self.getRegisterValue(v[1]);
                const add = x +% y;
                self.registers[0x0F] = if (add < x) 1 else 0;
                try self.setRegisterValue(v[0], add);
            },
            Instruction.SUB_X_Y => |v| {
                const x = try self.getRegisterValue(v[0]);
                const y = try self.getRegisterValue(v[1]);
                self.registers[0x0F] = if (x > y) 1 else 0;
                const sub = x -% y;
                try self.setRegisterValue(v[0], sub);
            },
            Instruction.SUB_Y_X => |v| {
                const x = try self.getRegisterValue(v[0]);
                const y = try self.getRegisterValue(v[1]);
                self.registers[0x0F] = if (y > x) 1 else 0;
                const sub = y -% x;
                try self.setRegisterValue(v[0], sub);
            },
            Instruction.SHIFT_R_X_Y => |v| {
                switch (self.emulatorType) {
                    // Only the COSMAC_VIP has different behaviour
                    EmulatorType.COSMAC_VIP => {
                        const y = try self.getRegisterValue(v[1]);
                        self.registers[0x0F] = @as(u8, @intCast(y & 0b00000001)); // This will be the carry bit
                        try self.setRegisterValue(v[0], (y >> 1));
                    },
                    else => {
                        const x = try self.getRegisterValue(v[0]);
                        self.registers[0x0F] = @as(u8, @intCast(x & 0b00000001)); // This will be the carry bit
                        try self.setRegisterValue(v[0], (x >> 1));
                    },
                }
            },
            Instruction.SHIFT_L_X_Y => |v| {
                switch (self.emulatorType) {
                    // Only the COSMAC_VIP has different behaviour
                    EmulatorType.COSMAC_VIP => {
                        const y = try self.getRegisterValue(v[1]);
                        self.registers[0x0F] = @as(u8, @intCast((y & 0b10000000) >> 7)); // This will be the carry bit
                        try self.setRegisterValue(v[0], (y << 1));
                    },
                    else => {
                        const x = try self.getRegisterValue(v[0]);
                        self.registers[0x0F] = @as(u8, @intCast((x & 0b10000000) >> 7)); // This will be the carry bit
                        try self.setRegisterValue(v[0], (x << 1));
                    },
                }
            },
            Instruction.SET_INDEX => |addr| self.i = addr,
            Instruction.JUMP_WITH_OFFSET => |addr| {
                switch (self.emulatorType) {
                    EmulatorType.COSMAC_VIP => {
                        const offset = try self.getRegisterValue(0);
                        self.pc = offset + addr;
                    },
                    else => {
                        const register = @as(u8, @intCast((addr & 0x0F00) >> 8));
                        const offset = try self.getRegisterValue(register);
                        self.pc = offset + addr;
                    },
                }
            },
            Instruction.SET_REGISTER_FROM_DELAY => |reg| try self.setRegisterValue(reg, self.delay),
            Instruction.SET_DELAY_FROM_REGISTER => |reg| self.delay = try self.getRegisterValue(reg),
            Instruction.SET_SOUND_FROM_REGISTER => |reg| self.sound = try self.getRegisterValue(reg),
            Instruction.ADD_INDEX => |reg| {
                self.i += try self.getRegisterValue(reg);
                if (self.i > 0x0FFF) self.registers[0x0F] = 1; // This is extra behaviour
            },
            //..//
            Instruction.FONT_CHARACTER => |reg| {
                const char = (try self.getRegisterValue(reg)) & 0x0F;
                if (char >= 16) return EmulatorError.UNKNOWN_FONT_CHARACTER;
                self.i = FONT_START + char * 5;
            },
            Instruction.BINARY_CODED_CONV => |reg| {
                const value = try self.getRegisterValue(reg);
                const addr = self.i;

                self.memory[addr] = @divFloor(value, 100); // Hundreds
                self.memory[addr + 1] = @divFloor(@mod(value, 100), 10); // Tens
                self.memory[addr + 2] = @mod(value, 10); // Ones
            },
            Instruction.STORE => |max_reg| {
                switch (self.emulatorType) {
                    EmulatorType.COSMAC_VIP => {
                        for (0..max_reg + 1) |n| {
                            const int_n = @as(u8, @intCast(n));
                            const value = try self.getRegisterValue(int_n);
                            self.memory[self.i] = value;
                            self.i += 1;
                        }
                    },
                    else => {
                        var n: u8 = 0;
                        while (n <= max_reg) : (n += 1) {
                            const value = try self.getRegisterValue(n);
                            self.memory[self.i + n] = value;
                        }
                    },
                }
            },
            Instruction.LOAD => |max_reg| {
                switch (self.emulatorType) {
                    EmulatorType.COSMAC_VIP => {
                        for (0..max_reg + 1) |n| {
                            const int_n = @as(u8, @intCast(n));
                            const value = self.memory[self.i];
                            try self.setRegisterValue(int_n, value);
                            self.i += 1;
                        }
                    },
                    else => {
                        var n: u8 = 0;
                        while (n <= max_reg) : (n += 1) {
                            const value = self.memory[self.i + n];
                            try self.setRegisterValue(n, value);
                        }
                    },
                }
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

// ===================================================================
// ============================ TESTING ==============================
// ===================================================================

test "Chip 8 Init" {
    const chip8 = try Chip8.init(testing.allocator, .{});
    defer chip8.deinit();
}

test "Fetch All Instruction" {
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

test "Execute Equal Too (0x3XNN)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[1] = 0xFF;

    var opcode: [8]u8 = .{ 0x31, 0xFF, 0x00, 0xE0, 0x31, 0xAA, 0x00, 0xE0 };
    emulator.load(&opcode);

    const instruction1 = try emulator.fetchInstruction();
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0x204, emulator.pc);

    const instruction2 = try emulator.fetchInstruction();
    try emulator.executeInstruction(instruction2);

    try testing.expectEqual(0x206, emulator.pc);
}

test "Execute Equal Too (0x4XNN)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[1] = 0xAA;

    var opcodes: [8]u8 = .{ 0x41, 0xFF, 0x00, 0xE0, 0x41, 0xAA, 0x00, 0xE0 };
    emulator.load(&opcodes);

    const instruction1 = try emulator.fetchInstruction();
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0x204, emulator.pc);

    const instruction2 = try emulator.fetchInstruction();
    try emulator.executeInstruction(instruction2);

    try testing.expectEqual(0x206, emulator.pc);
}

test "Execute Equal Registers (0x5XY0)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[1] = 0xAA;
    emulator.registers[2] = 0xAA;
    emulator.registers[3] = 0xBB;

    var opcodes: [8]u8 = .{ 0x51, 0x20, 0x00, 0xE0, 0x51, 0x30, 0x00, 0xE0 };
    emulator.load(&opcodes);

    const instruction1 = try emulator.fetchInstruction();
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0x204, emulator.pc);

    const instruction2 = try emulator.fetchInstruction();
    try emulator.executeInstruction(instruction2);

    try testing.expectEqual(0x206, emulator.pc);
}

test "Execute Not Equal Registers (0x9XY0)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[1] = 0xBB;
    emulator.registers[2] = 0xAA;
    emulator.registers[3] = 0xBB;

    var opcodes: [8]u8 = .{ 0x91, 0x20, 0x00, 0xE0, 0x91, 0x30, 0x00, 0xE0 };
    emulator.load(&opcodes);

    const instruction1 = try emulator.fetchInstruction();
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0x204, emulator.pc);

    const instruction2 = try emulator.fetchInstruction();
    try emulator.executeInstruction(instruction2);

    try testing.expectEqual(0x206, emulator.pc);
}

test "Execute Set Register (0x6XNN)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    const instruction = Instruction{ .SET_REGISTER = .{ 0x01, 0xFF } };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0xFF, emulator.registers[1]);
}

test "Execute Add Register (0x7XNN)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    // Non Wrapping
    emulator.registers[1] = 0x02;

    const instruction1 = Instruction{ .ADD_REGISTER = .{ 0x01, 0x05 } };
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0x07, emulator.registers[1]);

    // Wrapping
    emulator.registers[1] = 0xFF;

    const instruction2 = Instruction{ .ADD_REGISTER = .{ 0x01, 0x05 } };
    try emulator.executeInstruction(instruction2);

    try testing.expectEqual(0x04, emulator.registers[1]);
}

test "Execute Set X to Y (0x8XY0)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[1] = 0x02;
    emulator.registers[0] = 0xFF;

    const instruction1 = Instruction{ .SET_X_Y = .{ 0x01, 0x00 } };
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0xFF, emulator.registers[1]);
}

// OR_X_Y: struct { u8, u8 }, // 0x8XY1
test "Execute Or X and Y (0x8XY1)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[1] = 0x02;
    emulator.registers[0] = 0x05;

    const instruction1 = Instruction{ .OR_X_Y = .{ 0x01, 0x00 } };
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0x07, emulator.registers[1]);
}

// AND_X_Y: struct { u8, u8 }, // 0x8XY2
test "Execute AND X and Y (0x8XY2)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[1] = 0x03;
    emulator.registers[0] = 0x05;

    const instruction1 = Instruction{ .AND_X_Y = .{ 0x01, 0x00 } };
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0x01, emulator.registers[1]);
}

// XOR_X_Y: struct { u8, u8 }, // 0x8XY3
test "Execute XOR X and Y (0x8XY3)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[1] = 0x03;
    emulator.registers[0] = 0x05;

    const instruction1 = Instruction{ .XOR_X_Y = .{ 0x01, 0x00 } };
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0x06, emulator.registers[1]);
}

// ADD_X_Y: struct { u8, u8 }, // 0x8XY4
test "Execute ADD X and Y with Overflow (0x8XY4)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    // Non wrapping
    emulator.registers[1] = 0x03;
    emulator.registers[0] = 0x05;

    const instruction1 = Instruction{ .ADD_X_Y = .{ 0x01, 0x00 } };
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0x08, emulator.registers[1]);
    try testing.expectEqual(0x00, emulator.registers[0xF]);

    // Wrapping
    emulator.registers[1] = 0xFE;
    emulator.registers[0] = 0x05;

    const instruction2 = Instruction{ .ADD_X_Y = .{ 0x01, 0x00 } };
    try emulator.executeInstruction(instruction2);

    try testing.expectEqual(0x03, emulator.registers[1]);
    try testing.expectEqual(0x01, emulator.registers[0xF]);
}

// SUB_X_Y: struct { u8, u8 }, // 0x8XY5
test "Execute SUB X and Y with Overflow (0x8XY5)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    // Non wrapping
    emulator.registers[1] = 0x05;
    emulator.registers[0] = 0x03;

    const instruction1 = Instruction{ .SUB_X_Y = .{ 0x01, 0x00 } };
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0x02, emulator.registers[1]);
    try testing.expectEqual(0x01, emulator.registers[0xF]);

    // Wrapping
    emulator.registers[1] = 0x01;
    emulator.registers[0] = 0x05;

    const instruction2 = Instruction{ .SUB_X_Y = .{ 0x01, 0x00 } };
    try emulator.executeInstruction(instruction2);

    try testing.expectEqual(0xFC, emulator.registers[1]);
    try testing.expectEqual(0x00, emulator.registers[0xF]);
}

// SUB_Y_X: struct { u8, u8 }, // 0x8XY7
test "Execute SUB Y and X with Overflow (0x8XY7)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    // Non wrapping
    emulator.registers[0] = 0x05;
    emulator.registers[1] = 0x03;

    const instruction1 = Instruction{ .SUB_Y_X = .{ 0x01, 0x00 } };
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0x02, emulator.registers[1]);
    try testing.expectEqual(0x01, emulator.registers[0xF]);

    // Wrapping
    emulator.registers[0] = 0x01;
    emulator.registers[1] = 0x05;

    const instruction2 = Instruction{ .SUB_Y_X = .{ 0x01, 0x00 } };
    try emulator.executeInstruction(instruction2);

    try testing.expectEqual(0xFC, emulator.registers[1]);
    try testing.expectEqual(0x00, emulator.registers[0xF]);
}

// SHIFT_R_X_Y: struct { u8, u8 }, //0x8XY6
test "Execute Shift Right X and Y [COSMAC-VIP] (0x8XY6)" {
    var emulator = try Chip8.init(
        testing.allocator,
        EmulatorOptions{ .emulatorType = EmulatorType.COSMAC_VIP },
    );
    defer emulator.deinit();

    emulator.registers[0] = 0x11;

    const instruction = Instruction{ .SHIFT_R_X_Y = .{ 0x01, 0x00 } };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x01, emulator.registers[0x0F]); // Carry Bit
    try testing.expectEqual(0x08, emulator.registers[1]);
}

test "Execute Shift Right X and Y [CHIP-48 and SUPER-CHIP] (0x8XY6)" {
    var emulator = try Chip8.init(
        testing.allocator,
        EmulatorOptions{ .emulatorType = EmulatorType.SUPER_CHIP },
    );
    defer emulator.deinit();

    emulator.registers[1] = 0x10;

    const instruction = Instruction{ .SHIFT_R_X_Y = .{ 0x01, 0x00 } };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x00, emulator.registers[0x0F]); // Carry Bit
    try testing.expectEqual(0x08, emulator.registers[1]);
}

// SHIFT_L_X_Y: struct { u8, u8 }, //0x8XYE
test "Execute Shift Left X and Y [COSMAC-VIP] (0x8XYE)" {
    var emulator = try Chip8.init(
        testing.allocator,
        EmulatorOptions{ .emulatorType = EmulatorType.COSMAC_VIP },
    );
    defer emulator.deinit();

    emulator.registers[0] = 0x81;

    const instruction = Instruction{ .SHIFT_L_X_Y = .{ 0x01, 0x00 } };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x01, emulator.registers[0x0F]); // Carry Bit
    try testing.expectEqual(0x02, emulator.registers[1]);
}

test "Execute Shift Left X and Y [CHIP-48 and SUPER-CHIP] (0x8XYE)" {
    var emulator = try Chip8.init(
        testing.allocator,
        EmulatorOptions{ .emulatorType = EmulatorType.CHIP_48 },
    );
    defer emulator.deinit();

    emulator.registers[1] = 0x80;

    const instruction = Instruction{ .SHIFT_L_X_Y = .{ 0x01, 0x00 } };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x01, emulator.registers[0x0F]); // Carry Bit
    try testing.expectEqual(0x00, emulator.registers[1]);
}

// SET_INDEX: u16, // 0xANNN
test "Execute Set Index (0xANNN)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    const instruction = Instruction{ .SET_INDEX = 0x333 };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x333, emulator.i);
}

// JUMP_WITH_OFFSET: u16, // 0xBNNN or 0xBXNN
test "Execute Jump With Offset (0xBNNN) [COSMAC-VIP]" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[0] = 0x11;

    const instruction = Instruction{ .JUMP_WITH_OFFSET = 0x333 };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x344, emulator.pc);
}

test "Execute Jump With Offset (0xBXNN) [CHIP-48 and SUPER-CHIP]" {
    var emulator = try Chip8.init(testing.allocator, EmulatorOptions{
        .emulatorType = EmulatorType.CHIP_48,
    });
    defer emulator.deinit();

    emulator.registers[3] = 0x11;

    const instruction = Instruction{ .JUMP_WITH_OFFSET = 0x333 };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x344, emulator.pc);
}

// RANDOM: struct { u8, u8 }, // 0xCXNN    TODO
// DRAW: struct { u8, u8, u8 }, // 0xDXYN  TODO

// SKIP_IF_KEY: u8, // 0xEX9E              TODO
// SKIP_IF_NOT_KEY: u8, // 0xEXA1          TODO

test "Execute Set Register From Delay (0xFX07)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.delay = 0x2F;

    const instruction = Instruction{ .SET_REGISTER_FROM_DELAY = 0x03 };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x2F, emulator.registers[3]);
}

test "Execute Delay From Register (0xFX15)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[3] = 0x2F;

    const instruction = Instruction{ .SET_DELAY_FROM_REGISTER = 0x03 };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x2F, emulator.delay);
}

test "Execute Set Sound From Register (0xFX18)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[3] = 0x2F;

    const instruction = Instruction{ .SET_SOUND_FROM_REGISTER = 0x03 };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x2F, emulator.sound);
}

test "Execute Add Index (0xFX1E)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.i = 0x11;
    emulator.registers[1] = 0x11;

    const instruction = Instruction{ .ADD_INDEX = 0x01 };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x22, emulator.i);
}

test "Execute Add Index with overflow (0xFX1E)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.i = 0x0FFF;
    emulator.registers[1] = 0x02;

    const instruction = Instruction{ .ADD_INDEX = 0x01 };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x1001, emulator.i);
    try testing.expectEqual(0x01, emulator.registers[0x0F]);
}

// BLOCK_KEY_PRESS: u8, // 0xFX0A  TODO

test "Execute Font Character (0xFX29)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    const font_addresses: [16]u16 = [_]u16{
        FONT_START + 0 * 5, // 0x0
        FONT_START + 1 * 5, // 0x1
        FONT_START + 2 * 5, // 0x2
        FONT_START + 3 * 5, // 0x3
        FONT_START + 4 * 5, // 0x4
        FONT_START + 5 * 5, // 0x5
        FONT_START + 6 * 5, // 0x6
        FONT_START + 7 * 5, // 0x7
        FONT_START + 8 * 5, // 0x8
        FONT_START + 9 * 5, // 0x9
        FONT_START + 10 * 5, // 0xA
        FONT_START + 11 * 5, // 0xB
        FONT_START + 12 * 5, // 0xC
        FONT_START + 13 * 5, // 0xD
        FONT_START + 14 * 5, // 0xE
        FONT_START + 15 * 5, // 0xF
    };

    for (0..16) |n| {
        const int_n = @as(u8, @intCast(n));
        emulator.registers[1] = int_n;
        const instruction = Instruction{ .FONT_CHARACTER = 0x01 };
        try emulator.executeInstruction(instruction);
        try testing.expectEqual(font_addresses[n], emulator.i);
    }
}

// BINARY_CODED_CONV: u8, // 0xFX33  TODO
test "Execute Binary Coded Conversion (0xFX33)" {
    var emulator = try Chip8.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[1] = 123;
    emulator.i = 0x400;

    const instruction = Instruction{ .BINARY_CODED_CONV = 0x01 };
    try emulator.executeInstruction(instruction);

    for (0..3) |n| {
        try testing.expectEqual(n + 1, emulator.memory[emulator.i + n]);
    }
}

test "Execute Store (0xFX55) [COSMAC-VIP]" {
    var emulator = try Chip8.init(
        testing.allocator,
        EmulatorOptions{ .emulatorType = EmulatorType.COSMAC_VIP },
    );
    defer emulator.deinit();

    emulator.i = 0x0300;
    emulator.registers[0] = 0x10;

    // Test edge case
    const instruction1 = Instruction{ .STORE = 0x00 };
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0x10, emulator.memory[0x300]);
    try testing.expectEqual(0x301, emulator.i);

    emulator.i = 0x0300;
    emulator.registers[1] = 0x11;
    emulator.registers[2] = 0x12;
    emulator.registers[3] = 0x13;
    emulator.registers[4] = 0x14;
    emulator.registers[5] = 0x15;

    const instruction2 = Instruction{ .STORE = 0x05 };
    try emulator.executeInstruction(instruction2);

    for (0..6) |n| {
        try testing.expectEqual(0x10 + n, emulator.memory[0x300 + n]);
    }

    try testing.expectEqual(0x0306, emulator.i);
}

test "Execute Store (0xFX55) [CHIP-48 and SUPER-CHIP]" {
    var emulator = try Chip8.init(
        testing.allocator,
        EmulatorOptions{ .emulatorType = EmulatorType.SUPER_CHIP },
    );
    defer emulator.deinit();

    emulator.i = 0x0300;
    emulator.registers[0] = 0x10;

    // Test edge case
    const instruction1 = Instruction{ .STORE = 0x00 };
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0x10, emulator.memory[0x300]);
    try testing.expectEqual(0x300, emulator.i);

    emulator.i = 0x0300;
    emulator.registers[1] = 0x11;
    emulator.registers[2] = 0x12;
    emulator.registers[3] = 0x13;
    emulator.registers[4] = 0x14;
    emulator.registers[5] = 0x15;

    const instruction2 = Instruction{ .STORE = 0x05 };
    try emulator.executeInstruction(instruction2);

    for (0..6) |n| {
        try testing.expectEqual(0x10 + n, emulator.memory[0x300 + n]);
    }

    try testing.expectEqual(0x0300, emulator.i);
}

test "Execute Load (0xFX65) [COSMAC-VIP]" {
    var emulator = try Chip8.init(
        testing.allocator,
        EmulatorOptions{ .emulatorType = EmulatorType.COSMAC_VIP },
    );
    defer emulator.deinit();

    emulator.i = 0x0300;
    const some_data: [6]u8 = [_]u8{ 0x10, 0x11, 0x12, 0x13, 0x14, 0x15 };
    @memcpy(emulator.memory[0x0300..0x0306], &some_data);

    const instruction2 = Instruction{ .LOAD = 0x05 };
    try emulator.executeInstruction(instruction2);

    for (0..6) |n| {
        try testing.expectEqual(0x10 + n, emulator.registers[n]);
    }
    try testing.expectEqual(0x0306, emulator.i);
}

test "Execute Load (0xFX65) [CHIP-48 and SUPER-CHIP]" {
    var emulator = try Chip8.init(
        testing.allocator,
        EmulatorOptions{ .emulatorType = EmulatorType.SUPER_CHIP },
    );
    defer emulator.deinit();

    emulator.i = 0x0300;
    const some_data: [6]u8 = [_]u8{ 0x10, 0x11, 0x12, 0x13, 0x14, 0x15 };
    @memcpy(emulator.memory[0x0300..0x0306], &some_data);

    const instruction2 = Instruction{ .LOAD = 0x05 };
    try emulator.executeInstruction(instruction2);

    for (0..6) |n| {
        try testing.expectEqual(0x10 + n, emulator.registers[n]);
    }
    try testing.expectEqual(0x0300, emulator.i);
}
