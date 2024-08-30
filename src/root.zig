const std = @import("std");
const testing = std.testing;

// Original Chip8 Screen was 64x32 Pixels. TODO: Make this configurable
pub const SCREEN_WIDTH = 64;
pub const SCREEN_HEIGHT = 32;
pub const DIMS = SCREEN_WIDTH * SCREEN_HEIGHT;

const FRAMES_PER_SECOND: u64 = 60;
const FRAME_DURATION_NANO: u64 = 1_000_000_000 / FRAMES_PER_SECOND;
const INSTRUCTIONS_PER_SECOND: u64 = 700;
const INSTRUCTION_DURATION_NANO: u64 = 1_000_000_000 / INSTRUCTIONS_PER_SECOND;

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
pub const EmulatorType = enum { COSMAC_VIP, SUPER_CHIP, CHIP_48 };

// Errors
pub const EmulatorError = error{
    OUT_OF_MEMORY,
    UNKNOWN_INSTRUCTION,
    UNKNOWN_REGISTER,
    NO_KEY_PRESSED,
    UNKNOWN_FONT_CHARACTER,
    STACK_ERROR,
    GET_RANDOM_ERROR,
};

// Options
pub const EmulatorOptions = struct {
    emulatorType: ?EmulatorType = null,
    deviceCtx: ?DeviceCtx = null,
};

// The main emulator structure.
pub const Emulator = struct {
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

    ctx: DeviceCtx,

    const This = @This();

    pub fn init(allocator: std.mem.Allocator, options: EmulatorOptions) !This {
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

        const ctx = if (options.deviceCtx) |deviceCtx| deviceCtx else blk: {
            var mock = MockDeviceCtx{};
            const ctx = mock.ctx();
            break :blk ctx;
        };

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

            .ctx = ctx,
        };
    }

    pub fn run(self: *This, program: []u8) !void {
        self.load(program);

        var last_frame = try std.time.Instant.now();
        var instr_acc: u64 = 0;
        var frame_acc: u64 = 0;

        var instructionCount: u64 = 0;
        var per_second: u64 = 0;

        while (!self.ctx.exit()) {
            const start = try std.time.Instant.now();
            const since_frame = start.since(last_frame);
            last_frame = start;

            instr_acc += since_frame;
            frame_acc += since_frame;
            per_second += since_frame;

            var draws = false;
            while (instr_acc >= INSTRUCTION_DURATION_NANO) {
                const instruction = try self.fetchInstruction();

                // try printInstruction(instruction);
                try self.executeInstruction(instruction);
                if (instruction.draws()) draws = true;
                instr_acc -= INSTRUCTION_DURATION_NANO;
                instructionCount += 1;

                // Detect Infinite Loop
                switch (instruction) {
                    Instruction.JUMP => |addr| if (addr == self.pc - 2) break,
                    else => {},
                }
            }

            if (frame_acc >= FRAME_DURATION_NANO) {
                if (self.delay > 0) self.delay -= 1;
                if (self.sound > 0) self.sound -= 1;
                if (draws) try self.ctx.draw(self.screen);
                frame_acc -= FRAME_DURATION_NANO;
            }

            if (per_second >= std.time.ns_per_s) {
                std.debug.print("{d} Instructions per Second\n", .{instructionCount});
                instructionCount = 0;
                per_second = 0;
            }

            const end = try std.time.Instant.now();
            const elapsed = end.since(start);
            const delta: i64 = @as(i64, @intCast(FRAME_DURATION_NANO)) - @as(i64, @intCast(elapsed));
            if (delta > 0) {
                const wait = @as(u64, @intCast(delta));
                std.time.sleep(wait);
            }
        }
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
                    EmulatorType.COSMAC_VIP => {
                        const y = try self.getRegisterValue(v[1]);
                        self.registers[0x0F] = @as(u8, @intCast(y & 0b00000001)); // carry bit
                        try self.setRegisterValue(v[0], (y >> 1));
                    },
                    else => {
                        const x = try self.getRegisterValue(v[0]);
                        self.registers[0x0F] = @as(u8, @intCast(x & 0b00000001)); // carry bit
                        try self.setRegisterValue(v[0], (x >> 1));
                    },
                }
            },
            Instruction.SHIFT_L_X_Y => |v| {
                switch (self.emulatorType) {
                    EmulatorType.COSMAC_VIP => {
                        const y = try self.getRegisterValue(v[1]);
                        self.registers[0x0F] = @as(u8, @intCast((y & 0b10000000) >> 7)); // carry bit
                        try self.setRegisterValue(v[0], (y << 1));
                    },
                    else => {
                        const x = try self.getRegisterValue(v[0]);
                        self.registers[0x0F] = @as(u8, @intCast((x & 0b10000000) >> 7)); // carry bit
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
            Instruction.RANDOM => |v| {
                var prng = std.Random.DefaultPrng.init(blk: {
                    var seed: u64 = undefined;
                    std.posix.getrandom(std.mem.asBytes(&seed)) catch return EmulatorError.GET_RANDOM_ERROR;
                    break :blk seed;
                });
                const rand = prng.random();
                const value = rand.int(u8);
                try self.setRegisterValue(v[0], value & v[1]);
            },
            Instruction.DRAW => |v| {
                const x = @as(u16, try self.getRegisterValue(v[0]) % SCREEN_WIDTH);
                const y = @as(u16, try self.getRegisterValue(v[1]) % SCREEN_HEIGHT);
                self.registers[0x0F] = 0x00;

                for (0..v[2]) |n| {
                    if ((y + n) >= SCREEN_HEIGHT) continue;
                    const data = self.memory[self.i + n];
                    for (0..8) |i| {
                        if ((x + i) >= SCREEN_WIDTH) continue;
                        const bit_index = @as(u3, @intCast(7 - i));
                        const bit = (data >> bit_index) & 1;
                        if (bit == 1) {
                            if (self.isPixelOn(@as(u16, @intCast(x + i)), @as(u16, @intCast(y + n)))) {
                                self.turnPixelOff(@as(u16, @intCast(x + i)), @as(u16, @intCast(y + n)));
                                self.registers[0x0F] = 1;
                            } else {
                                self.turnPixelOn(@as(u16, @intCast(x + i)), @as(u16, @intCast(y + n)));
                            }
                        }
                    }
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
            Instruction.SKIP_IF_KEY => |reg| {
                const key = try self.getRegisterValue(reg);
                if (self.ctx.isPressed(key)) self.pc += 2;
            },
            Instruction.SKIP_IF_NOT_KEY => |reg| {
                const key = try self.getRegisterValue(reg);
                if (!(self.ctx.isPressed(key))) self.pc += 2;
            },
            Instruction.BLOCK_KEY_PRESS => |reg| {
                const key = self.ctx.wasPressed() catch {
                    self.pc -= 2; // Loop this instruction until key is pressed
                    // std.debug.print("No Key Pressed\n", .{});
                    return;
                };
                // std.debug.print("Key Pressed: {X:1}\n", .{key});
                try self.setRegisterValue(reg, key);
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

    fn turnPixelOn(self: *This, x: u16, y: u16) void {
        const i = y * SCREEN_WIDTH + x;
        self.screen[i] = 1;
    }

    fn turnPixelOff(self: *This, x: u16, y: u16) void {
        const i = y * SCREEN_WIDTH + x;
        self.screen[i] = 0;
    }

    fn isPixelOn(self: *This, x: u16, y: u16) bool {
        const i = y * SCREEN_WIDTH + x;
        return self.screen[i] == 1;
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

    fn draws(self: Instruction) bool {
        return switch (self) {
            Instruction.DRAW => |_| true,
            Instruction.CLEAR_SCREEN => |_| true,
            else => false,
        };
    }
};

fn printInstruction(instruction: Instruction) !void {
    switch (instruction) {
        Instruction.CLEAR_SCREEN => |_| std.debug.print("0x00E0 (Clear Screen)\n", .{}),
        Instruction.RETURN => |_| std.debug.print("0x00EE (Return)\n", .{}),
        Instruction.JUMP => |addr| std.debug.print("0x1{X:0>3} (Jump to address {X:0>3})\n", .{ addr, addr }),
        Instruction.JUMP_SUBROUTINE => |addr| std.debug.print("0x2{X:0>3} (Jump to subroutine {X:0>3})\n", .{ addr, addr }),
        Instruction.EQUAL_TO => |v| std.debug.print("0x3{X:1}{X:0>2} (Equal to Condition: {X})\n", .{ v[0], v[1], v[1] }),
        Instruction.NOT_EQUAL_TO => |v| std.debug.print("0x3{X:1}{X:0>2} (Not Equal to Condition: {X})\n", .{ v[0], v[1], v[1] }),
        Instruction.EQUAL_REGISTERS => |v| std.debug.print("0x5{X:1}{X:1}0 (Registers Equal Condition)\n", .{ v[0], v[1] }),
        Instruction.NOT_EQUAL_REGISTERS => |v| std.debug.print("0x9{X:1}{X:1}0 (Registers Not Equal Condition)\n", .{ v[0], v[1] }),
        Instruction.SET_REGISTER => |v| std.debug.print("0x6{X:1}{X:0>2} (Set Register V{X:1} to {X:0>2})\n", .{ v[0], v[1], v[0], v[1] }),
        Instruction.ADD_REGISTER => |v| std.debug.print("0x7{X:1}{X:0>2} (Add {X:0>2} to Register V{X:1})\n", .{ v[0], v[1], v[1], v[0] }),
        Instruction.SET_X_Y => |v| std.debug.print("0x8{X:1}{X:1}0 (V{X:1} is set to V{X:1})\n", .{ v[0], v[1], v[0], v[1] }),
        Instruction.OR_X_Y => |v| std.debug.print("0x8{X:1}{X:1}1 (V{X:1} is set to V{X:1} | V{X:1})\n", .{ v[0], v[1], v[0], v[0], v[1] }),
        Instruction.AND_X_Y => |v| std.debug.print("0x8{X:1}{X:1}2 (V{X:1} is set to V{X:1} & V{X:1})\n", .{ v[0], v[1], v[0], v[0], v[1] }),
        Instruction.XOR_X_Y => |v| std.debug.print("0x8{X:1}{X:1}3 (V{X:1} is set to V{X:1} ^ V{X:1})\n", .{ v[0], v[1], v[0], v[0], v[1] }),
        Instruction.ADD_X_Y => |v| std.debug.print("0x8{X:1}{X:1}4 (V{X:1} is set to V{X:1} + V{X:1})\n", .{ v[0], v[1], v[0], v[0], v[1] }),
        Instruction.SUB_X_Y => |v| std.debug.print("0x8{X:1}{X:1}5 (V{X:1} is set to V{X:1} - V{X:1})\n", .{ v[0], v[1], v[0], v[0], v[1] }),
        Instruction.SHIFT_R_X_Y => |v| std.debug.print("0x8{X:1}{X:1}6 (V{X:1} is set to V{X:1} >> 1)\n", .{ v[0], v[1], v[0], v[1] }),
        Instruction.SUB_Y_X => |v| std.debug.print("0x8{X:1}{X:1}7 (V{X:1} is set to V{X:1} - V{X:1})\n", .{ v[0], v[1], v[0], v[1], v[0] }),
        Instruction.SHIFT_L_X_Y => |v| std.debug.print("0x8{X:1}{X:1}E (V{X:1} is set to V{X:1} << 1)\n", .{ v[0], v[1], v[0], v[1] }),
        Instruction.SET_INDEX => |v| std.debug.print("0xA{X:0>3} (Set I to {X:0>3})\n", .{ v, v }),
        // .. //
        Instruction.DRAW => |v| std.debug.print("0xD{X:1}{X:1}{X:1} (Draw Sprite at {X:0>4} to with size)\n", .{ v[0], v[1], v[2], v[2] }),
        // .. //
        Instruction.BLOCK_KEY_PRESS => |reg| std.debug.print("0x2{X:1}0A (Block Until a key is press) -- ", .{reg}),
        else => std.debug.print("Unimplented print!\n", .{}),
    }
}

// ================== Device Context Interface ===================
// Bridges the emulator and platform specific (or cross-platform) handling
// of system peripherals.
pub const DeviceCtx = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    pub const VTable = struct {
        draw: *const fn (ctx: *anyopaque, screen_buffer: []u8) anyerror!void,
        exit: *const fn (ctx: *anyopaque) bool,
        isPressed: *const fn (ctx: *anyopaque, key: u8) bool,
        wasPressed: *const fn (ctx: *anyopaque) anyerror!u8,
    };

    pub inline fn draw(self: DeviceCtx, screen_buffer: []u8) !void {
        return self.vtable.draw(self.ptr, screen_buffer);
    }

    pub inline fn exit(self: DeviceCtx) bool {
        return self.vtable.exit(self.ptr);
    }

    pub inline fn isPressed(self: DeviceCtx, key: u8) bool {
        return self.vtable.isPressed(self.ptr, key);
    }

    pub inline fn wasPressed(self: DeviceCtx) !u8 {
        return self.vtable.wasPressed(self.ptr);
    }
};

const MockDeviceCtx = struct {
    keyPressed: ?u8 = null,

    fn draw(_: *anyopaque, screen_buffer: []u8) !void {
        std.debug.print("Drawing: {s}", .{screen_buffer});
    }

    fn exit(_: *anyopaque) bool {
        return false;
    }

    fn isPressed(_: *anyopaque, key: u8) bool {
        return key == 0x01;
    }

    fn wasPressed(ptr: *anyopaque) !u8 {
        const self: *MockDeviceCtx = @ptrCast(@alignCast(ptr));
        return self.keyPressed orelse EmulatorError.NO_KEY_PRESSED;
    }

    fn ctx(self: *MockDeviceCtx) DeviceCtx {
        return .{
            .ptr = self,
            .vtable = &.{
                .draw = draw,
                .exit = exit,
                .isPressed = isPressed,
                .wasPressed = wasPressed,
            },
        };
    }
};

// ===================================================================
// ============================ TESTING ==============================
// ===================================================================

test "Chip 8 Init" {
    const chip8 = try Emulator.init(testing.allocator, .{});
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

    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.load(&opcodes);

    for (instructions) |expected| {
        // std.debug.print("{}\n", .{expected});
        const actual = try emulator.fetchInstruction();
        try testing.expectEqualDeep(expected, actual);
    }
}

test "Execute Clear Screen (0x00E0)" {
    var emulator = try Emulator.init(testing.allocator, .{});
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
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    // Push a value onto the stack
    try emulator.stack.append(0x025);

    try emulator.executeInstruction(Instruction{ .RETURN = {} });

    try testing.expectEqual(0x025, emulator.pc);
}

test "Execute Jump (0x1NNN)" {
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    var opcode: [2]u8 = .{ 0x11, 0x23 };
    emulator.load(&opcode);
    const instruction = try emulator.fetchInstruction();
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x123, emulator.pc);
}

test "Execute Jump With Subroutine (0x2NNN)" {
    var emulator = try Emulator.init(testing.allocator, .{});
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
    var emulator = try Emulator.init(testing.allocator, .{});
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
    var emulator = try Emulator.init(testing.allocator, .{});
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
    var emulator = try Emulator.init(testing.allocator, .{});
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
    var emulator = try Emulator.init(testing.allocator, .{});
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
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    const instruction = Instruction{ .SET_REGISTER = .{ 0x01, 0xFF } };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0xFF, emulator.registers[1]);
}

test "Execute Add Register (0x7XNN)" {
    var emulator = try Emulator.init(testing.allocator, .{});
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
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[1] = 0x02;
    emulator.registers[0] = 0xFF;

    const instruction1 = Instruction{ .SET_X_Y = .{ 0x01, 0x00 } };
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0xFF, emulator.registers[1]);
}

test "Execute Or X and Y (0x8XY1)" {
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[1] = 0x02;
    emulator.registers[0] = 0x05;

    const instruction1 = Instruction{ .OR_X_Y = .{ 0x01, 0x00 } };
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0x07, emulator.registers[1]);
}

test "Execute AND X and Y (0x8XY2)" {
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[1] = 0x03;
    emulator.registers[0] = 0x05;

    const instruction1 = Instruction{ .AND_X_Y = .{ 0x01, 0x00 } };
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0x01, emulator.registers[1]);
}

test "Execute XOR X and Y (0x8XY3)" {
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[1] = 0x03;
    emulator.registers[0] = 0x05;

    const instruction1 = Instruction{ .XOR_X_Y = .{ 0x01, 0x00 } };
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0x06, emulator.registers[1]);
}

test "Execute ADD X and Y with Overflow (0x8XY4)" {
    var emulator = try Emulator.init(testing.allocator, .{});
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

test "Execute SUB X and Y with Overflow (0x8XY5)" {
    var emulator = try Emulator.init(testing.allocator, .{});
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

test "Execute SUB Y and X with Overflow (0x8XY7)" {
    var emulator = try Emulator.init(testing.allocator, .{});
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

test "Execute Shift Right X and Y [COSMAC-VIP] (0x8XY6)" {
    var emulator = try Emulator.init(
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
    var emulator = try Emulator.init(
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

test "Execute Shift Left X and Y [COSMAC-VIP] (0x8XYE)" {
    var emulator = try Emulator.init(
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
    var emulator = try Emulator.init(
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

test "Execute Set Index (0xANNN)" {
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    const instruction = Instruction{ .SET_INDEX = 0x333 };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x333, emulator.i);
}

test "Execute Jump With Offset (0xBNNN) [COSMAC-VIP]" {
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[0] = 0x11;

    const instruction = Instruction{ .JUMP_WITH_OFFSET = 0x333 };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x344, emulator.pc);
}

test "Execute Jump With Offset (0xBXNN) [CHIP-48 and SUPER-CHIP]" {
    var emulator = try Emulator.init(
        testing.allocator,

        EmulatorOptions{
            .emulatorType = EmulatorType.CHIP_48,
        },
    );
    defer emulator.deinit();

    emulator.registers[3] = 0x11;

    const instruction = Instruction{ .JUMP_WITH_OFFSET = 0x333 };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x344, emulator.pc);
}

// Not the best test but it'll do
test "Execute Random (0xCXNN)" {
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[1] = 0x00;

    const instruction1 = Instruction{ .RANDOM = .{ 0x1, 0xFF } };
    try emulator.executeInstruction(instruction1);

    const previous = emulator.registers[1];

    const instruction2 = Instruction{ .RANDOM = .{ 0x1, 0xFF } };
    try emulator.executeInstruction(instruction2);

    try testing.expect(previous != emulator.registers[1]);
}

test "Execute Draw with Font Character (0xDXYN)" {
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    // Clear Screen
    const instruction0 = Instruction{ .CLEAR_SCREEN = {} };
    try emulator.executeInstruction(instruction0);

    // Load in the font character 0 to the index register
    emulator.registers[1] = 0x00;
    const instruction1 = Instruction{ .FONT_CHARACTER = 0x01 };
    try emulator.executeInstruction(instruction1);

    // X and Y will be 10 and 10 respectively
    emulator.registers[2] = 10;
    emulator.registers[3] = 10;

    // Draw the font character at 5 rows (i.e. the length of the characters)
    const instruction2 = Instruction{ .DRAW = .{ 0x02, 0x03, 0x05 } };
    try emulator.executeInstruction(instruction2);

    // Test Screen pixels for 0xF0, 0x90, 0x90, 0x90, 0xF0
    const zero = [5][8]u8{
        [8]u8{ 1, 1, 1, 1, 0, 0, 0, 0 },
        [8]u8{ 1, 0, 0, 1, 0, 0, 0, 0 },
        [8]u8{ 1, 0, 0, 1, 0, 0, 0, 0 },
        [8]u8{ 1, 0, 0, 1, 0, 0, 0, 0 },
        [8]u8{ 1, 1, 1, 1, 0, 0, 0, 0 },
    };

    for (0.., zero) |i, expected_row| {
        const start_index = (10 + i) * SCREEN_WIDTH + 10;
        const actual_row = emulator.screen[start_index .. start_index + 8];
        try testing.expectEqualSlices(u8, &expected_row, actual_row);
    }
}

test "Execute Draw with non-font sprite (0xDXYN)" {
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    // Clear Screen
    const instruction0 = Instruction{ .CLEAR_SCREEN = {} };
    try emulator.executeInstruction(instruction0);

    // Happy Face
    const happy = [6]u8{ 0x00, 0x44, 0x10, 0x82, 0x7C, 0x00 };
    const happy_bit = [6][8]u8{
        [8]u8{ 0, 0, 0, 0, 0, 0, 0, 0 },
        [8]u8{ 0, 1, 0, 0, 0, 1, 0, 0 },
        [8]u8{ 0, 0, 0, 1, 0, 0, 0, 0 },
        [8]u8{ 1, 0, 0, 0, 0, 0, 1, 0 },
        [8]u8{ 0, 1, 1, 1, 1, 1, 0, 0 },
        [8]u8{ 0, 0, 0, 0, 0, 0, 0, 0 },
    };

    // Copy to emulator memory
    emulator.i = 0x400;
    @memcpy(emulator.memory[emulator.i .. emulator.i + 6], &happy);

    // 20, 20
    emulator.registers[2] = 20;
    emulator.registers[3] = 20;

    // Draw the font character at 5 rows (i.e. the length of the characters)
    const instruction2 = Instruction{ .DRAW = .{ 0x02, 0x03, 0x06 } };
    try emulator.executeInstruction(instruction2);

    // test
    for (0.., happy_bit) |i, expected_row| {
        const start_index = (20 + i) * SCREEN_WIDTH + 20;
        const actual_row = emulator.screen[start_index .. start_index + 8];
        try testing.expectEqualSlices(u8, &expected_row, actual_row);
    }
}

test "Execute Set Register From Delay (0xFX07)" {
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.delay = 0x2F;

    const instruction = Instruction{ .SET_REGISTER_FROM_DELAY = 0x03 };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x2F, emulator.registers[3]);
}

test "Execute Delay From Register (0xFX15)" {
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[3] = 0x2F;

    const instruction = Instruction{ .SET_DELAY_FROM_REGISTER = 0x03 };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x2F, emulator.delay);
}

test "Execute Set Sound From Register (0xFX18)" {
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[3] = 0x2F;

    const instruction = Instruction{ .SET_SOUND_FROM_REGISTER = 0x03 };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x2F, emulator.sound);
}

test "Execute Add Index (0xFX1E)" {
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.i = 0x11;
    emulator.registers[1] = 0x11;

    const instruction = Instruction{ .ADD_INDEX = 0x01 };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x22, emulator.i);
}

test "Execute Add Index with overflow (0xFX1E)" {
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.i = 0x0FFF;
    emulator.registers[1] = 0x02;

    const instruction = Instruction{ .ADD_INDEX = 0x01 };
    try emulator.executeInstruction(instruction);

    try testing.expectEqual(0x1001, emulator.i);
    try testing.expectEqual(0x01, emulator.registers[0x0F]);
}

test "Execute Font Character (0xFX29)" {
    var emulator = try Emulator.init(testing.allocator, .{});
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

test "Execute Binary Coded Conversion (0xFX33)" {
    var emulator = try Emulator.init(testing.allocator, .{});
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
    var emulator = try Emulator.init(
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
    var emulator = try Emulator.init(
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
    var emulator = try Emulator.init(
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
    var emulator = try Emulator.init(
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

// BLOCK_KEY_PRESS: u8, // 0xFX0A  TODO
test "Execute Block Key Press without Press (0xFX0A)" {
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    emulator.registers[5] = 0xAA;

    var instructions: [2]u8 = [_]u8{ 0xF5, 0x0A };
    emulator.load(&instructions);
    const instruction1 = try emulator.fetchInstruction();
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0x200, emulator.pc);
    try testing.expectEqual(0xAA, emulator.registers[5]);
}

test "Execute Block Key Press with Press (0xFX0A)" {
    var mockDeviceCtx = MockDeviceCtx{ .keyPressed = 0x0F };
    const ctx = mockDeviceCtx.ctx();

    var emulator = try Emulator.init(testing.allocator, .{ .deviceCtx = ctx });
    defer emulator.deinit();

    emulator.registers[5] = 0xAA;

    var instructions: [2]u8 = [_]u8{ 0xF5, 0x0A };
    emulator.load(&instructions);
    const instruction1 = try emulator.fetchInstruction();
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(0x202, emulator.pc);
    try testing.expectEqual(0x0F, emulator.registers[5]);
}

test "Execute Skip If Key (0xEX9E)" {
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    var pc = emulator.pc;
    emulator.registers[5] = 0x01;

    const instruction1 = Instruction{ .SKIP_IF_KEY = 0x05 };
    try emulator.executeInstruction(instruction1);

    // Normally because we fetch the instruction we would auto-increment the pc.
    // Here we skip that so it's only pc + 2, not pc + 4.
    try testing.expectEqual(pc + 2, emulator.pc);

    pc = emulator.pc;
    emulator.registers[5] = 0x02;

    const instruction2 = Instruction{ .SKIP_IF_KEY = 0x05 };
    try emulator.executeInstruction(instruction2);

    try testing.expectEqual(pc, emulator.pc);
}

test "Execute Skip If Not Key (0xEX9E)" {
    var emulator = try Emulator.init(testing.allocator, .{});
    defer emulator.deinit();

    var pc = emulator.pc;
    emulator.registers[5] = 0x01;

    const instruction1 = Instruction{ .SKIP_IF_NOT_KEY = 0x05 };
    try emulator.executeInstruction(instruction1);

    try testing.expectEqual(pc, emulator.pc);

    pc = emulator.pc;
    emulator.registers[5] = 0x02;

    const instruction2 = Instruction{ .SKIP_IF_NOT_KEY = 0x05 };
    try emulator.executeInstruction(instruction2);

    // Normally because we fetch the instruction we would auto-increment the pc.
    // Here we skip that so it's only pc + 2, not pc + 4.
    try testing.expectEqual(pc + 2, emulator.pc);
}
