const std = @import("std");
const raylib = @import("raylib");

const FPS = 60;
const BACKGROUND_COLOR = raylib.Color.black;

// There are a couple different versions of Chip8.
const EMULARTOR_TYPE = enum { COSMAC_VIP, SUPER_CHIP, CHIP_48 };

// Shift Behaviour
// The shift instructions 0x8XY6 and 0x8XYE have differing behaviour
// based on the emulator type.
// We default to the original. See
// [here](https://tobiasvl.github.io/blog/write-a-chip-8-emulator/#8xy6-and-8xye-shift) for more.
const SHIFT_BEHAVIOUR = EMULARTOR_TYPE.COSMAC_VIP;

// The jump with offset instruction 0xBNNN have differing behaviour based on the emulator type.
// We default to the original. See
// [here](https://tobiasvl.github.io/blog/write-a-chip-8-emulator/#bnnn-jump-with-offset) for more.
const JUMP_WITH_OFFSET_BEVHAVIOUR = EMULARTOR_TYPE.COSMAC_VIP;

// Original Chip8 Screen was 64x32 Pixels.
const SCREEN_WIDTH = 64;
const SCREEN_HEIGHT = 32;
const DIMS = SCREEN_WIDTH * SCREEN_HEIGHT;

// Pixel Buffer
var SCREEN: [DIMS]u8 = [_]u8{0} ** DIMS;

// Obviously we're going to scale 10x;
const SCALAR = 10;

// RAM Memory
var MEM: [4096]u8 = [_]u8{0} ** 4096;

// Program Counter
var PC: u16 = 0x200;

// Stack of 256 bytes (which should be more then enough)
const MAX_STACK = 256;
var SP: u16 = 0;
var STACK: [MAX_STACK]u16 = [_]u16{0} ** MAX_STACK;

// Index Register
var IN: u16 = 0;

// Delay and Sound Timers
var DELAY: u8 = 0;
var SOUND: u8 = 0;

// Variables
var V0: u8 = 0;
var V1: u8 = 0;
var V2: u8 = 0;
var V3: u8 = 0;
var V4: u8 = 0;
var V5: u8 = 0;
var V6: u8 = 0;
var V7: u8 = 0;
var V8: u8 = 0;
var V9: u8 = 0;
var VA: u8 = 0;
var VB: u8 = 0;
var VC: u8 = 0;
var VD: u8 = 0;
var VE: u8 = 0;
var VF: u8 = 0;

var global_err: ?[*:0]u8 = null;

// Font
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
    DRAW: struct { u8, u8, u8 }, // 0xDXYN
};

const EmulatorError = error{
    OUT_OF_MEMORY,
    UNKNOWN_INSTRUCTION,
    UNKNOWN_REGISTER,
};

fn initEmulator(program: []u8) void {
    // Load the font into main memory
    std.mem.copyForwards(u8, MEM[0x050..0x0A0], FONT[0..]);
    // Ensure Program Counter is set to 0x200
    PC = 0x200;
    // Load the program into main memory
    std.mem.copyForwards(u8, MEM[PC .. PC + program.len], program);
}

// Parse the currently pointed at instruction.
// [See here](https://tobiasvl.github.io/blog/write-a-chip-8-emulator/#execute)
// for opcode descriptions.
fn fetchInstruction() EmulatorError!Instruction {
    if (PC + 2 > MEM.len) return EmulatorError.OUT_OF_MEMORY;

    // Get a 2 byte slice of MEM at PC
    const slice = MEM[PC..(PC + 2)];
    PC += 2;

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
                std.debug.print("Unknown instruction '{X}'\n", .{opcode});
                return EmulatorError.UNKNOWN_INSTRUCTION;
            },
        },
        0x1000 => Instruction{ .JUMP = opcode & 0x0FFF },
        0x2000 => Instruction{ .JUMP_SUBROUTINE = opcode & 0x0FFF },
        0x3000 => Instruction{ .EQUAL_TO = .{
            @as(u8, @intCast((opcode & 0x0F00) >> 8)),
            @as(u8, @intCast(opcode & 0x00FF)),
        } },
        0x4000 => Instruction{ .EQUAL_TO = .{
            @as(u8, @intCast((opcode & 0x0F00) >> 8)),
            @as(u8, @intCast(opcode & 0x00FF)),
        } },
        0x5000 => Instruction{ .EQUAL_REGISTERS = .{
            @as(u8, @intCast((opcode & 0x0F00) >> 8)),
            @as(u8, @intCast((opcode & 0x00F0) >> 8)),
        } },
        0x6000 => Instruction{ .SET_REGISTER = .{
            @as(u8, @intCast((opcode & 0x0F00) >> 8)),
            @as(u8, @intCast(opcode & 0x00FF)),
        } },
        0x7000 => Instruction{ .ADD_REGISTER = .{
            @as(u8, @intCast((opcode & 0x0F00) >> 8)),
            @as(u8, @intCast(opcode & 0x00FF)),
        } },
        0x8000 => blk: {
            const subcategory = @as(u8, @intCast(opcode & 0x000F));
            break :blk switch (subcategory) {
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
        },
        0x9000 => Instruction{ .NOT_EQUAL_REGISTERS = .{
            @as(u8, @intCast((opcode & 0x0F00) >> 8)),
            @as(u8, @intCast((opcode & 0x00F0) >> 8)),
        } },
        0xA000 => Instruction{ .SET_INDEX = opcode & 0x0FFF },
        0xB000 => Instruction{ .JUMP_WITH_OFFSET = opcode & 0x0FFF },
        0xD000 => Instruction{ .DRAW = .{
            @as(u8, @intCast((opcode & 0x0F00) >> 8)),
            @as(u8, @intCast((opcode & 0x00F0) >> 4)),
            @as(u8, @intCast(opcode & 0x000F)),
        } },
        else => {
            std.debug.print("Unknown instruction '{X}'\n", .{opcode});
            return EmulatorError.UNKNOWN_INSTRUCTION;
        },
    };

    return instruction;
}

// Manipulate the above variables based on Instruction.
fn executeInstruction(instruction: Instruction) !void {
    switch (instruction) {
        Instruction.CLEAR_SCREEN => |_| raylib.clearBackground(BACKGROUND_COLOR),
        Instruction.RETURN => |_| PC = @as(u16, stack_pop().?),
        Instruction.JUMP => |addr| PC = addr,
        Instruction.JUMP_SUBROUTINE => |addr| {
            try stack_push(PC);
            PC = addr;
        },
        Instruction.EQUAL_TO => |v| {
            const value = try getRegisterValue(v[0]);
            if (value == v[1]) PC += 2;
        },
        Instruction.NOT_EQUAL_TO => |v| {
            const value = try getRegisterValue(v[0]);
            if (value != v[1]) PC += 2;
        },
        Instruction.EQUAL_REGISTERS => |v| {
            const value_x = try getRegisterValue(v[0]);
            const value_y = try getRegisterValue(v[1]);
            if (value_x == value_y) PC += 2;
        },
        Instruction.SET_REGISTER => |v| try setRegisterValue(v[0], v[1]),
        Instruction.ADD_REGISTER => |v| try setRegisterValue(v[0], try getRegisterValue(v[0]) +% v[1]),
        Instruction.NOT_EQUAL_REGISTERS => |v| {
            const value_x = try getRegisterValue(v[0]);
            const value_y = try getRegisterValue(v[1]);
            if (value_x != value_y) PC += 2;
        },
        Instruction.SET_X_Y => |v| try setRegisterValue(v[0], try getRegisterValue(v[1])),
        Instruction.OR_X_Y => |v| try setRegisterValue(
            v[0],
            try getRegisterValue(v[0]) | try getRegisterValue(v[1]),
        ),
        Instruction.AND_X_Y => |v| try setRegisterValue(
            v[0],
            try getRegisterValue(v[0]) & try getRegisterValue(v[1]),
        ),
        Instruction.XOR_X_Y => |v| try setRegisterValue(
            v[0],
            try getRegisterValue(v[0]) ^ try getRegisterValue(v[1]),
        ),
        Instruction.ADD_X_Y => |v| {
            const x = try getRegisterValue(v[0]);
            const y = try getRegisterValue(v[1]);
            const add = x +% y;
            VF = if (add < x) 1 else 0; // if true overflow happened
            try setRegisterValue(v[0], add);
        },
        Instruction.SUB_X_Y => |v| {
            const x = try getRegisterValue(v[0]);
            const y = try getRegisterValue(v[1]);
            VF = if (x > y) 1 else 0;
            const sub = x -% y;
            try setRegisterValue(v[0], sub);
        },
        Instruction.SUB_Y_X => |v| {
            const x = try getRegisterValue(v[0]);
            const y = try getRegisterValue(v[1]);
            VF = if (y > x) 1 else 0;
            const sub = y -% x;
            try setRegisterValue(v[0], sub);
        },
        Instruction.SHIFT_R_X_Y => |v| {
            switch (SHIFT_BEHAVIOUR) {
                // Only the COSMAC_VIP has different behaviour
                EMULARTOR_TYPE.COSMAC_VIP => {
                    const y = try getRegisterValue(v[1]);
                    VF = @as(u8, @intCast(y & 0b00000001)); // This will be the carry bit
                    try setRegisterValue(v[0], (y >> 1));
                },
                else => {
                    const x = try getRegisterValue(v[0]);
                    VF = @as(u8, @intCast(x & 0b00000001)); // This will be the carry bit
                    try setRegisterValue(v[0], (x >> 1));
                },
            }
        },
        Instruction.SHIFT_L_X_Y => |v| {
            switch (SHIFT_BEHAVIOUR) {
                // Only the COSMAC_VIP has different behaviour
                EMULARTOR_TYPE.COSMAC_VIP => {
                    const y = try getRegisterValue(v[1]);
                    VF = @as(u8, @intCast((y & 0b10000000) >> 7)); // This will be the carry bit
                    try setRegisterValue(v[0], (y << 1));
                },
                else => {
                    const x = try getRegisterValue(v[0]);
                    VF = @as(u8, @intCast((x & 0b10000000) >> 7)); // This will be the carry bit
                    try setRegisterValue(v[0], (x << 1));
                },
            }
        },
        Instruction.SET_INDEX => |v| IN = v,
        Instruction.JUMP_WITH_OFFSET => |addr| {
            switch (JUMP_WITH_OFFSET_BEVHAVIOUR) {
                EMULARTOR_TYPE.COSMAC_VIP => {
                    const offset = try getRegisterValue(0);
                    PC = offset + addr;
                },
                else => {
                    const register = @as(u8, @intCast((addr & 0x0F00) >> 8));
                    const offset = try getRegisterValue(register);
                    PC = offset + addr;
                },
            }
        },
        Instruction.DRAW => |v| {
            const x = @as(u16, try getRegisterValue(v[0]) % SCREEN_WIDTH);
            const y = @as(u16, try getRegisterValue(v[1]) % SCREEN_HEIGHT);
            VF = 0x00;

            for (0..v[2]) |n| {
                if ((y + n) > SCREEN_HEIGHT) continue;
                const data = MEM[IN + n];
                for (0..8) |i| {
                    if ((x + i) > SCREEN_WIDTH) continue;
                    const bit_index = @as(u3, @intCast(7 - i));
                    const bit = (data >> bit_index) & 1;
                    if (bit == 1) {
                        if (isPixelOn(@as(u16, @intCast(x + i)), @as(u16, @intCast(y + n)))) {
                            turnPixelOff(@as(u16, @intCast(x + i)), @as(u16, @intCast(y + n)));
                            VF = 1;
                        } else {
                            turnPixelOn(@as(u16, @intCast(x + i)), @as(u16, @intCast(y + n)));
                        }
                    }
                }
            }
        },
    }
}

fn setRegisterValue(register: u8, value: u8) EmulatorError!void {
    return switch (register) {
        0x00 => V0 = value,
        0x01 => V1 = value,
        0x02 => V2 = value,
        0x03 => V3 = value,
        0x04 => V4 = value,
        0x05 => V5 = value,
        0x06 => V6 = value,
        0x07 => V7 = value,
        0x08 => V8 = value,
        0x09 => V9 = value,
        0x0A => VA = value,
        0x0B => VB = value,
        0x0C => VC = value,
        0x0D => VD = value,
        0x0E => VE = value,
        0x0F => VF = value,
        else => return EmulatorError.UNKNOWN_REGISTER,
    };
}

fn getRegisterValue(register: u8) EmulatorError!u8 {
    return switch (register) {
        0x00 => V0,
        0x01 => V1,
        0x02 => V2,
        0x03 => V3,
        0x04 => V4,
        0x05 => V5,
        0x06 => V6,
        0x07 => V7,
        0x08 => V8,
        0x09 => V9,
        0x0A => VA,
        0x0B => VB,
        0x0C => VC,
        0x0D => VD,
        0x0E => VE,
        0x0F => VF,
        else => return EmulatorError.UNKNOWN_REGISTER,
    };
}

// ======== Stack Functions =======
const StackError = error{OUT_OF_MEMORY};

fn stack_push(value: u16) StackError!void {
    if (SP + 1 > MAX_STACK) return error.OUT_OF_MEMORY;
    STACK[SP] = value;
    SP += 1;
}

fn stack_pop() ?u16 {
    if (SP - 1 <= 0) return null;
    const value = STACK[SP];
    SP -= 1;
    return value;
}

// ======== Draw Functions =======
const DrawError = error{OUT_OF_BOUNDS};

// Draws a pixel on the screen.
// x is 0..SCREEN_WIDTH non-inclusive.
// y is 0..SCREEN_HEIGHT non-inclusive.
// Errors if x or y are out of bounds.
fn drawPixel(x: u16, y: u16) DrawError!void {
    if (x < 0 or x > SCREEN_WIDTH - 1) return error.OUT_OF_BOUNDS;
    if (y < 0 or y > SCREEN_HEIGHT - 1) return error.OUT_OF_BOUNDS;

    raylib.drawRectangle(
        x * SCALAR,
        y * SCALAR,
        1 * SCALAR,
        1 * SCALAR,
        raylib.Color.white,
    );
}

fn drawScreen() DrawError!void {
    for (0.., SCREEN) |i, pixel| {
        const x = @as(i32, @intCast(i % SCREEN_WIDTH));
        const y = @as(i32, @intCast(i / SCREEN_WIDTH));

        raylib.drawFPS(20, 20);
        raylib.drawRectangle(
            x * SCALAR,
            y * SCALAR,
            1 * SCALAR,
            1 * SCALAR,
            if (pixel == 1) raylib.Color.white else raylib.Color.black,
        );
    }
}

fn turnPixelOn(x: u16, y: u16) void {
    const i = y * SCREEN_WIDTH + x;
    SCREEN[i] = 1;
}

fn turnPixelOff(x: u16, y: u16) void {
    const i = y * SCREEN_WIDTH + x;
    SCREEN[i] = 0;
}

fn isPixelOn(x: u16, y: u16) bool {
    const i = y * SCREEN_WIDTH + x;
    return SCREEN[i] == 1;
}

// Simple Error Screen
fn drawError(err: [*:0]const u8) void {
    raylib.clearBackground(BACKGROUND_COLOR);
    raylib.drawText("Error:", 2 * SCALAR, 5 * SCALAR, 20, raylib.Color.red);
    raylib.drawText(err, 2 * SCALAR, 10 * SCALAR, 20, raylib.Color.red);
}

pub fn main() !void {
    raylib.initWindow(SCREEN_WIDTH * SCALAR, SCREEN_HEIGHT * SCALAR, "Chip8");
    defer raylib.closeWindow();
    raylib.setTargetFPS(FPS);

    var examples = try std.fs.cwd().openDir("examples", .{});
    defer examples.close();
    const ibmrom = try examples.openFile("IBM_Logo.ch8", .{});
    defer ibmrom.close();
    var buf: [256]u8 = undefined;
    const read = try ibmrom.readAll(&buf);

    initEmulator(buf[0..read]);

    // var err: ?[*:0]const u8 = null;

    while (!raylib.windowShouldClose()) {
        // Decrement Timers: Not sure of a way to do this
        // independently of execution time.
        if (DELAY > 0) DELAY -= 1;
        if (SOUND > 0) SOUND -= 1;

        raylib.beginDrawing();
        defer raylib.endDrawing();

        // Render Error
        if (global_err) |msg| {
            drawError(msg);
            continue;
        }

        // Fetch Instruction
        const instruction = try fetchInstruction();

        // Execute Instruction
        try executeInstruction(instruction);

        // Draw the Screen
        try drawScreen();
    }
}
