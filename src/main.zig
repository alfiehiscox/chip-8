const std = @import("std");
const raylib = @import("raylib");

const FPS = 60;
const BACKGROUND_COLOR = raylib.Color.black;

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
var DELAY: u8 = std.math.maxInt(u8);
var SOUND: u8 = std.math.maxInt(u8);

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
    CLEAR_SCREEN: void,
    RETURN: void,
    JUMP: u16,
    JUMP_SUBROUTINE: u16,
    EQUAL_TO: struct {
        register: u8,
        value: u8,
    },
    NOT_EQUAL_TO: struct {
        register: u8,
        value: u8,
    },
    EQUAL_REGISTERS: struct {
        register_x: u8,
        register_y: u8,
    },
    NOT_EQUAL_REGISTERS: struct {
        register_x: u8,
        register_y: u8,
    },
    SET_REGISTER: struct {
        register: u8,
        value: u8,
    },
    ADD_REGISTER: struct {
        register: u8,
        value: u8,
    },
    SET_INDEX: u16,
    DRAW: struct {
        register_x: u8,
        register_y: u8,
        size: u8,
    },
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
            else => return EmulatorError.UNKNOWN_INSTRUCTION,
        },
        0x1000 => Instruction{ .JUMP = opcode & 0x0FFF },
        0x2000 => Instruction{ .JUMP_SUBROUTINE = opcode & 0x0FFF },
        0x3000 => Instruction{ .EQUAL_TO = .{
            .register = @as(u8, @intCast((opcode & 0x0F00) >> 8)),
            .value = @as(u8, @intCast(opcode & 0x00FF)),
        } },
        0x4000 => Instruction{ .EQUAL_TO = .{
            .register = @as(u8, @intCast((opcode & 0x0F00) >> 8)),
            .value = @as(u8, @intCast(opcode & 0x00FF)),
        } },
        0x5000 => Instruction{ .EQUAL_REGISTERS = .{
            .register_x = @as(u8, @intCast((opcode & 0x0F00) >> 8)),
            .register_y = @as(u8, @intCast((opcode & 0x00F0) >> 8)),
        } },
        0x6000 => Instruction{ .SET_REGISTER = .{
            .register = @as(u8, @intCast((opcode & 0x0F00) >> 8)),
            .value = @as(u8, @intCast(opcode & 0x00FF)),
        } },
        0x7000 => Instruction{ .ADD_REGISTER = .{
            .register = @as(u8, @intCast((opcode & 0x0F00) >> 8)),
            .value = @as(u8, @intCast(opcode & 0x00FF)),
        } },
        0x9000 => Instruction{ .NOT_EQUAL_REGISTERS = .{
            .register_x = @as(u8, @intCast((opcode & 0x0F00) >> 8)),
            .register_y = @as(u8, @intCast((opcode & 0x00F0) >> 8)),
        } },
        0xA000 => Instruction{ .SET_INDEX = opcode & 0x0FFF },
        0xD000 => Instruction{ .DRAW = .{
            .register_x = @as(u8, @intCast((opcode & 0x0F00) >> 8)),
            .register_y = @as(u8, @intCast((opcode & 0x00F0) >> 4)),
            .size = @as(u8, @intCast(opcode & 0x000F)),
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
            const value = try getRegisterValue(v.register);
            if (value == v.value) PC += 2;
        },
        Instruction.NOT_EQUAL_TO => |v| {
            const value = try getRegisterValue(v.register);
            if (value != v.value) PC += 2;
        },
        Instruction.EQUAL_REGISTERS => |v| {
            const value_x = try getRegisterValue(v.register_x);
            const value_y = try getRegisterValue(v.register_y);
            if (value_x == value_y) PC += 2;
        },
        Instruction.SET_REGISTER => |v| {
            _ = switch (v.register) {
                0x00 => V0 = v.value,
                0x01 => V1 = v.value,
                0x02 => V2 = v.value,
                0x03 => V3 = v.value,
                0x04 => V4 = v.value,
                0x05 => V5 = v.value,
                0x06 => V6 = v.value,
                0x07 => V7 = v.value,
                0x08 => V8 = v.value,
                0x09 => V9 = v.value,
                0x0A => VA = v.value,
                0x0B => VB = v.value,
                0x0C => VC = v.value,
                0x0D => VD = v.value,
                0x0E => VE = v.value,
                0x0F => VF = v.value,
                else => return EmulatorError.UNKNOWN_REGISTER,
            };
        },
        Instruction.ADD_REGISTER => |v| {
            // We allow for wrapping back around to 0 from 255.
            _ = switch (v.register) {
                0x00 => V0 = V0 +% v.value,
                0x01 => V1 = V1 +% v.value,
                0x02 => V2 = V2 +% v.value,
                0x03 => V3 = V3 +% v.value,
                0x04 => V4 = V4 +% v.value,
                0x05 => V5 = V5 +% v.value,
                0x06 => V6 = V6 +% v.value,
                0x07 => V7 = V7 +% v.value,
                0x08 => V8 = V8 +% v.value,
                0x09 => V9 = V9 +% v.value,
                0x0A => VA = VA +% v.value,
                0x0B => VB = VB +% v.value,
                0x0C => VC = VC +% v.value,
                0x0D => VD = VD +% v.value,
                0x0E => VE = VE +% v.value,
                0x0F => VF = VF +% v.value,
                else => return EmulatorError.UNKNOWN_REGISTER,
            };
        },
        Instruction.NOT_EQUAL_REGISTERS => |v| {
            const value_x = try getRegisterValue(v.register_x);
            const value_y = try getRegisterValue(v.register_y);
            if (value_x != value_y) PC += 2;
        },
        Instruction.SET_INDEX => |v| IN = v,
        Instruction.DRAW => |v| {
            const x = @as(u16, try getRegisterValue(v.register_x) % SCREEN_WIDTH);
            const y = @as(u16, try getRegisterValue(v.register_y) % SCREEN_HEIGHT);
            VF = 0x00;

            for (0..v.size) |n| {
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
