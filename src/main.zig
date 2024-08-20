const std = @import("std");
const raylib = @import("raylib");

const FPS = 60;

// Original Chip8 Screen was 64x32 Pixels.
const SCREEN_WIDTH = 64;
const SCREEN_HEIGHT = 32;

// Obviously we're going to scale 10x;
const SCALAR = 10;

// RAM Memory
var MEM: [4096]u8 = [_]u8{0} ** 4096;

// Program Counter
var PC: u16 = 0x200;

// Stack of 256 bytes (which should be more then enough)
const MAX_STACK = 256;
var SP: u16 = 0;
var STACK: [MAX_STACK]u8 = [_]u8{0} ** MAX_STACK;

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

fn initEmulator() void {
    // Load the font into main memory
    std.mem.copyForwards(u8, MEM[0x050..0x0A0], FONT[0..]);
}

// ======== Stack Functions =======
const StackError = error{OUT_OF_MEMORY};

fn stack_push(value: u8) StackError!void {
    if (SP + 1 > MAX_STACK) return error.OUT_OF_MEMORY;
    STACK[SP] = value;
    SP += 1;
}

fn stack_pop() ?u8 {
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

// Simple Error Screen
fn drawError(err: [*:0]const u8) void {
    raylib.clearBackground(raylib.Color.black);
    raylib.drawText("Error:", 2 * SCALAR, 5 * SCALAR, 20, raylib.Color.red);
    raylib.drawText(err, 2 * SCALAR, 10 * SCALAR, 20, raylib.Color.red);
}

pub fn main() !void {
    raylib.initWindow(SCREEN_WIDTH * SCALAR, SCREEN_HEIGHT * SCALAR, "Chip8");
    defer raylib.closeWindow();
    raylib.setTargetFPS(FPS);

    initEmulator();

    while (!raylib.windowShouldClose()) {
        // Decrement Timers: Not sure of a way to do this
        // independently of execution time.
        if (DELAY > 0) DELAY -= 1;
        if (SOUND > 0) SOUND -= 1;

        raylib.beginDrawing();
        defer raylib.endDrawing();

        raylib.clearBackground(raylib.Color.black);
        try drawPixel(0, 0);
    }
}
