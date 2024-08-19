const std = @import("std");
const raylib = @import("raylib");

const FPS = 60;

// Original Chip8 Screen was 64x32 Pixels.
const SCREEN_WIDTH = 64;
const SCREEN_HEIGHT = 32;

// Obviously we're going to scale 10x;
const SCALAR = 10;

const DrawError = error{OUTOFBOUNDS};

// Draws a pixel on the screen.
// x is 0..SCREEN_WIDTH non-inclusive.
// y is 0..SCREEN_HEIGHT non-inclusive.
// Errors if x or y are out of bounds.
fn drawPixel(x: u16, y: u16) DrawError!void {
    if (x < 0 or x > SCREEN_WIDTH - 1) return error.OUTOFBOUNDS;
    if (y < 0 or y > SCREEN_HEIGHT - 1) return error.OUTOFBOUNDS;

    raylib.drawRectangle(
        x * SCALAR,
        y * SCALAR,
        1 * SCALAR,
        1 * SCALAR,
        raylib.Color.black,
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

    while (!raylib.windowShouldClose()) {
        raylib.beginDrawing();
        defer raylib.endDrawing();

        raylib.clearBackground(raylib.Color.white);
        try drawPixel(0, 0);
    }
}
