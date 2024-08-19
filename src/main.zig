const std = @import("std");
const raylib = @import("raylib");

pub fn main() !void {
    // raylib.SetConfigFlags(raylib.ConfigFlags{ .FLAG_WINDOW_RESIZABLE = true });
    raylib.initWindow(800, 600, "Hello world");
    defer raylib.closeWindow();
    raylib.setTargetFPS(60);

    while (!raylib.windowShouldClose()) {
        raylib.beginDrawing();
        defer raylib.endDrawing();

        raylib.clearBackground(raylib.Color.white);
        raylib.drawFPS(10, 10);

        raylib.drawText("Hello, world!", 100, 100, 20, raylib.Color.light_gray);
    }
}
