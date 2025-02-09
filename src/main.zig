const std = @import("std");
const raylib = @import("raylib");
const Chip8 = @import("root.zig");

//// Original Chip8 Screen was 64x32 Pixels.
const SCREEN_WIDTH = 64;
const SCREEN_HEIGHT = 32;

//// Obviously we're going to scale 10x;
const SCALAR = 10;

const Emulator = Chip8.Emulator;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var gpa = std.heap.ThreadSafeAllocator{ .child_allocator = arena.allocator() };
    const allocator = gpa.allocator();

    // ==== TERMINAL BASED CTX ====
    //var key_queue = FixedKeyQueue.init();
    //defer key_queue.deinit();

    //const input_thread = try std.Thread.spawn(.{ .allocator = allocator }, poll_input, .{&key_queue});
    //defer input_thread.detach();

    //var terminalDeviceCtx = try TerminalDeviceCtx.init(
    //    allocator,
    //    std.io.getStdOut(),
    //    key_queue,
    //);
    // ===========================

    var args = std.process.args();
    _ = args.skip();

    const rom_file = args.next().?;

    var rom = try std.fs.cwd().openFile(rom_file, .{});
    defer rom.close();

    var ctx = RaylibDeviceCtx{};
    RaylibDeviceCtx.init(&ctx);
    defer RaylibDeviceCtx.deinit(&ctx);

    var emulator = try Emulator.init(
        allocator,
        .{
            .emulatorType = Chip8.EmulatorType.COSMAC_VIP,
            .deviceCtx = ctx.ctx(),
        },
    );
    defer emulator.deinit();

    var buf: [512]u8 = undefined;
    const read = try rom.readAll(&buf);

    try emulator.run(buf[0..read]);
}

const DeviceCtx = Chip8.DeviceCtx;

const RaylibDeviceCtx = struct {
    const scale = 10;

    fn init(_: *RaylibDeviceCtx) void {
        raylib.initWindow(SCREEN_WIDTH * SCALAR, SCREEN_HEIGHT * SCALAR, "Chip8");
        raylib.setTargetFPS(60);
    }

    fn deinit(_: *RaylibDeviceCtx) void {
        raylib.closeWindow();
    }

    fn draw(_: *anyopaque, screen_buffer: []u8) !void {
        raylib.beginDrawing();
        defer raylib.endDrawing();

        for (0.., screen_buffer) |i, pixel| {
            const x = @as(i32, @intCast(i % SCREEN_WIDTH));
            const y = @as(i32, @intCast(i / SCREEN_WIDTH));

            //raylib.drawFPS(20, 20);
            raylib.drawRectangle(
                x * scale,
                y * scale,
                1 * scale,
                1 * scale,
                if (pixel == 1) raylib.Color.white else raylib.Color.black,
            );
        }
    }

    fn exit(_: *anyopaque) bool {
        return raylib.windowShouldClose();
    }

    fn isPressed(_: *anyopaque, key: u8) bool {
        return switch (key) {
            // [ 1 2 3 C ] == [ 1 2 3 4 ]
            0x01 => raylib.isKeyDown(raylib.KeyboardKey.key_one),
            0x02 => raylib.isKeyDown(raylib.KeyboardKey.key_two),
            0x03 => raylib.isKeyDown(raylib.KeyboardKey.key_three),
            0x0C => raylib.isKeyDown(raylib.KeyboardKey.key_four),
            // [ 4 5 6 D ] == [ Q W E R ]
            0x04 => raylib.isKeyDown(raylib.KeyboardKey.key_q),
            0x05 => raylib.isKeyDown(raylib.KeyboardKey.key_w),
            0x06 => raylib.isKeyDown(raylib.KeyboardKey.key_e),
            0x0D => raylib.isKeyDown(raylib.KeyboardKey.key_r),
            // [ 7 8 9 E ] == [ A S D F]
            0x07 => raylib.isKeyDown(raylib.KeyboardKey.key_a),
            0x08 => raylib.isKeyDown(raylib.KeyboardKey.key_s),
            0x09 => raylib.isKeyDown(raylib.KeyboardKey.key_d),
            0x0E => raylib.isKeyDown(raylib.KeyboardKey.key_f),
            // [ A 0 B F ] == [ Z X C V ]
            0x0A => raylib.isKeyDown(raylib.KeyboardKey.key_z),
            0x00 => raylib.isKeyDown(raylib.KeyboardKey.key_x),
            0x0B => raylib.isKeyDown(raylib.KeyboardKey.key_c),
            0x0F => raylib.isKeyDown(raylib.KeyboardKey.key_v),
            else => return false,
        };
    }

    fn wasPressed(_: *anyopaque) !u8 {
        var key = raylib.getKeyPressed();
        while (key != raylib.KeyboardKey.key_null) {
            switch (key) {
                raylib.KeyboardKey.key_one => return 0x01,
                raylib.KeyboardKey.key_two => return 0x02,
                raylib.KeyboardKey.key_three => return 0x03,
                raylib.KeyboardKey.key_four => return 0x0C,
                raylib.KeyboardKey.key_q => return 0x04,
                raylib.KeyboardKey.key_w => return 0x05,
                raylib.KeyboardKey.key_e => return 0x06,
                raylib.KeyboardKey.key_r => return 0x0D,
                raylib.KeyboardKey.key_a => return 0x07,
                raylib.KeyboardKey.key_s => return 0x08,
                raylib.KeyboardKey.key_d => return 0x09,
                raylib.KeyboardKey.key_f => return 0x0E,
                raylib.KeyboardKey.key_z => return 0x0A,
                raylib.KeyboardKey.key_x => return 0x00,
                raylib.KeyboardKey.key_c => return 0x0B,
                raylib.KeyboardKey.key_v => return 0x0F,
                else => {}, // Ignore everything else
            }
            key = raylib.getKeyPressed(); // Get Next in Queue
        }
        return Chip8.EmulatorError.NO_KEY_PRESSED;
    }

    fn ctx(self: *RaylibDeviceCtx) DeviceCtx {
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

const KeyQueue = @import("./KeyQueue.zig");
const FixedKeyQueue = @import("./FixedKeyQueue.zig");
const c = std.c;

fn poll_input(key_queue: *FixedKeyQueue) !void {
    // const debug = try std.fs.cwd().createFile("debug.log", .{});
    // defer debug.close();
    // const debugger = debug.writer();

    // Standard In has to be put in NON-CANOICAL mode for streamed input
    const std_in = std.io.getStdIn();
    const stdin_fd = std_in.handle;
    const termios = try std.posix.tcgetattr(stdin_fd);
    defer std.posix.tcsetattr(std.posix.STDIN_FILENO, .FLUSH, termios) catch {};

    var raw = termios;
    raw.lflag.ECHO = false;
    raw.lflag.ICANON = false;
    try std.posix.tcsetattr(std.posix.STDIN_FILENO, .FLUSH, raw);

    const reader = std_in.reader();
    var buff: [1]u8 = undefined;
    while (true) {
        const amount = try reader.read(&buff);
        // try debugger.print("We Got: {X}\n", .{buff[0]});
        if (amount > 0) key_queue.push(buff[0]);
        std.time.sleep(2 * std.time.ns_per_s);
    }
}

// We need a thread safe data structure that when the key is being
// pressed down that key value is true, and when it is lifted up
// change that to false. This all needs to be done by std-in.

// ========== Terminal Device Context =============
// Only working for posix systems atm.
const TerminalDeviceCtx = struct {
    allocator: std.mem.Allocator,
    std_out: std.fs.File,

    key_queue: FixedKeyQueue,

    const This = @This();

    fn init(allocator: std.mem.Allocator, std_out: std.fs.File, key_queue: FixedKeyQueue) !This {
        return .{
            .allocator = allocator,
            .std_out = std_out,
            .key_queue = key_queue,
        };
    }

    fn draw(ptr: *anyopaque, screen: []u8) !void {
        const self: *TerminalDeviceCtx = @alignCast(@ptrCast(ptr));

        var row_iter = std.mem.window(
            u8,
            screen,
            Chip8.SCREEN_WIDTH,
            Chip8.SCREEN_WIDTH,
        );

        const writer = self.std_out.writer();

        // Clear Screen
        try writer.print("\x1b[2J", .{}); // clears screen
        try writer.print("\x1b[H", .{}); // cursor to top left

        while (row_iter.next()) |row| {
            var pixels = try self.allocator.alloc(u8, row.len);
            defer self.allocator.free(pixels);

            for (0.., row) |i, pixel| {
                pixels[i] = if (pixel == 1) '@' else ' ';
            }

            try writer.print("{s}\n", .{pixels});
        }
    }

    fn exit(_: *anyopaque) bool {
        return false;
    }

    fn isPressed(_: *anyopaque, _: u8) bool {
        return false;
    }

    fn wasPressed(ptr: *anyopaque) !u8 {
        const self: *TerminalDeviceCtx = @ptrCast(@alignCast(ptr));

        var key = self.key_queue.pop();
        while (!self.key_queue.isEmpty()) {
            switch (key) {
                '1' => return 0x01,
                '2' => return 0x02,
                '3' => return 0x03,
                '4' => return 0x0C,
                'q' => return 0x04,
                'w' => return 0x05,
                'e' => return 0x06,
                'r' => return 0x0D,
                'a' => return 0x07,
                's' => return 0x08,
                'd' => return 0x09,
                'f' => return 0x0E,
                'z' => return 0x0A,
                'x' => return 0x00,
                'c' => return 0x0B,
                'v' => return 0x0F,
                else => {}, // Ignore everything else
            }
            key = self.key_queue.pop(); // Get Next in Queue
        }
        return Chip8.EmulatorError.NO_KEY_PRESSED;
    }

    fn ctx(self: *TerminalDeviceCtx) DeviceCtx {
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

const testing = std.testing;

test "KeyQueue" {
    var kq = KeyQueue.init(testing.allocator);
    defer kq.deinit();

    try kq.enqueue(9);
    try kq.enqueue(8);
    try kq.enqueue(7);
    try kq.enqueue(6);
    try testing.expectEqual(9, kq.dequeue());
    try testing.expectEqual(8, kq.dequeue());
    try testing.expectEqual(7, kq.dequeue());
    try testing.expectEqual(6, kq.dequeue());
}
