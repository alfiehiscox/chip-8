const std = @import("std");
const This = @This();

allocator: std.mem.Allocator,
start: ?*Node,
end: ?*Node,
mutex: std.Thread.Mutex = .{},

const Node = struct {
    key: u8,
    next: ?*Node,
};

pub fn init(allocator: std.mem.Allocator) @This() {
    return .{
        .allocator = allocator,
        .start = null,
        .end = null,
    };
}

pub fn deinit(self: *This) void {
    self.mutex.lock();
    defer self.mutex.unlock();
    var head = self.start orelse return;
    while (head.next != null) {
        const next = head.next.?;
        self.allocator.destroy(head);
        head = next;
    }
}

pub fn enqueue(self: *This, key: u8) !void {
    self.mutex.lock();
    defer self.mutex.unlock();
    const node = try self.allocator.create(Node);
    node.* = .{ .key = key, .next = null };
    if (self.end) |end| end.next = node else self.start = node;
    self.end = node;
}

pub fn dequeue(self: *This) ?u8 {
    self.mutex.lock();
    defer self.mutex.unlock();
    const start = self.start orelse return null;
    defer self.allocator.destroy(start);
    if (start.next) |next|
        self.start = next
    else {
        self.start = null;
        self.end = null;
    }
    return start.key;
}
