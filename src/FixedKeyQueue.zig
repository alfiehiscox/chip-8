const std = @import("std");
const fifo = std.fifo;
const Condition = std.Thread.Condition;
const This = @This();

const Queue = fifo.LinearFifo(u8, .{ .Static = 256 });

queue: Queue,
mutex: std.Thread.Mutex = .{},
not_full: Condition = .{},
not_empty: Condition = .{},

pub fn init() This {
    return .{ .queue = Queue.init() };
}

pub fn deinit(self: *This) void {
    self.queue.deinit();
}

pub fn pop(self: *This) u8 {
    self.mutex.lock();
    defer self.mutex.unlock();

    while (self.queue.readableLength() == 0) {
        self.not_empty.wait(&self.mutex);
    }

    if (self.queue.readableLength() == 256) {
        self.not_full.signal();
    }

    return self.queue.readItem().?;
}

pub fn push(self: *This, key: u8) void {
    self.mutex.lock();
    defer self.mutex.unlock();

    while (self.queue.readableLength() == 256) {
        self.not_full.wait(&self.mutex);
    }

    if (self.queue.readableLength() == 0) {
        self.not_empty.signal();
    }

    self.queue.writeItemAssumeCapacity(key);
}

pub fn isEmpty(self: *This) bool {
    self.mutex.lock();
    defer self.mutex.unlock();
    return self.queue.readableLength() == 0;
}
