//! platform.zig - Platform abstraction interface
const std = @import("std");

/// Platform abstraction interface
/// These functions must be implemented for each platform
pub const PlatformInterface = struct {
    /// Initialize terminal settings
    initFn: *const fn () bool,

    /// Restore terminal settings before exit
    cleanupFn: *const fn () void,

    /// Get terminal window size
    getSizeFn: *const fn (*i32, *i32) bool,

    /// Read a key from the terminal
    readKeyFn: *const fn () i32,

    /// Write string to terminal
    writeFn: *const fn ([]const u8) void,

    /// Read file contents
    readFileFn: *const fn ([]const u8, *[]u8, *usize, std.mem.Allocator) bool,

    /// Write buffer to file
    writeFileFn: *const fn ([]const u8, []const u8) bool,

    pub fn init(self: *const PlatformInterface) bool {
        return self.initFn();
    }

    pub fn cleanup(self: *const PlatformInterface) void {
        self.cleanupFn();
    }

    pub fn getSize(self: *const PlatformInterface, rows: *i32, cols: *i32) bool {
        return self.getSizeFn(rows, cols);
    }

    pub fn readKey(self: *const PlatformInterface) i32 {
        return self.readKeyFn();
    }

    pub fn write(self: *const PlatformInterface, s: []const u8) void {
        self.writeFn(s);
    }

    pub fn readFile(self: *const PlatformInterface, filename: []const u8, buffer: *[]u8, len: *usize, allocator: std.mem.Allocator) bool {
        return self.readFileFn(filename, buffer, len, allocator);
    }

    pub fn writeFile(self: *const PlatformInterface, filename: []const u8, buffer: []const u8) bool {
        return self.writeFileFn(filename, buffer);
    }
};

/// Global platform interface instance
pub var platform: *const PlatformInterface = undefined;

/// Initialize the platform interface
pub fn setPlatform(platform_impl: *const PlatformInterface) void {
    platform = platform_impl;
}

/// Convenience functions that use the global platform interface
pub fn platformInit() bool {
    return platform.init();
}

pub fn platformCleanup() void {
    platform.cleanup();
}

pub fn platformGetSize(rows: *i32, cols: *i32) bool {
    return platform.getSize(rows, cols);
}

pub fn platformReadKey() i32 {
    return platform.readKey();
}

pub fn platformWrite(s: []const u8) void {
    platform.write(s);
}

pub fn platformReadFile(filename: []const u8, buffer: *[]u8, len: *usize, allocator: std.mem.Allocator) bool {
    return platform.readFile(filename, buffer, len, allocator);
}

pub fn platformWriteFile(filename: []const u8, buffer: []const u8) bool {
    return platform.writeFile(filename, buffer);
}
