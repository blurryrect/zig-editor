//! linux.zig - Linux implementation of the platform interface
const std = @import("std");
const c = @cImport({
    @cInclude("unistd.h");
    @cInclude("termios.h");
    @cInclude("sys/ioctl.h");
    @cInclude("errno.h");
});

const platform = @import("../platform.zig");
const pleditor = @import("../pleditor.zig");

// Terminal file descriptors
const STDIN_FILENO = 0;
const STDOUT_FILENO = 1;

// Original terminal settings
var orig_termios: c.struct_termios = undefined;

/// Initialize the terminal for raw mode
fn platformInit() bool {
    if (c.tcgetattr(STDIN_FILENO, &orig_termios) == -1) {
        return false;
    }

    // Switch to alternate screen buffer
    _ = c.write(STDOUT_FILENO, "\x1b[?1049h", 8);

    var raw = orig_termios;

    // Input flags: disable break signal, disable CR to NL translation,
    // disable parity checking, disable stripping high bit
    raw.c_iflag &= ~@as(c_uint, c.BRKINT | c.ICRNL | c.INPCK | c.ISTRIP | c.IXON);

    // Output flags: disable post-processing
    raw.c_oflag &= ~@as(c_uint, c.OPOST);

    // Control flags: set 8-bit chars
    raw.c_cflag |= @as(c_uint, c.CS8);

    // Local flags: disable echoing, canonical mode,
    // disable extended functions, disable signal chars
    raw.c_lflag &= ~@as(c_uint, c.ECHO | c.ICANON | c.IEXTEN | c.ISIG);

    // Control chars: set return condition to return as soon as any input is available
    raw.c_cc[c.VMIN] = 0; // No minimum num of bytes required
    raw.c_cc[c.VTIME] = 1; // 100ms timeout between bytes

    if (c.tcsetattr(STDIN_FILENO, c.TCSAFLUSH, &raw) == -1) {
        return false;
    }

    return true;
}

/// Restore terminal settings
fn platformCleanup() void {
    // Return to normal screen buffer
    _ = c.write(STDOUT_FILENO, "\x1b[?1049l", 8);

    // Restore original terminal settings
    _ = c.tcsetattr(STDIN_FILENO, c.TCSAFLUSH, &orig_termios);
}

/// Get the terminal window size
fn platformGetSize(rows: *i32, cols: *i32) bool {
    var ws: c.struct_winsize = undefined;

    if (c.ioctl(STDOUT_FILENO, c.TIOCGWINSZ, &ws) == -1 or ws.ws_col == 0) {
        return false;
    } else {
        cols.* = @intCast(ws.ws_col);
        rows.* = @intCast(ws.ws_row);
        return true;
    }
}

/// Read a key from the terminal
fn platformReadKey() i32 {
    var nread: usize = undefined;
    var ch: u8 = undefined;

    while (true) {
        nread = @intCast(c.read(STDIN_FILENO, &ch, 1));
        if (nread == 1) break;
        if (nread == -1) return @intFromEnum(pleditor.Key.key_err);
    }

    // For non-escape characters, return immediately
    if (ch != pleditor.KEY_ESC) {
        return ch;
    }

    // Handle escape sequences
    var seq: [3]u8 = undefined;

    // Early returns for incomplete sequences
    if (c.read(STDIN_FILENO, &seq[0], 1) != 1) return pleditor.KEY_ESC;
    if (c.read(STDIN_FILENO, &seq[1], 1) != 1) return pleditor.KEY_ESC;

    // Handle '[' sequence type (e.g., cursor keys)
    if (seq[0] == '[') {
        // Handle numeric escape codes (like ESC[1~)
        if (seq[1] >= '0' and seq[1] <= '9') {
            if (c.read(STDIN_FILENO, &seq[2], 1) != 1) return pleditor.KEY_ESC;
            if (seq[2] == '~') {
                return switch (seq[1]) {
                    '1' => @intFromEnum(pleditor.Key.home_key),
                    '3' => @intFromEnum(pleditor.Key.del_key),
                    '4' => @intFromEnum(pleditor.Key.end_key),
                    '5' => @intFromEnum(pleditor.Key.page_up),
                    '6' => @intFromEnum(pleditor.Key.page_down),
                    '7' => @intFromEnum(pleditor.Key.home_key),
                    '8' => @intFromEnum(pleditor.Key.end_key),
                    else => pleditor.KEY_ESC,
                };
            }
            return pleditor.KEY_ESC;
        }

        // Handle arrow keys and others (like ESC[A)
        return switch (seq[1]) {
            'A' => @intFromEnum(pleditor.Key.arrow_up),
            'B' => @intFromEnum(pleditor.Key.arrow_down),
            'C' => @intFromEnum(pleditor.Key.arrow_right),
            'D' => @intFromEnum(pleditor.Key.arrow_left),
            'H' => @intFromEnum(pleditor.Key.home_key),
            'F' => @intFromEnum(pleditor.Key.end_key),
            else => pleditor.KEY_ESC,
        };
    }

    // Handle 'O' sequence type
    if (seq[0] == 'O') {
        return switch (seq[1]) {
            'H' => @intFromEnum(pleditor.Key.home_key),
            'F' => @intFromEnum(pleditor.Key.end_key),
            else => pleditor.KEY_ESC,
        };
    }

    return pleditor.KEY_ESC;
}

/// Write to the terminal
fn platformWrite(s: []const u8) void {
    _ = c.write(STDOUT_FILENO, s.ptr, s.len);
}

/// Read the contents of a file
fn platformReadFile(filename: []const u8, buffer: *[]u8, len: *usize, allocator: std.mem.Allocator) bool {
    // Create null-terminated filename for C API
    const filename_z = allocator.dupeZ(u8, filename) catch return false;
    defer allocator.free(filename_z);

    const file = std.fs.cwd().openFile(filename, .{}) catch return false;
    defer file.close();

    // Get file size
    const file_size = file.getEndPos() catch return false;

    // Allocate buffer
    const file_buffer = allocator.alloc(u8, file_size + 1) catch return false;

    // Read file
    const bytes_read = file.readAll(file_buffer[0..file_size]) catch {
        allocator.free(file_buffer);
        return false;
    };

    if (bytes_read < file_size) {
        allocator.free(file_buffer);
        return false;
    }

    // Success path - null-terminate and return
    file_buffer[bytes_read] = 0;
    buffer.* = file_buffer;
    len.* = bytes_read;
    return true;
}

/// Write buffer to a file
fn platformWriteFile(filename: []const u8, buf: []const u8) bool {
    const file = std.fs.cwd().createFile(filename, .{}) catch return false;
    defer file.close();

    const bytes_written = file.writeAll(buf) catch return false;
    _ = bytes_written;

    return true;
}

/// Linux platform implementation
pub const linux_platform = platform.PlatformInterface{
    .initFn = platformInit,
    .cleanupFn = platformCleanup,
    .getSizeFn = platformGetSize,
    .readKeyFn = platformReadKey,
    .writeFn = platformWrite,
    .readFileFn = platformReadFile,
    .writeFileFn = platformWriteFile,
};
