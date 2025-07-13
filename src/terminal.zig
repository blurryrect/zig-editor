//! terminal.zig - VT100 terminal control sequences
const std = @import("std");

// VT100 Terminal Control Sequences
pub const VT100_CLEAR_SCREEN = "\x1b[2J";
pub const VT100_CLEAR_LINE = "\x1b[K";
pub const VT100_CURSOR_HOME = "\x1b[H";
pub const VT100_CURSOR_HIDE = "\x1b[?25l";
pub const VT100_CURSOR_SHOW = "\x1b[?25h";

// ANSI Terminal Colors
pub const VT100_COLOR_RESET = "\x1b[0m";
pub const VT100_COLOR_BLACK = "\x1b[30m";
pub const VT100_COLOR_RED = "\x1b[31m";
pub const VT100_COLOR_GREEN = "\x1b[32m";
pub const VT100_COLOR_YELLOW = "\x1b[33m";
pub const VT100_COLOR_BLUE = "\x1b[34m";
pub const VT100_COLOR_MAGENTA = "\x1b[35m";
pub const VT100_COLOR_CYAN = "\x1b[36m";
pub const VT100_COLOR_WHITE = "\x1b[37m";
pub const VT100_COLOR_DARK_GRAY = "\x1b[0;90m"; // Bright black = dark gray

// Background colors
pub const VT100_BG_BLACK = "\x1b[40m";
pub const VT100_BG_RED = "\x1b[41m";
pub const VT100_BG_GREEN = "\x1b[42m";
pub const VT100_BG_YELLOW = "\x1b[43m";
pub const VT100_BG_BLUE = "\x1b[44m";
pub const VT100_BG_MAGENTA = "\x1b[45m";
pub const VT100_BG_CYAN = "\x1b[46m";
pub const VT100_BG_WHITE = "\x1b[47m";

// Text formatting
pub const VT100_BOLD = "\x1b[1m";
pub const VT100_UNDERLINE = "\x1b[4m";
pub const VT100_BLINK = "\x1b[5m";
pub const VT100_INVERSE = "\x1b[7m";

/// Format cursor position sequence into buffer
pub fn cursorPosition(row: u32, col: u32, buffer: []u8) ![]u8 {
    return std.fmt.bufPrint(buffer, "\x1b[{d};{d}H", .{ row, col });
}
