//! Root module for pleditor - A simple text editor written in Zig
//! This module exposes the main functionality of the pleditor library.

const std = @import("std");

// Re-export main modules
pub const pleditor = @import("pleditor.zig");
pub const platform = @import("platform.zig");
pub const syntax = @import("syntax.zig");
pub const terminal = @import("terminal.zig");

// Re-export commonly used types from pleditor
pub const State = pleditor.State;
pub const Row = pleditor.Row;
pub const Operation = pleditor.Operation;
pub const OperationParams = pleditor.OperationParams;
pub const OperationType = pleditor.OperationType;
pub const SearchDirection = pleditor.SearchDirection;
pub const Key = pleditor.Key;
pub const CTRL_KEY = pleditor.CTRL_KEY;

// Re-export constants
pub const VERSION = pleditor.VERSION;
pub const TAB_STOP = pleditor.TAB_STOP;
pub const QUIT_CONFIRM_TIMES = pleditor.QUIT_CONFIRM_TIMES;
pub const KEY_ESC = pleditor.KEY_ESC;
pub const KEY_BACKSPACE = pleditor.KEY_BACKSPACE;

// Re-export platform interface
pub const PlatformInterface = platform.PlatformInterface;
pub const setPlatform = platform.setPlatform;
pub const platformInit = platform.platformInit;
pub const platformCleanup = platform.platformCleanup;
pub const platformGetSize = platform.platformGetSize;
pub const platformReadKey = platform.platformReadKey;
pub const platformWrite = platform.platformWrite;
pub const platformReadFile = platform.platformReadFile;
pub const platformWriteFile = platform.platformWriteFile;

// Re-export syntax highlighting
pub const syntaxInit = syntax.syntaxInit;
pub const syntaxUpdateAll = syntax.syntaxUpdateAll;
pub const syntaxColorToAnsi = syntax.syntaxColorToAnsi;
pub const syntaxByFileExt = syntax.syntaxByFileExt;
pub const syntaxUpdateRow = syntax.syntaxUpdateRow;
pub const syntaxUpdateMultiline = syntax.syntaxUpdateMultiline;
pub const Highlight = syntax.Highlight;
pub const Syntax = syntax.Syntax;
pub const HighlightRow = syntax.HighlightRow;

// Version information
pub const version = "0.1.0";

test {
    std.testing.refAllDecls(@This());
}
