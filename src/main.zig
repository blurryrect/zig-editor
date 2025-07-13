//! main.zig - Entry point for pleditor
const std = @import("std");
const pleditor = @import("pleditor.zig");
const platform = @import("platform.zig");
const syntax = @import("syntax.zig");
const linux_platform = @import("platform/linux.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Get command line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // Initialize platform
    platform.setPlatform(&linux_platform.linux_platform);

    if (!platform.platformInit()) {
        std.debug.print("Failed to initialize terminal\n", .{});
        return;
    }
    defer platform.platformCleanup();

    // Initialize editor state
    var state = pleditor.State.init(allocator) catch {
        std.debug.print("Failed to initialize editor state\n", .{});
        return;
    };
    defer state.deinit();

    // Initialize syntax highlighting
    _ = syntax.syntaxInit(&state);

    // Open file if specified
    if (args.len >= 2) {
        if (!state.open(args[1])) {
            return;
        }
    }

    // Set initial status message
    state.setStatusMessage("HELP: Ctrl-S = save/save as | Ctrl-Q = quit | Ctrl-R = toggle line numbers", .{});

    // Main editor loop
    while (!state.should_quit) {
        state.refreshScreen();
        const c = platform.platformReadKey();
        state.handleKeypress(c);
    }
}
