//! pleditor.zig - Platform-independent core for PL Editor
const std = @import("std");
const terminal = @import("terminal.zig");
const platform = @import("platform.zig");
const syntax = @import("syntax.zig");

/// Editor config
pub const VERSION = "0.1.0";
pub const TAB_STOP = 4;
pub const QUIT_CONFIRM_TIMES = 3;

/// Key definitions
pub const CTRL_KEY = struct {
    pub fn key(k: u8) u8 {
        return k & 0x1f;
    }
};

pub const KEY_ESC: u8 = 0x1b;
pub const KEY_BACKSPACE: u8 = 127;

/// Special key codes
pub const Key = enum(i32) {
    key_err = -1,
    arrow_left = 1000,
    arrow_right,
    arrow_up,
    arrow_down,
    page_up,
    page_down,
    home_key,
    end_key,
    del_key,
};

/// Direction for search
pub const SearchDirection = enum {
    forward,
    backward,
};

/// Undo/Redo operation types
pub const OperationType = enum {
    insert_char,
    delete_char,
    insert_line,
    delete_line,
};

/// Undo/Redo operation parameters
pub const OperationParams = struct {
    type: OperationType,
    cx: i32,
    cy: i32,
    character: i32,
    line: ?[]u8,
    line_size: i32,
};

/// Undo/Redo operation structure
pub const Operation = struct {
    type: OperationType,
    cx: i32,
    cy: i32,
    character: i32,
    line: ?[]u8,
    line_size: i32,
    next: ?*Operation,

    pub fn deinit(self: *Operation, allocator: std.mem.Allocator) void {
        if (self.line) |line| {
            allocator.free(line);
        }
    }
};

/// Row of text in the editor
pub const Row = struct {
    size: i32,
    chars: ?[]u8,
    render_size: i32,
    render: ?[]u8,
    hl: ?syntax.HighlightRow,

    pub fn init(allocator: std.mem.Allocator) Row {
        _ = allocator;
        return Row{
            .size = 0,
            .chars = null,
            .render_size = 0,
            .render = null,
            .hl = null,
        };
    }

    pub fn deinit(self: *Row, allocator: std.mem.Allocator) void {
        if (self.chars) |chars| {
            allocator.free(chars);
            self.chars = null;
        }
        if (self.render) |render| {
            allocator.free(render);
            self.render = null;
        }
        if (self.hl) |*hl| {
            hl.deinit(allocator);
            self.hl = null;
        }
    }
};

/// Editor state
pub const State = struct {
    cx: i32,
    cy: i32,
    rx: i32,
    row_offset: i32,
    col_offset: i32,
    screen_rows: i32,
    screen_cols: i32,
    num_rows: usize,
    rows: ?[]Row,
    dirty: bool,
    filename: ?[]u8,
    status_msg: [80]u8,
    syntax: ?*const syntax.Syntax,
    show_line_numbers: bool,
    undo_stack: ?*Operation,
    redo_stack: ?*Operation,
    should_quit: bool,
    is_unredoing: bool,
    is_searching: bool,
    search_query: ?[]u8,
    last_match_row: i32,
    last_match_col: i32,
    search_direction: SearchDirection,
    allocator: std.mem.Allocator,

    /// For calculating the render index from a chars index
    pub fn cxToRx(self: *State, row: *Row, cx: i32) i32 {
        _ = self;
        var rx: i32 = 0;
        var j: i32 = 0;
        while (j < cx and row.chars != null) : (j += 1) {
            if (row.chars.?[@intCast(j)] == '\t') {
                rx += (TAB_STOP - 1) - @rem(rx, TAB_STOP);
            }
            rx += 1;
        }
        return rx;
    }

    /// Update the render string for a row (for handling tabs, etc.)
    pub fn updateRow(self: *State, row: *Row) void {
        var tabs: i32 = 0;
        var j: i32 = 0;
        while (j < row.size and row.chars != null) : (j += 1) {
            if (row.chars.?[@intCast(j)] == '\t') tabs += 1;
        }

        if (row.render) |render| {
            self.allocator.free(render);
        }

        const new_size = @as(usize, @intCast(row.size + tabs * (TAB_STOP - 1) + 1));
        row.render = self.allocator.alloc(u8, new_size) catch {
            self.should_quit = true;
            return;
        };

        var idx: usize = 0;
        j = 0;
        while (j < row.size and row.chars != null) : (j += 1) {
            if (row.chars.?[@intCast(j)] == '\t') {
                row.render.?[idx] = ' ';
                idx += 1;
                while (@rem(idx, TAB_STOP) != 0) {
                    row.render.?[idx] = ' ';
                    idx += 1;
                }
            } else {
                row.render.?[idx] = row.chars.?[@intCast(j)];
                idx += 1;
            }
        }
        row.render.?[idx] = 0;
        row.render_size = @intCast(idx);

        // Update syntax highlighting for the row if syntax highlighting is enabled
        if (self.syntax != null) {
            syntax.syntaxUpdateMultiline(self, @intCast(self.cy));
        }
    }

    /// Insert a row at the specified position
    pub fn insertRow(self: *State, at: usize, s: []const u8) void {
        if (at > self.num_rows) return;

        // Reallocate rows array
        const new_rows = if (self.rows) |rows|
            self.allocator.realloc(rows, self.num_rows + 1) catch return
        else
            self.allocator.alloc(Row, self.num_rows + 1) catch return;
        self.rows = new_rows;

        // Move existing rows to make space
        if (at < self.num_rows) {
            var i = self.num_rows;
            while (i > at) : (i -= 1) {
                self.rows.?[i] = self.rows.?[i - 1];
            }
        }

        // Initialize new row
        self.rows.?[at] = Row.init(self.allocator);
        self.rows.?[at].size = @intCast(s.len);
        self.rows.?[at].chars = self.allocator.alloc(u8, s.len + 1) catch return;
        @memcpy(self.rows.?[at].chars.?[0..s.len], s);
        self.rows.?[at].chars.?[s.len] = 0;

        self.updateRow(&self.rows.?[at]);

        // Update highlighting for the row if syntax highlighting is enabled
        if (self.syntax != null) {
            syntax.syntaxUpdateMultiline(self, at);
        }

        self.num_rows += 1;
        self.dirty = true;
    }

    /// Delete a row at the specified position
    pub fn deleteRow(self: *State, at: usize) void {
        if (at >= self.num_rows) return;

        self.rows.?[at].deinit(self.allocator);

        // Move rows down to fill the gap
        var i = at;
        while (i < self.num_rows - 1) : (i += 1) {
            self.rows.?[i] = self.rows.?[i + 1];
        }

        self.num_rows -= 1;
        self.dirty = true;
    }

    /// Insert a character at the current cursor position
    pub fn insertChar(self: *State, c: u8) void {
        // Don't insert control characters in the text (except TAB)
        if (std.ascii.isControl(c) and c != '\t') {
            return;
        }

        // Record the operation for undo - store the character being inserted
        const params = OperationParams{
            .type = .insert_char,
            .cx = self.cx,
            .cy = self.cy,
            .character = c,
            .line = null,
            .line_size = 0,
        };
        self.recordOperation(&params);

        if (self.cy == self.num_rows) {
            self.insertRow(self.num_rows, "");
        }

        const row = &self.rows.?[@intCast(self.cy)];
        const new_chars = if (row.chars) |chars|
            self.allocator.realloc(chars, @intCast(row.size + 2)) catch return
        else
            self.allocator.alloc(u8, @intCast(row.size + 2)) catch return;
        row.chars = new_chars;

        // Move characters to make space
        const cx_usize = @as(usize, @intCast(self.cx));
        const remaining = @as(usize, @intCast(row.size - self.cx + 1));
        if (cx_usize < row.chars.?.len and remaining > 0) {
            std.mem.copyBackwards(u8, row.chars.?[cx_usize + 1 .. cx_usize + 1 + remaining], row.chars.?[cx_usize .. cx_usize + remaining]);
        }

        row.size += 1;
        row.chars.?[cx_usize] = c;
        self.updateRow(row);

        // Update syntax highlighting for affected rows
        if (self.syntax != null) {
            syntax.syntaxUpdateMultiline(self, @intCast(self.cy));
        }

        self.cx += 1;
        self.dirty = true;
    }

    /// Insert a newline (Enter key)
    pub fn insertNewline(self: *State) void {
        // Before inserting a newline, we save information about the current row for undo
        if (self.cy < self.num_rows) {
            const row = &self.rows.?[@intCast(self.cy)];

            // Store a copy of the entire row for undoing properly
            const params = OperationParams{
                .type = .insert_line,
                .cx = self.cx,
                .cy = self.cy,
                .character = 0,
                .line = row.chars,
                .line_size = row.size,
            };
            self.recordOperation(&params);
        } else {
            // If we're at the end of the file, just record the position
            const params = OperationParams{
                .type = .insert_line,
                .cx = self.cx,
                .cy = self.cy,
                .character = 0,
                .line = null,
                .line_size = 0,
            };
            self.recordOperation(&params);
        }

        if (self.cx == 0) {
            self.insertRow(@intCast(self.cy), "");
        } else {
            const row = &self.rows.?[@intCast(self.cy)];
            const second_half = row.chars.?[@intCast(self.cx)..@intCast(row.size)];
            self.insertRow(@intCast(self.cy + 1), second_half);

            // Truncate current row
            const new_row = &self.rows.?[@intCast(self.cy)]; // Row pointer may have changed due to realloc
            new_row.size = self.cx;
            new_row.chars.?[@intCast(self.cx)] = 0;
            self.updateRow(new_row);

            // Update syntax highlighting for the modified current row and all affected rows
            if (self.syntax != null) {
                syntax.syntaxUpdateMultiline(self, @intCast(self.cy));
            }
        }
        self.cy += 1;
        self.cx = 0;
    }

    /// Delete the character at the current cursor position
    pub fn deleteChar(self: *State) void {
        if (self.cy == self.num_rows) return;
        if (self.cx == 0 and self.cy == 0) return;

        const row = &self.rows.?[@intCast(self.cy)];
        if (self.cx > 0) {
            // Record the operation for undo - save the character that will be deleted
            const del_char = row.chars.?[@intCast(self.cx - 1)];
            const params = OperationParams{
                .type = .delete_char,
                .cx = self.cx - 1,
                .cy = self.cy,
                .character = del_char,
                .line = null,
                .line_size = 0,
            };
            self.recordOperation(&params);

            const cx_usize = @as(usize, @intCast(self.cx - 1));
            const remaining = @as(usize, @intCast(row.size - self.cx + 1));
            if (cx_usize < row.chars.?.len and remaining > 0) {
                std.mem.copyForwards(u8, row.chars.?[cx_usize .. cx_usize + remaining], row.chars.?[cx_usize + 1 .. cx_usize + 1 + remaining]);
            }

            row.size -= 1;
            self.cx -= 1;
            self.updateRow(row);

            // Update syntax highlighting for affected rows
            if (self.syntax != null) {
                syntax.syntaxUpdateMultiline(self, @intCast(self.cy));
            }

            self.dirty = true;
        } else {
            // At start of line or DEL at end of previous line
            const prev_row = &self.rows.?[@intCast(self.cy - 1)];

            // Save the line for undo
            const params = OperationParams{
                .type = .delete_line,
                .cx = prev_row.size, // Store previous row's end position
                .cy = self.cy,
                .character = 0,
                .line = row.chars,
                .line_size = row.size,
            };
            self.recordOperation(&params);

            // Adjust cursor position for undo to beginning of previous row
            self.cx = prev_row.size;

            // Now merge the lines
            const new_chars = if (prev_row.chars) |chars|
                self.allocator.realloc(chars, @intCast(prev_row.size + row.size + 1)) catch return
            else
                self.allocator.alloc(u8, @intCast(prev_row.size + row.size + 1)) catch return;
            prev_row.chars = new_chars;

            if (row.chars) |chars| {
                @memcpy(prev_row.chars.?[@intCast(prev_row.size)..@intCast(prev_row.size + row.size)], chars[0..@intCast(row.size)]);
            }
            prev_row.size += row.size;
            prev_row.chars.?[@intCast(prev_row.size)] = 0;
            self.updateRow(prev_row);

            // Update syntax highlighting for affected rows
            if (self.syntax != null) {
                syntax.syntaxUpdateMultiline(self, @intCast(self.cy - 1));
            }

            self.deleteRow(@intCast(self.cy));
            self.cy -= 1;
        }
    }

    /// Calculate the number of digits in a given number
    fn digitCount(number: i32) i32 {
        if (number <= 0) return 1; // Handle 0 and negative cases

        var count: i32 = 0;
        var num = number;
        while (num > 0) {
            num = @divTrunc(num, 10);
            count += 1;
        }
        return count;
    }

    /// Calculate the width needed for line numbers (digits + space)
    pub fn getLineNumberWidth(self: *State) i32 {
        if (!self.show_line_numbers) {
            return 0;
        }

        // Calculate the maximum visible line number on screen
        var max_visible_line = self.row_offset + self.screen_rows;
        if (max_visible_line > self.num_rows) {
            max_visible_line = @intCast(self.num_rows);
        }

        // At least show single digit + space
        var line_number_width = digitCount(max_visible_line) + 1;

        // Ensure minimum of 2 characters wide + space
        if (line_number_width < 3) {
            line_number_width = 3;
        }

        return line_number_width;
    }

    /// Scroll the editor if cursor is outside visible area
    pub fn scroll(self: *State) void {
        self.rx = 0;
        if (self.cy < self.num_rows) {
            self.rx = self.cxToRx(&self.rows.?[@intCast(self.cy)], self.cx);
        }

        // Vertical scrolling
        if (self.cy < self.row_offset) {
            self.row_offset = self.cy;
        }
        if (self.cy >= self.row_offset + self.screen_rows) {
            self.row_offset = self.cy - self.screen_rows + 1;
        }

        // Calculate the effective screen width available for text,
        // accounting for line number display width if enabled
        var effective_screen_width = self.screen_cols;
        if (self.show_line_numbers) {
            effective_screen_width -= self.getLineNumberWidth();
        }

        if (self.rx < self.col_offset) {
            self.col_offset = self.rx;
        }
        if (self.rx >= self.col_offset + effective_screen_width) {
            self.col_offset = self.rx - effective_screen_width + 1;
        }
    }

    /// Draw a row of the editor
    pub fn drawRows(self: *State, buffer: []u8, len: *usize) void {
        var y: i32 = 0;
        while (y < self.screen_rows) : (y += 1) {
            const filerow = y + self.row_offset;

            // Draw line numbers if enabled
            if (self.show_line_numbers) {
                const line_number_width = self.getLineNumberWidth();

                // Check if this is a valid file line
                const is_file_line = filerow < self.num_rows;
                const is_current_line = filerow == self.cy;

                // Either it's an existing file line or it's the empty line right after
                // the file that's currently being edited
                if (is_file_line) {
                    if (is_current_line) {
                        // White text for current line
                        len.* += (std.fmt.bufPrint(buffer[len.*..], terminal.VT100_COLOR_WHITE, .{}) catch return).len;
                    } else {
                        // Light gray text for other lines
                        len.* += (std.fmt.bufPrint(buffer[len.*..], terminal.VT100_COLOR_DARK_GRAY, .{}) catch return).len;
                    }

                    // Format line number with correct padding
                    const line_position: usize = @intCast(filerow + 1);
                    const line_num_str = std.fmt.bufPrint(buffer[len.*..], "{:>2} ", .{line_position}) catch return;
                    len.* += line_num_str.len;

                    len.* += (std.fmt.bufPrint(buffer[len.*..], terminal.VT100_COLOR_RESET, .{}) catch return).len;
                } else {
                    // Create padding with correct width
                    var i: i32 = 0;
                    while (i < line_number_width) : (i += 1) {
                        len.* += (std.fmt.bufPrint(buffer[len.*..], " ", .{}) catch return).len;
                    }

                    len.* += (std.fmt.bufPrint(buffer[len.*..], terminal.VT100_COLOR_RESET, .{}) catch return).len;
                }
            }

            if (filerow >= self.num_rows) {
                // Draw welcome message or tilde for empty lines
                if (self.num_rows == 0 and y == @divTrunc(self.screen_rows, 3)) {
                    var welcome: [80]u8 = undefined;
                    const welcome_str = std.fmt.bufPrint(welcome[0..], "pleditor -- version {s}", .{VERSION}) catch return;
                    var welcomelen = @as(i32, @intCast(welcome_str.len));
                    if (welcomelen > self.screen_cols) welcomelen = self.screen_cols;

                    // Center the welcome message
                    var available_width = self.screen_cols;
                    if (self.show_line_numbers) {
                        available_width -= self.getLineNumberWidth();
                    }
                    var padding = @divTrunc(available_width - welcomelen, 2);
                    if (padding > 0) {
                        len.* += (std.fmt.bufPrint(buffer[len.*..], "~", .{}) catch return).len;
                        padding -= 1;
                    }
                    while (padding > 0) : (padding -= 1) {
                        len.* += (std.fmt.bufPrint(buffer[len.*..], " ", .{}) catch return).len;
                    }

                    len.* += (std.fmt.bufPrint(buffer[len.*..], "{s}", .{welcome_str}) catch return).len;
                } else {
                    len.* += (std.fmt.bufPrint(buffer[len.*..], "~", .{}) catch return).len;
                }
            } else {
                // Draw file content
                // Calculate available width for text after accounting for line numbers
                var available_width = self.screen_cols;
                if (self.show_line_numbers) {
                    available_width -= self.getLineNumberWidth();
                }

                var len_to_display = self.rows.?[@intCast(filerow)].render_size - self.col_offset;
                if (len_to_display < 0) len_to_display = 0;
                if (len_to_display > available_width) len_to_display = available_width;

                if (len_to_display > 0) {
                    const row_render = self.rows.?[@intCast(filerow)].render.?;
                    const c = row_render[@intCast(self.col_offset)..@intCast(self.col_offset + len_to_display)];
                    var current_color: i32 = -1;

                    // If this row has highlighting data
                    if (self.rows.?[@intCast(filerow)].hl) |hl| {
                        for (c, 0..) |char, j| {
                            const color = syntax.syntaxColorToAnsi(hl.hl.?[@as(usize, @intCast(self.col_offset)) + j]);

                            if (color != current_color) {
                                current_color = @intCast(color);
                                len.* += (std.fmt.bufPrint(buffer[len.*..], "\x1b[{d}m", .{color}) catch return).len;
                            }

                            if (len.* < buffer.len) {
                                buffer[len.*] = char;
                                len.* += 1;
                            }
                        }
                    } else {
                        @memcpy(buffer[len.* .. len.* + c.len], c);
                        len.* += c.len;
                    }

                    // Reset text color at end of line
                    len.* += (std.fmt.bufPrint(buffer[len.*..], terminal.VT100_COLOR_RESET, .{}) catch return).len;
                }
            }

            // Clear to end of line and add newline
            len.* += (std.fmt.bufPrint(buffer[len.*..], terminal.VT100_CLEAR_LINE ++ "\r\n", .{}) catch return).len;
        }
    }

    /// Truncate long file paths with ellipsis at the beginning
    fn truncatedPath(path: []const u8, max_len: usize, buffer: []u8) []u8 {
        if (path.len <= max_len) {
            // Path is short enough, return as is
            @memcpy(buffer[0..path.len], path);
            return buffer[0..path.len];
        } else {
            // Path is too long, truncate with ellipsis
            const ellipsis = "...";
            @memcpy(buffer[0..ellipsis.len], ellipsis);
            const start_pos = path.len - (max_len - ellipsis.len);
            @memcpy(buffer[ellipsis.len..max_len], path[start_pos..]);
            return buffer[0..max_len];
        }
    }

    /// Draw the status bar at the bottom of the screen
    pub fn drawStatusBar(self: *State, buffer: []u8, len: *usize) void {
        // Inverse video for status bar
        len.* += (std.fmt.bufPrint(buffer[len.*..], terminal.VT100_INVERSE, .{}) catch return).len;

        var status: [80]u8 = undefined;
        var rstatus: [80]u8 = undefined;
        var truncated_buf: [80]u8 = undefined;

        const display_filename = if (self.filename) |filename|
            truncatedPath(filename, 30, truncated_buf[0..])
        else
            "[No Name]";

        const status_str = std.fmt.bufPrint(status[0..], "{s} - {d} lines {s}", .{
            display_filename,
            self.num_rows,
            if (self.dirty) "(modified)" else "",
        }) catch return;

        // Add filetype information if available
        const filetype = if (self.syntax) |s| s.filetype else "no ft";

        const rstatus_str = std.fmt.bufPrint(rstatus[0..], "{s} | {d}/{d} ", .{
            filetype,
            self.cy + 1,
            self.num_rows,
        }) catch return;

        var status_len = @as(i32, @intCast(status_str.len));
        if (status_len > self.screen_cols) status_len = self.screen_cols;

        len.* += (std.fmt.bufPrint(buffer[len.*..], "{s}", .{status_str[0..@intCast(status_len)]}) catch return).len;

        while (status_len < self.screen_cols - @as(i32, @intCast(rstatus_str.len))) : (status_len += 1) {
            len.* += (std.fmt.bufPrint(buffer[len.*..], " ", .{}) catch return).len;
        }

        len.* += (std.fmt.bufPrint(buffer[len.*..], "{s}", .{rstatus_str}) catch return).len;

        // Reset text formatting
        len.* += (std.fmt.bufPrint(buffer[len.*..], terminal.VT100_COLOR_RESET ++ "\r\n", .{}) catch return).len;
    }

    /// Draw the message bar below the status bar
    pub fn drawMessageBar(self: *State, buffer: []u8, len: *usize) void {
        // Clear the message bar
        len.* += (std.fmt.bufPrint(buffer[len.*..], terminal.VT100_CLEAR_LINE, .{}) catch return).len;

        // Show status message if it exists
        const status_msg_len = std.mem.indexOfScalar(u8, &self.status_msg, 0) orelse self.status_msg.len;
        var msglen = @as(i32, @intCast(status_msg_len));
        if (msglen > self.screen_cols) msglen = self.screen_cols;
        if (msglen > 0) {
            len.* += (std.fmt.bufPrint(buffer[len.*..], "{s}", .{self.status_msg[0..@intCast(msglen)]}) catch return).len;
        }
    }

    /// Update the entire screen
    pub fn refreshScreen(self: *State) void {
        self.scroll();

        // Buffer to build screen update in (large enough for entire screen)
        const buffer_size = @as(usize, @intCast(self.screen_rows * self.screen_cols * 5));
        const buffer = self.allocator.alloc(u8, buffer_size) catch return;
        defer self.allocator.free(buffer);

        var len: usize = 0;

        // Hide cursor during screen update to avoid flicker
        len += (std.fmt.bufPrint(buffer[len..], terminal.VT100_CURSOR_HIDE ++ terminal.VT100_CURSOR_HOME, .{}) catch return).len;

        // Draw rows of text
        self.drawRows(buffer, &len);

        // Draw status bar and message bar
        self.drawStatusBar(buffer, &len);
        self.drawMessageBar(buffer, &len);

        // Position cursor
        var cursor_pos: [32]u8 = undefined;
        var cursor_screen_x = self.rx - self.col_offset + 1;

        // Add offset for line numbers if enabled
        if (self.show_line_numbers) {
            cursor_screen_x += self.getLineNumberWidth();
        }

        const cursor_str = terminal.cursorPosition(@intCast(self.cy - self.row_offset + 1), @intCast(cursor_screen_x), cursor_pos[0..]) catch return;
        len += (std.fmt.bufPrint(buffer[len..], "{s}", .{cursor_str}) catch return).len;

        // Show cursor
        len += (std.fmt.bufPrint(buffer[len..], terminal.VT100_CURSOR_SHOW, .{}) catch return).len;

        // Write buffer to terminal
        platform.platformWrite(buffer[0..len]);
    }

    /// Set a status message to display in the message bar
    pub fn setStatusMessage(self: *State, comptime fmt: []const u8, args: anytype) void {
        _ = std.fmt.bufPrint(&self.status_msg, fmt, args) catch return;
    }

    /// Display a prompt in the status bar and get input
    pub fn prompt(self: *State, prompt_text: []const u8) ?[]u8 {
        var bufsize: usize = 128;
        var buf = self.allocator.alloc(u8, bufsize) catch return null;
        var buflen: usize = 0;
        buf[0] = 0;

        while (true) {
            // Display the prompt and current input
            self.setStatusMessage("{s}: {s}", .{ prompt_text, buf[0..buflen] });
            self.refreshScreen();

            const c = platform.platformReadKey();

            if (c == @intFromEnum(Key.del_key) or c == KEY_BACKSPACE) {
                // Handle backspace/delete
                if (buflen > 0) {
                    buflen -= 1;
                    buf[buflen] = 0;
                }
            } else if (c == '\r' or c == '\n') {
                // Handle Enter/Return
                if (buflen != 0) {
                    self.setStatusMessage("", .{});
                    return self.allocator.realloc(buf, buflen + 1) catch buf;
                }
            } else if (c == KEY_ESC or c == CTRL_KEY.key('q')) {
                // Handle escape or quit
                self.setStatusMessage("", .{});
                self.allocator.free(buf);
                return null;
            } else if (!std.ascii.isControl(@intCast(c)) and c < 128) {
                // Append character to buffer
                if (buflen == bufsize - 1) {
                    bufsize *= 2;
                    buf = self.allocator.realloc(buf, bufsize) catch {
                        self.allocator.free(buf);
                        return null;
                    };
                }
                buf[buflen] = @intCast(c);
                buflen += 1;
                buf[buflen] = 0;
            }
        }
    }

    /// Move the cursor based on key press
    pub fn moveCursor(self: *State, key: i32) void {
        const row = if (self.cy >= self.num_rows) null else &self.rows.?[@intCast(self.cy)];

        switch (key) {
            @intFromEnum(Key.arrow_left) => {
                if (self.cx > 0) {
                    self.cx -= 1;
                } else if (self.cy > 0) {
                    // Move to end of previous line
                    self.cy -= 1;
                    self.cx = self.rows.?[@intCast(self.cy)].size;
                }
            },
            @intFromEnum(Key.arrow_right) => {
                if (row != null and self.cx < row.?.size) {
                    self.cx += 1;
                } else if (row != null and self.cx == row.?.size) {
                    // Move to beginning of next line
                    self.cy += 1;
                    self.cx = 0;
                }
            },
            @intFromEnum(Key.arrow_up) => {
                if (self.cy > 0) self.cy -= 1;
            },
            @intFromEnum(Key.arrow_down) => {
                if (self.cy < self.num_rows - 1) self.cy += 1;
            },
            else => {},
        }

        // Snap cursor to end of line if it's beyond line end
        const current_row = if (self.cy >= self.num_rows) null else &self.rows.?[@intCast(self.cy)];
        const rowlen = if (current_row) |r| r.size else 0;
        if (self.cx > rowlen) {
            self.cx = rowlen;
        }
    }

    /// Save the current file
    pub fn save(self: *State) void {
        // If no filename set, prompt the user for one
        if (self.filename == null) {
            const filename = self.prompt("Save as") orelse {
                self.setStatusMessage("Save aborted", .{});
                return;
            };
            if (self.filename) |old_filename| {
                self.allocator.free(old_filename);
            }
            self.filename = filename;

            // Select syntax highlighting based on new filename
            syntax.syntaxByFileExt(self, self.filename.?);

            // Apply highlighting if a syntax was selected
            if (self.syntax != null) {
                syntax.syntaxUpdateAll(self);
            }
        }

        // Create a single string of entire file
        var totlen: usize = 0;
        for (self.rows.?[0..self.num_rows]) |row| {
            totlen += @intCast(row.size + 1); // +1 for newline
        }

        const buf = self.allocator.alloc(u8, totlen + 1) catch {
            self.setStatusMessage("Can't save! Memory allocation failed", .{});
            return;
        };
        defer self.allocator.free(buf);

        var p: usize = 0;
        for (self.rows.?[0..self.num_rows]) |row| {
            if (row.chars) |chars| {
                @memcpy(buf[p .. p + @as(usize, @intCast(row.size))], chars[0..@intCast(row.size)]);
                p += @intCast(row.size);
            }
            buf[p] = '\n';
            p += 1;
        }

        // Write to file
        if (platform.platformWriteFile(self.filename.?, buf[0..totlen])) {
            self.dirty = false;
            self.setStatusMessage("{d} bytes written to disk", .{totlen});
        } else {
            self.setStatusMessage("Can't save! I/O error may occurred", .{});
        }
    }

    /// Process a keypress
    pub fn handleKeypress(self: *State, c: i32) void {
        const static = struct {
            var quit_times: i32 = QUIT_CONFIRM_TIMES;
        };

        // If in search mode, handle search-specific keys
        if (self.is_searching) {
            switch (c) {
                CTRL_KEY.key('n') => {
                    self.searchNext();
                    return;
                },
                CTRL_KEY.key('p') => {
                    self.searchPrevious();
                    return;
                },
                '\r', '\n', KEY_ESC, CTRL_KEY.key('q') => {
                    self.searchExit();
                    return;
                },
                else => {},
            }
        }

        // Clear status message on any keypress unless we're confirming quit
        if (!(c == CTRL_KEY.key('q') and self.dirty and static.quit_times > 0)) {
            self.setStatusMessage("", .{});
        }

        switch (c) {
            CTRL_KEY.key('q') => {
                if (self.dirty and static.quit_times > 0) {
                    self.setStatusMessage("WARNING!!! File has unsaved changes. Press Ctrl-Q {d} more times to quit.", .{static.quit_times});
                    static.quit_times -= 1;
                    return;
                }
                // Clear screen and reposition cursor before exit
                platform.platformWrite(terminal.VT100_CLEAR_SCREEN ++ terminal.VT100_CURSOR_HOME);
                self.should_quit = true;
            },
            CTRL_KEY.key('s') => {
                self.save();
            },
            CTRL_KEY.key('f') => {
                self.searchInit();
            },
            CTRL_KEY.key('r') => {
                self.show_line_numbers = !self.show_line_numbers;
                // Update status message to show current line number state
                self.setStatusMessage("Line numbers: {s}", .{if (self.show_line_numbers) "ON" else "OFF"});
            },
            CTRL_KEY.key('z') => {
                self.applyUndo();
            },
            CTRL_KEY.key('y') => {
                self.applyRedo();
            },
            KEY_BACKSPACE, CTRL_KEY.key('h') => {
                self.deleteChar();
            },
            @intFromEnum(Key.del_key) => {
                // If we're at the end of the document, do nothing
                if (self.num_rows == 0 or
                    (self.cy == self.num_rows - 1 and
                        self.cx == self.rows.?[@intCast(self.cy)].size))
                {
                    return;
                }
                // Store original cursor position before moving right
                const orig_cx = self.cx;
                const orig_cy = self.cy;

                self.moveCursor(@intFromEnum(Key.arrow_right));
                self.deleteChar();

                // Update the undo operation with the original cursor position
                // and mark the position by making cx negative to signal it was a DEL operation
                if (self.undo_stack) |undo_op| {
                    if (undo_op.type == .delete_char) {
                        undo_op.cx = -orig_cx - 1; // Store as negative to mark DEL op
                        undo_op.cy = orig_cy;
                    }
                }
            },
            '\r', '\n' => {
                self.insertNewline();
            },
            CTRL_KEY.key('l'), KEY_ESC => {
                // Just refresh screen
            },
            @intFromEnum(Key.arrow_up), @intFromEnum(Key.arrow_down), @intFromEnum(Key.arrow_left), @intFromEnum(Key.arrow_right) => {
                self.moveCursor(c);
            },
            @intFromEnum(Key.home_key) => {
                self.cx = 0;
            },
            @intFromEnum(Key.end_key) => {
                if (self.cy < self.num_rows) {
                    self.cx = self.rows.?[@intCast(self.cy)].size;
                }
            },
            @intFromEnum(Key.page_up), @intFromEnum(Key.page_down) => {
                if (c == @intFromEnum(Key.page_up)) {
                    self.cy = self.row_offset;
                } else if (c == @intFromEnum(Key.page_down)) {
                    self.cy = self.row_offset + self.screen_rows - 1;
                    if (self.cy > self.num_rows - 1) self.cy = @intCast(self.num_rows - 1);
                }

                var times = self.screen_rows;
                while (times > 0) : (times -= 1) {
                    self.moveCursor(if (c == @intFromEnum(Key.page_up)) @intFromEnum(Key.arrow_up) else @intFromEnum(Key.arrow_down));
                }
            },
            else => {
                self.insertChar(@intCast(c));
            },
        }

        static.quit_times = QUIT_CONFIRM_TIMES;
    }

    /// Initialize the editor state
    pub fn init(allocator: std.mem.Allocator) !State {
        var state = State{
            .cx = 0,
            .cy = 0,
            .rx = 0,
            .row_offset = 0,
            .col_offset = 0,
            .screen_rows = 24,
            .screen_cols = 80,
            .num_rows = 0,
            .rows = null,
            .dirty = false,
            .filename = null,
            .status_msg = std.mem.zeroes([80]u8),
            .syntax = null,
            .show_line_numbers = true,
            .undo_stack = null,
            .redo_stack = null,
            .should_quit = false,
            .is_unredoing = false,
            .is_searching = false,
            .search_query = null,
            .last_match_row = -1,
            .last_match_col = -1,
            .search_direction = .forward,
            .allocator = allocator,
        };

        if (!platform.platformGetSize(&state.screen_rows, &state.screen_cols)) {
            // Fallback if window size detection fails
            state.screen_rows = 24;
            state.screen_cols = 80;
        }

        // Leave room for status line and message bar
        state.screen_rows -= 2;

        return state;
    }

    /// Open a file in the editor
    pub fn open(self: *State, filename: []const u8) bool {
        if (self.filename) |old_filename| {
            self.allocator.free(old_filename);
        }

        // Copy the filename to the state
        self.filename = self.allocator.dupe(u8, filename) catch return false;

        var buffer: []u8 = undefined;
        var len: usize = undefined;

        if (!platform.platformReadFile(filename, &buffer, &len, self.allocator)) {
            self.setStatusMessage("New file: {s}", .{filename});

            // Select syntax highlighting based on filename
            syntax.syntaxByFileExt(self, filename);

            // No rows to highlight yet
            return true;
        }
        defer self.allocator.free(buffer);

        // Parse the file contents into rows
        var line_start: usize = 0;
        var i: usize = 0;
        while (i <= len) : (i += 1) {
            if (i == len or buffer[i] == '\n') {
                const line_length = i - line_start;
                self.insertRow(self.num_rows, buffer[line_start .. line_start + line_length]);
                line_start = i + 1;
            }
        }

        self.dirty = false;

        // Select syntax highlighting based on filename
        syntax.syntaxByFileExt(self, filename);

        // Apply syntax highlighting to all rows
        syntax.syntaxUpdateAll(self);

        return true;
    }

    /// Free editor resources
    pub fn deinit(self: *State) void {
        // Free each row
        if (self.rows) |rows| {
            for (rows[0..self.num_rows]) |*row| {
                row.deinit(self.allocator);
            }
            self.allocator.free(rows);
        }
        if (self.filename) |filename| {
            self.allocator.free(filename);
        }
        if (self.search_query) |query| {
            self.allocator.free(query);
        }
        self.freeOperationStack(&self.undo_stack);
        self.freeOperationStack(&self.redo_stack);
    }

    pub fn recordOperation(self: *State, params: *const OperationParams) void {
        // Don't record undo operations when undoing or redoing
        if (self.is_unredoing) return;

        // Clear redo stack when a new edit is made
        self.freeOperationStack(&self.redo_stack);

        const op = self.allocator.create(Operation) catch return;
        op.type = params.type;
        op.cx = params.cx;
        op.cy = params.cy;
        op.character = params.character;
        op.line = null;
        op.line_size = params.line_size;

        if (params.line) |line| {
            op.line = self.allocator.alloc(u8, @intCast(params.line_size + 1)) catch {
                self.allocator.destroy(op);
                return;
            };
            if (params.line_size > 0) {
                @memcpy(op.line.?[0..@intCast(params.line_size)], line[0..@intCast(params.line_size)]);
            }
            op.line.?[@intCast(params.line_size)] = 0;
        }

        op.next = self.undo_stack;
        self.undo_stack = op;
    }

    pub fn freeOperationStack(self: *State, stack: *?*Operation) void {
        var op = stack.*;
        while (op) |current| {
            const next = current.next;
            current.deinit(self.allocator);
            self.allocator.destroy(current);
            op = next;
        }
        stack.* = null;
    }

    pub fn applyUndo(self: *State) void {
        if (self.undo_stack == null) {
            self.setStatusMessage("Nothing to undo", .{});
            return;
        }

        // Set flag to prevent recording operations while undoing
        self.is_unredoing = true;
        defer self.is_unredoing = false;

        const op = self.undo_stack.?;
        self.undo_stack = op.next;

        // Save this operation to the redo stack
        const redo_op = self.allocator.create(Operation) catch {
            self.setStatusMessage("Undo failed: memory allocation error", .{});
            return;
        };

        redo_op.type = op.type;
        redo_op.cx = op.cx;
        redo_op.cy = op.cy;
        redo_op.character = op.character;
        redo_op.line = null;
        redo_op.line_size = op.line_size;

        if (op.line) |line| {
            redo_op.line = self.allocator.alloc(u8, @intCast(op.line_size + 1)) catch {
                self.allocator.destroy(redo_op);
                self.setStatusMessage("Undo failed: memory allocation error", .{});
                return;
            };
            if (op.line_size > 0) {
                @memcpy(redo_op.line.?[0..@intCast(op.line_size)], line[0..@intCast(op.line_size)]);
            }
            redo_op.line.?[@intCast(op.line_size)] = 0;
        }

        redo_op.next = self.redo_stack;
        self.redo_stack = redo_op;

        switch (op.type) {
            .insert_char => {
                // For insert char, we need to delete the character that was inserted
                self.cx = op.cx;
                self.cy = op.cy;
                if (self.cy < self.num_rows) {
                    const row = &self.rows.?[@intCast(self.cy)];
                    if (self.cx < row.size) {
                        // Get the character for redo before deleting it
                        redo_op.character = row.chars.?[@intCast(self.cx)];

                        // Delete the character at the cursor position
                        const cx_usize = @as(usize, @intCast(self.cx));
                        const remaining = @as(usize, @intCast(row.size - self.cx));
                        if (cx_usize < row.chars.?.len and remaining > 0) {
                            std.mem.copyForwards(u8, row.chars.?[cx_usize .. cx_usize + remaining], row.chars.?[cx_usize + 1 .. cx_usize + 1 + remaining]);
                        }
                        row.size -= 1;
                        self.updateRow(row);
                        self.dirty = true;
                        if (self.syntax != null) {
                            syntax.syntaxUpdateMultiline(self, @intCast(self.cy));
                        }
                    }
                }
            },
            .delete_char => {
                // For delete char, we need to re-insert the character
                var is_del_operation = false;

                // Check if this was a DEL operation (marked by negative cx)
                if (op.cx < 0) {
                    is_del_operation = true;
                    self.cx = -op.cx - 1; // Extract the original position
                } else {
                    self.cx = op.cx;
                }
                self.cy = op.cy;

                if (op.character != 0) {
                    // Insert character
                    if (self.cy == self.num_rows) {
                        self.insertRow(self.num_rows, "");
                    }

                    const row = &self.rows.?[@intCast(self.cy)];
                    const new_chars = if (row.chars) |chars|
                        self.allocator.realloc(chars, @intCast(row.size + 2)) catch return
                    else
                        self.allocator.alloc(u8, @intCast(row.size + 2)) catch return;
                    row.chars = new_chars;

                    const cx_usize = @as(usize, @intCast(self.cx));
                    const remaining = @as(usize, @intCast(row.size - self.cx + 1));
                    if (cx_usize < row.chars.?.len and remaining > 0) {
                        std.mem.copyBackwards(u8, row.chars.?[cx_usize + 1 .. cx_usize + 1 + remaining], row.chars.?[cx_usize .. cx_usize + remaining]);
                    }
                    row.size += 1;
                    row.chars.?[cx_usize] = @intCast(op.character);
                    self.updateRow(row);

                    if (self.syntax != null) {
                        syntax.syntaxUpdateMultiline(self, @intCast(self.cy));
                    }

                    // Only increment cursor for backspace, not for DEL
                    if (!is_del_operation) {
                        self.cx += 1;
                    }
                    self.dirty = true;
                }
            },
            .insert_line => {
                // For insert line, we need to properly merge the split lines back
                if (self.cy < self.num_rows) {
                    // Set cursor to the row that had the Enter key pressed
                    self.cy = op.cy;

                    // When we have the original line data from before the split
                    if (op.line) |line| {
                        // First handle the case of a proper newline (not at the beginning of a line)
                        if (op.cx > 0) {
                            // Delete the current row that was split
                            self.deleteRow(@intCast(self.cy));

                            // Insert the original line content that was saved before splitting
                            self.insertRow(@intCast(self.cy), line[0..@intCast(op.line_size)]);

                            // Delete any content from the next line that would be duplicate
                            if (self.cy + 1 < self.num_rows) {
                                // Delete the next line (which was created by the Enter key)
                                self.deleteRow(@intCast(self.cy + 1));
                            }
                        } else {
                            // For newline at the beginning of a line, just remove the empty line
                            self.deleteRow(@intCast(self.cy));
                        }

                        // Update the dirty flag since we've modified the content
                        self.dirty = true;
                    } else {
                        // For simple empty line insertion, just delete the row
                        self.deleteRow(@intCast(self.cy));
                        self.dirty = true;
                    }

                    // Set cursor to the position before the newline was inserted
                    self.cx = op.cx;
                }
            },
            .delete_line => {
                // Only break if the line pointer is NULL
                if (op.line == null) return;

                // Handle special case for Delete at end of line
                if (op.cy > 0 and op.cy <= self.num_rows) {
                    const prev_row = &self.rows.?[@intCast(op.cy - 1)];

                    // Check if this is a line joining operation (DEL at line end)
                    if (op.cx > 0) {
                        // Truncate previous line to remove second line content
                        prev_row.chars.?[@intCast(op.cx)] = 0;
                        prev_row.size = op.cx;
                        self.updateRow(prev_row);

                        if (self.syntax != null) {
                            syntax.syntaxUpdateMultiline(self, @intCast(op.cy - 1));
                        }
                    } else {
                        // Original backspace at line start case
                        const match_start = prev_row.size - op.line_size;

                        // Check if previous line ends with deleted line content
                        if (prev_row.size >= op.line_size and prev_row.chars != null and op.line != null) {
                            const prev_chars = prev_row.chars.?;
                            const del_line = op.line.?;
                            var matches = true;
                            var i: usize = 0;
                            while (i < op.line_size) : (i += 1) {
                                if (prev_chars[@as(usize, @intCast(match_start)) + i] != del_line[i]) {
                                    matches = false;
                                    break;
                                }
                            }
                            if (matches) {
                                // Truncate previous line
                                prev_chars[@intCast(match_start)] = 0;
                                prev_row.size = match_start;
                                self.updateRow(prev_row);

                                if (self.syntax != null) {
                                    syntax.syntaxUpdateMultiline(self, @intCast(op.cy - 1));
                                }
                            }
                        }
                    }
                }

                // Re-insert the deleted line
                self.insertRow(@intCast(op.cy), op.line.?[0..@intCast(op.line_size)]);

                // Position cursor at end of previous line for DEL at end of line case
                if (op.cy > 0) {
                    self.cy = op.cy - 1;
                    self.cx = op.cx;
                } else {
                    self.cy = op.cy;
                    self.cx = 0;
                }

                self.dirty = true;
            },
        }

        // Free the undo operation
        op.deinit(self.allocator);
        self.allocator.destroy(op);

        self.setStatusMessage("Undo successful", .{});
    }

    pub fn applyRedo(self: *State) void {
        if (self.redo_stack == null) {
            self.setStatusMessage("Nothing to redo", .{});
            return;
        }

        // Set flag to prevent recording operations while redoing
        self.is_unredoing = true;
        defer self.is_unredoing = false;

        const op = self.redo_stack.?;
        self.redo_stack = op.next;

        // Save this operation to the undo stack
        const undo_op = self.allocator.create(Operation) catch {
            self.setStatusMessage("Redo failed: memory allocation error", .{});
            return;
        };

        undo_op.type = op.type;
        undo_op.cx = op.cx;
        undo_op.cy = op.cy;
        undo_op.character = op.character;
        undo_op.line = null;
        undo_op.line_size = op.line_size;

        if (op.line) |line| {
            undo_op.line = self.allocator.alloc(u8, @intCast(op.line_size + 1)) catch {
                self.allocator.destroy(undo_op);
                self.setStatusMessage("Redo failed: memory allocation error", .{});
                return;
            };
            if (op.line_size > 0) {
                @memcpy(undo_op.line.?[0..@intCast(op.line_size)], line[0..@intCast(op.line_size)]);
            }
            undo_op.line.?[@intCast(op.line_size)] = 0;
        }

        undo_op.next = self.undo_stack;
        self.undo_stack = undo_op;

        // Apply the redo operation (similar to undo but in reverse)
        switch (op.type) {
            .insert_char => {
                // For redo of insert, we need to re-insert the character
                self.cx = op.cx;
                self.cy = op.cy;

                // Insert character
                if (self.cy == self.num_rows) {
                    self.insertRow(self.num_rows, "");
                }

                if (self.cy < self.num_rows) {
                    const row = &self.rows.?[@intCast(self.cy)];
                    const new_chars = if (row.chars) |chars|
                        self.allocator.realloc(chars, @intCast(row.size + 2)) catch return
                    else
                        self.allocator.alloc(u8, @intCast(row.size + 2)) catch return;
                    row.chars = new_chars;

                    const cx_usize = @as(usize, @intCast(self.cx));
                    const remaining = @as(usize, @intCast(row.size - self.cx + 1));
                    if (cx_usize < row.chars.?.len and remaining > 0) {
                        std.mem.copyBackwards(u8, row.chars.?[cx_usize + 1 .. cx_usize + 1 + remaining], row.chars.?[cx_usize .. cx_usize + remaining]);
                    }
                    row.size += 1;
                    row.chars.?[cx_usize] = @intCast(op.character);
                    self.updateRow(row);

                    if (self.syntax != null) {
                        syntax.syntaxUpdateMultiline(self, @intCast(self.cy));
                    }

                    self.cx += 1;
                    self.dirty = true;
                }
            },
            .delete_char => {
                // For redo of delete, we need to delete the character again
                var is_del_operation = false;

                // Check if this was a DEL operation (marked by negative cx)
                if (op.cx < 0) {
                    is_del_operation = true;
                    self.cx = -op.cx - 1; // Extract the original position
                } else {
                    self.cx = op.cx;
                }
                self.cy = op.cy;

                if (is_del_operation) {
                    // For DEL operation, simulate DEL key press
                    // Move cursor right, then delete
                    self.moveCursor(@intFromEnum(Key.arrow_right));
                    self.deleteChar();
                } else if (self.cy < self.num_rows) {
                    const row = &self.rows.?[@intCast(self.cy)];
                    if (self.cx == 0 and self.cy > 0) {
                        // This is a line join operation (deletion at beginning of line)
                        // Move cursor to the end of previous line where characters will be joined
                        const prev_row = &self.rows.?[@intCast(self.cy - 1)];
                        const prev_row_size = prev_row.size;

                        // Perform the delete character operation which will join the lines
                        self.deleteChar();

                        // Ensure cursor is at the join point
                        self.cy = op.cy - 1;
                        self.cx = prev_row_size;
                    } else if (self.cx < row.size) {
                        // Delete the character at the cursor position
                        const cx_usize = @as(usize, @intCast(self.cx));
                        const remaining = @as(usize, @intCast(row.size - self.cx));
                        if (cx_usize < row.chars.?.len and remaining > 0) {
                            std.mem.copyForwards(u8, row.chars.?[cx_usize .. cx_usize + remaining], row.chars.?[cx_usize + 1 .. cx_usize + 1 + remaining]);
                        }
                        row.size -= 1;
                        self.updateRow(row);
                        self.dirty = true;
                        if (self.syntax != null) {
                            syntax.syntaxUpdateMultiline(self, @intCast(self.cy));
                        }
                    }
                }
            },
            .insert_line => {
                // For redo of line insert, we need to re-insert the newline
                self.cx = op.cx;
                self.cy = op.cy;

                if (self.cy < self.num_rows) {
                    const row = &self.rows.?[@intCast(self.cy)];

                    if (op.cx == 0) {
                        // Insert an empty line before the current line
                        self.insertRow(@intCast(self.cy), "");
                        self.cy += 1;
                    } else if (op.cx <= row.size) {
                        // Split the line at cursor position
                        const second_half = row.chars.?[@intCast(op.cx)..@intCast(row.size)];
                        self.insertRow(@intCast(self.cy + 1), second_half);

                        // Truncate current line
                        const current_row = &self.rows.?[@intCast(self.cy)]; // Row pointer may have changed due to realloc
                        current_row.size = op.cx;
                        current_row.chars.?[@intCast(op.cx)] = 0;
                        self.updateRow(current_row);

                        if (self.syntax != null) {
                            syntax.syntaxUpdateRow(self, @intCast(self.cy));
                            syntax.syntaxUpdateRow(self, @intCast(self.cy + 1));
                        }

                        // Move cursor to beginning of next line
                        self.cy += 1;
                        self.cx = 0;
                    }

                    self.dirty = true;
                }
            },
            .delete_line => {
                // For redo of line delete, we need to delete the line again
                self.cx = op.cx;
                self.cy = op.cy;

                if (self.cy < self.num_rows) {
                    // Update the undo operation to save the current line content
                    if (undo_op.line) |old_line| {
                        self.allocator.free(old_line);
                        undo_op.line = null;
                    }

                    const current_row = &self.rows.?[@intCast(self.cy)];
                    undo_op.line_size = current_row.size;

                    undo_op.line = self.allocator.alloc(u8, @intCast(current_row.size + 1)) catch null;
                    if (undo_op.line) |new_line| {
                        if (current_row.chars) |chars| {
                            @memcpy(new_line[0..@intCast(current_row.size)], chars[0..@intCast(current_row.size)]);
                        }
                        new_line[@intCast(current_row.size)] = 0;
                    }

                    // Check if this is a DEL key operation at the end of the previous line
                    if (self.cy > 0 and op.line != null) {
                        // This is a DEL key at end of line case
                        const prev_row = &self.rows.?[@intCast(self.cy - 1)];
                        const join_point = prev_row.size;

                        // Merge the content with the previous line
                        const new_chars = if (prev_row.chars) |chars|
                            self.allocator.realloc(chars, @intCast(prev_row.size + op.line_size + 1)) catch return
                        else
                            self.allocator.alloc(u8, @intCast(prev_row.size + op.line_size + 1)) catch return;
                        prev_row.chars = new_chars;

                        if (op.line) |line| {
                            @memcpy(prev_row.chars.?[@intCast(prev_row.size)..@intCast(prev_row.size + op.line_size)], line[0..@intCast(op.line_size)]);
                        }
                        prev_row.size += op.line_size;
                        prev_row.chars.?[@intCast(prev_row.size)] = 0;
                        self.updateRow(prev_row);

                        // Update syntax highlighting for the next row and all affected rows
                        if (self.syntax != null) {
                            syntax.syntaxUpdateMultiline(self, @intCast(self.cy - 1));
                        }

                        // Delete the line
                        self.deleteRow(@intCast(self.cy));

                        // Position cursor at the join point
                        self.cy -= 1;
                        self.cx = @intCast(join_point);
                    } else {
                        // Regular line delete
                        self.deleteRow(@intCast(self.cy));

                        // If this was a DEL at end of line, position cursor at end of previous line
                        if (self.cy > 0 and op.cx > 0) {
                            self.cy -= 1;
                            self.cx = op.cx;
                        }
                    }
                    self.dirty = true;
                }
            },
        }

        // Free the redo operation
        op.deinit(self.allocator);
        self.allocator.destroy(op);

        self.setStatusMessage("Redo successful", .{});
    }

    /// Initialize search mode with a prompt for the query
    pub fn searchInit(self: *State) void {
        const query = self.prompt("Searching") orelse return;

        // Free any existing search query
        if (self.search_query) |old_query| {
            self.allocator.free(old_query);
        }

        // Save the search query and initialize search state
        self.search_query = query;
        self.is_searching = true;
        self.last_match_row = -1;
        self.last_match_col = -1;
        self.search_direction = .forward;

        // Perform initial search
        self.searchNext();
    }

    /// Find the next occurrence of the search query
    pub fn searchNext(self: *State) void {
        const query = self.search_query orelse return;

        self.search_direction = .forward;

        // Start from the current position or the last match + 1
        const start_row = if (self.last_match_row == -1) self.cy else self.last_match_row;
        const start_col = if (self.last_match_col == -1) self.cx + 1 else self.last_match_col + 1;

        // Loop through rows starting from the current position
        var i: usize = 0;
        while (i < self.num_rows) : (i += 1) {
            const current_row = @as(usize, @intCast(@rem(start_row + @as(i32, @intCast(i)), @as(i32, @intCast(self.num_rows)))));
            const row = &self.rows.?[current_row];

            // If we've wrapped around to the first row, make sure we start from beginning
            const col_offset = if (i == 0) @as(usize, @intCast(start_col)) else 0;

            if (col_offset > row.size) {
                // If we're beyond the end of this row, move to the next one
                continue;
            }

            // Look for the search term in this row
            if (row.chars) |chars| {
                const search_start = if (col_offset <= chars.len) col_offset else chars.len;
                if (std.mem.indexOf(u8, chars[search_start..@intCast(row.size)], query)) |match_offset| {
                    // Found a match!
                    const match_col = @as(i32, @intCast(search_start + match_offset));

                    // Update cursor position to the match
                    self.cy = @intCast(current_row);
                    self.cx = match_col;

                    // Save the match position
                    self.last_match_row = @intCast(current_row);
                    self.last_match_col = match_col;

                    // Ensure the match is visible on screen
                    self.row_offset = self.cy - @divTrunc(self.screen_rows, 2);
                    if (self.row_offset < 0) self.row_offset = 0;

                    self.setStatusMessage("Match found ('{s}'). Ctrl-N for next, Ctrl-P for previous.", .{query});
                    return;
                }
            }
        }

        // No match found
        self.setStatusMessage("No match found for '{s}'", .{query});

        // Reset last match position
        self.last_match_row = -1;
        self.last_match_col = -1;
    }

    /// Find the previous occurrence of the search query
    pub fn searchPrevious(self: *State) void {
        const query = self.search_query orelse return;

        self.search_direction = .backward;

        // Store current state to restore if no match is found
        const original_cy = self.cy;
        const original_cx = self.cx;

        // Start searching from one character before current position
        var start_row = if (self.last_match_row == -1) self.cy else self.last_match_row;
        var start_col = if (self.last_match_col == -1 or self.last_match_col == 0) blk: {
            if (start_row > 0) {
                break :blk self.rows.?[@intCast(start_row - 1)].size;
            } else {
                break :blk 0;
            }
        } else self.last_match_col - 1;

        // If we're at the beginning of the file, wrap to the end
        if (start_row == 0 and start_col == 0) {
            start_row = @intCast(self.num_rows - 1);
            start_col = self.rows.?[@intCast(start_row)].size;
        }

        // Loop through rows in reverse
        var i: usize = 0;
        while (i < self.num_rows) : (i += 1) {
            const current_row_signed = start_row - @as(i32, @intCast(i));
            const current_row_unsigned = if (current_row_signed < 0)
                @as(usize, @intCast(current_row_signed + @as(i32, @intCast(self.num_rows))))
            else
                @as(usize, @intCast(current_row_signed));
            const current_row = current_row_unsigned;
            const row = &self.rows.?[current_row];

            // For the first row, start from the specified column
            const search_limit = if (i == 0) @as(usize, @intCast(start_col)) else @as(usize, @intCast(row.size));

            // Search backward in this row
            var match_col: i32 = -1;
            if (search_limit >= query.len and row.chars != null) {
                var j: usize = 0;
                while (j <= search_limit - query.len) : (j += 1) {
                    if (std.mem.startsWith(u8, row.chars.?[j..], query)) {
                        match_col = @intCast(j);
                    }
                }
            }

            if (match_col != -1) {
                // Found a match!
                self.cy = @intCast(current_row);
                self.cx = match_col;

                // Save the match position
                self.last_match_row = @intCast(current_row);
                self.last_match_col = match_col;

                // Ensure the match is visible on screen
                self.row_offset = self.cy - @divTrunc(self.screen_rows, 2);
                if (self.row_offset < 0) self.row_offset = 0;

                self.setStatusMessage("Match found ('{s}'). Ctrl-N for next, Ctrl-P for previous.", .{query});
                return;
            }
        }

        // No match found, restore original position
        self.cy = original_cy;
        self.cx = original_cx;

        self.setStatusMessage("No match found for '{s}'", .{query});

        // Reset last match position
        self.last_match_row = -1;
        self.last_match_col = -1;
    }

    /// Exit search mode
    pub fn searchExit(self: *State) void {
        self.is_searching = false;
        self.setStatusMessage("Search exited", .{});
    }
};
