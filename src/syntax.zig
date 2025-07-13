//! syntax.zig - Syntax highlighting implementation
const std = @import("std");
const pleditor = @import("pleditor.zig");

/// Highlight types
pub const Highlight = enum(u8) {
    normal = 0,
    comment,
    multiline_comment,
    keyword1,
    keyword2,
    string,
    number,
    punctuation,
    func_class_name,
};

/// Data structure for highlighting in a row
pub const HighlightRow = struct {
    hl: ?[]u8, // Highlighting for each character
    hl_multiline_comment: bool, // Is this row part of a multi-line comment

    pub fn init(allocator: std.mem.Allocator, size: usize) !HighlightRow {
        const hl = try allocator.alloc(u8, size);
        @memset(hl, @intFromEnum(Highlight.normal));
        return HighlightRow{
            .hl = hl,
            .hl_multiline_comment = false,
        };
    }

    pub fn deinit(self: *HighlightRow, allocator: std.mem.Allocator) void {
        if (self.hl) |hl| {
            allocator.free(hl);
            self.hl = null;
        }
    }
};

/// Syntax definition structure
pub const Syntax = struct {
    filetype: []const u8, // Language/filetype name
    filematch: []const []const u8, // File patterns that match this syntax
    keywords: []const []const u8, // Keywords for the language
    singleline_comment_start: ?[]const u8, // Single line comment start
    multiline_comment_start: ?[]const u8, // Multi-line comment start
    multiline_comment_end: ?[]const u8, // Multi-line comment end
    flags: bool, // Syntax flags
};

// C-like language keywords
const C_HL_KEYWORDS = [_][]const u8{
    // C keywords
    "switch",   "if",       "while",     "for",       "break",     "continue",  "return",    "else",
    "struct",   "union",    "typedef",   "static",    "enum",      "case",      "#include",  "#define",
    "#ifdef",   "#ifndef",  "#endif",    "#pragma",   "volatile",  "register",  "sizeof",    "const",
    "auto",     "do",       "goto",      "default",   "extern",    "inline",    "restrict",

    // C++ keywords
     "namespace",
    "public",   "private",  "protected", "virtual",   "friend",    "new",       "delete",    "try",
    "catch",    "throw",    "this",      "constexpr", "final",     "override",  "explicit",  "using",

    // Types - keyword2
    "int|",     "long|",    "double|",   "float|",    "char|",     "unsigned|", "signed|",   "void|",
    "bool|",    "short|",   "size_t|",   "uint8_t|",  "uint16_t|", "uint32_t|", "uint64_t|", "int8_t|",
    "int16_t|", "int32_t|", "int64_t|",  "FILE|",     "time_t|",   "class|",    "template|",

    // Built-in values
    "true|",
    "false|",   "NULL|",    "nullptr|",
};

// Lua language keywords
const LUA_HL_KEYWORDS = [_][]const u8{
    // Lua keywords
    "function", "local",      "if",        "then",            "else",    "elseif",  "end",       "while",
    "do",       "for",        "repeat",    "until",           "break",   "return",  "in",        "and",
    "or",       "not",

    // Lua built-in values
           "true|",     "false|",          "nil|",

    // Lua built-in functions
       "print|",  "pairs|",    "ipairs|",
    "type|",    "tonumber|",  "tostring|", "require|",        "table|",  "string|", "math|",     "os|",
    "io|",      "coroutine|", "error|",    "assert|",         "pcall|",  "xpcall|", "select|",   "rawget|",
    "rawset|",  "rawequal|",  "rawlen|",   "collectgarbage|", "dofile|", "load|",   "loadfile|", "next|",
};

// Python language keywords
const PYTHON_HL_KEYWORDS = [_][]const u8{
    // Python keywords
    "def",         "class",     "if",        "elif",          "else",         "while",    "for",        "in",        "try",
    "except",      "finally",   "with",      "as",            "import",       "from",     "pass",       "return",    "break",
    "continue",    "lambda",    "yield",     "global",        "nonlocal",     "assert",   "raise",      "del",       "not",
    "and",         "or",        "is",        "async",         "await",        "match",    "case",

    // Python built-in values
          "True|",     "False|",
    "None|",

    // Python special identifiers
          "self|",     "super|",    "cls|",

    // Python built-in types
             "int|",         "str|",     "float|",     "list|",     "dict|",
    "tuple|",      "set|",      "bool|",     "bytes|",        "bytearray|",   "complex|", "frozenset|", "object|",   "type|",

    // Python built-in functions
    "print|",      "len|",      "range|",    "enumerate|",    "sorted|",      "sum|",     "min|",       "max|",      "abs|",
    "open|",       "id|",       "input|",    "format|",       "zip|",         "map|",     "filter|",    "any|",      "all|",
    "dir|",        "vars|",     "locals|",   "globals|",      "hasattr|",     "getattr|", "setattr|",   "delattr|",  "isinstance|",
    "issubclass|", "callable|", "property|", "staticmethod|", "classmethod|", "iter|",    "next|",      "reversed|", "exec|",
    "eval|",       "repr|",     "round|",    "pow|",
};

// Riddle language keywords
const RIDDLE_HL_KEYWORDS = [_][]const u8{
    // Riddle keywords
    "var",    "val",       "for",     "while",   "continue", "break",  "if",       "else",    "fun",
    "return", "import",    "package", "class",   "try",      "catch",  "override", "static",  "const",
    "public", "protected", "private", "virtual", "operator",

    // Types - keyword2
    "int|",   "long|",    "double|", "float|",
    "char|",  "void|",     "bool|",   "short|",

    // Riddle built-in values
     "true|",    "false|", "null|",
};

// Stamon language keywords
const STAMON_HL_KEYWORDS = [_][]const u8{
    // Stamon keywords
    "class", "def",      "extends", "func",
    "break", "continue", "if",      "else",
    "while", "for",      "in",      "return",
    "sfn",   "new",      "null",    "import",
    "true",  "false",
};

// Syntax definitions
const HLDB = [_]Syntax{
    // C-like language
    Syntax{
        .filetype = "c",
        .filematch = &[_][]const u8{ "c", "h", "cpp", "hpp", "cc", "cxx", "c++" },
        .keywords = &C_HL_KEYWORDS,
        .singleline_comment_start = "//",
        .multiline_comment_start = "/*",
        .multiline_comment_end = "*/",
        .flags = false,
    },
    // Lua language
    Syntax{
        .filetype = "lua",
        .filematch = &[_][]const u8{"lua"},
        .keywords = &LUA_HL_KEYWORDS,
        .singleline_comment_start = "--",
        .multiline_comment_start = "--[[",
        .multiline_comment_end = "]]",
        .flags = false,
    },
    // Python language
    Syntax{
        .filetype = "python",
        .filematch = &[_][]const u8{ "py", "pyw" },
        .keywords = &PYTHON_HL_KEYWORDS,
        .singleline_comment_start = "#",
        .multiline_comment_start = "\"\"\"",
        .multiline_comment_end = "\"\"\"",
        .flags = false,
    },
    // Riddle language
    Syntax{
        .filetype = "riddle",
        .filematch = &[_][]const u8{"rid"},
        .keywords = &RIDDLE_HL_KEYWORDS,
        .singleline_comment_start = "//",
        .multiline_comment_start = "/*",
        .multiline_comment_end = "*/",
        .flags = false,
    },
    // Stamon language
    Syntax{
        .filetype = "stamon",
        .filematch = &[_][]const u8{ "st", "stm" },
        .keywords = &STAMON_HL_KEYWORDS,
        .singleline_comment_start = "//",
        .multiline_comment_start = "/*",
        .multiline_comment_end = "*/",
        .flags = false,
    },
};

/// Is the character a separator
fn isSeparator(c: u8) bool {
    return c == 0 or std.ascii.isWhitespace(c) or
        std.mem.indexOfScalar(u8, ",.()+-/*=~%<>[];\\{}:\"'", c) != null;
}

/// Is the character a punctuation mark to highlight
fn isPunctuation(c: u8) bool {
    return std.mem.indexOfScalar(u8, ",.():;{}[]<>=%+-*/&|^~!", c) != null;
}

/// Is the character valid in an identifier
fn isIdentifierChar(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_';
}

/// Highlight function or class name in definitions or calls
fn highlightFunctionClass(state: *pleditor.State, row: *pleditor.Row, i: *usize) void {
    const line = row.render.?;
    const line_len = row.render_size;

    // Skip if position is out of bounds
    if (i.* >= line_len) return;

    // Check for function/class definition keywords
    var is_def = false;
    var kw_len: usize = 0;
    var j: usize = undefined;
    var idx: usize = undefined;

    // Look ahead for patterns based on language
    if (state.syntax) |syntax| {
        if (std.mem.eql(u8, syntax.filetype, "python")) {
            // Python: Check for 'def ' or 'class '
            if (i.* > 0 and isSeparator(line[i.* - 1])) {
                if (i.* + 3 < line_len and std.mem.startsWith(u8, line[i.*..], "def ")) {
                    is_def = true;
                    kw_len = 4;
                } else if (i.* + 5 < line_len and std.mem.startsWith(u8, line[i.*..], "class ")) {
                    is_def = true;
                    kw_len = 6;
                }
            }
        } else if (std.mem.eql(u8, syntax.filetype, "lua")) {
            // Lua: Check for 'function '
            if (i.* > 0 and isSeparator(line[i.* - 1])) {
                if (i.* + 8 < line_len and std.mem.startsWith(u8, line[i.*..], "function ")) {
                    is_def = true;
                    kw_len = 9;
                }
            }
        } else if (std.mem.eql(u8, syntax.filetype, "c")) {
            // Class declaration: "class Name"
            if (i.* > 0 and isSeparator(line[i.* - 1])) {
                if (i.* + 5 < line_len and std.mem.startsWith(u8, line[i.*..], "class ")) {
                    is_def = true;
                    kw_len = 6;
                } else if (i.* + 6 < line_len and std.mem.startsWith(u8, line[i.*..], "struct ")) {
                    is_def = true;
                    kw_len = 7;
                }
            }

            // C function definition check
            if (!is_def and i.* > 0) {
                // Check for function pattern: Look for spaces, then identifier, then (
                j = i.*;
                while (j < line_len and isIdentifierChar(line[j])) : (j += 1) {}

                // If it's followed by a ( after possible whitespace, it might be a function
                idx = j;
                while (idx < line_len and std.ascii.isWhitespace(line[idx])) : (idx += 1) {}

                if (idx < line_len and line[idx] == '(') {
                    // Likely a function, check if declaration or call
                    var has_body = false;

                    // Skip to the end of the parameter list
                    var paren_level: i32 = 1;
                    idx += 1;
                    while (idx < line_len and paren_level > 0) : (idx += 1) {
                        if (line[idx] == '(') paren_level += 1 else if (line[idx] == ')') paren_level -= 1;
                    }

                    // Check for { after the closing ) - indicates a function definition
                    while (idx < line_len and std.ascii.isWhitespace(line[idx])) : (idx += 1) {}
                    if (idx < line_len and line[idx] == '{') has_body = true;

                    // If it's a definition or we're not sure, highlight it
                    if (has_body or (j > i.*)) {
                        if (row.hl) |hl| {
                            idx = i.*;
                            while (idx < j) : (idx += 1) {
                                hl.hl.?[idx] = @intFromEnum(Highlight.func_class_name);
                            }
                        }
                        i.* = j - 1; // position just before the end
                        return;
                    }
                }
            }
        }
    }

    if (is_def) {
        // Skip the keyword and spaces
        i.* += kw_len;
        while (i.* < line_len and std.ascii.isWhitespace(line[i.*])) : (i.* += 1) {}

        // Highlight the function/class name
        const name_start = i.*;
        while (i.* < line_len and isIdentifierChar(line[i.*])) : (i.* += 1) {}

        if (i.* > name_start) {
            if (row.hl) |hl| {
                j = name_start;
                while (j < i.*) : (j += 1) {
                    hl.hl.?[j] = @intFromEnum(Highlight.func_class_name);
                }
            }
            // Don't increment i again since the loop will do it
            i.* -= 1;
        }
    } else {
        // Check for function calls: name(...
        const name_start = i.*;
        while (i.* < line_len and isIdentifierChar(line[i.*])) : (i.* += 1) {}

        // If we have an identifier followed by a parenthesis, it's likely a function call
        if (i.* > name_start) {
            idx = i.*;
            while (idx < line_len and std.ascii.isWhitespace(line[idx])) : (idx += 1) {}

            if (idx < line_len and line[idx] == '(') {
                if (row.hl) |hl| {
                    j = name_start;
                    while (j < i.*) : (j += 1) {
                        hl.hl.?[j] = @intFromEnum(Highlight.func_class_name);
                    }
                }
            }
        }
        // Don't increment i again since the loop will do it
        i.* -= 1;
    }
}

/// Handle punctuation highlighting
fn highlightPunctuation(row: *pleditor.Row, i: usize) void {
    const line = row.render.?;

    // Check for single character punctuation
    if (i < row.render_size and isPunctuation(line[i])) {
        if (row.hl) |hl| {
            hl.hl.?[i] = @intFromEnum(Highlight.punctuation);

            // Check for compound operators
            if (i + 1 < row.render_size) {
                const c1 = line[i];
                const c2 = line[i + 1];

                // Check for common compound operators where second char is '='
                if (c2 == '=' and std.mem.indexOfScalar(u8, "+-*/=!&|^<>%", c1) != null) {
                    hl.hl.?[i + 1] = @intFromEnum(Highlight.punctuation);
                }
                // Check for increment/decrement operators
                else if ((c1 == '+' and c2 == '+') or (c1 == '-' and c2 == '-')) {
                    hl.hl.?[i + 1] = @intFromEnum(Highlight.punctuation);
                }
                // Check for shift operators
                else if ((c1 == '<' and c2 == '<') or (c1 == '>' and c2 == '>')) {
                    hl.hl.?[i + 1] = @intFromEnum(Highlight.punctuation);
                }
                // Check for logical operators
                else if ((c1 == '&' and c2 == '&') or (c1 == '|' and c2 == '|')) {
                    hl.hl.?[i + 1] = @intFromEnum(Highlight.punctuation);
                }
                // Check for structure dereference operator ->
                else if (c1 == '-' and c2 == '>') {
                    hl.hl.?[i + 1] = @intFromEnum(Highlight.punctuation);
                }
            }
        }
    }
}

/// Handle hex, octal, or binary number formats
fn highlightBasedNumber(row: *pleditor.Row, i: *usize) bool {
    const line = row.render.?;

    if (i.* + 2 >= row.render_size or line[i.*] != '0')
        return false;

    const next = line[i.* + 1];
    if (std.mem.indexOfScalar(u8, "xXoObB", next) == null)
        return false;

    // Highlight the prefix (0x, 0o, 0b)
    if (row.hl) |hl| {
        hl.hl.?[i.*] = @intFromEnum(Highlight.number);
        hl.hl.?[i.* + 1] = @intFromEnum(Highlight.number);
    }
    i.* += 2;

    // Continue highlighting based on the number format
    while (i.* < row.render_size) {
        const c = line[i.*];
        var valid = false;

        if (next == 'x' or next == 'X') {
            valid = std.ascii.isHex(c);
        } else if (next == 'o' or next == 'O') {
            valid = c >= '0' and c <= '7';
        } else {
            valid = c == '0' or c == '1';
        }

        if (!valid) break;

        if (row.hl) |hl| {
            hl.hl.?[i.*] = @intFromEnum(Highlight.number);
        }
        i.* += 1;
    }

    return true;
}

/// Initialize syntax highlighting system
pub fn syntaxInit(state: *pleditor.State) bool {
    state.syntax = null;

    // Select syntax by filename if there is one
    if (state.filename) |filename| {
        syntaxByFileExt(state, filename);

        // Apply syntax highlighting to all rows
        syntaxUpdateAll(state);
    }

    return true;
}

/// Apply syntax highlighting to all rows in the file
pub fn syntaxUpdateAll(state: *pleditor.State) void {
    if (state.syntax == null) return;

    var i: usize = 0;
    while (i < state.num_rows) : (i += 1) {
        syntaxUpdateRow(state, i);
    }
}

/// Map highlight values to ansi escape code
pub fn syntaxColorToAnsi(hl: u8) u8 {
    return switch (@as(Highlight, @enumFromInt(hl))) {
        .comment, .multiline_comment => 90, // Gray
        .keyword1 => 34, // Blue
        .keyword2 => 32, // Green
        .string => 35, // Magenta
        .number => 31, // Red
        .punctuation => 33, // Yellow
        .func_class_name => 36, // Cyan
        else => 37, // White (default)
    };
}

/// Select syntax highlighting based on file extension
pub fn syntaxByFileExt(state: *pleditor.State, filename: []const u8) void {
    state.syntax = null;

    // Get file extension
    if (std.mem.lastIndexOfScalar(u8, filename, '.')) |dot_pos| {
        const ext = filename[dot_pos + 1 ..];

        // Try to match file extension with a syntax
        for (&HLDB) |*syntax| {
            for (syntax.filematch) |pattern| {
                if (std.mem.eql(u8, pattern, ext)) {
                    state.syntax = syntax;
                    return;
                }
            }
        }
    }
}

/// Update highlighting for a row
pub fn syntaxUpdateRow(state: *pleditor.State, row_idx: usize) void {
    const row = &state.rows.?[row_idx];

    // Free existing highlighting memory
    if (row.hl) |*hl| {
        hl.deinit(state.allocator);
        row.hl = null;
    }

    // Allocate new highlight struct
    row.hl = HighlightRow.init(state.allocator, @as(usize, @intCast(row.render_size))) catch return;

    // If no syntax, leave everything as normal
    const syntax = state.syntax orelse return;

    const keywords = syntax.keywords;
    const scs = syntax.singleline_comment_start;
    const mcs = syntax.multiline_comment_start;
    const mce = syntax.multiline_comment_end;

    var prev_sep = true;
    var in_string: u8 = 0;
    var in_comment = if (row_idx > 0 and state.rows.?[row_idx - 1].hl != null)
        state.rows.?[row_idx - 1].hl.?.hl_multiline_comment
    else
        false;

    const line = row.render.?;

    // Check for preprocessor directives in C/C++ at the beginning of the line
    if (std.mem.eql(u8, syntax.filetype, "c")) {
        if (row.render_size > 0 and line[0] == '#') {
            // Highlight the # character
            if (row.hl) |*hl| {
                hl.hl.?[0] = @intFromEnum(Highlight.keyword1);

                // Find the directive word (e.g., define, ifndef)
                var j: usize = 1;
                while (j < row.render_size and std.ascii.isWhitespace(line[j])) : (j += 1) {}

                const directive_start = j;
                while (j < row.render_size and std.ascii.isAlphabetic(line[j])) : (j += 1) {}

                // Highlight the directive
                var k = directive_start;
                while (k < j) : (k += 1) {
                    hl.hl.?[k] = @intFromEnum(Highlight.keyword1);
                }

                // Highlight what follows the directive for specific cases
                if (directive_start < j) {
                    const len = j - directive_start;
                    const directive = line[directive_start..j];

                    if ((len == 6 and std.mem.eql(u8, directive, "define")) or
                        (len == 6 and std.mem.eql(u8, directive, "ifndef")) or
                        (len == 5 and std.mem.eql(u8, directive, "ifdef")) or
                        (len == 7 and std.mem.eql(u8, directive, "include")) or
                        (len == 5 and std.mem.eql(u8, directive, "endif")) or
                        (len == 5 and std.mem.eql(u8, directive, "undef")) or
                        (len == 6 and std.mem.eql(u8, directive, "pragma")))
                    {

                        // Skip whitespace after directive
                        while (j < row.render_size and std.ascii.isWhitespace(line[j])) : (j += 1) {}

                        // Highlight the identifier
                        const ident_start = j;

                        // For #include, handle both <...> and "..." forms
                        if (len == 7 and j < row.render_size and (line[j] == '<' or line[j] == '"')) {
                            const end_char: u8 = if (line[j] == '<') '>' else '"';
                            hl.hl.?[j] = @intFromEnum(Highlight.keyword2); // Highlight the opening < or "
                            j += 1;

                            // Find the closing character
                            while (j < row.render_size and line[j] != end_char) : (j += 1) {
                                hl.hl.?[j] = @intFromEnum(Highlight.keyword2);
                            }
                            if (j < row.render_size) {
                                hl.hl.?[j] = @intFromEnum(Highlight.keyword2); // Highlight the closing > or "
                            }
                        } else {
                            // For other directives, highlight the identifier
                            while (j < row.render_size and (isIdentifierChar(line[j]) or line[j] == '.')) : (j += 1) {}

                            k = ident_start;
                            while (k < j) : (k += 1) {
                                hl.hl.?[k] = @intFromEnum(Highlight.keyword2);
                            }
                        }
                    }
                }
            }
        }
    }

    var i: usize = 0;
    while (i < row.render_size) : (i += 1) {
        const c = line[i];
        const prev_hl: u8 = if (i > 0 and row.hl != null) row.hl.?.hl.?[i - 1] else @intFromEnum(Highlight.normal);

        // String handling
        if (in_string != 0) {
            if (row.hl) |*hl| {
                hl.hl.?[i] = @intFromEnum(Highlight.string);
            }
            if (c == '\\' and i + 1 < row.render_size) {
                if (row.hl) |*hl| {
                    hl.hl.?[i + 1] = @intFromEnum(Highlight.string);
                }
                i += 1;
                continue;
            }
            if (c == in_string) in_string = 0;
            prev_sep = true;
            continue;
        }

        // Comment handling
        if (in_comment) {
            if (row.hl) |*hl| {
                hl.hl.?[i] = @intFromEnum(Highlight.multiline_comment);
            }
            if (mce) |end_seq| {
                if (std.mem.startsWith(u8, line[i..], end_seq)) {
                    var j: usize = 0;
                    while (j < end_seq.len) : (j += 1) {
                        if (row.hl) |*hl| {
                            hl.hl.?[i + j] = @intFromEnum(Highlight.multiline_comment);
                        }
                    }
                    i += end_seq.len - 1;
                    in_comment = false;
                    prev_sep = true;
                    continue;
                }
            }
            continue;
        }

        // Start of multi-line comment
        if (mcs) |start_seq| {
            if (std.mem.startsWith(u8, line[i..], start_seq)) {
                var j: usize = 0;
                while (j < start_seq.len) : (j += 1) {
                    if (row.hl) |*hl| {
                        hl.hl.?[i + j] = @intFromEnum(Highlight.multiline_comment);
                    }
                }
                i += start_seq.len - 1;
                in_comment = true;
                continue;
            }
        }

        // Start of single-line comment
        if (scs) |start_seq| {
            if (std.mem.startsWith(u8, line[i..], start_seq)) {
                var j = i;
                while (j < row.render_size) : (j += 1) {
                    if (row.hl) |*hl| {
                        hl.hl.?[j] = @intFromEnum(Highlight.comment);
                    }
                }
                break;
            }
        }

        // String start or include brackets <>
        if (c == '"' or c == '\'' or (c == '<' and prev_sep and std.mem.indexOf(u8, line, "#include") != null)) {
            // Set appropriate closing character
            const closing: u8 = if (c == '<') '>' else c;
            in_string = closing;
            if (row.hl) |*hl| {
                hl.hl.?[i] = @intFromEnum(Highlight.string);
            }
            continue;
        }

        // Number handling
        if (std.ascii.isDigit(c)) {
            // Check for special number formats (hex, octal, binary)
            if (highlightBasedNumber(row, &i)) {
                prev_sep = false;
                continue;
            }

            // Regular decimal number
            if (prev_sep or prev_hl == @intFromEnum(Highlight.number)) {
                if (row.hl) |*hl| {
                    hl.hl.?[i] = @intFromEnum(Highlight.number);
                }
                prev_sep = false;
                continue;
            }
        } else if (c == '.' and prev_hl == @intFromEnum(Highlight.number)) {
            // Decimal point in a number
            if (row.hl) |*hl| {
                hl.hl.?[i] = @intFromEnum(Highlight.number);
            }
            prev_sep = false;
            continue;
        }

        // Handle special case for colon in array slices and Python statements
        if (c == ':') {
            if (row.hl) |*hl| {
                hl.hl.?[i] = @intFromEnum(Highlight.punctuation);
            }
            prev_sep = true; // Treat colon as separator
            continue;
        }

        // Keyword handling
        if (prev_sep) {
            var found_keyword = false;
            for (keywords) |keyword| {
                var klen = keyword.len;
                const is_kw2 = keyword[klen - 1] == '|';

                if (is_kw2) klen -= 1;

                // Special handling for Python identifiers that need context checks
                var is_valid_match = std.mem.startsWith(u8, line[i..], keyword[0..klen]) and
                    (i + klen >= row.render_size or isSeparator(line[i + klen]));

                if (is_kw2) {
                    var back = if (i > 0) i - 1 else 0;
                    const search_limit = if (i >= 20) i - 20 else 0;
                    while (back >= search_limit) {
                        if (line[back] == '(' or line[back] == ',') {
                            // Found valid context
                            break;
                        }
                        if (!std.ascii.isWhitespace(line[back])) {
                            // Found non-whitespace that's not a separator we expect
                            if (i > 0 and back == i - 1) {
                                is_valid_match = false; // directly adjacent
                            }
                            break;
                        }
                        if (back == 0) break;
                        back -= 1;
                    }
                }

                if (is_valid_match) {
                    // Apply keyword highlighting
                    if (row.hl) |*hl| {
                        var k: usize = 0;
                        while (k < klen) : (k += 1) {
                            hl.hl.?[i + k] = if (is_kw2) @intFromEnum(Highlight.keyword2) else @intFromEnum(Highlight.keyword1);
                        }
                    }
                    i += klen - 1;
                    found_keyword = true;
                    break;
                }
            }

            if (found_keyword) {
                prev_sep = false;
                continue;
            }
        }

        // Highlight punctuation
        highlightPunctuation(row, i);

        // Check for function or class names
        if ((std.ascii.isAlphabetic(c) or c == '_') and prev_sep) {
            highlightFunctionClass(state, row, &i);
        }

        // Special handling for Python indentation
        if (std.mem.eql(u8, syntax.filetype, "python")) {
            // Mark beginning of line whitespace as special in Python
            if (i == 0 and std.ascii.isWhitespace(c)) {
                var indent_end: usize = 0;
                while (indent_end < row.render_size and std.ascii.isWhitespace(line[indent_end])) : (indent_end += 1) {}
                if (indent_end > 0) {
                    if (row.hl) |*hl| {
                        var k: usize = 0;
                        while (k < indent_end) : (k += 1) {
                            hl.hl.?[k] = @intFromEnum(Highlight.normal);
                        }
                    }
                }
            }
        }

        prev_sep = isSeparator(c);
    }

    // Update multiline comment status for this row
    if (row.hl) |*hl| {
        hl.hl_multiline_comment = in_comment;
    }
}

/// Update syntax highlighting for rows affected by multi-line comments
pub fn syntaxUpdateMultiline(state: *pleditor.State, start_row: usize) void {
    if (state.syntax == null) return;

    // Update the starting row and all subsequent rows that might be affected
    var i = start_row;
    while (i < state.num_rows) : (i += 1) {
        syntaxUpdateRow(state, i);

        // Stop updating when we reach a row not affected by multi-line comments
        if (i > start_row and (state.rows.?[i].hl == null or !state.rows.?[i].hl.?.hl_multiline_comment)) {
            break;
        }
    }
}
