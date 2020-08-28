//! A tokenizer and parser for the language Earl.  The syntax of Earl resembles
//! the syntax of Lisp or S-expressions.  The main difference is that Earl does
//! not define ordered pairs, i.e. `(x . y)`, but only lists, e.g. the empty
//! list `(x y z)`.  The design of multiline strings and nesting multiline
//! comments is possibly unorthodox.

use std::error::Error;
use std::fmt;

#[derive(Debug)]
/// Any kind of error that can pop up while tokenizing and parsing.
pub enum SyntaxError {
    /// Error while tokenizing or parsing.
    ParsingError(ParsingError),
    /// Error due to faulty assumptions in the code.
    AssumptionError(String),
}

impl From<ParsingError> for SyntaxError {
    fn from(error: ParsingError) -> Self {
        SyntaxError::ParsingError(error)
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::error::Error for SyntaxError {}

/// Error codes we might receive when tokenizing and parsing.
// TODO: change in the future.
#[derive(PartialEq, Debug)]
pub enum ErrorCode {
    UnterminatedNormalString,
    UnterminatedExtendedString,
    UnterminatedMultiLineComment,
    UnexpectedClosingDelimiter,
    DelimiterMismatch,
    PotentialErroneousClosingDelimiterFollowedByComment,
    UnclosedDelimiter,
    BadAssumption,
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

/// Error information returned to the user if there's a problem tokenizing or
/// parsing the input.
// NOTE: maybe find a better name?  This is both for tokenizing and parsing.
#[derive(Debug)]
pub struct ParsingError {
    pub error_code: ErrorCode,
    pub error_message: String,
}

impl fmt::Display for ParsingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}): {}", self.error_code, self.error_message)
    }
}

impl Error for ParsingError {}

impl ParsingError {
    /// Creates a new parsing error.
    // TODO: maybe need differnt ones for parsing since we have access to different kind of
    // information.
    // TODO: in future add hints or support for hints.
    #[inline]
    pub fn new(
        code: &[u8],
        start_index: usize,
        end_index: usize,
        error_code: ErrorCode,
        error_message: &str,
    ) -> ParsingError {
        // &[u8] -> ... -> String
        // TODO: in future remove these attributes
        #![allow(unused_variables, unused_assignments)]

        // TODO: add better error messages in the future

        let code = String::from_utf8_lossy(code);

        let error_message = String::from(error_message);

        let si = start_index;

        let mut sl = 0;

        let mut sc = 0;

        let ei = end_index;
        let mut el = 0;
        let mut ec = 0;

        // Find line and column for start and end index

        {
            let mut code_index = 0;
            let mut line_index = 0;
            let mut column_index = 0;

            let mut count = 2;

            for c in code.chars() {
                if count == 0 {
                    break;
                }

                if si == code_index {
                    // TODO: in future remove this attribute

                    sl = line_index;
                    sc = column_index;

                    count -= 1;
                }

                if ei == code_index {
                    el = line_index;
                    ec = column_index;

                    count -= 1;
                }

                // End

                code_index += 1;

                if c == '\n' {
                    line_index += 1;
                    column_index = 0;
                } else {
                    column_index += 1;
                }
            }
        }

        let lines: Vec<&str> = code.split("\n").collect();

        let mut msg: Vec<String> = Vec::new();

        let end_line_number_string = format!("{}", el + 1);

        let mut gutter_size = 0;
        gutter_size = std::cmp::max(gutter_size, end_line_number_string.len());

        let e_line = lines.get(el).unwrap();

        msg.push(String::from(""));
        msg.push(format!(" {} | {}", end_line_number_string, e_line));
        msg.push(format!(
            " {} | {}^",
            " ".repeat(gutter_size),
            " ".repeat(ec)
        ));

        msg.push(String::from(""));
        msg.push(format!("{}", error_message));

        let error_message = msg.join("\n");

        ParsingError {
            error_code,
            error_message,
        }
    }
}

// TODO: write a function that creates an error message based off of the error code and etc.

/// Token IDs or token types.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenId {
    Whitespace = 2,

    Word = 7,

    RoundBracketOpen = 10,
    RoundBracketClose = 11,
    CurlyBracketOpen = 12,
    CurlyBracketClose = 14,
    SquareBracketOpen = 15,
    SquareBracketClose = 16,

    StringNormal = 21,
    StringExtended = 22,

    CommentSingleLine = 31,

    CommentMultiLine = 32,
}

/// When tokenizing the input we do it in different states and transition between states
/// based on what we see in the input.
#[derive(Debug, PartialEq)]
enum State {
    Undetermined,
    Whitespace,

    CommentAmbiguous,
    CommentSingleLine,
    CommentMultiLine,

    StringAmbiguous,
    StringNormal,
    StringExtendedConfig, // config section
    StringExtendedBody,   // body section
    StringExtendedEnd,    // triggered by . or ,

    Word,
}

/// A token in Earl contains where it appears in the code and its type or ID.
///
/// * The field `id` is what type of token this is.
/// * The field `index` is the index of the byte where this token begins.
/// * The field `length` is how many bytes this token spans.
#[derive(Clone, Debug)]
pub struct Token {
    /// The ID or type of token.
    pub id: TokenId,
    /// The index of where the token appears in the code.
    pub index: usize,
    /// The length of the token in the code.
    pub length: usize,
}

impl Token {
    /// Creates a new token.
    #[inline]
    pub fn new(id: TokenId, index: usize, length: usize) -> Token {
        Token { id, index, length }
    }

    /// Returns the lexeme of this code.
    #[inline]
    pub fn get_lexeme<'a>(self: &Self, code: &'a str) -> &'a str {
        let i = self.index;
        let j = self.index + self.length;

        let x = &code[i..j];

        x
    }
}

// Toggle this on and off to debug the tokenizer.
//
// TODO: figure out a way to use conditional compilation instead of this.
const DEBUG: bool = false;

// Temporary const to remind me that I'm using a hack to circumvent problems that could occur with
// issue 132.
//
// TODO: once issue 132 has been resolved remove this const.
#[allow(dead_code)]
const HAS_ISSUE_132: bool = true;

/// Returns a tokenization result.
///
/// The input `code` is a byte slice.  It's expected its a byte slice of a UTF-8 code.
pub fn tokenize(
    code: &str,
    include_comments: bool,
    include_whitespace: bool,
) -> Result<Vec<Token>, SyntaxError> {
    let code = code.as_bytes();

    if DEBUG {
        println!("{}", "-".repeat(80));

        let code = std::str::from_utf8(code).unwrap();
        println!("Code: [{}]", code);
    }

    let mut tokens: Vec<Token> = Vec::new();

    let mut token_id = TokenId::Whitespace;
    let mut token_index = 0;
    let mut token_length = 0;

    let mut state: State = State::Undetermined;

    let mut attempting_to_close_comment = false;
    let mut attempting_to_open_comment = false;

    let mut start_semicolon_count = 0;
    let mut end_semicolon_count = 0;

    // We use this count to determine whether we're a normal string or an
    // extended string.
    //
    // We also use this number for extended strings to keep track of how
    // many consecutive double quotation marks we must match before the string
    // ends.
    let mut start_double_quote_count = 0;
    let mut end_double_quote_count = 0;

    let mut multi_line_comment_stack = Vec::new();

    // Previous character
    let mut pc = b'a';

    // NOTE: I don't reset the `token_id` after adding the token to the tokens.

    // We walk over the bytes in the code in the code loop.
    //
    // I need to be able to look arbitrary characters ahead. :'(
    'code_loop: for (code_index, c) in code.iter().enumerate() {
        let c = *c;

        let mut no_progress_iter = 0;

        // pc.len_utf8()

        if DEBUG {
            if code_index > 0 {
                // Resolution state
                println!("> State = {:?} (end)", state);
            }

            println!("{}", "-".repeat(80));
            println!("i = {}, c = [{}]", code_index, c);
        }

        // In the decision loop we decide what state we enter based on the byte we see and then
        // at that state how we process that byte.
        'decision_loop: loop {
            if DEBUG {
                if no_progress_iter == 0 {
                    println!("> State = {:?} (start)", state);
                } else {
                    println!("> State = {:?}", state);
                }
            }

            {
                // NOTE: Safety iteration block.  Remove this block in the future once this code
                // appears to be robust.
                no_progress_iter += 1;

                if no_progress_iter == 3 {
                    return Err(SyntaxError::AssumptionError(
                        "No progress while tokenizing.".to_string(),
                    ));
                }
            }

            match &state {
                State::Undetermined => {
                    // We enter this state to determine what state we want to enter based off of
                    // the character under consideration.

                    if c.is_ascii_whitespace() {
                        pc = c;

                        token_id = TokenId::Whitespace;
                        token_index = code_index;
                        token_length = 1;

                        state = State::Whitespace;

                        continue 'code_loop;
                    } else if c == b'(' {
                        pc = c;

                        token_id = TokenId::RoundBracketOpen;
                        token_index = code_index;
                        token_length = 1;

                        tokens.push(Token::new(token_id, token_index, token_length));

                        continue 'code_loop;
                    } else if c == b')' {
                        pc = c;

                        token_id = TokenId::RoundBracketClose;
                        token_index = code_index;
                        token_length = 1;

                        // NOTE: this is a temporary solution for Issue[132].

                        tokens.push(Token::new(token_id, token_index, token_length));

                        continue 'code_loop;
                    } else if c == b'[' {
                        pc = c;

                        token_id = TokenId::SquareBracketOpen;
                        token_index = code_index;
                        token_length = 1;

                        tokens.push(Token::new(token_id, token_index, token_length));

                        continue 'code_loop;
                    } else if c == b']' {
                        pc = c;

                        token_id = TokenId::SquareBracketClose;
                        token_index = code_index;
                        token_length = 1;

                        tokens.push(Token::new(token_id, token_index, token_length));

                        continue 'code_loop;
                    } else if c == b'{' {
                        pc = c;

                        token_id = TokenId::CurlyBracketOpen;
                        token_index = code_index;
                        token_length = 1;

                        tokens.push(Token::new(token_id, token_index, token_length));

                        continue 'code_loop;
                    } else if c == b'}' {
                        pc = c;

                        token_id = TokenId::CurlyBracketClose;
                        token_index = code_index;
                        token_length = 1;

                        tokens.push(Token::new(token_id, token_index, token_length));

                        continue 'code_loop;
                    } else if c == b'"' {
                        pc = c;

                        start_double_quote_count = 1;

                        token_index = code_index;
                        token_length = 1;

                        state = State::StringAmbiguous;

                        continue 'code_loop;
                    } else if c == b';' {
                        // Issue[132]: temporary strategy
                        if pc == b')' {
                            return Err(SyntaxError::ParsingError(ParsingError::new(
                                code,
                                code_index,
                                code_index,
                                ErrorCode::PotentialErroneousClosingDelimiterFollowedByComment,
                                "Unterminated potential erroneous closing delimiter followed by comment",
                            )));
                        }

                        pc = c;

                        token_index = code_index;
                        token_length = 1;

                        start_semicolon_count = 1;

                        state = State::CommentAmbiguous;

                        continue 'code_loop;
                    } else {
                        pc = c;

                        token_id = TokenId::Word;
                        token_index = code_index;
                        token_length = 1;

                        state = State::Word;

                        continue 'code_loop;
                    }
                }
                State::Whitespace => {
                    if c.is_ascii_whitespace() {
                        token_length += 1;

                        continue 'code_loop;
                    } else {
                        // Token done
                        if include_whitespace {
                            tokens.push(Token::new(token_id, token_index, token_length));
                        }

                        state = State::Undetermined;

                        continue 'decision_loop;
                    }
                }
                State::CommentAmbiguous => {
                    // We arrive at this state for the first time when we encounted ; comming from
                    // State::Undetermined.
                    //
                    // It has yet to be determined whether this state transitions to a single-line
                    // comment state or a multi-line comment state.

                    if c == b';' {
                        // We might be a multi-line comment or a single-line
                        // comment.  We just don't know... yet.
                        token_length += 1;
                        start_semicolon_count += 1;

                        continue 'code_loop;
                    } else if c == b'(' {
                        // Transition to a multi-line comment state.
                        multi_line_comment_stack.push(start_semicolon_count);

                        token_length += 1;

                        token_id = TokenId::CommentMultiLine;
                        state = State::CommentMultiLine;

                        attempting_to_open_comment = false;
                        attempting_to_close_comment = false;

                        continue 'code_loop;
                    } else if c == b'\n' {
                        // Transition to single-line comment state.

                        token_id = TokenId::CommentSingleLine;

                        if include_comments {
                            tokens.push(Token::new(token_id, token_index, token_length));
                        }

                        state = State::Undetermined;

                        continue 'decision_loop;
                    } else {
                        // Transition to single-line comment state.

                        token_length += 1;

                        token_id = TokenId::CommentSingleLine;

                        state = State::CommentSingleLine;

                        continue 'code_loop;
                    }
                }
                State::CommentSingleLine => {
                    // We arrive here the first time from State::CommentAmbiguous.
                    //
                    // The first time we reach this state is when we're in a text like,
                    //
                    //   ;;;..;;?
                    //          ^
                    // where ? is not ; and ? is not (.

                    // #[cfg(debug_block_comment_tokenization)]

                    if c == b'\n' {
                        // We're done
                        if include_comments {
                            tokens.push(Token::new(token_id, token_index, token_length));
                        }

                        state = State::Undetermined;

                        continue 'decision_loop;
                    } else {
                        token_length += 1;

                        continue 'code_loop;
                    }
                }
                State::CommentMultiLine => {
                    // We arrive here the first time from State::CommentAmbiguous.  When we enter
                    // this state for the first time then we're in a text like,
                    //
                    //   ;;;...;;(?
                    //            ^
                    //
                    // where we walked over a sequence of ; and just walked passed (, and now we're
                    // considering the character pointed by ^.

                    // TODO: we actually only need one variable for start_semicolon_count and
                    // end_semicolon_count

                    if DEBUG {
                        let s: Vec<String> = (&multi_line_comment_stack)
                            .into_iter()
                            .map(|n| format!("{}{{", ";".repeat(*n)))
                            .collect();

                        println!("    Stack: {:?}", s);

                        if attempting_to_open_comment {
                            println!("      Opening?: {}", ";".repeat(start_semicolon_count));
                        } else if attempting_to_close_comment {
                            println!("      Closing?: ){}", ";".repeat(end_semicolon_count));
                        }
                    }

                    // - * - * - * - * - * - * - * - * - * - * - * - * - * - * -

                    if attempting_to_open_comment {
                        if c == b';' {
                            // Keep on GOING
                            start_semicolon_count += 1;
                            token_length += 1;

                            continue 'code_loop;
                        } else if c == b'(' {
                            attempting_to_open_comment = false;
                            // Nest some more
                            token_length += 1;

                            multi_line_comment_stack.push(start_semicolon_count);

                            continue 'code_loop;
                        } else {
                            // Cancel, but don't know how to handle symbol
                            attempting_to_open_comment = false;

                            continue 'decision_loop;
                        }
                    } else if attempting_to_close_comment {
                        if c == b';' {
                            end_semicolon_count += 1;

                            token_length += 1;

                            continue 'code_loop;
                        } else {
                            if end_semicolon_count == 0 {
                                // Here we're in this text,
                                //
                                //   ?)?
                                //     ^.__ not ;
                                //
                                //
                                attempting_to_close_comment = false;

                                continue 'decision_loop;
                            } else {
                                // Here we're in this text,
                                //
                                //   ?);..;?
                                //         ^.__ not ;
                                //

                                // Now here we try to match our  );;..;  against something so we're
                                // no longer "attempting to close the comment".
                                attempting_to_close_comment = false;

                                // Try to match against something in the stack

                                let mut pop_off_stack = false;
                                let mut match_index = 0;

                                // Search the stack for a suitable opening mark to match against.
                                'stack_loop: for (i, stack_semicolon_count) in
                                    multi_line_comment_stack.iter().enumerate().rev()
                                {
                                    if end_semicolon_count < *stack_semicolon_count {
                                        // We're in this situtation,
                                        //
                                        //                .-- we're here and ? is not ;
                                        //               v
                                        //  ;;;;( ... );;?
                                        //  aaaaa     bbb
                                        //
                                        // Where aaaaa corresponds to the stack semicolon count in
                                        // the stack, and we can't look past it since it has more
                                        // semicolons.

                                        // Guarded, no we're done, we don't push anything
                                        // on top.
                                        attempting_to_close_comment = false;

                                        if DEBUG {
                                            println!("        Dropped!");
                                        }

                                        continue 'decision_loop;
                                    } else if end_semicolon_count == *stack_semicolon_count {
                                        // We found a match!
                                        //
                                        // We're in this sitation,
                                        //
                                        //  ;;;( ... );;;
                                        //  aaaa     bbbb
                                        //
                                        // where we found an exact match.

                                        pop_off_stack = true;
                                        match_index = i;

                                        if DEBUG {
                                            println!("        Matched!");
                                        }

                                        break 'stack_loop;
                                    }
                                }

                                if pop_off_stack {
                                    // Our stack could like this at this point,
                                    //
                                    // [  ;;;;(  ;;;(  ;(  ;;(  ]
                                    //
                                    // and we have  );;;   which matches this, -.
                                    //                                          |
                                    //              .___________________________'
                                    //              v
                                    // [  ;;;;(  ;;;(  ;(  ;;(  ]
                                    //                  ^    ^
                                    //                  |    |
                                    //          .-------`----'
                                    //          |
                                    // and these need to be popped off the stack.
                                    //
                                    //
                                    //       v            <- pop this and everything to the right
                                    // 0 1 2 3 4 5 6 7    <- idx
                                    // 1 2 3              <- len

                                    while multi_line_comment_stack.len() > match_index {
                                        multi_line_comment_stack.pop();

                                        if DEBUG {
                                            println!("        Pop!");
                                        }
                                    }

                                    // If we empty the stack then that means we've completed this
                                    // token.

                                    if multi_line_comment_stack.len() == 0 {
                                        // We have completed this token.

                                        if include_comments {
                                            tokens.push(Token::new(
                                                token_id,
                                                token_index,
                                                token_length,
                                            ));
                                        }

                                        state = State::Undetermined;

                                        continue 'decision_loop;
                                    }
                                }

                                attempting_to_close_comment = false;

                                continue 'decision_loop;
                            }
                        }
                    } else {
                        if c == b';' {
                            token_length += 1;

                            attempting_to_open_comment = true;
                            start_semicolon_count = 1;

                            continue 'code_loop;
                        } else if c == b')' {
                            token_length += 1;

                            attempting_to_close_comment = true;
                            end_semicolon_count = 0;

                            continue 'code_loop;
                        } else {
                            token_length += 1;

                            continue 'code_loop;
                        }
                    }

                    // - * - * - * - * - * - * - * - * - * - * - * - * - * - * -
                }
                State::StringAmbiguous => {
                    // At this point all we know is that we're a string.  We
                    // don't yet know whether or not we're a "normal" string or
                    // an "extended" string.

                    if c == b'"' {
                        token_length += 1;
                        start_double_quote_count += 1;

                        continue 'code_loop;
                    } else {
                        if start_double_quote_count == 1 {
                            // We're a normal string.
                            token_length += 1;

                            pc = c;

                            token_id = TokenId::StringNormal;

                            state = State::StringNormal;

                            continue 'code_loop;
                        } else if start_double_quote_count == 2 {
                            // This was an empty normal string

                            token_id = TokenId::StringNormal;

                            tokens.push(Token::new(token_id, token_index, token_length));

                            state = State::Undetermined;

                            continue 'decision_loop;
                        } else {
                            // start_double_quote_count >= 3
                            token_id = TokenId::StringExtended;

                            state = State::StringExtendedConfig;

                            // We're an "extended" string

                            continue 'decision_loop;
                        }
                    }
                }
                State::StringNormal => {
                    token_length += 1;

                    if c == b'"' && pc != b'\\' {
                        // We're done
                        tokens.push(Token::new(token_id, token_index, token_length));

                        state = State::Undetermined;
                    } else {
                        pc = c;
                    }

                    continue 'code_loop;
                }
                State::StringExtendedConfig => {
                    token_length += 1;

                    if c == b'.' || c == b',' {
                        // Continue on to body

                        state = State::StringExtendedBody;
                    }

                    continue 'code_loop;
                }
                State::StringExtendedBody => {
                    token_length += 1;

                    if c == b'.' || c == b',' {
                        // Go to end
                        end_double_quote_count = 0;

                        state = State::StringExtendedEnd;
                    }

                    continue 'code_loop;
                }
                State::StringExtendedEnd => {
                    if c == b'"' {
                        token_length += 1;
                        end_double_quote_count += 1;

                        if end_double_quote_count == start_double_quote_count {
                            // We're done !
                            tokens.push(Token::new(token_id, token_index, token_length));

                            state = State::Undetermined
                        }
                    } else if c == b'.' || c == b',' {
                        token_length += 1;
                        end_double_quote_count = 0;
                    } else {
                        token_length += 1;
                        state = State::StringExtendedBody;
                    }

                    continue 'code_loop;
                }
                State::Word => {
                    // NOTE: I don't like this code... at all.
                    //
                    // The problem with doing this is we've done all the
                    // checking here and need to redo it in State::Undetermined.
                    if c.is_ascii_whitespace()
                        || c == b'('
                        || c == b')'
                        || c == b'['
                        || c == b']'
                        || c == b'{'
                        || c == b'}'
                        || c == b'"'
                        || c == b';'
                    {
                        // Done
                        tokens.push(Token::new(token_id, token_index, token_length));

                        state = State::Undetermined;

                        continue 'decision_loop;
                    } else {
                        token_length += 1;
                        continue 'code_loop;
                    }
                }
            }
        }
    }

    if DEBUG {
        // Resolution state
        println!("> State = {:?} (end)", state);
        println!("{}", "-".repeat(80));
    }

    match &state {
        State::Undetermined => { /* NOTE: I think it should impossible to reach this state. */ }
        State::Whitespace => {
            if include_whitespace {
                tokens.push(Token::new(token_id, token_index, token_length));
            }
        }
        State::CommentAmbiguous => {
            // NOTE: This'll be treated as a single-line comment
            token_id = TokenId::CommentSingleLine;

            if include_comments {
                tokens.push(Token::new(token_id, token_index, token_length));
            }
        }
        State::CommentSingleLine => {
            if include_comments {
                tokens.push(Token::new(token_id, token_index, token_length));
            }
        }
        State::CommentMultiLine => {
            let mut ok = false;

            // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
            // <  copy code here >

            if attempting_to_close_comment {
                if end_semicolon_count > 0 {
                    let mut pop_off_stack = false;
                    let mut match_index = 0;

                    'leftover_stack_loop: for (i, stack_semicolon_count) in
                        multi_line_comment_stack.iter().enumerate().rev()
                    {
                        if end_semicolon_count < *stack_semicolon_count {
                            break 'leftover_stack_loop;
                        } else if end_semicolon_count == *stack_semicolon_count {
                            pop_off_stack = true;
                            match_index = i;

                            break 'leftover_stack_loop;
                        }
                    }

                    if pop_off_stack {
                        while multi_line_comment_stack.len() > match_index {
                            multi_line_comment_stack.pop();
                        }

                        if multi_line_comment_stack.len() == 0 {
                            ok = true;

                            if include_comments {
                                tokens.push(Token::new(token_id, token_index, token_length));
                            }
                        }
                    }
                }
            }

            // </ copy code here >
            // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

            if !ok {
                // Unterminated multi-line comment

                return Err(SyntaxError::ParsingError(ParsingError::new(
                    code,
                    token_index,
                    code.len(),
                    ErrorCode::UnterminatedMultiLineComment,
                    "Unterminated multiline comment",
                )));
            }
        }
        State::StringAmbiguous => {
            if start_double_quote_count == 1 {
                // Unterminated normal string
                return Err(SyntaxError::ParsingError(ParsingError::new(
                    code,
                    token_index,
                    code.len(),
                    ErrorCode::UnterminatedNormalString,
                    "Unterminated normal string",
                )));
            } else if start_double_quote_count == 2 {
                token_id = TokenId::StringNormal;
                // Empty string
                tokens.push(Token::new(token_id, token_index, token_length));
            } else {
                // Unterminated extended string
                return Err(SyntaxError::ParsingError(ParsingError::new(
                    code,
                    token_index,
                    code.len(),
                    ErrorCode::UnterminatedExtendedString,
                    "Unterminated extended string",
                )));
            }
        }
        State::StringNormal => {
            // Untermianted normal string
            return Err(SyntaxError::ParsingError(ParsingError::new(
                code,
                token_index,
                code.len(),
                ErrorCode::UnterminatedNormalString,
                "Unterminated normal string",
            )));
        }
        State::StringExtendedConfig | State::StringExtendedBody | State::StringExtendedEnd => {
            // Untermianted extended string
            return Err(SyntaxError::ParsingError(ParsingError::new(
                code,
                token_index,
                code.len(),
                ErrorCode::UnterminatedExtendedString,
                "Unterminated extended string",
            )));
        }
        State::Word => {
            tokens.push(Token::new(token_id, token_index, token_length));
        }
    }

    Ok(tokens)
}

// Unit tests

/// A parse node is either a token or a list of parse nodes.
#[derive(Debug)]
pub enum ParseNode {
    Token(Token),
    List(ParseNodeList),
}

/// A parse node list is a list of parse nodes.
#[derive(Debug)]
pub struct ParseNodeList {
    /// Index of the parse node list starts in the code.
    pub index: usize,
    /// The length of the code this parse node list spans over.
    pub length: usize,
    /// List of parse nodes
    pub children: Vec<ParseNode>,
}

impl ParseNodeList {
    /// Creates a new parse node list.
    #[inline]
    fn new(index: usize) -> ParseNodeList {
        ParseNodeList {
            index,
            length: 0,
            children: Vec::new(),
        }
    }

    fn add_child_length(&mut self, child: ParseNode) -> () {
        match &child {
            ParseNode::Token(token) => self.length += token.length,
            ParseNode::List(parse_node_list) => self.length += parse_node_list.length,
        }
    }

    fn add_child(&mut self, child: ParseNode) -> () {
        match &child {
            ParseNode::Token(token) => self.length += token.length,
            ParseNode::List(parse_node_list) => self.length += parse_node_list.length,
        }
        self.children.push(child);
    }
}

/// Parses the code and returns a result of parse trees or a parsing error.
pub fn parse(
    code: &str,
    include_comments: bool,
    include_whitespace: bool,
    include_list_delimiters: bool,
) -> Result<Vec<ParseNode>, SyntaxError> {
    let tokens: Vec<Token>;

    match tokenize(code, include_comments, include_whitespace) {
        Err(parsing_error) => {
            return Err(parsing_error);
        }
        Ok(result_tokens) => {
            tokens = result_tokens;
        }
    }

    let mut parse_node_list_stack = Vec::new();

    parse_node_list_stack.push(ParseNodeList::new(0));

    let mut open_bracket_token_stack = Vec::new();

    fn brackets_match(open_bracket: TokenId, close_bracket: TokenId) -> Result<bool, SyntaxError> {
        match open_bracket {
            TokenId::RoundBracketOpen => match close_bracket {
                TokenId::RoundBracketClose => return Ok(true),
                TokenId::CurlyBracketClose => return Ok(false),
                TokenId::SquareBracketClose => return Ok(false),
                _ => {
                    return Err(SyntaxError::AssumptionError(
                        "Logic error in brackets_match (1).".to_string(),
                    ))
                }
            },
            TokenId::CurlyBracketOpen => match close_bracket {
                TokenId::RoundBracketClose => return Ok(false),
                TokenId::CurlyBracketClose => return Ok(true),
                TokenId::SquareBracketClose => return Ok(false),
                _ => {
                    return Err(SyntaxError::AssumptionError(
                        "Logic error in brackets_match (2).".to_string(),
                    ))
                }
            },
            TokenId::SquareBracketOpen => match close_bracket {
                TokenId::RoundBracketClose => return Ok(false),
                TokenId::CurlyBracketClose => return Ok(false),
                TokenId::SquareBracketClose => return Ok(true),
                _ => {
                    return Err(SyntaxError::AssumptionError(
                        "Logic error in brackets_match (3).".to_string(),
                    ))
                }
            },
            _ => {
                return Err(SyntaxError::AssumptionError(
                    "Logic error in brackets_match (4).".to_string(),
                ))
            }
        }
    }

    for token in tokens {
        match token.id {
            TokenId::Whitespace => {
                let parse_node_list = parse_node_list_stack.last_mut().unwrap();

                if include_comments {
                    parse_node_list.add_child(ParseNode::Token(token));
                } else {
                    parse_node_list.add_child_length(ParseNode::Token(token));
                }
            }
            TokenId::CommentSingleLine | TokenId::CommentMultiLine => {
                let parse_node_list = parse_node_list_stack.last_mut().unwrap();

                if include_comments {
                    parse_node_list.add_child(ParseNode::Token(token));
                } else {
                    parse_node_list.add_child_length(ParseNode::Token(token));
                }
            }
            TokenId::Word | TokenId::StringNormal | TokenId::StringExtended => {
                parse_node_list_stack
                    .last_mut()
                    .unwrap()
                    .add_child(ParseNode::Token(token));
            }
            TokenId::RoundBracketOpen | TokenId::SquareBracketOpen | TokenId::CurlyBracketOpen => {
                // Increase nesting
                parse_node_list_stack.push(ParseNodeList::new(token.index));

                if include_list_delimiters {
                    parse_node_list_stack
                        .last_mut()
                        .unwrap()
                        .add_child(ParseNode::Token(token.clone()));
                }

                open_bracket_token_stack.push(token);
            }
            TokenId::RoundBracketClose
            | TokenId::SquareBracketClose
            | TokenId::CurlyBracketClose => {
                // Decrease nesting
                match open_bracket_token_stack.pop() {
                    None => {
                        // TODO: add better start / end index

                        let parsing_error = ParsingError::new(
                            code.as_bytes(),
                            0,
                            0,
                            ErrorCode::UnexpectedClosingDelimiter,
                            "Unexpected closing delimiter",
                        );

                        return Err(SyntaxError::ParsingError(parsing_error));
                    }
                    Some(open_bracket) => {
                        if brackets_match(open_bracket.id, token.id)? {
                            if include_list_delimiters {
                                parse_node_list_stack
                                    .last_mut()
                                    .unwrap()
                                    .add_child(ParseNode::Token(token));
                            }

                            let parse_node_level = parse_node_list_stack.pop().unwrap();

                            parse_node_list_stack
                                .last_mut()
                                .unwrap()
                                .add_child(ParseNode::List(parse_node_level));
                        } else {
                            // TODO: add better start / end index

                            let parsing_error = ParsingError::new(
                                code.as_bytes(),
                                0,
                                0,
                                ErrorCode::DelimiterMismatch,
                                "Delimiter mismatch",
                            );

                            return Err(SyntaxError::ParsingError(parsing_error));
                        }
                    }
                }
            }
        }
    }

    if parse_node_list_stack.len() != 1 {
        // TODO: add better start / end index

        let parsing_error = ParsingError::new(
            code.as_bytes(),
            0,
            0,
            ErrorCode::UnclosedDelimiter,
            "Unclosed delimiter mismatch",
        );

        return Err(SyntaxError::ParsingError(parsing_error));
    }

    let parse_trees = parse_node_list_stack.pop().unwrap().children;

    return Ok(parse_trees);
}

#[cfg(test)]
mod test_tokenizer {
    use super::tokenize as original_tokenize;
    use super::*;

    #[inline]
    fn tokenize(code: &str) -> Result<Vec<Token>, SyntaxError> {
        return original_tokenize(code, true, true);
    }

    /// Returns `true` if equal, otherwise `false`.
    #[inline]
    fn assert_eq_code_and_tokens_length(code: &str, tokens: &Vec<Token>) {
        let m = code.len();

        let mut n = 0;

        for token in tokens {
            n += token.length;
        }

        assert_eq!(m, n);
    }

    fn assert_code_is_ok(code: &str) {
        let tokenization_result = tokenize(code);
        match tokenization_result {
            Ok(_) => (),
            Err(syntax_error) => match syntax_error {
                SyntaxError::ParsingError(parsing_error) => panic!(parsing_error.error_message),
                SyntaxError::AssumptionError(error_message) => panic!(error_message),
            },
        }
    }

    fn assert_code_is_not_ok(code: &str) {
        let tokenization_result = tokenize(code);
        match tokenization_result {
            Ok(_) => {
                panic!();
            }
            Err(_) => (),
        }
    }

    // -------------------------------------------------------------------------

    #[test]
    fn test_misc() {
        assert_code_is_ok("");
        assert_code_is_ok(" ");

        if HAS_ISSUE_132 {
            assert_code_is_not_ok(");");
            assert_code_is_not_ok(";(););");

            assert_code_is_ok(";();");
            assert_code_is_ok(";(;(););");
            assert_code_is_ok(";(;(;();););");

            assert_code_is_ok(";;();;");

            //
            assert_code_is_ok(";;(;;();;);;");

            assert_code_is_not_ok(";(;;();;););");
        }
    }

    #[test]
    fn test_icelandic_name() {
        assert_code_is_ok(r#"(var name "Davíð")"#)
    }

    #[test]
    fn test_whitespace() {
        {
            let code = r#" "#;

            let tokenization_result = tokenize(code);
            let tokens = tokenization_result.unwrap();

            assert_eq_code_and_tokens_length(code, &tokens);

            assert!(tokens.len() == 1);

            let token = &tokens[0];

            assert!(token.id == TokenId::Whitespace);
            assert!(token.index == 0);
            assert!(token.length == 1);
        }
    }

    #[test]
    fn test_word() {
        {
            let code = r#"hallo"#;

            let tokenization_result = tokenize(code);
            let tokens = tokenization_result.unwrap();

            assert_eq_code_and_tokens_length(code, &tokens);

            assert!(tokens.len() == 1);

            let token = &tokens[0];

            assert!(token.id == TokenId::Word);
            assert!(token.index == 0);
            assert!(token.length == 5);
        }
    }

    #[test]
    fn test_round_bracket_open() {
        {
            let code = r#"()"#;

            let tokenization_result = tokenize(code);
            let tokens = tokenization_result.unwrap();

            assert_eq_code_and_tokens_length(code, &tokens);

            assert!(tokens.len() == 2);

            let token = &tokens[0];

            assert!(token.id == TokenId::RoundBracketOpen);
            assert!(token.index == 0);
            assert!(token.length == 1);
        }
    }

    #[test]
    fn test_round_bracket_close() {
        {
            let code = r#"()"#;

            let tokenization_result = tokenize(code);
            let tokens = tokenization_result.unwrap();

            assert_eq_code_and_tokens_length(code, &tokens);

            assert!(tokens.len() == 2);

            let token = &tokens[1];

            assert!(token.id == TokenId::RoundBracketClose);
            assert!(token.index == 1);
            assert!(token.length == 1);
        }
    }

    #[test]
    fn test_curly_bracket_open() {
        {
            let code = r#"{}"#;

            let tokenization_result = tokenize(code);
            let tokens = tokenization_result.unwrap();

            assert_eq_code_and_tokens_length(code, &tokens);

            assert!(tokens.len() == 2);

            let token = &tokens[0];

            assert!(token.id == TokenId::CurlyBracketOpen);
            assert!(token.index == 0);
            assert!(token.length == 1);
        }
    }

    #[test]
    fn test_curly_bracket_close() {
        {
            let code = r#"{}"#;

            let tokenization_result = tokenize(code);
            let tokens = tokenization_result.unwrap();

            assert_eq_code_and_tokens_length(code, &tokens);

            assert!(tokens.len() == 2);

            let token = &tokens[1];

            assert!(token.id == TokenId::CurlyBracketClose);
            assert!(token.index == 1);
            assert!(token.length == 1);
        }
    }

    #[test]
    fn test_square_bracket_open() {
        {
            let code = r#"[]"#;

            let tokenization_result = tokenize(code);
            let tokens = tokenization_result.unwrap();

            assert_eq_code_and_tokens_length(code, &tokens);

            assert!(tokens.len() == 2);

            let token = &tokens[0];

            assert!(token.id == TokenId::SquareBracketOpen);
            assert!(token.index == 0);
            assert!(token.length == 1);
        }
    }

    #[test]
    fn test_suqare_bracket_close() {
        {
            let code = r#"[]"#;

            let tokenization_result = tokenize(code);
            let tokens = tokenization_result.unwrap();

            assert_eq_code_and_tokens_length(code, &tokens);

            assert!(tokens.len() == 2);

            let token = &tokens[1];

            assert!(token.id == TokenId::SquareBracketClose);
            assert!(token.index == 1);
            assert!(token.length == 1);
        }
    }

    #[test]
    fn test_string_normal() {
        {
            let code = r#""hallo""#;

            let tokenization_result = tokenize(code);
            let tokens = tokenization_result.unwrap();

            assert_eq_code_and_tokens_length(code, &tokens);

            assert!(tokens.len() == 1);

            let token = &tokens[0];

            assert!(token.id == TokenId::StringNormal);
            assert!(token.index == 0);
            assert!(token.length == 7);
        }
    }

    #[test]
    fn test_string_extended() {
        {
            let code = r#"""".hallo.""""#;

            let tokenization_result = tokenize(code);
            let tokens = tokenization_result.unwrap();

            assert_eq_code_and_tokens_length(code, &tokens);

            assert!(tokens.len() == 1);

            let token = &tokens[0];

            assert!(token.id == TokenId::StringExtended);
            assert!(token.index == 0);
            assert!(token.length == 13);
        }
        {
            let code = r#"""",hallo.""""#;

            let tokenization_result = tokenize(code);
            let tokens = tokenization_result.unwrap();

            assert_eq_code_and_tokens_length(code, &tokens);

            assert!(tokens.len() == 1);

            let token = &tokens[0];

            assert!(token.id == TokenId::StringExtended);
            assert!(token.index == 0);
            assert!(token.length == 13);
        }
        {
            let code = r#"""".hallo,""""#;

            let tokenization_result = tokenize(code);
            let tokens = tokenization_result.unwrap();

            assert_eq_code_and_tokens_length(code, &tokens);

            assert!(tokens.len() == 1);

            let token = &tokens[0];

            assert!(token.id == TokenId::StringExtended);
            assert!(token.index == 0);
            assert!(token.length == 13);
        }
        {
            let code = r#"""",hallo,""""#;

            let tokenization_result = tokenize(code);
            let tokens = tokenization_result.unwrap();

            assert_eq_code_and_tokens_length(code, &tokens);

            assert!(tokens.len() == 1);

            let token = &tokens[0];

            assert!(token.id == TokenId::StringExtended);
            assert!(token.index == 0);
            assert!(token.length == 13);
        }
    }

    #[test]
    fn test_comment_single_line() {
        {
            let code = r#";"#;

            let tokenization_result = tokenize(code);
            let tokens = tokenization_result.unwrap();

            assert_eq_code_and_tokens_length(code, &tokens);

            assert!(tokens.len() == 1);

            let token = &tokens[0];

            assert!(token.id == TokenId::CommentSingleLine);
            assert!(token.index == 0);
            assert!(token.length == 1);
        }
        {
            let code = r#"; "#;

            let tokenization_result = tokenize(code);
            let tokens = tokenization_result.unwrap();

            assert_eq_code_and_tokens_length(code, &tokens);

            assert!(tokens.len() == 1);

            let token = &tokens[0];

            assert!(token.id == TokenId::CommentSingleLine);
            assert!(token.index == 0);
            assert!(token.length == 2);
        }
        {
            let code = r#";a"#;

            let tokenization_result = tokenize(code);
            let tokens = tokenization_result.unwrap();

            assert_eq_code_and_tokens_length(code, &tokens);

            assert!(tokens.len() == 1);

            let token = &tokens[0];

            assert!(token.id == TokenId::CommentSingleLine);
            assert!(token.index == 0);
            assert!(token.length == 2);
        }
        {
            let code = r#"; a"#;

            let tokenization_result = tokenize(code);
            let tokens = tokenization_result.unwrap();

            assert_eq_code_and_tokens_length(code, &tokens);

            assert!(tokens.len() == 1);

            let token = &tokens[0];

            assert!(token.id == TokenId::CommentSingleLine);
            assert!(token.index == 0);
            assert!(token.length == 3);
        }
        {
            let code = r#";;"#;

            let tokenization_result = tokenize(code);
            let tokens = tokenization_result.unwrap();

            assert_eq_code_and_tokens_length(code, &tokens);

            assert!(tokens.len() == 1);

            let token = &tokens[0];

            assert!(token.id == TokenId::CommentSingleLine);
            assert!(token.index == 0);
            assert!(token.length == 2);
        }
        {
            let code = r#";;;"#;

            let tokenization_result = tokenize(code);
            let tokens = tokenization_result.unwrap();

            assert_eq_code_and_tokens_length(code, &tokens);

            assert!(tokens.len() == 1);

            let token = &tokens[0];

            assert!(token.id == TokenId::CommentSingleLine);
            assert!(token.index == 0);
            assert!(token.length == 3);
        }
    }

    #[test]
    fn test_comment_multi_line_1() {
        assert_code_is_ok(";();");
    }

    #[test]
    fn test_comment_multi_line_2() {
        assert_code_is_not_ok(";;();");
    }

    #[test]
    fn test_comment_multi_line_3() {
        assert_code_is_not_ok(";();;");
    }

    #[test]
    fn test_comment_multi_line_4() {
        // Guard
        assert_code_is_not_ok(";(;;();");
    }

    #[test]
    fn test_comment_multi_line_5() {
        let code = r#";();"#;

        let tokenization_result = tokenize(code);
        let tokens = tokenization_result.unwrap();

        assert_eq_code_and_tokens_length(code, &tokens);

        assert!(tokens.len() == 1);

        let token = &tokens[0];

        assert!(token.id == TokenId::CommentMultiLine);
        assert!(token.index == 0);
        assert!(token.length == 4);
    }

    #[test]
    fn test_comment_multi_line_6() {
        let code = r#" ;(;();); "#;

        let tokenization_result = tokenize(code);
        let tokens = tokenization_result.unwrap();

        assert_eq_code_and_tokens_length(code, &tokens);

        assert!(tokens.len() == 3);

        let token = &tokens[1];

        assert!(token.id == TokenId::CommentMultiLine);
        assert!(token.index == 1);
        assert!(token.length == 8);
    }

    #[test]
    fn test_comment_multi_line_7() {
        let code = r#";(;(););"#;

        let tokenization_result = tokenize(code);
        let tokens = tokenization_result.unwrap();

        assert_eq_code_and_tokens_length(code, &tokens);

        assert!(tokens.len() == 1);

        let token = &tokens[0];

        assert!(token.id == TokenId::CommentMultiLine);
        assert!(token.index == 0);
        assert!(token.length == 8);
    }

    #[test]
    fn test_comment_multi_line_8() {
        let code = r#" ;(;(););"#;

        let tokenization_result = tokenize(code);
        let tokens = tokenization_result.unwrap();

        assert_eq_code_and_tokens_length(code, &tokens);

        assert!(tokens.len() == 2);

        let token = &tokens[1];

        assert!(token.id == TokenId::CommentMultiLine);
        assert!(token.index == 1);
        assert!(token.length == 8);
    }

    #[test]
    fn test_comment_multi_line_9() {
        let code = r#";(;();); "#;

        let tokenization_result = tokenize(code);
        let tokens = tokenization_result.unwrap();

        assert_eq_code_and_tokens_length(code, &tokens);

        assert!(tokens.len() == 2);

        let token = &tokens[0];

        assert!(token.id == TokenId::CommentMultiLine);
        assert!(token.index == 0);
        assert!(token.length == 8);
    }

    #[test]
    fn test_empty_normal_string() {
        let code = r#""""#;

        let tokenization_result = tokenize(code);
        let tokens = tokenization_result.unwrap();

        assert_eq_code_and_tokens_length(code, &tokens);

        assert!(tokens.len() == 1);

        let token = &tokens[0];

        assert!(token.id == TokenId::StringNormal);
        assert!(token.index == 0);
        assert!(token.length == 2);
    }

    #[test]
    fn test_unterminated_normal_string() {
        {
            let code = r#"""#;
            let tokenization_result = tokenize(code);
            match tokenization_result {
                Err(syntax_error) => match syntax_error {
                    SyntaxError::ParsingError(parsing_error) => assert_eq!(
                        parsing_error.error_code,
                        ErrorCode::UnterminatedNormalString
                    ),
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
        {
            let code = r#""a"#;
            let tokenization_result = tokenize(code);

            match tokenization_result {
                Err(syntax_error) => match syntax_error {
                    SyntaxError::ParsingError(parsing_error) => assert_eq!(
                        parsing_error.error_code,
                        ErrorCode::UnterminatedNormalString
                    ),
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
        {
            let code = r#""a\"#;
            let tokenization_result = tokenize(code);

            match tokenization_result {
                Err(syntax_error) => match syntax_error {
                    SyntaxError::ParsingError(parsing_error) => assert_eq!(
                        parsing_error.error_code,
                        ErrorCode::UnterminatedNormalString
                    ),
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
        {
            let code = r#""a\""#;
            let tokenization_result = tokenize(code);

            match tokenization_result {
                Err(syntax_error) => match syntax_error {
                    SyntaxError::ParsingError(parsing_error) => assert_eq!(
                        parsing_error.error_code,
                        ErrorCode::UnterminatedNormalString
                    ),
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
        {
            let tokenization_result = tokenize(r#""a\"c"#);

            match tokenization_result {
                Err(syntax_error) => match syntax_error {
                    SyntaxError::ParsingError(parsing_error) => assert_eq!(
                        parsing_error.error_code,
                        ErrorCode::UnterminatedNormalString
                    ),
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
    }

    #[test]
    fn test_unterminated_extended_string() {
        {
            let code = r#"""""#;
            let tokenization_result = tokenize(code);

            match tokenization_result {
                Err(syntax_error) => match syntax_error {
                    SyntaxError::ParsingError(parsing_error) => assert_eq!(
                        parsing_error.error_code,
                        ErrorCode::UnterminatedExtendedString
                    ),
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
        {
            let code = r#""""."#;
            let tokenization_result = tokenize(code);

            match tokenization_result {
                Err(syntax_error) => match syntax_error {
                    SyntaxError::ParsingError(parsing_error) => assert_eq!(
                        parsing_error.error_code,
                        ErrorCode::UnterminatedExtendedString
                    ),
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
        {
            let code = r#"""".""#;
            let tokenization_result = tokenize(code);

            match tokenization_result {
                Err(syntax_error) => match syntax_error {
                    SyntaxError::ParsingError(parsing_error) => assert_eq!(
                        parsing_error.error_code,
                        ErrorCode::UnterminatedExtendedString
                    ),
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
        {
            let code = r#""""."""#;
            let tokenization_result = tokenize(code);

            match tokenization_result {
                Err(syntax_error) => match syntax_error {
                    SyntaxError::ParsingError(parsing_error) => assert_eq!(
                        parsing_error.error_code,
                        ErrorCode::UnterminatedExtendedString
                    ),
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
    }
}

#[cfg(test)]
mod test_parser {
    use super::*;

    fn assert_code_is_ok(code: &str) {
        // let code = code.as_bytes();
        let result = parse(code, true, true, true);
        match result {
            Ok(_) => (),
            Err(syntax_error) => match syntax_error {
                SyntaxError::ParsingError(parsing_error) => panic!(parsing_error.error_message),
                SyntaxError::AssumptionError(error_message) => panic!(error_message),
            },
        }
    }

    fn assert_code_is_not_ok(code: &str) {
        // let code = code.as_bytes();
        let result = parse(code, true, true, true);
        match result {
            Ok(_) => {
                panic!();
            }
            Err(_) => (),
        }
    }

    #[test]
    fn test_misc() {
        assert_code_is_ok("");
        assert_code_is_ok(" ");

        assert_code_is_ok("()");

        assert_code_is_ok("(())");

        assert_code_is_ok("((()))");

        assert_code_is_ok("((())())");

        assert_code_is_ok("()");
        assert_code_is_ok("[]");
        assert_code_is_ok("{}");

        assert_code_is_ok("(())");
        assert_code_is_ok("([])");
        assert_code_is_ok("({})");

        assert_code_is_ok("[()]");
        assert_code_is_ok("[[]]");
        assert_code_is_ok("[{}]");

        assert_code_is_ok("{()}");
        assert_code_is_ok("{[]}");
        assert_code_is_ok("{{}}");
    }

    #[test]
    fn test_unexpected_closing_delimiter() {
        assert_code_is_not_ok(r#")"#);
        assert_code_is_not_ok(r#"]"#);
        assert_code_is_not_ok(r#"}"#);
    }

    #[test]
    fn test_delimiter_mismatch() {
        assert_code_is_not_ok(r#"(]"#);
        assert_code_is_not_ok(r#"(}"#);
        assert_code_is_not_ok(r#"[)"#);
        assert_code_is_not_ok(r#"[}"#);
        assert_code_is_not_ok(r#"{)"#);
        assert_code_is_not_ok(r#"{]"#);
    }

    #[test]
    fn test_unclosed_delimiter() {
        assert_code_is_not_ok(r#" ( "#);

        assert_code_is_not_ok(r#"("#);
        assert_code_is_not_ok(r#"["#);
        assert_code_is_not_ok(r#"{"#);
    }
}

#[cfg(test)]
mod test_lib {
    use super::{parse, ParseNode};
    use std::error::Error;

    fn t1() -> Result<Vec<ParseNode>, Box<dyn Error>> {
        let code = " ( hello )) ";

        let result = parse(code, false, false, false)?;

        Ok(result)
    }

    #[test]
    fn test_1() {
        let result = t1();

        println!("-> {:?}", result);
    }
}
