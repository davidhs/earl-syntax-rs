//! Lang. tokenizer
//!
//! Limitations
//!
//! * Handles only ASCII whitespace, not UTF-8 whitespace in general.  UTF-8 whitespace, that isn't
//!   ASCII whitespace, gets attached to word tokens.

// TODO: print better error messages.

use super::shared::{ErrorCode, ParsingError};

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

/// A Token's `id` is its type, but I can't use `type` as the name of a field in
/// a struct.
///
/// A token keeps track of what kind of token it is, i.e. `id`, and at what byte it was found in
/// the input and how many bytes it spans.
///
/// * The field `id` is what type of token this is.
/// * The field `index` is the index of the byte where this token begins.
/// * The field `length` is how many bytes this token spans.
#[derive(Clone, Debug)]
pub struct Token {
    /// asdfasdff
    pub id: TokenId,
    pub index: usize,
    /// asdfasdff
    pub length: usize,
    // This is a convenience thing...
    pub lexeme: String,
}

impl Token {
    /// Creates a new token.
    #[inline]
    #[allow(dead_code)]
    pub fn new(id: TokenId, index: usize, length: usize) -> Token {
        Token { id, index, length, lexeme: "".to_string() }
    }

    #[inline]
    pub fn new_from_code(id: TokenId, index: usize, length: usize, code: &str) -> Token {
        let i = index;
        let j = index + length;

        let x = &code[i..j];

        let y = x.to_string();

        let lexeme = y;

        Token { id, index, length, lexeme }
    }

    /// Convenience thing
    #[allow(dead_code)]
    pub fn finalize(self: &mut Self, code: &str) -> () {
        let i = self.index;
        let j = self.index + self.length;

        let x = &code[i..j];

        let y = x.to_string();

        self.lexeme = y;
    }

    #[allow(dead_code)]
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

// Convenience function
#[inline(always)]
fn create_token(id: TokenId, index: usize, length: usize, code: &str) -> Token {
    Token::new_from_code(id, index, length, code)
}

/// Returns a tokenization result.
///
/// The input `code` is a byte slice.  It's expected its a byte slice of a UTF-8 code.
pub fn tokenize(code: &str, include_comments: bool, include_whitespace: bool) -> Result<Vec<Token>, ParsingError> {
    // We need to

    let original_code = code;

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
                    panic!("No progress!");
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

                        tokens.push(create_token(token_id, token_index, token_length, original_code));

                        continue 'code_loop;
                    } else if c == b')' {
                        pc = c;

                        token_id = TokenId::RoundBracketClose;
                        token_index = code_index;
                        token_length = 1;

                        // NOTE: this is a temporary solution for Issue[132].

                        tokens.push(create_token(token_id, token_index, token_length, original_code));

                        continue 'code_loop;
                    } else if c == b'[' {
                        pc = c;

                        token_id = TokenId::SquareBracketOpen;
                        token_index = code_index;
                        token_length = 1;

                        tokens.push(create_token(token_id, token_index, token_length, original_code));

                        continue 'code_loop;
                    } else if c == b']' {
                        pc = c;

                        token_id = TokenId::SquareBracketClose;
                        token_index = code_index;
                        token_length = 1;

                        tokens.push(create_token(token_id, token_index, token_length, original_code));

                        continue 'code_loop;
                    } else if c == b'{' {
                        pc = c;

                        token_id = TokenId::CurlyBracketOpen;
                        token_index = code_index;
                        token_length = 1;

                        tokens.push(create_token(token_id, token_index, token_length, original_code));

                        continue 'code_loop;
                    } else if c == b'}' {
                        pc = c;

                        token_id = TokenId::CurlyBracketClose;
                        token_index = code_index;
                        token_length = 1;

                        tokens.push(create_token(token_id, token_index, token_length, original_code));

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
                            return Err(ParsingError::new(
                                code,
                                code_index,
                                code_index,
                                ErrorCode::PotentialErroneousClosingDelimiterFollowedByComment,
                                "Unterminated potential erroneous closing delimiter followed by comment",
                            ));
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
                            tokens.push(create_token(token_id, token_index, token_length, original_code));
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
                            tokens.push(create_token(token_id, token_index, token_length, original_code));
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
                            tokens.push(create_token(token_id, token_index, token_length, original_code));
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
                        let s: Vec<String> = (&multi_line_comment_stack).into_iter().map(|n| format!("{}{{", ";".repeat(*n))).collect();

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
                                'stack_loop: for (i, stack_semicolon_count) in multi_line_comment_stack.iter().enumerate().rev() {
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
                                            tokens.push(create_token(token_id, token_index, token_length, original_code));
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

                            tokens.push(create_token(token_id, token_index, token_length, original_code));

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
                        tokens.push(create_token(token_id, token_index, token_length, original_code));

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
                            tokens.push(create_token(token_id, token_index, token_length, original_code));

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
                    if c.is_ascii_whitespace() || c == b'(' || c == b')' || c == b'[' || c == b']' || c == b'{' || c == b'}' || c == b'"' || c == b';' {
                        // Done
                        tokens.push(create_token(token_id, token_index, token_length, original_code));

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
                tokens.push(create_token(token_id, token_index, token_length, original_code));
            }
        }
        State::CommentAmbiguous => {
            // NOTE: This'll be treated as a single-line comment
            token_id = TokenId::CommentSingleLine;

            if include_comments {
                tokens.push(create_token(token_id, token_index, token_length, original_code));
            }
        }
        State::CommentSingleLine => {
            if include_comments {
                tokens.push(create_token(token_id, token_index, token_length, original_code));
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

                    'leftover_stack_loop: for (i, stack_semicolon_count) in multi_line_comment_stack.iter().enumerate().rev() {
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
                                tokens.push(create_token(token_id, token_index, token_length, original_code));
                            }
                        }
                    }
                }
            }

            // </ copy code here >
            // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

            if !ok {
                // Unterminated multi-line comment

                return Err(ParsingError::new(code, token_index, code.len(), ErrorCode::UnterminatedMultiLineComment, "Unterminated multiline comment"));
            }
        }
        State::StringAmbiguous => {
            if start_double_quote_count == 1 {
                // Unterminated normal string
                return Err(ParsingError::new(code, token_index, code.len(), ErrorCode::UnterminatedNormalString, "Unterminated normal string"));
            } else if start_double_quote_count == 2 {
                token_id = TokenId::StringNormal;
                // Empty string
                tokens.push(create_token(token_id, token_index, token_length, original_code));
            } else {
                // Unterminated extended string
                return Err(ParsingError::new(code, token_index, code.len(), ErrorCode::UnterminatedExtendedString, "Unterminated extended string"));
            }
        }
        State::StringNormal => {
            // Untermianted normal string
            return Err(ParsingError::new(code, token_index, code.len(), ErrorCode::UnterminatedNormalString, "Unterminated normal string"));
        }
        State::StringExtendedConfig | State::StringExtendedBody | State::StringExtendedEnd => {
            // Untermianted extended string
            return Err(ParsingError::new(code, token_index, code.len(), ErrorCode::UnterminatedExtendedString, "Unterminated extended string"));
        }
        State::Word => {
            tokens.push(create_token(token_id, token_index, token_length, original_code));
        }
    }

    Ok(tokens)
}

// Debug functions

pub fn debug_print_tokenize(code: &str) -> () {
    let tokenization_result = tokenize(code, true, true);

    match tokenization_result {
        Err(parsing_error) => {
            println!("Error code: {:?}", parsing_error.error_code);
            println!("Error message:\n{}", parsing_error.error_message);
        }
        Ok(tokens) => {
            for token in tokens {
                let index = token.index;
                let length = token.length;
                let id = token.id;

                let i = index;
                let j = index + length;

                let s = &code[i..j];

                println!("> i: {}, l: {}, t: ({:?}) -> [{}]", index, length, id, s);
            }
        }
    }
}

// Unit tests

#[cfg(test)]
mod tests {
    use super::tokenize as original_tokenize;
    use super::*;

    #[inline]
    fn tokenize(code: &str) -> Result<Vec<Token>, ParsingError> {
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
            Err(parsing_error) => {
                panic!(parsing_error.error_message);
            }
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
                Ok(_) => panic!(),
                Err(parsing_error) => {
                    assert_eq!(parsing_error.error_code, ErrorCode::UnterminatedNormalString);
                }
            }
        }
        {
            let code = r#""a"#;
            let tokenization_result = tokenize(code);

            match tokenization_result {
                Ok(_) => panic!(),
                Err(parsing_error) => {
                    assert_eq!(parsing_error.error_code, ErrorCode::UnterminatedNormalString);
                }
            }
        }
        {
            let code = r#""a\"#;
            let tokenization_result = tokenize(code);

            match tokenization_result {
                Ok(_) => panic!(),
                Err(parsing_error) => {
                    assert_eq!(parsing_error.error_code, ErrorCode::UnterminatedNormalString);
                }
            }
        }
        {
            let code = r#""a\""#;
            let tokenization_result = tokenize(code);

            match tokenization_result {
                Ok(_) => panic!(),
                Err(parsing_error) => {
                    assert_eq!(parsing_error.error_code, ErrorCode::UnterminatedNormalString);
                }
            }
        }
        {
            let tokenization_result = tokenize(r#""a\"c"#);

            match tokenization_result {
                Ok(_) => panic!(),
                Err(parsing_error) => {
                    assert_eq!(parsing_error.error_code, ErrorCode::UnterminatedNormalString);
                }
            }
        }
    }

    #[test]
    fn test_unterminated_extended_string() {
        {
            let code = r#"""""#;
            let tokenization_result = tokenize(code);

            match tokenization_result {
                Ok(_) => panic!(),
                Err(parsing_error) => {
                    assert_eq!(parsing_error.error_code, ErrorCode::UnterminatedExtendedString);
                }
            }
        }
        {
            let code = r#""""."#;
            let tokenization_result = tokenize(code);

            match tokenization_result {
                Ok(_) => panic!(),
                Err(parsing_error) => {
                    assert_eq!(parsing_error.error_code, ErrorCode::UnterminatedExtendedString);
                }
            }
        }
        {
            let code = r#"""".""#;
            let tokenization_result = tokenize(code);

            match tokenization_result {
                Ok(_) => panic!(),
                Err(parsing_error) => {
                    assert_eq!(parsing_error.error_code, ErrorCode::UnterminatedExtendedString);
                }
            }
        }
        {
            let code = r#""""."""#;
            let tokenization_result = tokenize(code);

            match tokenization_result {
                Ok(_) => panic!(),
                Err(parsing_error) => {
                    assert_eq!(parsing_error.error_code, ErrorCode::UnterminatedExtendedString);
                }
            }
        }
    }
}
