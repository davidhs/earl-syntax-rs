//! Shared stuff between the tokenizer and the parser.

/// Error codes we might receive when tokenizing and parsing.
#[derive(PartialEq, Debug)]
pub enum ErrorCode {
    UnterminatedNormalString,
    UnterminatedExtendedString,
    UnterminatedMultiLineComment,
    UnexpectedClosingDelimiter,
    DelimiterMismatch,
    PotentialErroneousClosingDelimiterFollowedByComment,
    UnclosedDelimiter,
}

/// Collection of error information returned to the user if there's a problem tokenizing or parsing
/// the input.
///
/// NOTE: maybe find a better name?  This is both for tokenizing and parsing.
#[derive(Debug)]
pub struct ParsingError {
    pub error_code: ErrorCode,
    pub error_message: String,
}

impl ParsingError {
    /// This is for tokenization
    ///
    /// TODO: maybe need differnt ones for parsing since we have access to different kind of
    /// information.
    ///
    /// TODO: in future add hints or support for hints.
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
