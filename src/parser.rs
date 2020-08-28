//! Lang. parser

use super::shared::{ErrorCode, ParsingError};
use super::tokenizer::{tokenize, Token, TokenId};

#[derive(Debug)]
pub enum ParseNode {
    Token(Token),
    List(ParseNodeList),
}

#[derive(Debug)]
pub struct ParseNodeList {
    pub index: usize,
    pub length: usize,
    pub children: Vec<ParseNode>,
}

impl ParseNodeList {
    #[inline]
    fn new(index: usize) -> ParseNodeList {
        ParseNodeList { index, length: 0, children: Vec::new() }
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

/// Returns parse trees
pub fn parse(code: &str, include_comments: bool, include_whitespace: bool, include_list_delimiters: bool) -> Result<Vec<ParseNode>, ParsingError> {
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

    fn brackets_match(open_bracket: TokenId, close_bracket: TokenId) -> bool {
        match open_bracket {
            TokenId::RoundBracketOpen => match close_bracket {
                TokenId::RoundBracketClose => return true,
                TokenId::CurlyBracketClose => return false,
                TokenId::SquareBracketClose => return false,
                _ => panic!(),
            },
            TokenId::CurlyBracketOpen => match close_bracket {
                TokenId::RoundBracketClose => return false,
                TokenId::CurlyBracketClose => return true,
                TokenId::SquareBracketClose => return false,
                _ => panic!(),
            },
            TokenId::SquareBracketOpen => match close_bracket {
                TokenId::RoundBracketClose => return false,
                TokenId::CurlyBracketClose => return false,
                TokenId::SquareBracketClose => return true,
                _ => panic!(),
            },
            _ => panic!(),
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
                parse_node_list_stack.last_mut().unwrap().add_child(ParseNode::Token(token));
            }
            TokenId::RoundBracketOpen | TokenId::SquareBracketOpen | TokenId::CurlyBracketOpen => {
                // Increase nesting
                parse_node_list_stack.push(ParseNodeList::new(token.index));

                if include_list_delimiters {
                    parse_node_list_stack.last_mut().unwrap().add_child(ParseNode::Token(token.clone()));
                }

                open_bracket_token_stack.push(token);
            }
            TokenId::RoundBracketClose | TokenId::SquareBracketClose | TokenId::CurlyBracketClose => {
                // Decrease nesting
                match open_bracket_token_stack.pop() {
                    None => {
                        // TODO: add better start / end index

                        let parsing_error = ParsingError::new(code.as_bytes(), 0, 0, ErrorCode::UnexpectedClosingDelimiter, "Unexpected closing delimiter");

                        return Err(parsing_error);
                    }
                    Some(open_bracket) => {
                        if brackets_match(open_bracket.id, token.id) {
                            if include_list_delimiters {
                                parse_node_list_stack.last_mut().unwrap().add_child(ParseNode::Token(token));
                            }

                            let parse_node_level = parse_node_list_stack.pop().unwrap();

                            parse_node_list_stack.last_mut().unwrap().add_child(ParseNode::List(parse_node_level));
                        } else {
                            // TODO: add better start / end index

                            let parsing_error = ParsingError::new(code.as_bytes(), 0, 0, ErrorCode::DelimiterMismatch, "Delimiter mismatch");

                            return Err(parsing_error);
                        }
                    }
                }
            }
        }
    }

    if parse_node_list_stack.len() != 1 {
        // TODO: add better start / end index

        let parsing_error = ParsingError::new(code.as_bytes(), 0, 0, ErrorCode::UnclosedDelimiter, "Unclosed delimiter mismatch");

        return Err(parsing_error);
    }

    let parse_trees = parse_node_list_stack.pop().unwrap().children;

    return Ok(parse_trees);
}

fn do_pretty_print(code: &str, parse_node: &ParseNode, with_border: bool, level: usize) -> () {
    let indent_size = 2;

    // TODO: either something is wrong with the parsing or something is wrong with the parsing

    let pad = " ".repeat(indent_size * level);

    match parse_node {
        ParseNode::Token(token) => {
            let i = token.index;
            let j = token.index + token.length;

            if with_border {
                print!("{}[{}]", pad, &code[i..j]);
            } else {
                print!("{}{}", pad, &code[i..j]);
            }
        }
        ParseNode::List(list) => {
            if with_border {
                print!("{}[(]", pad);
            } else {
                print!("{}(", pad);
            }

            for child in list.children.iter() {
                println!();
                do_pretty_print(code, child, with_border, level + 1);
            }
            println!();

            if with_border {
                print!("{}[)]", pad);
            } else {
                print!("{})", pad);
            }
        }
    }
}

pub fn pretty_print(code: &str, parse_tree: &ParseNode, with_border: bool) -> () {
    do_pretty_print(code, parse_tree, with_border, 0);
}

pub fn debug_print_parse(code: &str) -> () {
    let parse_result = parse(code, true, false, false);

    match parse_result {
        Err(parsing_error) => {
            println!("Error code: {:?}", parsing_error.error_code);
            println!("Error message:\n{}", parsing_error.error_message);
        }
        Ok(parse_trees) => {
            //
            println!("Parse trees");
            println!("-----------");
            println!();
            // println!("{:#?}", parse_trees);

            for parse_tree in parse_trees.iter() {
                pretty_print(code, parse_tree, true);
                println!();
                println!();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_code_is_ok(code: &str) {
        // let code = code.as_bytes();
        let result = parse(code, true, true, true);
        match result {
            Ok(_) => (),
            Err(parsing_error) => {
                panic!(parsing_error.error_message);
            }
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

    // -------------------------------------------------------------------------

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
