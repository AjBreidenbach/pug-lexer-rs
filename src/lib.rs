extern crate lazy_static;
extern crate rhai;
extern crate serde;
extern crate serde_json;

use lazy_static::lazy_static;
use regex::{Captures, Match, Regex};
use serde::Serialize;
use std::cmp::{max, min, PartialEq};
use std::str;

const BOM: &str = "\u{FEFF}";
const NO_INDENT: &str = "expected indent regext to be initialized already";
const CANNOT_HOLD_VALUE: &str = "tried to assign value to an invalid token";
const CANNOT_SET_MODE: &str = "tried to set mode of invalid token";
const NO_END_BRACKET: &str = "End of line was reached with no closing bracket for interpolation.";

#[derive(Clone, Serialize)]
struct Position {
    line: usize,
    column: usize,
}

#[derive(Clone, Serialize)]
struct Loc {
    start: Option<Position>,
    filename: Option<String>,
    end: Option<Position>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum AttributeValue {
    StringValue(String),
    BooleanValue(bool),
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum BlockMode {
    Replace,
    Prepend,
    Append,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum LexToken {
    Colon,
    AndAttributes,
    Attribute {
        name: String,
        val: AttributeValue,
        must_escape: bool,
    },
    Block {
        val: Option<String>,
        mode: BlockMode,
    },
    Blockcode,
    Call {
        val: Option<String>,
        args: String,
    },
    Case {
        val: Option<String>,
    },
    Class {
        val: Option<String>,
    },
    Code {
        must_escape: bool,
        buffer: bool,
        val: Option<String>,
    },
    Comment {
        val: Option<String>,
        buffer: bool,
    },
    Default,
    Doctype {
        val: Option<String>,
    },
    Dot,
    Each {
        val: Option<String>,
        key: Option<String>,
        code: String,
    },
    EachOf {
        val: Option<String>,
        value: String,
        code: String,
    },
    ElseIf {
        val: Option<String>,
    },
    Else {
        val: Option<String>,
    },
    EndAttributes,
    EndPipelessText,
    EndPugInterpolation,
    EOS,
    Extends,
    Filter {
        val: Option<String>,
    },
    ID {
        val: Option<String>,
    },
    If {
        val: Option<String>,
    },
    Include,
    Indent {
        val: usize,
    },
    InterpolatedCode {
        must_escape: bool,
        buffer: bool,
        val: Option<String>,
    },
    Interpolation {
        val: Option<String>,
    },
    MixinBlock,
    Mixin {
        val: Option<String>,
        args: Option<String>,
    },
    Newline,
    Outdent,
    Path {
        val: Option<String>,
    },
    Slash,
    StartAttributes,
    StartPipelessText,
    StartPugInterpolation,
    Tag {
        val: Option<String>,
    },
    TextHTML {
        val: Option<String>,
    },
    Text {
        val: Option<String>,
    },
    When {
        val: Option<String>,
    },
    While {
        val: Option<String>,
    },
    Yield,
}
impl LexToken {
    fn empty_text() -> Self {
        Self::Text { val: None }
    }

    fn has_val(&self) -> bool {
        match self {
            Self::Block { val, .. }
            | Self::Call { val, .. }
            | Self::Class { val, .. }
            | Self::Code { val, .. }
            | Self::Comment { val, .. }
            | Self::Each { val, .. }
            | Self::EachOf { val, .. }
            | Self::ElseIf { val, .. }
            | Self::Else { val, .. }
            | Self::Filter { val, .. }
            | Self::ID { val, .. }
            | Self::If { val, .. }
            | Self::InterpolatedCode { val, .. }
            | Self::Interpolation { val, .. }
            | Self::Mixin { val, .. }
            | Self::Tag { val, .. }
            | Self::TextHTML { val, .. }
            | Self::Text { val, .. }
            | Self::When { val, .. }
            | Self::While { val, .. } => val.is_some(),
            _ => false,
        }
    }

    fn set_mode(&mut self, mode: BlockMode) {
        let mut new = match self {
            Self::Block { val, .. } => Self::Block {
                mode,
                val: val.take(),
            },
            _ => std::panic::panic_any(CANNOT_SET_MODE),
        };

        std::mem::swap(self, &mut new);
    }

    fn indent_val(val: usize) -> Self {
        Self::Indent { val }
    }

    fn get_str_val(&self) -> Option<&String> {
        match self {
            Self::Block { val, .. }
            | Self::Call { val, .. }
            | Self::Class { val, .. }
            | Self::Code { val, .. }
            | Self::Comment { val, .. }
            | Self::Each { val, .. }
            | Self::EachOf { val, .. }
            | Self::ElseIf { val, .. }
            | Self::Else { val, .. }
            | Self::Filter { val, .. }
            | Self::ID { val, .. }
            | Self::If { val, .. }
            | Self::InterpolatedCode { val, .. }
            | Self::Interpolation { val, .. }
            | Self::Mixin { val, .. }
            | Self::Tag { val, .. }
            | Self::TextHTML { val, .. }
            | Self::Text { val, .. }
            | Self::When { val, .. }
            | Self::While { val, .. } => val.as_ref(),
            _ => std::panic::panic_any("cannot get string val"),
        }
    }

    fn with_val(self, _val: String) -> Self {
        if self.has_val() {
            return self;
        }
        let val = Some(_val.clone());
        match self {
            Self::Attribute {
                must_escape, name, ..
            } => Self::Attribute {
                must_escape,
                name,
                val: AttributeValue::StringValue(_val),
            },
            Self::Block { mode, .. } => Self::Block { mode, val },
            Self::Call { args, .. } => Self::Call { args, val },
            Self::Class { .. } => Self::Class { val },
            Self::Code {
                must_escape,
                buffer,
                ..
            } => Self::Code {
                must_escape,
                buffer,
                val,
            },
            Self::Comment { buffer, .. } => Self::Comment { buffer, val },
            Self::Each { key, code, .. } => Self::Each { key, code, val },
            Self::EachOf { value, code, .. } => Self::EachOf { value, code, val },
            Self::ElseIf { .. } => Self::ElseIf { val },
            Self::Else { .. } => Self::Else { val },
            Self::Filter { .. } => Self::Filter { val },
            Self::ID { .. } => Self::ID { val },
            Self::If { .. } => Self::If { val },
            //Self::Indent {..} => Self::Indent{val},
            Self::InterpolatedCode {
                must_escape,
                buffer,
                ..
            } => Self::InterpolatedCode {
                must_escape,
                buffer,
                val,
            },
            Self::Interpolation { .. } => Self::Interpolation { val },
            Self::Mixin { args, .. } => Self::Mixin { args, val },
            Self::Tag { .. } => Self::Tag { val },
            Self::TextHTML { .. } => Self::TextHTML { val },
            Self::Text { .. } => Self::Text { val },
            Self::When { .. } => Self::When { val },
            Self::While { .. } => Self::While { val },
            _ => std::panic::panic_any(CANNOT_HOLD_VALUE),
        }
    }
}

//type Tokenizer = fn(&mut Lexer) -> bool;
mod tokenizers {
    use super::*;

    fn match_to_len(m: Match) -> usize {
        m.as_str().len()
    }

    pub(super) fn blank(lexer: &mut Lexer) -> bool {
        //println!("--blank");
        let search = BLANK_LINE_RE.find(&lexer.input).map(match_to_len);
        if let Some(_match) = search {
            lexer.consume(max(_match, 1) - 1);
            lexer.increment_line(1);
            return true;
        }
        false
    }

    pub(super) fn eos(lexer: &mut Lexer) -> bool {
        //println!("--eos");
        if lexer.input.len() > 0 {
            return false;
        }
        if lexer.interpolated {
            std::panic::panic_any(NO_END_BRACKET);
        }

        for i in &lexer.indent_stack {
            if *i == 0 {
                break;
            }
            lexer
                .tokens
                .push(lexer.tokend(lexer.tok(LexToken::Outdent)))
        }

        lexer.tokens.push(lexer.tokend(lexer.tok(LexToken::EOS)));
        lexer.ended = true;

        return true;
    }

    pub(super) fn end_interpolation(lexer: &mut Lexer) -> bool {
        //println!("--end_interpolation");
        if lexer.interpolated && lexer.input.starts_with(']') {
            lexer.input.replace_range(0..1, "");
            lexer.ended = true;
            return true;
        }

        return false;
    }

    pub(super) fn doctype(lexer: &mut Lexer) -> bool {
        //println!("--doctype");
        let tok = lexer.scan_eol(&DOCTYPE_RE, LexToken::Doctype { val: None });
        if let Some(tok) = tok {
            lexer.tokens.push(lexer.tokend(tok));
            return true;
        }

        false
    }

    /*
    pub(super) fn interpolation(lexer: &mut Lexer) -> bool {
        let _match = lexer.scan_eol(&DOCTYPE_RE, LexToken::Doctype { val: None });
    }
    */

    pub(super) fn case(lexer: &mut Lexer) -> bool {
        //println!("--case");
        let tok = lexer.scan_eol(&CASE_RE, LexToken::Case { val: None });
        if let Some(tok) = tok {
            lexer.tokens.push(lexer.tokend(tok));
            return true;
        }

        // TODO negative case

        false
    }

    /*
    pub(super) fn when(lexer: &mut Lexer) -> bool {
        let tok = lexer.scan_eol(&WHEN_RE, LexToken::When { val: None });
        if let Some(tok) = tok {
            lexer.tokens.push(lexer.tokend(tok));
            return true;
        }

        false
    }
    */

    pub(super) fn default(lexer: &mut Lexer) -> bool {
        //println!("--default");
        let tok = lexer.scan_eol(&DEFAULT_RE, LexToken::Default);
        if let Some(tok) = tok {
            lexer.tokens.push(lexer.tokend(tok));
            return true;
        }

        //TODO negative case

        false
    }

    pub(super) fn block(lexer: &mut Lexer) -> bool {
        //println!("--block");
        let captures = BLOCK_NEWLINE_RE.captures(&lexer.input);
        if let Some(captures) = captures {
            let mut name = captures
                .get(1)
                .map(|m| m.as_str().trim())
                .unwrap_or_default()
                .to_string();

            let mut comment = String::with_capacity(0);

            if name.find("//").is_some() {
                comment = format!(
                    "//{}",
                    name.split("//").skip(1).collect::<Vec<&str>>().join("//")
                );

                name = name
                    .split("//")
                    .next()
                    .map(|s| s.trim())
                    .unwrap_or_default()
                    .to_string();
            }

            if name.len() == 0 {
                return false;
            }

            let val = Some(name);
            let mut tok = lexer.tok(LexToken::Block {
                val,
                mode: BlockMode::Replace,
            });
            let captures_0_len = captures
                .get(0)
                .map(|m| m.as_str().len())
                .unwrap_or_default();

            let mut length = min(comment.len(), captures_0_len) - comment.len();

            while {
                let c = lexer.input.chars().nth(length - 1).unwrap();
                c == '\n' || c == '\t' || c == ' '
            } {
                length -= 1
            }

            lexer.increment_column(length);
            lexer.tokens.push(lexer.tokend(tok));
            lexer.consume(min(comment.len(), captures_0_len) - comment.len());
            lexer.increment_column(
                min(comment.len() + length, captures_0_len) - comment.len() - length,
            );

            return true;
        }

        false
    }

    pub(super) fn mixin_block(lexer: &mut Lexer) -> bool {
        //println!("--mixin_block");
        let tok = lexer.scan_eol(&BLOCK_RE, LexToken::MixinBlock);
        if let Some(tok) = tok {
            lexer.tokens.push(lexer.tokend(tok));
            true
        } else {
            false
        }
    }

    pub(super) fn include(lexer: &mut Lexer) -> bool {
        //println!("--include");
        let tok = lexer.scan(&INCLUDE_RE, LexToken::Include);
        if let Some(tok) = tok {
            lexer.tokens.push(lexer.tokend(tok));
            //TODO filters
            let is_path = path(lexer);
            /*
            if ! is_path {
                TODO error
            }
            */
            return true;
        }
        false
    }

    pub(super) fn path(lexer: &mut Lexer) -> bool {
        //println!("--path");
        let mut tok = lexer.scan_eol(&PATH_RE, LexToken::Path { val: None });
        if let Some(mut tok) = tok {
            tok.lex_token = match tok.lex_token {
                LexToken::Path { val: Some(val) } => LexToken::Path {
                    val: Some(val.trim().to_string()),
                },
                _ => tok.lex_token,
            };

            lexer.tokens.push(lexer.tokend(tok));
            return true;
        }
        false
    }

    pub(super) fn tag(lexer: &mut Lexer) -> bool {
        //println!("--tag");
        if let Some(captures) = &TAG_RE.captures(&lexer.input) {
            let name = captures.get(1).map(|m| m.as_str().to_string());
            let captures_0_len = captures
                .get(0)
                .map(|m| m.as_str().len())
                .unwrap_or_default();

            lexer.consume(captures_0_len);
            let tok = lexer.tok(LexToken::Tag { val: name });
            lexer.increment_column(captures_0_len);
            lexer.tokens.push(lexer.tokend(tok));
            return true;
        }
        false
    }

    pub(super) fn id(lexer: &mut Lexer) -> bool {
        //println!("--id");
        let tok = lexer.scan(&ID_RE, LexToken::ID { val: None });
        if let Some(tok) = tok {
            // no idea why I have to clone here to get the length
            if let Some(length) = tok.lex_token.get_str_val().clone().map(|s| s.len()) {
                lexer.increment_column(length);
            }
            lexer.tokens.push(lexer.tokend(tok));

            return true;
        }

        // TODO negative case

        false
    }

    pub(super) fn class_name(lexer: &mut Lexer) -> bool {
        //println!("--class_name");
        let tok = lexer.scan(&CLASSNAME_RE, LexToken::Class { val: None });
        if let Some(tok) = tok {
            lexer.increment_column(
                tok.lex_token
                    .get_str_val()
                    .map(|s| s.len())
                    .unwrap_or_default(),
            );
            lexer.tokens.push(lexer.tokend(tok));
            return true;
        }

        false
    }

    pub(super) fn _yield(lexer: &mut Lexer) -> bool {
        //println!("--yield");
        let tok = lexer.scan_eol(&YIELD_RE, LexToken::Yield);
        if let Some(tok) = tok {
            lexer.tokens.push(lexer.tokend(tok));
            true
        } else {
            false
        }
    }

    pub(super) fn comment(lexer: &mut Lexer) -> bool {
        //println!("--comment");
        //let search = COMMENT_RE.find(&lexer.input).map(match_to_len);
        let mut val = String::with_capacity(0);
        let mut buffer = false;
        let mut consume: usize = 0;
        let mut result = false;
        {
            let captures = COMMENT_RE.captures(&lexer.input);
            if let Some(captures) = captures {
                //println!("captures = {:?}", captures);
                val = captures
                    .get(2)
                    .map(|m| m.as_str())
                    .unwrap_or("")
                    .to_string();
                buffer = if let Some(_match) = captures.get(1) {
                    _match.as_str() != "-"
                } else {
                    true
                };
                //consume = captures[0].len();
                consume = captures
                    .get(0)
                    .map(|m| m.as_str().len())
                    .unwrap_or_default();
                result = true
            }
        }

        if result {
            let val = Some(val);
            lexer.consume(consume);
            let tok = lexer.tok(LexToken::Comment { val, buffer });
            lexer.increment_column(consume);
            lexer.tokens.push(lexer.tokend(tok));
            lexer.pipeless_text(0);
        }

        return result;
    }

    pub(super) fn indent(lexer: &mut Lexer) -> bool {
        //println!("--indent");
        let captures = lexer.scan_indentation();
        if let Some(captures) = captures.as_ref() {
            //println!("captures {:?}", captures)
        } else {
            //println!("no captures")
        }

        if let Some(captures) = captures {
            let indents = captures.get(1).map(|m| m.as_str().len()).unwrap_or(0);

            lexer.increment_line(1);

            lexer.consume(indents + 1);

            //TODO INVALID_INDENTATION

            if lexer.input.starts_with('\n') {
                lexer.interpolation_allowed = true;
                //return lexer.tokend(lexer.tok(LexToken::Newline));
                return true;
            }

            if indents < *lexer.indent_stack.get(0).unwrap_or(&0usize) {
                let mut outdent_count = 0;
                while *lexer.indent_stack.get(0).unwrap_or(&0usize) > indents {
                    //TODO INCONSISTENT_INDENTATION
                    outdent_count += 1;
                    lexer.indent_stack.remove(0);
                }

                while outdent_count > 0 {
                    outdent_count -= 1;
                    lexer.colno = 1;
                    let tok = lexer.tok(LexToken::Outdent);
                    lexer.colno = *lexer.indent_stack.get(0).unwrap_or(&0usize) + 1;
                    lexer.tokens.push(lexer.tokend(tok));
                }
            } else if indents != 0 && indents != *lexer.indent_stack.get(0).unwrap_or(&0usize) {
                let tok = lexer.tok(LexToken::indent_val(indents));
                lexer.colno = 1 + indents;
                lexer.tokens.push(lexer.tokend(tok));
                lexer.indent_stack.insert(0, indents);
            } else {
                let tok = lexer.tok(LexToken::Newline);
                lexer.colno = 1 + min(lexer.indent_stack.get(0).unwrap_or(&0), &indents);
                lexer.tokens.push(lexer.tokend(tok));
            }
            lexer.interpolation_allowed = true;
            return true;
        }

        false
    }

    pub(super) fn text(lexer: &mut Lexer) -> bool {
        //println!("--text");
        let tok = lexer
            .scan(&TEXT_RE_1, LexToken::Text { val: None })
            .or_else(|| lexer.scan(&TEXT_RE_2, LexToken::Text { val: None }))
            .or_else(|| lexer.scan(&TEXT_RE_3, LexToken::Text { val: None }));

        if let Some(tok) = tok {
            let val = tok
                .lex_token
                .get_str_val()
                .map(|s| s.clone())
                .unwrap_or_default();
            lexer.add_text(tok.lex_token, val, None, 0);
            true
        } else {
            false
        }
    }

    pub(super) fn text_html(lexer: &mut Lexer) -> bool {
        //println!("--text_html");
        let tok = lexer.scan(&TEXT_HTML_RE, LexToken::TextHTML { val: None });
        if let Some(tok) = tok {
            let val = tok
                .lex_token
                .get_str_val()
                .map(|s| s.clone())
                .unwrap_or_default();
            lexer.add_text(tok.lex_token, val, None, 0);
            true
        } else {
            false
        }
    }
}

#[derive(Clone, Serialize)]
pub struct Token {
    lex_token: LexToken,
    loc: Loc,
}

pub struct LexerOptions {
    pub filename: Option<String>,
    pub interpolated: bool,
    pub starting_line: usize,
    pub starting_column: usize,
}

lazy_static! {
    static ref CARRIAGE_RETURN: Regex = Regex::new(r"\r\n|\r").unwrap();
    static ref BLANK_LINE_RE: Regex = Regex::new(r"^\n[ \t]*\n").unwrap();
    static ref COMMENT_RE: Regex = Regex::new(r"^//(-)?([^\n]*)").unwrap();
    static ref TABS_RE: Regex = Regex::new(r"^\n(\t*) *").unwrap();
    static ref SPACES_RE: Regex = Regex::new(r"^\n( *)").unwrap();
    static ref STRING_INTERP_RE: Regex = Regex::new(r"(\\)?([#!])\{((?:.|\n)*)$").unwrap();
    static ref SCAN_EOL_WHITESPACE_RE: Regex = Regex::new(r"^([ ]+)([^ ]*)").unwrap();
    static ref SCAN_EOL_1_RE: Regex = Regex::new(r"^[ \t]*(\n|$)").unwrap();
    static ref SCAN_EOL_2_RE: Regex = Regex::new(r"^[ \t]*").unwrap();
    static ref YIELD_RE: Regex = Regex::new(r"^yield").unwrap();
    static ref BLOCK_RE: Regex = Regex::new(r"^block").unwrap();
    static ref BLOCK_NEWLINE_RE: Regex = Regex::new(r"^block +([^\n]+)").unwrap();
    static ref EXTENDS_RE: Regex = Regex::new(r"^extends?(?= |$|\n)").unwrap();
    static ref DOT_RE: Regex = Regex::new(r"^\.").unwrap();
    static ref TEXT_HTML_RE: Regex = Regex::new(r"^(<[^\n]*)").unwrap();
    static ref TEXT_RE_1: Regex = Regex::new(r"^(?:\| ?| )([^\n]+)").unwrap();
    static ref TEXT_RE_2: Regex = Regex::new(r"^( )").unwrap();
    static ref TEXT_RE_3: Regex = Regex::new(r"^\|( ?)").unwrap();
    static ref CLASSNAME_RE: Regex =
        Regex::new(r"(?i)^\.([_a-z0-9\-]*[_a-z][_a-z0-9\-]*)").unwrap();
    static ref ID_RE: Regex = Regex::new(r"^#([\w-]+)").unwrap();
    static ref INVALID_ID_RE: Regex = Regex::new(r"^#").unwrap();
    static ref INVALID_ID_CAPTURE: Regex = Regex::new(r".[^ \t\(\#\.\:]*").unwrap();
    static ref DOCTYPE_RE: Regex = Regex::new(r"^doctype *([^\n]*)").unwrap();
    static ref FILTER_RE: Regex = Regex::new(r"^:([\w\-]+)").unwrap();
    static ref TAG_RE: Regex = Regex::new(r"^(\w(?:[-:\w]*\w)?)").unwrap();
    static ref INTERPOLATION_RE: Regex = Regex::new(r"^#\{").unwrap();
    static ref CASE_RE: Regex = Regex::new(r"^case +([^\n]+)").unwrap();
    static ref CASE_NEGATIVE_RE: Regex = Regex::new(r"^case\b").unwrap();
    static ref WHEN_RE: Regex = Regex::new(r"^when +([^:\n]+)").unwrap();
    static ref WHEN_NEGATIVE_RE: Regex = Regex::new(r"^when\b").unwrap();
    static ref DEFAULT_RE: Regex = Regex::new(r"^default").unwrap();
    static ref DEFAULT_NEGATIVE_RE: Regex = Regex::new(r"^default\b").unwrap();
    static ref INCLUDE_RE: Regex = Regex::new(r"^include(?=:| |$|\n)").unwrap();
    static ref INCLUDE_NEGATIVE_RE: Regex = Regex::new(r"^include\b").unwrap();
    static ref PATH_RE: Regex = Regex::new(r"^ ([^\n]+)").unwrap();
}

fn remove_bom(str: &str) -> String {
    if str.starts_with(BOM) {
        String::from(str::from_utf8(&str.as_bytes()[3..]).unwrap())
    } else {
        str.to_string()
    }
}

fn normalize_line_endings(str: &str) -> String {
    CARRIAGE_RETURN.replace_all(str, "\n").into()
}

struct Lexer {
    input: String,
    original_input: String,
    filename: Option<String>,
    interpolated: bool,
    lineno: usize,
    colno: usize,
    indent_stack: Vec<usize>,
    indent_re: Option<Regex>,
    interpolation_allowed: bool,
    whitespace_re: Option<Regex>,
    tokens: Vec<Token>,
    ended: bool,
}

impl Lexer {
    fn new(source: &str, options: LexerOptions) -> Self {
        let source = remove_bom(source);

        let input = normalize_line_endings(&source);
        let original_input = input.clone();
        let filename = options.filename;
        let interpolated = options.interpolated; // || false?
        let lineno = min(options.starting_line, 1);
        let colno = min(options.starting_column, 1);
        let indent_stack = Vec::with_capacity(4);
        let indent_re = None;
        let interpolation_allowed = true;
        let whitespace_re = Some(Regex::new(r"[ \n\t]").unwrap());
        let tokens = Vec::new();
        let ended = false;

        return Lexer {
            input,
            original_input,
            filename,
            interpolated,
            lineno,
            colno,
            indent_stack,
            indent_re,
            interpolation_allowed,
            whitespace_re,
            tokens,
            ended,
        };
    }

    fn fail(&self) -> bool {
        std::panic::panic_any(format!("unexpected text '{}'", &self.input[0..5]));
    }

    fn bracket_expression(skip: usize) -> usize {
        // TODO assert open bracket here

        0
    }

    fn tok(&self, lex_token: LexToken) -> Token {
        Token {
            lex_token,
            loc: Loc {
                start: Some(Position {
                    line: self.lineno,
                    column: self.colno,
                }),
                end: None,
                filename: self.filename.clone(),
            },
        }
    }

    fn tokend(&self, token: Token) -> Token {
        let mut token = token;
        token.loc.end = Some(Position {
            line: self.lineno,
            column: self.colno,
        });
        token
    }

    fn increment_line(&mut self, increment: usize) {
        self.lineno += increment;
        if increment != 0 {
            self.colno = 1;
        }
    }

    fn increment_column(&mut self, increment: usize) {
        self.colno += increment
    }

    fn consume(&mut self, len: usize) {
        //println!("consuming {}", len);
        self.input.replace_range(0..len, "");

        //println!("input = {:?}", self.input);
    }

    fn pipeless_text(&mut self, indents: usize) -> bool {
        //println!("pipeless_text indents {}", indents);
        while tokenizers::blank(self) {}

        let captures = self.scan_indentation();
        let mut indents = indents;

        if indents == 0 {
            if let Some(captures) = captures {
                //println!("captures = {:?}", captures);
                indents = captures[1].len()
            }
        }

        if indents
            > if self.indent_stack.len() == 0 {
                0
            } else {
                self.indent_stack[0]
            }
        {
            //println!("start-pipeless-text");
            self.tokens
                .push(self.tokend(self.tok(LexToken::StartPipelessText)));

            let mut tokens = Vec::new();
            let mut token_indent = Vec::new();
            let mut stringptr = 0;

            while {
                let nl_index = if let Some(i) = self.input[(stringptr + 1)..].find("\n") {
                    i
                } else {
                    self.input.len() - stringptr - 1
                };

                // possibly different because of substr
                let mut begin = stringptr + 1;
                let end = min(begin + nl_index, self.input.len() - 1);
                begin = min(begin, end);
                let buffer = &self.input[begin..end];
                let line_targets = format!("\n{}", buffer);
                let line_captures = self
                    .indent_re
                    .as_ref()
                    .expect(NO_INDENT)
                    .captures(&line_targets);

                let line_indents = line_captures
                    .as_ref()
                    .map(|captures| captures[1].len())
                    .unwrap_or_default();
                let mut is_match = line_indents >= indents;
                token_indent.push(is_match);
                is_match = is_match || buffer.trim().len() == 0;

                if is_match {
                    stringptr += buffer.len() + 1;
                    let begin = if buffer.len() == 0 {
                        0
                    } else {
                        min(buffer.len() - 1, indents)
                    };
                    tokens.push(String::from(&buffer[begin..]))
                } else if line_indents > *self.indent_stack.get(0).unwrap_or(&0usize) {
                    self.tokens.pop();
                    return self.pipeless_text(line_captures.unwrap()[1].len());
                }

                (self.input.len() - stringptr != 0) && is_match
            } {}

            self.consume(stringptr);

            //println!("n tokens = {}", tokens.len());
            while self.input.len() == 0 && tokens[tokens.len() - 1] == "" {
                tokens.pop();
            }

            for (i, token) in tokens.into_iter().enumerate() {
                //println!("token {}, i {}", token, i);
                self.increment_line(1);
                let mut tok = None;

                if i != 0 {
                    tok = Some(self.tok(LexToken::Newline))
                }
                if token_indent[i] {
                    self.increment_column(indents)
                }
                if let Some(tok) = tok {
                    self.tokens.push(self.tokend(tok))
                }

                self.add_text(LexToken::empty_text(), token, None, 0);
            }

            //println!("end-pipeless-text");
            self.tokens
                .push(self.tokend(self.tok(LexToken::EndPipelessText)));

            return true;
        }

        false
    }

    fn add_text(
        &mut self,
        lex_token: LexToken,
        value: String,
        prefix: Option<String>,
        escaped: usize,
    ) {
        /*println!(
            "addText lex_token {:?}; value {}; prefix {:?}; escaped {}",
            lex_token, value, prefix, escaped
        );*/
        //        let mut tok = None;
        //
        let is_prefixed = prefix.is_some();
        let mut prefix = prefix.unwrap_or(String::with_capacity(0));
        let mut value = value;

        if is_prefixed && value.len() + prefix.len() == 0 {
            return;
        }

        let index_end = if self.interpolated {
            value.find(']')
        } else {
            None
        }
        .unwrap_or(usize::MAX);

        let index_start = if self.interpolation_allowed {
            value.find("#[")
        } else {
            None
        }
        .unwrap_or(usize::MAX);

        let index_escaped = if self.interpolation_allowed {
            value.find("\\#[")
        } else {
            None
        }
        .unwrap_or(usize::MAX);

        let string_interp_match = STRING_INTERP_RE.find(&value);
        let string_interp_index = if self.interpolation_allowed {
            string_interp_match.map(|m| m.start())
        } else {
            None
        }
        .unwrap_or(usize::MAX);

        if index_escaped != usize::MAX
            && index_escaped < index_end
            && index_escaped < index_start
            && index_escaped < string_interp_index
        {
            //println!("break1");
            prefix.push_str(&value[0..index_escaped]);
            prefix.push_str("#[");

            value.replace_range(0..index_escaped + 3, "");
            return self.add_text(lex_token, value, Some(prefix), escaped + 1);
        }

        if index_start != usize::MAX
            && index_start < index_end
            && index_start < index_escaped
            && index_start < string_interp_index
        {
            //println!("break2");
            let tok = self.tok(lex_token.clone().with_val(format!(
                "{}{}",
                prefix,
                &value[0..index_start]
            )));
            self.increment_column(prefix.len() + index_start + escaped);
            self.tokens.push(self.tokend(tok));
            let tok = self.tok(LexToken::StartPugInterpolation);
            self.increment_column(2);
            self.tokens.push(self.tokend(tok));

            let mut child = Lexer::new(
                &value[index_start + 2..],
                LexerOptions {
                    filename: self.filename.clone(),
                    interpolated: true,
                    starting_line: self.lineno,
                    starting_column: self.colno, // plugins
                },
            );
            let mut interpolated = child.get_tokens();
            self.colno = child.colno;
            self.tokens.append(&mut interpolated);
            let tok = self.tok(LexToken::EndPugInterpolation);
            self.increment_column(1);
            self.tokens.push(self.tokend(tok));
            self.add_text(lex_token, child.input, None, 0);
            return;
        }

        if index_end != usize::MAX
            && index_end < index_start
            && index_end < index_escaped
            && index_end < string_interp_index
        {
            //println!("path1");
            let value_slice = &value[0..index_end];
            if prefix.len() + value_slice.len() != 0 {
                self.add_text(lex_token, String::from(value_slice), Some(prefix), 0);
            }
            self.ended = true;
            let mut temp = String::with_capacity(0);
            std::mem::swap(&mut self.input, &mut temp);
            self.input = String::from(
                &value[if let Some(i) = value.find(']') {
                    i + 1usize
                } else {
                    0
                }..],
            );
            self.input.push_str(&temp);

            return;
        }

        // TODO extra crap goes here

        //println!("path2");

        value = if prefix.len() > 0 {
            let mut p = prefix;
            p.push_str(&value);
            p
        } else {
            value
        };

        let value_len = value.len();
        let tok = self.tok(lex_token.with_val(value));
        self.increment_column(value_len + escaped);
        self.tokens.push(self.tokend(tok))
    }

    fn scan_indentation(&mut self) -> Option<Captures> {
        //println!("scan_indentation");
        //TODO can this be find instead of captures?
        if let Some(ref indent_re) = self.indent_re {
            indent_re.captures(&self.input)
        } else {
            if let Some(captures) = TABS_RE.captures(&self.input) {
                self.indent_re = Some(Regex::new(r"^\n(\t*) *").unwrap());

                if captures
                    .get(1)
                    .map(|m| m.as_str().len())
                    .unwrap_or_default()
                    == 0
                {
                    self.indent_re = Some(Regex::new(r"^\n( *)").unwrap());
                    SPACES_RE.captures(&self.input)
                } else {
                    Some(captures)
                }
            } else {
                None
            }
        }
    }

    fn scan(&mut self, regexp: &Regex, lex_token: LexToken) -> Option<Token> {
        let captures = regexp.captures(&self.input);
        if let Some(captures) = captures {
            let captures_0_len = captures
                .get(0)
                .map(|m| m.as_str().len())
                .unwrap_or_default();
            let val = captures.get(1).map(|m| m.as_str());
            let diff = captures_0_len - val.map(|v| v.len()).unwrap_or_default();

            let tok = self.tok(if let Some(val) = val {
                lex_token.with_val(val.to_string())
            } else {
                lex_token
            });
            self.consume(captures_0_len);
            self.increment_column(diff);

            return Some(tok);
        }
        None
    }

    fn scan_eol(&mut self, regexp: &Regex, lex_token: LexToken) -> Option<Token> {
        let captures = regexp.captures(&self.input);
        if let Some(captures) = captures {
            let mut whitespace_len = 0;
            let mut captures_0_len = 0;
            let mut captures_1 = String::with_capacity(0);
            {
                let captures_0 = captures.get(0).map(|m| m.as_str()).unwrap_or_default();
                captures_1 = captures
                    .get(1)
                    .map(|m| m.as_str())
                    .unwrap_or_default()
                    .to_string();
                captures_0_len = captures_0.len();
                let whitespace = SCAN_EOL_WHITESPACE_RE.captures(captures_0);
                if let Some(whitespace) = whitespace {
                    whitespace_len = whitespace
                        .get(1)
                        .map(|m| m.as_str().len())
                        .unwrap_or_default();
                    self.increment_column(whitespace_len);
                }
            }

            let new_input = &self.input[captures_0_len..];

            if new_input.starts_with(':') {
                self.input = new_input.to_string();
                let tok = self.tok(lex_token.with_val(captures_1));
                self.increment_column(captures_0_len - whitespace_len);
                return Some(tok);
            }

            if SCAN_EOL_1_RE.is_match(new_input) {
                let begin = SCAN_EOL_2_RE
                    .find(new_input)
                    .map(|m| m.as_str().len())
                    .unwrap_or_default();
                self.input = new_input[begin..].to_string();

                let tok = self.tok(lex_token.with_val(captures_1));
                self.increment_column(captures_0_len - whitespace_len);
                return Some(tok);
            }
        }
        None
    }

    fn advance(&mut self) {
        //println!("advance");
        tokenizers::blank(self)
            || tokenizers::eos(self)
            || tokenizers::end_interpolation(self)
            || tokenizers::_yield(self)
            || tokenizers::doctype(self)
            //|| tokenizers::interpolation(self)
            || tokenizers::case(self)
            //|| tokenizers::when(self)
            || tokenizers::default(self)
            //|| tokenizers::extends(self)
            //|| tokenizers::append(self)
            //|| tokenizers::prepend(self)
            || tokenizers::block(self)
            || tokenizers::mixin_block(self)
            //|| tokenizers::include(self)
            //|| tokenizers::mixin(self)
            //|| tokenizers::call(self)
            //|| tokenizers::conditional(self)
            //|| tokenizers::each_of(self)
            //|| tokenizers::each(self)
            //|| tokenizers::while(self)
            || tokenizers::tag(self)
            //|| tokenizers::filter(self)
            //|| tokenizers::block_code(self)
            //|| tokenizers::code(self)
            || tokenizers::id(self)
            //|| tokenizers::dot(self)
            || tokenizers::class_name(self)
            //|| tokenizers::attrs(self)
            //|| tokenizers::attributes_block(self)
            || tokenizers::indent(self)
            || tokenizers::text(self)
            || tokenizers::text_html(self)
            || tokenizers::comment(self)
            //|| tokenizers::slash(self)
            //|| tokenizers::colon(self)
            || self.fail();
    }

    fn get_tokens(&mut self) -> Vec<Token> {
        while !self.ended {
            self.advance()
        }

        let tokens = std::mem::take(&mut self.tokens);

        tokens
    }
}

pub fn lex(source: String, options: LexerOptions) -> Vec<Token> {
    let mut lexer = Lexer::new(&source, options);
    lexer.get_tokens()
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_remove_bom() {
        assert_eq!(remove_bom("\u{FEFF}hello world"), "hello world".to_string());
    }

    #[test]
    fn test_set_mode() {
        let mut blocktok = LexToken::Block {
            val: Some(String::from("smth")),
            mode: BlockMode::Prepend,
        };

        blocktok.set_mode(BlockMode::Replace);

        assert_eq!(
            LexToken::Block {
                val: Some(String::from("smth")),
                mode: BlockMode::Replace
            },
            blocktok
        )
    }

    #[test]
    fn test_regex() {
        let captures = SPACES_RE
            .captures("\n  // bar\n  li one\n  // baz\n  li two\n\n//\n  ul\n")
            .unwrap();
        assert_eq!(&captures[1], "  ");
        assert_eq!(&captures[0], "\n  ");
    }
}
