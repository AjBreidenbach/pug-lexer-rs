use serde_json::Value;
use std::fs;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

fn evaluate_pugjs<P: AsRef<Path>>(filename: P) -> Value {
    println!("evaluate_pugjs");
    let node = Command::new("node")
        .args(&["tests/run-example.js", &filename.as_ref().to_string_lossy()])
        .output()
        .unwrap();

    let input = std::str::from_utf8(&node.stdout).unwrap();
    //println!("{}", input);
    //let r: serde_json::Result<Value> = serde_json::from_str("{}");
    let r: serde_json::Result<Value> = serde_json::from_str(input);
    let r = r.unwrap();

    println!("pugjs = \n{}", r);

    r
}

fn evaluate_pugrs<P: AsRef<Path>>(filename: P) -> Value {
    println!("evaluate_pugrs");
    let current_dir = std::env::current_dir().unwrap();
    let path = current_dir.join("tests/cases/").join(filename);
    println!("path = {:?}", path);
    let source = fs::read_to_string(path.clone()).unwrap();

    let options = pug_lexer::LexerOptions {
        filename: Some(String::from(path.to_string_lossy())),
        interpolated: false,
        starting_line: 1,
        starting_column: 1,
    };

    let r = serde_json::to_string(&pug_lexer::lex(source, options)).unwrap();

    let mut normalize_process = Command::new("node")
        .arg("tests/normalize-rust.js")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    let child_stdin = normalize_process.stdin.as_mut().unwrap();
    child_stdin.write_all(r.as_bytes()).unwrap();

    let normalize_output = normalize_process.wait_with_output().unwrap();

    let r: serde_json::Result<Value> =
        serde_json::from_str(std::str::from_utf8(&normalize_output.stdout).unwrap());
    let r = r.unwrap();

    println!("pugrs =\n{}", r);

    r
}

macro_rules! evaluation_test {
    ($test_name:ident, $file:literal) => {
        #[test]
        fn $test_name() {
            assert_eq!(evaluate_pugjs($file), evaluate_pugrs($file))
        }
    };
}

evaluation_test!(test_simple_comment, "comment.pug");
evaluation_test!(test_comments_source, "comments.source.pug");
evaluation_test!(test_comments_markup, "comments.pug");
evaluation_test!(test_utf8bom, "utf8bom.pug");
evaluation_test!(test_blockquote, "blockquote.pug");
//evaluation_test!(test_block_expansion, "block-expansion.pug");
evaluation_test!(test_basic, "basic.pug");
