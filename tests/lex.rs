use tlp::syntax::{
    cst::{
        data::SyntaxKind,
        lex::{self, Token},
    },
    span::ByteSpan,
};

fn force_lex(src: &str) -> Vec<Token> {
    let (tks, errs) = lex::from_str(src);

    if !errs.is_empty() {
        let errs = errs.iter().map(|e| format!("{}", e)).collect::<Vec<_>>();
        panic!("{:#?}", errs);
    }

    tks
}

#[test]
fn one_byte_tokens() {
    let src = "()";

    assert_eq!(
        self::force_lex(src),
        vec![
            Token {
                kind: SyntaxKind::LParen,
                sp: ByteSpan { lo: 0, hi: 1 },
            },
            Token {
                kind: SyntaxKind::RParen,
                sp: ByteSpan { lo: 1, hi: 2 },
            },
        ]
    );
}

#[test]
fn ws() {
    let src = "( \n)";

    assert_eq!(
        self::force_lex(src),
        vec![
            Token {
                kind: SyntaxKind::LParen,
                sp: ByteSpan { lo: 0, hi: 1 },
            },
            Token {
                kind: SyntaxKind::Ws,
                sp: ByteSpan { lo: 1, hi: 3 },
            },
            Token {
                kind: SyntaxKind::RParen,
                sp: ByteSpan { lo: 3, hi: 4 },
            },
        ]
    );
}

#[test]
fn num() {
    let src = "(* 1 3)";
    //         0 2 4 6

    assert_eq!(
        self::force_lex(src),
        vec![
            Token {
                kind: SyntaxKind::LParen,
                sp: ByteSpan { lo: 0, hi: 1 },
            },
            Token {
                kind: SyntaxKind::Ident,
                sp: ByteSpan { lo: 1, hi: 2 },
            },
            Token {
                kind: SyntaxKind::Ws,
                sp: ByteSpan { lo: 2, hi: 3 },
            },
            Token {
                kind: SyntaxKind::Num,
                sp: ByteSpan { lo: 3, hi: 4 },
            },
            Token {
                kind: SyntaxKind::Ws,
                sp: ByteSpan { lo: 4, hi: 5 },
            },
            Token {
                kind: SyntaxKind::Num,
                sp: ByteSpan { lo: 5, hi: 6 },
            },
            Token {
                kind: SyntaxKind::RParen,
                sp: ByteSpan { lo: 6, hi: 7 },
            },
        ]
    );
}

#[test]
fn nil() {
    let src = "nil";

    assert_eq!(
        self::force_lex(src),
        vec![Token {
            kind: SyntaxKind::Nil,
            sp: ByteSpan { lo: 0, hi: 3 },
        }],
    );
}

#[test]
fn string() {
    let src = r##"("str!")"##;
    //            0 2 4 67

    assert_eq!(
        self::force_lex(src),
        vec![
            Token {
                kind: SyntaxKind::LParen,
                sp: ByteSpan { lo: 0, hi: 1 },
            },
            Token {
                kind: SyntaxKind::StrEnclosure,
                sp: ByteSpan { lo: 1, hi: 2 },
            },
            Token {
                kind: SyntaxKind::StrContent,
                sp: ByteSpan { lo: 2, hi: 6 },
            },
            Token {
                kind: SyntaxKind::StrEnclosure,
                sp: ByteSpan { lo: 6, hi: 7 },
            },
            Token {
                kind: SyntaxKind::RParen,
                sp: ByteSpan { lo: 7, hi: 8 },
            },
        ],
    );
}
