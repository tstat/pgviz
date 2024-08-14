use super::Eval;
use ariadne::{Color, Label, Report, ReportKind, Source};
use lalrpop_util::{lalrpop_mod, lexer::Token, ParseError};

#[derive(Debug)]
pub enum Graph<'a> {
    TopLevel { inner: Vec<Expr<'a>> },
    Subgraph { name: &'a str, inner: Vec<Expr<'a>> },
}

#[derive(Debug)]
pub enum Expr<'a> {
    And {
        lhs: Box<Expr<'a>>,
        rhs: Box<Expr<'a>>,
    },
    Or {
        lhs: Box<Expr<'a>>,
        rhs: Box<Expr<'a>>,
    },
    Difference {
        lhs: Box<Expr<'a>>,
        rhs: Box<Expr<'a>>,
    },
    Within {
        table_name: &'a str,
        num_edges: u32,
    },
    Schema {
        schema_name: &'a str,
    },
}

#[derive(Debug)]
pub struct Ann<T> {
    start: usize,
    end: usize,
    inner: T,
}

#[derive(Debug)]
pub enum SExpr<'a> {
    App {
        func: Box<Ann<SExpr<'a>>>,
        args: Vec<Ann<SExpr<'a>>>,
    },
    Value(Value<'a>),
}

#[derive(Debug)]
pub enum Value<'a> {
    Ident(&'a str),
    Nat(u32),
}

impl<'a> Ann<SExpr<'a>> {
    fn infer(&self) -> Type {
        match &self.inner {
            SExpr::Value(Value::Ident(_)) => Type::String,
            SExpr::Value(Value::Nat(_)) => Type::Natural,
            SExpr::App { func, args } => {
                let funcType = func.infer();
                let res = args.iter().fold(funcType, |b, a| {
                    let (input, output) = match b {
                        Type::Arr { lhs, rhs } => (lhs, rhs),
                        _ => panic!("malformed type"),
                    };
                    let argType = a.infer();
                    if *input != argType {
                        panic!("malformed type");
                    }
                    *output
                });
                res
            }
        }
    }

    fn postfix(&self) -> Vec<Eval<'a>> {
        let mut eval_queue = Vec::new();
        let mut expr_queue: Vec<&Ann<SExpr<'a>>> = vec![self];

        while let Some(filt) = expr_queue.pop() {
            todo!()
        }
        eval_queue
    }
}

enum Constraint<'a> {
    IsType { term: &'a Ann<SExpr<'a>>, typ: Type },
}

#[derive(Debug, PartialEq, Eq)]
enum Type {
    Natural,
    String,
    Query,
    Arr { lhs: Box<Type>, rhs: Box<Type> },
}

fn prettyParseError<'a>(e: ParseError<usize, Token<'a>, &'a str>) -> Report<'a> {
    match e {
        ParseError::InvalidToken { location } => Report::build(ReportKind::Error, (), location)
            .with_message("Invalid Token")
            .with_label(Label::new(location - 1..location))
            .finish(),
        ParseError::UnrecognizedEof { location, expected } => {
            Report::build(ReportKind::Error, (), location)
                .with_message("Unrecognized EOF")
                .with_label(
                    Label::new(location..location)
                        .with_message(format!("Expected: {}", expected.join(", "))),
                )
                .finish()
        }
        ParseError::UnrecognizedToken {
            token: (start, tok, end),
            expected,
        } => Report::build(ReportKind::Error, (), start)
            .with_message(format!("Unrecognized Token: {tok}"))
            .with_label(
                Label::new(start..end).with_message(format!("Expected: {}", expected.join(", "))),
            )
            .finish(),
        ParseError::ExtraToken {
            token: (start, tok, end),
        } => Report::build(ReportKind::Error, (), start)
            .with_message(format!("Extra token: {tok}"))
            .with_label(Label::new(start..end))
            .finish(),
        ParseError::User { error: _ } => unreachable!(),
    }
}

fn parse2(input: &str) -> Ann<SExpr<'_>> {
    let parser = grammar::ExprParser::new();
    match parser.parse(input) {
        Ok(x) => x,
        Err(e) => {
            prettyParseError(e).eprint(Source::from(input)).unwrap();
            std::process::exit(1)
        }
    }
}

pub fn parse(input: &str) -> Vec<Eval<'_>> {
    todo!()
    // let parser = grammar::ExprParser::new();
    // parser.parse(input).unwrap().postfix()
}

impl<'a> Expr<'a> {
    fn postfix(&self) -> Vec<Eval<'a>> {
        let mut eval_queue = Vec::new();
        let mut expr_queue: Vec<&Expr<'a>> = vec![self];

        while let Some(filt) = expr_queue.pop() {
            match filt {
                Expr::Within {
                    table_name,
                    num_edges,
                } => {
                    todo!();
                    // eval_queue.push(Eval::Within {
                    //     table_name,
                    //     num_edges: *num_edges,
                    // });
                }
                Expr::Schema { schema_name } => todo!(), // eval_queue.push(Eval::Schema { schema_name }),
                Expr::Difference { lhs, rhs } => {
                    eval_queue.push(Eval::Difference);
                    expr_queue.push(lhs);
                    expr_queue.push(rhs);
                }
                Expr::Or { lhs, rhs } => {
                    eval_queue.push(Eval::Or);
                    expr_queue.push(lhs);
                    expr_queue.push(rhs);
                }
                Expr::And { lhs, rhs } => {
                    eval_queue.push(Eval::And);
                    expr_queue.push(lhs);
                    expr_queue.push(rhs);
                }
            }
        }
        eval_queue
    }
}

lalrpop_mod!(grammar);

#[cfg(test)]
mod tests {
    use super::*;
    #[ignore]
    #[test]
    fn bonk() {
        let parsed = grammar::ExprParser::new()
            .parse("within 2 bonkers and (within 1 zonk or within 0 bonk)");
        println!("{parsed:?}");
        assert!(parsed.is_ok());
    }

    #[test]
    fn zonk() {
        let parsed = parse2("(within 2 (tables one two three))");
        println!("{parsed:?}");
    }
}
