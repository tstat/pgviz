use super::{Eval, Literal};
use ariadne::{Color, Label, Report, ReportKind, Source};
use lalrpop_util::{lalrpop_mod, lexer::Token, ParseError};
use regex::Regex;

#[derive(Debug)]
pub struct Ann<T> {
    start: usize,
    end: usize,
    inner: T,
}

#[derive(Debug)]
pub enum SExpr<'a> {
    App {
        func: Ann<&'a str>,
        args: Vec<Ann<SExpr<'a>>>,
    },
    Value(Value<'a>),
}

#[derive(Debug)]
pub enum Value<'a> {
    Ident(&'a str),
    String(&'a str),
    Nat(u32),
}

static SCHEMA_SIG: FuncSig = FuncSig {
    args: &[Type::String],
    rest: Some(Type::String),
    typ: Type::Query,
};

static TABLE_SIG: FuncSig = FuncSig {
    args: &[Type::String],
    rest: Some(Type::String),
    typ: Type::Query,
};

static WITHIN_SIG: FuncSig = FuncSig {
    args: &[Type::Natural, Type::Query],
    rest: None,
    typ: Type::Query,
};

static OR_SIG: FuncSig = FuncSig {
    args: &[Type::Query],
    rest: Some(Type::Query),
    typ: Type::Query,
};

static AND_SIG: FuncSig = FuncSig {
    args: &[Type::Query],
    rest: Some(Type::Query),
    typ: Type::Query,
};

static DIFF_SIG: FuncSig = FuncSig {
    args: &[Type::Query, Type::Query],
    rest: None,
    typ: Type::Query,
};

static SUBGRAPH_SIG: FuncSig = FuncSig {
    args: &[Type::String, Type::Graph],
    rest: None,
    typ: Type::Subgraph,
};

static GRAPH_SIG: FuncSig = FuncSig {
    args: &[Type::Query],
    rest: Some(Type::Subgraph),
    typ: Type::Graph,
};

struct FuncSig {
    args: &'static [Type],
    rest: Option<Type>,
    typ: Type,
}

fn get_func_sig(name: &str) -> Option<&'static FuncSig> {
    match name {
        "table" => Some(&TABLE_SIG),
        "within" => Some(&WITHIN_SIG),
        "schema" => Some(&SCHEMA_SIG),
        "or" => Some(&OR_SIG),
        "and" => Some(&AND_SIG),
        "-" => Some(&DIFF_SIG),
        "graph" => Some(&GRAPH_SIG),
        "subgraph" => Some(&SUBGRAPH_SIG),
        _ => None,
    }
}

fn mismatch(expr: &Ann<SExpr<'_>>, expected: Type, actual: Type) -> Box<Report<'static>> {
    Box::new(
        Report::build(ReportKind::Error, (), expr.start)
            .with_label(
                Label::new(expr.start..expr.end)
                    .with_color(Color::Red)
                    .with_message(format!(
                        "Expected something of type: {expected:?}, but this has type: {actual:?}"
                    )),
            )
            .with_message("Type mismatch")
            .finish(),
    )
}

fn assert_type(
    expr: &Ann<SExpr<'_>>,
    expected: Type,
    actual: Type,
) -> Result<(), Box<Report<'static>>> {
    if expected != actual {
        return Err(mismatch(expr, expected, actual));
    }
    Ok(())
}

impl<'a> Ann<SExpr<'a>> {
    fn check(&self) -> Result<Type, Box<Report<'a>>> {
        enum Work<'a> {
            // when we arrive here the top of the stack be of the form:
            // `<arg0 type> ... <argn type>`
            AppFuncCont {
                func: &'a Ann<&'a str>,
                func_sig: &'static FuncSig,
                args: &'a Vec<Ann<SExpr<'a>>>,
            },
            Infer(&'a Ann<SExpr<'a>>),
        }
        let mut stack: Vec<Type> = Vec::new();
        let mut queue: Vec<Work> = vec![Work::Infer(self)];
        while let Some(work) = queue.pop() {
            match work {
                Work::AppFuncCont {
                    func,
                    func_sig,
                    args,
                } => {
                    let arg_types = args
                        .iter()
                        .rev()
                        .map(|e| {
                            let typ = stack.pop().unwrap();
                            (e, typ)
                        })
                        .rev()
                        .enumerate();
                    for (i, (arg_expr, arg_type)) in arg_types {
                        let expected =
                            func_sig
                                .args
                                .get(i)
                                .or(func_sig.rest.as_ref())
                                .ok_or_else(|| {
                                    Report::build(ReportKind::Error, (), arg_expr.start)
                                        .with_label(
                                            Label::new(arg_expr.start..arg_expr.end)
                                                .with_color(Color::Red)
                                                .with_message(format!(
                                                    "{} wasn't expecting any more arguments, but encountered one of type: {arg_type:?}", func.inner
                                                )),
                                        )
                                        .with_message("Too many arguments")
                                        .finish()
                                })?;
                        assert_type(arg_expr, *expected, arg_type)?;
                    }
                    if let Some(missing_type) = func_sig.args.get(args.len()) {
                        let app_end: usize = args.last().map(|e| e.end).unwrap_or(func.end);
                        let report = Report::build(ReportKind::Error, (), app_end)
                            .with_label(Label::new(app_end..app_end).with_message(format!(
                                "{} expects another argument of type: {missing_type:?}",
                                func.inner
                            )))
                            .with_message("Not enough arguments")
                            .finish();
                        return Err(Box::new(report));
                    }
                    stack.push(func_sig.typ);
                }
                Work::Infer(e0) => match &e0.inner {
                    SExpr::App { func, args } => {
                        let func_sig = match get_func_sig(func.inner) {
                            Some(x) => x,
                            None => {
                                let report = Report::build(ReportKind::Error, (), func.start)
                                    .with_label(
                                        Label::new(func.start..func.end)
                                            .with_color(Color::Red)
                                            .with_message("Unknown function"),
                                    )
                                    .with_message("Unknown function")
                                    .finish();
                                return Err(Box::new(report));
                            }
                        };
                        queue.push(Work::AppFuncCont {
                            func,
                            func_sig,
                            args,
                        });
                        for arg in args {
                            queue.push(Work::Infer(arg));
                        }
                    }
                    SExpr::Value(x) => match x {
                        Value::Ident(_) => stack.push(Type::String),
                        Value::String(_) => stack.push(Type::String),
                        Value::Nat(_) => stack.push(Type::Natural),
                    },
                },
            }
        }
        Ok(stack.pop().unwrap())
    }

    fn postfix(&self) -> Vec<Eval<'a>> {
        let mut eval_queue = Vec::new();
        let mut expr_queue: Vec<&Ann<SExpr<'a>>> = vec![self];

        while let Some(filt) = expr_queue.pop() {
            match &filt.inner {
                SExpr::App { func, args } => {
                    let func = match func.inner {
                        "table" => Eval::Table {
                            arg_count: args.len(),
                        },
                        "schema" => Eval::Schema {
                            arg_count: args.len(),
                        },
                        "within" => Eval::Within,
                        "or" => Eval::Or,
                        "and" => Eval::And,
                        "-" => Eval::Difference,
                        "graph" => Eval::Graph {
                            subgraph_count: args.len() - 1,
                        },
                        "subgraph" => Eval::Subgraph,
                        x => panic!("postfix: unknown function {x}"),
                    };
                    expr_queue.extend(args.iter().rev());
                    eval_queue.push(func);
                }
                SExpr::Value(v) => match v {
                    Value::Ident(x) => eval_queue.push(Eval::Literal(Literal::String(x))),
                    Value::String(x) => eval_queue.push(Eval::Literal(Literal::String(x))),
                    Value::Nat(x) => eval_queue.push(Eval::Literal(Literal::Natural(*x))),
                },
            }
        }
        eval_queue
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Type {
    Natural,
    String,
    Query,
    Subgraph,
    Graph,
}

fn pretty_parse_error<'a>(e: ParseError<usize, Token<'a>, &'a str>) -> Report<'a> {
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

pub fn parse_res(input: &str, expected: Type) -> Result<Vec<Eval<'_>>, Box<Report<'_>>> {
    let str_regex = Regex::new(r#""((?:[^"\\]|(?:\\).)*)""#).unwrap();
    let parser = grammar::ExprParser::new();
    let expr = parser
        .parse(&str_regex, input)
        .map_err(|e| pretty_parse_error(e))?;
    let typ = expr.check()?;
    assert_type(&expr, expected, typ)?;

    let res = expr.postfix();
    Ok(res)
}

pub fn parse(input: &str, expected: Type) -> Vec<Eval<'_>> {
    parse_res(input, expected).unwrap_or_else(|e| {
        e.eprint(Source::from(input)).unwrap();
        std::process::exit(1)
    })
}

lalrpop_mod!(grammar);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zonk() {
        let parsed = parse("(within 2 (table one two three))", Type::Query);
        println!("{parsed:?}");
    }
}
