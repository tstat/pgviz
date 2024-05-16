use super::Eval;
use lalrpop_util::lalrpop_mod;

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

pub fn parse<'a>(input: &'a str) -> Vec<Eval<'a>> {
    let parser = grammar::ExprParser::new();
    parser.parse(input).unwrap().postfix()
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
                    eval_queue.push(Eval::Within {
                        table_name,
                        num_edges: *num_edges,
                    });
                }
                Expr::Schema { schema_name } => eval_queue.push(Eval::Schema { schema_name }),
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
    #[test]
    fn bonk() {
        let parsed = grammar::ExprParser::new()
            .parse("within 2 bonkers and (within 1 zonk or within 0 bonk)");
        println!("{parsed:?}");
        assert!(parsed.is_ok());
    }
}
