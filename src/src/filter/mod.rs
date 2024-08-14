pub mod parser;
use super::*;
use std::{
    collections::BTreeMap,
    ops::{BitAnd, BitOr},
};

pub async fn apply<'a, 'b>(
    transaction: &Transaction<'a>,
    _tables: &BTreeMap<Oid, Table>,
    mut interpret_queue: Vec<Eval<'b>>,
    dont_follow: &Option<BTreeSet<Oid>>,
    fks: &BTreeMap<Oid, Vec<Oid>>,
    rfks: &BTreeMap<Oid, Vec<Oid>>,
) -> Result<BTreeSet<Oid>, tokio_postgres::Error> {
    let visible = {
        let mut stack: Vec<Value<'b>> = Vec::new();

        while let Some(eval_elem) = interpret_queue.pop() {
            match eval_elem {
                Eval::Within => {
                    let table_name = stack.pop().unwrap().string();
                    let num_edges = stack.pop().unwrap().natural();
                    let mut visible = BTreeSet::new();
                    let oids = get_matching_tables(transaction, table_name).await?;
                    let mut oids = std::pin::pin!(oids);
                    while let Some(oid) = oids.next().await {
                        bfs(&mut visible, dont_follow, fks, rfks, num_edges, oid);
                    }
                    stack.push(Value::Oids(visible));
                }
                Eval::Schema => {
                    let schema_name = stack.pop().unwrap().string();
                    let oids = get_matching_schemas(transaction, schema_name)
                        .await?
                        .collect()
                        .await;
                    stack.push(Value::Oids(oids));
                }
                Eval::Difference => {
                    let y = stack.pop().unwrap().oids();
                    let x = stack.pop().unwrap().oids();
                    stack.push(Value::Oids(&x - &y));
                }
                Eval::And => {
                    let y = stack.pop().unwrap().oids();
                    let x = stack.pop().unwrap().oids();
                    stack.push(Value::Oids(x.bitand(&y)));
                }
                Eval::Or => {
                    let y = stack.pop().unwrap().oids();
                    let x = stack.pop().unwrap().oids();
                    stack.push(Value::Oids(x.bitor(&y)));
                }
                Eval::Literal(x) => match x {
                    Literal::String(x) => stack.push(Value::String(x)),
                    Literal::Natural(x) => stack.push(Value::Natural(x)),
                },
            }
        }
        stack.pop().unwrap().oids()
    };

    Ok(visible)
}

#[derive(Debug)]
pub enum Value<'a> {
    String(&'a str),
    Natural(u32),
    Oids(BTreeSet<u32>),
}

impl<'a> Value<'a> {
    fn string(self) -> &'a str {
        match self {
            Value::String(x) => x,
            x => panic!("Expected a string but got: {x:?}"),
        }
    }

    fn natural(self) -> u32 {
        match self {
            Value::Natural(x) => x,
            x => panic!("Expected a natural but got: {x:?}"),
        }
    }

    fn oids(self) -> BTreeSet<u32> {
        match self {
            Value::Oids(x) => x,
            x => panic!("Expected a set of oids but got: {x:?}"),
        }
    }
}

#[derive(Debug)]
pub enum Eval<'a> {
    And,
    Or,
    Difference,
    Within,
    Schema,
    Literal(Literal<'a>),
}

#[derive(Debug)]
pub enum Literal<'a> {
    String(&'a str),
    Natural(u32),
}

pub fn bfs(
    visible: &mut BTreeSet<Oid>,
    dont_follow: &Option<BTreeSet<Oid>>,
    fks: &BTreeMap<Oid, Vec<Oid>>,
    rfks: &BTreeMap<Oid, Vec<Oid>>,
    fuel: u32,
    initial: Oid,
) {
    visible.insert(initial);
    if fuel > 0 {
        let mut queue: VecDeque<(Oid, u32)> = VecDeque::new();
        queue.push_back((initial, fuel));
        while let Some((oid, fuel)) = queue.pop_front() {
            if dont_follow.as_ref().and_then(|x| x.get(&oid)).is_none() {
                let fuel = fuel - 1;
                if let Some(targets) = fks.get(&oid) {
                    for tgt in targets {
                        let newly_inserted = visible.insert(*tgt);
                        if newly_inserted && fuel > 0 {
                            queue.push_back((*tgt, fuel));
                        }
                    }
                }
                if let Some(sources) = rfks.get(&oid) {
                    for src in sources {
                        let newly_inserted = visible.insert(*src);
                        if newly_inserted && fuel > 0 {
                            queue.push_back((*src, fuel));
                        }
                    }
                }
            }
        }
    }
}
