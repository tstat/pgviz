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
        let mut stack: Vec<BTreeSet<Oid>> = Vec::new();

        while let Some(eval_elem) = interpret_queue.pop() {
            match eval_elem {
                Eval::Within {
                    table_name,
                    num_edges,
                } => {
                    let mut visible = BTreeSet::new();
                    let oids = get_matching_tables(transaction, table_name).await?;
                    let mut oids = std::pin::pin!(oids);
                    while let Some(oid) = oids.next().await {
                        bfs(&mut visible, dont_follow, fks, rfks, num_edges, oid);
                    }
                    stack.push(visible);
                }
                Eval::Schema { schema_name } => {
                    let oids = get_matching_schemas(transaction, schema_name)
                        .await?
                        .collect()
                        .await;
                    stack.push(oids);
                }
                Eval::Difference => {
                    let y = stack.pop().unwrap();
                    let x = stack.pop().unwrap();
                    stack.push(&x - &y);
                }
                Eval::And => {
                    let y = stack.pop().unwrap();
                    let x = stack.pop().unwrap();
                    stack.push(x.bitand(&y));
                }
                Eval::Or => {
                    let y = stack.pop().unwrap();
                    let x = stack.pop().unwrap();
                    stack.push(x.bitor(&y));
                }
            }
        }
        stack.pop().unwrap()
    };

    Ok(visible)
}

#[derive(Debug)]
pub enum Eval<'a> {
    And,
    Or,
    Difference,
    Within { table_name: &'a str, num_edges: u32 },
    Schema { schema_name: &'a str },
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
