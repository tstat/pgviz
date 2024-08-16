pub mod parser;
use super::*;
use std::{
    collections::BTreeMap,
    ops::{BitAnd, BitOr},
};

pub(super) async fn apply<'a, 'b>(
    transaction: &Transaction<'a>,
    _tables: &BTreeMap<Oid, Table>,
    interpret_queue: Vec<Eval<'b>>,
    dont_follow: &Option<BTreeSet<Oid>>,
    fks: &BTreeMap<Oid, Vec<Oid>>,
    rfks: &BTreeMap<Oid, Vec<Oid>>,
) -> Result<Graph, tokio_postgres::Error> {
    interpret(
        transaction,
        _tables,
        interpret_queue,
        dont_follow,
        fks,
        rfks,
    )
    .await
    .map(|x| x.graph())
}

pub(super) async fn run_query<'a, 'b>(
    transaction: &Transaction<'a>,
    _tables: &BTreeMap<Oid, Table>,
    interpret_queue: Vec<Eval<'b>>,
    dont_follow: &Option<BTreeSet<Oid>>,
    fks: &BTreeMap<Oid, Vec<Oid>>,
    rfks: &BTreeMap<Oid, Vec<Oid>>,
) -> Result<BTreeSet<u32>, tokio_postgres::Error> {
    interpret(
        transaction,
        _tables,
        interpret_queue,
        dont_follow,
        fks,
        rfks,
    )
    .await
    .map(|x| x.oids())
}

async fn interpret<'a, 'b>(
    transaction: &Transaction<'a>,
    _tables: &BTreeMap<Oid, Table>,
    mut interpret_queue: Vec<Eval<'b>>,
    dont_follow: &Option<BTreeSet<Oid>>,
    fks: &BTreeMap<Oid, Vec<Oid>>,
    rfks: &BTreeMap<Oid, Vec<Oid>>,
) -> Result<Value<'b>, tokio_postgres::Error> {
    let visible = {
        let mut stack: Vec<Value<'b>> = Vec::new();

        while let Some(eval_elem) = interpret_queue.pop() {
            match eval_elem {
                Eval::Within => {
                    let num_edges = stack.pop().unwrap().natural();
                    let oids = stack.pop().unwrap().oids();
                    let mut visible = BTreeSet::new();
                    for oid in oids {
                        bfs(&mut visible, dont_follow, fks, rfks, num_edges, oid);
                    }
                    stack.push(Value::Oids(visible));
                }
                Eval::Subgraph => {
                    let label = stack.pop().unwrap().string();
                    let graph: Graph = stack.pop().unwrap().graph();
                    stack.push(Value::Subgraph(Subgraph {
                        label: label.to_string(),
                        graph,
                    }));
                }
                Eval::Graph { subgraph_count } => {
                    let nodes = stack.pop().unwrap().oids();
                    let subgraphs: Vec<Subgraph> = stack
                        .split_off(stack.len() - subgraph_count)
                        .into_iter()
                        .map(|x| x.subgraph())
                        .collect();
                    stack.push(Value::Graph(Graph { nodes, subgraphs }));
                }
                Eval::Table { arg_count } => {
                    let tables: Vec<&'b str> = stack
                        .split_off(stack.len() - arg_count)
                        .into_iter()
                        .map(|x| x.string())
                        .collect();
                    let oids = get_matching_tables(transaction, &tables)
                        .await?
                        .collect()
                        .await;
                    stack.push(Value::Oids(oids));
                }
                Eval::Schema { arg_count } => {
                    let schemas: Vec<&'b str> = stack
                        .split_off(stack.len() - arg_count)
                        .into_iter()
                        .map(|x| x.string())
                        .collect();
                    let oids = get_matching_schemas(transaction, &schemas)
                        .await?
                        .collect()
                        .await;
                    stack.push(Value::Oids(oids));
                }
                Eval::Difference => {
                    let x = stack.pop().unwrap().oids();
                    let y = stack.pop().unwrap().oids();
                    stack.push(Value::Oids(&x - &y));
                }
                Eval::And => {
                    let x = stack.pop().unwrap().oids();
                    let y = stack.pop().unwrap().oids();
                    stack.push(Value::Oids(x.bitand(&y)));
                }
                Eval::Or => {
                    let x = stack.pop().unwrap().oids();
                    let y = stack.pop().unwrap().oids();
                    stack.push(Value::Oids(x.bitor(&y)));
                }
                Eval::Literal(x) => match x {
                    Literal::String(x) => stack.push(Value::String(x)),
                    Literal::Natural(x) => stack.push(Value::Natural(x)),
                },
            }
        }
        stack.pop().unwrap()
    };

    Ok(visible)
}

#[derive(Debug)]
enum Value<'a> {
    String(&'a str),
    Natural(u32),
    Oids(BTreeSet<u32>),
    Subgraph(Subgraph),
    Graph(Graph),
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

    fn subgraph(self) -> Subgraph {
        match self {
            Value::Subgraph(x) => x,
            x => panic!("Expected a subgraph but got: {x:?}"),
        }
    }

    fn graph(self) -> Graph {
        match self {
            Value::Graph(x) => x,
            x => panic!("Expected a graph but got: {x:?}"),
        }
    }
}

#[derive(Debug)]
pub enum Eval<'a> {
    And,
    Or,
    Difference,
    Within,
    Table { arg_count: usize },
    Schema { arg_count: usize },
    Subgraph,
    Graph { subgraph_count: usize },
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
