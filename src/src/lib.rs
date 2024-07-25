pub mod args;
pub mod filter;

use futures::{Stream, StreamExt};
use postgres_types::Oid;
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::{fmt::Write as FmtWrite, io};
use tokio::{self};
use tokio_postgres::{self, IsolationLevel, NoTls, Transaction};

#[derive(Debug)]
pub struct Schema {
    pub oid: Oid,
    pub name: String,
}

#[derive(Debug)]
pub struct Table {
    pub schema_oid: Oid,
    pub oid: Oid,
    pub name: String,
    pub columns: BTreeMap<i16, Column>,
    pub composite_unique_constraints: Vec<Vec<i16>>,
}

#[derive(Debug)]
pub struct Column {
    pub num: i16,
    pub name: String,
    pub typ: String,
    pub not_null: bool,
    pub column_constraint: Option<ColumnConstraint>,
}

#[derive(Debug, Copy, Clone)]
pub enum ColumnConstraint {
    PrimaryKey,
    Unique,
}

#[derive(Debug)]
pub enum MutType {
    NoAction,
    Restrict,
    Cascade,
    SetNull,
    SetDefault,
}

#[derive(Debug)]
pub struct ForeignKeyConstraint {
    pub source: Oid,
    pub source_cols: Vec<i16>,
    pub target: Oid,
    pub target_cols: Vec<i16>,
    pub update_action: MutType,
    pub delete_action: MutType,
}

pub struct Db {
    pub inner: BTreeMap<Oid, Table>,
}

pub async fn write_graph<'a, W: io::Write>(
    conn_str: &str,
    dont_follow: &Option<String>,
    edge_labels: bool,
    filter: Vec<filter::Eval<'a>>,
    output: &mut W,
) -> Result<(), tokio_postgres::Error> {
    // Connect to the database.
    let (mut client, connection) = tokio_postgres::connect(conn_str, NoTls).await?;

    // The connection object performs the actual communication with the database,
    // so spawn it off to run on its own.
    tokio::spawn(async move {
        if let Err(e) = connection.await {
            eprintln!("connection error: {}", e);
        }
    });

    let transaction = client
        .build_transaction()
        .isolation_level(IsolationLevel::Serializable)
        .start()
        .await?;
    let foreign_keys = get_foreign_keys(&transaction).await?;
    let mut fk_map: BTreeMap<Oid, Vec<Oid>> = BTreeMap::new();
    let mut rfk_map: BTreeMap<Oid, Vec<Oid>> = BTreeMap::new();
    for x in &foreign_keys {
        let _ = fk_map
            .entry(x.source)
            .and_modify(|oids| oids.push(x.target))
            .or_insert(vec![x.target]);
        let _ = rfk_map
            .entry(x.target)
            .and_modify(|oids| oids.push(x.source))
            .or_insert(vec![x.source]);
    }
    let mut tables = get_tables(&transaction).await?;
    let dont_follow: Option<BTreeSet<Oid>> = match dont_follow.as_ref() {
        None => None,
        Some(s) => {
            let res = get_matching_tables(&transaction, s).await?.collect().await;
            Some(res)
        }
    };
    let visible = filter::apply(
        &transaction,
        &tables,
        filter,
        &dont_follow,
        &fk_map,
        &rfk_map,
    )
    .await?;
    for oid in &visible {
        if let Some(tbl) = tables.get_mut(oid) {
            tbl.populate_table(&transaction).await?;
        }
    }
    write_dot(output, &tables, &foreign_keys, &visible, edge_labels).unwrap();
    Ok(())
}

pub async fn get_foreign_keys<'a>(
    t: &Transaction<'a>,
) -> Result<Vec<ForeignKeyConstraint>, tokio_postgres::Error> {
    let sql: &str = concat!(
        "select conrelid, confrelid, conkey, confkey, confupdtype, confdeltype ",
        "from pg_constraint ",
        "where contype = 'f' "
    );
    let rows = t.query(sql, &[]).await?;
    fn parse_mut_type(c: i8) -> MutType {
        match c as u8 as char {
            'a' => MutType::NoAction,
            'r' => MutType::Restrict,
            'c' => MutType::Cascade,
            'n' => MutType::SetNull,
            'd' => MutType::SetDefault,
            _ => panic!("Unexpected mut type: {c}"),
        }
    }
    let res = rows
        .into_iter()
        .map(|row| ForeignKeyConstraint {
            source: row.get("conrelid"),
            source_cols: row.get("conkey"),
            target: row.get("confrelid"),
            target_cols: row.get("confkey"),
            update_action: parse_mut_type(row.get("confupdtype")),
            delete_action: parse_mut_type(row.get("confdeltype")),
        })
        .collect();
    Ok(res)
}

pub async fn get_matching_tables<'a>(
    t: &Transaction<'a>,
    table_regex: &str,
) -> Result<impl Stream<Item = Oid>, tokio_postgres::Error> {
    let sql: &str = concat!(
        "select c.oid ",
        "from pg_catalog.pg_class c ",
        "where c.relkind = any(array['r','p']) ",
        "and c.relname = $1 ",
    );
    let rows = t.query_raw(sql, &[&table_regex]).await?;
    let result = rows.map(|x| x.unwrap().get("oid"));
    Ok(result)
}

pub async fn get_matching_schemas<'a>(
    t: &Transaction<'a>,
    schema_name: &str,
) -> Result<impl Stream<Item = Oid>, tokio_postgres::Error> {
    let sql: &str = concat!(
        "select c.oid ",
        "from pg_catalog.pg_class c ",
        "where c.relkind = any(array['r','p']) ",
        "and c.relnamespace in (select oid from pg_catalog.pg_namespace where nspname = $1) ",
    );
    let rows = t.query_raw(sql, &[&schema_name]).await?;
    let result = rows.map(|x| x.unwrap().get("oid"));
    Ok(result)
}

pub async fn get_tables<'a>(
    t: &Transaction<'a>,
) -> Result<BTreeMap<Oid, Table>, tokio_postgres::Error> {
    let sql: &str = concat!(
        "select c.relnamespace, c.oid, c.relname ",
        "from pg_catalog.pg_class c ",
        "where c.relkind = any(array['r','p']) ",
    );
    let args: &[&i32; 0] = &[];
    let rows = t.query_raw(sql, args).await?;
    let mut rows = std::pin::pin!(rows);
    let mut tables: BTreeMap<Oid, Table> = BTreeMap::new();
    while let Some(row) = rows.next().await {
        let row = row?;
        let oid: Oid = row.get("oid");
        let table: Table = Table {
            schema_oid: row.get("relnamespace"),
            oid,
            name: row.get("relname"),
            columns: BTreeMap::new(),
            composite_unique_constraints: Vec::new(),
        };
        tables.insert(oid, table);
    }
    Ok(tables)
}

impl Table {
    pub async fn populate_table<'a>(
        &mut self,
        t: &Transaction<'a>,
    ) -> Result<(), tokio_postgres::Error> {
        let sql: &str = include_str!("sql/populate_table.sql");
        let rows = t.query(sql, &[&self.oid]).await?;
        for row in rows {
            let cc = row
                .get::<&str, Vec<i8>>("constraint")
                .into_iter()
                .fold(None, |b, a| match (a as u8 as char, b) {
                    (_, Some(ColumnConstraint::PrimaryKey)) => b,
                    ('p', _) => Some(ColumnConstraint::PrimaryKey),
                    ('u', _) => Some(ColumnConstraint::Unique),
                    _ => panic!("unexpected constraint char: {a}"),
                });
            let column = Column {
                num: row.get("attnum"),
                name: row.get("attname"),
                typ: row.get("atttype"),
                not_null: row.get("attnotnull"),
                column_constraint: cc,
            };
            self.columns.insert(column.num, column);
        }

        // now the composite unique constraints
        let sql: &str = include_str!("sql/table_composite_unique_constraints.sql");
        let rows = t.query(sql, &[&self.oid]).await?;
        for row in rows {
            self.composite_unique_constraints.push(row.get("conkey"));
        }
        Ok(())
    }

    pub fn write_dot<W: io::Write>(&self, f: &mut W) -> io::Result<()> {
        const PRIMARY_KEY_COLOR: &str = "#ddddff";
        const UNIQUE_COLOR: &str = "#ddffdd";
        const NULLABLE_UNIQUE_COLOR: &str = "#bbccbb";
        const NULLABLE_COLOR: &str = "#e8e8e8";
        const DEFAULT_COLOR: &str = "#ffffff";
        let mut label_string: String = String::new();
        write!(
            &mut label_string,
            "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" cellpadding=\"4\">"
        )
        .unwrap();
        write!(
            &mut label_string,
            "<tr> <td> <b> {} </b> </td> </tr>",
            self.name
        )
        .unwrap();
        for (_, col) in &self.columns {
            let bg_color: &str = match (col.not_null, col.column_constraint) {
                (_, Some(ColumnConstraint::PrimaryKey)) => PRIMARY_KEY_COLOR,
                (true, Some(ColumnConstraint::Unique)) => UNIQUE_COLOR,
                (false, Some(ColumnConstraint::Unique)) => NULLABLE_UNIQUE_COLOR,
                (false, _) => NULLABLE_COLOR,
                _ => DEFAULT_COLOR,
            };
            write!(
                &mut label_string,
                "<tr> <td bgcolor=\"{}\" port=\"f{}\" align=\"left\"> {} <font color=\"#888888\"> :: {} </font> </td> </tr>",
                bg_color, col.num, col.name, col.typ
            )
            .unwrap();
        }
        for conkeys in &self.composite_unique_constraints {
            let conkeys_named: Vec<&str> = conkeys
                .into_iter()
                .map(|x| self.columns.get(&x).unwrap().name.as_str())
                .collect();
            write!(
                &mut label_string,
                "<tr> <td bgcolor=\"{UNIQUE_COLOR}\" align=\"left\"> ({}) <font color=\"#888888\"> </font> </td> </tr>",
                conkeys_named.join(", "),
            ).unwrap();
        }
        write!(&mut label_string, "</table>").unwrap();
        write!(
            f,
            "\"{}\" [ label = <{}> shape = \"plain\"]\n",
            self.oid, &label_string
        )?;
        Ok(())
    }
}

pub fn write_dot<W: io::Write>(
    f: &mut W,
    tables: &BTreeMap<Oid, Table>,
    fks: &Vec<ForeignKeyConstraint>,
    visible: &BTreeSet<Oid>,
    edge_labels: bool,
) -> io::Result<()> {
    write!(f, "digraph g {{\n")?;
    write!(f, "graph [nodesep=\"2.0\"]\n")?;
    write!(
        f,
        "node [fontcolor=\"#000000\" fontname=\"Helvetica,sans-serif\"]\n"
    )?;
    write!(
        f,
        "edge [fontname=\"Helvetica,sans-serif\" fontsize=10.0 labeldistance=2.0]\n"
    )?;
    for oid in visible {
        if let Some(tbl) = tables.get(oid) {
            tbl.write_dot(f)?;
        }
    }
    for fk in fks {
        if visible.contains(&fk.source) && visible.contains(&fk.target) {
            match (fk.source_cols.first(), fk.target_cols.first()) {
                (Some(src_col), Some(tgt_col)) => {
                    let edge_label = if edge_labels {
                        format!("[fontname=\"Helvetica,sans-serif\" fontsize=10.0 labeldistance=2.0 label=<Update: {:?}<br/>Delete: {:?}>]", fk.update_action, fk.delete_action)
                    } else {
                        "".to_string()
                    };
                    write!(
                        f,
                        "\"{}\":f{} -> \"{}\":f{} {edge_label}\n",
                        fk.source, src_col, fk.target, tgt_col
                    )?;
                }
                _ => {
                    write!(f, "\"{}\" -> \"{}\"\n", fk.source, fk.target)?;
                }
            }
        }
    }
    write!(f, "}}\n")?;
    Ok(())
}