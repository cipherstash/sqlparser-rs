// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use crate::{ast::{Format, Statement}, dialect::Dialect, keywords::Keyword, parser::{Parser, ParserError}, tokenizer::Token};

// A [`Dialect`] for [ClickHouse](https://clickhouse.com/).
#[derive(Debug)]
pub struct ClickHouseDialect {}

impl Dialect for ClickHouseDialect {
    fn is_identifier_start(&self, ch: char) -> bool {
        // See https://clickhouse.com/docs/en/sql-reference/syntax/#syntax-identifiers
        ch.is_ascii_lowercase() || ch.is_ascii_uppercase() || ch == '_'
    }

    fn is_identifier_part(&self, ch: char) -> bool {
        self.is_identifier_start(ch) || ch.is_ascii_digit()
    }

    fn parse_statement(&self, parser: &mut Parser) -> Option<Result<Statement, ParserError>> {
        Some(parse_statement_with_format(parser))
    }
}

fn parse_statement_with_format(parser: &mut Parser) -> Result<Statement, ParserError> {
    let statement = parser.parse_statement_inner()?;
    // TODO: Just use parse_format and check output/input for the different statements
    if parser.parse_keyword(Keyword::FORMAT) {
        let format = parse_format_for_output(parser)?;
        Ok(set_format(statement, format))
    } else {
        Ok(statement)
    }
}

fn set_format(statement: Statement, target: Format) -> Statement {
    match statement {
        Statement::Query(mut query) => {
            query.format = Some(target);
            Statement::Query(query)
        }
        Statement::ShowTables { format: _, full, db_name, filter, extended } => Statement::ShowTables {
            format: Some(target),
            filter,
            full,
            db_name,
            extended,
        },
        _ => statement,
    }
}

 fn parse_format_for_output(parser: &mut Parser) -> Result<Format, ParserError> {
    let format = parse_format(parser)?;
    if format.valid_for_output() {
        Ok(format)
    } else {
        Err(ParserError::ParserError(format!("{} is not a valid output format", format)))
    }
}

// Parse Clickhouse format clause
fn parse_format(parser: &mut Parser) -> Result<Format, ParserError> {
    if let Token::Word(word) = parser.next_token().token {
        match word.value.as_str() {
            "TabSeparated" => Ok(Format::TabSeparated),
            "TabSeparatedRaw" => Ok(Format::TabSeparatedRaw),
            "TabSeparatedWithNames" => Ok(Format::TabSeparatedWithNames),
            "TabSeparatedWithNamesAndTypes" => Ok(Format::TabSeparatedWithNamesAndTypes),
            "TabSeparatedRawWithNames" => Ok(Format::TabSeparatedRawWithNames),
            "TabSeparatedRawWithNamesAndTypes" => Ok(Format::TabSeparatedRawWithNamesAndTypes),
            "Template" => Ok(Format::Template),
            "TemplateIgnoreSpaces" => Ok(Format::TemplateIgnoreSpaces),
            "CSV" => Ok(Format::CSV),
            "CSVWithNames" => Ok(Format::CSVWithNames),
            "CSVWithNamesAndTypes" => Ok(Format::CSVWithNamesAndTypes),
            "CustomSeparated" => Ok(Format::CustomSeparated),
            "CustomSeparatedWithNames" => Ok(Format::CustomSeparatedWithNames),
            "CustomSeparatedWithNamesAndTypes" => Ok(Format::CustomSeparatedWithNamesAndTypes),
            "SQLInsert" => Ok(Format::SQLInsert),
            "Values" => Ok(Format::Values),
            "Vertical" => Ok(Format::Vertical),
            "JSON" => Ok(Format::JSON),
            "JSONAsString" => Ok(Format::JSONAsString),
            "JSONStrings" => Ok(Format::JSONStrings),
            "JSONColumns" => Ok(Format::JSONColumns),
            "JSONColumnsWithMetadata" => Ok(Format::JSONColumnsWithMetadata),
            "JSONCompact" => Ok(Format::JSONCompact),
            "JSONCompactStrings" => Ok(Format::JSONCompactStrings),
            "JSONCompactColumns" => Ok(Format::JSONCompactColumns),
            "JSONEachRow" => Ok(Format::JSONEachRow),
            "PrettyJSONEachRow" => Ok(Format::PrettyJSONEachRow),
            "JSONEachRowWithProgress" => Ok(Format::JSONEachRowWithProgress),
            "JSONStringsEachRow" => Ok(Format::JSONStringsEachRow),
            "JSONStringsEachRowWithProgress" => Ok(Format::JSONStringsEachRowWithProgress),
            "JSONCompactEachRow" => Ok(Format::JSONCompactEachRow),
            "JSONCompactEachRowWithNames" => Ok(Format::JSONCompactEachRowWithNames), 
            "JSONCompactEachRowWithNamesAndTypes" => Ok(Format::JSONCompactEachRowWithNamesAndTypes),
            "JSONCompactStringsEachRow" => Ok(Format::JSONCompactStringsEachRow),
            "JSONCompactStringsEachRowWithNames" => Ok(Format::JSONCompactStringsEachRowWithNames),
            "JSONCompactStringsEachRowWithNamesAndTypes" => Ok(Format::JSONCompactStringsEachRowWithNamesAndTypes),
            "JSONObjectEachRow" => Ok(Format::JSONObjectEachRow),
            "BSONEachRow" => Ok(Format::BSONEachRow),
            "TSKV" => Ok(Format::TSKV),
            "Pretty" => Ok(Format::Pretty),
            "PrettyNoEscapes" => Ok(Format::PrettyNoEscapes),
            "PrettyMonoBlock" => Ok(Format::PrettyMonoBlock),
            "PrettyNoEscapesMonoBlock" => Ok(Format::PrettyNoEscapesMonoBlock),
            "PrettyCompact" => Ok(Format::PrettyCompact),
            "PrettyCompactNoEscapes" => Ok(Format::PrettyCompactNoEscapes),
            "PrettyCompactMonoBlock" => Ok(Format::PrettyCompactMonoBlock),
            "PrettyCompactNoEscapesMonoBlock" => Ok(Format::PrettyCompactNoEscapesMonoBlock),
            "PrettySpace" => Ok(Format::PrettySpace),
            "PrettySpaceNoEscapes" => Ok(Format::PrettySpaceNoEscapes),
            "PrettySpaceMonoBlock" => Ok(Format::PrettySpaceMonoBlock),
            "PrettySpaceNoEscapesMonoBlock" => Ok(Format::PrettySpaceNoEscapesMonoBlock),
            "Prometheus" => Ok(Format::Prometheus),
            "Protobuf" => Ok(Format::Protobuf),
            "ProtobufSingle" => Ok(Format::ProtobufSingle),
            "Avro" => Ok(Format::Avro),
            "AvroConfluent" => Ok(Format::AvroConfluent),
            "Parquet" => Ok(Format::Parquet),
            "ParquetMetadata" => Ok(Format::ParquetMetadata),
            "Arrow" => Ok(Format::Arrow),
            "ArrowStream" => Ok(Format::ArrowStream),
            "ORC" => Ok(Format::ORC),
            "One" => Ok(Format::One),
            "Npy" => Ok(Format::Npy),
            "RowBinary" => Ok(Format::RowBinary),
            "RowBinaryWithNames" => Ok(Format::RowBinaryWithNames),
            "RowBinaryWithNamesAndTypes" => Ok(Format::RowBinaryWithNamesAndTypes),
            "RowBinaryWithDefaults" => Ok(Format::RowBinaryWithDefaults),
            "Native" => Ok(Format::Native),
            "Null" => Ok(Format::Null),
            "XML" => Ok(Format::XML),
            "CapnProto" => Ok(Format::CapnProto),
            "LineAsString" => Ok(Format::LineAsString),
            "Regexp" => Ok(Format::Regexp),
            "RawBLOB" => Ok(Format::RawBLOB),
            "MsgPack" => Ok(Format::MsgPack),
            "MySQLDump" => Ok(Format::MySQLDump),
            "DWARF" => Ok(Format::DWARF),
            "Markdown" => Ok(Format::Markdown),
            _ => {
                return Err(ParserError::ParserError(
                    format!("Unknown FORMAT {}", word.value),
                ));
            }
        }
    } else {
        return Err(ParserError::ParserError(
            "Expected FORMAT identifier".to_string(),
        ));
    }
}