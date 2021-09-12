/*!
Intermadiate representations between AST and bytecode

# Data flow

* macro expansion: ?
* name resolution: AST → ItemTree → ?
* type inference: ?

# TODO

* Source map pattern (AstID → AST)
*/

pub mod db;

pub mod data;
pub mod lower;
