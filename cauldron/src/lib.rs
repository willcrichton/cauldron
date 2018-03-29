#![feature(
  rustc_private,
  quote,
  proc_macro,
  proc_macro_internals,
  pattern_parentheses,
  stmt_expr_attributes,
  box_syntax)]

// // for Rc
// #![feature(
//   lang_items,
//   box_syntax,
//   optin_builtin_traits,
//   dropck_eyepatch,
//   specialization,
//   generic_param_attrs,
//   alloc,
//   core_intrinsics,
//   unsize,
//   coerce_unsized,
//   allocator_api)]

extern crate proc_macro;
extern crate rustc;
extern crate rustc_errors;
extern crate rustc_metadata;
extern crate rustc_driver;
extern crate rustc_trans_utils;
extern crate rustc_resolve;
extern crate rustc_incremental;
extern crate rustc_typeck;
extern crate syntax_pos;
extern crate syntax;
extern crate getopts;

extern crate simple_bind;
// // for Rc
// extern crate core;
// extern crate alloc;
// mod rc;

mod trans;
mod driver;

#[proc_macro]
pub fn auto_gc(ts: proc_macro::TokenStream) -> proc_macro::TokenStream {
  driver::auto_gc(ts)
}
