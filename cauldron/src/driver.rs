use proc_macro;
use rustc;
use getopts;
use rustc_errors;
use rustc::{hir, session};
use rustc_driver;
use rustc_trans_utils;
use rustc_metadata;
use syntax::{ext, ast, tokenstream};
use syntax::ext::quote::rt::ToTokens;
use syntax::codemap::FileLoader;
use rustc_driver::{CompilerCalls, Compilation};
use rustc_driver::driver::{CompileController, CompileState};
use rustc::session::config::Input;
use rustc::middle::cstore::CrateStore;
use rustc::hir::def_id::DefId;

use std::path::Path;
use std::io;
use std::path::PathBuf;
use std::cell::RefCell;
use std::rc::Rc;
use std::str::FromStr;

use trans;
use trans::Folder;

// NOTE: if you are seeing "error[E0463]: can't find crate for `std`"
// then you need to change this SYSROOT to your specific machine's.
// static SYSROOT: &'static str = "/Users/will/.rustup/toolchains/nightly-x86_64-apple-darwin";
static SYSROOT: &'static str = "/Users/will/Code/rust/build/x86_64-apple-darwin/stage2";

struct CauldronCompilerCalls {
  pub output: Rc<RefCell<Option<String>>>
}

struct StringLoader {
  pub string: String
}

impl FileLoader for StringLoader {
  fn file_exists(&self, _path: &Path) -> bool { true }
  fn abs_path(&self, _path: &Path) -> Option<PathBuf> { None }
  fn read_file(&self, _path: &Path) -> io::Result<String> { Ok(self.string.clone()) }
}

impl<'a> CompilerCalls<'a> for CauldronCompilerCalls {
  fn early_callback(
    &mut self,
    _matches: &getopts::Matches,
    _: &session::config::Options,
    _: &ast::CrateConfig,
    _: &rustc_errors::registry::Registry,
    _: session::config::ErrorOutputType,
  ) -> Compilation {
    Compilation::Continue
  }

  fn late_callback(
    &mut self,
    _trans: &rustc_trans_utils::trans_crate::TransCrate,
    _matches: &getopts::Matches,
    _sess: &rustc::session::Session,
    _cstore: &rustc::middle::cstore::CrateStore,
    _input: &Input,
    _odir: &Option<PathBuf>,
    _ofile: &Option<PathBuf>,
  ) -> Compilation {
    Compilation::Continue
  }

  fn no_input(
    &mut self,
    _matches: &getopts::Matches,
    _: &rustc::session::config::Options,
    _: &ast::CrateConfig,
    _: &Option<PathBuf>,
    _: &Option<PathBuf>,
    _: &rustc_errors::registry::Registry,
  ) -> Option<(Input, Option<PathBuf>)> {
    None
  }

  fn build_controller(
    &mut self,
    session: &rustc::session::Session,
    _matches: &getopts::Matches,
  ) -> CompileController<'a> {
    let mut controller = CompileController::basic();
    controller.after_analysis.stop = Compilation::Stop;

    let output = self.output.clone();
    controller.after_analysis.callback = box move |state: &mut CompileState| {
      let tcx = &state.tcx.expect("No tcx");
      let mut resolver = ext::base::DummyResolver;
      let ecx = ext::base::ExtCtxt::new(
        &state.session.parse_sess,
        ext::expand::ExpansionConfig::default("".to_string()),
        &mut resolver,
      );
      let folder = trans::TestFolder {ecx: &ecx, tcx: tcx};
      let krate = &tcx.hir.krate();

      let std_crate_num = tcx.crates().iter().find(|crate_num| {
        tcx.crate_name(**crate_num) == "cauldron_std"
      }).unwrap().clone();

      let meta_any = tcx.crate_data_as_rc_any(std_crate_num);
      let crate_metadata = meta_any.downcast_ref::<rustc_metadata::cstore::CrateMetadata>().unwrap();
      let (def_index, _) =
        crate_metadata.root.lang_items.decode(crate_metadata).next().unwrap();
      let def_id = DefId {
        krate: std_crate_num,
        index: def_index
      };

      println!("{:?}", tcx.type_of(def_id));

      let ts: tokenstream::TokenStream =
        match krate.body(krate.body_ids[0]).value.node {
          hir::Expr_::ExprBlock(ref block) => block
            .stmts.iter()
            .flat_map(|stmt| folder.fold_stmt(stmt).to_tokens(&ecx))
            .collect(),
          _ => unreachable!()
        };
      *output.borrow_mut() = Some(format!("{}", ts));
    };

    controller
  }
}

#[allow(unused_variables)]
pub fn auto_gc(ts: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = format!("extern crate cauldron_std; fn main() {{ {} }}", ts);

  let output = rustc_driver::in_rustc_thread(move || {
    let mut callbacks = CauldronCompilerCalls { output: Rc::new(RefCell::new(None)) };
    let args: Vec<String> = format!("_ _ --sysroot {} --cap-lints allow -L ../cauldron-std/target/debug/ -l cauldron_std", SYSROOT)
      .split(' ').map(|s| s.to_string()).collect();
    let loader = StringLoader { string: input };
    let (result, _) = rustc_driver::run_compiler(&args, &mut callbacks, Some(Box::new(loader)), None);
    println!("result {:?}", result);
    let output = callbacks.output.borrow();
    format!("{}", output.clone().expect("No output"))
  }).expect("in_rustc_thread failed");

  proc_macro::TokenStream::from_str(&output).expect("Failed to parse back to TokenStream")
}
