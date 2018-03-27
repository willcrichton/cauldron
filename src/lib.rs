#![feature(rustc_private, quote, proc_macro, proc_macro_internals, pattern_parentheses, stmt_expr_attributes)]

extern crate proc_macro;
extern crate rustc;
extern crate rustc_errors;
extern crate rustc_metadata;
extern crate rustc_driver;
extern crate rustc_trans_utils;
extern crate rustc_resolve;
extern crate rustc_incremental;
extern crate rustc_typeck as typeck;
extern crate syntax_pos;
extern crate syntax;
extern crate simple_bind;

use std::sync::mpsc;
use std::rc::Rc;
use std::path::PathBuf;

use rustc::{ty, hir, session};
use rustc_errors::registry::Registry;
use rustc_metadata::cstore::CStore;
use rustc_driver::driver;
use rustc_resolve::MakeGlobMap;
use syntax::{ext, ast};
use syntax_pos::DUMMY_SP;
use syntax::ptr::P;
use syntax::print::pprust;
use syntax::ext::quote::rt::ToTokens;
use syntax::{tokenstream, parse};
use syntax::parse::token;
use syntax_pos::symbol::Ident;
use ty::TypeVariants;

use simple_bind::bind;


use tokenstream::TokenTree;

struct SliceWrapper<T>(pub Vec<T>);

macro_rules! impl_to_tokens_slice {
  ($t: ty, $sep: expr) => {
    impl ToTokens for SliceWrapper<$t> {
      fn to_tokens(&self, cx: &ext::base::ExtCtxt) -> Vec<TokenTree> {
        let mut v = vec![];
        for (i, x) in self.0.iter().enumerate() {
          if i > 0 {
            v.extend_from_slice(&$sep);
          }
          v.extend(x.to_tokens(cx));
        }
        v
      }
    }
  };
}

impl_to_tokens_slice! { P<ast::Pat>, [TokenTree::Token(DUMMY_SP, token::Comma)] }
impl_to_tokens_slice! { P<ast::Expr>, [TokenTree::Token(DUMMY_SP, token::Comma)] }
impl_to_tokens_slice! { P<ast::Ty>, [TokenTree::Token(DUMMY_SP, token::Comma)] }


// NOTE: if you are seeing "error[E0463]: can't find crate for `std`"
// then you need to change this SYSROOT to your specific machine's.
static SYSROOT: &'static str = "/Users/will/.rustup/toolchains/nightly-x86_64-apple-darwin";

trait Folder<'a> {
  fn fold_expr(&self, expr: &'a hir::Expr) -> P<ast::Expr>;
  fn fold_stmt(&self, stmt: &'a hir::Stmt) -> ast::Stmt;
  fn fold_pat(&self, stmt: &'a P<hir::Pat>) -> P<ast::Pat>;
}

struct TestFolder<'a, 'tcx: 'a, 'ecx: 'a> {
  ecx: &'a ext::base::ExtCtxt<'ecx>,
  tcx: ty::TyCtxt<'a, 'tcx, 'tcx>,
}


impl<'a, 'tcx: 'a, 'ecx> TestFolder<'a, 'tcx, 'ecx> {
  fn get_type(&self, expr: &'tcx hir::Expr) -> &'tcx ty::TyS<'tcx> {
    let table = self.tcx.typeck_tables_of(expr.hir_id.owner_def_id());
    table.node_id_to_type(expr.hir_id)
  }

  fn convert_type(&self, ty: &'tcx ty::TyS<'tcx>) -> P<ast::Ty> {
    use ast::{IntTy, FloatTy};
    let ty = match ty.sty {
      TypeVariants::TyInt(intty) => {
        match intty {
          IntTy::I32 => quote_ty!(&self.ecx, i32),
          _ => panic!("IntTy: Not yet implemented"),
        }
      }
      TypeVariants::TyFloat(floatty) => {
        match floatty {
          FloatTy::F64 => quote_ty!(&self.ecx, f64),
          _ => panic!("FloatTy: Not yet implemented"),
        }
      }
      TypeVariants::TyClosure(defid, closurety) => {
        let fnty = closurety.closure_sig_ty(defid, self.tcx);
        match fnty.sty {
          TypeVariants::TyFnPtr(sig) => {
            let inputs: Vec<ast::Ty> = sig.inputs().skip_binder().iter()
              .map(|arg| self.convert_type(arg).into_inner()).collect();
            let inputs: &[ast::Ty] = &inputs;
            let output = self.convert_type(sig.output().skip_binder());
            quote_ty!(&self.ecx, Box<Fn($inputs) -> $output>)
          }
          _ => panic!("NO")
        }
      }
      TypeVariants::TyTuple(tys) => {
        let tys: Vec<ast::Ty> =
          tys.iter().map(|arg| self.convert_type(arg).into_inner()).collect();
        let tys: &[ast::Ty] = &tys;
        quote_ty!(&self.ecx, ($tys))
      }
      _ => panic!("TypeVariants: Not yet implemented for {:?}", ty),
    };
    quote_ty!(&self.ecx, Rc<$ty>)
  }

  fn unwrap_ty_container(&self, ty: &P<ast::Ty>) -> P<ast::Ty> {
    bind!{let ast::TyKind::Path(_, ref segs) = ty.node;}
    let seg = &segs.segments[0];
    bind!{let Some(ref segparams) = seg.parameters;}
    bind!{let ast::PathParameters::AngleBracketed(ref params) = **segparams;}
    params.types[0].clone()
  }

  fn unwrap_ty_function(&self, ty: &P<ast::Ty>) -> (P<ast::Ty>, P<ast::Ty>) {
    bind!{let ast::TyKind::Path(_, ref segs) = ty.node;}
    let seg = &segs.segments[0];
    bind!{let Some(ref segparams) = seg.parameters;}
    bind!{let ast::PathParameters::Parenthesized(ref params) = **segparams;}
    (params.inputs.clone().into_iter().nth(0).unwrap(),
     params.output.clone().unwrap_or_else(|| quote_ty!(&self.ecx, ())))
  }
}

impl<'a, 'tcx: 'a, 'ecx> Folder<'tcx> for TestFolder<'a, 'tcx, 'ecx> {
  fn fold_expr(&self, expr: &'tcx hir::Expr) -> P<ast::Expr> {
    use hir::{Expr_, BinOp_, QPath};
    let new_expr = match expr.node {
      Expr_::ExprLit(ref lit) => quote_expr!(&self.ecx, $lit),

      Expr_::ExprPath(QPath::Resolved(_, ref path)) => {
        let ident = Ident::with_empty_ctxt(path.segments[0].name);
        return quote_expr!(&self.ecx, $ident.clone());
      }

      Expr_::ExprBinary(ref binop, ref e1, ref e2) => {
        let e1 = self.fold_expr(e1);
        let e2 = self.fold_expr(e2);
        match binop.node {
          BinOp_::BiAdd => quote_expr!(&self.ecx, *$e1 + *$e2),
          BinOp_::BiEq => quote_expr!(&self.ecx, *$e1 == *$e2),
          _ => panic!("ExprBinary: Not yet implemented"),
        }
      }

      Expr_::ExprIf(ref cond, ref then, ref else_) => {
        let cond = self.fold_expr(cond);
        let then = self.fold_expr(then);
        match else_ {
          &Some(ref else_) => {
            let else_ = self.fold_expr(else_);
            quote_expr!(&self.ecx, if *$cond { *$then } else { *$else_ })
          }
          &None => quote_expr!(&self.ecx, if *$cond { *$then }),
        }
      }

      Expr_::ExprBlock(ref block) => {
        let new_block: Vec<ast::Stmt> = block
          .stmts
          .iter()
          .map(|stmt| self.fold_stmt(stmt))
          .collect();
        match block.expr {
          Some(ref expr) => {
            let expr = self.fold_expr(expr);
            quote_expr!(&self.ecx, {$new_block; *$expr})
          }
          None => quote_expr!(&self.ecx, {$new_block}),
        }
      }

      Expr_::ExprCall(ref func, ref args) => {
        let func = self.fold_expr(func);
        let args: Vec<P<ast::Expr>> =
          args.iter().map(|arg| self.fold_expr(arg)).collect();
        let args = SliceWrapper(args);
        quote_expr!(&self.ecx, *(*$func)(Rc::new(($args))))
      }

      Expr_::ExprClosure(_, _, body_id, _, _) => {
        let krate = &self.tcx.hir.krate();
        let body = krate.body(body_id);
        let ty = self.convert_type(self.get_type(expr));
        let fnty = self.unwrap_ty_container(&self.unwrap_ty_container(&ty));
        let (input_ty, output_ty) = self.unwrap_ty_function(&fnty);
        let input_tup = self.unwrap_ty_container(&input_ty);

        let args: Vec<P<ast::Pat>> =
          body.arguments.iter()
          .map(|arg| {
            let pat = self.fold_pat(&arg.pat);
            bind!{let ast::PatKind::Ident(_, ref ident, _) = pat.node;}
            let mut pat = pat.clone().into_inner();;
            pat.node = ast::PatKind::Ident(
              ast::BindingMode::ByRef(ast::Mutability::Immutable),
              ident.clone(),
              None);
            P(pat)
          })
          .collect();
        let args: SliceWrapper<P<ast::Pat>> = SliceWrapper(args);
        let body_expr = self.fold_expr(&body.value);
        let arg = quote_pat!(&self.ecx, ($args));

        quote_expr!(
          &self.ecx,
          Box::new(|__arg : $input_ty| -> $output_ty {
            let $arg : ($input_tup) = *__arg;
            $body_expr
          }))
      }
      _ => panic!("fold_expr: Not yet implemented"),
    };
    quote_expr!(&self.ecx, Rc::new($new_expr))
  }

  fn fold_stmt(&self, stmt: &'tcx hir::Stmt) -> ast::Stmt {
    use hir::{Stmt_, Decl_, PatKind};
    match stmt.node {
      Stmt_::StmtDecl(ref decl, _) => {
        match decl.node {
          Decl_::DeclLocal(ref local) => {
            let name = if let PatKind::Binding(_, _, ref name, _) = local.pat.node {
              Ident::with_empty_ctxt(name.node)
            } else {
              panic!("PatKind: Not yet implemented")
            };

            let expr = &local.init.iter().next().unwrap();
            let ty = self.convert_type(self.get_type(expr));

            let expr = self.fold_expr(expr);
            quote_stmt!(&self.ecx, let $name: $ty = $expr;).unwrap()
          }
          _ => panic!("Decl: Not yet implemented"),
        }
      }
      _ => panic!("fold_stmt: Not yet implemented"),
    }
  }

  fn fold_pat(&self, pat: &'tcx P<hir::Pat>) -> P<ast::Pat> {
    use hir::PatKind;
    match pat.node {
      PatKind::Binding(_, _, ref name, _) => {
        let ident = Ident::with_empty_ctxt(name.node);
        quote_pat!(&self.ecx, $ident)
      },
      _ => panic!("fold_pat: Not yet implemented")
    }
  }
}


#[allow(unused_variables)]
#[proc_macro]
pub fn auto_gc(ts: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let codemap = syntax::codemap::CodeMap::new(syntax::codemap::FilePathMapping::empty());
  codemap.new_filemap(syntax_pos::FileName::Custom("".to_string()), "".to_string());

  let mut opts = session::config::basic_options();
  opts.maybe_sysroot = Some(PathBuf::from(SYSROOT));
  let sess =
    session::build_session_with_codemap(opts, None, Registry::new(&[]), Rc::new(codemap), None);

  let cstore = CStore::new(
    rustc_trans_utils::trans_crate::MetadataOnlyTransCrate::new().metadata_loader(),
  );

  let mut resolver = ext::base::DummyResolver;
  let ecx = ext::base::ExtCtxt::new(
    &sess.parse_sess,
    ext::expand::ExpansionConfig::default("".to_string()),
    &mut resolver,
  );

  let ts = proc_macro::__internal::token_stream_inner(ts);
  let mut parser = parse::stream_to_parser(&sess.parse_sess, ts);
  let mut stmts = Vec::new();
  while let Some(stmt) = parser.parse_full_stmt(false).unwrap() {
    stmts.push(stmt);

    if parser.token == parse::token::Token::Eof {
      break;
    }
  }

  let krate = ast::Crate {
    module: ast::Mod {
      inner: DUMMY_SP,
      items: vec![
        quote_item!(&ecx, fn main(){$stmts}).unwrap()
      ],
    },
    attrs: vec![],
    span: DUMMY_SP,
  };

  let driver::ExpansionResult {
    expanded_crate,
    defs,
    analysis,
    resolutions,
    mut hir_forest,
  } = driver::phase_2_configure_and_expand(
    &sess,
    &cstore,
    krate,
    None,
    "test",
    None,
    MakeGlobMap::No,
    |_| Ok(()),
  ).unwrap();

  let arenas = ty::AllArenas::new();

  let hir_map = hir::map::map_crate(&sess, &cstore, &mut hir_forest, &defs);

  let mut local_providers = ty::maps::Providers::default();
  driver::default_provide(&mut local_providers);

  let mut extern_providers = local_providers;
  driver::default_provide_extern(&mut extern_providers);

  let query_result_on_disk_cache = rustc_incremental::load_query_result_cache(&sess);

  let (tx, _rx) = mpsc::channel();

  let input = session::config::Input::Str {
    name: syntax_pos::FileName::Custom("".to_string()),
    input: "".to_string(),
  };
  let output_filenames = driver::build_output_filenames(&input, &None, &None, &[], &sess);

  ty::TyCtxt::create_and_enter(&sess, &cstore, local_providers, extern_providers, &arenas, resolutions, hir_map, query_result_on_disk_cache, "", tx, &output_filenames, |tcx| {
    typeck::check_crate(tcx).expect("typeck failure");
    let folder = TestFolder {ecx: &ecx, tcx: tcx};
    let krate = &tcx.hir.krate();
    let ts: tokenstream::TokenStream =
      match krate.body(krate.body_ids[0]).value.node {
        hir::Expr_::ExprBlock(ref block) => block
          .stmts.iter()
          .flat_map(|stmt| folder.fold_stmt(stmt).to_tokens(&ecx))
          .collect(),
        _ => unreachable!()
      };
    println!("GENERATED: {}", pprust::tokens_to_string(ts.clone()));
    proc_macro::__internal::token_stream_wrap(ts)
  })
}
