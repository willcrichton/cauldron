use rustc::{ty, hir};
use syntax::{ext, ast};
use syntax_pos::DUMMY_SP;
use syntax::ptr::P;
use syntax::ext::quote::rt::ToTokens;
use syntax::parse::token;
use syntax_pos::symbol::Ident;
use syntax::tokenstream::TokenTree;

use simple_bind::bind;

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

pub trait Folder<'a> {
  fn fold_expr(&self, expr: &'a hir::Expr) -> P<ast::Expr>;
  fn fold_stmt(&self, stmt: &'a hir::Stmt) -> ast::Stmt;
  fn fold_pat(&self, stmt: &'a P<hir::Pat>) -> P<ast::Pat>;
}

pub struct TestFolder<'a, 'tcx: 'a, 'ecx: 'a> {
  pub ecx: &'a ext::base::ExtCtxt<'ecx>,
  pub tcx: &'a ty::TyCtxt<'a, 'tcx, 'tcx>,
}

impl<'a, 'tcx: 'a, 'ecx> TestFolder<'a, 'tcx, 'ecx> {
  fn get_type(&self, expr: &'tcx hir::Expr) -> &'tcx ty::TyS<'tcx> {
    let table = self.tcx.typeck_tables_of(expr.hir_id.owner_def_id());
    table.node_id_to_type(expr.hir_id)
  }

  fn is_copyable(&self, ty: ty::Ty<'tcx>) -> bool {
    !ty.moves_by_default(*self.tcx, ty::ParamEnv::empty(), DUMMY_SP)
  }

  fn hirty_to_astty(&self, ty: ty::Ty<'tcx>) -> P<ast::Ty> {
    use syntax::ast::{IntTy, FloatTy};
    use rustc::ty::TypeVariants;
    match ty.sty {
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

      TypeVariants::TyBool => quote_ty!(&self.ecx, bool),

      TypeVariants::TyClosure(defid, closurety) => {
        let fnty = closurety.closure_sig_ty(defid, *self.tcx);
        bind!{let TypeVariants::TyFnPtr(sig) = fnty.sty;}
        let inputs: Vec<ast::Ty> = sig.inputs().skip_binder().iter()
          .map(|arg| self.hirty_to_astty(arg).into_inner()).collect();
        let inputs: &[ast::Ty] = &inputs;
        let output = self.hirty_to_astty(sig.output().skip_binder());
        quote_ty!(&self.ecx, Rc<Box<Fn($inputs) -> $output>>)
      }

      TypeVariants::TyTuple(tys) => {
        let tys: Vec<ast::Ty> =
          tys.iter().map(|arg| self.hirty_to_astty(arg).into_inner()).collect();
        let tys: &[ast::Ty] = &tys;
        quote_ty!(&self.ecx, ($tys))
      }

      TypeVariants::TyRef(_region, ty) => {
        if let TypeVariants::TyStr = ty.ty.sty {
          quote_ty!(&self.ecx, Rc<String>)
        } else {
          panic!("TyRef: not supported")
        }
      }
      _ => panic!("TypeVariants: Not yet implemented for {:?}", ty),
    }
  }

  fn convert_type(&self, ty: ty::Ty<'tcx>) -> ty::Ty<'tcx> {
    use syntax::ast::{IntTy, FloatTy};

    // let ty = self.tcx.mk_ty(
    //   ty::TypeVariants::TyAdt());
    // ty

    panic!()

    // if self.is_copyable(ty) {
    //   match ty.sty {
    //     _ => ty.clone()
    //   }
    // else {

    // }

    // let new_ty = match ty.sty {
    //   TypeVariants::TyInt(_) | TypeVariants:: => {
    //     match intty {
    //       IntTy::I32 => quote_ty!(&self.ecx, i32),
    //       _ => panic!("IntTy: Not yet implemented"),
    //     }
    //   }

    //   TypeVariants::TyFloat(floatty) => {
    //     match floatty {
    //       FloatTy::F64 => quote_ty!(&self.ecx, f64),
    //       _ => panic!("FloatTy: Not yet implemented"),
    //     }
    //   }

    //   TypeVariants::TyBool => quote_ty!(&self.ecx, bool),

    //   TypeVariants::TyClosure(defid, closurety) => {
    //     let fnty = closurety.closure_sig_ty(defid, self.tcx);
    //     bind!{let TypeVariants::TyFnPtr(sig) = fnty.sty;}
    //     let inputs: Vec<ast::Ty> = sig.inputs().skip_binder().iter()
    //       .map(|arg| self.convert_type(arg).0.into_inner()).collect();
    //     let inputs: &[ast::Ty] = &inputs;
    //     let output = self.convert_type(sig.output().skip_binder()).0;
    //     return (quote_ty!(&self.ecx, Rc<Box<Fn($inputs) -> $output>>), false);
    //   }

    //   TypeVariants::TyTuple(tys) => {
    //     let tys: Vec<ast::Ty> =
    //       tys.iter().map(|arg| self.convert_type(arg).0.into_inner()).collect();
    //     let tys: &[ast::Ty] = &tys;
    //     quote_ty!(&self.ecx, ($tys))
    //   }

    //   TypeVariants::TyRef(_region, ty) => {
    //     if let TypeVariants::TyStr = ty.ty.sty {
    //       return (quote_ty!(&self.ecx, Rc<String>), false);
    //     } else {
    //       panic!("TyRef: not supported")
    //     }
    //   }
    //   _ => panic!("TypeVariants: Not yet implemented for {:?}", ty),
    // };

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
    use rustc::hir::{Expr_, BinOp_, QPath};
    let ty = self.get_type(expr);
    let copyable = self.is_copyable(ty);

    let new_expr = match expr.node {
      Expr_::ExprLit(ref lit) => match lit.node {
        ast::LitKind::Str(_, _) => quote_expr!(&self.ecx, String::from($lit)),
        _ => quote_expr!(&self.ecx, $lit),
      }

      Expr_::ExprPath(QPath::Resolved(_, ref path)) => {
        let ident = Ident::with_empty_ctxt(path.segments[0].name);
        return if copyable {
          quote_expr!(&self.ecx, $ident)
        } else {
          quote_expr!(&self.ecx, $ident.clone())
        };
      }

      Expr_::ExprBinary(ref binop, ref e1, ref e2) => {
        let e1 = self.fold_expr(e1);
        let e2 = self.fold_expr(e2);
        match binop.node {
          BinOp_::BiAdd => quote_expr!(&self.ecx, $e1 + $e2),
          BinOp_::BiEq => quote_expr!(&self.ecx, $e1 == $e2),
          _ => panic!("ExprBinary: Not yet implemented"),
        }
      }

      Expr_::ExprIf(ref cond, ref then, ref else_) => {
        let cond = self.fold_expr(cond);
        let then = self.fold_expr(then);
        match else_ {
          &Some(ref else_) => {
            let else_ = self.fold_expr(else_);
            quote_expr!(&self.ecx, if $cond { $then } else { $else_ })
          }
          &None => quote_expr!(&self.ecx, if $cond { $then }),
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
            quote_expr!(&self.ecx, {$new_block; $expr})
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
        println!("{:?}, {:?}", expr, ty);
        panic!()
        // let fnty = self.unwrap_ty_container(&self.unwrap_ty_container(&ty));
        // let (input_ty, output_ty) = self.unwrap_ty_function(&fnty);
        // println!("{:?}, {:?}, {:?}", fnty, input_ty, output_ty);
        // let input_tup = self.unwrap_ty_container(&input_ty);

        // let args: Vec<P<ast::Pat>> =
        //   body.arguments.iter()
        //   .map(|arg| {
        //     let pat = self.fold_pat(&arg.pat);
        //     bind!{let ast::PatKind::Ident(_, ref ident, _) = pat.node;}
        //     let mut pat = pat.clone().into_inner();;
        //     pat.node = ast::PatKind::Ident(
        //       ast::BindingMode::ByRef(ast::Mutability::Immutable),
        //       ident.clone(),
        //       None);
        //     P(pat)
        //   })
        //   .collect();
        // let args = SliceWrapper(args);
        // let body_expr = self.fold_expr(&body.value);
        // let arg = quote_pat!(&self.ecx, ($args));

        // quote_expr!(
        //   &self.ecx,
        //   Box::new(|__arg : $input_ty| -> $output_ty {
        //     let $arg : ($input_tup) = *__arg;
        //     $body_expr
        //   }))
      }
      _ => panic!("fold_expr: Not yet implemented"),
    };

    if copyable {
      new_expr
    } else {
      quote_expr!(&self.ecx, Rc::new($new_expr))
    }
  }

  fn fold_stmt(&self, stmt: &'tcx hir::Stmt) -> ast::Stmt {
    use rustc::hir::{Stmt_, Decl_, PatKind};
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
            let ty = self.hirty_to_astty(self.convert_type(self.get_type(expr)));

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
    use rustc::hir::PatKind;
    match pat.node {
      PatKind::Binding(_, _, ref name, _) => {
        let ident = Ident::with_empty_ctxt(name.node);
        quote_pat!(&self.ecx, $ident)
      },
      _ => panic!("fold_pat: Not yet implemented")
    }
  }
}
