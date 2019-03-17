
type subst = (string,Ast.texpr) Hashtbl.t

val create : unit -> subst

val extend : subst -> string -> Ast.texpr -> unit

val remove : subst -> string -> unit

val remove_all : subst list -> string -> unit

val lookup : subst -> string -> Ast.texpr option

val apply_to_texpr : subst -> Ast.texpr -> Ast.texpr

val apply_to_expr : subst -> Ast.expr -> Ast.expr

val apply_to_env : subst -> subst -> unit

val apply_to_envs : subst -> subst list -> subst list

val string_of_subs : subst -> string

val domain : subst -> string list

val check_all : subst list -> (Ast.texpr*Ast.texpr) list

val join : subst list -> subst
