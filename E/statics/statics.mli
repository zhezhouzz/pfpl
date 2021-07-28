open Abt

type sort_context

val abt_sort : abt -> sort_context -> sort option

type typ_context

val abt_typ : abt -> typ_context -> typ option
