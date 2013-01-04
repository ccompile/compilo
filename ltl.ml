open Ertl
open Register
open Rtl
open Kildall

module M = Map.Make(struct type t=register let compare= compare end)

type arcs = { prefs : Register.set; intfs : Register.set }
type graph = arcs M.t


