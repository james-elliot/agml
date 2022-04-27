(*==============================================================================

    A Genetic Algorithm library, written in Objective Caml

    Copyright (C) 2010 Direction Générale de l'Aviation Civile (France)

    Authors: Jean-Marc Alliot, Nicolas Durand, David Gianazza,
             Pascal Brisset, Cyril Allignol

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published
    by the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received, along with this program, a copy of the
    GNU General Public License (GPL) and the GNU Lesser General Public
    License (LGPL), which is a set of  additional permissions on top
    of the GPL. If not, see <http://www.gnu.org/licenses/>.

==============================================================================*)

(* $Id: ga_crossmut.ml 3262 2010-11-15 16:43:27Z allignol $ *)

open Ga_types

let tabnum = ref [||]
let nbnum = ref 0

let init_tabnum n = 
  tabnum := Array.init n (fun i -> i);
  nbnum := n

let get_ind () = 
  let i = Random.int !nbnum in
  let ind = !tabnum.(i) in
  decr nbnum;
  !tabnum.(i) <- !tabnum.(!nbnum);
  ind
  
let rec getnfirst = fun l n ->
  if n = 0 then []
  else match l with
  | [] -> []
  | x :: xs -> x :: getnfirst xs (n - 1)

let random_couple = fun n ->
  let r = Random.int n in
  let s = (r+(Random.int (n - 1))+1) mod n in
  (r,s)

let cross = fun local_eval local_cross numgen pool pop nb ->
  let n = Array.length pool in
  for k = 0 to nb - 1 do
    let i, j = random_couple n in
    let a, b = local_cross numgen pool.(i).data pool.(j).data in
    pop.(get_ind ()) <- { r_fit = local_eval numgen a; s_fit = 0.; data = a };
    pop.(get_ind ()) <- { r_fit = local_eval numgen b; s_fit = 0.; data = b }
  done

let mutate = fun local_eval local_mutate numgen pool pop nb ->
  let n = Array.length pool in
  for k = 0 to nb - 1 do
    let i = Random.int n in
    let a = local_mutate numgen pool.(i).data in
    pop.(get_ind()) <- {r_fit = local_eval numgen a; s_fit = 0.; data = a}
  done

let swap = fun data i j ->
  let tmp = data.(i) in
  data.(i) <- data.(j);
  data.(j) <- tmp

let choose_m_from_n = fun list data n m ->
  assert (n <= Array.length data && m <= n);
  let rec iter = fun n m l ->
    if m = 0 then l
    else
      let r = Random.int n in
      let elt = data.(r) in
      swap data r (n - 1);
      iter (n - 1) (m - 1) (elt :: l) in
  iter n m list

let choose_elts = fun protected nb_to_choose nbelems ->
  let size = List.length protected in
  if size >= nb_to_choose then getnfirst protected nb_to_choose
  else (* on choisit (nb_to_choose - size) indices parmi nb_elems *)
    let pool = Array.init nbelems (fun i -> i) in
    let rec protect = fun n to_protect ->
      match to_protect with
      | [] -> ()
      | i :: is -> begin swap pool i n; protect (n - 1) is end in
    protect (nbelems - 1) protected;
    choose_m_from_n protected pool (nbelems - size) (nb_to_choose - size)

let from_old_pop = fun eval numgen pool pop nb_old protected evolutive ->
  let n = Array.length pool in
  let from_old = choose_elts protected nb_old n in
  List.iter
    (fun i ->
      let r_fit =
	if evolutive then eval numgen pool.(i).data else pool.(i).r_fit in
      pop.(get_ind()) <- { r_fit = r_fit; s_fit = 0.; data = pool.(i).data };
    ) from_old

let crossmut = fun numgen l_eval l_cross l_mutate pool pop protected gvars ->
  let n = Array.length pool in
  init_tabnum (Array.length pop);
  let crossings = truncate (float n *. gvars.pcross) / 2
  and mutations = truncate (float n *. gvars.pmut) in
  let nb_old = n-(2*crossings + mutations) in
  cross l_eval l_cross numgen pool pop crossings;
  mutate l_eval l_mutate numgen pool pop mutations;
  from_old_pop l_eval numgen pool pop nb_old protected gvars.evolutive
