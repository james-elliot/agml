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

(* $Id: ga_reproduce.ml 3262 2010-11-15 16:43:27Z allignol $ *)

open Ga_types

(* Calcule a partir du tableau des fitness (v) le tableau des fitness *)
(* divise par la somme des fitness (v2) *)
let norm_fitness = fun pop ->
  let sum = Array.fold_right (fun elt acc -> elt.s_fit +. acc) pop 0. in
  Array.map (fun x -> x.s_fit /. sum) pop

let remainder = fun x ->
  let int_part = truncate x in
  let float_part = x -. float int_part in
  (int_part, float_part)

let compute_remainders = fun normalized_fitness n ->
  let kept = Array.make n 0
  and remainders = Array.make n 0.
  and p = ref 0 in
  Array.iteri
    (fun i norm_fit ->
      let (keep, remainder) = remainder (norm_fit *. float n) in
      kept.(i) <- keep;
      remainders.(i) <- remainder;
      p := !p + keep
    ) normalized_fitness;
  (kept, remainders, !p)

let cumul_remainders = fun remainders nb_kept n ->
  let cumul = Array.make n 0.
  and sum = ref 0.
  and s = float (n - nb_kept) in
  for i = 0 to Array.length remainders - 1 do
    cumul.(i) <- (remainders.(i) +. !sum) /. s;
    sum := !sum +. remainders.(i)
  done;
  cumul

let update_kept = fun kept cumul_remainders rem ->
  let probabilities = Array.init rem (fun _ -> Random.float 1.0) in
  Array.sort (compare) probabilities;
  let rec iter_update = fun i_proba i_kept ->
    if i_proba >= rem then ()
    else if probabilities.(i_proba) > cumul_remainders.(i_kept) then
      iter_update i_proba (i_kept + 1)
    else begin
      kept.(i_kept) <- kept.(i_kept) + 1;
      iter_update (i_proba + 1) i_kept end
  in iter_update 0 0

let create_pool = fun pop n kept protected ->
  let pool = Array.make n pop.(0) in
  let rec iter_create = fun k newprotected i ->
    if i = n then
      List.sort
	(fun i j -> -(compare pool.(i).r_fit pool.(j).r_fit))
	newprotected
    else begin
      for j = 0 to kept.(i) - 1 do
	pool.(k + j) <- pop.(i)
      done;
      if List.mem i protected then begin
	assert (kept.(i) > 0);
	iter_create (k + kept.(i)) (k :: newprotected) (i + 1) end
      else iter_create (k + kept.(i)) newprotected (i + 1)
    end in
  let newprotected = iter_create 0 [] 0 in
  (newprotected, pool)

exception Cant_Protect

let get_largest = fun kept protected ->
  let (_, largest) = Array.fold_left
      (fun (i, largest) elt ->
	if (largest = -1 || elt > kept.(largest)) &&
	  (elt > 1 || not (List.mem i protected)) then (i + 1, i)
	else (i + 1, largest)
      ) (0, -1) kept in
  if largest = -1 then raise Cant_Protect else largest

let verify_kept = fun kept protected ->
  Array.iteri (fun i nb_i ->
    if nb_i = 0 && List.mem i protected then
      let j = get_largest kept protected in
      kept.(i) <- kept.(i) + 1;
      kept.(j) <- kept.(j) - 1) kept

(* Cette fonction construit une nouvelle population en utilisant la *)
(* technique du stochastic remainder  *)
let reproduce = fun pop protected ->
  let n = Array.length pop in

(* On calcule le tableau v contenant les fitness divisees chacune par *)
(* la somme des fitness de facon a ce que la somme des elements de v *)
(* fasse 1.0 *)
  let normalized_fitness = norm_fitness pop in

(* On calcule : *)
(*  - kept qui contient le nombre de representants "certains" pour chaque *)
(*    element de population; *)
(*  - remainders qui contient le remainder pour chaque element de population *)
(*  - nb_kept qui est la somme des elements de kept *)
  let (kept, remainders, nb_kept) = compute_remainders normalized_fitness n in

(* On calcule : *)
(*  - v3 qui est le tableau des remainders cumulees divise par la somme *)
(*    des remainders. Le dernier element de ce tableau vaut 1 par *)
(*    construction. Si l'on tire un nombre aleatoire r entre 0 et 1, il *)
(*    suffit de chercher l'indice i de v3 tel que v3(i)<r<v3(i+1) pour *)
(*    savoir qu'il faut augmenter d'une unite le nombre de *)
(*    representants de l'element i dans la nouvelle population *)
  let cumulative_remainders = cumul_remainders remainders nb_kept n in

(* On met a jour kept en fonction des informations de cumulative_remainders, *)
(* comme on vient de l'expliquer *)
  update_kept kept cumulative_remainders (n - nb_kept);

(* Maintenant on verifie que tous les elements proteges sont biens *)
(* dans le tableau. S'ils n'y sont pas on les rajoute  *)
  verify_kept kept protected;

(* On construit la nouvelle population a partir de l'ancienne en *)
(* fonction du nombre de representants de chaque element donne par kept *)
(* et on tire le resultat de facon a ce que lors du croisement, ce *)
(* soit les meilleurs elements qui soient conserves en priorite *)
  create_pool pop n kept protected
