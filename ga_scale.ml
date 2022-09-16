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

(* $Id: ga_scale.ml 3262 2010-11-15 16:43:27Z allignol $ *)

open Ga_types

let mean_sigma = fun pop ->
  let sum, squares_sum =
    Array.fold_right
      (fun x (sum, squares_sum) ->
	(sum +. x.s_fit, squares_sum +. x.s_fit *. x.s_fit)
      ) pop (0., 0.)
  and fn = float (Array.length pop) in
  let mean = sum /. fn in
  (mean, sqrt (squares_sum /. fn -. mean *. mean))

let min_max = fun pop ->
  Array.fold_right
    (fun x (maxf, minf, e) ->
      if x.s_fit > maxf then (x.s_fit, min x.s_fit minf, Some x)
      else (maxf, min x.s_fit minf, e)
    ) pop (neg_infinity, infinity, None)

let normalize = fun pop ->
  let maxf, minf, e = min_max pop in
  let diff = maxf -. minf in
  if diff <= 0. then Array.iter (fun x -> x.s_fit <- 1.) pop
  else Array.iter (fun x -> x.s_fit <- (x.s_fit -. minf) /. diff) pop;
  match e with
  | Some x -> x
  | None -> raise (Failure (Printf.sprintf "Ga_scale.normalize : minf:%f maxf:%f" minf maxf))

let sigma_truncation = fun pop ->
  let c = 2.0
  and mean, sigma = mean_sigma pop in
  Array.iter
    (fun x -> x.s_fit <- max 0. (x.s_fit -. (mean -. c *. sigma))
    ) pop;
  normalize pop

let power_low = fun numgen pop nbgens ->
  let s = 0.1 and s0 = 0.1 and p1 = 0.05 and p2 = 0.1 and alpha = 0.1 in
  let t1 = p2 *. ((s0 /. s) ** alpha)
  and t2 = (float numgen) /. (float (nbgens + 1)) *. (3.1415927 /. 2.0) in
  let t3 = (tan t2) ** t1 and t4 = (s /. s0) ** p1 in
  let k = t3 *. t4 in
  Array.iter
    (fun x -> x.s_fit <- x.s_fit ** k)
    pop;
  normalize pop

(* Modifie la s_fit pour faire une selection de type ranking *)
let ranking = fun pop ->
  let n = Array.length pop in
  let ranks = Array.init n (fun i -> i) in
  Array.sort (fun i j -> -(compare pop.(i).s_fit pop.(j).s_fit)) ranks;
  Array.iteri (fun i rank -> pop.(rank).s_fit <- float (n - i)) ranks;
  normalize pop

module T = Domainslib.Task;;

let scale = fun numgen pop gvars pool ->
  if gvars.ncores>1 then 
    T.run pool
      (fun _ -> 
        T.parallel_for 0 (Array.length pop -1)
          (fun i -> pop.(i).s_fit <- Lazy.force pop.(i).r_fit) pool)
  else
    Array.iteri (fun i _ -> pop.(i).s_fit <- Lazy.force pop.(i).r_fit) pop;
  let e = normalize pop in
  match gvars.scaling with
  | No_scaling -> e
  | Sigma_truncation -> sigma_truncation pop
  | Power_low -> power_low numgen pop gvars.nbgens
  | Ranking -> ranking pop;;
