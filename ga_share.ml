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

(* $Id: ga_share.ml 3262 2010-11-15 16:43:27Z allignol $ *)

open Ga_types

type 'a cluster={
    mutable center : 'a;
    mutable nb_elems : int}

type 'a chrom = {chromosome : 'a chromosome; mutable clus : 'a cluster option}

(*** Do we use complex dmax-dmin computation (see dmean and minmaxfactor) *)
(*** See "complex_sharing" variable in Gvars module *)

(*** Local variables for dmin and dmax in clustering *)
(* dmax = dmean / minmaxfactor *)
let minmaxfactor = ref 1.
let dmean = ref 0.5

(*** Find the best element of a population *)
let find_best = fun pop ->
  let best = ref 0 in
  Array.iteri
    (fun i popi ->
      if Lazy.force popi.chromosome.r_fit < 0. then
	failwith "The use of sharing requires a positive evaluation function!"
      else if Lazy.force popi.chromosome.r_fit > Lazy.force pop.(!best).chromosome.r_fit then
	best := i
    ) pop;
  !best

(*** Merge two clusters when too close to each other *)
let merge = fun barycenter pop cluster1 cluster2 ->
  let new_center = barycenter
      cluster1.center cluster1.nb_elems
      cluster2.center cluster2.nb_elems
  and sum_elems= cluster1.nb_elems + cluster2.nb_elems in
  cluster2.center <- new_center; cluster2.nb_elems <- sum_elems;
  Array.iter
    (fun chrom ->
      try
        let cluster = Option.get chrom.clus in
        if (compare cluster cluster1)=0 then chrom.clus <- Some cluster2
        with Invalid_argument _ -> ()
      (*
      match chrom.clus with
      |	Some cluster ->
         if (compare cluster cluster1)=0 then chrom.clus <- Some cluster2
      | _ -> ()
       *)
    ) pop;
  cluster2

(*** Try to merge clusters *)
let havetomergecluster = fun compare_data barycenter distance pop clust clusters dmax ->
  let dmin = dmax /. 3.0 in
  let rec mergeclusters = fun cluster new_clusters clusters ->
    match clusters with
    | []   -> cluster :: new_clusters
    | c :: cs ->
       if (compare_data c.center cluster.center)=0 then
         mergeclusters cluster new_clusters cs
       else
         let d = distance c.center cluster.center in
	 if d >= dmin then mergeclusters cluster (c :: new_clusters) cs
         else
	   let new_cluster = merge barycenter pop c cluster in
	   mergeclusters new_cluster [] (new_clusters @ cs)
  in mergeclusters clust [] clusters

(*** Make clustering for sharing *)
(* Compute distance and returns a couple (used by fold_left) *)
let func = fun distance chromosom (ref_cluster, d) cluster ->
  let new_d = distance chromosom cluster.center in
  if new_d < d then (cluster, new_d) else (ref_cluster, d)

let do_clusters = fun compare_data distance barycenter pop ->
  let dmax = !dmean /. !minmaxfactor in
  let first_cluster = {center = pop.(0).chromosome.data; nb_elems = 1} in
  pop.(0).clus <- Some first_cluster;
  let rec iter_do_clusters = fun i clusters ->
    if i = Array.length pop then clusters
    else
      (* les 2 lignes qui suivent sont-elles utiles ? *)
      let first = List.hd clusters in
      let inf = distance pop.(i).chromosome.data first.center in
      let (cluster, d) = List.fold_left
	  (func distance pop.(i).chromosome.data) (first,inf) clusters in
      if d > dmax then begin
	let new_cluster = {center = pop.(i).chromosome.data; nb_elems = 1} in
	pop.(i).clus <-  Some new_cluster;
	iter_do_clusters (i + 1) (new_cluster :: clusters) end
      else begin
	let new_center = barycenter
	    cluster.center cluster.nb_elems
	    pop.(i).chromosome.data 1 in
	cluster.center <- new_center; cluster.nb_elems <- cluster.nb_elems + 1;
	pop.(i).clus <- Some cluster;
	let merged_clusters =
	  havetomergecluster compare_data barycenter distance pop cluster clusters dmax in
	iter_do_clusters (i + 1) merged_clusters end
  in iter_do_clusters 1 [first_cluster]

(* Attention, il y a un probleme avec la notion d'elitisme et de sharing.
Qu'est ce que ca veut dire etre a 90% du meilleur element? Ca n'a pas
de sens sur [-Inf;Inf]. En utilisant la s_fitness, on provoque une forte
instabilite liee a la distribution des valeurs des elements de population.
Ici, je suppose que l'on travaille avec une r_fit sur [0;Inf] A refaire. *)
let get_best = fun sharing bestgen pop clusters compare_data ->
  let rec best_in_cluster = fun cluster i best max_fitness ->
    if i = Array.length pop then (best, max_fitness)
    else match pop.(i).clus with
         | Some c
              when (compare_data c.center cluster.center)=0 ->
	let r = Lazy.force pop.(i).chromosome.r_fit in
	if r > max_fitness then best_in_cluster cluster (i + 1) i r
	else best_in_cluster cluster (i + 1) best max_fitness
    | _ -> best_in_cluster cluster (i + 1) best max_fitness in
  List.fold_left
    (fun bests cluster ->
      let (best, max_fitness) = best_in_cluster cluster 0 (-1) neg_infinity in
      if max_fitness > sharing *. Lazy.force pop.(bestgen).chromosome.r_fit
      then best :: bests
      else bests
    ) [] clusters

(*** Compute mean distance to compute dmin and dmax *)
let compute_dmean = fun distance pop clusters ->
  let add_distance = fun data clusters ->
    List.fold_left (fun r cluster ->
      r +. distance data cluster.center) 0. clusters in
  let d = Array.fold_left (fun d elt ->
    d +. add_distance elt.chromosome.data clusters) 0. pop in
  d /. float (Array.length pop) /. float (List.length clusters)


(*** Make sharing *)
let share = fun distance barycenter compare_data gvars popinit ->
  let pop = Array.map
      (fun chrom -> {chromosome = chrom; clus = None}) popinit in
  let bestgen = find_best pop in
  if gvars.sharing = 0.
  then 
    {protected = if gvars.elitist then [bestgen] else [];
     clusters = 0; optima = 0; dmean = 0.}
      (* returns the best element from population *)
  else (
    if gvars.complex_sharing <> 0. then
      (dmean := gvars.complex_sharing; minmaxfactor := 1.);
    let clusters = do_clusters compare_data distance barycenter pop in
    (* Get the best elements' list from clusters *)
    let protected = get_best gvars.sharing bestgen pop clusters compare_data in
    let nb_optima = List.length protected
    and nb_clusters = List.length clusters in
    if gvars.complex_sharing = 0. then (
      (* If we use complex sharing computation, then recompute *)
      (* dmean and minmaxfactor variables *)
      let max = float nb_optima /.  float nb_clusters in
      dmean := compute_dmean distance pop clusters;
      if max > 0.85 && !minmaxfactor < 100. then
	minmaxfactor := !minmaxfactor *. 1.05
      else if max < 0.75 && !minmaxfactor > 1. then
	minmaxfactor := !minmaxfactor *. 0.95
    );
    (* The sharing function *)
    Array.iter
      (fun elt ->
      	match elt.clus with
	| Some c ->
	   elt.chromosome.s_fit <- elt.chromosome.s_fit /. float c.nb_elems
      	| None -> failwith "Ga_share.share"
      ) pop;
    {protected = if gvars.elitist then protected else [];
     clusters  = nb_clusters;
     optima    = nb_optima;
     dmean     = !dmean}
  )
