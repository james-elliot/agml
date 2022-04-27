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

(* $Id: ga_optimize.mli 3262 2010-11-15 16:43:27Z allignol $ *)

(** Genetic Algorithm.

    This module implements a genetic algorithm, as described in {i Genetic
    Algorithms}, David Goldberg, 1989.

*)

module type LOCAL = sig

  open Ga_types

  type data      (** Data encoding. *)
  type user_data (** Data passed by the user when calling the AG. *)
  type result    (** Result to be returned by terminate_AG. *)

  exception Fin_AG
  (** To be raised when the termination criterium is met. *)

  val gvars : gvars
  (** Variables read in the configuration file. *)

  val eval : user_data -> int -> data -> float Lazy.t
  (** [eval user_data numgen data] returns the evaluation (fitness) of
      [data]. *)

  val generate : user_data -> int -> data
  (** [generate user_data i] generates a new data. [i] is the index of this
      data in the population. *)

  val cross : user_data -> int -> data -> data -> data * data
  (** [cross user_data numgen parent1 parent2] returns a pair of children
      issued from the crossing of [parent1] and [parent2].
      CAUTION : if [parent1] or [parent2] contain references, arrays,
      hashtables, they must be copied before making any changes. No in-place
      modification is to be made. *)

  val mutate : user_data -> int -> data -> data
  (** [mutate user_data numgen element] returns the mutation of [element].
      CAUTION : same as for [cross]. *)

  val distance : user_data -> data -> data -> float
  (** Returns the distance between two data. *)

  val barycenter : user_data -> data -> int -> data -> int -> data
  (** [barycenter user_data elt1 coeff1 elt2 coeff2]. *)

  val compare_data : data ->  data -> int 
  (** [compare_data elt1 elt2]. *)

  val init : user_data -> unit
  (** Called at the very beginning of the AG. *)

  val prepare_ag : user_data -> data population -> unit
  (** [prepare_ag user_data population]. Called right after the initial
      population has been generated. *)

  val prepare_gen : user_data -> int -> data population -> data population
  (** [prepare_gen user_data numgen population]. Called at the beginning of
      each generation. *)

  val after_scale : user_data -> int -> data population
    -> data chromosome -> unit
  (** [after_scale user_data numgen population best]. Called after scaling.
      [best] is the best element of [population]. *)

  val after_share : user_data -> int -> data population -> sharing -> unit
  (** [after_share user_data numgen population sharing]. Called after
      sharing. [sharing] contains the list of indices of protected elements,
      the number of best clusters, the total number clusters and the value of
      dmean. *)

  val after_reproduce : user_data -> int -> data population -> int list
    -> unit
  (** [after_reproduce user_data numgen population protected]. Called after
      reproduction. [protected] contains the indexes of protected elements. *)

  val after_gen : user_data -> int -> data population -> unit
  (** [after_gen user_data numgen population]. Called at the end of each
      generation. *)

  val terminate_ag : user_data -> data population -> int list -> int ->
    result
  (** [terminate_ag user_data population bests elapsed]. Called at the end of
      the AG. [bests] contains the indexes of the best element in each cluster,
      [elapsed] is the number of generations done. *)

end
(** Input signature for the functor {!Ga_optimize.Make}. *)

module Make(Local : LOCAL) : sig

  (** [opti user_data]. *)
  val opti : Local.user_data -> Local.result

end
(** Functor building an implementation of a genetic algorithm, given a set of
  functions for population generation and evaluation, elements type, mutation
  and crossover operators on elements. *)
