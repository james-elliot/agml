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

(* $Id: ga_crossmut.mli 3262 2010-11-15 16:43:27Z allignol $ *)

(** Crossover and mutation. *)

val crossmut :
    'a ->
    ('a -> 'b -> float Lazy.t) ->
    ('a -> 'b -> 'b -> 'b * 'b) ->
    ('a -> 'b -> 'b) ->
    'b Ga_types.population ->
    'b Ga_types.population -> int list -> Ga_types.gvars -> unit
(** [crossmut numgen eval cross mutate pool population protected gvars].
    @param numgen Generation number.
    @param eval Evaluation function.
    @param cross Crossover operator.
    @param mutate Mutation operator.
    @param pool Pool of elements to select from for crossover and mutation.
    @param population The population.
    @param protected List of indexes of protected elements. *)
