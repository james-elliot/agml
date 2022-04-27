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

(* $Id: ga_types.ml 3262 2010-11-15 16:43:27Z allignol $ *)

(** Types used in the genetic algorithm. *)

type 'a chromosome = {
    mutable r_fit : float Lazy.t; (** Raw fitness. *)
    mutable s_fit : float;        (** Scaled fitness. *)
    data          : 'a            (** Data. *)
  }
(** The type of the population elements. *)

type 'a population = 'a chromosome array
(** The type of a population. *)

type scaling = No_scaling | Sigma_truncation | Power_low | Ranking
(** Scaling method. *)

type sharing = {
    protected : int list; (** Indexes of protected elements. *)
    clusters  : int;      (** Number of clusters. *)
    optima    : int;      (** Number of local optima. *)
    dmean     : float     (** *)
  }
(** Type for the output of the sharing phase. *)

type gvars = {
  seed            : int;     (** Seed for the random generator. *)
  ncores          : int; (** Number of cores to use **)
  nbgens          : int;     (** Number of generations. *)
  nbelems         : int;     (** Population size. *)
  pcross          : float;   (** Crossing rate. *)
  pmut            : float;   (** Mutation rate. *)
  scaling         : scaling; (** Scaling method. *)
  elitist         : bool;    (** If true, keep the best element for next
				 generation. *)
  display_time    : bool;    (** Display times for all operations *)
  sharing         : float;   (** Sharing rate. *)
  complex_sharing : float;   (** Complex sharing. *)
  evolutive       : bool     (** *)
  }
(** Type for the genetic algorithm parameters. *)
