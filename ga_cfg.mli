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

(* $Id: ga_cfg.mli 3262 2010-11-15 16:43:27Z allignol $ *)

(** Configuration file reader. *)

val read_config : ?verbose:bool -> string -> Ga_types.gvars
(** [read_config ~verbose file] reads [file] and ouputs the parameters for the
    genetic algorithm. If [verbose] is [true], parameters are printed on
    [stderr]; default value is [false]. *)
