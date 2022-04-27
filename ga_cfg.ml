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

(* $Id: ga_cfg.ml 3262 2010-11-15 16:43:27Z allignol $ *)

open Ga_types

let bool_of_string x = x = "true"

module Map_strings =
  Map.Make(struct
    type t = string
    let compare x y = compare x y
  end)

let map = fun file ->
  let m = ref Map_strings.empty
  and inch = open_in file
  and sep = Str.regexp "[ \t=]+" in
  try
    while true do
      let st = input_line inch in
      let sttab = Array.of_list (Str.split sep st) in
      if Array.length sttab >= 2
      then m := Map_strings.add sttab.(0) sttab.(1) !m;
    done;
    failwith "Ga_cfg.map: Unreachable"
  with End_of_file -> close_in inch; !m

let read_config = fun ?(verbose = true) cfg ->
  let map = map cfg in
  let find_val s =
    try
      let res = Map_strings.find s map in
      if verbose then Printf.fprintf stderr "%s=%s\n%!" s res;
      res
    with x ->
      Printf.fprintf stderr "Ga_cfg.read_config: %s expected in cfg file\n%!" s;
      raise x in
  let find_float x = float_of_string (find_val x)
  and find_int x = int_of_string (find_val x)
  and find_bool x = bool_of_string (find_val x) in
  let pcross    = find_float "pcross"
  and pmut      = find_float "pmut"
  and scaling   = find_int "scaling"
  and elitist   = find_bool "elitist"
  and display_time = find_bool "display_time"
  and sharing   = find_float "sharing"
  and complex_sharing = find_float "complex_sharing"
  and evolutive = find_bool "evolutive"
  and nbgens = find_int "nbgens"
  and nbelems = find_int "nbelems"
  and ncores = find_int "ncores"
  and seed = find_int "seed" in
  let scaling = match scaling with
    0 -> No_scaling
  | 1 -> Sigma_truncation
  | 2 -> Power_low
  | 3 -> Ranking
  | _ -> failwith "Ga_cfg.read_config: scaling cfg" in
  {
    seed = seed;
    nbgens = nbgens;
    nbelems = nbelems;
    pcross = pcross;
    pmut = pmut;
    scaling = scaling;
    elitist = elitist;
    display_time = display_time;
    sharing = sharing;
    complex_sharing = complex_sharing;
    evolutive = evolutive;
    ncores=ncores;
}
