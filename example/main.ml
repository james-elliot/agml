(* Optimisation de la fonction de Griewank *)
(* L'optimum (101) est obtenu avec un vecteur dont toutes les coordonnées valent 0 *)

(* dimension du problème *)
let dim=100;;

 (*la fonction a minimiser*)
let f x =  
  let sum=ref 0. and prod=ref 1.
  and dim=Array.length x in
  for i=0 to dim-1 do
    sum:= !sum +. x.(i)*.x.(i);
    prod:= !prod*. (cos (x.(i)/.(sqrt (float (i+1)))))
  done;
  let res = (!sum/.400./. (float dim)) -. !prod in
  let res = if res >100. then 100. else res in
  100. -. res;;
    
module L = struct
  open Ga_types

  let gvars = Ga_cfg.read_config "general.cfg"
      
  type data = float array
  type user_data= unit
  type result = unit
  exception Fin_AG


  let eval u numgen data = 
    (lazy (f data))

  let compare_data = compare
                   
  let generate u numgen =
    Array.init dim (fun i -> -10. +. Random.float 20.0)
      
  let cross u numgen a b =
    let dim= Array.length a in
    let newa = Array.copy a
    and newb = Array.copy b 
    and alpha=(Random.float 2.) -.0.5 in
    for i=0 to dim-1 do
      if Random.int 2 =0 then 
	(newa.(i) <- (alpha*. a.(i) +. (1.-.alpha)*. b.(i));
	 newb.(i) <- (alpha*. b.(i) +. (1.-.alpha)*. a.(i)))
      else 
	(newa.(i) <- b.(i) ;newb.(i) <- a.(i)) 
    done;
    (newa,newb)
      
  let mutate u numgen a =
    let dim= Array.length a in
    let newa=Array.copy a in
    let i = Random.int dim in
    newa.(i)<-newa.(i) +. (Random.float 1.) -.0.5;
    newa

      
  let distance u d1 d2 =
    let dim= Array.length d1 in
    let sum=ref 0. in
    for i=0 to dim-1 do
      sum:= !sum+. (d1.(i)-.d2.(i))*.(d1.(i)-.d2.(i))
    done;
    sqrt (!sum/.(float dim))
            
  let barycenter u d1 n1 d2 n2 =
    let dim= Array.length d1 in
    let f1=float n1 and f2=float n2 in
    let g=Array.make dim 0. in
    for i=0 to dim-1 do
      g.(i) <- ((f1*. d1.(i) +. f2*. d2.(i))/.(f1+. f2))
    done;
    g

  let init u = 
    Random.init gvars.seed
      
  let prepare_ag u pop = ()
    
  let prepare_gen u numgen pop =  pop
      
  let after_scale u numgen pop best =
    Array.iter (fun x-> Printf.printf "%f " x) best.data;
    Printf.printf "r_fit=%f" (Lazy.force best.r_fit);
    print_newline();
    ()

  let after_share u numgen pop clus = ()

  let after_reproduce u numgen pop prot = ()

  let after_gen u numgen pop = ()
      
  let terminate_ag u pop1 best_elems nb_done = ()
      
end
    
module M = Ga_optimize.Make(L);;


let start_ag  = M.opti () ;;


