module L = struct
  open Ga_types
    
  let gvars = Ga_cfg.read_config "general.cfg"
    
  type data = float array array array
  type user_data= unit
  type result = unit
  exception Fin_AG

  type rocket = {
      mutable y : float;
      mutable vy : float;
      mutable gy : float;
      mutable carb : float;
      mutable poussee : float;
    }
	
  let print_rocket fp {y=y;vy=vy;gy=gy;carb=carb;poussee=poussee} = 
    Printf.fprintf fp "%f %f %f %f %f\n" y vy gy carb poussee

  let init_random_tab tab = 
    for i=0 to (Array.length tab)-1 do
      tab.(i)<- (Random.float 2.0)-.1.0
    done
      
  let create_simple_net nb_in nb_hidden nb_out =
    let net = Array.make 2 [||] in
    net.(0)<- Array.make_matrix nb_hidden (nb_in+1) 0.;
    net.(1)<- Array.make_matrix nb_out (nb_hidden+1) 0.;
    Array.iter 
      (function t1 -> 
	Array.iter
	  (function t2 -> init_random_tab t2) 
	  t1) 
      net;
    net

  let one_step_forward a v =
    let dim2=Array.length v 
    and dim1 = Array.length a in
    let nv = Array.make dim1 0. in
    for i=0 to dim1-1 do
      for j=0 to dim2-1 do
	nv.(i)<-nv.(i)+.a.(i).(j)*.v.(j)
      done;
      nv.(i)<-nv.(i)+.a.(i).(dim2);
      nv.(i)<- 1./.(1.+.(exp (0.-.nv.(i))))
    done;
    nv

  let all_steps_forward net v = 
    let curr_v = ref v in
    for i=0 to (Array.length net)-1 do
      let nv = one_step_forward net.(i) !curr_v in
      curr_v := nv
    done;
    !curr_v


let g0 = 1.6;; (* metres / s^2 *)
let r0 = 1737000.;; (* metres *)
let masse = 5000. ;; (* kilos *)
let init_carb = 10000. ;; (* kilos *)
let init_altitude=15000.;; (* metres *)
let poussee_max = 45000.;; (* Newtons*)
let temps_vol_max = 600;;

let factorcarb = 150.;; (* Conso pour une poussee max par seconde en kg/s *)


let nb_inputs = 3;;
let nb_outputs = 1;;
let compute_a elt {y=y; vy=vy; carb=carb}=
  let df= all_steps_forward elt [|y/.init_altitude;vy/.50.;carb/.init_carb|] in
  df.(0);;

let avance p a dt =
  p.poussee<-a;
  let f = poussee_max*.a in
  let g = g0 *. (r0/.(r0+.p.y))**2.
  and conso = (a*.dt*.factorcarb) in
  let fr = if conso>p.carb then p.carb/.dt/.factorcarb
  else f in
  let gy = 0. -. g +. (fr)/.(masse +. p.carb) in
  p.gy<-gy;
  p.vy <- p.vy +. gy*.dt;
  p.y<-p.y +. p.vy*.dt;
  if p.y<0. then p.y<-0. ;
  p.carb <- p.carb -. conso;
  if p.carb<0. then p.carb<-0.
  ;;

  
exception Sortie of int;;
exception Sortie2 of float;;
exception Sortie3 of float;;

let timestep=1.;;
let evalData2 elt debug = 
  let mobile={y=init_altitude;vy=0.;gy=0.;carb=init_carb;poussee=0.}
  and tolvy = -2.5 in
  let fpo = if debug then Some (open_out "tmp.txt") else None in
  try (
    for i=0 to truncate ((float temps_vol_max)/.timestep) do
      let a = compute_a elt mobile in
      avance mobile a timestep;
      (match fpo with 
	 Some fp ->
          print_rocket fp mobile;
	| None -> ());
      if mobile.y=0. && mobile.vy > tolvy then raise (Sortie3 mobile.carb)
      else if mobile.y=0. then raise (Sortie2 mobile.vy)
    done;
    0.)
  with
    Sortie n ->  
      (match fpo with
	  Some fp -> close_out fp;Sys.rename "tmp.txt" "res.txt";
	| None -> ());
      (float temps_vol_max)-.(float n)*.timestep
  | Sortie3 carbr ->  
      (match fpo with
	  Some fp -> close_out fp;Sys.rename "tmp.txt" "res.txt";
	| None -> ());
    1.+.carbr/.10.
  | Sortie2 vy ->  
      (match fpo with
	  Some fp -> close_out fp;Sys.rename "tmp.txt" "res.txt";
	| None -> ());
    sqrt(1./.(1.+.tolvy-. vy))
  | exc ->
    raise exc;;


let compare_data = compare;;

let eval u numgen v = 
  let res = evalData2 v false in
  (Lazy.from_val res);;

let generate u i= 
  create_simple_net nb_inputs (10*nb_inputs+1) nb_outputs;;

let valmax=10.;;
let scale tab l i k = 
  if tab.(l).(i).(k) >valmax then 
    tab.(l).(i).(k)<-2.*.valmax-.tab.(l).(i).(k)
  else if tab.(l).(i).(k) < (-.valmax) then 
    tab.(l).(i).(k)<- -.2.*.valmax -. tab.(l).(i).(k);;

(* Attention! Au cas ou le type data contient des references ou des *)
(* tableaux, il est indispensable de les copier avant de les modifier! *)
(* Ne jamais faire de modification "in-place"! *)
  let cross u numgen a b = 
  let l = Random.int (Array.length a) in
  let i = Random.int (Array.length a.(l)) in
  let k = Random.int (Array.length a.(l).(i)) in
  let newa = Array.copy a
  and newb = Array.copy b 
  and alpha = (Random.float 2.0) -. 0.5 in
  newa.(l) <- Array.copy a.(l);
  newa.(l).(i) <- Array.copy a.(l).(i);
  newa.(l).(i).(k) <- alpha*. a.(l).(i).(k) +. (1.0-. alpha)*. b.(l).(i).(k);
  newb.(l) <- Array.copy b.(l);
  newb.(l).(i) <- Array.copy b.(l).(i);
  newb.(l).(i).(k) <- alpha*. b.(l).(i).(k) +. (1.0-. alpha)*. a.(l).(i).(k);
  (scale newa l i k);
  (scale newb l i k);
  (newa,newb);;

      
(* Attention! Memes mesures de prudence que pour le croisement *)
  let mutate u numgen a = 
  let l = Random.int (Array.length a) in
  let i = Random.int (Array.length a.(l)) in
  let k = Random.int (Array.length a.(l).(i)) in
  let newa = Array.copy a
  and noise= ((Random.float 1.0)-. 0.5)/.10.0 in
  newa.(l) <- Array.copy a.(l);
  newa.(l).(i) <- Array.copy a.(l).(i);
  newa.(l).(i).(k) <- a.(l).(i).(k)+. noise;
  scale newa l i k;
  newa;;


(*
  let outc = Unix.open_process_out "/usr/bin/gnuplot" ;;
  let plotline = "plot 'res.txt' u 1:2 w lp";;

  let tmp = ref true;;
           
  let print_data c data = 
    print_newline ();
    let toto = open_out "net" in
    Marshal.to_channel toto data [];
    close_out toto;
    let _ = evalData2 data true in
    Printf.fprintf outc "%s\n" plotline; flush outc;
    print_newline();
    if !tmp then (
      let _ = input_line stdin in
      tmp := false);;
 *)
  let print_data c data = ();;
      
  let distance u d1 d2 = 
  let d = ref 0. in
  for i=0 to (Array.length d1) -1 do
    for j=0 to (Array.length d1.(i)) -1 do
      for k=0 to (Array.length d1.(i).(j)) -1 do
	d:= !d+. abs_float (d1.(i).(j).(k)-.d2.(i).(j).(k))
      done;
    done;
  done;
  !d;;

      
  let barycenter u d1 n1 d2 n2 = d1

      
  let fileout = stdout
      
  let computeRMeanSigmaBestMaxfMinf pop = 
    let sum = ref 0.0 and sum2= ref 0.0 and best = ref 0 in 
    for i = 0 to Array.length pop - 1 do 
      let v = Lazy.force pop.(i).r_fit in
      sum := !sum +. v ;
      sum2 := !sum2 +. v *. v;
      if (v > Lazy.force pop.(!best).r_fit) then best:=i
    done; 
    let fn = float (Array.length pop) in
    let sum = !sum /. fn in
    let sum2 = sqrt (!sum2 /. fn -. sum *. sum) in
    (sum, sum2, !best)
      
  let stat numgen pop = 
    let (moy,sigma,best) = computeRMeanSigmaBestMaxfMinf pop in
    let best_elem = pop.(best) in
    Printf.fprintf fileout "generation : %d\n" numgen;
    Printf.fprintf fileout "best_elem=%a" print_data best_elem.data;
    Printf.fprintf fileout " r_fit = %f\n"  (Lazy.force best_elem.r_fit);
    Printf.fprintf fileout "moy=%f sigma=%f\n\n"  moy sigma;
    flush fileout;
    best_elem
      
      
  let init u = Random.init gvars.seed
      
  let prepare_ag u pop = ()
      
  let prepare_gen u numgen pop = pop
      
  let after_scale u numgen pop best =
    Printf.fprintf fileout "Generation: %d\n" numgen;
    Printf.fprintf fileout "best_elem=";
    print_data fileout best.data;
    Printf.fprintf fileout " r_fit = %f\n" (Lazy.force best.r_fit);
    flush fileout
      
  let after_share u numgen pop sharing = ()
      
  let after_reproduce u numgen pop prot = ()
      
  let after_gen u numgen pop = ()
      
  let terminate_ag u pop1 best_elems nb_done =
    let indbest = List.hd best_elems in
    Printf.fprintf fileout "\nResultats:\n";
    Printf.fprintf fileout "best_elem=";
    print_data fileout pop1.(indbest).data;
    Printf.fprintf fileout " r_fit = %f\n" (Lazy.force pop1.(indbest).r_fit);
    flush fileout
(*
   output_string fileout "Meilleurs elements :\n";
   List.iter  
   (fun i -> 
   Printf.fprintf fileout "element= %a r_fit=%f\n" print_data pop1.(i).data pop1.(i).r_fit)
   best_elems ;;*)
      
end

module M = Ga_optimize.Make(L);;

M.opti ();;
