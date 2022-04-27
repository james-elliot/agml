module L = struct
  open Ga_types
    
  let gvars = Ga_cfg.read_config "general.cfg";;
  let fileout = stdout;;

  type op1 = Op1 of (float->float)*string;;
  let eval_op1 o x = let Op1 (f,_) = o in f x;;
  let print_op1 o = let Op1 (_,s) = o in s;;
    
  type op2 = Op2 of (float -> float -> float)*string;;
  let eval_op2 o x y = let Op2 (f,_) = o in f x y;;
  let print_op2 o = let Op2 (_,s) = o in s;;
  
  type func =
    | Var of int 
    | Leaf of float
    | Arr1 of op1 * func
    | Arr2 of op2 * func * func;;

  exception Eval_Error;;
  let rec eval_func f t =
    match f with
    | Var i -> t.(i)
    | Leaf c -> c
    | Arr1 (f,arg) ->
       let v = eval_op1 f (eval_func arg t) in
       if Float.is_nan v then raise Eval_Error else v
    | Arr2 (f,left,right) ->
       let v = eval_op2 f (eval_func left t) (eval_func right t) in
       if Float.is_nan v then raise Eval_Error else v


  let ops1 = [|Op1 (log,"Log")|];;
  let ops2 = [| Op2 ((+.),"+"); Op2 (( *. ),"*")|];;
(*
  let ops2 = [| Op2 ((+.),"+"); Op2 (( *. ),"*"); Op2 (( ** ),"^")|];;
 *)
  type data = func;;
  type user_data= unit;;
  type result = unit;;
  exception Fin_AG;;

  (*
  let rec develop a =
    match a with
    | Arr2 (Op2(_,s),e1,e2) ->
       if s="*" then
         match e1 with
         | Arr2(Op2(_,s2),e3,e4) ->
            if s2="+" then
            develop Arr2(Op2 ((+.),"+"),
                         Arr2(Op2 (( *.),"*"),e2,e3),
                         Arr2(Op2 (( *.),"*"),e2,e4))
   *)
  
  let rec prints_data f =
    match f with
    | Var i -> Printf.sprintf "x%0d" i
    | Leaf c -> Printf.sprintf "%f" c
    | Arr1 (f,arg) ->
       (print_op1 f) ^"("^(prints_data arg)^")"
    | Arr2 (f,left,right) ->
       (print_op2 f) ^"("^(prints_data left)^","^(prints_data right)^")"
      
  let print_data fileout f =
    Printf.fprintf fileout "%s" (prints_data f);;
       
  let eval u numgen f =
    try
      let sum = ref 0.01 in
      for i=0 to 10 do
        let x = (float i)/.10. in
        let res = eval_func f [|x|] in
        let res = abs_float (res-.x*.x+.1.+.x) in
        sum := !sum+.res
      done;
      (Lazy.from_val (1./. !sum))
    with Eval_Error -> Lazy.from_val 0.;;
      
  let rec generate u i=
    let r = Random.int 3 in
    match r with
    | 0 -> Var 0
    | 1 -> Leaf (Random.float 10.0 -. 5.0)
    | 2 ->
       let n = Random.int (Array.length ops2) in
       Arr2 (ops2.(n),generate u i,generate u i)
    | _ -> failwith "Never";;

      
(* Attention! Au cas ou le type data contient des references ou des *)
(* tableaux, il est indispensable de les copier avant de les modifier! *)
(* Ne jamais faire de modification "in-place"! *)
  let cross u numgen a b = 
  (a,b);;

      
(* Attention! Memes mesures de prudence que pour le croisement *)
  let rec mutate u numgen a =
    if Random.int 2 = 0 then (
      let n = Random.int (Array.length ops2) in
      let b = generate u 0 in
      Arr2(ops2.(n),a,b)
    )
    else (
    match a with
    | Leaf x -> Leaf (x+.Random.float 1.0)
    | Var i -> Var i
    | Arr1 (op,e)->Arr1 (op,e)
    | Arr2 (op,e1,e2) ->
       let n = Random.int 5 in
       match n with
       | 0 -> Arr2(op,mutate u numgen e1,e2)
       | 1 -> Arr2(op,e1,mutate u numgen e2)
       | 2 -> e1
       | 3 -> e2
       | 4 -> a
       | _ -> failwith "Never"
    );;


  let distance u d1 d2 =
    try
      let sum = ref 0. in
      for i=0 to 10 do
        let x = (float i)/.10. in
        let res1 = eval_func d1 [|x|] in
        let res2 = eval_func d2 [|x|] in
        let res = abs_float (res2-.res1) in
        sum := !sum+.res
      done;
      !sum
    with Eval_Error -> 10000.;;


  let barycenter u d1 n1 d2 n2 = d1;;

  let rec compare_data d1 d2 = if (d1==d2) then 0 else 1;;
    
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
    Printf.fprintf fileout " r_fit = %f\n"  (Lazy.force best_elem.r_fit);
    Printf.fprintf fileout "moy=%f sigma=%f\n\n"  moy sigma;
    flush fileout;
    best_elem;;
      
      
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
