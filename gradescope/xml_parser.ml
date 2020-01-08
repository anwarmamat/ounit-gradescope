(*
  This program converts the Ounit2 XML output to gradescope JSON format 
*)
open Markup;;

(*let in_channel = open_in "log2.xml"  in 
    let xml = channel in_channel in *)

let print_xml1 filename = 
  let (xml,_) = file filename in Printf.printf "%s\n" (to_string xml)
;;

let rec process_signal strm s =
  match s with
  | `Start_element ((_,name), children) -> process_element strm name children
  | _-> ("",true)
      
(** Find the end of the element. It finds the failure if the current test failed *)
and is_false strm =
  let s = next strm in
  match s with
  |Some elem ->
    (
    match elem with
    | `End_element -> false
    | `Start_element ((_,name), _) -> String.equal name "failure"
    | _->is_false strm
    )
  |None->false
       
(** Find the test name. Returns (test_name, true) if test passed or (test_name,false) test failed *)     
and process_element strm name children =
  if String.equal name "testcase" then
    let (_,test_name) = List.find (fun ((_,x),s) -> String.equal x "name") children in 
    (test_name, (is_false strm))
  else
    ("",true)
;;

let print_xml2 filename = 
  let (xml,_) = file filename in
  let strm = xml |> parse_xml |> signals in
  let lst =   fold (fun acc s ->
                  let name = process_signal strm s in
                  name::acc)
                [] strm
 in 
 lst = List.filter (fun (a,_)-> a <> "") lst
 ;;

type json = [
  | `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of json list
  | `Null
  | `String of string
  ]
;;


let to_json lst =
  let result = `Assoc [ 
        (
        "tests", 
                    `List [
                          `Assoc [("name", `String "test 1");("score",`Int 10)]; 
                          `Assoc [("name", `String "test 2");("score",`Int 20)]
                          ]
        )
                 ] in 
  

  let t = Yojson.Basic.pretty_to_string result in
    Printf.printf "%s\n" t
;;


let () = to_json (print_xml2 "log2.xml")
;;
