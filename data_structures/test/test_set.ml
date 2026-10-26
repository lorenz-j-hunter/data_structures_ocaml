open Set
open Singly_linked_list.Sll

let test_member_1 () =
  Alcotest.(check bool) "member" true begin
    try
      let sll: 'a sll ref = {head=(ref None)} |> ref in
        List.iter (fun x -> sll := append x !sll) [1;2;3;4];
        let set: 'a set = {elems=(!sll)} in
          member 3 set
    with _ -> false end

let () =
  let open Alcotest in
  run "Set" [
    "member", [
      test_case "Test" `Quick test_member_1
    ];
  ]
