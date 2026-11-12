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

let test_member_2 () =
  Alcotest.(check bool) "member" true begin
    try
      let sll: 'a sll ref = {head=(ref None)} |> ref in
        List.iter (fun x -> sll := append x !sll) [1;2;3;4];
        let set: 'a set = {elems=(!sll)} in
          member 1 set
    with _ -> false end

let test_not_member_1 () =
  Alcotest.(check bool) "not member" false begin
    try
      let sll: 'a sll ref = {head=(ref None)} |> ref in
        List.iter (fun x -> sll := append x !sll) [1;2;3;4];
        let set: 'a set = {elems=(!sll)} in
          not_member 4 set
    with _ -> false end

let test_not_member_2 () =
  Alcotest.(check bool) "not member" true begin
    try
      let sll: 'a sll ref = {head=(ref None)} |> ref in
        List.iter (fun x -> sll := append x !sll) [1;2;3;4;5];
        let set: 'a set = {elems=(!sll)} in
          not_member 100 set
    with _ -> false end

let test_cardinality_1 () =
  Alcotest.(check int) "cardinality" 10 begin
    try
      let sll: 'a sll ref = {head=(ref None)} |> ref in
        List.iter (fun x -> sll := append x !sll) [1;2;3;4;5;6;7;8;9;10];
        let set: 'a set = {elems=(!sll)} in
          cardinality set
    with _ -> -1 end

let test_cardinality_2 () =
  Alcotest.(check int) "cardinality" 0 begin
    try
      let sll: 'a sll ref = {head=(ref None)} |> ref in
        let set: 'a set = {elems=(!sll)} in
          cardinality set
    with _ -> -1 end

let test_is_ordered_1 () =
  Alcotest.(check bool) "is_ordered" true begin
    try
      let sll: 'a sll ref = {head=(ref None)} |> ref in
        List.iter (fun x -> sll := append x !sll) [1;2;3;4;5;6;7;8;9;10];
        let set: 'a set = {elems=(!sll)} in
          is_ordered set
    with _ -> false end

let test_is_ordered_2 () =
  Alcotest.(check bool) "is_ordered" true begin
    try
      let sll: 'a sll ref = {head=(ref None)} |> ref in
        List.iter (fun x -> sll := append x !sll) [1];
        let set: 'a set = {elems=(!sll)} in
          is_ordered set
    with _ -> false end

let test_is_ordered_3 () =
  Alcotest.(check bool) "is_ordered" true begin
    try
      let sll: 'a sll ref = {head=(ref None)} |> ref in
        List.iter (fun x -> sll := append x !sll) [10;9;8;7;6;5;4;3;2;1];
        let set: 'a set = {elems=(!sll)} in
          not (is_ordered set)
    with _ -> false end

let test_is_ordered_4 () =
  Alcotest.(check bool) "is_ordered" true begin
    try
      let sll: 'a sll ref = {head=(ref None)} |> ref in
        List.iter (fun x -> sll := append x !sll) [-1;0;1];
        let set: 'a set = {elems=(!sll)} in
          is_ordered set
    with _ -> false end

let test_union_1 () =
  Alcotest.(check string) "union" "correct" begin
    try
      let sll1 = {head=(ref None)} |> ref and sll2 = {head=(ref None)} |> ref and intended = {head=(ref None)} |> ref in
        List.iter (fun x -> sll1 := append x !sll1) [1;2;3];
        List.iter (fun x -> sll2 := append x !sll2) [3;4;5];
        List.iter (fun x -> intended := append x !intended) [1;2;3;4;5];
        let s1 = {elems=(!sll1)} and s2 = {elems=(!sll2)} and i_set = {elems=(!intended)} in 
          if to_list (union s1 s2) = to_list i_set then "correct" else "incorrect"
    with exn -> Stdlib.raise exn end

let test_union_2 () =
  Alcotest.(check string) "union" "correct" begin
    try
      let sll1 = {head=(ref None)} |> ref and sll2 = {head=(ref None)} |> ref and intended = {head=(ref None)} |> ref in
        List.iter (fun x -> sll1 := append x !sll1) [];
        List.iter (fun x -> sll2 := append x !sll2) [];
        List.iter (fun x -> intended := append x !intended) [];
        let s1 = {elems=(!sll1)} and s2 = {elems=(!sll2)} and i_set = {elems=(!intended)} in 
          if to_list (union s1 s2) = to_list i_set then "correct" else "incorrect"
    with exn -> Stdlib.raise exn end

let test_union_3 () =
  Alcotest.(check string) "union" "correct" begin
    try
      let sll1 = {head=(ref None)} |> ref and sll2 = {head=(ref None)} |> ref and intended = {head=(ref None)} |> ref in
        List.iter (fun x -> sll1 := append x !sll1) [1];
        List.iter (fun x -> sll2 := append x !sll2) [2];
        List.iter (fun x -> intended := append x !intended) [1;2];
        let s1 = {elems=(!sll1)} and s2 = {elems=(!sll2)} and i_set = {elems=(!intended)} in 
          if to_list (union s1 s2) = to_list i_set then "correct" else "incorrect"
    with exn -> Stdlib.raise exn end

let test_intersection_1 () =
  Alcotest.(check string) "intersection" "correct" begin
    try
      let l1 = {head=(ref None)} |> ref and l2 = {head=(ref None)} |> ref and intended = {head=(ref None)} |> ref in
        List.iter (fun x -> l1 := append x !l1) [1;2;3;4];
        List.iter (fun x -> l2 := append x !l2) [3;4;5;6];
        List.iter (fun x -> intended := append x !intended) [3;4];
        let s1 = {elems=(!l1)} and s2 = {elems=(!l2)} and i_set = {elems=(!intended)} in 
          if to_list (intersection s1 s2) = to_list i_set then "correct" else "incorrect"
    with exn -> Stdlib.raise exn end

let test_intersection_2 () =
  Alcotest.(check string) "intersection" "correct" begin
    try
      let l1 = {head=(ref None)} |> ref and l2 = {head=(ref None)} |> ref and intended = {head=(ref None)} |> ref in
        List.iter (fun x -> l1 := append x !l1) [];
        List.iter (fun x -> l2 := append x !l2) [];
        List.iter (fun x -> intended := append x !intended) [];
        let s1 = {elems=(!l1)} and s2 = {elems=(!l2)} and i_set = {elems=(!intended)} in 
          if to_list (intersection s1 s2) = to_list i_set then "correct" else "incorrect"
    with exn -> Stdlib.raise exn end

let test_intersection_3 () =
  Alcotest.(check string) "intersection" "correct" begin
    try
      let l1 = {head=(ref None)} |> ref and l2 = {head=(ref None)} |> ref and intended = {head=(ref None)} |> ref in
        List.iter (fun x -> l1 := append x !l1) [1];
        List.iter (fun x -> l2 := append x !l2) [2];
        List.iter (fun x -> intended := append x !intended) [3];
        let s1 = {elems=(!l1)} and s2 = {elems=(!l2)} and i_set = {elems=(!intended)} in 
          if to_list (intersection s1 s2) = to_list i_set then "incorrect" else "correct"
    with exn -> Stdlib.raise exn end

let test_difference_1 () =
  Alcotest.(check string) "difference" "correct" begin
    try
      let l1 = {head=(ref None)} |> ref and l2 = {head=(ref None)} |> ref and intended = {head=(ref None)} |> ref in
        List.iter (fun x -> l1 := append x !l1) [1;2;3;4];
        List.iter (fun x -> l2 := append x !l2) [3;4;5;6];
        List.iter (fun x -> intended := append x !intended) [1;2];
        let s1 = {elems=(!l1)} and s2 = {elems=(!l2)} and i_set = {elems=(!intended)} in 
          if to_list (difference s1 s2) = to_list i_set then "correct" else "incorrect"
    with exn -> Stdlib.raise exn end

let test_sym_diff_1 () =
  Alcotest.(check string) "sym_diff" "correct" begin
    try
      let l1 = {head=(ref None)} |> ref and l2 = {head=(ref None)} |> ref and intended = {head=(ref None)} |> ref in
        List.iter (fun x -> l1 := append x !l1) [1;2;3;4];
        List.iter (fun x -> l2 := append x !l2) [3;4;5;6];
        List.iter (fun x -> intended := append x !intended) [1;2;5;6];
        let s1 = {elems=(!l1)} and s2 = {elems=(!l2)} and i_set = {elems=(!intended)} in 
          if to_list (sym_diff s1 s2) = to_list i_set then "correct" else "incorrect"
    with exn -> Stdlib.raise exn end

let test_is_subset_1 () =
  Alcotest.(check string) "is_subset" "correct" begin
    try
      let l1 = {head=(ref None)} |> ref and l2 = {head=(ref None)} |> ref in
        List.iter (fun x -> l1 := append x !l1) [1;2;3;4;5;6;7;8];
        List.iter (fun x -> l2 := append x !l2) [6;7];
        let s1 = {elems=(!l1)} and s2 = {elems=(!l2)} in
          if is_subset s2 s1 then "correct" else "incorrect"
    with _ -> "failure"
  end

let test_is_subset_2 () =
  Alcotest.(check string) "is_subset" "correct" begin
    try
      let l1 = {head=(ref None)} |> ref and l2 = {head=(ref None)} |> ref in
        List.iter (fun x -> l1 := append x !l1) [1;2;3;4;5;6;7;8];
        List.iter (fun x -> l2 := append x !l2) [6;7];
        let s1 = {elems=(!l1)} and s2 = {elems=(!l2)} in
          if is_subset s1 s2 then "incorrect" else "correct"
    with _ -> "failure"
  end

let test_is_superset_1 () =
  Alcotest.(check string) "is_superset" "correct" begin
    try
      let l1 = {head=(ref None)} |> ref and l2 = {head=(ref None)} |> ref in
        List.iter (fun x -> l1 := append x !l1) [1;2;5];
        List.iter (fun x -> l2 := append x !l2) [2];
        let s1 = {elems=(!l1)} and s2 = {elems=(!l2)} in
          if is_superset s1 s2 then "correct" else "incorrect"
    with _ -> "failure"
  end

let test_is_superset_2 () =
  Alcotest.(check string) "is_superset" "correct" begin
    try
      let l1 = {head=(ref None)} |> ref and l2 = {head=(ref None)} |> ref in
        List.iter (fun x -> l1 := append x !l1) [1;2;5;8];
        List.iter (fun x -> l2 := append x !l2) [5;10;9];
        let s1 = {elems=(!l1)} and s2 = {elems=(!l2)} in
          if is_superset s1 s2 then "incorrect" else "correct"
    with _ -> "failure"
  end

let test_is_disjoint_1 () =
  Alcotest.(check string) "is_disjoint" "correct" begin
    try
      let l1 = {head=(ref None)} |> ref and l2 = {head=(ref None)} |> ref in
        List.iter (fun x -> l1 := append x !l1) [1;2;5];
        List.iter (fun x -> l2 := append x !l2) [8;10;11];
        let s1 = {elems=(!l1)} and s2 = {elems=(!l2)} in
          if is_disjoint s1 s2 then "correct" else "incorrect"
    with _ -> "failure"
  end

let test_is_disjoint_2 () =
  Alcotest.(check string) "is_disjoint" "correct" begin
    try
      let l1 = {head=(ref None)} |> ref and l2 = {head=(ref None)} |> ref in
        List.iter (fun x -> l1 := append x !l1) [1;2;5;8];
        List.iter (fun x -> l2 := append x !l2) [5;10;9];
        let s1 = {elems=(!l1)} and s2 = {elems=(!l2)} in
          if is_disjoint s1 s2 then "incorrect" else "correct"
    with _ -> "failure"
  end

let test_rbs_1 () =
  Alcotest.(check string) "rbs" "correct" begin
    try
      let l1 = {head=(ref None)} |> ref in
        List.iter (fun x -> l1 := append x !l1) [1;2;3;4];
        let s1 = {elems=(!l1)} in
          if rbs 1 s1 = true then "correct" else "incorrect"
    with _ -> "failure"
  end

let test_rbs_2 () =
  Alcotest.(check string) "rbs" "correct" begin
    try
      let l1 = {head=(ref None)} |> ref in
        List.iter (fun x -> l1 := append x !l1) [1];
        let s1 = {elems=(!l1)} in
          if rbs 1 s1 = true then "correct" else "incorrect"
    with _ -> "failure"
  end

let test_rbs_3 () =
  Alcotest.(check string) "rbs" "correct" begin
    try
      let l1 = {head=(ref None)} |> ref in
        List.iter (fun x -> l1 := append x !l1) [];
        let s1 = {elems=(!l1)} in
          if rbs 1 s1 = false then "correct" else "incorrect"
    with _ -> "failure"
  end

let () =
  let open Alcotest in
  run "Set" [
    "member", [
      test_case "Test" `Quick test_member_1;
      test_case "Test" `Quick test_member_2
    ];
    "not member", [
      test_case "Test" `Quick test_not_member_1;
      test_case "Test" `Quick test_not_member_2
    ];
    "cardinality", [
      test_case "Test" `Quick test_cardinality_1;
      test_case "Test" `Quick test_cardinality_2
    ];
    "is_ordered", [
      test_case "Test" `Quick test_is_ordered_1;
      test_case "Test" `Quick test_is_ordered_2;
      test_case "Test" `Quick test_is_ordered_3;
      test_case "Test" `Quick test_is_ordered_4
    ];
    "union", [
      test_case "Test" `Quick test_union_1;
      test_case "Test" `Quick test_union_2;
      test_case "Test" `Quick test_union_3;
    ];
    "intersection", [
      test_case "Test" `Quick test_intersection_1;
      test_case "Test" `Quick test_intersection_2;
      test_case "Test" `Quick test_intersection_3
    ];
    "difference", [
      test_case "Test" `Quick test_difference_1
    ];
    "sym_diff", [
      test_case "Test" `Quick test_sym_diff_1
    ];
    "is_subset", [
      test_case "Test" `Quick test_is_subset_1;
      test_case "Test" `Quick test_is_subset_2;
    ];
    "is_superset", [
      test_case "Test" `Quick test_is_superset_1;
      test_case "Test" `Quick test_is_superset_2;
    ];
    "is_disjoint", [
      test_case "Test" `Quick test_is_disjoint_1;
      test_case "Test" `Quick test_is_disjoint_2;
    ];
    "rbs", [
      test_case "Test" `Quick test_rbs_1;
      test_case "Test" `Quick test_rbs_2;
      test_case "Test" `Quick test_rbs_3;
    ]
  ]
