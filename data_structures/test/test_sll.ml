open Singly_linked_list.Sll

let test_append_1 () =
  Alcotest.(check bool) "Append" true begin
    try
      let getv node = node.v in
        (append 1 {head=(ref None)}).head |> unpeel |> getv = 1
    with _ -> false
    end

let test_size_1 () =
  Alcotest.(check int) "Size" 1 begin
    try
      let sll = new_sll 0 in ( 
        size sll
      )
    with _ -> -1 
    end

let test_append_2 () =
  Alcotest.(check int) "Append" 5 begin
    try
      let sll = ref (new_sll 0) in (
        List.iter (fun x -> sll := append x !sll) [1;2;3;4];
        size !sll
      )
    with _ -> -1
  end

let test_search_1 () =
  Alcotest.(check int) "Search" 3 begin
    try
      let sll = ref (new_sll 0) in (
        List.iter (fun x -> sll := append x !sll) [1;2;3;4];
        (search !sll 3)
      )
    with _ -> -1
  end

let test_search_2 () =
  Alcotest.(check bool) "Search" false begin
    try
      let sll = ref (new_sll 0) in (
        List.iter (fun x -> sll := append x !sll) [1;2;3;4];
        if (search !sll 5) = 5 then true else false 
      )
    with _ -> false 
  end

let test_search_3 () =
  Alcotest.(check bool) "Search" false begin
    try
      let sll = ref (new_sll 0) in (
        List.iter (fun x -> sll := append x !sll) [1;2;3;4];
        if (search !sll (-1)) = 5 then true else false 
      )
    with _ -> false 
  end

let test_insert_1 () =
  Alcotest.(check int) "Insert" 10 begin
    try
      let sll = ref (new_sll 0) in (
        List.iter (fun x -> sll := append x !sll) [1;2;3;4];
        sll := insert (*ll*)!sll (*v*)10 (*index*)3;
        (search !sll 3)
      )
    with _ -> -1
  end

let test_insert_2 () =
  Alcotest.(check int) "Insert" 0 begin
    try
      let sll = ref (new_sll 1) in (
        List.iter (fun x -> sll := append x !sll) [2;3;4];
        sll := insert !sll (*v**)0 (*index*)0;
        (search !sll 0)
      )
    with _ -> -1
  end

let test_remove_1 () =
  Alcotest.(check int) "Remove" 4 begin
    try
      let sll = ref (new_sll 0) in (
        List.iter (fun x -> sll := append x !sll) [1;2;3;4];
        sll := remove !sll 3;
        (search !sll 3)
      )
    with _ -> -1
  end

let test_extend_1 () =
  Alcotest.(check int) "Extend" 9 begin
    try
      let pair = (ref (new_sll 0), ref (new_sll 5)) in (
        List.iter (fun x -> (fst pair) := append x !(fst pair)) [1;2;3;4];
        List.iter (fun x -> (snd pair) := append x !(snd pair)) [6;7;8];
        let copy = extend !(fst pair) !(snd pair) in
          (size copy)
      )
    with _ -> -1
  end

let test_extend_2 () =
  Alcotest.(check int) "Extend" 7 begin
    try
      let pair = (ref (new_sll 0), ref (new_sll 5)) in (
        List.iter (fun x -> (fst pair) := append x !(fst pair)) [1;2;3;4];
        List.iter (fun x -> (snd pair) := append x !(snd pair)) [6;7;8];
        let copy = extend !(fst pair) !(snd pair) in
          (search copy 7)
      )
    with _ -> -1
  end

let test_reverse_1 () =
  Alcotest.(check (list int)) "Reverse" [9; 0] begin
    try
      let sll = ref (new_sll 0) in (
        List.iter (fun x -> sll := append x !sll) [1;2;3;4;5;6;7;8;9];
        sll := reverse !sll; 
        [(search !sll 8); (search !sll 0)]
      )
    with _ -> [-1] 
  end

let test_reverse_2 () =
  Alcotest.(check (list int)) "Reverse" [2; 0] begin
    try
      let sll = new_sll 0 |> ref in (
        List.iter (fun x -> sll := append x !sll) [1;2];
        sll := reverse !sll;
        [(search !sll 2); (search !sll 0)]
      )
    with _ -> [-1] 
  end

let () =
  let open Alcotest in
  run "SLL" [
    "append", [
      test_case "Test" `Quick test_append_1;
      test_case "Test" `Quick test_append_2
    ];
    "size", [
      test_case "Test" `Quick test_size_1
    ];
    "search", [
      test_case "Test" `Quick test_search_1;
      test_case "Test" `Quick test_search_2;
      test_case "Test" `Quick test_search_3
    ];
    "insert", [
      test_case "Test" `Quick test_insert_1;
      test_case "Test" `Quick test_insert_2
    ];
    "remove", [
      test_case "Test" `Quick test_remove_1;
    ];
    "extend", [
      test_case "Test" `Quick test_extend_1;
      test_case "Test" `Quick test_extend_2;
    ];
    "reverse", [
      test_case "Test" `Quick test_reverse_1;
      test_case "Test" `Quick test_reverse_2;
    ]
  ]