open Singly_linked_list.Sll

let test_append_1 () =
  Alcotest.(check bool) "Append" true begin
    try
      let sll = new_sll 0 in
        append ~ll:sll ~v:1;
      true
    with _ -> false
    end

let test_size_1 () =
  Alcotest.(check int) "Size" 1 begin
    try
      let sll = new_sll 0 in ( 
        size ~ll:sll
      )
    with _ -> -1 
    end

let test_append_2 () =
  Alcotest.(check int) "Append" 5 begin
    try
      let sll = new_sll 0 in (
        List.iter (fun x -> append ~v:x ~ll:sll) [1;2;3;4];
        size ~ll:sll
      )
    with _ -> -1
  end

let test_search_1 () =
  Alcotest.(check int) "Search" 3 begin
    try
      let sll = new_sll 0 in (
        List.iter (fun x -> append ~v:x ~ll:sll) [1;2;3;4];
        (search ~ll:sll ~index:3)
      )
    with _ -> -1
  end

let test_search_2 () =
  Alcotest.(check bool) "Search" false begin
    try
      let sll = new_sll 0 in (
        List.iter (fun x -> append ~v:x ~ll:sll) [1;2;3;4];
        if (search ~ll:sll ~index:5) = 5 then true else false 
      )
    with _ -> false 
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
      test_case "Test" `Quick test_search_2
    ];
  ]