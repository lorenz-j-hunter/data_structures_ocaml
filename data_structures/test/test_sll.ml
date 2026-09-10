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

let () =
  let open Alcotest in
  run "SLL" [
    "append", [
      test_case "Test" `Quick test_append_1;
      test_case "Test" `Quick test_append_2
    ];
    "size", [
      test_case "Test" `Quick test_size_1
    ]
  ]