open Singly_linked_list

let test_append_1 () =
  Alcotest.(check bool) "Append" true begin
    try
      let sll = Sll.new_sll 0 in
        Sll.append ~ll:sll ~v:1;
      true
    with _ -> false
    end

let test_size_1 () =
  Alcotest.(check int) "Size" 1 begin
    try
      let sll = Sll.new_sll 0 in ( 
        Sll.size ~ll:sll
      )
    with _ -> -1 
    end

let () =
  let open Alcotest in
  run "SLL" [
    "append", [
      test_case "Test" `Quick test_append_1
    ];
    "size", [
      test_case "Test" `Quick test_size_1
    ]
  ]