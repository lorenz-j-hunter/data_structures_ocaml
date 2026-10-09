open Set

let test_member_1 () =
  Alcotest.(check bool) "Member" true begin
    try
      false
    with _ -> false
    end

let () =
  let open Alcotest in
  run "Set" [
    "member", [
      test_case "Test" `Quick test_member_1
    ];
  ]
