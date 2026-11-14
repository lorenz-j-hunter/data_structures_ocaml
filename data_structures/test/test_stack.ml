open Stack
open Singly_linked_list.Sll

let test_peek_1 () =
  Alcotest.(check bool) "peek" true begin
    try
      let stack = new_stack 0 in
        peek stack = 0
    with _ -> false;
  end

let test_peek_2 () =
  Alcotest.(check bool) "peek" false begin
    try
      let stack = {layers={head=ref None}} in (*empty stack*)
        peek stack = 0 
    with _ -> true;
  end

let test_push_1 () =
  Alcotest.(check string) "push" "correct" begin
    try
      let stack = {layers=new_sll 0} in
      let pushed = stack |> push 1 |> push 2 |> push 3 in
        if peek pushed = 3 then "correct" else "incorrect"
    with exn -> Printexc.to_string exn;
  end

let test_push_2 () =
  Alcotest.(check string) "push" "correct" begin
    try
      let stack = {layers={head=ref None}} in
      let pushed = stack |> push 1 |> push 2 |> push 3 in
        if peek pushed = 3 then "correct" else "incorrect"
    with exn -> Printexc.to_string exn;
  end

let test_pop_1 () =
  Alcotest.(check string) "pop" "correct" begin
    try
      let stack = {layers={head=ref None}} in
      let popped = stack |> push 1 |> push 2 |> push 3 |> pop in
        if peek popped = 2 then "correct" else "incorrect"
    with exn -> Printexc.to_string exn;
  end

let test_pop_2 () =
  Alcotest.(check string) "pop" "correct" begin
    try
      let stack = {layers={head=ref None}} in
      if peek stack = 0 then "failure" else "failure"
    with _ -> "correct" 
  end

let test_is_empty_1 () =
  Alcotest.(check string) "is_empty" "correct" begin
    try
      let stack = {layers={head=ref None}} in
      if is_empty stack = true then "correct" else "incorrect"
    with _ -> "failure" 
  end

let test_is_empty_2 () =
  Alcotest.(check string) "is_empty" "correct" begin
    try
      let stack = {layers={head=ref None}} |> push 0 |> push 1 |> push 2 in
      if is_empty stack = false then "correct" else "incorrect"
    with _ -> "failure" 
  end

let () =
  let open Alcotest in
  run "Stack" [
    "peek", [
      test_case "Test" `Quick test_peek_1;
      test_case "Test" `Quick test_peek_1;
    ];
    "push", [
      test_case "Test" `Quick test_push_1;
      test_case "Test" `Quick test_push_2;
    ];
    "pop", [
      test_case "Test" `Quick test_pop_1;
      test_case "Test" `Quick test_pop_2;
    ];
    "is_empty", [
      test_case "Test" `Quick test_is_empty_1;
      test_case "Test" `Quick test_is_empty_2;
    ];
  ]