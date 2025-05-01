open Alcotest

(* Another test case *)
let test_haha () = Yarrow.haha()

(* Define the test set *)
let test_suites = [
  test_case "haha" `Quick test_haha;
]

(* Main function to run the tests *)
let () =
  run "yarrow" [
    "haha", test_suites;
  ]