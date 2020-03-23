module Q = QCheck

let suite =
  List.concat
  [ Test_Listutils.testlist
  ; Test_Strutils.testlist
  ; Test_Sm.testlist
  ; Test_HashedString.testlist
  ] in
    R.run_tests_main suite
