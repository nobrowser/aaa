module R = QCheck_base_runner

let suite =
  List.concat
  [ Test_Listutils.testlist
  ; Test_Strutils.testlist
  ; Test_Sm.testlist
  ; Test_HashedString.testlist
  ; Test_SkewBin.testlist
  ]

let _ = R.run_tests_main suite
