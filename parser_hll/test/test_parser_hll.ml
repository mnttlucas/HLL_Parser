let open Alcotest in
run "Tests" [
	"HLL Sections", Test_sections.tests;
]