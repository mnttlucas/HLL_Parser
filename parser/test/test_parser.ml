let open Alcotest in
run "Tests" [
	"LLL Errors", Test_errors.tests;
	"LLL Sections", Test_sections.tests;
	"LLL LFD Restriction 1", Test_r1.tests;
	"LLL LFD Restriction 2", Test_r2.tests;
	"LLL LFD Restriction 3", Test_r3.tests;
	"LLL LFD Restriction 4", Test_r4.tests;
	"LLL LFD Restriction 5", Test_r5.tests;
	"LLL LFD Restriction 6", Test_r6.tests;
]