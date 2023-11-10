//
// test21
//
// semantic analysis
// - constant folding
//

module test21;

var i: integer;

begin             // mode:   strict   simple   relaxed
  i := -2147483648;       //  fail     pass      pass
  i := -2147483647;       //  pass     pass      pass
  i :=  2147483647;       //  pass     pass      pass
  i :=  2147483648;       //  fail     fail      fail

  i := -2147483648 + 1;   //  fail     pass      pass
  i := -2147483648 * 3;   //  fail     fail      pass

  i := 0
end test21.

