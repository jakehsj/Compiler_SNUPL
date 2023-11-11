//
// test13
//
// semantic analysis
// - constant expressions
//

module test13;

const
  M : integer = 5;
  K : integer = 5 + 5 * 3;
var
  L : integer;
  N : integer;

begin
  L := 5 + 5 * 3;
  N := K + L
end test13.
