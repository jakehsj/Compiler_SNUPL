//
// test08
//
// IR generation
// - assignments to boolean variables
//

module test08;

var i: integer;
    b: boolean;

begin
  b := (i < 3) && (i > 0);
  if (b) then
    i := 0
  end
end test08.
