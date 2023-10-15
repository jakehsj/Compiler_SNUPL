//
// test09
//
// parser test:
// - control flow statements
//
module test09;

var a,b,c: integer;

begin
  a:= 0; b := 1; c := 2;

  if (a > b) then
    c := a - b
  end;

  if (b > c) then
    a := b - c
  else
    a := c - b
  end;

  while (a > 0) do
    a := a - 1
  end
end test09.
