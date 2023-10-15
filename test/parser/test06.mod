//
// test06
//
// parser test:
// - integer & boolean data types
// - binary and unary expressions
//
module test06;

var a,b,c: integer;
    b0,b1: boolean;

procedure unary(a: integer; b: boolean);
var x: integer;
    y: boolean;
begin
  x := -a;
  y := !b
end unary;

procedure binary(a: integer; b: boolean);
var x: integer;
    y: boolean;
begin
  x := -a * 3;
  y := !b || b
end binary;

begin
end test06.
