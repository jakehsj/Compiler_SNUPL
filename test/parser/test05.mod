//
// test05
//
// parser test:
// - parameters and local variables
//

module test05;

var a  : integer;

function bar(a: integer): integer;
begin
  return a+1
end bar;

procedure foo(a: integer);
var t1, t2, t3: integer;
begin
  t1 := a;
  t2 := a + t1;
  t3 := a + t2
end foo;

begin
  a := bar(73);
  foo(75)
end test05.
