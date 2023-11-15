//
// test02
//
// IR generation
// - function and procedure calls
//

module test02;

var i: integer;

procedure foo(v: integer);
begin
  i := (1 + 2) * v
end foo;

function bar(p1, p2, p3, p4: integer): integer;
begin
  return p1 + p2 + p3 * p4
end bar;

begin
  foo(1);
  bar(1,2,3,4)
end test02.
