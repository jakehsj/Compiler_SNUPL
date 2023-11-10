//
// test20
//
// semantic analysis
// - subroutine parameters
//

module test20;

procedure foo(a, b, c: integer);
begin
end foo;

begin
  foo(1, 2, 3);
  foo(1, 2);
  foo(1, 2, 3, 4)
end test20.
