//
// test19
//
// semantic analysis
// - arrays
//

module test19;

procedure foo(a: integer[]);
begin
end foo;

procedure bar(a: integer[10]);
begin
  foo(a)
end bar;

var A: integer[10];
    B: integer[20];
    C: integer[10][10];

begin
  bar(A);
  bar(B);
  bar(C)
end test19.
