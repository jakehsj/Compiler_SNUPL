//
// test04
//
// parser test:
// - functions and procedures
//

module test04;

var a  : integer;

function bar(): integer;
begin
  return 1
end bar;

procedure foo();
begin
end foo;

begin
  a := bar();
  foo()
end test04.
