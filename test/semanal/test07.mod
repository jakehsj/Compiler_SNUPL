//
// test07
//
// semantic analysis
// - invalid subroutine call
//

module test07;

var foo: integer;

procedure bar();
begin
end bar;

begin
  foo()
end test07.
