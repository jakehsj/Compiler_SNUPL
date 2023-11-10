//
// test06
//
// semantic analysis
// - multiply defined identifier
//

module test06;

procedure test();
begin
end test;

function test(i: integer): integer;
begin
  return 2*i
end test;

end test06.
