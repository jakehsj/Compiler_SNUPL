//
// test18
//
// semantic analysis
// - arrays
//

module test18;

const
  N : integer = 10;
  M : integer = 20;
  K : integer = 5;

var
  A : integer[N][K];
  B : integer[K][M];
  C : integer[10][20];

begin
  A[0][K-7] := 5
end test18.
