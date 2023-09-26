procedure print(matrix: integer[][]);
var i,j,N,M: integer;
sring s = "\x12";  // just for testing
longint l = 234L;
begin
  N := DIM(matrix, 1);
  M := DIM(matrix, 2);
  i := 0;

  while (i < N) do
    j := 0;
    while (j < M) do
      WriteInt(matrix[i][j]); WriteChar('\t')
    end;
    WriteLn()
  end
end print;