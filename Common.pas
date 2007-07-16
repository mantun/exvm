unit Common;

interface

type
  TAddress = Cardinal;
  T32Cell = Cardinal;
  T16Cell = Word;
  T8Cell = Byte;
  TCell = T32Cell;
  P32Cell = ^T32Cell;
  P16Cell = ^T16Cell;
  P8Cell = ^T8Cell;
  PCell = ^TCell;

  TOpSize = (sz32, sz16, sz8);

const
  CountBytes : Array[TOpSize] of Cardinal = (4, 2, 1);

implementation

end.
