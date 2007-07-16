unit Registers;

interface

uses Common;

function getreg(index : Byte; size : TOpSize) : TCell;
procedure setreg(index : Byte; size : TOpSize; value : TCell);

implementation

var
  RegisterFile : Array[Byte] of Cardinal;

function getreg(index : Byte; size : TOpSize) : TCell;
begin
  case size of
    sz32 : Result := P32Cell(@(RegisterFile[index]))^;
    sz16 : Result := P16Cell(@(RegisterFile[index]))^;
    sz8 : Result := P8Cell(@(RegisterFile[index]))^;
    else begin Assert(False); Result := 0; end;
  end
end;

procedure setreg(index : Byte; size : TOpSize; value : TCell);
begin
  case size of
    sz32 : P32Cell(@(RegisterFile[index]))^:= value;
    sz16 : P16Cell(@(RegisterFile[index]))^:= value;
    sz8 : P8Cell(@(RegisterFile[index]))^:= value;
    else Assert(False);
  end
end;

end.
