unit Registers;

interface

uses Common;

type
  TRegistersFile = Array[Byte] of TCell;
var
  RegistersFile : TRegistersFile;

procedure Clear;

implementation

procedure Clear;
begin
  FillChar(RegistersFile, SizeOf(RegistersFile), 0);
end;

end.
