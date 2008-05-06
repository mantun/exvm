unit Registers;

interface

uses Common;

type
  TRegistersFile = Array[Byte] of TCell;
var
  RegistersFile : TRegistersFile;

implementation

uses Reset;

procedure Clear;
begin
  FillChar(RegistersFile, SizeOf(RegistersFile), 0);
end;

initialization
  Reset.Attach(Clear);
end.
