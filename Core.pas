unit Core;

interface

uses SysUtils, Common;

var
  _IP : TAddress;
  _SP : TAddress;
  Flags : Byte;
  InstructionTable : Array[Word] of procedure;

const
  CFlag = 4;
  NFlag = 2;
  ZFlag = 1;

type
  EBadInstructionException = class(Exception);

procedure Reset;
procedure ExecuteNext;
procedure InvalidInstruction;

implementation

uses Memory, Instructions;

procedure ExecuteNext;
begin
  InstructionTable[Swap(P16Cell(GetCellAddr(_IP, 2))^)];
end;

procedure Reset;
begin
  _IP := 0;
  _SP := 0;
  Flags := 0;
end;

procedure InvalidInstruction;
begin
  raise EBadInstructionException.Create('Invalid instruction at '
          + IntToHex(_IP, 8) + ': ' + IntToHex(Swap(P16Cell(GetCellAddr(_IP, 2))^), 4));
end;

end.
