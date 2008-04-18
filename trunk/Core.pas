unit Core;

interface

uses SysUtils, Common;

var
  _IP : TAddress;
  _SP : TAddress;
  Flags : Cardinal;
  InstructionTable : Array[Word] of procedure;

const
  CFlag = 1;
  NFlag = 1 shl 7;
  ZFlag = 1 shl 6;

type
  EBadInstructionException = class(Exception);

procedure Reset;
procedure ExecuteNext;

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

procedure InitEmptyInstructionTable;
var i : Integer;
begin
  for i := Low(InstructionTable) to High(InstructionTable) do
    InstructionTable[i] := @InvalidInstruction;
end;

initialization
  InitEmptyInstructionTable;
  Instructions.InitInstructionTable;
end.
