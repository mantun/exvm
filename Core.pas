unit Core;

interface

uses Common;

var
  IP : TAddress;
  SP : TAddress;

procedure ExecuteNext;

implementation

uses SysUtils, Memory;

type
  TProcedure = procedure;
var
  InstructionTable : Array[Byte] of TProcedure;

procedure ExecuteNext;
var
  opcode : Byte;
begin
  opcode := getmem(IP, sz8);
  InstructionTable[opcode];
end;

procedure InitInstructionTable;
begin

end;

procedure BreakPointInstruction;
begin
  WriteLn('Breakpont at ' + IntToHex(IP, 8));
end;

initialization
  InitInstructionTable;

end.
