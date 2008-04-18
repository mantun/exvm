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
procedure Run;
procedure SingleStep;

implementation

uses Memory, Instructions;

var
  Stopped : Boolean;

procedure Run;
asm
  push esi
  push edi
  push ebx

  mov Stopped, 0
@@step:
  mov eax, _IP
  mov edx, 7
  call GetCellAddr
  mov ebx, eax
  mov ax, [eax]
  xchg al, ah
  movzx eax, ax
  mov eax, [eax * 4 + InstructionTable]
  call eax
  cmp Stopped, 0
  jz @@step

  pop ebx
  pop edi
  pop esi
end;

procedure SingleStep;
asm
  push esi
  push edi
  push ebx

  mov eax, _IP
  mov edx, 7
  call GetCellAddr
  mov ebx, eax
  mov ax, [eax]
  xchg al, ah
  movzx eax, ax
  mov eax, [eax * 4 + InstructionTable]
  call eax

  pop ebx
  pop edi
  pop esi
end;

procedure Reset;
begin
  _IP := 0;
  _SP := 0;
  Flags := 0;
  Stopped := True;
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

procedure HaltInstruction;
begin
  Stopped := True;
end;

type TProcedure = procedure;
procedure PutBulkInstruction(opcode : Byte; proc : TProcedure);
var i : Integer;
begin
  for i := 0 to $FF do
    InstructionTable[opcode shl 8 + i] := proc;
end;

initialization
  InitEmptyInstructionTable;
  Instructions.InitInstructionTable;
  PutBulkInstruction($AA, @HaltInstruction);
end.
