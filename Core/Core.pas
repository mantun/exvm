unit Core;

interface

uses SysUtils, Common;

var
  _IP : TAddress;
  _SP : TAddress;
  _Flags : Cardinal;
  InstructionTable : array[Word] of procedure;
  SysRegisters : array[Byte] of ^TCell;

const
  _CF = 1;
  _NF = 1 shl 7;
  _ZF = 1 shl 6;
  _OF = 1 shl 11;

type
  EBadInstructionException = class(Exception);

procedure Run;
procedure SingleStep;

implementation

uses Memory, Instructions, Reset;

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

procedure Clear;
begin
  _IP := $FFFFFFF0;
  _SP := 0;
  _Flags := 0;
  Stopped := True;
end;

procedure InvalidInstruction;
begin
  raise EBadInstructionException.Create('Invalid instruction at '
          + IntToHex(_IP, 8) + ': ' + IntToHex(Swap(P16Cell(GetCellAddr(_IP, 2))^), 4));
end;

procedure HaltInstruction;
begin
  Stopped := True;
end;

procedure InitSysRegisters;
type
  TRegs = array[Byte] of TCell;
var
  i : Integer;
  p : ^TRegs;
begin
  GetMem(p, SizeOf(TRegs));
  for i := 0 to High(Byte) do
    SysRegisters[i] := @(p^[i]);
  SysRegisters[0] := @_IP;
  SysRegisters[1] := @_Flags;
  SysRegisters[2] := @_SP;
end;

procedure InitEmptyInstructionTable;
var i : Integer;
begin
  for i := Low(InstructionTable) to High(InstructionTable) do
    InstructionTable[i] := @InvalidInstruction;
end;

type TProcedure = procedure;
procedure PutBulkInstruction(opcode : Byte; proc : TProcedure);
var i : Integer;
begin
  for i := 0 to $FF do
    InstructionTable[opcode shl 8 + i] := proc;
end;

initialization
  InitSysRegisters;
  InitEmptyInstructionTable;
  Instructions.InitInstructionTable;
  PutBulkInstruction($AA, @HaltInstruction);
  Reset.Attach(Clear);
end.
