unit Tests;

interface

procedure RunAllTests;

implementation

uses SysUtils, Common, Memory, Registers, Core;

type
  TBytes = array[Word] of Byte;
  PBytes = ^TBytes;

  TVMState = record
    _IP, _SP : TAddress;
    Flags : Byte;
    Registers : TRegistersFile;
  end;

function GetState : TVMState;
begin
  Result._IP := Core._IP;
  Result._SP := Core._SP;
  Result.Flags := Core.Flags;
  Result.Registers := RegistersFile;
end;

procedure ClearAll;
begin
  Memory.Clear;
  Registers.Clear;
  Core.Reset;
end;

procedure CompareStates(const s1, s2 : TVMState; TestName : String);
var i : Integer;
begin
  Assert(s1._IP = s2._IP, TestName + ': IP'#13#10 + IntToHex(s1._IP, 8) + ' ' + IntToHex(s2._IP, 8));
  Assert(s1._SP = s2._SP, TestName + ': SP'#13#10 + IntToHex(s1._SP, 8) + ' ' + IntToHex(s2._SP, 8));
  Assert(s1.Flags = s2.Flags, TestName + ': FL'#13#10 + IntToHex(s1.Flags, 2) + ' ' + IntToHex(s2.Flags, 2));
  for i := Low(s1.Registers) to High(s1.Registers) do
    Assert(s1.Registers[i] = s2.Registers[i], TestName + ': R[' + IntToHex(i, 2) + ']'#13#10 + IntToHex(s1.Registers[i], 8) + ' ' + IntToHex(s2.Registers[i], 8));
end;

procedure Test_Add_Imm_Reg_32;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 7);
  p^[0] := $01; // add
  p^[1] := $00; // dest:reg, src:imm, size:32
  p^[2] := $02; // reg no
  P32Cell(@(p^[3]))^ := $12345678;
  RegistersFile[$02] := $87654321;
  s := GetState;
  ExecuteNext;
  s.Registers[$02] := $12345678 + $87654321;
  s._IP := s._IP + 7;
  CompareStates(s, GetState, 'Test_Add_Imm_Reg_32');
end;

procedure Test_Add_Imm_Reg_16;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 5);
  p^[0] := $01; // add
  p^[1] := $40; // dest:reg, src:imm, size:32
  p^[2] := $02; // reg no
  P32Cell(@(p^[3]))^ := $BBBBCDEF; // bb is next instr
  RegistersFile[$02] := $87654321;
  s := GetState;
  ExecuteNext;
  s.Registers[$02] := ($CDEF + $4321) and $FFFF or $87650000;
  s._IP := s._IP + 5;
  CompareStates(s, GetState, 'Test_Add_Imm_Reg_16');
end;

procedure Test_Add_Imm_Reg_8;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 4);
  p^[0] := $01; // add
  p^[1] := $80; // dest:reg, src:imm, size:32
  p^[2] := $02; // reg no
  P32Cell(@(p^[3]))^ := $BBBBBBEF; // bb is next instr
  RegistersFile[$02] := $87654321;
  s := GetState;
  ExecuteNext;
  s.Registers[$02] := ($EF + $21) and $FF or $87654300;
  s._IP := s._IP + 4;
  CompareStates(s, GetState, 'Test_Add_Imm_Reg_8');
end;

procedure Test_Add_Reg_Reg_32;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 4);
  p^[0] := $01; // add
  p^[1] := $01; // dest:reg, src:reg, size:32
  p^[2] := $02; // dest reg no
  p^[3] := $03; // src reg no
  RegistersFile[$02] := $87654321;
  RegistersFile[$03] := $12345678;
  s := GetState;
  ExecuteNext;
  s.Registers[$02] := $12345678 + $87654321;
  s._IP := s._IP + 4;
  CompareStates(s, GetState, 'Test_Add_Reg_Reg_32');
end;

procedure Test_Add_RegInd_Reg_32;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 4);
  p^[0] := $01; // add
  p^[1] := $02; // dest:reg, src:regind, size:32
  p^[2] := $02; // dest reg no
  p^[3] := $03; // src reg no
  RegistersFile[$02] := $87654321;
  RegistersFile[$03] := $05;
  RegistersFile[$05] := $12345678;
  s := GetState;
  ExecuteNext;
  s.Registers[$02] := $12345678 + $87654321;
  s._IP := s._IP + 4;
  CompareStates(s, GetState, 'Test_Add_RegInd_Reg_32');
end;

procedure Test_Add_RegIndDispl_Reg_32;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 5);
  p^[0] := $01; // add
  p^[1] := $03; // dest:reg, src:regind+displ, size:32
  p^[2] := $02; // dest reg no
  p^[3] := $03; // src reg base
  p^[4] := $FE; // src reg no
  RegistersFile[$02] := $87654321;
  RegistersFile[$03] := $07;
  RegistersFile[$05] := $12345678;
  s := GetState;
  ExecuteNext;
  s.Registers[$02] := $12345678 + $87654321;
  s._IP := s._IP + 5;
  CompareStates(s, GetState, 'Test_Add_RegIndDispl_Reg_32');
end;

procedure Test_Add_Direct_Reg_32;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 7);
  p^[0] := $01; // add
  p^[1] := $04; // dest:reg, src:direct, size:32
  p^[2] := $02; // reg no
  P32Cell(@(p^[3]))^ := $20; // dest addr
  p := GetCellAddr($20, 4);
  P32Cell(p)^ := $12345678;
  RegistersFile[$02] := $87654321;
  s := GetState;
  ExecuteNext;
  s.Registers[$02] := $87654321 + $12345678;
  s._IP := s._IP + 7;
  CompareStates(s, GetState, 'Test_Add_Direct_Reg_32');
end;

procedure Test_Add_Indirect_Reg_32;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 4);
  p^[0] := $01; // add
  p^[1] := $05; // dest:reg, src:indirect, size:32
  p^[2] := $02; // reg no
  p^[3] := $03; // dest addr
  p := GetCellAddr($20, 4);
  P32Cell(p)^ := $12345678;
  RegistersFile[$03] := $20;
  RegistersFile[$02] := $87654321;
  s := GetState;
  ExecuteNext;
  s.Registers[$02] := $87654321 + $12345678;
  s._IP := s._IP + 4;
  CompareStates(s, GetState, 'Test_Add_Indirect_Reg_32');
end;

procedure Test_Add_IndirectDispl_Reg_32;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 8);
  p^[0] := $01; // add
  p^[1] := $06; // dest:reg, src:indirect+displ, size:32
  p^[2] := $02; // reg no
  p^[3] := $03; // dest addr
  P32Cell(@(p^[4]))^ := 5;
  p := GetCellAddr($25, 4);
  P32Cell(p)^ := $12345678;
  RegistersFile[$03] := $20;
  RegistersFile[$02] := $87654321;
  s := GetState;
  ExecuteNext;
  s.Registers[$02] := $87654321 + $12345678;
  s._IP := s._IP + 8;
  CompareStates(s, GetState, 'Test_Add_IndirectDispl_Reg_32');
end;

procedure Test_Add_IndirectShortDispl_Reg_32;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 5);
  p^[0] := $01; // add
  p^[1] := $0E; // dest:reg, src:indirect+shortdispl, size:32
  p^[2] := $02; // reg no
  p^[3] := $03; // dest addr
  p^[4] := 5;
  p^[5] := $BB;
  p := GetCellAddr($25, 4);
  P32Cell(p)^ := $12345678;
  RegistersFile[$03] := $20;
  RegistersFile[$02] := $87654321;
  s := GetState;
  ExecuteNext;
  s.Registers[$02] := $87654321 + $12345678;
  s._IP := s._IP + 5;
  CompareStates(s, GetState, 'Test_Add_IndirectShortDispl_Reg_32');
end;

procedure Test_Add_ShortImm_Reg_32;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 4);
  p^[0] := $01; // add
  p^[1] := $08; // dest:reg, src:shortimm, size:32
  p^[2] := $02; // reg no
  p^[3] := $FE;
  // MUST NOT ADD THIS!
  p^[4] := $BB; // next instr
  RegistersFile[$02] := $12345678;
  s := GetState;
  ExecuteNext;
  s.Registers[$02] := $FE + $12345678;
  s._IP := s._IP + 4;
  CompareStates(s, GetState, 'Test_Add_ShortImm_Reg_32');
end;

procedure Test_Add_Imm_RegInd_32;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 7);
  p^[0] := $01; // add
  p^[1] := $10; // dest:regind, src:imm, size:32
  p^[2] := $02; // reg no
  P32Cell(@(p^[3]))^ := $12345678;
  RegistersFile[$02] := $03;
  RegistersFile[$03] := $87654321;
  s := GetState;
  ExecuteNext;
  s.Registers[$03] := $12345678 + $87654321;
  s._IP := s._IP + 7;
  CompareStates(s, GetState, 'Test_Add_Imm_RegInd_32');
end;

procedure Test_Not_Reg_32;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 3);
  p^[0] := $0A; // not
  p^[1] := $01; // src:reg, size:32
  p^[2] := $02; // reg no
  RegistersFile[$02] := $87654321;
  s := GetState;
  ExecuteNext;
  s.Registers[$02] := not $87654321;
  s._IP := s._IP + 3;
  CompareStates(s, GetState, 'Test_Not_Reg_32');
end;

procedure Test_Not_Imm_32;
var p : PBytes;
begin
  ClearAll;
  p := GetCellAddr(_IP, 7);
  p^[0] := $0A; // not
  p^[1] := $00; // src:imm, size:32
  P32Cell(@(p^[2]))^ := $12345678;
  try
    ExecuteNext;
    Assert(False, 'Test_Not_Imm_32');
  except
    on EBadInstructionException do ;
    on Exception do raise;
  end;
end;

procedure Test_Not_ShortImm_32;
var p : PBytes;
begin
  ClearAll;
  p := GetCellAddr(_IP, 7);
  p^[0] := $0A; // not
  p^[1] := $08; // src:shortimm, size:32
  P32Cell(@(p^[2]))^ := $12345678;
  try
    ExecuteNext;
    Assert(False, 'Test_Not_ShortImm_32');
  except
    on EBadInstructionException do ;
    on Exception do raise;
  end;
end;

procedure RunAllTests;
begin
  Test_Add_Imm_Reg_32;
  Test_Add_Imm_Reg_16;
  Test_Add_Imm_Reg_8;
  Test_Add_Reg_Reg_32;
  Test_Add_RegInd_Reg_32;
  Test_Add_RegIndDispl_Reg_32;
  Test_Add_Direct_Reg_32;
  Test_Add_Indirect_Reg_32;
  Test_Add_IndirectDispl_Reg_32;
  Test_Add_IndirectShortDispl_Reg_32;
// add relative:8,16,32
// add+direction?
// fails! Test_Add_ShortImm_Reg_32;
  Test_Add_Imm_RegInd_32;
  Test_Not_Reg_32;
// fails! Test_Not_Imm_32;
// fails! Test_Not_ShortImm_32;

// branches: indirect+shortdispl, relative:8,16,32

end;

end.
