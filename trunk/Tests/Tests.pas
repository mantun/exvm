unit Tests;

interface

procedure RunAllTests;

implementation

uses SysUtils, Common, Memory, Registers, Core, ImageFile;

type
  TBytes = array[Word] of Byte;
  PBytes = ^TBytes;

  TVMState = record
    _IP, _SP : TAddress;
    _Flags : Cardinal;
    Registers : TRegistersFile;
  end;

function GetState : TVMState;
begin
  Result._IP := Core._IP;
  Result._SP := Core._SP;
  Result._Flags := Core._Flags;
  Result.Registers := RegistersFile;
end;

procedure ClearAll;
begin
  Memory.Clear;
  Registers.Clear;
  Core.Reset;
end;

procedure CompareStates(const s1, s2 : TVMState; TestName : String);
var
  i : Integer;
  fl1, fl2 : Cardinal;
begin
  Assert(s1._IP = s2._IP,
          TestName + ': IP'#13#10 + IntToHex(s1._IP, 8) + ' ' + IntToHex(s2._IP, 8));
  Assert(s1._SP = s2._SP,
          TestName + ': SP'#13#10 + IntToHex(s1._SP, 8) + ' ' + IntToHex(s2._SP, 8));
  fl1 := s1._Flags and (_ZF or _CF or _NF or _OF);
  fl2 := s2._Flags and (_ZF or _CF or _NF or _OF);
  Assert(fl1 = fl2, TestName + ': FL'#13#10 + IntToHex(fl1, 4) + ' ' + IntToHex(fl2, 4));
  for i := Low(s1.Registers) to High(s1.Registers) do
    Assert(s1.Registers[i] = s2.Registers[i],
            TestName + ': R[' + IntToHex(i, 2) + ']'#13#10 + IntToHex(s1.Registers[i], 8) + ' ' + IntToHex(s2.Registers[i], 8));
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
  SingleStep;
  s.Registers[$02] := $12345678 + $87654321;
  s._Flags := s._Flags or _NF;
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
  SingleStep;
  s.Registers[$02] := ($CDEF + $4321) and $FFFF or $87650000;
  s._Flags := s._Flags or _CF;
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
  SingleStep;
  s.Registers[$02] := ($EF + $21) and $FF or $87654300;
  s._Flags := s._Flags or _CF;
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
  SingleStep;
  s.Registers[$02] := $12345678 + $87654321;
  s._Flags := s._Flags or _NF;
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
  SingleStep;
  s.Registers[$02] := $12345678 + $87654321;
  s._Flags := s._Flags or _NF;
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
  SingleStep;
  s.Registers[$02] := $12345678 + $87654321;
  s._Flags := s._Flags or _NF;
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
  SingleStep;
  s.Registers[$02] := $87654321 + $12345678;
  s._Flags := s._Flags or _NF;
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
  SingleStep;
  s.Registers[$02] := $87654321 + $12345678;
  s._Flags := s._Flags or _NF;
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
  SingleStep;
  s.Registers[$02] := $87654321 + $12345678;
  s._Flags := s._Flags or _NF;
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
  SingleStep;
  s.Registers[$02] := $87654321 + $12345678;
  s._Flags := s._Flags or _NF;
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
  SingleStep;
  s.Registers[$02] := Integer($FFFFFFFE) + $12345678;
  s._Flags := s._Flags or _CF;
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
  SingleStep;
  s.Registers[$03] := $12345678 + $87654321;
  s._Flags := s._Flags or _NF;
  s._IP := s._IP + 7;
  CompareStates(s, GetState, 'Test_Add_Imm_RegInd_32');
end;

procedure Test_Add_Reg_Reg_Right_32;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 4);
  p^[0] := $01; // add
  p^[1] := $21; // dest:reg, src:reg, size:32
  p^[2] := $02; // dest reg no
  p^[3] := $03; // src reg no
  RegistersFile[$02] := $87654321;
  RegistersFile[$03] := $12345678;
  s := GetState;
  SingleStep;
  s.Registers[$03] := $12345678 + $87654321;
  s._Flags := s._Flags or _NF;
  s._IP := s._IP + 4;
  CompareStates(s, GetState, 'Test_Add_Reg_Reg_Right_32');
end;

procedure Test_Add_Imm_Reg_Right_32;
var p : PBytes;
begin
  ClearAll;
  p := GetCellAddr(_IP, 7);
  p^[0] := $01; // add
  p^[1] := $20; // dir:right, dest:reg, src:imm, size:32
  p^[2] := $02; // reg no
  P32Cell(@(p^[3]))^ := $12345678;
  RegistersFile[$02] := $87654321;
  try
    SingleStep;
    Assert(False, 'Test_Add_Imm_Reg_Right_32');
  except
    on EBadInstructionException do ;
    on Exception do raise;
  end;
end;

procedure Test_Sbb_Reg_Reg_32;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 4);
  p^[0] := $04; // sbb
  p^[1] := $01; // dest:reg, src:reg, size:32
  p^[2] := $02; // dest reg no
  p^[3] := $03; // src reg no
  RegistersFile[$02] := $7654321;
  RegistersFile[$03] := $1234567;
  _Flags := _Flags or _CF;
  s := GetState;
  SingleStep;
  s.Registers[$02] := $7654321 - $1234567 - 1;
  s._Flags := s._Flags and not _CF;
  s._IP := s._IP + 4;
  CompareStates(s, GetState, 'Test_Sbb_Reg_Reg_32');
end;

procedure Test_Cpc_Reg_Reg_32;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 4);
  p^[0] := $06; // cpc
  p^[1] := $01; // dest:reg, src:reg, size:32
  p^[2] := $02; // dest reg no
  p^[3] := $03; // src reg no
  RegistersFile[$02] := $1234567;
  RegistersFile[$03] := $1234567;
  _Flags := _Flags or _CF;
  s := GetState;
  SingleStep;
  s._IP := s._IP + 4;
  s._Flags := s._Flags or _NF;
  CompareStates(s, GetState, 'Test_Cpc_Reg_Reg_32');
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
  SingleStep;
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
    SingleStep;
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
    SingleStep;
    Assert(False, 'Test_Not_ShortImm_32');
  except
    on EBadInstructionException do ;
    on Exception do raise;
  end;
end;

procedure Test_Clr_Reg_32;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 3);
  p^[0] := $0F; // jmp rel
  p^[1] := $01; // offset
  p^[2] := $02; // next instr
  RegistersFile[$02] := $ABCDEF;
  s := GetState;
  SingleStep;
  s.Registers[$02] := 0;
  s._IP := s._IP + 3;
  CompareStates(s, GetState, 'Test_Clr_Reg_32');
end;

procedure Test_Jmp_Rel;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 2);
  p^[0] := $80; // jmp rel
  p^[1] := $AA; // offset
  p^[2] := $BB; // next instr
  s := GetState;
  SingleStep;
  s._IP := s._IP + $FFFFFFAA;
  CompareStates(s, GetState, 'Test_Jmp_Rel');
end;

procedure Test_Jmp_Reg_Abs;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 3);
  p^[0] := $81; // jmp abs
  p^[1] := $01; // src:reg, size:32
  p^[2] := $02; // reg no
  RegistersFile[$02] := $ABABABAB;
  s := GetState;
  SingleStep;
  s._IP := $ABABABAB;
  CompareStates(s, GetState, 'Test_Jmp_Reg_Abs');
end;

procedure Test_Call_Rel;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  _IP := $100;
  _SP := $200;
  p := GetCellAddr(_IP, 2);
  p^[0] := $82; // jmp rel
  p^[1] := $25; // offset
  p^[2] := $BB; // next instr
  s := GetState;
  SingleStep;
  s._IP := s._IP + $25;
  s._SP := s._SP - 4;
  CompareStates(s, GetState, 'Test_Call_Rel');
  p := GetCellAddr(s._SP, 4);
  Assert(P32Cell(p)^ = $102, 'Test_Call_Rel: return address'#13#10 +
      IntToHex($102, 8) + ' ' + IntToHex(P32Cell(p)^, 8));
end;

procedure Test_jc_true;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 2);
  p^[0] := $8A; // jc
  p^[1] := $25; // offset
  p^[2] := $BB; // next instr
  _Flags := _Flags or _CF;
  s := GetState;
  SingleStep;
  s._IP := s._IP + $25;
  CompareStates(s, GetState, 'Test_jc_true');
end;

procedure Test_jz_false;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 2);
  p^[0] := $88; // jz
  p^[1] := $25; // offset
  p^[2] := $BB; // next instr
  _Flags := _Flags and not _ZF;
  s := GetState;
  SingleStep;
  s._IP := s._IP + $2;
  CompareStates(s, GetState, 'Test_jz_false');
end;

procedure Test_jge_true;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 2);
  p^[0] := $8E; // jge
  p^[1] := $25; // offset
  p^[2] := $BB; // next instr
  _Flags := _Flags and not _OF;
  _Flags := _Flags and not _CF;
  s := GetState;
  SingleStep;
  s._IP := s._IP + $25;
  CompareStates(s, GetState, 'Test_ge_true');
end;

procedure Test_Ret;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 1);
  p^[0] := $84; // ret
  p^[1] := $BB; // next instr
  _SP := $100;
  p := GetCellAddr(_SP, 4);
  P32Cell(p)^ := $12345678;
  s := GetState;
  SingleStep;
  s._IP := $12345678;
  s._SP := s._SP + 4;
  CompareStates(s, GetState, 'Test_Ret');
end;

procedure Test_Push_Reg_32;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 3);
  p^[0] := $A5; // push
  p^[1] := $01; // src:reg, size:32
  p^[2] := $02; // regno
  _SP := $100;
  RegistersFile[$02] := $12345678;
  s := GetState;
  SingleStep;
  s._IP := s._IP + 3;
  s._SP := s._SP - 4;
  CompareStates(s, GetState, 'Test_Push_Reg_32');
  p := GetCellAddr(s._SP, 4);
  Assert(P32Cell(p)^ = $12345678, 'Test_Push_Reg_32: value'#13#10 +
    '12345678' + IntToHex(P32Cell(p)^, 8));
end;

procedure Test_Pop_Reg_32;
var
  p : PBytes;
  s : TVMState;
begin
  ClearAll;
  p := GetCellAddr(_IP, 3);
  p^[0] := $A6; // pop
  p^[1] := $01; // src:reg, size:32
  p^[2] := $02; // regno
  _SP := $100;
  p := GetCellAddr(_SP, 4);
  P32Cell(p)^ := $12345678;
  s := GetState;
  SingleStep;
  s._IP := s._IP + 3;
  s._SP := s._SP + 4;
  s.Registers[$02] := $12345678;
  CompareStates(s, GetState, 'Test_Pop_Reg_32');
end;

procedure Test_Sum;
{
0000: A0 00 00 00 01 00 00   mov r0, $100
0007: A0 08 01 00            mov r1, 0
000B: A0 08 02 00            mov r2, 0
000F:                      loop:
000F: 01 85 02 00            add.b r2, [r0]
0013: 0C 01 00               inc r0
0016: 0C 01 01               inc r1
0019: 05 08 01 0A            cmp r1, 10
001D: 8A F2                  jc loop
001F: A0 24 02 FF 00 00 00   mov [$FF], r2
0016: AA                     halt             
}
const
  bytes : array[1..$27] of byte = (
    $A0, $00, $00, $00, $01, $00, $00, //   mov r0, $100
    $A0, $08, $01, $00,                //   mov r1, 0
    $A0, $08, $02, $00,                //   mov r2, 0
                                       // loop:
    $01, $85, $02, $00,                //   add.b r2, [r0]
    $0C, $01, $00,                     //   inc r0
    $0C, $01, $01,                     //   inc r1
    $05, $08, $01, $0A,                //   cmp r1, 10
    $8A, $F2,                          //   jc loop
    $A0, $24, $02, $FF, $00, $00, $00, //   mov [$FF], r2
    $AA                                //   halt
);
var
  p : PBytes;
  s : TVMState;
  i : Integer;
  sum : Byte;
begin
  ClearAll;
  _IP := $10;
  p := GetCellAddr(_IP, SizeOf(bytes));
  Move(bytes, p^, SizeOf(bytes));

  p := GetCellAddr($100, 10);
  sum := 0;
  for i := 1 to 10 do begin
    p^[i - 1] := i * 2;
    sum := sum + i * 2;
  end;

  s := GetState;
  Run;

  p := GetCellAddr($FF, 1);
  Assert(P8Cell(p)^ = sum, 'Test_Sum: sum'#13#10 +
      IntToHex(sum, 8) + ' ' + IntToHex(P32Cell(p)^, 8));

  s.Registers[0] := $10A;
  s.Registers[1] := 10;
  s.Registers[2] := sum;
  s._IP := s._IP + SizeOf(bytes) - 1;
  s._Flags := s._Flags or _ZF;
  CompareStates(s, GetState, 'Test_Sum');
end;

procedure Test_CrossPageRead;
var p : PBytes;
begin
  p := GetCellAddr($FFFE, 2);
  p^[0] := $21;
  p^[1] := $43;
  p := GetCellAddr($10000, 2);
  p^[0] := $65;
  p^[1] := $87;
  p := GetCellAddr($FFFE, 4);
  Assert(P32Cell(p)^ = $87654321, 'Test_CrossPageRead');
end;

procedure Test_CrossPageWrite;
var p : PBytes;
begin
  p := GetCellAddrWrite($FFFE, 4);
  P32Cell(p)^ := $87654321;
  CommitCellWrite;
  p := GetCellAddr($FFFE, 2);
  Assert(P16Cell(p)^ = $4321, 'Test_CrossPageWrite');
  p := GetCellAddr($10000, 2);
  Assert(P16Cell(p)^ = $8765, 'Test_CrossPageWrite');
end;

procedure Test_LoadImageFile;
var
  f : File;
  s : string[8];
  k : Cardinal;
begin
  ClearAll;
  s := 'EXVMIMG'#0;
  AssignFile(f, 'test.exvmimage');
  Rewrite(f, 1);
  BlockWrite(f, s[1], Length(s));
  k := _IP;
  BlockWrite(f, k, SizeOf(k));
  k := 1;
  BlockWrite(f, k, SizeOf(k));
  k := $AA;
  BlockWrite(f, k, 1);
  CloseFile(f);
  LoadImageFile('test.exvmimage');
  Run;
end;

procedure Test_AddressingModes_Direction;
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
  Test_Add_ShortImm_Reg_32;
  Test_Add_Imm_RegInd_32;
  Test_Add_Reg_Reg_Right_32;
  Test_Add_Imm_Reg_Right_32;
end;

procedure Test_SingleInstructions;
begin
  Test_AddressingModes_Direction;

  Test_Sbb_Reg_Reg_32;
  Test_Cpc_Reg_Reg_32;

  Test_Not_Reg_32;
  Test_Not_Imm_32;
  Test_Not_ShortImm_32;

  Test_Clr_Reg_32;

  Test_Jmp_Rel;
  Test_Jmp_Reg_Abs;

  Test_Call_Rel;

  Test_jc_true;
  Test_jz_false;
  Test_jge_true;

  Test_Ret;

  Test_Push_Reg_32;
  Test_Pop_Reg_32;
end;

procedure RunAllTests;
begin
  Test_SingleInstructions;

  Test_Sum;

  Test_CrossPageRead;
  Test_CrossPageWrite;

  Test_LoadImageFile;
  // other instructions
  // storage
  // exceptions
end;

end.
