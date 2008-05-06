unit Memory;

interface

Uses Common, SysUtils;

type
  EMemoryManagerException = class(Exception);

const
  MaxReqSize = 16;

function GetCellAddr(address : TAddress; size : Integer) : Pointer;
function GetCellAddrWrite(address : TAddress; size : Integer) : Pointer;
procedure CommitCellWrite;
procedure Clear;

implementation

uses Classes, Reset;

type
  TMemPack = Array[0..$FFFF] of Byte;
  PMemPack = ^TMemPack;
var
  MemPacks : Array[0..$FFFF] of PMemPack;

  CrossPackWrite : record
    Address : TAddress;
    Pack1, Pack2 : PMemPack;
    PackOffs : TAddress;
    SizePart1 : Integer;
    Size : Integer;
  end;
  CrossPackBuff : array[0..MaxReqSize - 1] of Byte;

function GetPack(PackNo : TAddress) : PMemPack;
begin
  Result := MemPacks[PackNo];
  if Result = nil then begin
    New(Result);
    FillChar(Result^, SizeOf(TMemPack), 0);
    MemPacks[PackNo] := Result;
  end;
end;

function GetCellAddr(address : TAddress; size : Integer) : Pointer;
var
  Pack : PMemPack;
  PackNo, PackOffs : TAddress;
  SizePart1 : Integer;
begin
  Assert(CrossPackWrite.Address = 0);
  PackNo := address shr 16;
  PackOffs := address and $FFFF;
  pack := GetPack(PackNo);
  if PackOffs <= TAddress($10000 - size) then
    Result := @(pack^[PackOffs])
  else begin
    SizePart1 := $10000 - PackOffs;
    Move(pack^[PackOffs], CrossPackBuff, SizePart1);
    CrossPackWrite.Pack1 := pack;
    pack := GetPack((PackNo + 1) and $FFFF);
    Move(pack^[0], CrossPackBuff[SizePart1], size - SizePart1);
    Result := @CrossPackBuff;
    CrossPackWrite.Pack2 := pack;
    CrossPackWrite.Size := size;
    CrossPackWrite.PackOffs := PackOffs;
    CrossPackWrite.SizePart1 := SizePart1;
  end;
end;

function GetCellAddrWrite(address : TAddress; size : Integer) : Pointer;
begin
  CrossPackWrite.Size := 0;
  Result := GetCellAddr(address, size);
  CrossPackWrite.Address := address;
end;

procedure CommitCellWrite;
begin
  Assert(CrossPackWrite.Address <> 0);
  if CrossPackWrite.Size <> 0 then begin
    Move(CrossPackBuff, CrossPackWrite.Pack1^[CrossPackWrite.PackOffs], CrossPackWrite.SizePart1);
    Move(CrossPackBuff[CrossPackWrite.SizePart1], CrossPackWrite.Pack2^[0], CrossPackWrite.Size - CrossPackWrite.SizePart1);
  end;
  CrossPackWrite.Address := 0;
end;

procedure Clear;
var
  i : Integer;
  p : PMemPack;
begin
  for i := Low(MemPacks) to High(MemPacks) do begin
    p := MemPacks[i];
    if p <> nil then
      Dispose(p);
    MemPacks[i] := nil;
  end;
end;

initialization
  Reset.Attach(Clear);
end.
