unit Memory;

interface

Uses Common, SysUtils;

type
  EMemoryManagerException = class(Exception);

function GetCellAddr(address : TAddress; size : Integer) : Pointer;
function GetCellAddrWrite(address : TAddress; size : Integer) : Pointer;
procedure CommitCellWrite;
procedure Clear;

implementation

uses Classes;

type
  TMemPack = Array[0..$FFFF] of Byte;
  PMemPack = ^TMemPack;
var
  MemPacks : Array[0..$FFFF] of PMemPack;

  CrossPackWrite : record
    Address : TAddress;
    Pack1, Pack2 : PMemPack;
    Size : Integer;
  end;
  CrossPackBuff : array[0..15] of Byte;

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
  if PackOffs <= $10000 - size then
    Result := @(pack^[PackOffs])
  else begin
    SizePart1 := $10000 - PackOffs;
    Move(pack^[PackOffs], CrossPackBuff, SizePart1);
    pack := GetPack((PackNo + 1) and $FFFF);
    Move(pack^[0], CrossPackBuff[SizePart1], size - SizePart1);
    Result := @CrossPackBuff; 
  end;
    raise EMemoryManagerException.Create('Page bounds violated');
end;

function GetCellAddrWrite(address : TAddress; size : Integer) : Pointer;
begin
  Result := GetCellAddr(address, size);
end;

procedure CommitCellWrite;
begin
  ;
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

end.
