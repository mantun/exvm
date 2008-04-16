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
  WriteCellAddr : Pointer;
  WriteCellBuf : TCell;

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
begin
  Assert(WriteCellAddr = nil);
  PackNo := address shr 16;
  PackOffs := address and $FFFF;
  pack := GetPack(PackNo);
  if PackOffs <= $10000 - size then
    Result := @(pack^[PackOffs])
  else
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
