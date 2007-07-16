unit Memory;

interface

Uses Common, SysUtils;

type
  EMemoryManagerException = class(Exception);

function getmem(address : TAddress; size : TOpSize) : TCell;
procedure setmem(address : TAddress; size : TOpSize; value : TCell);

implementation

uses Classes;

type
  TMemPack = Array[0..$FFFF] of Byte;
  PMemPack = ^TMemPack;
var
  MemPacks : Array[0..$FFFF] of PMemPack;

function GetPack(PackNo : TAddress) : PMemPack;
begin
  Result := MemPacks[PackNo];
  if Result = nil then begin
    New(Result);
    FillChar(Result^, SizeOf(TMemPack), 0);
    MemPacks[PackNo] := Result;
  end;
end;

function getmem(address : TAddress; size : TOpSize) : TCell;
var
  Pack : PMemPack;
  PackNo, PackOffs : TAddress;
begin
  PackNo := address shr 16;
  PackOffs := address and $FFFF;
  pack := GetPack(PackNo);
  if PackOffs <= $10000 - CountBytes[size] then
    case size of
      sz32 : Result := P32Cell(@(pack^[PackOffs]))^;
      sz16 : Result := P16Cell(@(pack^[PackOffs]))^;
      sz8 : Result := P8Cell(@(pack^[PackOffs]))^;
      else begin Assert(False); Result := 0; end;
    end
  else
    raise EMemoryManagerException.Create('Page bounds violated');
end;

procedure setmem(address : TAddress; size : TOpSize; value : TCell);
var
  Pack : PMemPack;
  PackNo, PackOffs : TAddress;
begin
  PackNo := address shr 16;
  PackOffs := address and $FFFF;
  pack := GetPack(PackNo);
  if PackOffs <= $10000 - CountBytes[size] then
    case size of
      sz32 : P32Cell(@(pack^[PackOffs]))^:= value;
      sz16 : P16Cell(@(pack^[PackOffs]))^:= value;
      sz8 : P8Cell(@(pack^[PackOffs]))^:= value;
      else Assert(False);
    end
  else
    raise EMemoryManagerException.Create('Page bounds violated');
end;

end.
