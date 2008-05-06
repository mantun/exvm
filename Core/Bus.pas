unit Bus;

interface

Uses Common;

type
  TPeriferial = class
    procedure Write(address : TAddress; data : TCell); virtual; abstract;
    function Read(address : TAddress; out data : TCell) : Boolean; virtual; abstract;
  end;

procedure Write(address : TAddress; data : TCell);
function Read(address : TAddress) : TCell;
procedure Attach(device : TPeriferial);
procedure Detach(device : TPeriferial);

implementation

uses Classes;

var
  PeriferialList : TList;

procedure Write(address : TAddress; data : TCell);
var i : Integer;
begin
  for i := 0 to PeriferialList.Count - 1 do
    TPeriferial(PeriferialList[i]).Write(address, data);
end;

function Read(address : TAddress) : TCell;
var i : Integer;
begin
  Result := 0;
  for i := 0 to PeriferialList.Count - 1 do
    if TPeriferial(PeriferialList[i]).Read(address, Result) then
      Break;
end;

procedure Attach(device : TPeriferial);
begin
  PeriferialList.Add(device);
end;

procedure Detach(device : TPeriferial);
begin
  PeriferialList.Remove(device);
end;

initialization
  PeriferialList := TList.Create;
finalization
  PeriferialList.Free;
end.
