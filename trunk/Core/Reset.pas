unit Reset;

interface

type
  TResetProc = procedure;

procedure DoReset;
procedure Attach(ResetProc : TResetProc);
procedure Detach(ResetProc : TResetProc);

implementation

uses Classes;

type
  PResetCallback = ^TResetCallback;
  TResetCallback = record
    proc : TResetProc;
    next : PResetCallback;
  end;

var
  ResetList : TList;

procedure DoReset;
var i : Integer;
begin
  for i := 0 to ResetList.Count - 1 do
    TResetProc(ResetList[i]);
end;

procedure Attach(ResetProc : TResetProc);
begin
  ResetList.Add(@ResetProc);
end;

procedure Detach(ResetProc : TResetProc);
begin
  ResetList.Remove(@ResetProc);
end;

initialization
  ResetList := TList.Create;
finalization
  ResetList.Free;
end.
