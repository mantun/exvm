program ProcGen;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  StrUtils;

type
  TParser = class
  private
    FString : String;
    FTokPos : array of Integer;
    FTokLen : array of Integer;
    FCurrTok : Integer;

    procedure Tokenize;

    // Grammar:
    //   StringSequence -> StringEntity*
    //   StringEntity -> '{' StringSequence ('|' StringSequence)* '}'
    //   StringEntity -> <character sequence>
    function StringEntity : String;
    function StringSequence : String;
  public
    constructor Create(str : String);
    function Parse : String;
  end;

  TMacro = class
  private
    FName : String;
    FParams : array of String;
    FBody : String;
  public
    property Name : String read FName;
    constructor Create(head, body : String);
    function Expand(params : array of String) : String;
    function GetParamMatches(params : array of String) : Integer;
    function ToString : String;
  end;

{ Globals }

var
  Macros : TList;

function ArrayToString(params : array of String) : String;
var i : Integer;
begin
  Result := '';
  for i := Low(params) to High(params) do
    if Result = '' then
      Result := params[i]
    else
      Result := Result + ' ' + params[i];
end;

function FindMacro(const name : String; const params : array of String) : TMacro;
var
  i, mr, MatchRank : Integer;
  m : TMacro;
begin
  Result := nil;
  MatchRank := -1;
  for i := 0 to Macros.Count - 1 do begin
    m := TMacro(Macros[i]);
    if m.Name = name then begin
      mr := m.GetParamMatches(params);
      if mr > MatchRank then begin
        MatchRank := mr;
        Result := m;
      end else if (mr = MatchRank) and (mr <> -1) then begin
        raise Exception.Create('Ambiguous macro invocation: '#13#10'  1: '
                + Result.ToString + #13#10'  2: '
                + m.ToString + #13#10'  params: ' + ArrayToString(params));
      end;
    end;
  end;
  if Result = nil then
    raise Exception.Create('Cannot match macro invocation: ' + name + ' ' + ArrayToString(params));
end;

{ TParser }

constructor TParser.Create(str : String);
begin
  FString := str;
end;

procedure TParser.Tokenize;
var
  i, j, count : Integer;
  marks : array of Boolean;
begin
  SetLength(marks, Length(FString));
  FillChar(marks[0], Length(marks) * SizeOf(marks[0]), 0);
  count := 0;
  for i := 1 to Length(FString) do
    if FString[i] in ['{', '|', '}'] then begin
      marks[i - 1] := True;
      inc(count);
    end else
      if (i = 1) or (i > 1) and marks[i - 2] then
        inc(count);
  SetLength(FTokPos, count);
  SetLength(FTokLen, count);
  j := 0;
  for i := 0 to Length(marks) - 1 do
    if marks[i] or (i = 0) or (i > 0) and marks[i - 1] then begin
      FTokPos[j] := i + 1;
      Inc(j);
    end;
  for i := 0 to Length(FTokLen) - 2 do
    FTokLen[i] := FTokPos[i + 1] - FTokPos[i];
  FTokLen[High(FTokLen)] := Length(FString) - FTokPos[High(FTokLen)] + 1;
end;

function TParser.StringEntity : String;
var
  invocation, params : Array of String;
  macro : TMacro;
begin
  params := nil; // compiler complains without this?
  if FString[FTokPos[FCurrTok]] = '{' then begin
    Inc(FCurrTok);
    if FCurrTok >= Length(FTokPos) then
      raise Exception.Create('Missing value after {');
    SetLength(invocation, 1);
    invocation[0] := StringSequence;
    if FCurrTok >= Length(FTokPos) then
      raise Exception.Create('Missing }');
    while FString[FTokPos[FCurrTok]] = '|' do begin
      Inc(FCurrTok);
      if FCurrTok >= Length(FTokPos) then
        raise Exception.Create('Missing value after |');
      SetLength(invocation, Length(invocation) + 1);
      invocation[High(invocation)] := StringSequence;
    end;
    if FString[FTokPos[FCurrTok]] <> '}' then
      raise Exception.Create('Missing }');
    Inc(FCurrTok);
    params := Copy(invocation, 1, Length(invocation) - 1);
    macro := FindMacro(invocation[0], params);
    Result := macro.Expand(params);
  end else begin
    Result := Copy(FString, FTokPos[FCurrTok], FTokLen[FCurrTok]);
    Inc(FCurrTok);
  end;
end;

function TParser.StringSequence : String;
var c : Char;
begin
  Result := '';
  repeat
    c := FString[FTokPos[FCurrTok]];
    if (c <> '}') and (c <> '|') then
      Result := Result + StringEntity
    else
      Break;
  until FCurrTok >= Length(FTokPos);
end;

function TParser.Parse : String;
begin
  Tokenize;
  FCurrTok := 0;
  Result := StringSequence;
end;

{ TMacro }

constructor TMacro.Create(head, body : String);
var i, k : Integer;
begin
  i := 1;
  while (i <= Length(head)) and (head[i] = ' ') do Inc(i);
  k := i;
  while (i <= Length(head)) and (head[i] <> ' ') do Inc(i);
  FName := Copy(head, k, i - k);
  if FName = '' then
    raise Exception.Create('Missing macro name (check for consecutive empty lines)');
  repeat
    while (i <= Length(head)) and (head[i] = ' ') do Inc(i);
    k := i;
    while (i <= Length(head)) and (head[i] <> ' ') do Inc(i);
    if k <> i then begin
      SetLength(FParams, Length(FParams) + 1);
      FParams[High(FParams)] := Copy(head, k, i - k);
    end;
  until k = i;
  FBody := body;
end;

function TMacro.Expand(params : array of String) : String;
var
  i : Integer;
  p : TParser;
begin
  Result := FBody;
  if Length(params) <> Length(FParams) then
    raise Exception.Create('Bad number of params in invocation of ' + FName);
  for i := 0 to High(FParams) do
    if FParams[i][1] <> '''' then
      Result := StringReplace(Result, '{' + FParams[i] + '}', params[i], [rfReplaceAll]);
  if Result = '' then exit;
  p := TParser.Create(Result);
  try
    try
      Result := p.Parse;
    except
      Exception(ExceptObject).Message := Exception(ExceptObject).Message + #13#10'called in ' + ToString;
      raise;
    end;
  finally
    p.Free;
  end;
end;

function TMacro.GetParamMatches(params : array of String) : Integer;
var i : Integer;
begin
  if Length(params) <> Length(FParams) then begin
    Result := -1;
    Exit;
  end;
  Result := 0;
  for i := 0 to High(FParams) do
    if FParams[i][1] = '''' then
      if FParams[i] = '''' + params[i] then
        Inc(Result, Length(FParams) + 1)
      else begin
        Result := -1;
        Exit;
      end;
end;

function TMacro.ToString : String;
begin
  Result := FName;
  if Length(FParams) <> 0 then
    Result := Result + ' ' + ArrayToString(FParams);
end;

var
  t : TextFile;
  s, head : String;
  sl : TStringList;
  m : TMacro;
begin
  if ParamCount = 0 then Exit;
  Macros := TList.Create;
  Assign(t, ParamStr(1));
  Reset(t);
  sl := TStringList.Create;
  try
    while not EOF(t) do begin
      sl.Clear;
      repeat
        ReadLn(t, head);
      until (head <> '') and (head[1] <> ';');
      while not EOF(t) do begin
        ReadLn(t, s);
        if s = '' then Break;
        if s[1] <> ';' then
          sl.Add(s);
      end;
      s := sl.GetText;
      m := TMacro.Create(head, Trim(s));
      Macros.Add(m);
    end;
  finally
    sl.Free;
    CloseFile(t);
  end;
  m := FindMacro('target', []);
  if m = nil then
    raise Exception.Create('Macro named ''target'' must be specified');
  try
  WriteLn(m.Expand([]));except writeln(exception(exceptobject).message); end;
end.
