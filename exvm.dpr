program exvm;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Memory in 'Memory.pas',
  Registers in 'Registers.pas',
  Common in 'Common.pas',
  Core in 'Core.pas',
  Instructions in 'Instructions.pas',
  Tests in 'Tests.pas';

begin
  try
    RunAllTests;
    WriteLn('All Tests passed');
  except
    WriteLn(Exception(ExceptObject).Message);
  end;
  readln;
end.
