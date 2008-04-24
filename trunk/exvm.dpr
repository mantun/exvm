program exvm;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Registers in 'Core\Registers.pas',
  Common in 'Core\Common.pas',
  Core in 'Core\Core.pas',
  Instructions in 'Core\Instructions.pas',
  Memory in 'Core\Memory.pas',
  Tests in 'Tests\Tests.pas';

begin
  try
    RunAllTests;
    WriteLn('All Tests passed');
  except
    WriteLn(Exception(ExceptObject).Message);
  end;
  readln;
end.
