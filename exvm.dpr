program exvm;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Registers in 'Core\Registers.pas',
  Common in 'Core\Common.pas',
  Core in 'Core\Core.pas',
  Instructions in 'Core\Instructions.pas',
  Memory in 'Core\Memory.pas',
  Tests in 'Tests\Tests.pas',
  ImageFile in 'Tools\ImageFile.pas';

begin
  if ParamCount = 0 then begin
    WriteLn('Usage:');
    WriteLn('  exvm <imagefile>                - load memory image and run');
    WriteLn('  exvm -t                         - run unit tests');
  end else if ParamStr(1) = '-t' then
    try
      RunAllTests;
      WriteLn('All Tests passed');
    except
      WriteLn(Exception(ExceptObject).Message);
    end
  else begin
    LoadImageFile(ParamStr(1));
    Run;
  end;

  readln;
end.

