unit ImageFile;

interface

uses SysUtils;

type
  EImageFileException = class(Exception);

procedure LoadImageFile(const FileName : String);

implementation

uses Classes, Common, Memory;

const
  Signature = 'EXVMIMG'#0;

procedure InvalidFile(const FileName : String);
begin
  raise EImageFileException.Create('File ' + FileName + ' does not contain valid memory image');
end;

procedure LoadImageFile(const FileName : String);
var
  buffer : array[0..MaxReqSize] of byte;
  count, moved : Cardinal;
  BaseAddr : TAddress;
  ImageSize : Cardinal;
begin
  Assert(Length(Signature) <= MaxReqSize);
  with TFileStream.Create(FileName, fmOpenRead) do
    try
      count := Read(buffer, Length(Signature));
      if (Integer(count) < Length(Signature)) or not CompareMem(@Signature[1], @buffer, count) then
        InvalidFile(FileName);
      count := Read(BaseAddr, SizeOf(BaseAddr));
      if (count < SizeOf(BaseAddr)) then
        InvalidFile(FileName);
      count := Read(ImageSize, SizeOf(ImageSize));
      if (count < SizeOf(ImageSize)) then
        InvalidFile(FileName);
      moved := 0;
      while moved < ImageSize do begin
        count := Read(buffer, MaxReqSize);
        if count = 0 then
          raise EImageFileException.Create('Data in memory image is smaller than specified');
        Move(buffer, GetCellAddrWrite(BaseAddr + moved, count)^, count);
        CommitCellWrite;
        moved := moved + count;
      end;
    finally
      Free;
    end;
end;

end.
