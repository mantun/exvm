unit Core;

interface

uses SysUtils, Common;

var
  IP : TAddress;
  SP : TAddress;

type
  EBadInstructionException = class(Exception);
  TAddrMode = (amImmediate,
    amRegisterDirect, amRegisterIndirect, amRegisterIndirectWDispl,
    amDirect, amIndirect, amIndirectWDispl,
    amRelative,
    amShortImmediate, amIndirectWShortDispl);

function AddrDescrSrcToMode(adescr : T8Cell) : TAddrMode;
function InstrSize(adescr : T8Cell) : TAddrMode;

procedure ExecuteNext;

implementation

uses Memory, InstrLib;

function AddrDescrSrcToMode(adescr : T8Cell) : TAddrMode;
begin
  case adescr and $0F of
    $0 : Result := amImmediate;
    $1 : Result := amRegisterDirect;
    $2 : Result := amRegisterIndirect;
    $3 : Result := amRegisterIndirectWDispl;
    $4 : Result := amDirect;
    $5 : Result := amIndirect;
    $6 : Result := amIndirectWDispl;
    $7 : Result := amRelative;
    $8 : Result := amShortImmediate;
    $E : Result := amIndirectWShortDispl;
    else raise EBadInstructionException.Create('invalid addressing mode');
  end;
end;

procedure ExecuteNext;
var
  opcode : Byte;
begin
  opcode := getmem(IP, sz8);
  InstructionTable[opcode];
end;

end.
