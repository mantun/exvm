unit Core;

interface

uses Common;

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

function AddressingDescriptorToMode(adescr : T8Cell) : TAddressingMode;

procedure ExecuteNext;

implementation

uses Memory, InstrLib;

function AddressingDescriptorToMode(adescr : T8Cell) : TAddressingMode;
begin
end;

procedure ExecuteNext;
var
  opcode : Byte;
begin
  opcode := getmem(IP, sz8);
  InstructionTable[opcode];
end;

end.
