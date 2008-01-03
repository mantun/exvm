unit InstrLib;

interface

type
  TProcedure = procedure;
var
  InstructionTable : Array[Byte] of TProcedure;

implementation

uses SysUtils, Common, Core, Memory, Registers;

const
  OpSize : Array[TAddrMode] of Integer = (
  );

function GetSource(IP : TAddress; amode : TAddrMode; opsize : TOpSize) : TCell;
var
  reg, regdispl : T8Cell;
  addr, adispl : TAddress;
begin
  case amode of
    amImmediate : Result := getmem(IP + 2, opsize);
    amRegisterDirect : begin
      reg := getmem(IP + 2, sz8);
      Result := getreg(reg, opsize);
    end;
    amRegisterIndirect : begin
      reg := getmem(IP + 2, sz8);
      reg := getreg(reg, sz8);
      Result := getreg(reg, opsize);
    end;
    amRegisterIndirectWDispl : begin
      reg := getmem(IP + 2, sz8);
      reg := getreg(reg, sz8);
      regdispl := getmem(IP + 3, sz8);
      Result := getreg((reg + regdispl) and $FF, opsize);
    end;
    amDirect : begin
      addr := getmem(IP + 2, sz32);
      Result := getmem(addr, opsize);
    end;
    amIndirect : begin
      reg := getmem(IP + 2, sz8);
      addr := getreg(reg, sz32);
      Result := getmem(addr, opsize);
    end;
    amIndirectWDispl : begin
      reg := getmem(IP + 2, sz8);
      addr := getreg(reg, sz32);
      adispl := getmem(IP + 3, sz32);
      Result := getmem(addr + adispl, opsize);
    end;
    amRelative : Result := getmem(IP + 2, opsize) + IP;
    amShortImmediate : Result := getmem(IP + 2, sz8);
    amIndirectWShortDispl : begin
      reg := getmem(IP + 2, sz8);
      addr := getreg(reg, sz32);
      adispl := getmem(IP + 3, sz8);
      Result := getmem(addr + adispl, opsize);
    end;
    else begin
      Assert(False);
      Result := 0;
    end;
  end;
end;

function GetDestination(IP : TAddress; amode : TAddrMode; opsize : TOpSize) : TCell;
begin
  case amode of
    amRegisterDirect : begin
      reg := getmem(IP + ?, sz8);
      Result := getreg(reg, opsize);
    end;
    amRegisterIndirect : begin
      reg := getmem(IP + ?, sz8);
      reg := getreg(reg, sz8);
      Result := getreg(reg, opsize);
    end;
    else begin
      Assert(False);
      Result := 0;
    end;
  end;
end;

function CalcEffectiveAddress(PAD : TAddress) : TAddress;
begin
end;

procedure InstrAdd;
begin

end;

{

01	add	R, A	R <- R + A		ZCN
02	adc	R, A	R <- R + A + C		ZCN
03	sub	R, A	R <- R - A		ZCN
04	sbc	R, A	R <- R - A - C		ZCN
05	and	R, A	R <- R & A		ZN
06	or	R, A	R <- R | A		ZN
07	xor	R, A	R <- R ^ A		ZN
08	not	A	A <- ~A			ZN
09	neg	A	A <- -A			ZN
0A	inc	A	A <- A + 1		ZN
0B	dec	A	A <- A - 1		ZN
0C	tst	A	A <- A & A		ZN
0D	clr	A	A <- 0			-
0E	mul	R, A	Rn:Rn+1 <- R * A	ZC
0E	mulw	Rw, A	R <- Rw * A		ZC
0E	mulb	Rb, A	Rw <- Rb * A		ZC
0F	div?

Flow control:
10	jmp	K	IP <- IP + K + isz	-
11	jmp	A	IP <- A			-
12	call	K	STACK <- IP + isz	-
			IP <- IP + K + isz
13	call	A	STACK <- IP + isz	-
			IP <- A
14	ret		IP <- STACK		-
15	cmp	R, A	R - A			ZCN
16	cpc	R, A	R - A - C		ZCN
18	jz	K	if (Z) IP <- IP + K + isz
19	jnz	K	if (!Z) IP <- IP + K + isz
1A	jc	K	if (C) IP <- IP + K + isz
1B	jnc	K	if (!C) IP <- IP + K + isz
1C	jn	K	if (N) IP <- IP + K + isz
1D	jp	K	if (!N) IP <- IP + K + isz
90	nop		-			-
00	break		breaks			-
CC	break		breaks			-
A5	halt		stops the VM

Data transfer:
20	mov	R, A	R <- A			-
21	mov	A, R	A <- R			-
22	in?
23	out?
24	push	A	STACK <- A		-
25	pop	A	A <- STACK		-
26	pushf		STACK <- FLAGS		-
27	popf		FLAGS <- STACK 		*

Bitwise:
30	shl	A, K	A <- A << K		ZCN
31	shr	A, K	A <- A >> K		ZCN
32	sar	A, K	A <- A >>> K		ZCN
33	rol	A, K	A <- A << 1 | C		ZCN
34	ror	A, K	A <- A >> 1 | C	<< 7	ZCN
36	clz		Z <- 0			Z
37	stz		Z <- 1			Z
38	clc		C <- 0			C
39	stc		C <- 1			C
3A	cln		N <- 0			N
3B	stn		N <- 1			N
}

procedure BreakPointInstruction;
begin
  WriteLn('Breakpont at ' + IntToHex(IP, 8));
end;

procedure InitInstructionTable;
begin

end;

initialization
  InitInstructionTable;

end.
