unit CRC32Buf;

{ Dependency-free version of CRC32.pas. Just bare algorithm for a custom buffer. }

interface

var
  CRCTable: array[0..255] of Cardinal;

function CRC32OfBuffer(const Buf; Size: Integer): Cardinal;

{
  If you need to calculate a large buffer or multiple buffers do it like this:

    Checksum := CRCInit;

    while buffers do ...
      CRC32AddBuffer(Checksum, MyBuffer, MySize);

    Checksum := Checksum xor CRCStop;   // final checksum.
}

const
  CRCInit = $FFFFFFFF;
  CRCStop = $FFFFFFFF;

procedure CRC32AddBuffer(var Checksum: Cardinal; const Buf; Size: Integer);

implementation

procedure InitTable;
var
  I, C, J: Cardinal;
begin
  for I := 0 to Length(CRCTable) do
  begin
    C := I;

    for J := 0 to 7 do
      if C and 1 = 0 then
        C := C div 2
        else
          C := (C div 2) xor $EDB88320;

    CRCTable[I] := C;
  end;
end;

function CRC32OfBuffer(const Buf; Size: Integer): Cardinal;
begin
  Result := CRCInit;
  CRC32AddBuffer(Result, Buf, Size);
  Result := Result xor CRCStop;
end;

procedure CRC32AddBuffer(var Checksum: Cardinal; const Buf; Size: Integer);
var
  I: Cardinal;
begin
  if Size > 0 then
    for I := 0 to Size - 1 do
      Checksum := CRCTable[(Checksum and $FF) xor PByte(Cardinal(@Buf) + I)^] xor (((Checksum and $FFFFFF00) div 256) and $FFFFFF);
end;

initialization
  InitTable;
end.