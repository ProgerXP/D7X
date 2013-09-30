unit CRC32;

interface

uses Classes;

var
  CRCTable: array[0..255] of Cardinal;

// when Size = 0 stream will be rewound to the beginning and Size will be set to stream's Size.
function CRC32OfStream(S: TStream; Size: Cardinal = 0): Cardinal;
function CRC32OfFile(FileName: WideString): Cardinal;

procedure InitializeCRCTable;

implementation

uses SysUtils, FileStreamW;

var
  TableInitialized: Boolean = False;

procedure InitializeCRCTable;
var
  I, C, J: Cardinal;
begin
  if not TableInitialized then
    for I := 0 to 255 do
    begin
      C := I;

      for J := 0 to 7 do
        if C and 1 = 0 then
          C := C div 2
          else
            C := (C div 2) xor $EDB88320;

      CRCTable[I] := C;
    end;

  TableInitialized := True;
end;

function CRC32OfStream(S: TStream; Size: Cardinal = 0): Cardinal;
const
  CRCBlock = 4096;
var
  Pos: Integer;
  C, I, J, BytesRead: Cardinal;
  Block: array[0..CRCBlock - 1] of Byte;
begin
  Pos := S.Position;
  try
    if Size = 0 then
    begin
      S.Position := 0;
      Size := S.Size;
    end;

    C := $FFFFFFFF;

    for I := 0 to Size div CRCBlock + 1 do
    begin
      BytesRead := Cardinal(S.Read(Block, CRCBlock));
      if BytesRead = 0 then
        Break;

      if Size < BytesRead then
        BytesRead := Size;
      Dec(Size, BytesRead);

      for J := 0 to BytesRead - 1 do
        C := CRCTable[(C and $FF) xor Block[J]] xor (((C and $FFFFFF00) div 256) and $FFFFFF)
    end;

    Result := C xor $FFFFFFFF;
  finally
    S.Position := Pos;
  end;
end;

function CRC32OfFile(FileName: WideString): Cardinal;
var
  F: TFileStreamW;
begin
  F := TFileStreamW.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CRC32OfStream(F);
  finally
    F.Free;
  end
end;

initialization
  InitializeCRCTable;
end.
