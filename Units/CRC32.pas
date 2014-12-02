unit CRC32;

interface

uses Classes;

// when Size = 0 stream will be rewound to the beginning and Size will be set to stream's Size.
function CRC32OfStream(S: TStream; Size: Cardinal = 0): Cardinal;
function CRC32OfFile(FileName: WideString): Cardinal;

implementation

uses SysUtils, FileStreamW;

function CRC32OfStream(S: TStream; Size: Cardinal = 0): Cardinal;
const
  CRCBlock = 4096;
var
  Pos: Integer;
  I, J, BytesRead: Cardinal;
  Block: array[0..CRCBlock - 1] of Byte;
begin
  Pos := S.Position;
  try
    if Size = 0 then
    begin
      S.Position := 0;
      Size := S.Size;
    end;

    Result := CRCInit;

    for I := 0 to Size div CRCBlock + 1 do
    begin
      BytesRead := Cardinal(S.Read(Block, CRCBlock));
      if BytesRead = 0 then
        Break;

      if Size < BytesRead then
        BytesRead := Size;
      Dec(Size, BytesRead);

      CRC32AddBuffer(Result, Block[0], BytesRead);
    end;

    Result := Result xor CRCStop;
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
