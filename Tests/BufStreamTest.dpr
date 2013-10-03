program BufStreamTest;

{$APPTYPE CONSOLE}

{$IFOPT C-}
  {$MESSAGE ERROR 'Please run tests with Assertions $C+ enabled.}
{$ENDIF}

uses
  BufStream,
  Windows,
  SysUtils,
  Classes;

type
  TOpenBufStream = class (TBufStream);

procedure BasicTest;
const
  Data = '0123456789abcdef';
var
  S: TStringStream;
  B: TOpenBufStream;
begin
  S := TStringStream.Create(Data);
  B := TOpenBufStream.Create(S);
  B.Resize(4, 3);   // 4 buffers 3 bytes each.
  Assert(B.FBuffers.Count = 0);
  Assert(B.Read(3, 2) = '34');
  Assert(B.FBuffers.Count = 1);
  Assert(B.Read(4, 2) = '45');
  Assert(B.FBuffers.Count = 1);   // same buffer.
  Assert(B.Read(4, 3) = '456');
  Assert(B.FBuffers.Count = 2);
  Assert(B.Read(11, 999) = 'bcdef');
  Assert(B.FBuffers.Count = 2);   // too long to buffer - pass-thru reading.
  Assert(B.Read(0, 999) = Data);
  Assert(B.FBuffers.Count = 2);
  Assert(B.Read(14, 999) = 'ef');
  Assert(B.FBuffers.Count = 3);
  Assert(B.Read(-5, 0) = '');
  Assert(B.FBuffers.Count = 3);
  Assert(B.Read(5, -5) = '');
  Assert(B.FBuffers.Count = 3);
  Assert(B.Read(7, 1) = '7');
  Assert(B.FBuffers.Count = 4);
  Assert(B.Read(12, 2) = 'cd');
  Assert(B.FBuffers.Count = 4);   // turn over.

  B.Resize(2, 5);
  Assert(B.FBuffers.Count = 2);
  B.Resize(5, 5);
  Assert(B.FBuffers.Count = 2);

  S.Size := S.Size - 1;
  Assert(B.Read(13, 5) = 'de');
  Assert(B.FBuffers.Count = 1);   // refreshed on size change.
  S.Position := S.Size;
  S.WriteString('-');
  Assert(B.Read(13, 5) = 'de-');
  Assert(B.FBuffers.Count = 1);

  B.OwnsStream := False;
  B.Free;
  S.Free;
end;

begin
  BasicTest;

  WriteLn('All ok.');
  ReadLn
end.