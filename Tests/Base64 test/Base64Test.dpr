program Base64Test;

{$APPTYPE CONSOLE}

uses
  FileStreamW,
  Base64,
  Windows,
  SysUtils,
  MMSystem,
  Utils;

var
  DoOutput: Boolean   = False;
  TestsRunCount: Word = 0;

procedure TestBase64(const FileName: WideString; EncProc, DecProc: Byte; TestSize: DWord = $FFFFFF);
var
  F: TFileStreamW;
  Source, Base64, Decoded: String;
  EncTimer, DecTimer: DWord;         
begin
  F := TFileStreamW.Create(FileName, fmOpenRead or fmShareDenyNone);

  if TestSize > F.Size then
    TestSize := F.Size;

  SetLength(Source, TestSize);
  F.Read(Source[1], TestSize);

  EncTimer := timeGetTime;
  case EncProc of
  0: Base64 := Base64BuiltInEncode(Source);
  1: Base64 := Base64XPEncode(Source);
  2: Base64 := Base64Encode(Source);
  end;
  EncTimer := timeGetTime - EncTimer;

  DecTimer := timeGetTime;
  case DecProc of
  0: Decoded := Base64BuiltInDecode(Base64);
  1: Decoded := Base64XPDecode(Base64);
  2: Decoded := Base64Decode(Base64);
  end;
  DecTimer := timeGetTime - DecTimer;

  if DoOutput then
  begin
    WriteLn('xp = ', EncProc, '/', DecProc, '...');

    WriteLn(Format('  Encoded %d bytes in %1.3f seconds (base64 string length was %d chars)',
                   [TestSize, EncTimer / 1000, Length(Base64)]));
    WriteLn(Format('  Decoded %d bytes in %1.3f seconds (base64 string length was %d chars)',
                   [TestSize, DecTimer / 1000, Length(Base64)]));

    if EncTimer = 0 then
      EncTimer := 1;
    WriteLn(Format('Decoding took %1.1f%% of encoding time.', [DecTimer / EncTimer * 100]));
    WriteLn;
  end;

  if not AreBytesEqual(Source[1], Decoded[1], Length(Source)) then
    WriteLn('Assertion failed!');

  Inc(TestsRunCount);
end;

procedure RunTests(const FileName: WideString; TestSize: DWord = $FFFFFF);
const
  DoNotUseXP  = 0;
  UseXP       = 1;
  UseAny      = 2;
begin
  TestBase64(FileName, DoNotUseXP, DoNotUseXP, TestSize);
  TestBase64(FileName, UseXP,      UseXP,      TestSize);

  TestBase64(FileName, UseXP,      DoNotUseXP, TestSize);
  TestBase64(FileName, DoNotUseXP, UseXP,      TestSize);

  TestBase64(FileName, UseAny,     UseAny,     TestSize);

  if DoOutput then
    WriteLn('Disabling Crypt32.dll support...');

  DoNotUseCrypt32;
  TestBase64(FileName, UseAny,     UseAny,     TestSize);
  TryToUseCrypt32;
end;

const                   
  ThroughTestBytes = 4 * 1024;

var
  I: Word;

begin
  WriteLn('Testing binary file: ', #10, '    ', ParamStr(0));
  WriteLn;

  TestsRunCount := 0;

  for I := 0 to ThroughTestBytes do
  begin
    RunTests(ParamStr(0), I);
    if I mod 300 = 0 then
      WriteLn(I, '/', ThroughTestBytes, '...');
  end;

  WriteLn;
  WriteLn('By-byte tests have been run, now testing one shot with full file size.');
  WriteLn;

  DoOutput := True;
  RunTests(ParamStr(0));

  WriteLn('Finished Base64 tests, ', TestsRunCount, ' run.');
  ReadLn
end.