unit FileStreamW;

interface

uses
  Classes, Windows, SysUtils;

type
  TFileStreamW = class (TFileStream)
  protected
    FFileName: WideString;
  public
    // this method handles UTF-8/Unicode/Unicode-BE files with signatures treating others as
    // using native ANSI charset. If AsIsAnsi = True signatureless files are not converted from
    // system codepage to Unicode but simply copied with high-order byte set to 0. This is useful
    // if you're planning to manually convert it later and do not want any chars get corrupted.
    class function LoadUnicodeFrom(const FileName: WideString;
      AsIsAnsi: Boolean = False): WideString;

    constructor Create(const FileName: WideString; Mode: Word);

    // CreateCustom works like Create(fmCreate) but allows more specified access mode
    // (FileCreate always opens new file with exclusive access). Mode supports fmForcePath.
    constructor CreateCustom(const FileName: WideString; Mode: DWord);

    property FileName: WideString read FFileName;
  end;

const
  fmOpenRead       = SysUtils.fmOpenRead;
  fmOpenWrite      = SysUtils.fmOpenWrite;
  fmOpenReadWrite  = SysUtils.fmOpenReadWrite;

  fmShareCompat    = SysUtils.fmShareCompat;
  fmShareExclusive = SysUtils.fmShareExclusive;
  fmShareDenyWrite = SysUtils.fmShareDenyWrite;
  fmShareDenyRead  = SysUtils.fmShareDenyRead;
  fmShareDenyNone  = SysUtils.fmShareDenyNone;

  fmForcePath      = $80000000;   // TFileStreamW-specific flag used in CreateCustom to ForceDirectories.

implementation

uses Utils;

const
  UTF8Signature: array[0..2] of Char              = (#$EF, #$BB, #$BF);
  UnicodeSignature: array[0..1] of Char           = (#$FF, #$FE);
  BigEndianUnicodeSignature: array[0..1] of Char  = (#$FE, #$FF);

// SysUtils.pas: 4788
function FileCreateW(const FileName: WideString; Mode: LongWord; CreationMode: LongWord = CREATE_ALWAYS): Integer;
const
  AccessMode: array[0..2] of LongWord = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of LongWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  Result := Integer(CreateFileW(PWideChar(FileName), AccessMode[Mode and 3],
                    ShareMode[(Mode and $F0) shr 4], NIL, CreationMode, FILE_ATTRIBUTE_NORMAL, 0))
end;

{ TFileStreamW }

class function TFileStreamW.LoadUnicodeFrom(const FileName: WideString;
  AsIsAnsi: Boolean = False): WideString;
type
  TEncoding = (ANSI, UTF8, Unicode, UnicodeBE);
var
  Stream: TFileStreamW;
  Sign: array[0..2] of Char;
  Encoding: TEncoding;
  Data: String;
  I: Integer;
  Temp: Char;
begin
  Stream := Self.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Stream.Read(Sign[0], 3);

    if Sign = UTF8Signature then
      Encoding := UTF8
      else if Copy(Sign, 1, 2) = UnicodeSignature then
        Encoding := Unicode
        else if Copy(Sign, 1, 2) = BigEndianUnicodeSignature then
          Encoding := UnicodeBE
          else
            Encoding := ANSI;

    case Encoding of
    Unicode, UnicodeBE: Stream.Position := 2;
    ANSI:               Stream.Position := 0;
    end;

    SetLength(Data, Stream.Size - Stream.Position);
    Stream.ReadBuffer(Data[1], Length(Data));

    case Encoding of
    ANSI:
      if AsIsAnsi then
      begin
        SetLength(Result, Length(Data));
        for I := 1 to Length(Data) do
          Result[I] := WideChar( Byte(Data[I]) );
      end
        else
          Result := Data;   // Delphi converts strings automatically from system codepage.
    UTF8:
      Result := UTF8Decode(Data);
    Unicode, UnicodeBE:
      begin
        if Encoding = UnicodeBE then
          for I := 1 to Length(Data) do
            if I mod 2 = 0 then
            begin
              Temp := Data[I - 1];
              Data[I - 1] := Data[I];
              Data[I] := Temp;
            end;

        SetLength(Result, Length(Data) div 2);
        Move(Data[1], Result[1], Length(Data));
      end;
    end;
  finally
    Stream.Free;
  end;
end;

function FileOpenW(const FileName: WideString; Mode: LongWord): Integer;
begin
  Result := FileCreateW(FileName, Mode, OPEN_EXISTING)
end;

{ TFileStreamW }

constructor TFileStreamW.Create(const FileName: WideString; Mode: Word);
begin
  FFileName := FileName;

  if Mode = fmCreate then
  begin
    inherited Create(FileCreateW(FileName, fmOpenReadWrite or fmShareExclusive));
    if FHandle < 0 then
      raise EFCreateError.CreateFmt('TFileStreamW: Cannot create file "%s". %s', [ExpandFileName(FileName), SysErrorMessage(GetLastError)])
  end
    else
    begin
      inherited Create(FileOpenW(FileName, Mode));
      if FHandle < 0 then
        raise EFOpenError.CreateFmt('TFileStreamW: Cannot open file "%s". %s', [ExpandFileName(FileName), SysErrorMessage(GetLastError)])
    end
end;

constructor TFileStreamW.CreateCustom;
begin
  FFileName := FileName;

  if (Mode = fmCreate) or (Mode and (fmOpenRead or fmOpenWrite or fmOpenReadWrite) <> 0) then
    raise EFCreateError.CreateFmt('TFileStreamW: CreateCustom implies fmCreate, don''t specify other modes.', [])
    else
    begin
      if Mode and fmForcePath <> 0 then
        ForceDirectories(ExtractFilePath(FileName));

      inherited Create(FileCreateW(FileName, Mode and $FFFF + 2));

      if FHandle < 0 then
        raise EFCreateError.CreateFmt('TFileStreamW: Cannot CreateCustom(%s): %s', [ExpandFileName(FileName), SysErrorMessage(GetLastError)])
    end
end;

end.

