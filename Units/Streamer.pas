unit Streamer;

interface

uses Windows, SysUtils, Classes, ZLibEx;

{
  @ offset

  [S]  Signature
  [W]  Version
  [B]  Flags
  -- extra, if written --
  @ HeaderSize

  [DW] Checksum if sfAdler32 in Flags
  -- data, compressed if sfZlib in Flags --
}

type
  TStreamerVersion = type Word;
  TStreamerSignature = type AnsiString;
  TStreamerFlags = set of (sfZlib, sfAdler32);
  TStreamerCheck = set of (scSignature, scOlderVersion, scNewerVersion, scChecksum);
  TStreamerCompressionLevel = ZLibEx.TZCompressionLevel;

  TStreamer = class
  protected
    FHost: TObject;
    FSignature: TStreamerSignature;
    FVersion: TStreamerVersion;
    FFlags: TStreamerFlags;
    FCompressionLevel: TStreamerCompressionLevel;
    FHeaderSize: Integer;   // set by Write and Read.
    FChecksum: DWord;       // set by Read.

    // Reads from Stream.Position. BlindAt relative to current Stream.Position.
    function CalculateAdler(Stream: TStream; BlindAt: Integer = -1; BlindSize: Integer = 4): DWord;
    // Stream needs not be positioned.
    procedure Compress(Stream: TStream; Offset: Integer);
    // Stream needs not be positioned.
    function Uncompress(Stream: TStream; Offset: Integer): TMemoryStream;

    procedure CheckSignature(const Read: TStreamerSignature; SkipChecks: TStreamerCheck); virtual;
    procedure CheckVersion(Read: TStreamerVersion; SkipChecks: TStreamerCheck); virtual;
    procedure WriteExtraHeader(Stream: TStream); virtual;
    procedure ReadExtraHeader(Stream: TStream); virtual;
                         
    procedure SetHost(Value: TObject);
    procedure SetSignature(const Value: TStreamerSignature);
    procedure SetVersion(Value: TStreamerVersion);
    procedure SetFlags(Value: TStreamerFlags);
  public
    constructor Create(const Signature: TStreamerSignature; Version: TStreamerVersion); virtual;

    // Is included in error messages. Defaults to Self. Never nil.
    property Host: TObject read FHost write SetHost;
    property Signature: TStreamerSignature read FSignature write SetSignature;
    property Version: TStreamerVersion read FVersion write SetVersion;
    // Flahs are written in WriteTo and read in Validate.
    property Flags: TStreamerFlags read FFlags write SetFlags;

    procedure Write(Stream: TStream);
    // Host class must call this once he has done writing data to Stream (passed to Write).
    // Offset - start of header data (Stream.Position before Stream was passed to Write).
    // Flags should not be changed between Write and PostWrite.
    procedure PostWrite(Stream: TStream; Offset: Integer = 0);
    // Can return Stream so check if they are not equal before Freeing its result.
    // Reads from current position so rewind stream if necessary.
    // Updates Self.Signature, Version and Flags according to those read from Stream.
    function Read(Stream: TStream; SkipChecks: TStreamerCheck = []): TStream;
    // Frees Stream; free result when you are done yourself.
    function ReadAndFree(Stream: TStream; SkipChecks: TStreamerCheck = []): TStream;
  end;

{ Exceptions }

type
  EStreamer = class (Exception)
    Host: TObject;
  end;

  EStreamerEmpty = class (EStreamer)
    Operation: String;
    Stream: TStream;
    constructor Create(Host: TObject; Operation: String; Stream: TStream);
  end;

  EStreamerBadSignature = class (EStreamer)
    StreamSign, ExpectedSign: TStreamerSignature;
    constructor Create(Host: TObject; StreamSign, ExpectedSign: TStreamerSignature);
  end;

  EStreamerBadChecksum = class (EStreamer)
    StreamSum, ExpectedSum: DWord;
    constructor Create(Host: TObject; StreamSum, ExpectedSum: DWord);
  end;

  EStreamerBadVersion = class (EStreamer)
    StreamVer, SupportedVer: TStreamerVersion;
    constructor Create(Host: TObject; StreamVer, SupportedVer: TStreamerVersion);
  end;

  EStreamerCompression = class (EStreamer)
    ZlibMessage: String;
    constructor Create(Host: TObject; ZlibMessage: String);
  end;

implementation

uses StringUtils, Math;

{$L adler32.obj}
function Adler32(Adler: DWord; const Buf; Len: Integer): DWord; external;

{ VersionControl }

constructor TStreamer.Create(const Signature: TStreamerSignature; Version: TStreamerVersion);
begin
  Host := nil;
  Self.Signature := Signature;
  Self.Version := Version;
  Self.Flags := [sfZlib, sfAdler32];
  FCompressionLevel := zcDefault;
end;
                        
procedure TStreamer.SetHost(Value: TObject);
begin
  if Value = nil then
    Value := Self;
  FHost := Value;
end;

procedure TStreamer.SetSignature(const Value: TStreamerSignature);
begin
  if Value = '' then
    raise EInvalidArgument.CreateFmt('%s.SetSignature expects a non-empty string.', [ClassName]);
  FSignature := Value;
end;

procedure TStreamer.SetVersion(Value: TStreamerVersion);
begin
  if Value = 0 then
    raise EInvalidArgument.CreateFmt('%s.SetVersion expects a non-zero integer.', [ClassName]);
  FVersion := Value;
end;
        
procedure TStreamer.SetFlags(Value: TStreamerFlags);
begin
  FFlags := Value;
end;

procedure TStreamer.Write(Stream: TStream);
var
  ChecksumStub: DWord;
begin
  FHeaderSize := Stream.Position;
  Stream.WriteBuffer(FSignature[1], Length(FSignature));
  Stream.WriteBuffer(FVersion, SizeOf(FVersion));
  Stream.WriteBuffer(FFlags, SizeOf(FFlags));
  WriteExtraHeader(Stream);
  FHeaderSize := Stream.Position - FHeaderSize;

  ChecksumStub := 0;
  // Checksum is part of the data stream, not header.
  Stream.WriteBuffer(ChecksumStub, SizeOf(ChecksumStub));
end;

procedure TStreamer.PostWrite(Stream: TStream; Offset: Integer = 0);
var
  Sum: DWord;
begin
  if sfAdler32 in FFlags then
  begin
    Stream.Position := Offset;
    Sum := CalculateAdler(Stream, -1);
    Stream.Position := Offset + FHeaderSize;
    Stream.WriteBuffer(Sum, SizeOf(Sum));
    Stream.Position := Stream.Size;
  end;

  if sfZlib in FFlags then
    Compress(Stream, Offset + FHeaderSize);
end;

function TStreamer.CalculateAdler(Stream: TStream; BlindAt, BlindSize: Integer): DWord;
const
  Chunk = 4 * 1024 * 1024;
var
  Buf: Pointer;
  Size: Integer;
begin
  if Stream.Size <= Stream.Position then
    raise EStreamerEmpty.Create(FHost, 'CalculateAdler', Stream);

  Result := 0;
  repeat
    Size := Chunk;
    if (BlindAt >= 0) and (BlindAt < Size) and (BlindAt + BlindSize >= Size) then
      Inc(Size, BlindSize);

    Size := Min(Size, Stream.Size - Stream.Position);
    if Size <= 0 then
      Break;

    GetMem(Buf, Size);
    try
      Stream.ReadBuffer(Buf^, Size);

      if BlindAt >= 0 then
      begin
        if BlindAt < Size then
        begin
          BlindSize := Min(BlindSize, Size - BlindAt - BlindSize);
          if BlindSize > 0 then
            FillChar(Pointer(DWord(Buf) + DWord(BlindAt))^, BlindSize, 0);
        end;

        Dec(BlindAt, Size);
      end;

      Result := Adler32(Result, Buf^, Size);
    finally
      FreeMem(Buf, Size);
    end;
  until False;
end;

procedure TStreamer.Compress(Stream: TStream; Offset: Integer);
var
  Buf, CompBuf: Pointer;
  Size, CompSize: Integer;
begin
  Size := Stream.Size - Offset;
  if Size <= 0 then
    raise EStreamerEmpty.Create(FHost, 'Compress', Stream);

  GetMem(Buf, Size);
  try
    Stream.Position := Offset;
    Stream.ReadBuffer(Buf^, Size);
    try
      ZCompress(Buf, Size, CompBuf, CompSize, FCompressionLevel);
    except
      on E: EZLibError do
        raise EStreamerCompression.Create(FHost, E.Message);
    end;
  finally
    FreeMem(Buf, Size);
  end;

  if CompSize = 0 then
    raise EStreamerEmpty.Create(FHost, 'ZCompress', Stream);

  try
    Stream.Size := Offset + CompSize;
    Stream.Position := Offset;
    Stream.WriteBuffer(CompBuf^, CompSize);
    Stream.Size := Stream.Position;
  finally
    FreeMem(CompBuf, CompSize);
  end;
end;
                         
function TStreamer.Uncompress(Stream: TStream; Offset: Integer): TMemoryStream;
var
  Buf, Uncomp: Pointer;
  Size, UncompSize: Integer;
begin
  Size := Stream.Size - Offset;

  GetMem(Buf, Size);
  try
    Stream.Position := Offset;
    Stream.Read(Buf^, Size);
    try
      ZDecompress(Buf, Size, Uncomp, UncompSize);
    except
      on E: EZLibError do
        raise EStreamerCompression.Create(FHost, E.Message);
    end;
  finally
    FreeMem(Buf, Size);
  end;

  Result := TMemoryStream.Create;
  try
    if UncompSize <> 0 then
      try
        Result.Size := Offset + UncompSize;
        Stream.Position := 0;
        Result.CopyFrom(Stream, Offset);
        Result.WriteBuffer(Uncomp^, UncompSize);
      finally
        FreeMem(Uncomp, UncompSize);
      end;

    Result.Position := Offset;
  except
    Result.Free;
    raise;
  end;
end;

function TStreamer.Read(Stream: TStream; SkipChecks: TStreamerCheck): TStream;
var
  Signature: TStreamerSignature;
  Version: TStreamerVersion;
  Offset: Integer;
  Checksum: DWord;
begin
  Offset := Stream.Position;

  SetLength(Signature, Length(FSignature));
  Stream.ReadBuffer(Signature[1], Length(FSignature));
  CheckSignature(Signature, SkipChecks);
  FSignature := Signature;

  Stream.ReadBuffer(Version, SizeOf(Version));
  CheckVersion(Version, SkipChecks);
  FVersion := Version;

  Stream.ReadBuffer(FFlags, SizeOf(FFlags));
  ReadExtraHeader(Stream);
  FHeaderSize := Stream.Position - Offset;

  if sfZlib in FFlags then
    Result := Uncompress(Stream, Offset + FHeaderSize)
  else
    Result := Stream;

  if sfAdler32 in FFlags then
  begin
    Result.ReadBuffer(FChecksum, SizeOf(FChecksum));

    if not (scChecksum in SkipChecks) then
    begin
      Result.Position := Offset;
      Checksum := CalculateAdler(Result, FHeaderSize);
      if Checksum <> FChecksum then
        raise EStreamerBadChecksum.Create(FHost, Checksum, FChecksum);
    end;
  end;

  Result.Position := Offset + FHeaderSize + SizeOf(FChecksum);
end;            

procedure TStreamer.CheckSignature(const Read: TStreamerSignature; SkipChecks: TStreamerCheck);
begin
  if not (scSignature in SkipChecks) and (Read <> FSignature) then
    raise EStreamerBadSignature.Create(FHost, Read, FSignature);
end;

procedure TStreamer.CheckVersion(Read: TStreamerVersion; SkipChecks: TStreamerCheck);
begin
  if Read <> FVersion then
    if (not (scOlderVersion in SkipChecks) and (Read < FVersion)) or
       (not (scNewerVersion in SkipChecks) and (Read > FVersion)) then
      raise EStreamerBadVersion.Create(FHost, Read, FVersion);
end;

function TStreamer.ReadAndFree(Stream: TStream; SkipChecks: TStreamerCheck): TStream;
begin
  Result := Read(Stream, SkipChecks);
  if Result <> Stream then
    Stream.Free;
end;             

procedure TStreamer.WriteExtraHeader(Stream: TStream);
begin
end;

procedure TStreamer.ReadExtraHeader(Stream: TStream);
begin
end;

{ Exceptions }

constructor EStreamerEmpty.Create(Host: TObject; Operation: String; Stream: TStream);
begin
  Self.Host := Host;
  Self.Operation := Operation;
  Self.Stream := Stream;
  CreateFmt('Attempted to %s.%s on exhausted stream.', [Host.ClassName, Operation]);
end;

constructor EStreamerBadSignature.Create(Host: TObject; StreamSign, ExpectedSign: TStreamerSignature);
begin
  Self.Host := Host;
  Self.StreamSign := StreamSign;
  Self.ExpectedSign := ExpectedSign;
  CreateFmt('%s stream must start with signature ''%s'' but it starts with ''%s''.',
            [Host.ClassName, ExpectedSign, StreamSign])
end;

constructor EStreamerBadChecksum.Create(Host: TObject; StreamSum, ExpectedSum: DWord);
begin
  Self.Host := Host;
  Self.StreamSum := StreamSum;
  Self.ExpectedSum := ExpectedSum;
  CreateFmt('%s stream checksum (%.8X) is wrong - expected %.8X.',
            [Host.ClassName, StreamSum, ExpectedSum]);
end;

constructor EStreamerBadVersion.Create(Host: TObject; StreamVer, SupportedVer: TStreamerVersion);
begin
  Self.Host := Host;
  Self.StreamVer := StreamVer;
  Self.SupportedVer := SupportedVer;
  CreateFmt('Attempted to load %s stream of unsupported version (%s) - current version is %s.',
            [Host.ClassName, FormatVersion(StreamVer), FormatVersion(SupportedVer)]);
end;

constructor EStreamerCompression.Create(Host: TObject; ZlibMessage: String);
begin
  Self.Host := Host;
  Self.ZlibMessage := ZlibMessage;
  CreateFmt('Zlib reports compression error (''%s'') for %s.', [ZlibMessage, Host.ClassName]);
end;

end.
