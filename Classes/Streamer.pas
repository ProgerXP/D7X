unit Streamer;

interface

uses Windows, SysUtils, Classes, ZLibEx;

type
  TStreamFlahs = set of (ZlibbedStream, Adler32Checksummed);
  TZCompressionLevel = ZLibEx.TZCompressionLevel;

  EStreamer = class (Exception)
  end;

    ENonEmptyStreamer = class (EStreamer)
    public
      constructor Create(ClassName: String; Op: String);
    end;

    EStreamSignature = class (EStreamer)
    public
      constructor Create(ClassName: String; ExpectedSign, ReadSign: WideString);
    end;

    EStreamChecksum = class (EStreamer)
    public
      constructor Create(ClassName: String; ExpectedChecksuim, ReadChecksum: DWord);
    end;

    EUnsupportedVersion = class (EStreamer)
    public
      constructor Create(ClassName: String; Version, SupportedVersion: Word);
    end;

  TStreamer = class
  protected
    FSignature: String;
    FVersion: Word;
    FFlags: TStreamFlahs;

    FHeaderSize: Word; // is set in WriteTo.
    FCompressionLevel: TZCompressionLevel;

    procedure WriteAdlerTo(const Stream: TStream);
    procedure Compress(const Stream: TStream);

    procedure CheckAdlerIn(const Stream: TStream);
    function Uncompress(const Stream: TStream): TMemoryStream;
  public
    constructor Create(Signature: String; Version: Word);

    property Signature: String read FSignature;
    property Version: Word read FVersion;
    // Flahs are written in WriteTo and read in Validate.
    property Flags: TStreamFlahs read FFlags write FFlags;

    procedure WriteTo(const Stream: TStream);
    procedure PostWrite(const Stream: TStream);

    // it can return Stream so check if they are not equal before Freeing its result.
    function GetStream(const Stream: TStream): TStream;
  end;

{$L adler32.obj}
function Adler32(Adler: LongInt; const Buf; Len: Integer): LongInt; external;

implementation

uses StringUtils;

{ Exceptions }

constructor ENonEmptyStreamer.Create(ClassName: String; Op: String);
begin
  CreateFmt('%s.%s should be used on an empty stream (when Stream.Position = 0).', [ClassName, Op]);
end;

constructor EStreamSignature.Create(ClassName: String; ExpectedSign, ReadSign: WideString);
begin
  CreateFmt('%s expected to read signature "%s" but actually read "%s"', [ClassName, ExpectedSign, ReadSign])
end;

constructor EStreamChecksum.Create(ClassName: String; ExpectedChecksuim, ReadChecksum: DWord);
begin
  CreateFmt('%s encountered wrong checksum: expected %.8x but calculated %.8x.',
            [ClassName, ExpectedChecksuim, ReadChecksum]);
end;

constructor EUnsupportedVersion.Create(ClassName: String; Version, SupportedVersion: Word);
begin
  CreateFmt('%s cannot load a stream because it''s of unsupported version %s - only supporting %s',
            [ClassName, FormatVersion(Version), FormatVersion(SupportedVersion)]);
end;

{ VersionControl }

constructor TStreamer.Create(Signature: String; Version: Word);
begin
  FSignature := Signature;
  FVersion := Version;
  FFlags := [ZlibbedStream, Adler32Checksummed];
  FCompressionLevel := zcDefault;
end;

procedure TStreamer.WriteTo(const Stream: TStream);
var
  SpaceForChecksum: DWord;
begin
  if Stream.Position <> 0 then
    raise ENonEmptyStreamer.Create(ClassName, 'WriteLn');

  Stream.Write(FSignature[1], Length(FSignature));
  Stream.Write(FVersion, SizeOf(FVersion));
  Stream.Write(FFlags, SizeOf(FFlags));
  FHeaderSize := Stream.Position;

  SpaceForChecksum := 0;
  Stream.Write(SpaceForChecksum, 4); // it's not included in the header.
end;

procedure TStreamer.PostWrite(const Stream: TStream);
begin
  if Adler32Checksummed in FFlags then
    WriteAdlerTo(Stream);
  if ZlibbedStream in FFlags then
    Compress(Stream);
end;

procedure TStreamer.Compress(const Stream: TStream);
var
  Buf, CompressedBuf: Pointer;
  Size, CompressedSize: Integer;
begin
  Size := Stream.Size - FHeaderSize;
  GetMem(Buf, Size);
  try
    Stream.Position := FHeaderSize;
    Stream.Read(Buf^, Size);

    ZCompress(Buf, Size, CompressedBuf, CompressedSize, FCompressionLevel);
    try
      if CompressedSize <> 0 then
      begin
        Stream.Size := FHeaderSize + CompressedSize;
        Stream.Position := FHeaderSize;
        Stream.Write(CompressedBuf^, CompressedSize);
      end;
    finally
      FreeMem(CompressedBuf, CompressedSize);
    end;
  finally
    FreeMem(Buf, Size);
  end;
end;

procedure TStreamer.WriteAdlerTo(const Stream: TStream);
var
  Buf: Pointer;
  Adler: Integer;
begin
  GetMem(Buf, Stream.Size);
  try
    Stream.Position := 0;
    Stream.Read(Buf^, Stream.Size);

    Adler := Adler32(0, Buf^, Stream.Size);
    Stream.Position := FHeaderSize;
    Stream.Write(Adler, 4);
  finally
    FreeMem(Buf, Stream.Size);
  end;
end;

function TStreamer.GetStream(const Stream: TStream): TStream;
var
  SignatureRead: String;
  VersionRead: Word;
begin
  if Stream.Position <> 0 then
    raise ENonEmptyStreamer.Create(ClassName, 'GetStream');

  SetLength(SignatureRead, Length(FSignature));
  Stream.Read(SignatureRead[1], Length(SignatureRead));
  if FSignature <> SignatureRead then
    raise EStreamSignature.Create(ClassName, FSignature, SignatureRead);

  Stream.Read(VersionRead, SizeOf(FVersion));
  if FVersion <> VersionRead then
    raise EUnsupportedVersion.Create(ClassName, VersionRead, FVersion);

  Stream.Read(FFlags, SizeOf(FFlags));
  FHeaderSize := Stream.Position;

  if ZlibbedStream in FFlags then
    Result := Uncompress(Stream)
    else
      Result := Stream;

  if Adler32Checksummed in FFlags then
    CheckAdlerIn(Result);

  Result.Position := FHeaderSize + 4;
end;

function TStreamer.Uncompress(const Stream: TStream): TMemoryStream;
var
  Buf, Uncomp: Pointer;
  Size, UncompSize: Integer;
begin
  Result := NIL;

  Size := Stream.Size - FHeaderSize;
  GetMem(Buf, Size);
  try
    Stream.Position := FHeaderSize;
    Stream.Read(Buf^, Size);

    ZDecompress(Buf, Size, Uncomp, UncompSize);
    try
      if UncompSize <> 0 then
      begin
        Result := TMemoryStream.Create;
        Result.Size := FHeaderSize + UncompSize;
        Result.Position := 0;

        Stream.Position := 0;
        Result.CopyFrom(Stream, FHeaderSize);
        Result.Write(Uncomp^, UncompSize);
      end;
    finally
      FreeMem(Uncomp, UncompSize);
    end;
  finally
    FreeMem(Buf, Size);
  end;
end;

procedure TStreamer.CheckAdlerIn(const Stream: TStream);
var
  Buf: Pointer;
  AdlerRead, AdlerCalculated: DWord;
begin
  GetMem(Buf, Stream.Size);
  try
    Stream.Position := 0;
    Stream.Read(Buf^, Stream.Size);
    DWord(Ptr( DWord(Buf) + FHeaderSize )^) := 0;

    Stream.Position := FHeaderSize;
    Stream.Read(AdlerRead, 4);

    AdlerCalculated := Adler32(0, Buf^, Stream.Size);
    if AdlerRead <> AdlerCalculated then
      raise EStreamChecksum.Create(ClassName, AdlerCalculated, AdlerRead);
  finally
    FreeMem(Buf, Stream.Size);
  end;
end;

end.
