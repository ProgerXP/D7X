unit DataProvider;

(* todo: (!) make Reading and Writing buffered (like TFileStream.CopyFrom), whith chuck size like 65 kb *)

interface

uses Contnrs, SysUtils, Windows, Classes; // Windows is used for DWord definition only.

const
  PositionStackSize = $FFFF;

  soFromBeginning = 0;
  soFromCurrent   = 1;
  soFromEnd       = 2;

type
  TDataArray = array of Byte;

  EDataProvider = class (Exception)
  end;

    EDPReadOnly = class (EDataProvider)
      constructor Create(DPClass: String);
    end;

    EDPNotOpened = class (EDataProvider)
      constructor Create(DPClass, Operation: String);
    end;

    EDPBufferEnd = class (EDataProvider)
      constructor Create(DPClass, Operation: String; Expected, Actual: DWord);
    end;

    EDPTooLongStringToRead = class (EDataProvider)
      constructor Create(DPClass, StrType: String; MaxLength: DWord);
    end;

  TDataProvider = class
  protected
    FReadOnly: Boolean;
    FLongestStrToRead: DWord;
    FErrorOnBufferEnd: Boolean;
    FSavedPositions: TStack;

    function GetID: WideString; virtual;
    function GetReadOnly: Boolean; virtual;
    procedure Locked; virtual;

    function GetSize: DWord; virtual; abstract;
    procedure SetSize(NewSize: DWord); virtual; abstract;

    procedure DoSeek(Position: DWord); virtual; abstract;
    function DoTell: DWord; virtual; abstract;
    function DoRead(Dest: Pointer; Size: DWord): DWord; virtual; abstract;
    function DoWrite(Source: Pointer; Size: DWord): DWord; virtual; abstract;

    procedure ChangeInt(Delta: Integer; Size: Byte);
    procedure CheckAmount(Operation: String; Expected, Actual: DWord);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property IsReadOnly: Boolean read GetReadOnly;
    procedure Lock;

    // in chars, not bytes:
    property LongestStrToRead: DWord read FLongestStrToRead write FLongestStrToRead default 1000;
    property ErrorOnBufferEnd: Boolean read FErrorOnBufferEnd write FErrorOnBufferEnd default True;

    function IsOpened: Boolean; virtual;
    // not necessary unique but a string identifying the provider (such as a file name).
    property ID: WideString read GetID;

    function Tell: DWord;
    procedure Seek(Position: DWord); overload;
    property Position: DWord read Tell write Seek;  // alias to Tell.

    property Size: DWord read GetSize write SetSize;
    function BytesLeft: DWord;
    function IsAtEOF: Boolean;

    procedure AdvanceBy(Amount: DWord);
    procedure RewindBy(Amount: DWord);
    procedure Seek(Position: Integer; Direction: Word); overload;
    procedure ToTheStart;
    procedure ToTheEnd;

    { note: buffer (Dest and Source) is already allocated. It is freed by the called so no need to FreeMem it here. }
    function Read(Dest: Pointer; Size: DWord): DWord; virtual;
    function ReadFrom(Position: DWord; Dest: Pointer; Size: DWord): DWord;
    function Write(Source: Pointer; Size: DWord): DWord; virtual;
    function WriteAt(Position: DWord; Source: Pointer; Size: DWord): DWord;

    { Utility fnctions. }
    function SavePosition: DWord;        // saves and returns the current position (= Tell).
    function LoadPosition: DWord;       // restores last saved position and returns it.
    function DropPosition: DWord;       // returns last saved position removing it from the list.
    function LastSavedPosition: DWord;  // only returns the position without restoring it.
    procedure ShortenBy(Amount: DWord);
    procedure Trim; // makes file end right at the current position. like Size := Tell;

    procedure ChangeDWordAt(Position: DWord; Delta: Integer);
    procedure ChangeDWord(Delta: Integer);
    procedure ChangeWordAt(Position: DWord; Delta: Integer);
    procedure ChangeWord(Delta: Integer);
    procedure ChangeByteAt(Position: DWord; Delta: SmallInt);
    procedure ChangeByte(Delta: SmallInt);

    function CompareAt(Position: DWord; Buf: String): Boolean;
    function CompareWith(Buf: String): Boolean;
    // will return to original position after inserting.
    // NewSize can be less than OldSize (then it will remove extra bytes).
    procedure InsertBytesAt(Position: DWord; OldSize, NewSize: DWord);
    procedure InsertBytes(OldSize, NewSize: DWord);
    // when Size = 0 it will copy entire Source (but from the current posiiton of Self).
    function CopyFrom(Source: TStream; Size: DWord = 0): DWord; overload;
    function CopyFrom(Source: TDataProvider; Size: DWord = 0): DWord; overload;
    // saves the whole data, from the beginning to the end. returns number of bytes written.
    function SaveDataToFile(const FileName: WideString): DWord; overload;
    // saves data from the current position and with the length of Size.
    function SaveDataToFile(const FileName: WideString; Size: DWord): DWord; overload;
    // sets Self as a full copy (position, size, content) of CopyFrom.
    procedure Replicate(Other: TDataProvider);

    procedure ReadArray(var WSArray: array of WideString); overload;
    procedure ReadArray(var DWArray: array of DWord); overload;
    function ReadWS: WideString;

    function ReadASCIIZ: String;
    function ReadUnicodeZ: WideString;
    function ReadStrZ: String;      // alias for ReadASCIIZ.
    function ReadWideZ: WideString; // alias for ReadUnicodeZ.

    procedure WriteWS(const Str: WideString);
    procedure WriteArray(const WSArray: array of WideString); overload;
    procedure WriteArray(const DWArray: array of DWord); overload;
  end;

implementation

uses DPFile, DPStream, Math;

{ Exceptions }

constructor EDPReadOnly.Create(DPClass: String);
begin
  CreateFmt('Cannot write to the read-only %s data provider.', [DPClass]);
end;                

constructor EDPNotOpened.Create(DPClass, Operation: String);
begin
  CreateFmt('Attempted to %s from a non-opened %s data provider.', [Operation, DPClass]);
end;                  

constructor EDPBufferEnd.Create(DPClass, Operation: String; Expected, Actual: DWord);
begin
  CreateFmt('%s data provider expected to %s %d bytes but only %d were.', [DPCLass, Operation, Expected, Actual]);
end;

constructor EDPTooLongStringToRead.Create(DPClass, StrType: String; MaxLength: DWord);
begin
  CreateFmt('%s data provider was reading %s string but it exceeded the max limit of %d characters.', [DPClass, StrType, MaxLength]);
end;

{ TDataProvider }

constructor TDataProvider.Create;
begin
  FSavedPositions := TStack.Create;
  FReadOnly := False;
  FLongestStrToRead := 1000;
  FErrorOnBufferEnd := True;
end;

destructor TDataProvider.Destroy;
begin
  FSavedPositions.Free;
  inherited;
end;

function TDataProvider.GetReadOnly;
begin
  Result := FReadOnly or not IsOpened
end;

function TDataProvider.IsAtEOF;
begin
  Result := not IsOpened or (Tell >= Size)
end;

procedure TDataProvider.Lock;
begin
  FReadOnly := True;
  Locked
end;

procedure TDataProvider.Locked;
begin
end;

function TDataProvider.IsOpened: Boolean;
begin
  Result := True;
end;

procedure TDataProvider.CheckAmount(Operation: String; Expected, Actual: DWord);
begin
  if FErrorOnBufferEnd and (Expected <> Actual) then
    raise EDPBufferEnd.Create(ClassName, Operation, Expected, Actual);
end;

function TDataProvider.Read;
begin
  Size := Min(BytesLeft, Size);
  if Size = 0 then
    Result := 0
    else if not IsOpened then
      raise EDPNotOpened.Create(ClassName, 'read')
      else
      begin
        Result := DoRead(Dest, Size);
        AdvanceBy(Result);           
        CheckAmount('read', Size, Result);
      end;
end;

function TDataProvider.ReadFrom;
begin
  Seek(Position);
  Result := Read(Dest, Size);
end;

function TDataProvider.WriteAt;
begin
  Seek(Position);
  Result := Write(Source, Size);
end;

function TDataProvider.Write;
begin
  if not IsReadOnly then
  begin
    if Self.Size < Tell + Size then
      Self.Size := Tell + Size;

    Result := DoWrite(Source, Size);
    AdvanceBy(Result);
    CheckAmount('write', Size, Result);
  end
    else if not IsOpened then
      raise EDPNotOpened.Create(ClassName, 'write')
      else
        raise EDPReadOnly.Create(ClassName);
end;

function TDataProvider.CompareAt(Position: DWord; Buf: String): Boolean;
begin
  Seek(Position);
  Result := CompareWith(Buf);
end;

function TDataProvider.CompareWith;
var
  Current: String;
begin
  SavePosition;
  SetLength(Current, Length(Buf));
  Read(@Current[1], Length(Buf));
  Result := Current = Buf;
  if Result then
    DropPosition
    else
      LoadPosition;
end;

procedure TDataProvider.ReadArray(var WSArray: array of WideString);
var
  I: Word;
begin
  if Length(WSArray) <> 0 then
    for I := 0 to Length(WSArray) - 1 do
      WSArray[I] := ReadWS;
end;

procedure TDataProvider.ReadArray(var DWArray: array of DWord);
begin
  if Length(DWArray) <> 0 then
    Read(@DWArray[0], SizeOf(DWArray[0]) * Length(DWArray));
end;

function TDataProvider.ReadWS: WideString;
var
  Len: Word;
begin
  Read(@Len, 2);
  SetLength(Result, Len);
  Read(@Result[1], Len * 2)
end;

procedure TDataProvider.WriteArray(const WSArray: array of WideString);
var
  I: Word;
begin
  if Length(WSArray) <> 0 then
    for I := 0 to Length(WSArray) -  1 do
      WriteWS(WSArray[I]);
end;

procedure TDataProvider.WriteArray(const DWArray: array of DWord);
begin
  if Length(DWArray) <> 0 then
    Write(@DWArray[0], SizeOf(DWArray[0]) * Length(DWArray))
end;

procedure TDataProvider.WriteWS(const Str: WideString);
var
  Len: Word;
begin
  Len := Length(Str);
  Write(@Len, SizeOf(Len));
  Write(@Str[1], Len * 2);
end;

procedure TDataProvider.AdvanceBy;
begin
  Seek(Amount, soFromCurrent);
end;

procedure TDataProvider.RewindBy;
begin
  Seek(-Amount, soFromCurrent);
end;

procedure TDataProvider.Seek(Position: Integer; Direction: Word);
begin
  case Direction of
  soFromBeginning:  Seek(Position);
  soFromCurrent:    Seek(Max(0, Tell + Int64(Position)));
  soFromEnd:        Seek(Max(0, Size - Int64(Position)));
  end;
end;

procedure TDataProvider.Seek(Position: DWord);
begin
  if IsOpened then
    DoSeek(Min(Size, Position))
    else
      raise EDPNotOpened.Create(ClassName, 'seek');
end;

function TDataProvider.Tell: DWord;
begin
  if IsOpened then
    Result := DoTell
    else
      raise EDPNotOpened.Create(ClassName, 'tell');
end;

procedure TDataProvider.ToTheStart;
begin
  Seek(0);
end;

procedure TDataProvider.ToTheEnd;
begin
  Seek(Size);
end;

function TDataProvider.BytesLeft: DWord;
begin
  Result := Max(0, Size - Tell);
end;

procedure TDataProvider.ShortenBy(Amount: DWord);
begin
  Size := Size - Amount;
end;

procedure TDataProvider.Trim;
begin
  Size := Tell;
end;

function TDataProvider.SavePosition: DWord;
begin
  Result := Tell;
  FSavedPositions.Push(Ptr(Result));
end;

function TDataProvider.LoadPosition: DWord;
begin
  Result := DWord(FSavedPositions.Pop);
  Seek(Result);
end;

function TDataProvider.DropPosition: DWord;
begin
  Result := DWord(FSavedPositions.Pop);
end;

function TDataProvider.LastSavedPosition: DWord;
begin
  Result := DWord(FSavedPositions.Peek);
end;

procedure TDataProvider.InsertBytesAt(Position, OldSize, NewSize: DWord);
begin
  Seek(Position);
  InsertBytes(OldSize, NewSize);
end;

procedure TDataProvider.InsertBytes(OldSize, NewSize: DWord);
var
  BufSize: DWord;
  Buf: Pointer;
begin
  // todo: make it use MaxBufSize (or at least don't hold the whole chunk in the memory?
  //       use an array of pointers?)

  SavePosition;
  AdvanceBy(OldSize);

  BufSize := Size - Tell;
  GetMem(Buf, BufSize);
  try
    Read(Buf, BufSize);

    Size := Size + NewSize - OldSize;
    WriteAt(LastSavedPosition + NewSize, Buf, BufSize);
  finally
    FreeMem(Buf, BufSize);
    Seek(LoadPosition);
  end;
end;

function TDataProvider.CopyFrom(Source: TStream; Size: DWord): DWord;
var
  StreamAsDP: TDPStream;
begin
  StreamAsDP := TDPStream.Create(Source);
  try
    Result := CopyFrom(StreamAsDP, Size);
  finally
    StreamAsDP.Free;
  end;
end;

// Classes.pas: 4941.
function TDataProvider.CopyFrom(Source: TDataProvider; Size: DWord = 0): DWord;
const
  MaxBufSize = $F000;
var
  BufSize, N: Integer;
  Buffer: Pointer;
begin
  Result := 0;
  if Size = 0 then
    Size := Source.Size;

  BufSize := Min(MaxBufSize, Size);
  GetMem(Buffer, BufSize);
  try
    while Size <> 0 do
    begin
      N := Min(BufSize, Size);
      Inc(Result, Source.Read(Buffer, N));
      Write(Buffer, N);
      Dec(Size, N);
    end;
  finally
    FreeMem(Buffer, BufSize);
  end;
end;

procedure TDataProvider.ChangeDWordAt(Position: DWord; Delta: Integer);
begin
  Seek(Position);
  ChangeDWord(Delta);
end;

procedure TDataProvider.ChangeWordAt(Position: DWord; Delta: Integer);
begin
  Seek(Position);
  ChangeWord(Delta);
end;

procedure TDataProvider.ChangeByteAt(Position: DWord; Delta: SmallInt);
begin
  Seek(Position);
  ChangeByte(Delta);
end;

procedure TDataProvider.ChangeDWord(Delta: Integer);
begin
  ChangeInt(Delta, 4);
end;

procedure TDataProvider.ChangeWord(Delta: Integer);
begin
  ChangeInt(Delta, 2);
end;

procedure TDataProvider.ChangeByte(Delta: SmallInt);
begin
  ChangeInt(Delta, 1);
end;

procedure TDataProvider.ChangeInt(Delta: Integer; Size: Byte);
var
  Int: Integer;
begin
  if Delta <> 0 then
  begin
    Read(@Int, Size);
    Inc(Int, Delta);
    RewindBy(Size);
    Write(@Int, Size);
  end;
end;

function TDataProvider.GetID: WideString;
begin
  Result := '<' + ClassName + '>';
end;

function TDataProvider.ReadASCIIZ: String;
var
  Len: Word;
  Ch: Byte;
begin
  SetLength(Result, FLongestStrToRead);
  Len := 1;

  while Len < FLongestStrToRead do
  begin
    Read(@Ch, SizeOf(Ch));
    if Ch = 0 then
    begin
      SetLength(Result, Len - 1);
      Exit;
    end;
    Result[Len] := Char(Ch);
    Inc(Len);
  end;

  raise EDPTooLongStringToRead.Create(ClassName, 'ASCIIZ', FLongestStrToRead);
end;

function TDataProvider.ReadUnicodeZ: WideString;
var
  Len, Ch: Word;
begin
  SetLength(Result, FLongestStrToRead);
  Len := 1;

  while Len < FLongestStrToRead do
  begin
    Read(@Ch, SizeOf(Ch));
    if Ch = 0 then
    begin
      SetLength(Result, Len - 1);
      Exit;
    end;
    Result[Len] := WideChar(Ch);
    Inc(Len);
  end;

  raise EDPTooLongStringToRead.Create(ClassName, 'UnicodeZ', FLongestStrToRead);
end;

function TDataProvider.ReadStrZ: String;
begin
  Result := ReadASCIIZ;
end;

function TDataProvider.ReadWideZ: WideString;
begin
  Result := ReadUnicodeZ;
end;

function TDataProvider.SaveDataToFile(const FileName: WideString): DWord;
begin
  SavePosition;
  try
    ToTheStart;
    Result := SaveDataToFile(FileName, Size);
  finally
    LoadPosition;
  end;
end;

function TDataProvider.SaveDataToFile(const FileName: WideString; Size: DWord): DWord;
var
  F: TDPFile;
begin
  F := TDPFile.CreateNew(FileName);
  try
    Result := F.CopyFrom(Self, Size);
  finally
    F.Free;
  end;
end;

procedure TDataProvider.Replicate(Other: TDataProvider);
begin
  Other.SavePosition;
  try
    Size := Other.Size;
    ToTheStart;
    Other.ToTheStart;
    CopyFrom(Other, Size);
  finally
    Seek(Other.LoadPosition);
  end;
end;

end.
