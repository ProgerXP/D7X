unit BufStream;

interface

uses Windows, SysUtils, Classes, Contnrs;

type
{
  Doesn't track changes (writes) to Stream. If done, buffers may run out of sync.
  You can use Refresh to clear the buffers.
}
  TBufStream = class
  protected
    FStream: TStream;
    FOwnsStream: Boolean;
    FBuffers: TObjectList;  // of TBuffer.
    FBufCount, FBufSize: Integer;
    FLastStreamSize: Int64;

    procedure Reading;
    procedure DoneReading;
    function FillBuffer(Pos, Size: Int64): Integer;
  public
    constructor Create(Stream: TStream; Own: Boolean = True);
    destructor Destroy; override;

    procedure Rebind(Value: TStream);
    property Stream: TStream read FStream write Rebind;
    property OwnsStream: Boolean read FOwnsStream write FOwnsStream default True;
    procedure Refresh;
    // BufCount of 0 turns off the buffering.
    procedure Resize(BufCount, BufSize: Integer);
    property BufCount: Integer read FBufCount;
    property BufSize: Integer read FBufSize;

    function Read(var Buf; const Pos: Int64; Size: Integer): Integer; overload;
    function Read(const Pos: Int64; Size: Integer): AnsiString; overload;
    procedure ReadBuffer(var Buf; const Pos: Int64; Size: Integer);
  end;

implementation

uses Math, RTLConsts;

type
  TBuffer = class
  public
    BufMemory: array of Char;
    BufPos: Int64;
    BufSize: Integer;

    constructor Create(const Pos: Int64; Size: Integer);
    procedure RangeError(const Offset: Int64);
    function At(Pos: Int64): Pointer;
    function Left(Pos: Int64; Size: Integer): Int64;
    procedure Resize(Size: Integer);
    function Contains(const Pos: Int64; Size: Integer): Boolean;
  end;

{ TBufStream }

constructor TBufStream.Create(Stream: TStream; Own: Boolean);
begin
  FStream := Stream;
  FOwnsStream := Own;
  FBuffers := TObjectList.Create;
  Resize(8, 65536);   // 8 x 64K
end;

destructor TBufStream.Destroy;
begin
  FBuffers.Free;
  if FOwnsStream then
    FStream.Free;
  inherited;
end;

function TBufStream.Read(const Pos: Int64; Size: Integer): AnsiString;
begin
  SetLength(Result, Size);
  SetLength(Result, Read(Result[1], Pos, Size));
end;

function TBufStream.Read(var Buf; const Pos: Int64; Size: Integer): Integer;
var
  I: Integer;
begin
  if Size = 0 then
    Result := 0
  else
  begin
    Reading;
    I := FillBuffer(Pos, Size);

    if I = -1 then
    begin
      FStream.Position := Pos;
      Result := FStream.Read(Buf, Size);
    end
    else
      with FBuffers[I] as TBuffer do
      begin
        Result := Left(Pos, Size);
        Move(At(Pos)^, Buf, Result);
      end;

    DoneReading;
  end;
end;

procedure TBufStream.ReadBuffer(var Buf; const Pos: Int64; Size: Integer);
begin
  if Read(Buf, Pos, Size) <> Size then
    raise EReadError.CreateRes(@SReadError);
end;

procedure TBufStream.Refresh;
begin
  FBuffers.Clear;
end;

procedure TBufStream.Resize(BufCount, BufSize: Integer);
begin
  if (BufCount < 0) or (BufSize < 0) then
    BufCount := 0;
  if BufSize > 0 then
    FBufSize := BufSize;
    
  FBufCount := BufCount;

  while BufCount < FBuffers.Count do
    FBuffers.Delete(0);
end;

function TBufStream.FillBuffer(Pos, Size: Int64): Integer;
var
  Buf: TBuffer;
begin
  Size := Min(Size, FStream.Size - Pos);

  if (FBufCount = 0) or (Size <= 0) or (Size > FBufSize) then
    Result := -1
  else
  begin
    for Result := 0 to FBuffers.Count - 1 do
      with FBuffers[Result] as TBuffer do
        if Contains(Pos, Size) then
          Exit;

    if FBufCount <= FBuffers.Count then
      FBuffers.Delete(0);   

    Buf := TBuffer.Create(Pos, Min(FStream.Size - Pos, FBufSize));
    Result := FBuffers.Add(Buf);
    FStream.Position := Pos;
    Buf.Resize( FStream.Read(Buf.BufMemory[0], Buf.BufSize) );
  end;
end;

procedure TBufStream.Reading;
begin
  if FStream.Size <> FLastStreamSize then
    Refresh;
end;

procedure TBufStream.DoneReading;
begin
  FLastStreamSize := FStream.Size;
end;

procedure TBufStream.Rebind(Value: TStream);
begin
  if Value = nil then
    raise EInvalidPointer.CreateFmt('Attempted to set nil Stream for %s.', [ClassName]);
  Refresh;
  FStream := Value;
end;

{ TBuffer }

constructor TBuffer.Create(const Pos: Int64; Size: Integer);
begin                        
  BufPos := Pos;
  BufSize := Size;
  SetLength(BufMemory, Size);
end;

procedure TBuffer.RangeError(const Offset: Int64);
begin
  raise ERangeError.CreateFmt('Offset %d is out of scope of this stream buffer.', [Offset]);
end;

function TBuffer.At(Pos: Int64): Pointer;
begin
  Dec(Pos, BufPos);
  if (Pos < 0) or (Pos >= BufSize) then
  begin
    Result := nil;
    RangeError(Pos);
  end
  else
    Result := @BufMemory[Pos];
end;

function TBuffer.Left(Pos: Int64; Size: Integer): Int64;
begin
  Result := Pos;
  Dec(Result, BufPos);
  if Result < 0 then
    RangeError(Pos);
  Result := Min(BufSize - Result, Size);
  if Result < 0 then
    Result := 0;
end;

function TBuffer.Contains(const Pos: Int64; Size: Integer): Boolean;
begin
  Result := (BufPos <= Pos) and (Pos + Size <= BufPos + BufSize);
end;

procedure TBuffer.Resize(Size: Integer);
begin
  SetLength(BufMemory, Size);
  BufSize := Size;
end;

end.
