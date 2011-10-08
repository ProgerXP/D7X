unit DPMemoryWrapper;

// todo: fix: IT DOES not advance FPosition when reading/writing.

interface

uses CommonExceptions, DataProvider, Windows, Classes;

type
  TBufExceededCallback = procedure (Requested, Limit: DWord);

  DPMemoryWrapper = class (TDataProvider)
  protected
    FUnderlying: TDataProvider;

    FBuffer: Pointer;
    FAllocated: TRange;
    FMaxBuffer: DWord;
    FBufExceededCallback: TBufExceededCallback;

    function GetReadOnly: Boolean; override;
    procedure SetUnderlying(Value: TDataProvider);

    procedure ResetAllocated;
    procedure BufExceeded(Requested, Limit: DWord);
    procedure ReadFromBuf(Dest: Pointer; Size: DWord);

    procedure Seek(Position: DWord); override;
    procedure DoRead(Dest: Pointer; Size: DWord); override;
    procedure DoWrite(Source: Pointer; Size: DWord); override;
  public
    constructor Create; virtual;

    property BufExceededCallback: TBufExceededCallback write FBufExceededCallback;

    property Underlying: TDataProvider read FUnderlying write SetUnderlying;
    function Release: TDataProvider;

    function Unassigned: Boolean;
    function IsAllocated: Boolean;

    procedure Clear;

    function IsAvailable(Position: DWord); overload;
    function IsAvailable(FromTill: TRange); overload;

    function TransferAll: Boolean;
    function TransferFrom(StartPos: DWord): Boolean;
    function Transfer(StartPos, FinishPos: DWord): Boolean;
  end;

implementation

constructor DPMemoryWrapper.Create;
begin
  inherited;

  FUnderlying := NIL;
  FBufExceededCallback := NIL;

  FAllocated := TRange.Create(0, 0);
  ResetAllocated
end;

procedure DPMemoryWrapper.SetUnderlying;
begin
  Release;
  FUnderlying := Value
end;

function DPMemoryWrapper.Unassigned;
begin
  Result := FUnderlying = NIL
end;

function DPMemoryWrapper.IsAllocated;
begin
  Result := FAllocated.Valid
end;

function DPMemoryWrapper.Release;
begin
  Result := FUnderlying;
  FUnderlying := NIL
end;

function DPMemoryWrapper.GetReadOnly;
begin
  Result := Unassigned or FUnderlying.IsReadOnly
end;

procedure DPMemoryWrapper.Clear;
begin
	if IsAllocated then
	begin
		FreeMem(FBuffer, FAllocated.Extent);
		ResetAllocated
	end
end;

procedure DPMemoryWrapper.ResetAllocated;
begin
  // range should be  not Valid
  FAllocated.SetTo(100, 0)
end;

function DPMemoryWrapper.IsAvailable(Position: DWord);
begin
  Result := FAllocated.Include(Position)
end;

function DPMemoryWrapper.IsAvailable(FromTill: TRange);
begin
  Result := FAllocated.Include(FromTill)
end;

function DPMemoryWrapper.TransferAll;
begin
  Result := TransferFrom(0)
end;

function DPMemoryWrapper.TransferFrom;
begin
  Result := Transfer(StartPos, StartPos + FMaxBuffer - 1)
end;

function DPMemoryWrapper.Transfer;
var
  Size: DWord;
begin
  if not Unassigned then
  begin
    Clear;

    Size := FinishPos - StartPos;
    Result := Size > FMaxBuffer;
    if not Result then
    begin
      BufExceeded(Size, FMaxBuffer);
      Exit
    end;

    try
      GetMem(FBuffer, Size);
      Read(FBuffer, Size)
    except
      FreeMem(FBuffer, Size);
      raise
    end;

    FAllocated.SetTo(StartPos, FinishPos);
    Seek(0)
  end
    else
      raise ENotOpened.Create(ClassName, 'transferring')
end;

procedure DPMemoryWrapper.BufExceeded;
begin
  if Assigned(FBufExceededCallback) then
    FBufExceededCallback(Requested, Limit)
    else
      raise EBufferExceeded.Create(ClassName, Requested, Limit)
end;

procedure DPMemoryWrapper.Seek;
begin
  if not Unassigned then
    if IsAllocated then
      FBufPosition := Position
      else
        FUnderlying.Seek(Position)
    else
      raise ENotOpened.Create(ClassName, 'seeking')
end;

procedure DPMemoryWrapper.DoRead;
begin
  if not Unassigned then
    if IsAllocated then
      ReadFromBuf(Dest, Size)
      else
        FUnderlying.DoRead(Dest, Size)
    else
      raise ENotOpened.Create(ClassName, 'reading')
end;

procedure DPMemoryWrapper.ReadFromBuf;
begin
  Move(FBuffer[FBufPosition]^, Dest^, Size)
end;

procedure DPMemoryWrapper.DoWrite;
begin
  if not Unassigned then
    FUnderlying.DoWrite(Source, Size)
    else
      raise ENotOpened.Create(ClassName, 'seeking')
end;

end.
