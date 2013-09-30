unit Stacks;

interface

uses Windows; // Windows is used for DWord definition only.

type
  TNumberStack = class
  protected
    FStack: array of DWord;
    FFreeIndex: DWord;

    function GetMaxStackSize: DWord;
    procedure SetMaxStackSize(const Value: DWord);
  public
    constructor Create; virtual;
                    
    property MaxStackSize: DWord read GetMaxStackSize write SetMaxStackSize;
    property Count: DWord read FFreeIndex;
    procedure Clear;

    procedure Push(Number: DWord);
    function TopValue: DWord;
    procedure WipeOut(Count: DWord);
    function Pop: DWord;
  end;
  
  TStringStack = class
  protected
    FStack: array of WideString;
    FFreeIndex: DWord;

    function GetMaxStackSize: DWord;
    procedure SetMaxStackSize(const Value: DWord);
  public
    constructor Create; virtual;
                    
    property MaxStackSize: DWord read GetMaxStackSize write SetMaxStackSize;
    property Count: DWord read FFreeIndex;
    procedure Clear;

    procedure Push(Str: WideString);
    function TopValue: WideString;    
    procedure WipeOut(Count: DWord);
    function Pop: WideString;
  end;

implementation

{ TNumberStack }

constructor TNumberStack.Create;
begin
  MaxStackSize := $FFFF;                
  Clear;
end;

function TNumberStack.TopValue: DWord;
begin
  if FFreeIndex = 0 then
    Result := 0
    else
      Result := FStack[FFreeIndex - 1];
end;

function TNumberStack.Pop: DWord;
begin
  Result := TopValue;
  if FFreeIndex <> 0 then
    Dec(FFreeIndex);
end;

procedure TNumberStack.Push(Number: DWord);
begin
  FStack[FFreeIndex] := Number;
  Inc(FFreeIndex);
end;

function TNumberStack.GetMaxStackSize: DWord;
begin
  Result := Length(FStack);
end;

procedure TNumberStack.SetMaxStackSize(const Value: DWord);
begin
  SetLength(FStack, Value);
end;

procedure TNumberStack.Clear;
begin
  FFreeIndex := 0;
end;        

procedure TNumberStack.WipeOut(Count: DWord);
begin
  if Integer(FFreeIndex - Count) < 0 then
    Clear
    else
      Dec(FFreeIndex, Count);
end;

{ TStringStack }

constructor TStringStack.Create;
begin
  MaxStackSize := $FFFF;                
  Clear;
end;

function TStringStack.TopValue: WideString;
begin
  if FFreeIndex = 0 then
    Result := ''
    else
      Result := FStack[FFreeIndex - 1];
end;

function TStringStack.Pop: WideString;
begin
  Result := TopValue;
  if FFreeIndex <> 0 then
    Dec(FFreeIndex);
end;

procedure TStringStack.Push(Str: WideString);
begin
  FStack[FFreeIndex] := Str;
  Inc(FFreeIndex);
end;

function TStringStack.GetMaxStackSize: DWord;
begin
  Result := Length(FStack);
end;

procedure TStringStack.SetMaxStackSize(const Value: DWord);
begin
  SetLength(FStack, Value);
end;

procedure TStringStack.Clear;
begin
  FFreeIndex := 0;
end;

procedure TStringStack.WipeOut(Count: DWord);
begin
  if Integer(FFreeIndex - Count) < 0 then
    Clear
    else
      Dec(FFreeIndex, Count);
end;

end.