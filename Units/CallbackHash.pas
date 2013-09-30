unit CallbackHash;

interface

uses SysUtils, StringsW, StringUtils;

type
  TCallbackHash = class
  private
  protected
    FHash: TObjectHash;

    function GetDuplicates: TDuplicatesEx;
    procedure SetDuplicates(const Value: TDuplicatesEx);
    function GetCaseSens: Boolean;
    procedure SetCaseSens(Value: Boolean);
  public
    constructor Create(AllowDuplicates: Boolean);
    destructor Destroy; override;

    procedure Assign(Source: TCallbackHash);

    procedure Add(Method: TMethod; const Key: WideString = '');
    function Delete(Method: TMethod): Boolean; overload;
    function Delete(const Key: WideString): Boolean; overload;

    // returns True if a callback existed and was replaced, False if it was added.
    function Replace(Key: WideString; Method: TMethod): Boolean;

    procedure ToMethod(Index: Integer; out Callback);
    function ToMethodBy(Key: WideString; out Callback): Boolean;
                                                                              
    function Count: Integer;
    property Duplicates: TDuplicatesEx read GetDuplicates write SetDuplicates default dupError;
    property CaseSensitive: Boolean read GetCaseSens write SetCaseSens default False;

    function JoinKeys(Glue: WideString): WideString;
    function ToString: WideString;    // alias for JoinKeys(', ').
  end;
                           
  TArgList = class
  protected
    FValues: TWideStringArray;
    FCurrent: Integer;

    procedure Parse(Str: WideString); virtual;

    function Get(Index: Integer): WideString;
    procedure Put(Index: Integer; const Value: WideString);
    function GetNumeric(Index: Integer): Integer;
    procedure SetNumeric(Index: Integer; Value: Integer);
  public
    constructor Create(const Str: WideString);

    procedure SetFromString(Str: WideString);
    function ToString(const Separator: WideString = ', '): WideString;

    property Values[Index: Integer]: WideString read Get write Put; default;
    property Numeric[Index: Integer]: Integer read GetNumeric write SetNumeric;

    function Count: Integer;
    function IsEmpty: Boolean;

    procedure Add(const Value: WideString); overload;
    procedure Add(Value: Integer); overload;

    function First(StartIndex: Integer = 0): WideString;
    function Next: WideString;
    function Current: WideString;
    function AnyLeft: Boolean;
  end;

implementation

uses Classes;

type
  TCallback = class
  protected
    FMethod: TMethod;
    FKey: WideString;
  public
    constructor Create(Method: TMethod; Key: WideString = '');
    procedure ToMethod(out Callback);   // Callback must be of type TMethod.

    property Key: WideString read FKey;
    property Code: Pointer read FMethod.Code;
    property Data: Pointer read FMethod.Data;
  end;

{ TCallback }

constructor TCallback.Create(Method: TMethod; Key: WideString = '');
begin
  FMethod := Method;
  FKey := Key;
end;

procedure TCallback.ToMethod(out Callback);
begin
  Move(FMethod, Callback, SIzeOf(TMethod));
end;

{ TCallbackHash }

constructor TCallbackHash.Create;
begin
  FHash := TObjectHash.Create(True);

  CaseSensitive := False;
  Duplicates := StringsW.dupError;
end;

destructor TCallbackHash.Destroy;
begin
  FHash.Free;
  inherited;
end;

procedure TCallbackHash.Assign(Source: TCallbackHash);
begin
  if Source = NIL then
    FHash.Clear
    else if Source is TCallbackHash then
      FHash.Assign(Source.FHash)
      else
        raise EConvertError.CreateFmt('Cannot assign a %s to %s.', [Source.ClassName, ClassName]);
end;

procedure TCallbackHash.Add(Method: TMethod; const Key: WideString = '');
begin
  FHash.AddObject(Key, TCallback.Create(Method, Key));
end;

function TCallbackHash.Delete(Method: TMethod): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with (FHash.Objects[I] as TCallback) do
      if (Code = Method.Code) and (Data = Method.Data) then
      begin
        FHash.Delete(I);
        Result := True;
        Exit;
      end;

  Result := False;
end;
                
function TCallbackHash.Delete(const Key: WideString): Boolean;
begin
  Result := FHash.Delete(Key);
end;

function TCallbackHash.Replace(Key: WideString; Method: TMethod): Boolean;
begin
  Result := Delete(Key);
  Add(Method, Key);
end;

procedure TCallbackHash.ToMethod(Index: Integer; out Callback);
begin
  ( FHash.Objects[Index] as TCallback ).ToMethod(Callback);
end;

function TCallbackHash.ToMethodBy(Key: WideString; out Callback): Boolean;
var
  I: Integer;
begin
  I := FHash.IndexOf(Key);
  Result := I <> -1;
  if Result then
    ToMethod(I, Callback);
end;

function TCallbackHash.JoinKeys(Glue: WideString): WideString;
begin
  Result := FHash.JoinNames(Glue);
end;

function TCallbackHash.ToString: WideString;
begin
  Result := JoinKeys(', ');
end;

function TCallbackHash.GetDuplicates: TDuplicatesEx;
begin
  Result := FHash.Duplicates;
end;

procedure TCallbackHash.SetDuplicates(const Value: TDuplicatesEx);
begin  
  FHash.Sorted := Value <> StringsW.dupIgnore;
  FHash.Duplicates := Value;
end;

function TCallbackHash.Count: Integer;
begin
  Result := FHash.Count;
end;

function TCallbackHash.GetCaseSens: Boolean;
begin
  Result := FHash.CaseSensitive;
end;

procedure TCallbackHash.SetCaseSens(Value: Boolean);
begin
  FHash.CaseSensitive := Value;
end;
    
{ TArgList }

constructor TArgList.Create(const Str: WideString);
begin
  SetFromString(Str);
end;

procedure TArgList.SetFromString(Str: WideString);
begin
  Parse(Str);
  First;
end;

function TArgList.ToString(const Separator: WideString = ', '): WideString;
begin
  Result := Join(FValues, Separator);
end;

function TArgList.Count: Integer;
begin
  Result := Length(FValues);
end;

function TArgList.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TArgList.First(StartIndex: Integer = 0): WideString;
begin
  FCurrent := StartIndex;
  Result := Next;
end;

function TArgList.Next: WideString;
begin
  if not AnyLeft then
    Result := ''
    else
    begin
      Result := Current;
      Inc(FCurrent);
    end;
end;

function TArgList.Current: WideString;
begin
  if not AnyLeft then
    Result := ''
    else
      Result := Values[FCurrent];
end;

function TArgList.AnyLeft: Boolean;
begin
  Result := FCurrent >= Count;
end;

procedure TArgList.Parse(Str: WideString);
begin
  FValues := Explode(',', Str);
  FValues := TrimStringArray(FValues);
end;

function TArgList.Get(Index: Integer): WideString;
begin
  if Index >= Count then
    raise EListError.CreateFmt('No argument with index %d to get (total %d args).', [Index + 1, Count]);
  Result := FValues[Index];
end;

procedure TArgList.Put(Index: Integer; const Value: WideString);
begin
  if Index >= Count then
    raise EListError.CreateFmt('No argument with index %d to set (total %d args).', [Index + 1, Count]);
  FValues[Index] := Value;
end;

function TArgList.GetNumeric(Index: Integer): Integer;
var
  Str: WideString;
begin
  Str := Values[Index];
  if Str = '' then
    Result := 0
    else if not TryStrToIntStrict(Str, Result) then
      raise EConvertError.CreateFmt('Argument %d must be an integer, "%s" given.', [Index + 1, Str]);
end;

procedure TArgList.SetNumeric(Index: Integer; Value: Integer);
begin
  Values[Index] := IntToStr(Value);
end;

procedure TArgList.Add(const Value: WideString);
begin
  SetLength(FValues, Count + 1);
  FValues[Count - 1] := Value;
end;

procedure TArgList.Add(Value: Integer);
begin
  Add(IntToStr(Value));
end;

end.
