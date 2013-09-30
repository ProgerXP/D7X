unit RPNit;

{
  This unit is a part of the D7X Library for Delphi 7.
  RPNit by Proger_XP | http://proger.i-forge.net/RPNit
}

interface

uses Windows, SysUtils, Math, Classes, Contnrs, StringsW, StringUtils;

type
  TRpnEvaluator = class;
  TRpnOperatorClass = class of TRpnOperator;
  TRpnVariableClass = class of TRpnVariable;

  TRpnValueKind = (valNum, valBool, valBytes, valStr, valCustom);
  TRpnValueKinds = set of TRpnValueKind;

  PRpnScalar = ^TRpnScalar;
  TRpnScalar = record
    Kind: TRpnValueKinds;

    Num: Double;
    Bool: Boolean;
    Bytes: AnsiString;
    Str: WideString;
    Custom: Pointer;      // is not used in RPNit but reserved for external use.
  end;

  TRpnVarCallback = function (Eval: TRpnEvaluator; Name: WideString): TRpnScalar of object;
  TRpnNotExistingVarCallback = function (Eval: TRpnEvaluator; const Name: WideString; out Value: TRpnScalar): Boolean of object;

  ERPN = class (Exception)
  end;

    EEmptyRpnVarCharList = class (ERPN)
      constructor Create;
    end;

    EDuplicateRpnOperator = class (ERPN)
      constructor Create(Op: WIdeChar; OpClass: TRpnOperatorClass);
    end;

    EEmptyRPN = class (ERPN)
      constructor Create;
    end;

    EWrongRpnSyntax = class (ERPN)
      constructor Create(Expr: WideString; Pos: Integer = 0);   // Pos - 1-based.
    end;

      EUnterminatedString = class (EWrongRpnSyntax)
        constructor Create(const Str: WideString; StartPos: Integer; const Expr: WideString);
      end;

    EInvalidRpnFloatStr = class (ERPN)
    public
      Num, Expr: WideString;
      constructor Create(Num: WideString; Expr: WideString = '');
    end;

    ERpnEvaluation = class (ERPN)
    end;

      EEmptyStackOperation = class (ERpnEvaluation)
        constructor Create(Operation: String);
      end;

      EWrongTopStackValueKind = class (ERpnEvaluation)
        constructor Create(Got, Expected: TRpnValueKinds);
      end;

      EIntegerExpectedOnStackTop = class (ERpnEvaluation)
        constructor Create(Got: Double);
      end;

      ENotEnoughRpnArguments = class (ERpnEvaluation)
        constructor Create(Operator: String; NeedArgs, HasArgs: Integer);
      end;

      EMissingVarFuncArgument = class (ERpnEvaluation)
        constructor Create(Func: WideString);
      end;

      EMissingRpnVariable = class (ERpnEvaluation)
        constructor Create(Name: WideString);
      end;

      EVariablelessRPN = class (ERpnEvaluation)
        constructor Create(Name: WideString);
      end;

      ENonEmptyRpnStack = class (ERpnEvaluation)
        constructor Create(StackItemsLeft: Integer);
      end;

      EEmptyRpnStack = class (ERpnEvaluation)
        constructor Create;
      end;

      // custom error class for external errors (e.g. wrong function operands).
      EInvalidRpnOperation = class (ERpnEvaluation)
        constructor Create(const Msg: String; Fmt: array of const);
      end;

  TRpnOperators = class
  protected
    FAnsi: array[0..High(Byte)] of TRpnOperatorClass;
    FDefinedAnsi: Byte;
    FHash: TObjectHash;

    function GetClass(Op: WideChar): TRpnOperatorClass;
    procedure PutClass(Op: WideChar; const Value: TRpnOperatorClass);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure AddFrom(Other: TRpnOperators);    // overwrites existing.
    procedure Copy(Other: TRpnOperators);       // clears self.

    procedure Add(Op: WideChar; OpClass: TRpnOperatorClass);
    procedure Delete(Op: WideChar);
    procedure DeleteAll(OpClass: TRpnOperatorClass);
    function Exists(Op: WideChar): Boolean;

    function Count: Integer;
    property ClassBy[Op: WideChar]: TRpnOperatorClass read GetClass write PutClass; default;
    function OpAt(Index: Integer): WideChar;
    function ClassAt(Index: Integer): TRpnOperatorClass;
  end;

  TRpnVariables = class (THash)
  protected
    FNotExistingVarCallback: TRpnNotExistingVarCallback;

    procedure Notify(Index: Integer; Action: TListNotification); override;
  public
    constructor Create; override;

    function Get(Eval: TRpnEvaluator; const Name: WideString): TRpnScalar; reintroduce;
    procedure Add(const Name: WideString; const Value: TRpnScalar); reintroduce;

    property NotExistingVarCallback: TRpnNotExistingVarCallback read FNotExistingVarCallback write FNotExistingVarCallback;

    function Exists(const Name: WideString): Boolean;
    function GetIfExists(Eval: TRpnEvaluator; const Name: WideString;
      out Value: TRpnScalar): Boolean; virtual;

    function GetByIndex(Index: Integer): TRpnScalar;
    function GetMatching(const WildcardName: WideString): TWideStringArray;

    procedure CopyAsKeyValuesFrom(Other: TStrings); overload;
    procedure CopyAsKeyValuesFrom(Other: TStringsW); overload;
  end;

  TRpnValueStack = class
  protected
    FValues: array[0..8191] of TRpnScalar;   // 136 KiB.
    FCount: Integer;
  public
    constructor Create;

    procedure Clear;
    function IsEmpty: Boolean;
    property Count: Integer read FCount;

    function Peek: TRpnScalar;
    procedure Push(Value: TRpnScalar);
    function PushIfSet(Value: TRpnScalar): Boolean;
    function Pop: TRpnScalar;

    function PopNum: Double;
    // unlike PopNum which returns any number PopInt checks if it's integral (with fractional part = 0).
    function PopInt: Double;
    function PopStr: WideString;

    function Reverse: Boolean;
    function GetResult: TRpnScalar; virtual;
    function Debug: WideString;
  end;

  TRpnCompSettings = record
    CacheCompiled: Boolean;   // doesn't work if Operators <> NIL.
    PrefixNotation: Boolean;
    VariableClass: TRpnVariableClass;

    Operators: TRpnOperators;
    Variables: TRpnVariables;
    VarCallback: TRpnVarCallback;
  end;

  TCompiledRPN = class (TObjectList)
  public
    class function CompileCaching(const Expr: WideString; const Settings: TRpnCompSettings): TCompiledRPN;
    class function Compile(const Expr: WideString; Settings: TRpnCompSettings): TCompiledRPN;
    class function Normalize(const Expr: WideString): WideString;
    class function Cached(const Expr: WideString; const Settings: TRpnCompSettings): TCompiledRPN;
    class procedure Cache(const Expr: WideString; const Settings: TRpnCompSettings; Compiled: TCompiledRPN);

    procedure Reverse;
    function Debug: WideString;
  end;

  { FVariables is not freed. }
  TRpnEvaluator = class
  protected
    FCompiled: TCompiledRPN;
    FStack: TRpnValueStack;

    FOperators: TRpnOperators;
    FVariables: TRpnVariables;
    FVarCallback: TRpnVarCallback;

    function GetResult: TRpnScalar; virtual;
  public
    constructor Create(Compiled: TCompiledRPN); virtual;
    destructor Destroy; override;

    property Operators: TRpnOperators read FOperators;
    property Variables: TRpnVariables read FVariables write FVariables;
    property VarCallback: TRpnVarCallback read FVarCallback write FVarCallback;

    function Evaluate: TRpnScalar;

    function GetVariable(Name: WideString): TRpnScalar;
    property Stack: TRpnValueStack read FStack;
  end;

  TRpnValue = class
  public
    function Value(Eval: TRpnEvaluator): TRpnScalar; virtual; abstract;
  end;

    TRpnConst = class (TRpnValue)
    protected
      FValue: TRpnScalar;
    public
      constructor Create(Value: TRpnScalar);
      function Value(Eval: TRpnEvaluator): TRpnScalar; override;
    end;

    TRpnVariable = class (TRpnValue)
    protected
      FName: WideString;
    public
      constructor Create(Name: WideString);

      property Name: WideString read FName;
      function Value(Eval: TRpnEvaluator): TRpnScalar; override;
    end;

      { This isn't used in this unit but can be used as a base class in apps using RPNit. }
      TRpnFuncVariable = class (TRpnVariable)
      protected
        function ExecFunc(Eval: TRpnEvaluator; const Name: WideString): TRpnScalar; virtual; abstract;
      public
        function Value(Eval: TRpnEvaluator): TRpnScalar; override;
      end;

  TRpnOperator = class
  public
    function Execute(Eval: TRpnEvaluator): TRpnScalar; virtual; abstract;
  end;

    TRpnMathOperator = class (TRpnOperator)
    protected
      function Eval(A, B: Double): Double; virtual; abstract;
    public
      function Execute(Eval: TRpnEvaluator): TRpnScalar; override;
    end;

      TRpnAdd = class (TRpnMathOperator)
      protected
        function Eval(A, B: Double): Double; override;
      end;

      TRpnSubtract = class (TRpnMathOperator)
      protected
        function Eval(A, B: Double): Double; override;
      end;

      TRpnMultiply = class (TRpnMathOperator)
      protected
        function Eval(A, B: Double): Double; override;
      end;

      TRpnDivide = class (TRpnMathOperator)
      protected
        function Eval(A, B: Double): Double; override;
      end;

      TRpnPower = class (TRpnMathOperator)
      protected
        function Eval(A, B: Double): Double; override;
      end;

      TRpnModulus = class (TRpnMathOperator)
      protected
        function Eval(A, B: Double): Double; override;
      end;

    TRpnCompOperator = class (TRpnOperator)
    protected
      function Holds(A, B: Double): Boolean; virtual; abstract;
    public
      function Execute(Eval: TRpnEvaluator): TRpnScalar; override;
    end;

      TRpnMore = class (TRpnCompOperator)
      protected
        function Holds(A, B: Double): Boolean; override;
      end;

      TRpnLess = class (TRpnCompOperator)
      protected
        function Holds(A, B: Double): Boolean; override;
      end;

// raises EInvalidRpnFloatStr.
function StrToFloatRPN(Str: WideString; Expr: WideString = ''): Double;
function RpnVarChars: WideString;
procedure SetRpnVarChars(const Chars: WideString);
procedure ClearRpnCache;

function RpnNum(Num: Double): TRpnScalar; overload;
// more efficient 32-bit parameter passing.
function RpnNum(Num: Integer): TRpnScalar; overload;
function RpnBool(Value: Boolean): TRpnScalar;
function RpnBytes(const Bytes: String): TRpnScalar;
function RpnStr(const Str: WideString): TRpnScalar;
function RpnKindToStr(Kind: TRpnValueKinds): String;
function RpnValueToStr(Value: TRpnScalar; Null: WideString = ''''''): WideString;
function RpnValueToInt(Value: TRpnScalar): Integer;

// raises EDuplicateRpnOperator:
procedure RegisterDefaultRpnOperator(Op: WideChar; OpClass: TRpnOperatorClass);
function RpnOpClassByName(const Name: String): TRpnOperatorClass;
// these 2 do nothing if Op is not defined:
procedure UnregisterDefaultRpnOperator(Op: WideChar; OpClass: TRpnOperatorClass);
procedure UnregisterDefaultRpnOperatorsOf(OpClass: TRpnOperatorClass);
// returns new value; sets to New if given.
function DefaultRpnVarClass(New: TRpnVariableClass = NIL): TRpnVariableClass;

function EvalRPN(Expr: WideString; Variables: TRpnVariables = NIL): TRpnScalar; overload;
function EvalRPN(Expr: WideString; VarCallback: TRpnVarCallback): TRpnScalar; overload;
function EvalRPN(Expr: WideString; const Settings: TRpnCompSettings): TRpnScalar; overload;

var
  DefaultRpnSettings: TRpnCompSettings;

implementation

type
  TVarValue = class
  public
    Value: TRpnScalar;
    constructor Create(AValue: TRpnScalar);
  end;

var
  FRpnVarChars: WideString = 'abcdefghijklmnopqrstuvwxyz0123456789_';
  CachedExprs: TObjectHash;   // of TCompiledRPN
  DefaultOperators: TRpnOperators;

{ Functions }

function StrToFloatRPN(Str: WideString; Expr: WideString = ''): Double;
var
  FS: TFormatSettings;
begin
  if PosW('.', Str) = 0 then
    FS.DecimalSeparator := ','
    else
      FS.DecimalSeparator := '.';

  if not TryStrToFloatStrict(Str, Result, FS) then
    raise EInvalidRpnFloatStr.Create(Str, Expr);
end;

function RpnVarChars: WideString;
begin
  Result := FRpnVarChars;
end;

procedure SetRpnVarChars(const Chars: WideString);
begin
  if TrimLeft(Chars) = '' then
    raise EEmptyRpnVarCharList.Create;

  FRpnVarChars := Chars;
  ClearRpnCache;
end;

procedure ClearRpnCache;
begin
  if CachedExprs <> NIL then
    CachedExprs.Clear;
end;

function RpnNum(Num: Double): TRpnScalar;
begin
  ZeroMemory(@Result, SizeOf(Result));
  Result.Kind := [valNum];
  Result.Num := Num;
end;

function RpnNum(Num: Integer): TRpnScalar;
begin
  ZeroMemory(@Result, SizeOf(Result));
  Result.Kind := [valNum];
  Result.Num := Num;
end;

function RpnBool(Value: Boolean): TRpnScalar;
begin
  ZeroMemory(@Result, SizeOf(Result));
  Result.Kind := [valBool];
  Result.Bool := Value;
end;

function RpnBytes(const Bytes: String): TRpnScalar;
begin
  ZeroMemory(@Result, SizeOf(Result));
  Result.Kind := [valBytes];
  Result.Bytes := Bytes;
end;

function RpnStr(const Str: WideString): TRpnScalar;
begin
  ZeroMemory(@Result, SizeOf(Result));
  Result.Kind := [valStr];
  Result.Str := Str;
end;

function RpnKindToStr(Kind: TRpnValueKinds): String;
begin
  Result := '';

  if valStr in Kind then
    Result := Result + 's';
  if valBytes in Kind then
    Result := Result + 'r';
  if valNum in Kind then
    Result := Result + 'i';
  if valBool in Kind then
    Result := Result + 'b';
  if valCustom in Kind then
    Result := Result + 'c';

  if Result = '' then
    Result := '-';
end;

function RpnValueToStr(Value: TRpnScalar; Null: WideString = ''''''): WideString;
begin
  with Value do
    if valStr in Kind then
      Result := Str
      else if valBytes in Kind then
        Result := BinToHex(Bytes[1], Length(Bytes), ' ')
        else if valNum in Kind then
          if Frac(Num) = 0 then
            Result := IntToStr(Trunc(Num))
            else
              Result := FloatToStr(Num)
          else if valBool in Kind then
            Result := BoolToStr(Bool, True)
            else
              Result := Null;
end;

function RpnValueToInt(Value: TRpnScalar): Integer;
  procedure Error;
  begin
    raise EConvertError.Create('Cannot convert RPN value into a number.');
  end;

begin
  with Value do
    if valStr in Kind then
      if TryStrToInt(Str, Result) then
        {OK}
        else
          Error
      else if valNum in Kind then
        if Frac(Num) = 0 then
          Result := Trunc(Num)
          else
            Error
        else
          Error;
end;

procedure RegisterDefaultRpnOperator(Op: WideChar; OpClass: TRpnOperatorClass);
begin
  if DefaultOperators = NIL then
    DefaultOperators := TRpnOperators.Create;

  DefaultOperators.Add(Op, OpClass);
end;

function RpnOpClassByName(const Name: String): TRpnOperatorClass;
const
  Error = 'RpnOpClassByName retrieved %s class which doesn''t descend from %s.';
begin
  if Copy(Name, 1, 1) <> 'T' then
    Result := TRpnOperatorClass( GetClass('TRpn' + Name) )
    else
      Result := TRpnOperatorClass( GetClass(Name) );

  if not Result.InheritsFrom(TRpnOperator) then
    raise EInvalidArgument.CreateFmt(Error, [Result, TRpnOperator]);
end;

procedure UnregisterDefaultRpnOperator(Op: WideChar; OpClass: TRpnOperatorClass);
begin
  DefaultOperators.Delete(Op);
end;

procedure UnregisterDefaultRpnOperatorsOf(OpClass: TRpnOperatorClass);
begin
  DefaultOperators.DeleteAll(OpClass);
end;

function DefaultRpnVarClass(New: TRpnVariableClass = NIL): TRpnVariableClass;
begin
  if New <> NIL then
    if not New.InheritsFrom(TRpnVariable) then
      raise EInvalidArgument.CreateFmt('RPN variable class must inherit from %s.', [TRpnVariable])
      else
        DefaultRpnSettings.VariableClass := New;

  Result := DefaultRpnSettings.VariableClass;
end;

function EvalRPN(Expr: WideString; Variables: TRpnVariables): TRpnScalar;
var
  Settings: TRpnCompSettings;
begin
  Settings := DefaultRpnSettings;
  Settings.Variables := Variables;
  Result := EvalRPN(Expr, Settings);
end;

function EvalRPN(Expr: WideString; VarCallback: TRpnVarCallback): TRpnScalar;
var
  Settings: TRpnCompSettings;
begin
  Settings := DefaultRpnSettings;
  Settings.VarCallback := VarCallback;
  Result := EvalRPN(Expr, Settings);
end;

function EvalRPN(Expr: WideString; const Settings: TRpnCompSettings): TRpnScalar; overload;
var
  Eval: TRpnEvaluator;
begin
  Eval := TRpnEvaluator.Create( TCompiledRPN.CompileCaching(Expr, Settings) );
  try
    Eval.Variables := Settings.Variables;
    Eval.VarCallback := Settings.VarCallback;
    Result := Eval.Evaluate;
  finally
    Eval.Free;
  end;
end;

function DebugDump(Obj: TObject): WideString;
begin
  if Obj is TRpnConst then
    Result := RpnValueToStr(TRpnConst(Obj).FValue)
  else if Obj is TRpnVariable then
    Result := TRpnVariable(Obj).Name
  else
    Result := Copy(Obj.ClassName, 2, MaxInt);
end;

{ Exceptions }

constructor EEmptyRpnVarCharList.Create;
begin
  inherited Create('Attempted to set empty RpnVarChars.');
end;

constructor EDuplicateRpnOperator.Create(Op: WIdeChar; OpClass: TRpnOperatorClass);
begin
  CreateFmt('Attempted to add a new operator "%s" of class %s to the RPN operator list' +
            ' but it was already taken.', [Op, OpClass]);
end;

constructor EEmptyRPN.Create;
begin
  inherited Create('Empty RPN expression string.');
end;

constructor EWrongRpnSyntax.Create(Expr: WideString; Pos: Integer);
const
  PreviewLength = 20;
begin
  if Pos > 0 then
    CreateFmt('Invalid RPN expression syntax at "%s" (position %d, full string: "%s").',
                        [Copy(Expr, Pos, PreviewLength), Pos, Expr])
    else
      CreateFmt('Invalid RPN expression string "%s".', [Expr]);
end;

constructor EUnterminatedString.Create(const Str: WideString; StartPos: Integer; const Expr: WideString);
begin
  CreateFmt('Unterminated string "%s" started at %d in RPN expression "%s".', [Str, StartPos, Expr]);
end;

constructor EInvalidRpnFloatStr.Create(Num: WideString; Expr: WideString = '');
begin
  Self.Num := Num;
  Self.Expr := Expr;

  if Expr = '' then
    CreateFmt('Invalid floating point constant "%s" in an RPN expression.', [Num])
    else
      CreateFmt('Invalid floating point constant "%s" in RPN expression "%s".', [Num, Expr]);
end;

constructor EEmptyStackOperation.Create(Operation: String);
begin
  CreateFmt('Attempted to %s from empty TRpnValueStack.', [Operation]);
end;

constructor EWrongTopStackValueKind.Create(Got, Expected: TRpnValueKinds);
begin
  CreateFmt('Expected a %s value on top of TRpnValueStack but got %s.', [RpnKindToStr(Expected), RpnKindToStr(Got)]);
end;

constructor EIntegerExpectedOnStackTop.Create(Got: Double);
begin
  CreateFmt('Expected an integer value without fractional part on top of TRpnValueStack but got %s.', [Got]);
end;

constructor ENotEnoughRpnArguments.Create(Operator: String; NeedArgs, HasArgs: Integer);
begin
  if Copy(Operator, 0, 4) = 'TRpn' then
    Operator := Copy(Operator, 5, Length(Operator));

  CreateFmt('Too few arguments on stack for %s (needs %d, has %d).', [Operator, NeedArgs, HasArgs]);
end;

constructor EMissingVarFuncArgument.Create(Func: WideString);
begin
  CreateFmt('Not enough arguments on stack for function "%s".', [Func]);
end;

constructor EMissingRpnVariable.Create(Name: WideString);
begin
  CreateFmt('Missing RPN expression variable "%s".', [Name]);
end;

constructor EVariablelessRPN.Create(Name: WideString);
begin
  CreateFmt('Variable "%s" was found in an RPN expression but no variables were' +
            ' bound to the expression prior to its evaluation.', [Name]);
end;

constructor ENonEmptyRpnStack.Create(StackItemsLeft: Integer);
begin
  CreateFmt('There were %d items instead of just one left on stack after evaluating an RPN expression.', [StackItemsLeft]);
end;

constructor EEmptyRpnStack.Create;
begin
  inherited Create('There were no items left on stack after evaluating an RPN expression' +
                   ' - but at least one is required to determine the expression result.');
end;

constructor EInvalidRpnOperation.Create(const Msg: String; Fmt: array of const);
begin
  CreateFmt(Msg, Fmt);
end;

{ TVarValue }

constructor TVarValue.Create(AValue: TRpnScalar);
begin
  Value := AValue;
end;

{ TRpnOperators }

constructor TRpnOperators.Create;
begin
  FHash := TObjectHash.Create(False);
  FHash.CaseSensitive := False;
  FHash.Duplicates := dupError;
  FHash.Sorted := True;

  Clear;
end;

destructor TRpnOperators.Destroy;
begin
  FHash.Free;
  inherited;
end;

procedure TRpnOperators.Clear;
begin
  ZeroMemory(@FAnsi[0], SizeOf(FAnsi));
  FDefinedAnsi := 0;

  FHash.Clear;
end;

procedure TRpnOperators.AddFrom(Other: TRpnOperators);
var
  I, Index: Integer;
begin
  for I := 0 to Length(FAnsi) - 1 do
    if Assigned( Other.FAnsi[I] ) then
    begin
      if not Assigned(FAnsi[I]) then
        Inc(FDefinedAnsi);
      FAnsi[I] := Other.FAnsi[I];
    end;

  for I := 0 to Other.FHash.Count - 1 do
  begin
    Index := FHash.IndexOf(Other.FHash.Strings[I]);
    if Index = -1 then
      FHash.AddObject(Other.FHash.Strings[I], Other.FHash.Objects[I])
      else
        FHash.Objects[Index] := Other.FHash.Objects[I];
  end;
end;

procedure TRpnOperators.Copy(Other: TRpnOperators);
begin
  Move(Other.FAnsi[0], FAnsi[0], SizeOf(FAnsi));
  FDefinedAnsi := Other.FDefinedAnsi;

  FHash.Assign(Other.FHash);
end;

procedure TRpnOperators.Add(Op: WIdeChar; OpClass: TRpnOperatorClass);
begin
  if Exists(Op) then
    raise EDuplicateRpnOperator.Create(Op, OpClass)
    else
      ClassBy[Op] := OpClass;
end;

procedure TRpnOperators.Delete(Op: WIdeChar);
begin
  if Word(Op) < High(Byte) then
  begin
    FAnsi[Word(Op)] := NIL;
    Dec(FDefinedAnsi);
  end
    else
      FHash.Delete(Op);
end;

procedure TRpnOperators.DeleteAll(OpClass: TRpnOperatorClass);
var
  I: Integer;
begin
  for I := 0 to Length(FAnsi) - 1 do
    if FAnsi[I] = OpClass then
    begin
      FAnsi[I] := NIL;
      Dec(FDefinedAnsi);
    end;

  while True do
  begin
    I := FHash.IndexOfObject(TObject(OpClass));
    if I = -1 then
      Break;
    FHash.Delete(I);
  end;
end;

function TRpnOperators.Exists(Op: WideChar): Boolean;
begin
  if Word(Op) < High(Byte) then
    Result := Assigned(FAnsi[Word(Op)])
    else
      Result := FHash.IndexOf(Op) <> -1;
end;

function TRpnOperators.Count: Integer;
begin
  Result := FDefinedAnsi + FHash.Count;
end;

function TRpnOperators.GetClass(Op: WideChar): TRpnOperatorClass;
begin
  if Word(Op) < High(Byte) then
    Result := FAnsi[Word(Op)]
    else
      Result := TRpnOperatorClass(FHash[Op]);
end;

procedure TRpnOperators.PutClass(Op: WideChar; const Value: TRpnOperatorClass);
begin
  if Word(Op) < High(Byte) then
  begin
    if not Assigned( FAnsi[Word(Op)] ) then
      Inc(FDefinedAnsi);
    FAnsi[Word(Op)] := Value;
  end
    else
      FHash[Op] := TObject(Value);
end;

function TRpnOperators.OpAt(Index: Integer): WideChar;
var
  I: Byte;
begin
  Result := '_';

  if Index > FDefinedAnsi then
    Result := FHash.Names[Index][1]
    else
      for I := 0 to Length(FAnsi) - 1 do
        if Assigned(FAnsi[I]) then
          if Index > 0 then
            Dec(Index)
            else
            begin
              Result := WideChar(I);
              Exit;
            end;
end;

function TRpnOperators.ClassAt(Index: Integer): TRpnOperatorClass;
var
  I: Byte;
begin
  Result := NIL;

  if Index > FDefinedAnsi then
    Result := TRpnOperatorClass(FHash.Objects[Index])
    else
      for I := 0 to Length(FAnsi) - 1 do
        if Assigned(FAnsi[I]) then
          if Index > 0 then
            Dec(Index)
            else
            begin
              Result := FAnsi[I];
              Exit;
            end;
end;

{ TRpnVariables }

constructor TRpnVariables.Create;
begin
  inherited;

  Sorted := True;
  Duplicates := dupError;   // variables should be set using Add method.
end;

function TRpnVariables.Get(Eval: TRpnEvaluator; const Name: WideString): TRpnScalar;
begin
  if not GetIfExists(Eval, Name, Result) then
    Result.Kind := [];
end;

procedure TRpnVariables.Add(const Name: WideString; const Value: TRpnScalar);
var
  I: Integer;
begin
  I := IndexOf(Name);
  if I = -1 then
    AddObject(Name, TVarValue.Create(Value))
    else
      TVarValue(Objects[I]).Value := Value;
end;

function TRpnVariables.Exists(const Name: WideString): Boolean;
begin
  Result := IndexOf(Name) <> -1;
end;

function TRpnVariables.GetIfExists(Eval: TRpnEvaluator; const Name: WideString;
  out Value: TRpnScalar): Boolean;
var
  I: Integer;
begin
  I := IndexOf(Name);
  Result := I <> -1;

  if Result then
    Value := TVarValue(Objects[I]).Value
    else if Assigned(FNotExistingVarCallback) then
      Result := FNotExistingVarCallback(Eval, Name, Value);
end;

procedure TRpnVariables.CopyAsKeyValuesFrom(Other: TStrings);
var
  I: Integer;
  Name, Value: WideString;
begin
  for I := 0 to Other.Count - 1 do
    if Split(Other[I], '=', Name, Value) then
      Add(Trim(Name), RpnNum( StrToFloatRPN(Trim(Value)) ));
end;

procedure TRpnVariables.CopyAsKeyValuesFrom(Other: TStringsW);
var
  I: Integer;
  Name, Value: WideString;
begin
  for I := 0 to Other.Count - 1 do
    if Split(Other[I], '=', Name, Value) then
      Add(Trim(Name), RpnNum( StrToFloatRPN(Trim(Value)) ));
end;

procedure TRpnVariables.Notify(Index: Integer; Action: TListNotification);
begin
  if Action = lnDeleted then
    Objects[Index].Free;

  inherited;
end;

function TRpnVariables.GetByIndex(Index: Integer): TRpnScalar;
begin
  Result := TVarValue(Objects[Index]).Value;
end;

function TRpnVariables.GetMatching(const WildcardName: WideString): TWideStringArray;
  function MinPos(PosA, PosB: Integer): Integer;
  begin
    if PosA = 0 then
      Result := PosB
      else if PosB = 0 then
        Result := PosA
        else if PosA > PosB then
          Result := PosB
          else
            Result := PosA;
  end;

var
  Cur, Pos: Integer;
  Prefix: WideString;
begin
  SetLength(Result, 0);

  Pos := MinPos(PosW('?', WildcardName), PosW('*', WildcardName));
  Pos := MinPos(Pos, PosW('+', WildcardName));

  if Pos = 0 then
  begin
    if Exists(WildcardName) then
    begin
      SetLength(Result, 1);
      Result[0] := WildcardName;
    end;
  end
    else
    begin
      Cur := 0;
      SetLength(Result, $40000);

      Prefix := Copy(WildcardName, 1, Pos - 1);

      Pos := 0;
      if Prefix <> '' then
        while (Pos < Count) and (Copy(Strings[Pos], 1, Length(Prefix)) <> Prefix) do
          Inc(Pos);

      for Pos := Pos to Count - 1 do
        if (Length(Prefix) > 0) and (Copy(Strings[Pos], 1, Length(Prefix)) <> Prefix) then
          Break   // the list is sorted, see Create.
          else if MaskMatch(Strings[Pos], WildcardName) then
            if Cur >= Length(Result) then
              raise EListError.CreateFmt('Too many RPN variables matching "%s" - more than %d.', [WildcardName, Length(Result)])
              else
              begin
                Result[Cur] := Strings[Pos];
                Inc(Cur);
              end;

      SetLength(Result, Cur);
    end;
end;

{ TRpnValueStack }

constructor TRpnValueStack.Create;
begin
  Clear;
end;

procedure TRpnValueStack.Clear;
begin
  FCount := 0;
end;

function TRpnValueStack.IsEmpty: Boolean;
begin
  Result := FCount < 1;
end;

function TRpnValueStack.Peek: TRpnScalar;
begin
  if IsEmpty then
    raise EEmptyStackOperation.Create('Peek')
    else
      Result := FValues[FCount - 1];
end;

procedure TRpnValueStack.Push(Value: TRpnScalar);
begin
  FValues[FCount] := Value;
  Inc(FCount);
end;

function TRpnValueStack.PushIfSet(Value: TRpnScalar): Boolean;
begin
  Result := Value.Kind <> [];
  if Result then
    Push(Value);
end;

function TRpnValueStack.Pop: TRpnScalar;
begin
  if IsEmpty then
    raise EEmptyStackOperation.Create('Pop')
    else
    begin
      Dec(FCount);
      Result := FValues[FCount];
    end;
end;

function TRpnValueStack.PopNum: Double;
var
  Op: TRpnScalar;
begin
  if IsEmpty then
    raise EEmptyStackOperation.Create('PopNum')
    else
    begin
      Op := Pop;
      if not (valNum in Op.Kind) then
        raise EWrongTopStackValueKind.Create(Op.Kind, [valNum]);

      Result := Op.Num;
    end;
end;

function TRpnValueStack.PopInt: Double;
begin
  Result := PopNum;
  if Frac(Result) <> 0 then
    raise EIntegerExpectedOnStackTop.Create(Result);
end;

function TRpnValueStack.PopStr: WideString;
var
  Op: TRpnScalar;
begin
  if IsEmpty then
    raise EEmptyStackOperation.Create('PopStr')
    else
    begin
      Op := Pop;
      if not (valStr in Op.Kind) then
        raise EWrongTopStackValueKind.Create(Op.Kind, [valStr]);

      Result := Op.Str;
    end;
end;

function TRpnValueStack.Reverse: Boolean;
var
  I: Integer;
  Temp: TRpnScalar;
begin
  Result := Count > 0;

  for I := 0 to Count div 2 - 1 do
  begin
    Temp := FValues[I];
    FValues[I] := FValues[Count - I - 1];
    FValues[Count - I - 1] := Temp;
  end;
end;

function TRpnValueStack.GetResult: TRpnScalar;
begin
  if Count = 1 then
    Result := Pop
    else if Count > 0 then
      raise ENonEmptyRpnStack.Create(Count)
      else if Count <= 0 then
        raise EEmptyRpnStack.Create;
end;

function TRpnValueStack.Debug: WideString;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    Result := Result + ' ' + RpnKindToStr(FValues[I].Kind) + RpnValueToStr(FValues[I], '') + '.';
  System.Delete(Result, 1, 1);
end;

{ TCompiledRPN }

class function TCompiledRPN.CompileCaching(const Expr: WideString; const Settings: TRpnCompSettings): TCompiledRPN;
begin
  Result := Cached(Expr, Settings);

  if Result = NIL then
  begin
    Result := Compile(Expr, Settings);
    Cache(Expr, Settings, Result);
  end;
end;

class function TCompiledRPN.Compile(const Expr: WideString; Settings: TRpnCompSettings): TCompiledRPN;
var
  Pos: Integer;
  VarChars: WideString;

  function NextChar(Chars: WideString): Boolean;
  begin
    Result := (Pos < Length(Expr)) and (PosW(Expr[Pos + 1], Chars) > 0);
  end;

  procedure Operator(AClass: TRpnOperatorClass; OpLength: Integer = 1);
  begin
    Result.Add(AClass.Create);
    Inc(Pos, OpLength);
  end;

  function CharMatches(Ch: WideChar; const Chars: WideString): Boolean;
  begin
    Result := (PosW(Ch, Chars) <> 0) or (PosW(LowerCase(Ch), Chars) <> 0);
  end;

  function CutName(const Chars: WideString): WideString;
  begin
    Result := '';

    while (Pos <= Length(Expr)) and CharMatches(Expr[Pos], Chars) do
    begin
      Result := Result + Expr[Pos];
      Inc(Pos);
    end;
  end;

    procedure StringOperand(const Str: WideString);
    begin
      Result.Add(TRpnConst.Create( RpnStr(Str) ));
    end;

    procedure NumOperand;
    var
      Sign: ShortInt;
    begin
      Sign := +1;

      case Char(Expr[Pos]) of
      '-':  begin
              Sign := -1;
              Inc(Pos);
            end;
      '+':  Inc(Pos);
      end;

      Result.Add(TRpnConst.Create(RpnNum( Sign * StrToFloatRPN(CutName('0123456789,.')) )));
    end;

    procedure Variable;
    begin
      Result.Add(Settings.VariableClass.Create( CutName(VarChars) ));
    end;

var
  Handled, IsVar, IsString: Boolean;
  OpClass: TRpnOperatorClass;
  CurStr: WideString;
begin
  if Normalize(Expr) = '' then
    raise EEmptyRPN.Create;

  if Settings.Operators = NIL then
    Settings.Operators := DefaultOperators;

  VarChars := LowerCase(FRpnVarChars);

  Result := Self.Create;
  try
    Pos := 1;
    IsString := False;
    CurStr := '';

    while Pos <= Length(Expr) do
    begin
      Handled := True;

      if IsString then
      begin
        if (Expr[Pos] = '''') and
           ( (Pos >= Length(Expr)) or (Expr[Pos + 1] <> '''') ) then
        begin
          StringOperand(CurStr);

          IsString := False;
          CurStr := '';
        end
        else
        begin
          CurStr := CurStr + Expr[Pos];
          if Expr[Pos] = '''' then
            Inc(Pos);   // double ' = single '
        end;

        Inc(Pos);
      end
        else
          case Char(Expr[Pos]) of
          ' ':        Inc(Pos);
          '''':       begin
                        IsString := True;
                        Inc(Pos);
                      end;
          '0'..'9':   NumOperand;
          '-', '+':   if NextChar('0123456789') then
                        NumOperand
                        else
                          Handled := False;
          else
            Handled := False;
          end;

      if not Handled then
      begin
        OpClass := Settings.Operators[Expr[Pos]];
        IsVar := CharMatches(Expr[Pos], VarChars);

        if Assigned(OpClass) and IsVar and
           (Pos < Length(Expr)) and not NextChar(' 0123456789') then
          Variable
          else if Assigned(OpClass) then
            Operator(OpClass)
            else if IsVar then
              Variable
              else
                raise EWrongRpnSyntax.Create(Expr, Pos);
      end;
    end;

    if IsString then
      raise EUnterminatedString.Create(CurStr, Length(Expr) - Length(CurStr), Expr);

    if Settings.PrefixNotation then
      Result.Reverse;
  except
    Result.Free;
    raise;
  end;
end;

class function TCompiledRPN.Normalize(const Expr: WideString): WideString;
begin
  Result := Trim(Expr);
end;

class function TCompiledRPN.Cached(const Expr: WideString; const Settings: TRpnCompSettings): TCompiledRPN;
begin
  with Settings do
    if not CacheCompiled or ( (CachedExprs = NIL) or ((Operators <> NIL) and (Operators <> DefaultOperators)) ) then
      Result := NIL
      else
        Result := CachedExprs[Normalize(Expr)] as TCompiledRPN;
end;

class procedure TCompiledRPN.Cache(const Expr: WideString; const Settings: TRpnCompSettings; Compiled: TCompiledRPN);
begin
  with Settings do
    if CacheCompiled and ( (Operators = NIL) or (Operators = DefaultOperators) ) then
    begin
      if CachedExprs = NIL then
      begin
        CachedExprs := TObjectHash.Create(True);
        CachedExprs.CaseSensitive := True;
      end;

      CachedExprs.AddObject(Normalize(Expr), Compiled);
    end;
end;

procedure TCompiledRPN.Reverse;
var
  I: Integer;
begin
  for I := 0 to Count div 2 - 1 do
    Exchange(I, Count - I - 1);
end;

function TCompiledRPN.Debug: WideString;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Result := Result + ' ' + DebugDump(Items[I]);
  System.Delete(Result, 1, 1);
end;

{ TRpnConst }

constructor TRpnConst.Create(Value: TRpnScalar);
begin
  FValue := Value;
end;

function TRpnConst.Value(Eval: TRpnEvaluator): TRpnScalar;
begin
  Result := FValue;
end;

{ TRpnVariable }

constructor TRpnVariable.Create(Name: WideString);
begin
  FName := Name;
end;

function TRpnVariable.Value(Eval: TRpnEvaluator): TRpnScalar;
begin
  Result := Eval.GetVariable(FName);
end;

{ TRpnFuncVariable }

function TRpnFuncVariable.Value(Eval: TRpnEvaluator): TRpnScalar;
begin
  try
    Result := ExecFunc(Eval, FName);
    if Result.Kind = [] then
      Result := inherited Value(Eval);
  except
    on E: EEmptyStackOperation do
      raise EMissingVarFuncArgument.Create(FName);
  end;
end;

{ TRpnEvaluator }

constructor TRpnEvaluator.Create(Compiled: TCompiledRPN);
begin
  FVariables := NIL;

  FOperators := TRpnOperators.Create;
  FOperators.Copy(DefaultOperators);

  FStack := TRpnValueStack.Create;

  FCompiled := Compiled;
end;

destructor TRpnEvaluator.Destroy;
begin
  FStack.Free;
  FOperators.Free;
  inherited;
end;

function TRpnEvaluator.Evaluate: TRpnScalar;
var
  I: Integer;
  Item: TObject;
begin
  Result := RpnNum(0);    // compiler warning, not used.

  for I := 0 to FCompiled.Count - 1 do
  begin
    Item := FCompiled[I];

    if Item.InheritsFrom(TRpnValue) then
      FStack.Push( TRpnValue(Item).Value(Self) )
      else if Item.InheritsFrom(TRpnOperator) then
        FStack.Push( TRpnOperator(Item).Execute(Self) )
        else
          raise ERPN.CreateFmt('Invalid object of class %s in a compiled RPN object.', [Item.ClassName]);
  end;

  Result := GetResult;
end;

function TRpnEvaluator.GetResult: TRpnScalar;
begin
  Result := FStack.GetResult;
end;

function TRpnEvaluator.GetVariable(Name: WideString): TRpnScalar;
begin
  if Variables <> NIL then
  begin
    if not Variables.GetIfExists(Self, Name, Result) then
      raise EMissingRpnVariable.Create(Name);
  end
    else if Assigned(VarCallback) then
      Result := VarCallback(Self, Name)
      else
        raise EVariablelessRPN.Create(Name);
end;

{ TRpnMathOperator }

function TRpnMathOperator.Execute(Eval: TRpnEvaluator): TRpnScalar;
var
  A: Double;
begin
  if Eval.Stack.Count < 2 then
    raise ENotEnoughRpnArguments.Create(ClassName, 2, Eval.Stack.Count);

  A := Eval.Stack.PopNum;
  Result := RpnNum( Self.Eval(Eval.Stack.PopNum, A) );
end;

{ TRpnAdd }

function TRpnAdd.Eval(A, B: Double): Double;
begin
  Result := A + B;
end;

{ TRpnSubtract }

function TRpnSubtract.Eval(A, B: Double): Double;
begin
  Result := A - B;
end;

{ TRpnMultiply }

function TRpnMultiply.Eval(A, B: Double): Double;
begin
  Result := A * B;
end;

{ TRpnDivide }

function TRpnDivide.Eval(A, B: Double): Double;
begin
  Result := A / B;
end;

{ TRpnPower }

function TRpnPower.Eval(A, B: Double): Double;
begin
  Result := Power(A, B);
end;

{ TRpnModulus }

function TRpnModulus.Eval(A, B: Double): Double;
begin
  Result := A - Int(A / B) * B;
end;

{ TRpnCompOperator }

function TRpnCompOperator.Execute(Eval: TRpnEvaluator): TRpnScalar;
var
  A: Double;
begin
  if Eval.Stack.Count < 2 then
    raise ENotEnoughRpnArguments.Create(ClassName, 2, Eval.Stack.Count);

  A := Eval.Stack.PopNum;
  Result := RpnBool( Self.Holds(Eval.Stack.PopNum, A) );
end;

{ TRpnMore }

function TRpnMore.Holds(A, B: Double): Boolean;
begin
  Result := A > B;
end;

{ TRpnLess }

function TRpnLess.Holds(A, B: Double): Boolean;
begin
  Result := A < B;
end;

initialization
  ZeroMemory(@DefaultRpnSettings, SizeOf(DefaultRpnSettings));
  DefaultRpnSettings.CacheCompiled := True;
  DefaultRpnSettings.VariableClass := TRpnVariable;

  RegisterDefaultRpnOperator('+', TRpnAdd);
  RegisterDefaultRpnOperator('-', TRpnSubtract);
  RegisterDefaultRpnOperator('*', TRpnMultiply);
  RegisterDefaultRpnOperator('/', TRpnDivide);
  RegisterDefaultRpnOperator('^', TRpnPower);
  RegisterDefaultRpnOperator('%', TRpnModulus);

  { Equality operators (= <> !=) are not part of standard pack because of ambiguous notion of
    'equality' (are valBytes and valNum equal? if so, when?). For ops below this is less problematic. }
  RegisterDefaultRpnOperator('>', TRpnMore);
  RegisterDefaultRpnOperator('<', TRpnLess);

finalization
  if CachedExprs <> NIL then
    CachedExprs.Free;
  if DefaultOperators <> NIL then
    DefaultOperators.Free;
end.
