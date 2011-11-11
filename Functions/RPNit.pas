unit RPNit;

{
  This unit is a part of the D7X Library for Delphi 7.
  RPNit by Proger_XP | http://proger.i-forge.net/RPNit
}

interface

uses Windows, SysUtils, Math, Classes, Contnrs, StringsW, StringUtils;

type
  TRpnVarCallback = function (Name: WideString): Single of object;
  TRpnOperatorClass = class of TRpnOperator;

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

    EInvalidRpnFloatStr = class (ERPN)
    public
      Num, Expr: WideString;
      constructor Create(Num: WideString; Expr: WideString = '');
    end;

    ERpnEvaluation = class (ERPN)
    end;

      ENotEnoughRpnArguments = class (ERpnEvaluation)
        constructor Create(Operator: String; NeedArgs, HasArgs: Integer);
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
    function GetVar(const Name: WideString): Single;
    procedure PutVar(const Name: WideString; const Value: Single);
  public
    // returns 0 if variable didn't exist.
    property Variables[const Name: WideString]: Single read GetVar write PutVar; default;
    function Exists(const Name: WideString): Boolean;
    function GetIfExists(const Name: WideString; out Value: Single): Boolean;

    procedure CopyAsKeyValuesFrom(Other: TStrings); overload;
    procedure CopyAsKeyValuesFrom(Other: TStringsW); overload;
  end;

  TFloatStack = class (TStack)
  public
    function Peek: Single; reintroduce;
    procedure Push(Value: Single); reintroduce;
    function Pop: Single; reintroduce;
  end;

  TCompiledRPN = class (TObjectList)
  public
    constructor Create;
  end;

  { FVariables is not freed. }
  TRpnEvaluator = class
  protected
    FCompiled: TCompiledRPN;
    FStack: TFloatStack;

    FOperators: TRpnOperators;
    FVariables: TRpnVariables;
    FVarCallback: TRpnVarCallback;
  public
    constructor Create(Compiled: TCompiledRPN);
    destructor Destroy; override;

    property Operators: TRpnOperators read FOperators;
    property Variables: TRpnVariables read FVariables write FVariables;
    property VarCallback: TRpnVarCallback read FVarCallback write FVarCallback;

    function Evaluate: Single;

    function GetVariable(Name: WideString): Single;
    property Stack: TFloatStack read FStack;
  end;

  TRpnOperand = class
  public
    function Value(Eval: TRpnEvaluator): Single; virtual; abstract;
  end;

    TRpnConst = class (TRpnOperand)
    protected
      FValue: Single;
    public
      constructor Create(Value: Single);
      function Value(Eval: TRpnEvaluator): Single; override;
    end;

    TRpnVariable = class (TRpnOperand)
    protected
      FName: WideString;
    public
      constructor Create(Name: WideString);

      property Name: WideString read FName;
      function Value(Eval: TRpnEvaluator): Single; override;
    end;

  TRpnOperator = class
  public
    function Execute(Eval: TRpnEvaluator): Single; virtual; abstract;
  end;

    TRpnMathOperator = class (TRpnOperator)
    protected
      function Eval(A, B: Single): Single; virtual; abstract;
    public
      function Execute(Eval: TRpnEvaluator): Single; override;
    end;

// raises EInvalidRpnFloatStr.
function StrToFloatRPN(Str: WideString; Expr: WideString = ''): Single;
function RpnVarChars: WideString;
procedure SetRpnVarChars(const Chars: WideString);
procedure ClearRpnCache;

// raises EDuplicateRpnOperator:
procedure RegisterDefaultRpnOperator(Op: WideChar; OpClass: TRpnOperatorClass);
// these 2 do nothing if Op is not defined:
procedure UnregisterDefaultRpnOperator(Op: WideChar; OpClass: TRpnOperatorClass);
procedure UnregisterDefaultRpnOperatorsOf(OpClass: TRpnOperatorClass);

function EvalRPN(Expr: WideString; Variables: TRpnVariables; Operators: TRpnOperators = NIL): Single; overload;
function EvalRPN(Expr: WideString; VarCallback: TRpnVarCallback; Operators: TRpnOperators = NIL): Single; overload;

implementation

type
  TRpnAdd = class (TRpnMathOperator)
  protected
    function Eval(A, B: Single): Single; override;
  end;

  TRpnSubtract = class (TRpnMathOperator)
  protected
    function Eval(A, B: Single): Single; override;
  end;

  TRpnMultiply = class (TRpnMathOperator)
  protected
    function Eval(A, B: Single): Single; override;
  end;

  TRpnDivide = class (TRpnMathOperator)
  protected
    function Eval(A, B: Single): Single; override;
  end;

  TRpnPower = class (TRpnMathOperator)
  protected
    function Eval(A, B: Single): Single; override;
  end;

  TRpnModulus = class (TRpnMathOperator)
  protected
    function Eval(A, B: Single): Single; override;
  end;

var
  FRpnVarChars: WideString = 'abcdefghijklmnopqrstuvwxyz0123456789_';
  CachedExprs: TObjectHash;   // of TCompiledRPN
  DefaultOperators: TRpnOperators;

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

constructor EInvalidRpnFloatStr.Create(Num: WideString; Expr: WideString = '');
begin
  Self.Num := Num;
  Self.Expr := Expr;

  if Expr = '' then
    CreateFmt('Invalid floating point constant "%s" in an RPN expression.', [Num])
    else
      CreateFmt('Invalid floating point constant "%s" in RPN expression "%s".', [Num, Expr]);
end;

constructor ENotEnoughRpnArguments.Create(Operator: String; NeedArgs, HasArgs: Integer);
begin
  if Copy(Operator, 0, 4) = 'TRpn' then
    Operator := Copy(Operator, 5, Length(Operator));

  CreateFmt('Too few arguments on stack for %s (needs %d, has %d).', [Operator, NeedArgs, HasArgs]);
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

function TRpnVariables.GetVar(const Name: WideString): Single;
begin
  if not GetIfExists(Name, Result) then
    Result := 0;
end;

procedure TRpnVariables.PutVar(const Name: WideString; const Value: Single);
var
  I: Integer;
begin
  I := IndexOf(Name);
  if I = -1 then
    AddObject(Name, TObject(Value))
    else
      Objects[I] := TObject(Value);
end;

function TRpnVariables.Exists(const Name: WideString): Boolean;
begin
  Result := IndexOfName(Name) <> -1;
end;

function TRpnVariables.GetIfExists(const Name: WideString; out Value: Single): Boolean;
var
  I: Integer;
begin
  I := IndexOf(Name);
  Result := I <> -1;
  if Result then
    Value := Single(Objects[I]);
end;

procedure TRpnVariables.CopyAsKeyValuesFrom(Other: TStrings);
var
  I: Integer;
  Name, Value: WideString;
begin
  for I := 0 to Other.Count - 1 do
    if Split(Other[I], '=', Name, Value) then
      Variables[Trim(Name)] := StrToFloatRPN(Trim(Value));
end;

procedure TRpnVariables.CopyAsKeyValuesFrom(Other: TStringsW);
var
  I: Integer;
  Name, Value: WideString;
begin
  for I := 0 to Other.Count - 1 do
    if Split(Other[I], '=', Name, Value) then
      Variables[Trim(Name)] := StrToFloatRPN(Trim(Value));
end;

{ TFloatStack }

function TFloatStack.Peek: Single;
begin
  Result := Single(inherited Peek);
end;

function TFloatStack.Pop: Single;
begin
  Result := Single(inherited Pop);
end;

procedure TFloatStack.Push(Value: Single);
begin
  inherited Push(Pointer(Value));
end;

{ TCompiledRPN }

constructor TCompiledRPN.Create;
begin
  inherited Create(True);
end;

{ TRpnConst }

constructor TRpnConst.Create(Value: Single);
begin
  FValue := Value;
end;

function TRpnConst.Value(Eval: TRpnEvaluator): Single;
begin
  Result := FValue;
end;

{ TRpnVariable }

constructor TRpnVariable.Create(Name: WideString);
begin
  FName := Name;
end;

function TRpnVariable.Value(Eval: TRpnEvaluator): Single;
begin
  Result := Eval.GetVariable(FName);
end;

{ Functions }

function StrToFloatRPN(Str: WideString; Expr: WideString = ''): Single;
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

procedure RegisterDefaultRpnOperator(Op: WideChar; OpClass: TRpnOperatorClass);
begin
  if DefaultOperators = NIL then
    DefaultOperators := TRpnOperators.Create;

  DefaultOperators.Add(Op, OpClass);
end;

procedure UnregisterDefaultRpnOperator(Op: WideChar; OpClass: TRpnOperatorClass);
begin
  DefaultOperators.Delete(Op);
end;

procedure UnregisterDefaultRpnOperatorsOf(OpClass: TRpnOperatorClass);
begin
  DefaultOperators.DeleteAll(OpClass);
end;

function NormalizeRPN(Expr: WideString): WideString;
begin
  Result := LowerCase(Trim(Expr));
end;

function GetCache(Expr: WideString; Operators: TRpnOperators): TCompiledRPN;
begin
  if (CachedExprs = NIL) or ((Operators <> NIL) and (Operators <> DefaultOperators)) then
    Result := NIL
    else
      Result := TCompiledRPN( CachedExprs[NormalizeRPN(Expr)] );
end;

procedure SetCache(Expr: WideString; Operators: TRpnOperators; COmpiled: TCompiledRPN);
begin
  if (Operators = NIL) or (Operators = DefaultOperators) then
  begin
    if CachedExprs = NIL then
      with CachedExprs do
      begin
        CachedExprs := TObjectHash.Create(True);
        CaseSensitive := False;
      end;

    CachedExprs.AddObject(NormalizeRPN(Expr), Compiled);
  end;
end;

function CompileRPN(Expr: WideString; Operators: TRpnOperators = NIL): TCompiledRPN;
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

    procedure ConstOperand;
    begin
      Result.Add(TRpnConst.Create( StrToFloatRPN(CutName('0123456789,.')) ));
    end;

    procedure Variable;
    begin
      Result.Add(TRpnVariable.Create( CutName(VarChars) ));
    end;

var
  Handled, IsVar: Boolean;
  OpClass: TRpnOperatorClass;
begin
  if NormalizeRPN(Expr) = '' then
    raise EEmptyRPN.Create;

  if Operators = NIL then
    Operators := DefaultOperators;

  VarChars := LowerCase(FRpnVarChars);

  Result := TCompiledRPN.Create;
  try
    Pos := 1;

    while Pos <= Length(Expr) do
    begin
      Handled := True;

        case Char(Expr[Pos]) of
        ' ':        Inc(Pos);
        '0'..'9':   ConstOperand;
        '-', '+':
          if NextChar('0123456789') then
            ConstOperand
            else
              Handled := False;
        else
          Handled := False;
        end;

      if not Handled then
      begin
        OpClass := Operators[Expr[Pos]];
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
  except
    Result.Free;
    raise;
  end;
end;

function CompileCaching(Expr: WideString; Operators: TRpnOperators = NIL): TCompiledRPN;
begin
  Result := GetCache(Expr, Operators);

  if Result = NIL then
  begin
    Result := CompileRPN(Expr);
    SetCache(Expr, Operators, Result);
  end;
end;

function EvalRPN(Expr: WideString; Variables: TRpnVariables; Operators: TRpnOperators): Single;
var
  Eval: TRpnEvaluator;
begin
  Eval := TRpnEvaluator.Create(CompileCaching(Expr, Operators));
  try
    Eval.Variables := Variables;
    Result := Eval.Evaluate;
  finally
    Eval.Free;
  end;
end;

function EvalRPN(Expr: WideString; VarCallback: TRpnVarCallback; Operators: TRpnOperators): Single;
var
  Eval: TRpnEvaluator;
begin
  Eval := TRpnEvaluator.Create(CompileCaching(Expr, Operators));
  try
    Eval.VarCallback := VarCallback;
    Result := Eval.Evaluate;
  finally
    Eval.Free;
  end;
end;

{ TRpnEvaluator }

constructor TRpnEvaluator.Create(Compiled: TCompiledRPN);
begin
  FOperators := TRpnOperators.Create;
  FOperators.Copy(DefaultOperators);

  FStack := TFloatStack.Create;

  FCompiled := Compiled;
end;

destructor TRpnEvaluator.Destroy;
begin
  FStack.Free;
  FOperators.Free;
  inherited;
end;

function TRpnEvaluator.Evaluate: Single;
var
  I: Integer;
  Item: TObject;
begin
  Result := 0;

  for I := 0 to FCompiled.Count - 1 do
  begin
    Item := FCompiled[I];

    if Item.InheritsFrom(TRpnOperand) then
      FStack.Push( TRpnOperand(Item).Value(Self) )
      else if Item.InheritsFrom(TRpnOperator) then
        FStack.Push( TRpnOperator(Item).Execute(Self) )
        else
          raise ERPN.CreateFmt('Invalid object of class %s in a compiled RPN object.', [Item.ClassName]);
  end;

  if FStack.Count = 1 then
    Result := FStack.Pop
    else if FStack.Count > 0 then
      raise ENonEmptyRpnStack.Create(FStack.Count)
      else if FStack.Count <= 0 then
        raise EEmptyRpnStack.Create;
end;

function TRpnEvaluator.GetVariable(Name: WideString): Single;
begin
  if Variables <> NIL then
  begin
    if not Variables.GetIfExists(Name, Result) then
      raise EMissingRpnVariable.Create(Name);
  end
    else if Assigned(VarCallback) then
      Result := VarCallback(Name)
      else
        raise EVariablelessRPN.Create(Name);
end;

{ TRpnMathOperator }

function TRpnMathOperator.Execute(Eval: TRpnEvaluator): Single;
var
  A: Single;
begin
  if Eval.Stack.Count < 2 then
    raise ENotEnoughRpnArguments.Create(ClassName, 2, Eval.Stack.Count);

  A := Eval.Stack.Pop;
  Result := Self.Eval(Eval.Stack.Pop, A);
end;

{ TRpnAdd }

function TRpnAdd.Eval(A, B: Single): Single;
begin
  Result := A + B;
end;

{ TRpnSubtract }

function TRpnSubtract.Eval(A, B: Single): Single;
begin
  Result := A - B;
end;

{ TRpnMultiply }

function TRpnMultiply.Eval(A, B: Single): Single;
begin
  Result := A * B;
end;

{ TRpnDivide }

function TRpnDivide.Eval(A, B: Single): Single;
begin
  Result := A / B;
end;

{ TRpnPower }

function TRpnPower.Eval(A, B: Single): Single;
begin
  Result := Power(A, B);
end;

{ TRpnModulus }

function TRpnModulus.Eval(A, B: Single): Single;
begin
  Result := A - Int(A / B) * B;
end;

initialization
  RegisterDefaultRpnOperator('+', TRpnAdd);
  RegisterDefaultRpnOperator('-', TRpnSubtract);
  RegisterDefaultRpnOperator('*', TRpnMultiply);
  RegisterDefaultRpnOperator('/', TRpnDivide);
  RegisterDefaultRpnOperator('^', TRpnPower);
  RegisterDefaultRpnOperator('%', TRpnModulus);

finalization
  if CachedExprs <> NIL then
    CachedExprs.Free;
  if DefaultOperators <> NIL then
    DefaultOperators.Free;
end.
