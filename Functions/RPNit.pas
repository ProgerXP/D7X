unit RPNit;

{
  This unit is a part of the D7X Library for Delphi 7.
  RPNit by Proger_XP | http://proger.i-forge.net/RPNit
}

interface

uses SysUtils, Classes, Contnrs, StringsW, StringUtils;

type
  TRpnVarCallback = function (Name: WideString): Single of object;
  TRpnOperatorClass = class of TRpnOperator;

  ERPN = class (Exception)
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

    FVariables: TRpnVariables;
    FVarCallback: TRpnVarCallback;
  public
    constructor Create(Compiled: TCompiledRPN);
    destructor Destroy; override;
                                 
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

      TRpnAdd = class (TRpnMathOperator)
      protected
        function Eval(A, B: Single): Single; override;
      end;

      TRpnSubstract = class (TRpnMathOperator)
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

const
  // case-insensitive:
  RpnVarChars:    WideString = 'abcdefghijklmnopqrstuvwxyz0123456789_./\';

// raises EInvalidRpnFloatStr.
function StrToFloatRPN(Str: WideString; Expr: WideString = ''): Single;

function EvalRPN(Expr: WideString; Variables: TRpnVariables): Single; overload;
function EvalRPN(Expr: WideString; VarCallback: TRpnVarCallback): Single; overload;

implementation

var
  CachedExprs: TObjectHash;   // of TCompiledRPN            

{ Exceptions }

constructor EEmptyRPN.Create;
begin
  inherited Create('Empty RPN expression string.');
end;                     

constructor EWrongRpnSyntax.Create(Expr: WideString; Pos: Integer);
begin
  if Pos > 0 then
    CreateFmt('Invalid RPN expression syntax at "%s" (position %d, full string: "%s").',
                        [Copy(Expr, Pos, Length(Expr)), Pos, Expr])
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
  CreateFmt('Too few arguments on stack for %s operator (need %d, has %d).', [Operator, NeedArgs, HasArgs]);
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

function NormalizeRPN(Expr: WideString): WideString;
begin
  Result := LowerCase(Trim(Expr));
end;

function GetCache(Expr: WideString): TCompiledRPN;
begin
  if CachedExprs = NIL then
    Result := NIL
    else
      Result := TCompiledRPN( CachedExprs[NormalizeRPN(Expr)] );
end;

procedure SetCache(Expr: WideString; COmpiled: TCompiledRPN);
begin
  if CachedExprs = NIL then
    with CachedExprs do
    begin
      CachedExprs := TObjectHash.Create(True);
      CaseSensitive := False;
    end;

  CachedExprs.AddObject(NormalizeRPN(Expr), Compiled);
end;

function CompileRPN(Expr: WideString): TCompiledRPN;
var
  Pos: Integer;

  function NextChar(Chars: WideString): Boolean;
  begin
    Result := (Pos < Length(Expr)) and (PosW(Expr[Pos], Chars) > 0);
  end;

  procedure Operator(AClass: TRpnOperatorClass; OpLength: Integer = 1);
  begin
    Result.Add(AClass.Create);
    Inc(Pos, OpLength);
  end;

  function CutName(Chars: WideString): WideString;
  begin
    Result := '';

    while (Pos <= Length(Expr)) and (PosW(Expr[Pos], Chars) <> 0) do
    begin
      Result := Result + Expr[Pos];
      Inc(Pos);
    end;
  end;

    procedure ConstOperand;
    begin
      Result.Add(TRpnConst.Create( StrToFloatRPN(CutName('0123456789')) ));
    end;

    procedure Variable;
    begin
      Result.Add(TRpnVariable.Create( CutName(RpnVarChars) ));
    end;

begin
  if NormalizeRPN(Expr) = '' then
    raise EEmptyRPN.Create;

  Result := TCompiledRPN.Create;
  try
    Pos := 1;

    while Pos <= Length(Expr) do
      case Char(Expr[Pos]) of
      ' ':        Inc(Pos);
      '0'..'9':   ConstOperand;
      '+':        Operator(TRpnAdd);
      '*':        Operator(TRpnMultiply);
      '-':
        if NextChar('0123456789') then
          ConstOperand
          else
            Operator(TRpnSubstract);
      '/':
        if (Pos >= Length(Expr)) or NextChar(' 0123456789') then
          Operator(TRpnDivide)
          else
            Variable;
      else
        if PosW(Expr[Pos], RpnVarChars) <> 0 then
          Variable
          else
            raise EWrongRpnSyntax.Create(Expr, Pos);
      end;
  except
    Result.Free;
    raise;
  end;
end;

function CompileCaching(Expr: WideString): TCompiledRPN;
begin
  Result := GetCache(Expr);

  if Result = NIL then
  begin
    Result := CompileRPN(Expr);
    SetCache(Expr, Result);
  end;
end;

function EvalRPN(Expr: WideString; Variables: TRpnVariables): Single;
var
  Eval: TRpnEvaluator;
begin
  Eval := TRpnEvaluator.Create(CompileCaching(Expr));
  try
    Eval.Variables := Variables;
    Result := Eval.Evaluate;
  finally
    Eval.Free;
  end;
end;

function EvalRPN(Expr: WideString; VarCallback: TRpnVarCallback): Single;
var
  Eval: TRpnEvaluator;
begin
  Eval := TRpnEvaluator.Create(CompileCaching(Expr));
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
  FCompiled := Compiled;
  FStack := TFloatStack.Create;
end;

destructor TRpnEvaluator.Destroy;
begin
  FStack.Free;
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

{ TRpnSubstract }

function TRpnSubstract.Eval(A, B: Single): Single;
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

initialization

finalization
  if CachedExprs <> NIL then
    CachedExprs.Free;
end.
