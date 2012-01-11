unit CommandLine;

interface

{ This unit is standalone. }

uses Windows, SysUtils, Math, Classes, StringUtils, Utils, StringsW;

const
  MaxCLArgs = $7FFF;

type
  TCLWaitOnExit = (clAlwaysWait, clWaitOnError, clNeverWait);
  TCLItemType = (clShortOptions, clLongOption, clArgument);

  ECommandLine = class (Exception)
  end;

    ECLTooFewArguments = class (ECommandLine)
    end;

      ECLTooFewTaskArgs = class (ECLTooFewArguments)
      public
        Task: WideString;
        Required: Integer;  // 1-based
        constructor Create(const Task: WideString; Required: Integer);
      end;

  TCLHash = class (THash)
  public
    constructor Create; override;
  end;

  TCLSplitter = class
  protected
    FCommandLine: WideString;
    FLastCLPos: Integer;

    procedure SetCommandLine(const Value: WideString);
  public
    constructor Create;
    procedure Clear;
    procedure ToStart;

    property CommandLine: WideString read FCommandLine write SetCommandLine;
    function HasMore: Boolean;
    function Next: WideString;
  end;

  TCLParser = class
  protected
    FNotPassed: WideChar;
    FOptions: TCLHash;
    FAliases: TCLHash;
    FArgCount: Integer;
    FArguments: array[0..MaxCLArgs] of WideString;

    function GetArgument(Index: Integer): WideString;
    function GetOption(Name: WideString): WideString;

    procedure AppendFrom(CLParser: TCLSplitter);
    procedure ParseItem(const Item: WideString; out ItemType: TCLItemType; out Key, Value: WideString);
    procedure AddShortOptions(const Key, Value: WideString);
    procedure AddLongOption(const Key, Value: WideString);
    procedure AddArgument(const Key, Value: WideString);
    function GetAlias(Alias: WideString): WideString;
    procedure SetAlias(Alias: WideString; const Value: WideString);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear;
    procedure ClearAliases;
    property Aliases[Alias: WideString]: WideString read GetAlias write SetAlias;

    procedure ParseCL; overload;
    procedure ParseCL(const CLString: WideString); overload;

    // special value to return when an option or an argument wasn't passed.
    property NotPassed: WideChar read FNotPassed write FNotPassed default #0;
    property Options[Name: WideString]: WideString read GetOption; default;
    property ArgCount: Integer read FArgCount;
    property Arguments[Index: Integer]: WideString read GetArgument;
    function IsSwitchOn(Name: WideString): Boolean;
    function GetArgByExt(const Extension: WideString): WideString;

    class procedure RunTests;  // debug method.
  end;

  TCLApplicationLanguage = record
    UnknownTask, WaitOnExit: WideString;
    Exception, FewTaskArgs: WideString;
  end;

  TCLApplication = class
  protected
    F_EOLN: WideString;
    FWaitOnExit: TCLWaitOnExit;

    FLastProgress: String;
    FProgressPrecision: Byte;

    FTaskAliases: TCLHash;
    FCLParser: TCLParser;
    FExitCode: Integer;

    function IsConsoleHandle(Handle: DWord): Boolean;
      procedure WriteStringTo(Handle: DWord; const Str: WideString);
        procedure ErrorWrite(const Str: WideString);
          procedure ErrorWriteLn(const Str: WideString = '');
        procedure ConsoleWrite(const Str: WideString);
          procedure ConsoleWriteLn(const Str: WideString = '');
    procedure ConsoleWaitForEnter;
    procedure ConsoleGoUpOneLine;

    function TryDoingTask(Task: WideString): Boolean; virtual;    // Task is not an alias
    function DoStandardTask(Task: WideString): Boolean;           // Task is not an alias
    procedure UnknownTaskPassed(const Task: WideString);
    procedure BeforeExit; virtual;

    function FormatVersion: WideString;
    function FormatDate: WideString;
    function GetConsoleXY(IncY: SmallInt): TCoord;
    procedure ResetProgress;
    procedure UpdateProgress(Current, Max: DWord; Extra: WideString = '');

    function GetRanOK: Boolean; virtual;
    procedure SetRanOK(Value: Boolean); virtual;
    procedure SetProgressPrecision(Value: Byte);
    function GetTaskAlias(Alias: WideString): WideString;
    procedure SetTaskAlias(ALias: WideString; const Value: WideString);
  public
    Name, Author: WideString;
    Version: Word;
    WWW, Email: WideString;
    BuildDate: DWord;

    Help, HelpDetails: WideString;
    Language: TCLApplicationLanguage;

    constructor Create; virtual;
    destructor Destroy; override;

    procedure SetDefaultLanguage;
    procedure Clear;
    procedure SetDefaults; virtual; abstract;

    property RanOK: Boolean read GetRanOK write SetRanOK default False;
    property ExitCode: Integer read FExitCode write FExitCode default 1;
    property TaskAliases[Alias: WideString]: WideString read GetTaskAlias write SetTaskAlias;
    function TaskAliasIfAnyFor(const Task: WideString): WideString;

    property CommandLine: TCLParser read FCLParser;
    property EOLN: WideString read F_EOLN write F_EOLN;
    property ProgressPrecision: Byte read FProgressPrecision write SetProgressPrecision default 1;
    property WaitOnExit: TCLWaitOnExit read FWaitOnExit write FWaitOnExit default clWaitOnError;
    function IsNakedTask(Task: WideString): Boolean; virtual;

    procedure Run; virtual;
    function GetCLString: WideString; virtual;
    procedure AcquireOptions; virtual;
    procedure HandleException(E: Exception); virtual;
    procedure DoTask(Task: WideString); virtual;

    function TaskArg(const Task: WideString; Index: Integer = 0): WideString;

    procedure OutputHeader;
      function GetHeader: WideString; virtual;
    procedure OutputVersion;
    procedure OutputHelp(Detailed: Boolean = False);
  end;

implementation

var
  StdError, StdOut, StdIn: THandle;

{ Exceptions }

constructor ECLTooFewTaskArgs.Create(const Task: WideString; Required: Integer);
begin
  Self.Task := Task;
  Self.Required := Required;
end;

{ TCLHash }

constructor TCLHash.Create;
begin
  inherited;
  NameValueSeparator := #0;
end;

{ TCLParser }

constructor TCLParser.Create;
begin
  FOptions := TCLHash.Create;
  FAliases := TCLHash.Create;

  FNotPassed := #0;
  Clear;
end;

procedure TCLParser.Clear;
begin
  FOptions.Clear;
  FArgCount := 0;
  ClearAliases;
end;

procedure TCLParser.CLearAliases;
begin
  FAliases.Clear;
end;

destructor TCLParser.Destroy;
begin
  FAliases.Free;
  FOptions.Free;
  inherited;
end;

function TCLParser.GetOption(Name: WideString): WideString;
var
  Index: Integer;
begin
  Result := FNotPassed;

  Index := FOptions.IndexOfName(Name);
  if Index <> -1 then
    Result := FOptions.ValueFromIndex[Index];

  if Result = FNotPassed then
  begin
    Index := FAliases.IndexOfName(Name);
    if Index <> -1 then
      Result := Options[FAliases.ValueFromIndex[Index]];
  end;
end;

function TCLParser.GetArgument(Index: Integer): WideString;
begin
  if Index >= FArgCount then
    Result := FNotPassed
    else
      Result := FArguments[Index];
end;

procedure TCLParser.ParseCL;
begin
  ParseCL( GetCommandLineW );
end;

procedure TCLParser.ParseCL(const CLString: WideString);
var
  CLParser: TCLSplitter;
begin
  Clear;

  CLParser := TCLSplitter.Create;
  try
    CLParser.CommandLine := CLString;
    CLParser.Next;  // to skip EXE's path.
    AppendFrom(CLParser);
  finally
    CLParser.Free;
  end;
end;

procedure TCLParser.AppendFrom(CLParser: TCLSplitter);
var
  ItemType: TCLItemType;
  Key, Value: WideString;
begin
  FOptions.BeginUpdate;
  try
    while CLParser.HasMore do
    begin
      ParseItem( CLParser.Next, ItemType, Key, Value );

      case ItemType of
      clShortOptions: AddShortOptions(Key, Value);
      clLongOption:  AddLongOption(Key, Value);
      else
        AddArgument(Key, Value);
      end;
    end;
  finally
    FOptions.EndUpdate;
  end;
end;

procedure TCLParser.ParseItem(const Item: WideString; out ItemType: TCLItemType;
  out Key, Value: WideString);
var
  Prefix: String[2];
  Delim: Integer;
begin
  Prefix := Item;
  if Prefix = '--' then
  begin
    ItemType := clLongOption;

    Delim := PosW('=', Item);
    if Delim = 0 then
      Delim := $FFFF;  // all is Key.

    Key := Copy(Item, 3, Delim - 3);
    Value := Copy(Item, Delim + 1, $FFFF);
  end
    else if Prefix[1] = '-' then
    begin
      ItemType := clShortOptions;
      Key := Copy(Item, 2, $FFFF);
    end
      else
      begin
        ItemType := clArgument;
        Value := Item;
      end;
end;

procedure TCLParser.AddShortOptions(const Key, Value: WideString);
var
  I: Word;
begin
  for I := 1 to Length(Key) do
    AddLongOption(Key[I], '1');
end;

procedure TCLParser.AddLongOption(const Key, Value: WideString);
begin
  FOptions.Add(Key + FOptions.NameValueSeparator + Value);
end;

procedure TCLParser.AddArgument(const Key, Value: WideString);
begin
  if FArgCount >= Length(FArguments) then
    raise ECommandLine.CreateFmt('%s: max number of arguments (%d) reached.', [ClassName, Length(FArguments)]);

  FArguments[FArgCount] := Value;
  Inc(FArgCount);
end;

function TCLParser.GetArgByExt(const Extension: WideString): WideString;
var
  I: Integer;
begin
  for I := 0 to FArgCount - 1 do
    if ExtractFileExt(FArguments[I]) = Extension then
    begin
      Result := FArguments[I];
      Exit;
    end;

  Result := '';
end;

function TCLParser.IsSwitchOn(Name: WideString): Boolean;
begin
  Name := LowerCase( Options[Name] );
  Result := (Name <> FNotPassed) and (Name <> '0') and (Name <> 'false') and
            (Name <> 'off') and (Name <> 'no');
end;

function TCLParser.GetAlias(Alias: WideString): WideString;
begin
  Result := FAliases.Values[Alias];
end;

procedure TCLParser.SetAlias(ALias: WideString; const Value: WideString);
begin
  FAliases.Values[Alias] := Value;
end;

{ TCLSplitter }

constructor TCLSplitter.Create;
begin
  Clear;
end;

procedure TCLSplitter.Clear;
begin
  FCommandLine := '';
  ToStart;
end;

procedure TCLSplitter.ToStart;
begin
  FLastCLPos := 0;
end;

procedure TCLSplitter.SetCommandLine(const Value: WideString);
begin
  FCommandLine := Value;
  ToStart;
end;

function TCLSplitter.HasMore: Boolean;
begin
  Result := FLastCLPos < Length(FCommandLine);
end;

function TCLSplitter.Next: WideString;
  procedure SetNext;
  var
    Join: Boolean;
  begin
    Join := False;

    while HasMore do
    begin
      Inc(FLastCLPos);

      if FCommandLine[FLastCLPos] = '"' then
        Join := not Join
        else if (FCommandLine[FLastCLPos] = ' ') and not Join then
          Break
          else
            Result := Result + FCommandLine[FLastCLPos];
    end;
  end;

begin
  repeat
    Result := '';
    SetNext;
  until not HasMore or (Result <> '');
end;

{ Debug }

class procedure TCLParser.RunTests;
  procedure Assert(Condition: Boolean);
  begin
    if not Condition then
      raise EAssertionFailed.Create('');
  end;

  procedure RunTests;
  const
    NotPassed: WideChar = #5;
  var
    Parser: TCLSplitter;
    CL: TCLParser;
  begin
    Parser := TCLSplitter.Create;
    try
      Assert( not Parser.HasMore );
        Assert( Parser.Next = '' );

      Parser.CommandLine := ' 1st "2 nd" "3 rd " 4th 5 6 "7 "7" 7" ';

      Assert( Parser.HasMore );
        Assert( Parser.Next = '1st' );
        Assert( Parser.Next = '2 nd' );
        Assert( Parser.Next = '3 rd ' );
        Assert( Parser.Next = '4th' );
        Assert( Parser.Next = '5' );
        Assert( Parser.Next = '6' );
        Assert( Parser.Next = '7 7 7' );
      Assert( not Parser.HasMore );
        Assert( Parser.Next = '' );
      Assert( not Parser.HasMore );
    finally
      Parser.Free;
    end;

    CL := TCLParser.Create;
    try
      CL.NotPassed := NotPassed;

      CL.ParseCL('arg_1 --long-opt -sho -rt arg_2 --loo=ong arg_3');

      Assert( CL['not-passed'] = NotPassed );
      Assert( CL['long-opt'] = '' );
      Assert( CL['loo'] = 'ong' );

      Assert( CL['l'] = NotPassed );
      Assert( CL['s'] = '1' );
      Assert( CL['h'] = '1' );
      Assert( CL['o'] = '1' );
      Assert( CL['r'] = '1' );
      Assert( CL['t'] = '1' );

      Assert( CL.Arguments[0] = 'arg_1' );
      Assert( CL.Arguments[1] = 'arg_2' );
      Assert( CL.Arguments[2] = 'arg_3' );
      Assert( CL.Arguments[4] = NotPassed );
    finally
      CL.Free;
    end;
  end;

begin
  try
    RunTests;
  except
    raise;
  end;
end;

{ TCLApplication }

constructor TCLApplication.Create;
begin
  F_EOLN := #13#10;
  FProgressPrecision := 1;
  FWaitOnExit := clWaitOnError;
  FTaskAliases := TCLHash.Create;
  FCLParser := TCLParser.Create;
  RanOK := False;

  SetDefaultLanguage;
  Clear;

  SetDefaults;
end;

destructor TCLApplication.Destroy;
begin
  FCLParser.Free;
  FTaskAliases.Free;
  inherited;
end;

procedure TCLApplication.SetDefaultLanguage;
begin
  Language.UnknownTask := 'Unknown task "%s" specified.';
  Language.Exception := StringOfChar('-', 80) +
                        '  Oops! Some <%s> exception has occured. Here''s what it says:' + F_EOLN + '%s';
  Language.FewTaskArgs := 'Argument #%d is required for task "%s".';
  Language.WaitOnExit := '______________________' + F_EOLN +
                          'Press Enter to exit...';
end;

procedure TCLApplication.Clear;
begin
  Name := '';
  Version := 0;
  Author := '';
  WWW := '';
  Email := '';
  BuildDate := 0;

  Help := '';
  HelpDetails := '';

  FTaskAliases.Clear;
  FCLParser.Clear;
  ResetProgress;
end;

procedure TCLApplication.OutputHeader;
begin
  ConsoleWriteLn( GetHeader );
end;

function TCLApplication.GetHeader: WideString;
  function SecondLine: WideString;
  begin
    if Author <> '' then
    begin
      Result := '  by ' + Author;
      if BuildDate <> 0 then
        Result := Result + ',';
    end;

    if BuildDate <> 0 then
      Result := Result + ' ' + FormatDate;
  end;

begin
  Result := '~ ' + Name + ' ';
  if Version <> 0 then
    Result := Result + FormatVersion + ' ';
  Result := Result + '~ ';

  if WWW <> '' then
    Result := Result + ' ' + WWW + ' ';
  if Email <> '' then
    Result := Result + ' ' + Email + ' ';

  Result := WideFormat('%-79s' + F_EOLN + '%80s', [Result, SecondLine]);
end;

function TCLApplication.FormatVersion: WideString;
begin
  Result := Format('%d.%d', [Hi(Version), Lo(Version)]);
end;

function TCLApplication.FormatDate: WideString;
var
  StrDate: String;
begin
  StrDate := IntToHex(BuildDate, 8);
  Result := DateTimeToStr(EncodeDate(StrToInt(Copy(StrDate, 5, 4)),   {year}
                                     StrToInt(Copy(StrDate, 3, 2)),   {month}
                                     StrToInt(Copy(StrDate, 1, 2)))); {day}
end;

function TCLApplication.TryDoingTask(Task: WideString): Boolean;
begin
  Result := DoStandardTask(Task);
end;

function TCLApplication.DoStandardTask(Task: WideString): Boolean;
begin
  Task := LowerCase(Task);
  Result := True;

  if Task = FCLParser.NotPassed then
    OutputHelp(False)
    else if Task = 'h' then
      OutputHelp(True)
      else if Task = 'v' then
        OutputVersion
        else
          Result := False;
end;

procedure TCLApplication.OutputHelp(Detailed: Boolean = False);
begin
  ConsoleWrite(Help);
  RanOK := False;

  if Detailed then
  begin
    ConsoleWriteLn;
    ConsoleWrite(HelpDetails);
  end;
end;

procedure TCLApplication.ErrorWriteLn(const Str: WideString);
begin
  ErrorWrite(Str);
  ErrorWrite(F_EOLN);
end;

procedure TCLApplication.ErrorWrite(const Str: WideString);
begin
  WriteStringTo(StdError, Str);
end;

procedure TCLApplication.ConsoleWriteLn(const Str: WideString = '');
begin
  ConsoleWrite(Str);
  ConsoleWrite(F_EOLN);
end;

procedure TCLApplication.ConsoleWrite(const Str: WideString);
begin
  WriteStringTo(StdOut, Str);
end;

procedure TCLApplication.WriteStringTo(Handle: DWord; const Str: WideString);
var
  WrittenCount: DWord;
begin
  if IsConsoleHandle(Handle) then
    WriteConsoleW(Handle, @Str[1], Length(Str), WrittenCount, NIL)
    else
      WriteFile(Handle, Str[1], Length(Str) * 2, WrittenCount, NIL);
end;

function TCLApplication.IsConsoleHandle(Handle: DWord): Boolean;
begin
  Result := GetFileType(Handle) = FILE_TYPE_CHAR;
end;

procedure TCLApplication.ConsoleWaitForEnter;
var
  Ch: WideChar;
  CharsRead: DWord;
begin
  if IsConsoleHandle(StdIn) and IsConsolehandle(StdOut) then
    repeat
      if not ReadConsoleW(StdIn, @Ch, 1, CharsRead, NIL) then
        Break;
    until (Byte(Ch) in [13, 10]) or (CharsRead = 0);
end;

function TCLApplication.GetConsoleXY(IncY: SmallInt): TCoord;
var
  Info: TConsoleScreenBufferInfo;
begin
  FillMemory(@Result, SizeOf(Result), 0);
  if GetConsoleScreenBufferInfo(StdOut, Info) then
    with Info.dwCursorPosition do
    begin
      Result.X := X;
      Result.Y := Y + IncY
    end
end;

procedure TCLApplication.ResetProgress;
begin
  FLastProgress := '';
end;

procedure TCLApplication.UpdateProgress(Current, Max: DWord; Extra: WideString = '');
var
  Str: WideString;
begin
  Str := Format( '  [ %.' + IntToStr(FProgressPrecision) + 'f%% ]... ',
                [Current / Max * 100] ) + Extra;
  if Str <> FLastProgress then
  begin
    if FLastProgress <> '' then
      ConsoleGoUpOneLine;

    ConsoleWriteLn(Str);
    FLastProgress := Str;
  end;
end;

procedure TCLApplication.ConsoleGoUpOneLine;
begin
  if SetConsoleCursorPosition(StdOut, GetConsoleXY(-1)) then
  begin
    ConsoleWrite( StringOfChar(' ', 80) );
    SetConsoleCursorPosition(StdOut, GetConsoleXY(-1));
  end;
end;

procedure TCLApplication.SetProgressPrecision(Value: Byte);
begin
  FProgressPrecision := Min(9, Value);
end;

procedure TCLApplication.Run;
var
  Task: WideString;
begin
  RanOK := True;  // note that tasks in TryDoingTask may also set this.

  try
    FCLParser.ParseCL( GetCLString );
    AcquireOptions;

    Task := FCLParser.Arguments[0];

    if IsNakedTask(Task) then
      FWaitOnExit := clNeverWait
      else
      begin
        OutputHeader;
        ConsoleWriteLn;
      end;

    DoTask(Task);
  except
    on E: Exception do
    begin
      RanOK := False;
      HandleException(E);
    end;
  end;

  BeforeExit;
end;

procedure TCLApplication.HandleException(E: Exception);
var
  Msg: WideString;
begin
  if E.InheritsFrom(ECLTooFewTaskArgs) then
    Msg := WideFormat(Language.FewTaskArgs, [ECLTooFewTaskArgs(E).Task, ECLTooFewTaskArgs(E).Required + 1])
    else
      Msg := WideFormat(Language.Exception, [E.ClassName, E.Message]);

  ConsoleWrite(Msg);
end;

function TCLApplication.GetCLString: WideString;
begin
  Result := GetCommandLineW;
end;

procedure TCLApplication.AcquireOptions;
begin
  if FCLParser.IsSwitchOn('no-wait') then
    FWaitOnExit := clNeverWait
    else if FCLParser.IsSwitchOn('wait') then
      FWaitOnExit := clAlwaysWait
      else if FCLParser.IsSwitchOn('wait-on-error') then
        FWaitOnExit := clWaitOnError;
end;

// recall naked C functions and the name probably won't seem strange to you...
// naked tasks have no header output and never wait for Enter after they end.
function TCLApplication.IsNakedTask(Task: WideString): Boolean;
begin
  Task := TaskAliasIfAnyFor(Task);
  Result := LowerCase(Task) = 'v';
end;

procedure TCLApplication.DoTask(Task: WideString);
begin
  Task := TaskAliasIfAnyFor(Task);

  if not TryDoingTask(Task) then
    UnknownTaskPassed(Task);
end;

procedure TCLApplication.UnknownTaskPassed(const Task: WideString);
begin
  ConsoleWriteLn( WideFormat(Language.UnknownTask, [Task]) );
  ConsoleWriteLn;
  OutputHelp;
end;

procedure TCLApplication.BeforeExit;
begin
  if ((FWaitOnExit = clAlwaysWait) or ((FWaitOnExit = clWaitOnError) and not RanOK)) and IsConsolehandle(StdOut) then
  begin
    ConsoleWriteLn;
    ConsoleWriteLn;
    ConsoleWrite(Language.WaitOnExit);
    ConsoleWaitForEnter;
  end;
end;

procedure TCLApplication.OutputVersion;
begin
  ConsoleWrite(Name + '  v');
  if Version = 0 then
    ConsoleWrite( FormatDate )
    else
      ConsoleWrite( FormatVersion );
end;

function TCLApplication.GetTaskAlias(Alias: WideString): WideString;
begin
  Result := FTaskAliases.Values[Alias];
end;

procedure TCLApplication.SetTaskAlias(ALias: WideString; const Value: WideString);
begin
  FTaskAliases.Values[Alias] := Value;
end;

function TCLApplication.TaskAliasIfAnyFor(const Task: WideString): WideString;
var
  Real: WideString;
begin
  Real := TaskAliases[Task];
  if Real = '' then
    Result := Task
    else
      Result := Real;
end;

function TCLApplication.GetRanOK: Boolean;
begin
  Result := FExitCode = 0;
end;

procedure TCLApplication.SetRanOK(Value: Boolean);
begin
  if Value then
    FExitCode := 0
    else
      FExitCode := 1;
end;

function TCLApplication.TaskArg(const Task: WideString; Index: Integer): WideString;
begin
  Result := FCLParser.Arguments[Index];
  if Result = FCLParser.NotPassed then
    raise ECLTooFewTaskArgs.Create(Task, Index);
end;

initialization
  StdError := GetStdHandle(STD_ERROR_HANDLE);
  StdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  StdIn := GetStdHandle(STD_INPUT_HANDLE);
end.
