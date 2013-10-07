unit CommandLine;

interface

uses Classes, Windows, SysUtils, Math, FileStreamW, StringsW, IniFIlesW, ColorConsole,
     StringUtils, Utils;

const
  MaxCLArgs = $7FFF;

type
  TCLAppLang = class;

  TCLWaitOnExit = (clAlwaysWait, clWaitOnError, clNeverWait);
  TCLItemType = (clShortOptions, clLongOption, clArgument);

  TCLOnUndefinedLang = function (Lang: TCLAppLang; Str: WideString): WideString of object;

  ECommandLine = class (Exception)
  end;

    ECLUnknownTask = class (ECommandLine)
    public
      Task: WideString;
      constructor Create(const Task: WideString);
    end;

    ECLMissingParam = class (ECommandLine)
    end;

      ECLTooFewTaskArgs = class (ECLMissingParam)
      public
        Task: WideString;
        RequiredCount: Integer;

        constructor Create(const Task: WideString; RequiredCount: Integer);
      end;

      ECLNoRequiredTaskOption = class (ECLMissingParam)
      public
        Task, Option: WideString;
        constructor Create(const Task, Option: WideString);
      end;

    ECLText = class (ECommandLine)
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
    function IsPassed(const Option: WideString): Boolean;
    function GetOrDefault(const Option, Default: WideString): WideString;
    function IsSwitchOn(Name: WideString; Default: Boolean = False): Boolean;
    function GetArgByExt(const Extension: WideString): WideString;

    class procedure RunTests;  // debug method.
  end;

  TCLAppLang = class (THash)
  protected
    F_EOLN: WideString;
    FOnUndefined: TCLOnUndefinedLang;

    function GetTr(What: WideString): WideString;
    procedure SetTr(What: WideString; const Value: WideString);
  public
    constructor Create; override;

    procedure AddDefaultStrings;
    procedure SetDefaultStrings; virtual;

    function LoadIniFromResource(const Name: WideString; Lang: Word = 0): TMemIniFileW;
    function ExtendedIniToSimple(const Ext: WideString): WideString;
    // if Section isn't found the first section in the INI file is used.
    procedure LoadFromResource(const Name: WideString; Lang: Word = 0;
      Section: WideString = 'Console');
    function AddObject(const S: WideString; AObject: TObject): Integer; override;

    procedure LoadFromIniFile(const FN: WideString; Section: WideString = 'Console');

    property EOLN: WideString read F_EOLN write F_EOLN;
    property OnUndefined: TCLOnUndefinedLang read FOnUndefined;
    property Translate[What: WideString]: WideString read GetTr write SetTr; default;

    function Format(const What: WideString; Fmt: array of const): WideString;
    // raises ECLText calling Format(What, Fmt).
    procedure RaiseText(const What: WideString; Fmt: array of const);
  end;

  TCLApplication = class
  protected
    FLang: TCLAppLang;
    F_EOLN: WideString;

    FWaitOnExit: TCLWaitOnExit;
    FShowExitCodeOnExit: Boolean;
    FShowUtf8ConsoleWarning: Boolean;

    FLastProgress: String;
    FProgressPrecision: Byte;

    FTaskAliases: TCLHash;
    FLastTask: WideString;
    FCLParser: TCLParser;
    FExitCode: Integer;

    procedure Init; virtual;

    // will convert standalone LFs into CRLF pairs if F_EOLN = CRLF.
    procedure WriteStringTo(Handle: DWord; Str: WideString); virtual;
    function IsConsoleHandle(Handle: DWord = 0): Boolean;
    function NormalizeWritingStr(const Str: WideString): WideString;

      procedure ConsoleWrite(const Str: WideString);
      procedure ConsoleWriteLn(const Str: WideString = '');
      procedure ErrorWrite(const Str: WideString);
      procedure ErrorWriteLn(const Str: WideString = '');

    procedure ConsoleWaitForEnter;
    procedure ConsoleGoUpOneLine;
    procedure EnsureConsoleAtNewLine;
    function OnCtrlEvent(Event: DWord): Boolean; virtual;

    function GetTaskArgument: WideString; virtual;
    function TryDoingTask(Task: WideString): Boolean; virtual;    // Task is not an alias
    function DoStandardTask(Task: WideString): Boolean;           // Task is not an alias
    procedure BeforeExit; virtual;

    function CreateLang: TCLAppLang; virtual;
    function FormatVersion: WideString;
    function FormatDate: WideString;
    // IncY is added to Result.Y - useful to pass Result directly to SetConsoleCursorPosition.
    function GetConsoleXY(IncY: SmallInt = 0): TCoord;
    procedure ResetProgress;
    procedure UpdateProgress(Current, Max: DWord; Extra: WideString = '');

    function GetRanOK: Boolean; virtual;
    procedure SetRanOK(Value: Boolean); virtual;
    procedure SetProgressPrecision(Value: Byte);
    function GetTaskAlias(Alias: WideString): WideString;
    procedure SetTaskAlias(ALias: WideString; const Value: WideString);
    procedure SetEOLN(const Value: WideString);
  public
    Name, Author: WideString;
    Version: Word;
    WWW, Email: WideString;
    BuildDate: DWord;

    Help, HelpDetails: WideString;

    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear;
    procedure SetInfo; virtual; abstract;
    procedure SetupConsoleTricks; virtual;

    property RanOK: Boolean read GetRanOK write SetRanOK default False;
    property ExitCode: Integer read FExitCode write FExitCode default 1;
    property TaskAliases[Alias: WideString]: WideString read GetTaskAlias write SetTaskAlias;
    function TaskAliasIfAnyFor(const Task: WideString): WideString;

    property CommandLine: TCLParser read FCLParser;
    property Language: TCLAppLang read FLang;
    property EOLN: WideString read F_EOLN write SetEOLN;
    property ProgressPrecision: Byte read FProgressPrecision write SetProgressPrecision default 1;
    property WaitOnExit: TCLWaitOnExit read FWaitOnExit write FWaitOnExit default clWaitOnError;
    property ShowExitCodeOnExit: Boolean read FShowExitCodeOnExit write FShowExitCodeOnExit default True;
    property ShowUtf8ConsoleWarning: Boolean read FShowUtf8ConsoleWarning write FShowUtf8ConsoleWarning default True;

    function IsNakedTask(Task: WideString): Boolean; virtual;

    procedure Run; virtual;
    function GetCLString: WideString; virtual;
    procedure AcquireOptions; virtual;
    procedure HandleException(E: Exception); virtual;
    procedure DoTask(Task: WideString); virtual;

    function TaskArg(const Task: WideString; Index: Integer = 0): WideString;

    procedure OutputHeader; virtual;
      function GetHeader: TWideStringArray; virtual;
    procedure OutputExitCode(Code: Integer); virtual;
    procedure OutputVersion;
    procedure OutputHelp(Detailed: Boolean = False);
  end;

  TCLColorApplication = class (TCLApplication)
  protected
    FInitialConsoleCP: DWord;
    FCCParsing: TCCParsingOptions;
    FCCWriting: TCCWritingOptions;

    procedure Init; override;
    function CreateLang: TCLAppLang; override;

    procedure WriteStringTo(Handle: DWord; Str: WideString); override;
  public
    destructor Destroy; override;

    procedure AcquireOptions; override;
    procedure HandleException(E: Exception); override;

    procedure OutputHeader; override;
    procedure OutputExitCode(Code: Integer); override;

    function CCQuote(const Str: WideString): WideString;
  end;

const
  ClExitCodeForHelp = 8083;

implementation

var
  StdError, StdOut, StdIn: THandle;

{ Exceptions }

constructor ECLUnknownTask.Create(const Task: WideString);
begin
  CreateFmt('Unknown task passed (%s).', [Task]);
  Self.Task := Task;
end;

constructor ECLTooFewTaskArgs.Create(const Task: WideString; RequiredCount: Integer);
begin
  Self.Task := Task;
  Self.RequiredCount := RequiredCount;

  CreateFmt('"%s" task requires at least %d arguments.', [Task, RequiredCount]);
end;

constructor ECLNoRequiredTaskOption.Create(const Task, Option: WideString);
begin
  Self.Task := Task;
  Self.Option := Option;

  CreateFmt('"%s" task requires "%s" option.', [Task, Option]);
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

function TCLParser.IsPassed(const Option: WideString): Boolean;
begin
  Result := Options[Option] <> FNotPassed;
end;

function TCLParser.GetOrDefault(const Option, Default: WideString): WideString;
begin
  Result := Options[Option];
  if Result = FNotPassed then
    Result := Default;
end;

function TCLParser.IsSwitchOn(Name: WideString; Default: Boolean = False): Boolean;
begin
  Name := Options[Name];

  if Name = FNotPassed then
    Result := Default
    else
    begin
      Name := LowerCase(Name);
      Result := (Name <> '0') and (Name <> 'false') and (Name <> 'off') and (Name <> 'no');
    end;
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

{ TCLAppLang }

constructor TCLAppLang.Create;
begin
  inherited;
  CaseSensitive := True;

  SetDefaultStrings;
end;

procedure TCLAppLang.SetDefaultStrings;
begin
  Clear;
  AddDefaultStrings;
end;

procedure TCLAppLang.AddDefaultStrings;
begin
  Translate['window title'] := '%s%s - %s';

  Translate['header 1'] := '~ %s%s~%s%s';
  Translate['header line prefix'] := '  ';
  Translate['header 2'] := '%s';
  Translate['author'] := 'by %s';
  Translate['header joiner'] := ', ';

  Translate['exception'] := StringOfChar('-', 80) +
                            '  Oops! An <%s> exception has occured. Here''s what it says:' + F_EOLN +
                            '%s' + F_EOLN + 'Exception address: %.8X';
  Translate['error: unknown task'] := 'Unknown task "%s" specified.';
  Translate['error: no task argument'] := '"%s" task requires at least %d arguments.';
  Translate['error: no task option'] := '"%s" task requires "%s" option.';

  Translate['code on exit'] := 'Exiting with code %d.';
  Translate['wait on exit'] := '______________________' + F_EOLN +
                               'Press Enter to exit...';
  Translate['progress'] := '  [ %.$f%% ]... %s';
  Translate['version'] := '%s  v%s';
  Translate['utf-8 message'] := 'Note: above line contained UTF-8 characters; if' +
                                ' they are not properly displayed' + F_EOLN +
                                '       try changing Font in the Properties dialog of' +
                                ' the program''s EXE file.';
end;

function TCLAppLang.GetTr(What: WideString): WideString;
var
  I: Integer;
begin
  I := IndexOfName(What);

  if I = -1 then
    if Assigned(FOnUndefined) then
      Result := FOnUndefined(Self, What)
      else
        Result := What
    else
      Result := ValueFromIndex[I];
end;

procedure TCLAppLang.SetTr(What: WideString; const Value: WideString);
begin
  Add(What + NameValueSeparator + Value);
end;

function TCLAppLang.Format(const What: WideString; Fmt: array of const): WideString;
begin
  Result := WideFormat(Translate[What], Fmt);
end;

function TCLAppLang.LoadIniFromResource(const Name: WideString; Lang: Word = 0): TMemIniFileW;
  procedure Error;
  begin
    raise EResNotFound.CreateFmt('Resource "%s" (language %.4X) to load language strings from doesn''t exist.', [Name, Lang]);
  end;

var
  ResInfo: HRSRC;
  Handle: HGLOBAL;
  Buf: String;
begin
  ResInfo := FindResourceExW(hInstance, PWideChar(RT_RCDATA), PWideChar(Name), Lang);

  Handle := LoadResource(hInstance, ResInfo);
  if Handle = 0 then
    Error;

  try
    SetLength(Buf, SizeOfResource(hInstance, ResInfo));
    if Length(Buf) = 0 then
      Error;

    Result := TMemIniFileW.Create;
    try
      CopyMemory(@Buf[1], LockResource(Handle), Length(Buf));
      Result.LoadFromString( ExtendedIniToSimple(UTF8Decode(Buf)) );
    except
      Result.Free;
      raise;
    end;
  finally
    UnlockResource(Handle);
    FreeResource(Handle);
  end;
end;

function TCLAppLang.ExtendedIniToSimple(const Ext: WideString): WideString;
type
  TFolding = (flNone, flWithNewlines, flNoNewlines);
const
  NL = '{NL}';
var
  Strings: TStringListW;
  Index, FoldStart: Integer;
  S, Folded: WideString;
  Folding: TFolding;

  procedure EndFolding;
  var
    I: Integer;
  begin
    if Folding <> flNone then
    begin
      Strings[FoldStart] := Strings[FoldStart] + Folded;

      for I := Index - 1 downto FoldStart + 1 do
        Strings.Delete(I);

      Index := FoldStart;
      Folding := flNone;
      Folded := '';
    end;
  end;

begin
  Strings := TStringListW.Create;
  try
    Strings.Text := Ext;
    Folding := flNone;

    Index := 0;
    while Index < Strings.Count do
    begin
      if Folding <> flNone then
        if (Copy(Strings[Index], 1, 2) = '  ') or (Strings[Index] = '') then
        begin
          if (Folded <> '') and (Folding = flWithNewlines) then
            Folded := Folded + NL;

          Folded := Folded + Copy(Strings[Index], 3, Length(Strings[Index]));
        end
          else
          begin
            if Folding = flWithNewlines then
              Folded := Copy(Folded, 1, Length(Folded) - Length(NL));
            EndFolding;
          end;

      if (Folding = flNone) and
         (Index < Strings.Count - 1) and (Copy(Strings[Index + 1], 1, 2) = '  ') then
      begin
        S := TrimRight(Strings[Index]);
        if CountSubstr(NameValueSeparator, S) = 1 then
          if S[Length(S)] = NameValueSeparator then
            Folding := flWithNewlines
            else if S[Length(S)] = '>' then
            begin
              Folding := flNoNewlines;
              Strings[Index] := Copy(S, 1, Length(S) - 1);
            end;

        if Folding <> flNone then
        begin
          FoldStart := Index;
          Folded := '';
        end;
      end;

      Inc(Index);
    end;

    EndFolding;

    Strings.LineBreak := F_EOLN;
    Result := Strings.Text;
  finally
    Strings.Free;
  end;
end;

procedure TCLAppLang.LoadFromResource(const Name: WideString; Lang: Word;
  Section: WideString);
var
  Ini: TMemIniFileW;
begin
  Ini := LoadIniFromResource(Name, Lang);
  try
    if not Ini.SectionExists(Section) then
      Section := Ini.FirstSection;

    Ini.ReadSectionValues(Section, Self, False);
  finally
    Ini.Free;
  end;
end;

function TCLAppLang.AddObject(const S: WideString; AObject: TObject): Integer;
begin
  Result := IndexOfName(ExtractName(S));
  if Result = -1 then
    Result := inherited AddObject(S, AObject)
    else
    begin
      Strings[Result] := S;
      Objects[Result] := AObject;
    end;
end;

procedure TCLAppLang.LoadFromIniFile(const FN: WideString; Section: WideString);
var
  Ini: TMemIniFileW;
begin
  Ini := TMemIniFileW.Create;
  try
    Ini.LoadFromString( ExtendedIniToSimple(TFileStreamW.LoadUnicodeFrom(FN)) );

    if not Ini.SectionExists(Section) then
      Section := Ini.FirstSection;

    Ini.ReadSectionValues(Section, Self, False);
  finally
    Ini.Free;
  end;
end;

procedure TCLAppLang.RaiseText(const What: WideString; Fmt: array of const);
begin
  raise ECLText.Create( Format(What, Fmt) );
end;

{ TCLApplication }

constructor TCLApplication.Create;
begin
  Init;
  Clear;
  SetInfo;
  SetupConsoleTricks;
end;

destructor TCLApplication.Destroy;
begin
  FLang.Free;
  FCLParser.Free;
  FTaskAliases.Free;

  inherited;
end;

procedure TCLApplication.Init;
begin
  F_EOLN := #13#10;
  FProgressPrecision := 1;

  FWaitOnExit := clWaitOnError;
  FShowExitCodeOnExit := True;
  FShowUtf8ConsoleWarning := True;

  FTaskAliases := TCLHash.Create;
  FCLParser := TCLParser.Create;
  RanOK := False;

  FLang := CreateLang;
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

  FLastTask := '';

  FTaskAliases.Clear;
  FCLParser.Clear;
  ResetProgress;
end;

var
  CPObj: TCLApplication = NIL;

function ConsoleProc(Event: DWord): Boolean; stdcall;
begin
  Result := Assigned(CPObj) and CPObj.OnCtrlEvent(Event);
end;

procedure TCLApplication.SetupConsoleTricks;
var
  Ver: WideString;
begin
  if Version = 0 then
    Ver := ''
    else
      Ver := ' ' + FormatVersion;

  SetConsoleTitleW(PWideChar( FLang.Format('window title', [Name, Ver, ParamStrW(0)]) ));

  CPObj := Self;
  SetConsoleCtrlHandler(@ConsoleProc, True);
end;

function TCLApplication.OnCtrlEvent(Event: DWord): Boolean;
begin
  Result := False;
end;

procedure TCLApplication.OutputHeader;
var
  Lines: TWideStringArray;
  S: WideString;
begin
  Lines := GetHeader;

  if IsConsoleHandle then
    S := StrPad(Lines[0], 80) + WideFormat('%80s', [Lines[1]])
    else
      S := Lines[0] + F_EOLN + WideFormat('%80s', [Lines[1]]);

  ConsoleWriteLn(S);
end;

function TCLApplication.GetHeader: TWideStringArray;
  function SecondLine: WideString;
  begin
    if Author <> '' then
      Result := FLang.Format('author', [Author]);

    if BuildDate <> 0 then
      if Result = '' then
        Result := Result + FLang['header line prefix'] + FormatDate
        else
          Result := Result + FLang['header joiner'] + FormatDate;
  end;

  function Pad(const S: WideString): WideString;
  begin
    Result := S;
    if S <> '' then
      Insert(' ', Result, 1);
  end;

var
  Ver: WideString;
begin
  if Version = 0 then
    Ver := ''
    else
      Ver := ' ' + FormatVersion;

  SetLength(Result, 2);
  Result[0] := FLang.Format('header 1', [Name, Ver, Pad(WWW), Pad(Email)]);
  Result[1] := FLang.Format('header 2', [SecondLine]);
end;

function TCLApplication.CreateLang: TCLAppLang;
begin
  Result := TCLAppLang.Create;
  Result.EOLN := F_EOLN;
end;

function TCLApplication.FormatVersion: WideString;
begin
  Result := WideFormat('%d.%d', [Hi(Version), Lo(Version)]);
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

  if (Task = FCLParser.NotPassed) or (Task = 'help') then
  begin
    OutputHelp(Task = 'help');
    FExitCode := ClExitCodeForHelp;
  end
    else if Task = 'version' then
      OutputVersion
      else
        Result := False;
end;

procedure TCLApplication.OutputHelp(Detailed: Boolean = False);
begin
  ConsoleWriteLn(Help);

  if Detailed then
    ConsoleWriteLn(F_EOLN + HelpDetails);
end;

procedure TCLApplication.ErrorWriteLn(const Str: WideString);
begin
  ErrorWrite(Str + F_EOLN);
end;

procedure TCLApplication.ErrorWrite(const Str: WideString);
begin
  WriteStringTo(StdError, Str);
end;

procedure TCLApplication.ConsoleWriteLn(const Str: WideString = '');
begin
  ConsoleWrite(Str + F_EOLN);
end;

procedure TCLApplication.ConsoleWrite(const Str: WideString);
var
  I: Integer;
begin
  WriteStringTo(StdOut, Str);

  if FShowUtf8ConsoleWarning and IsConsoleHandle and (GetConsoleOutputCP = CP_UTF8) then
    for I := 1 to Length(Str) do
      if Ord(Str[I]) > 127 then
      begin
        FShowUtf8ConsoleWarning := False;
        EnsureConsoleAtNewLine;
        ConsoleWriteLn(FLang['utf-8 message']);

        Break;
      end;
end;

procedure TCLApplication.WriteStringTo(Handle: DWord; Str: WideString);
var
  WrittenCount: DWord;
begin
  Str := NormalizeWritingStr(Str);

  if IsConsoleHandle(Handle) then
    WriteConsoleW(Handle, @Str[1], Length(Str), WrittenCount, NIL)
    else
      WriteFile(Handle, Str[1], Length(Str) * 2, WrittenCount, NIL);
end;

  function TCLApplication.NormalizeWritingStr(const Str: WideString): WideString;
  const
    CR = #13;
    LF = #10;
  var
    I: Integer;
  begin
    Result := Str;

    if F_EOLN = CR + LF then
      for I := Length(Result) downto 1 do
        if (Result[I] = LF) and ((I = 1) or (Result[I - 1] <> CR)) then
          Insert(CR, Result, I);
  end;

  function TCLApplication.IsConsoleHandle(Handle: DWord): Boolean;
  begin
    if Handle = 0 then
      Handle := StdOut;
    Result := GetFileType(Handle) = FILE_TYPE_CHAR;
  end;

procedure TCLApplication.ConsoleWaitForEnter;
begin
  if IsConsoleHandle(StdIn) and IsConsolehandle(StdOut) then
    ReadLn;

    { The code below is supposed to work but ReadConsole often returns old keypresses
      (e.g. put two ReadConsole one after enother - they'll behave in a strange way. }
    {repeat
      if not ReadConsoleW(StdIn, @Ch, 1, CharsRead, NIL) then
        Break;
    until (Byte(Ch) in [13, 10]) or (CharsRead = 0);}
end;

function TCLApplication.GetConsoleXY(IncY: SmallInt = 0): TCoord;
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
  Str := FLang.Format('progress bar', [Current / Max * 100, Extra]);
  Str := StrReplace(Str, '$', IntToStr(FProgressPrecision), [rfReplaceAll]);

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

procedure TCLApplication.EnsureConsoleAtNewLine;
begin
  if GetConsoleXY.X > 0 then
    ConsoleWriteLn;
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

    Task := GetTaskArgument;

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
      HandleException(E);
  end;

  BeforeExit;
end;

function TCLApplication.GetTaskArgument: WideString;
begin
  Result := FCLParser.Arguments[0];    
end;

procedure TCLApplication.HandleException(E: Exception);
var
  Msg: WideString;
begin
  Msg := '';

  if RanOK then
    RanOK := False;   // resets ExitCode so only calling if it's still 0 (success).

  if E is ECLUnknownTask then
  begin
    RanOK := False;

    ConsoleWriteLn( FLang.Format('error: unknown task', [ECLUnknownTask(E).Task]) );
    ConsoleWriteLn;
    OutputHelp;
  end
    else
    begin
      if E is ECLTooFewTaskArgs then
        with ECLTooFewTaskArgs(E) do
          Msg := FLang.Format('error: no task argument', [Task, RequiredCount])
      else if E is ECLNoRequiredTaskOption then
        with ECLNoRequiredTaskOption(E) do
          Msg := FLang.Format('error: no task option', [Task, Option])
      else if E is ECLText then
        Msg := E.Message
      else
        Msg := FLang.Format('exception', [E.ClassName, E.Message, DWord(ExceptAddr)]);

      Msg := Msg + F_EOLN;
    end;

  if Msg <> '' then
    ConsoleWriteLn(Msg);
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

  FShowExitCodeOnExit := FCLParser.IsSwitchOn('show-exit-code', True);
  FShowUtf8ConsoleWarning := FCLParser.IsSwitchOn('utf8-warning', True);
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
  FLastTask := Task;

  if not TryDoingTask(Task) then
    raise ECLUnknownTask.Create(Task);
end;

procedure TCLApplication.BeforeExit;
begin
  if FShowExitCodeOnExit and (FExitCode <> ClExitCodeForHelp) and
     ((FExitCode <> 0 {Success}) or (FWaitOnExit = clAlwaysWait)) then
    OutputExitCode(FExitCode);

  if ((FWaitOnExit = clAlwaysWait) or ((FWaitOnExit = clWaitOnError) and not RanOK)) and IsConsolehandle(StdOut) then
  begin
    ConsoleWriteLn;
    ConsoleWrite(FLang['wait on exit']);
    ConsoleWaitForEnter;
  end;
end;

procedure TCLApplication.OutputExitCode(Code: Integer);
begin
  ConsoleWrite( FLang.Format('code on exit', [FExitCode]) );
end;

procedure TCLApplication.OutputVersion;
var
  Ver: WideString;
begin
  if Version = 0 then
    Ver := FormatDate
    else
      Ver := FormatVersion;

  ConsoleWrite( FLang.Format('version', [Name, Ver]) );
end;

function TCLApplication.GetTaskAlias(Alias: WideString): WideString;
begin
  Result := FTaskAliases.Values[Alias];
end;

procedure TCLApplication.SetTaskAlias(ALias: WideString; const Value: WideString);
begin
  FTaskAliases.Values[Alias] := Value;
end;

procedure TCLApplication.SetEOLN(const Value: WideString);
begin
  F_EOLN := Value;
  FLang.EOLN := Value;
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
  Result := FCLParser.Arguments[Index + 1];   // argument #0 is the task name itself.
  if Result = FCLParser.NotPassed then
    raise ECLTooFewTaskArgs.Create(Task, Index + 1);
end;

{ TCLColorApplication }

procedure TCLColorApplication.Init;
begin
  inherited;

  FInitialConsoleCP := GetConsoleOutputCP;

  FCCParsing := CCParsing;
  FCCWriting := CCWriting;

  FCCWriting.VarGetter := TCCVarList.Create(['SP', ' ']);
  (FCCWriting.VarGetter as TCCVarList).OnUndefined := (CCWriting.VarGetter as TCCVarList).FindUndefined;
end;

function TCLColorApplication.CreateLang: TCLAppLang;
begin
  Result := inherited CreateLang;

  Result['header 1'] := '{w@b {<{wi@bi  %s%s }%s%s {80}}}';
  Result['header 2'] := '{>{y %s}}';
  Result['author'] := 'by {yi %s}';

  Result['exception'] := '{@r -{80}}' +
                         '{@r  {wi@ri  Oops! An {ri  %s } exception has occured. Here''s what it says: {79}} }' +
                         '{@r -{80}}' +
                         '{wi %s}{NL}' +
                         '{wi@r Exception address: {wi@ri %.8X}{NL}}';;
  Result['error: unknown task'] := '{ri >} Unknown task {ri %s} specified.';
  Result['error: no task argument'] := '{ri >} {wi %s} task requires at least {ri %d} {wi arguments}.';
  Result['error: no task option'] := '{ri >} {wi %s} task requires {ri %s} option.';

  Result['code on exit'] := '{%s Exiting with code {i%0:s %d}.}';
  Result['wait on exit'] := '{i ______________________}{NL}Press {wi Enter} to exit...';
  Result['progress'] := '  [ {wi %.$f%%} ]... {wi %s}';
  Result['version'] := '{yi %s}  {y v%s}';
  Result['utf-8 message'] := '{yi Note:} above line contained {wi UTF-8 characters}; if' +
                             ' they are not properly displayed{NL}      try changing {wi Font} in the' +
                             ' {wi Properties} dialog of the program''s {wi EXE file}.';

  { New strings: }
  Result['wrong --console-cp'] := 'Invalid {ri --console-cp} value {wi %s}: must be either' +
                                  ' {wi numeric}, {gi auto} ({wi blank}) or {gi utf8}.';
end;

destructor TCLColorApplication.Destroy;
begin
  if IsConsoleHandle then
    SetConsoleOutputCP(FInitialConsoleCP);

  FCCWriting.VarGetter.Free;
  inherited;
end;

procedure TCLColorApplication.HandleException(E: Exception);
begin
  if not (E is ECLText) then
    E.Message := CCQuote(E.Message);
  inherited HandleException(E);
end;

procedure TCLColorApplication.WriteStringTo(Handle: DWord; Str: WideString);
var
  WritingOpt: TCCWritingOptions;
begin
  WritingOpt := FCCWriting;
  WritingOpt.Handle := Handle;

  Str := NormalizeWritingStr(Str);
  WriteColored(Str, FCCParsing, FCCWriting);
end;

procedure TCLColorApplication.OutputHeader;
var
  Lines: TWideStringArray;
  S: WideString;
begin
  Lines := GetHeader;

  if IsConsoleHandle then
    S := Join(Lines, '')
    else
      S := Join(Lines, F_EOLN);

  ConsoleWriteLn(S);
end;

procedure TCLColorApplication.OutputExitCode(Code: Integer);
const
  ExitColors: array[Boolean] of WideChar = ('r', 'g');
begin
  ConsoleWrite( FLang.Format('code on exit', [ExitColors[FExitCode = 0], FExitCode]) );
end;

procedure TCLColorApplication.AcquireOptions;
var
  CP: WideString;
  IntCP: Integer;
begin
  inherited;

  FCCWriting.Colors := FCLParser.IsSwitchOn('colors', True);

  CP := LowerCase( FCLParser.GetOrDefault('console-cp', IntToStr(GetConsoleOutputCP)) );
  if (CP = '') or (CP = 'auto') then
    FCCWriting.Codepage := GetConsoleOutputCP
    else if Copy(CP, 1, 3) = 'utf' then
      FCCWriting.Codepage := CP_UTF8
      else if TryStrToInt(CP, IntCP) then
        FCCWriting.Codepage := IntCP
        else
          ErrorWriteLn( FLang.Format('wrong --console-cp', [CP]) );
end;

function TCLColorApplication.CCQuote(const Str: WideString): WideString;
begin
  Result := ColorConsole.CCQuote(Str, FCCParsing);
end;

initialization
  StdError := GetStdHandle(STD_ERROR_HANDLE);
  StdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  StdIn := GetStdHandle(STD_INPUT_HANDLE);
end.
