unit ColorConsole;

{ ColorConsole unit - part of D7X library :: by Proger_XP
  Public domain :: http://proger.i-forge.net/ColorConsole  }

interface

uses Contnrs, Classes, SysUtils, Windows, StringsW, StringUtils;

type
  TCCPartClass = class of TCCPart;
  TCCRootParts = class;
  TCCPostWriters = class;
  TCCVarGetter = class;
  TCCVarList = class;

  TCCWriter = procedure (Str: WideString; var Context {TCCWriting}) of object;
  TCCOnUndefinedVar = function (Vars: TCCVarList; Name: WideString): WideString of object;

  PCCParsingOptions = ^TCCParsingOptions;
  TCCParsingOptions = record
    Custom: Pointer;              // can be called by the caller to store arbitrary data

    Opener, Closer: WideChar;     // { and }
    Parts: array of TCCPartClass;

    { Settings for standard part classes (in Parts array) }
    OpenerDelimiter: WideChar;    // space, e.g. in expression "{ri "
    AlignCenter: WideChar;        // <
    AlignRight: WideChar;         // >
    BGSeparator: WideChar;        // @
    Repeater: WideChar;           // x; if #0 - needs none (e.g. "{55}")
    AbsFill: WideChar;            // #0
    HexChar: WideChar;            // # (e.g. "{#2592}"
    NextHexChar: WideChar;        // # (e.g. "{#AD#50C}"
    Variable: WideChar;           // #0 (e.g. "{var_name}")
  end;

  TCCParsing = record
    Str: WideString;
    Pos: Integer;
    Options: TCCParsingOptions;
  end;

  PCCWritingOptions = ^TCCWritingOptions;
  TCCWritingOptions = record
    Custom: Pointer;              // can be called by the caller to store arbitrary data
    Handle: THandle;

    // if this is True and Handle = StdOut and Writer is the default CC writer then
    // SetConsoleOutputCP(CP_UTF8) is called; also, it this is True all input is converted to
    // UTF-8 when writing to Handle; if this is False input is converted into current codepage.
    UTF8: Boolean;
    Writer: TCCWriter;
    VarGetter: TCCVarGetter;      // are not freed automatically; by default contains "NL" and "TAB".
  end;

  TCCWriting = record
    Original: WideString;
    Coord: TCoord;
    LastPartIndex: Integer;
    PostWriters: TCCPostWriters;
    Options: TCCWritingOptions;
  end;

  TCCPostWriter = procedure (var Context: TCCWriting; Start: TCoord) of object;

  EColorConsole = class (Exception);

    ECCVarList = class (EColorConsole);

    ECCParsing = class (EColorConsole)
      constructor Create(const Context: TCCParsing; const Msg: WideString; Fmt: array of const);
    end;

    ECCWriting = class (EColorConsole);

  TCCPostWriters = class
  protected
    FList: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Func: TCCPostWriter);
    procedure Remove(Func: TCCPostWriter);
    procedure Call(var Context: TCCWriting; Start: TCoord);
  end;

  TCCVarGetter = class
  protected
    FCustom: Pointer;

    function Get(Name: WideString): WideString; virtual; abstract;
  public
    property Variables[Name: WideString]: WideString read Get; default;
    property Custom: Pointer read FCustom;
  end;

    TCCVarList = class (TCCVarGetter)
    protected
      FList: THash;
      FOnUndefined: TCCOnUndefinedVar;

      procedure Init;

      function Get(Name: WideString): WideString; override;

      function GetCaseSensitive: Boolean;
      function GetDuplicates: TDuplicatesEx;
      procedure SetCaseSensitive(const Value: Boolean);
      procedure SetDuplicates(const Value: TDuplicatesEx);
    public
      constructor Create(Vars: TStringsW); overload;
      constructor Create(Vars: TStrings); overload;
      constructor Create(Vars: array of const); overload;

      destructor Destroy; override;

      property OnUndefined: TCCOnUndefinedVar read FOnUndefined write FOnUndefined;
      // this method is suitable for delegating OnUndefined from one TCCVarList to another.
      function FindUndefined(Vars: TCCVarList; Name: WideString): WideString;

      property Duplicates: TDuplicatesEx read GetDuplicates write SetDuplicates;
      property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    end;

  TCCParsedStr = class
  protected
    FParsingOpt: TCCParsingOptions;
    FWritingOpt: TCCWritingOptions;
    FOriginal: WideString;
    FParts: TCCRootParts;

    class procedure Writer(Str: WideString; var Context);
    class procedure AdvanceCoord(var Coord: TCoord; Str: WideString);

    procedure Parse(const ParsingOpt: TCCParsingOptions);
  public
    class function Quote(const Str: WideString; const ParsingOpt: TCCParsingOptions): WideString;

    constructor Create(const Str: WideString; const ParsingOpt: TCCParsingOptions);
    destructor Destroy; override;

    property ParsingOptions: TCCParsingOptions read FParsingOpt;
    property WritingOptions: TCCWritingOptions read FWritingOpt;
    property Original: WideString read FOriginal;

    procedure Write(const WritingOpt: TCCWritingOptions);
  end;

  TCCPart = class
  protected
    class function GetPrefixedSpec(var Context: TCCParsing;
      Prefix: WideChar; out Inner: WideString; out Pos: Integer): Boolean;
    class function FindSpecEnd(var Context: TCCParsing; out Inner: WideString;
      var Pos: Integer): Boolean;
    class function TryParsingNum(var Context: TCCParsing; Prefix: WideChar;
      out Num: Integer): Boolean;

    procedure Init; virtual;
    procedure WriteStrTo(var Context: TCCWriting; const Str: WideString);
  public
    class function TryParsing(var Context: TCCParsing): TCCPart; virtual;

    // if the part preceding this part is a TCCTextPart with Text longer than
    // Result characters it will be split in two.
    function PrecedingTextLength: Integer; virtual;
    function Splitable: Boolean; virtual;
    function PlainText: WideString; virtual;

    procedure Write(var Context: TCCWriting); virtual;
    // Iterations is 1-based.
    function Repeats(var Context: TCCWriting; Iterations: Integer): Boolean; virtual;
    procedure ExitWrite; virtual;
  end;

    TCCTextPart = class (TCCPart)
    protected
      FText: WideString;
    public
      constructor Create(const Text: WideString);

      property Text: WideString read FText;
      // MaxLength can be negative to split from the end.
      function Split(MaxLength: Integer): TCCTextPart;
      function Splitable: Boolean; override;
      function PlainText: WideString; override;

      procedure Write(var Context: TCCWriting); override;
    end;

      TCCHexCharsPart = class (TCCTextPart)
      protected
        class function HexToStr(var Context: TCCParsing; const Hex: WideString;
          Delim: WideChar): WideString;
      public
        class function TryParsing(var Context: TCCParsing): TCCPart; override;
        function Splitable: Boolean; override;
      end;

    TCCParts = class (TCCPart)
    protected
      FNeedsCloser: Boolean;
      FParts: TObjectList;    // of TCCPart
      FOriginal: WideString;

      procedure Init; override;
      procedure Parse(var Context: TCCParsing); virtual;
    public
      destructor Destroy; override;

      function PlainText: WideString; override;
      procedure Write(var Context: TCCWriting); override;
    end;

      TCCRootParts = class (TCCParts)
      protected
        procedure InitContext(var Context: TCCWriting);
      public
        constructor StartParsing(const Expr: WideString; const Opt: TCCParsingOptions);
        procedure StartWriting(const Opt: TCCWritingOptions);
      end;

      TCCGroupPart = class (TCCParts)
      public
        class function TryParsing(var Context: TCCParsing): TCCPart; override;
        constructor Create(var Context: TCCParsing);
      end;

        TCCColorPart = class (TCCGroupPart)
        protected
          FAttrs: DWord;

          procedure Parse(var Context: TCCParsing); override;
          function ParseColors(const Context: TCCParsing; const Colors, Delim: WideString): DWord;

          procedure PostWrite(var Context: TCCWriting; Start: TCoord);
        public
          class function TryParsing(var Context: TCCParsing): TCCPart; override;
          class function ValidColorFlags: WideString;

          procedure Write(var Context: TCCWriting); override;
        end;

        TCCAlignedPart = class (TCCGroupPart)
        protected
          FAlign: TAlignment;
          FPadChar: WideChar;

          procedure Parse(var Context: TCCParsing); override;
        public
          class function TryParsing(var Context: TCCParsing): TCCPart; override;
          procedure Write(var Context: TCCWriting); override;
        end;

    TCCRepeatingPart = class (TCCPart)
    protected
      procedure EnsureFollowsAnyPart(const Context: TCCWriting);
    public
      function PrecedingTextLength: Integer; override;
    end;

      TCCRepeatPart = class (TCCRepeatingPart)
      protected
        FRepeats: Integer;
      public
        class function TryParsing(var Context: TCCParsing): TCCPart; override;

        constructor Create(Repeats: Integer);

        function PlainText: WideString; override;
        function Repeats(var Context: TCCWriting; Iterations: Integer): Boolean; override;
      end;

      TCCAbsFillPart = class (TCCRepeatingPart)
      protected
        FX, FLastY: Integer;
      public
        class function TryParsing(var Context: TCCParsing): TCCPart; override;

        constructor Create(X: Integer);

        function Repeats(var Context: TCCWriting; Iterations: Integer): Boolean; override;
        procedure ExitWrite; override;
      end;

    TCCVarPart = class (TCCPart)
    protected
      FVar: WideString;
    public
      class function TryParsing(var Context: TCCParsing): TCCPart; override;

      constructor Create(const VarName: WideString);
      procedure Write(var Context: TCCWriting); override;
    end;

var
  CCParsing: TCCParsingOptions;
  CCWriting: TCCWritingOptions;

procedure WriteColored(const Str: Widestring; Cache: Boolean = True); overload;  // uses default options.
procedure WriteColored(const Str: Widestring; const ParsingOpt: TCCParsingOptions;
  const WritingOpt: TCCWritingOptions; Cache: Boolean = True); overload;
function CCQuote(const Str: WideString; const Opt: TCCParsingOptions): WideString;

function ParseCC(const Str: Widestring; const Opt: TCCParsingOptions): TCCParsedStr;
procedure OutputCC(const ParsedStr: TCCParsedStr; const Opt: TCCWritingOptions);
function DefaultCCWriter: TCCWriter;
function DefaultCCVars: TCCVarList;

implementation

const
  LineLen = 80;

var
  CCVars: TCCVarList;
  CCCache: TObjectHash;   // of TCCParsedStr.

procedure WriteColored(const Str: Widestring; Cache: Boolean = True);
begin
  WriteColored(Str, CCParsing, CCWriting);
end;

procedure WriteColored(const Str: Widestring; const ParsingOpt: TCCParsingOptions;
  const WritingOpt: TCCWritingOptions; Cache: Boolean = True);

  function HashOf(const Str: WideString; const Opt: TCCParsingOptions): WideString;
  var
    OptSize: Integer;
  begin
    OptSize := SizeOf(Opt) div 2 + SizeOf(Opt) mod 2;

    SetLength(Result, Length(Str) * 2 + OptSize);
    Move(Opt, Result[1], SizeOf(OptSize));
    Move(Str[1], Result[OptSize + 1], Length(Str) * 2);
  end;

var
  Hash: WideString;
  I: Integer;
begin
  if Length(Str) = 0 then
    Exit;

  if Cache then
  begin
    if CCCache = NIL then
    begin
      CCCache := TObjectHash.Create(True);
      CCCache.CaseSensitive := True;
    end;

    Hash := HashOf(Str, ParsingOpt);
    I := CCCache.IndexOf(Hash);

    if I = -1 then
      I := CCCache.AddObject( Hash, TCCParsedStr.Create(Str, ParsingOpt) );

    (CCCache.Objects[I] as TCCParsedStr).Write(WritingOpt);
  end
    else
      with TCCParsedStr.Create(Str, ParsingOpt) do
        try
          Write(WritingOpt);
        finally
          Free;
        end;
end;

function CCQuote(const Str: WideString; const Opt: TCCParsingOptions): WideString;
begin
  Result := TCCParsedStr.Quote(Str, Opt);
end;

function ParseCC(const Str: Widestring; const Opt: TCCParsingOptions): TCCParsedStr;
begin
  Result := TCCParsedStr.Create(Str, Opt);
end;

procedure OutputCC(const ParsedStr: TCCParsedStr; const Opt: TCCWritingOptions);
begin
  ParsedStr.Write(Opt);
end;

function DefaultCCWriter: TCCWriter;
begin
  Result := TCCParsedStr.Writer;
end;

function DefaultCCVars: TCCVarList;
begin
  Result := CCVars;
end;

{ Exceptions }

constructor ECCParsing.Create(const Context: TCCParsing; const Msg: WideString; Fmt: array of const);
const
  Text = 'ColorConsole expression error: %s; expression: %s; position: %d (on "%s").';
var
  I: Integer;
begin
  for I := 0 to Length(Fmt) - 1 do
    if Fmt[I].VType = vtChar then
    begin
      Fmt[I].VType := vtString;
      Fmt[I].VPChar := PChar( String(Fmt[I].VChar) );
    end
      else if Fmt[I].VType = vtWideChar then
      begin
        Fmt[I].VType := vtWideString;
        Fmt[I].VPWideChar := PWideChar( WideString(Fmt[I].VWideChar) );
      end;

  with Context do
    CreateFmt(Text, [Format(Msg, Fmt), Str, Pos, Copy(Str, Pos, 20)]);
end;

{ TCCVarList }

constructor TCCVarList.Create(Vars: TStringsW);
begin
  Init;
  Vars.Assign(FList);
end;

constructor TCCVarList.Create(Vars: TStrings);
begin
  Init;
  Vars.Assign(FList);
end;

constructor TCCVarList.Create(Vars: array of const);
  function RecToStr(I: Integer): WideString;
  begin
    with Vars[I] do
      case VType of
      vtPChar,
      vtAnsiString: Result := VPChar;
      vtPWideChar,
      vtWideString: Result := VPWideChar;
      vtChar:       Result := VChar;
      vtWideChar:   Result := VWideChar;
      else
        raise ECCVarList.CreateFmt('Invalid Vars[%d].Vtype (%d) for TCCVarList.', [I, VType]);
      end;
  end;

var
  Separ: WideChar;
  I: Integer;
begin
  Init;

  if Length(Vars) mod 2 <> 0 then
    raise ECCVarList.CreateFmt('TCCVarList must be passed an even count of Vars, %d items given.', [Length(Vars)]);

  Separ := FList.NameValueSeparator;
  FList.BeginUpdate;
  try
    for I := 0 to Length(Vars) div 2 - 1 do
      FList.Add(RecToStr(I * 2) + Separ + RecToStr(I * 2 + 1));
  finally
    FList.EndUpdate;
  end;
end;

procedure TCCVarList.Init;
begin
  FList := THash.Create;

  FList.Duplicates := dupError;
  FList.CaseSensitive := True;
  FList.Sorted := True;

  FList.NameValueSeparator := #0;
end;

destructor TCCVarList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TCCVarList.Get(Name: WideString): WideString;
const
  Error = 'Undefined ColorConsole expression variable "%s"; defined are: %s.';
var
  I: Integer;
begin
  I := FList.IndexOfName(Name);
  if I = -1 then
    if Assigned(FOnUndefined) then
      Result := FOnUndefined(Self, Name)
      else
        raise ECCVarList.CreateFmt(Error, [Name, FList.JoinNames(', ')])
    else
      Result := FList.ValueFromIndex[I];
end;

function TCCVarList.GetCaseSensitive: Boolean;
begin
  Result := FList.CaseSensitive;
end;

function TCCVarList.GetDuplicates: TDuplicatesEx;
begin
  Result := FList.Duplicates;
end;

procedure TCCVarList.SetCaseSensitive(const Value: Boolean);
begin
  FList.CaseSensitive := Value;
end;

procedure TCCVarList.SetDuplicates(const Value: TDuplicatesEx);
begin
  FList.Duplicates := Value;
end;

function TCCVarList.FindUndefined(Vars: TCCVarList; Name: WideString): WideString;
begin
  Result := Variables[Name];
end;

{ TCCPostWriters }

type
  TPWWrapper = class
    Func: TCCPostWriter;
  end;

constructor TCCPostWriters.Create;
begin
  FList := TObjectList.Create(True);
end;

destructor TCCPostWriters.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TCCPostWriters.Add(Func: TCCPostWriter);
var
  Wrapper: TPWWrapper;
begin
  Wrapper := TPWWrapper.Create;
  Wrapper.Func := Func;
  FList.Add(Wrapper);
end;

procedure TCCPostWriters.Remove(Func: TCCPostWriter);
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
    if @(FList[I] as TPWWrapper).Func = @Func then
    begin
      FList.Delete(I);
      Break;
    end;
end;

procedure TCCPostWriters.Call(var Context: TCCWriting; Start: TCoord);
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    (FList[I] as TPWWrapper).Func(Context, Start);
end;

{ TCCParsedStr }

class procedure TCCParsedStr.Writer(Str: WideString; var Context);
var
  Written: DWord;
  Ansi: String;
  Start: TCoord;
begin
  with TCCWriting(Context) do
  begin
    if Options.UTF8 then
      Ansi := UTF8Encode(Str)
      else
        Ansi := Str;

    WriteFile(Options.Handle, Ansi[1], Length(Ansi), Written, NIL);

    Start := Coord;
    AdvanceCoord(Coord, Str);
    PostWriters.Call(TCCWriting(Context), Start);
  end;
end;

class procedure TCCParsedStr.AdvanceCoord(var Coord: TCoord; Str: WideString);
const
  CR = #13;
  LF = #10;
var
  I: Integer;

  procedure FlushX;
  begin
    if Coord.X >= LineLen then
    begin
      Inc(Coord.Y, Coord.X div LineLen);
      Coord.X := Coord.X mod LineLen;
    end;
  end;

begin
  Str := StrReplace(Str, CR + LF, LF, [rfReplaceAll]);

  for I := 1 to Length(Str) do
    if (Str[I] = LF) or (Str[I] = CR) then
    begin
      FlushX;
      Coord.X := 0;
      Inc(Coord.Y);
    end
      else
        Inc(Coord.X);

  FlushX;
end;

class function TCCParsedStr.Quote(const Str: WideString; const ParsingOpt: TCCParsingOptions): WideString;
var
  I: Integer;
begin
  Result := Str;

  with ParsingOpt do
    for I := Length(Result) downto 1 do
      if (Result[I] = Opener) or (Result[I] = Closer) then
        Insert(Opener, Result, I);
end;

constructor TCCParsedStr.Create(const Str: WideString; const ParsingOpt: TCCParsingOptions);
begin
  if Length(Str) = 0 then
    raise EColorConsole.Create('Empty string to output using ColorConsole.');

  FOriginal := Str;
  FParts := NIL;

  Parse(ParsingOpt);
end;

destructor TCCParsedStr.Destroy;
begin
  if FParts <> NIL then
    FParts.Free;

  inherited;
end;

procedure TCCParsedStr.Parse(const ParsingOpt: TCCParsingOptions);
begin
  if FParts <> NIL then
    FreeAndNIL(FParts);

  FParsingOpt := ParsingOpt;
  FParts := TCCRootParts.StartParsing(FOriginal, ParsingOpt);
end;

procedure TCCParsedStr.Write(const WritingOpt: TCCWritingOptions);
begin
  FWritingOpt := WritingOpt;

  if WritingOpt.UTF8 and (WritingOpt.Handle = GetStdHandle(STD_OUTPUT_HANDLE)) and
     not SetConsoleOutputCP(CP_UTF8) then
    RaiseLastOSError;

  FParts.StartWriting(WritingOpt);
end;

{ TCCPart }

class function TCCPart.TryParsing(var Context: TCCParsing): TCCPart;
begin
  raise ECCParsing.Create(Context, '%s can''t be used as a part class', [ClassName]);
end;

class function TCCPart.GetPrefixedSpec(var Context: TCCParsing; Prefix: WideChar;
  out Inner: WideString; out Pos: Integer): Boolean;
begin
  Result := False;

  Pos := Context.Pos;
  if (Pos <= Length(Context.Str)) and ((Prefix = #0) or (Context.Str[Pos] = Prefix)) then
  begin
    if Prefix <> #0 then
      Inc(Pos);

    if FindSpecEnd(Context, Inner, Pos) then
    begin
      if Prefix <> #0 then
        Delete(Inner, 1, 1);
      Result := True;
    end;
  end;
end;

class function TCCPart.TryParsingNum(var Context: TCCParsing; Prefix: WideChar;
  out Num: Integer): Boolean;
var
  Inner: WideString;
  Pos: Integer;
begin
  Result := GetPrefixedSpec(Context, Prefix, Inner, Pos) and TryStrToIntStrict(Inner, Num, +1);
  if Result then
    Context.Pos := Pos;
end;

class function TCCPart.FindSpecEnd(var Context: TCCParsing; out Inner: WideString;
  var Pos: Integer): Boolean;
begin
  Pos := PosW(Context.Options.Closer, Context.Str, Pos);
  Result := Pos > 0;

  if Result then
  begin
    Inner := Copy(Context.Str, Context.Pos, Pos - Context.Pos);
    Inc(Pos);
  end;
end;

procedure TCCPart.Init;
begin
  ExitWrite;
end;

function TCCPart.PrecedingTextLength: Integer;
begin
  Result := MaxInt;
end;

function TCCPart.Splitable: Boolean;
begin
  Result := False;
end;

procedure TCCPart.Write(var Context: TCCWriting);
begin
end;

procedure TCCPart.WriteStrTo(var Context: TCCWriting; const Str: WideString);
var
  Func: TCCWriter;
begin
  if Str <> '' then
  begin
    Func := Context.Options.Writer;
    if not Assigned(Func) then
      raise ECCWriting.CreateFmt('CC-Writer callback is not assigned; writing string "%s".', [Str]);

    Func(Str, Context);
  end;
end;

function TCCPart.Repeats(var Context: TCCWriting; Iterations: Integer): Boolean;
begin
  Result := Iterations = 0;
end;

procedure TCCPart.ExitWrite;
begin
end;

function TCCPart.PlainText: WideString;
begin
  Result := '';
end;

{ TCCTextPart }

constructor TCCTextPart.Create(const Text: WideString);
begin
  Init;
  FText := Text;
end;

function TCCTextPart.PlainText: WideString;
begin
  Result := FText;
end;

function TCCTextPart.Split(MaxLength: Integer): TCCTextPart;
type
  TCCTextPartClass = class of TCCTextPart;
begin
  if MaxLength < 0 then
    Inc(MaxLength, Length(FText));

  Result := TCCTextPartClass(Self.ClassType).Create( Copy(FText, MaxLength + 1, Length(FText)) );
  FText := Copy(FText, 1, MaxLength);
end;

function TCCTextPart.Splitable: Boolean;
begin
  Result := True;
end;

procedure TCCTextPart.Write(var Context: TCCWriting);
begin
  WriteStrTo(Context, FText);
end;

{ TCCParts }

procedure TCCParts.Write(var Context: TCCWriting);
  procedure WritePart(PartI: Integer);
  var
    Iterations: Integer;
    Part: TCCPart;
  begin
    Iterations := 0;
    Part := FParts[PartI] as TCCPart;

    while True do
    begin
      Context.LastPartIndex := PartI;
      if not Part.Repeats(Context, Iterations) then
        Break;

      if Part.InheritsFrom(TCCRepeatingPart) and (PartI > 0) then
        WritePart(PartI - 1);
      Part.Write(Context);

      Inc(Iterations);
    end;

    Part.ExitWrite;
  end;

var
  I: Integer;
begin
  if FParts.Count > 0 then
  begin
    for I := 0 to FParts.Count - 2 do
      if not FParts[I + 1].InheritsFrom(TCCRepeatingPart) then
        WritePart(I);

    WritePart(FParts.Count - 1);
  end;
end;

procedure TCCParts.Init;
begin
  inherited;

  FNeedsCloser := True;
  FParts := TObjectList.Create(True);
end;

destructor TCCParts.Destroy;
begin
  FParts.Free;
  inherited;
end;

procedure TCCParts.Parse(var Context: TCCParsing);
var
  Plain: WideString;

  function Ch(Delta: Integer = 0): WideChar;
  begin
    Result := Context.Str[Context.Pos + Delta];
  end;

  procedure FlushPlain;
  begin
    if Plain <> '' then
    begin
      FParts.Add(TCCTextPart.Create(Plain));
      Plain := '';
    end;
  end;

  function QuotedOpening: Boolean;
  begin
    Result := True;

    with Context, Options do
      if Pos >= Length(Str) then
        Plain := Plain + Opener
        else if (Str[Pos + 1] = Opener) or (Str[Pos + 1] = Closer) then
          Plain := Plain + Str[Pos + 1]
          else
            Result := False;
  end;

  procedure SplitLastTextPartIf(MaxLength: Integer);
  begin
    if FParts.Count > 0 then
      with TCCTextPart( FParts[FParts.Count - 1] ) do
        if Splitable and (Length(Text) > MaxLength) then
          FParts.Add( Split(-1 * MaxLength) );
  end;

  procedure HandleSpec;
  var
    Part: TCCPart;
    I: Integer;
  begin
    Inc(Context.Pos);
    FlushPlain;

    with Context.Options do
      for I := 0 to Length(Parts) - 1 do
      begin
        Part := Context.Options.Parts[I].TryParsing(Context);
        if Part <> NIL then
        begin
          SplitLastTextPartIf(Part.PrecedingTextLength);
          FParts.Add(Part);

          Exit;
        end;
      end;

    with Context.Options do
      raise ECCParsing.Create(Context, 'invalid format of %sspec%s - might be an unescaped spec', [Opener, Closer]);
  end;

var
  StartPos: Integer;
begin
  StartPos := Context.Pos;

  while True do
    if Context.Pos > Length(Context.Str) then
      if FNeedsCloser then
        raise ECCParsing.Create(Context, 'unclosed %sspec%s (opened at %d)', [Context.Options.Opener, Context.Options.Closer, StartPos])
        else
          Break
    else if Ch = Context.Options.Closer then
      if FNeedsCloser then
      begin
        Inc(Context.Pos);
        Break;
      end
        else
          raise ECCParsing.Create(Context, 'unescaped "%s" while not in Context', [Context.Options.Closer])
    else if Ch = Context.Options.Opener then
      if QuotedOpening then
        Inc(Context.Pos, 2)
        else
          HandleSpec
    else
    begin
      Plain := Plain + Ch;
      Inc(Context.Pos);
    end;

  FlushPlain;
  FOriginal := Copy(Context.Str, StartPos, Context.Pos - StartPos - Byte(FNeedsCloser));
end;

function TCCParts.PlainText: WideString;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FParts.Count - 1 do
    Result := Result + (FParts[I] as TCCPart).PlainText;
end;

{ TCCRootParts }

constructor TCCRootParts.StartParsing(const Expr: WideString; const Opt: TCCParsingOptions);
var
  Context: TCCParsing;
begin
  Init;
  FNeedsCloser := False;

  ZeroMemory(@Context, SizeOf(Context));
  Context.Str := Expr;
  Context.Pos := 1;
  Context.Options := Opt;

  Parse(Context);
end;

procedure TCCRootParts.StartWriting(const Opt: TCCWritingOptions);
var
  Context: TCCWriting;
begin
  ZeroMemory(@Context, SizeOf(Context));
  Context.Options := Opt;
  Context.Original := FOriginal;

  InitContext(Context);

  Context.PostWriters := TCCPostWriters.Create;
  try
    Write(Context);
  finally
    FreeAndNIL(Context.PostWriters);
  end;
end;

procedure TCCRootParts.InitContext(var Context: TCCWriting);
var
  Info: TConsoleScreenBufferInfo;
begin
  if GetConsoleScreenBufferInfo(Context.Options.Handle, Info) then
    Context.Coord := Info.dwCursorPosition;
end;

{ TCCGroupPart }

class function TCCGroupPart.TryParsing(var Context: TCCParsing): TCCPart;
begin
  with Context do
    if Str[Pos] = Options.OpenerDelimiter then
    begin
      Inc(Pos);
      Result := Self.Create(Context);
    end
      else
        Result := NIL;
end;

constructor TCCGroupPart.Create(var Context: TCCParsing);
begin
  Init;
  Parse(Context);
end;

{ TCCColorPart }

class function TCCColorPart.TryParsing(var Context: TCCParsing): TCCPart;
var
  Colors, Rest: WideString;
begin
  with Context do
    if Split(Copy(Str, Pos, Length(Str)), Options.OpenerDelimiter, Colors, Rest) and
       (TrimLeft(Colors, Context.Options.BGSeparator + ValidColorFlags) = '') then
      Result := Self.Create(Context)
      else
        Result := NIL;
end;

class function TCCColorPart.ValidColorFlags: WideString;
begin
  Result := 'rgbiwymc';
end;

procedure TCCColorPart.Parse(var Context: TCCParsing);
var
  EndPos: Integer;
  ColorStr: WideString;
begin
  EndPos := PosW(Context.Options.OpenerDelimiter, Context.Str, Context.Pos);
  if EndPos = -1 then
    with Context.Options do
      raise ECCParsing.Create(Context, 'color %sspec%s contains no "%s"-separator',
                              [Opener, Closer, OpenerDelimiter]);

  ColorStr := Copy(Context.Str, Context.Pos, EndPos - Context.Pos);
  FAttrs := ParseColors(Context, ColorStr, Context.Options.BGSeparator);
  Context.Pos := EndPos + 1;

  inherited;
end;

function TCCColorPart.ParseColors(const Context: TCCParsing; const Colors, Delim: WideString): DWord;
  function ToAttrs(const S: WideString): DWord;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 1 to Length(S) do
      case Char(S[I]) of
      'r':  Result := Result + FOREGROUND_RED;
      'g':  Result := Result + FOREGROUND_GREEN;
      'b':  Result := Result + FOREGROUND_BLUE;
      'i':  Result := Result + FOREGROUND_INTENSITY;
      'w':  Result := Result + FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE;
      'y':  Result := Result + FOREGROUND_RED or FOREGROUND_GREEN;
      'm':  Result := Result + FOREGROUND_RED or FOREGROUND_BLUE;
      'c':  Result := Result + FOREGROUND_GREEN or FOREGROUND_BLUE;
      else
        raise ECCParsing.Create(Context, 'unrecognized color flag "%s", known are "%s"', [S[I], ValidColorFlags]);
      end;
  end;

var
  FG, BG: WideString;
begin
  Split(LowerCase(Colors), Delim, FG, BG);
  Result := ToAttrs(FG) + (ToAttrs(BG) shl 4);
end;

procedure TCCColorPart.Write(var Context: TCCWriting);
begin
  Context.PostWriters.Add(PostWrite);
  try
    inherited;
  finally
    Context.PostWriters.Remove(PostWrite);
  end;
end;

procedure TCCColorPart.PostWrite(var Context: TCCWriting; Start: TCoord);
var
  Len: Integer;
  Written: DWord;
begin
  Len := (Context.Coord.Y - Start.Y) * LineLen + (Context.Coord.X - Start.X);
  if Len > 0 then
    FillConsoleOutputAttribute(Context.Options.Handle, FAttrs, Len, Start, Written);
end;

{ TCCAlignedPart }

class function TCCAlignedPart.TryParsing(var Context: TCCParsing): TCCPart;
begin
  with Context do
    if (Str[Pos] = Options.AlignCenter) or (Str[Pos] = Options.AlignRight) then
      Result := Self.Create(Context)
      else
        Result := NIL;
end;

procedure TCCAlignedPart.Parse(var Context: TCCParsing);
begin
  if Context.Str[Context.Pos] = Context.Options.AlignCenter then
    FAlign := taCenter
    else
      FAlign := taRightJustify;

  Inc(Context.Pos);

  if Copy(Context.Str, Context.Pos + 1, 1) = Context.Options.OpenerDelimiter then
  begin
    FPadChar := Context.Str[Context.Pos];
    Inc(Context.Pos, 2);
  end
    else
      FPadChar := ' ';

  inherited;
end;

procedure TCCAlignedPart.Write(var Context: TCCWriting);
var
  Padding: WideString;
begin
  if FAlign = taCenter then
    Padding := StrRepeat(FPadChar, (LineLen - Length(PlainText)) div 2)
    else
      Padding := StrRepeat(FPadChar, LineLen - Length(PlainText));

  WriteStrTo(Context, Padding);

  inherited;
end;

{ TCCRepeatingPart }

procedure TCCRepeatingPart.EnsureFollowsAnyPart(const Context: TCCWriting);
begin
  if Context.LastPartIndex < 1 then
    raise ECCWriting.CreateFmt('%s must follow an item to be repeated but none exists.',
                               [Copy(ClassName, 4, $FFFF)]);
end;

function TCCRepeatingPart.PrecedingTextLength: Integer;
begin
  Result := 1;
end;

{ TCCRepeatPart }

class function TCCRepeatPart.TryParsing(var Context: TCCParsing): TCCPart;
var
  Count: Integer;
begin
  if TryParsingNum(Context, Context.Options.Repeater, Count) then
    Result := Self.Create(Count)
    else
      Result := NIL;
end;

constructor TCCRepeatPart.Create(Repeats: Integer);
begin
  Init;
  FRepeats := Repeats;
end;

function TCCRepeatPart.Repeats(var Context: TCCWriting; Iterations: Integer): Boolean;
begin
  EnsureFollowsAnyPart(Context);
  Result := FRepeats > Iterations;
end;

function TCCRepeatPart.PlainText: WideString;
begin
  Result := StrRepeat(' ', FRepeats);
end;

{ TCCAbsFillPart }

class function TCCAbsFillPart.TryParsing(var Context: TCCParsing): TCCPart;
var
  X: Integer;
begin
  if TryParsingNum(Context, Context.Options.AbsFill, X) then
    Result := Self.Create(X)
    else
      Result := NIL;
end;

constructor TCCAbsFillPart.Create(X: Integer);
begin
  Init;
  FX := X;
end;

function TCCAbsFillPart.Repeats(var Context: TCCWriting; Iterations: Integer): Boolean;
begin
  EnsureFollowsAnyPart(Context);

  if FLastY = -1 then
    FLastY := Context.Coord.Y;

  Result := (FLastY = Context.Coord.Y) and (FX > Context.Coord.X);
end;

procedure TCCAbsFillPart.ExitWrite;
begin
  FLastY := -1;
end;

{ TCCHexCharPart }

class function TCCHexCharsPart.TryParsing(var Context: TCCParsing): TCCPart;
var
  Hex: WideString;
  Pos: Integer;
begin
  if GetPrefixedSpec(Context, Context.Options.HexChar, Hex, Pos) then
  begin
    Result := Self.Create( HexToStr(Context, Hex, Context.Options.NextHexChar) );
    Context.Pos := Pos;
  end
    else
      Result := NIL;
end;

class function TCCHexCharsPart.HexToStr(var Context: TCCParsing; const Hex: WideString;
  Delim: WideChar): WideString;
const
  HexChars  = '0123456789abcdefABCDEF';
  LongCode  = 'hex char code "%s" in %s%sspec%s "%s" is too long (%d) - max length is %d for Unicode symbol';
  WrongChar = 'hex char %s%sspec%s contains a non-hex digit in "%s" part of "%s"';
var
  Hexes: TWideStringArray;
  I: Integer;
  Buf: String;
begin
  Hexes := Explode(Delim, Hex);

  for I := 0 to Length(Hexes) - 1 do
  begin
    if Length(Hexes[I]) > 4 then
      with Context, Options do
        raise ECCParsing.Create(Context, LongCode, [ Hexes[I], Opener, HexChar, Closer, Hex, Length(Hexes[I]), 4 ]);

    Hexes[I] := StrPadLeft(Hexes[I], 4, '0');
    Hexes[I] := Copy(Hexes[I], 3, 2) + Copy(Hexes[I], 1, 2);

    if TrimLeft(Hexes[I], HexChars) <> '' then
      with Context, Options do
        raise ECCParsing.Create(Context, WrongChar, [Opener, HexChar, Closer, Hexes[I], Hex]);
  end;

  Buf := HexToBin(Join(Hexes, ''));

  SetLength(Result, Length(Buf) div 2);
  Move(Buf[1], Result[1], Length(Buf));
end;

function TCCHexCharsPart.Splitable: Boolean;
begin
  Result := False;
end;

{ TCCVarPart }

class function TCCVarPart.TryParsing(var Context: TCCParsing): TCCPart;
var
  VarName: WideString;
  Pos, Num: Integer;
begin
  if GetPrefixedSpec(Context, Context.Options.Variable, VarName, Pos) and not TryStrToInt(VarName, Num) then
  begin
    Result := Self.Create(VarName);
    Context.Pos := Pos;
  end
    else
      Result := NIL;
end;

constructor TCCVarPart.Create(const VarName: WideString);
begin
  Init;
  FVar := VarName;
end;

procedure TCCVarPart.Write(var Context: TCCWriting);
begin
  if Context.Options.VarGetter = NIL then
    raise ECCWriting.CreateFmt('ColorConsole expression "%s" was not assigned any variables.', [Context.Original]);

  WriteStrTo(Context, Context.Options.VarGetter[FVar]);
end;

initialization
  CCVars := TCCVarList.Create(['NL', #13#10, 'TAB', #9]);

  with CCParsing do
  begin
    Custom := NIL;

    Opener := '{';
    Closer := '}';

    OpenerDelimiter := ' ';
    BGSeparator := '@';
    AlignCenter := '<';
    AlignRight := '>';
    Repeater := 'x';
    AbsFill := #0;
    HexChar := '#';
    NextHexChar := '#';
    Variable := #0;

    SetLength(Parts, 7);
    Parts[0] := TCCGroupPart;
    Parts[1] := TCCColorPart;
    Parts[2] := TCCAlignedPart;
    Parts[3] := TCCRepeatPart;
    Parts[4] := TCCAbsFillPart;
    Parts[5] := TCCHexCharsPart;
    Parts[6] := TCCVarPart;
  end;

  with CCWriting do
  begin
    Custom := NIL;
    Handle := GetStdHandle(STD_OUTPUT_HANDLE);
    UTF8 := True;
    Writer := DefaultCCWriter();
    VarGetter := DefaultCCVars;
  end;

finalization
  if CCCache <> NIL then
    CCCache.Free;
end.
