unit StringUtils;

interface

uses SysUtils, Windows;

type
  TStringArray = array of String;
  TWideStringArray = array of WideString;

  // return True to exit the loop.
  TCallOnEachLineInCallback = function (Line: WideString; Data: Pointer): Boolean;
  TCallOnEachLineInCallback_OO = function (Line: WideString; Data: DWord): Boolean of object;

  TMaskMatchInfo = record
    Matched: Boolean;
    StrPos: Word;
    MatchLength: Word;
  end;

  TReplaceFlags = SysUtils.TReplaceFlags;

  TGenericFormatLanguage = record
    { Note: WideFormat is buggy when working with %f specifier. It won't format it correctly
            (but Format does) so NumberFormat is used in Format which is then passed to
            WideFormat as a string. See GenericFormat for the exact mechanism. }
    // "basic" is a number that is smaller than the smallest grade, eg. 500 is a basic
    //   number of bytes since it's less than 1 KiB. And so is 1023.
    BasicNumberFormat: String;
    NumberFormat: String;
    ResultFormat: WideString;
    Measures: array of record
      Capacity: DWord;
      Measure:  WideString;
    end;
  end;
  
  TCallOnEachLineOptions_OO = record
    Callback: TCallOnEachLineInCallback_OO;
    UserData: DWord;
    EOLN: WideString;
  end;

// todo: make Lower/UpperCase wrapper for Wide*Case.
// todo: reconvertion table for Lower/UpperCase?
// todo: use StringTypeW in RemoveNonWordChars?

// unlike TryStrToInt this considers strings with leading/trailing spaces as invalid.
function TryStrToIntStrict(const S: String; out Value: Integer): Boolean;
function DetectEolnStyleIn(Str: WideString): WideString;

// not more than 65536 resulting pieces are supported.
function Explode(Delimiter, Str: WideString; Count: Integer = 0; SkipEmpty: Boolean = False): TWideStringArray;
// removes EscapeChars from First; returns False, sets First to Str and Second to '' if Str contained no Splitter.
function Split(Str: WideString; Splitter: WideString; var First, Second: WideString;
  const Escaper: WideString = ''): Boolean;
function CharsOfString(const Str: WideString): TWideStringArray;
function Join(WSArray: array of WideString; Glue: WideString = ', '): WideString;

{ Standard SysUtils' string functions; I suggest avoid using them because they're strangely
  written IMHO (e.g. using PChar here and there) - use alternatives below. }
function QuotedStr(const S: WideString; Quote: WideChar): WideString;
function ExtractQuotedStr(var Src: PWideChar; Quote: WideChar): WideString;
function DequotedStr(const S: WideString; AQuote: WideChar): WideString;
function StrEndW(const Str: PWideChar): PWideChar;
function StrScanW(Str: PWideChar; Chr: WideChar): PWideChar;
                       
  { Unlike AnsiQuotedStr, this function won't surround result with CharToQuote. }
  function Quote(const Str, CharToQuote: WideString; StartAt: Integer = 1): WideString;     
  { Unlike AnsiExtractQuotedStr, Str doesn't need to start with QuoteChar; however, it must
    end with it (EndPos will be set AFTER its position). If there was no ending QuoteChar
    found EndPos is set to -1 and Result is undefined. If quote char was last char in Str
    EndPos will be set to Length(Str) + 1. }
  function Unquote(const Str, QuoteChar: WideString; out EndPos: Integer; StartAt: Integer): WideString; overload;
  // will raise EConvertError if QuoteChar was not found in Str (i.e. if EndPos was set to -1).
  function Unquote(const Str, QuoteChar: WideString; StartAt: Integer = 1): WideString; overload;

  function PascalQuote(const Str: WideString): WideString;
  // sets Pos to -1 if string ended properly on an apostrophe ('), otherwise Pos is set
  // after the closing apostrophe if it was found earlier or past the end of Str if there was none.
  function PascalUnquote(const Str: WideString; var Pos: Integer): WideString; overload;
  function PascalUnquote(Str: WideString; MustStartWithQuote: Boolean = True): WideString; overload;

function IsDelimiter(const Delimiters, S: WideString; Index: Integer): Boolean;
// only European languages (no Japanese and such). Space is considered a word character.
function RemoveNonWordChars(const Str: WideString; DoNotRemove: WideString = ''): WideString;
function IsQuoteChar(const Chr: WideChar): Boolean;

{ functions called with MaxWidth = 0 will just return the original string. }
// note: unlike the standard WrapText, it will remove spaces if they occur at the beginning/end of new line.
function WrapText(const Str: WideString; const Delimiter: WideString; const MaxWidth: Word): WideString;
function PadText(const Str: WideString; const NewLine, PadStr: WideString; const MaxWidth: Word): WideString;
function PadTextWithVariableLineLength(const Str: WideString; const NewLine, PadStr: WideString;
  const LineLengths: array of Integer): WideString;
                       
function StrPad(const Str: WideString; ToLength: Integer; PadChar: WideChar = ' '): WideString;
function StrPadLeft(const Str: WideString; ToLength: Integer; PadChar: WideChar = ' '): WideString;
function StrRepeat(const Str: WideString; Times: Integer): WideString;
// it's like PHP's strtr() - replacing chars from CharChars with those in ToChars or
// removes them if ToChars is shorter than FromChars.
function StrReplace(const Str: WideString; FromChars, ToChars: WideString): WideString; overload;
function StrReplace(const S, OldPattern, NewPattern: WideString;
  Flags: TReplaceFlags): WideString; overload;

function EscapeString(const Str: WideString; CharsToEscape: WideString = ''): WideString;
function UnescapeString(const Str: WideString; CharsToEscape: WideString = ''): WideString;

function FormatVersion(Version: Word): WideString;
// formats date in format $DDMMYYYY. This format is useful because it's locale-independent.
function FormatDate(Date: DWord): WideString;
function FormatNumber(Number: DWord): WideString; // adds thousand separators.
function GenericFormat(Number: Single; const Language: TGenericFormatLanguage): WideString;
function FormatInterval(Millisecs: DWord): WideString;
function FormatSize(Bytes: DWord): WideString;

function PosLast(const Substr, Str: String; Start: Word = 1): Integer;
function PosLastW(const Substr, Str: WideString; Start: Word = 1): Integer;
function PosW(const Substr, Str: WideString; Start: Word = 1): Integer;

function TrimStringArray(WSArray: TWideStringArray): TWideStringArray;
function Trim(Str: WideString; const Chars: WideString): WideString; overload;
function TrimLeft(Str: WideString; const Chars: WideString): WideString; overload;
function TrimRight(Str: WideString; const Chars: WideString): WideString; overload;      
function ConsistsOfChars(const Str, Chars: WideString): Boolean;

function CallOnEachLineIn(Str: WideString; const Callback: TCallOnEachLineInCallback;
  const UserData: Pointer = NIL): DWord; overload;
function CallOnEachLineIn(Str: WideString; const Callback: TCallOnEachLineInCallback_OO;
  const UserData: DWord = 0): DWord; overload;
function CallOnEachLineIn(Str: WideString; Options: TCallOnEachLineOptions_OO): DWord; overload;

function CompareStr(const S1, S2: WideString; Flags: DWord = 0): Integer;
function CompareText(const S1, S2: WideString): Integer;

function MaskMatch(const Str, Mask: WideString): Boolean;
{ Info can have special values in some cases:
  * Matched = True but MatchLength = 0 (and StrPos having random value) - this means that Mask consisted of only "*" and
    thus no particular substring could be specified (since it could match any part of the string). }
function MaskMatchInfo(const Str, Mask: WideString; StartingPos: Word = 1): TMaskMatchInfo;

function UpperCase(const Str: WideString): WideString;
function LowerCase(const Str: WideString): WideString;
function UpperCaseFirst(const Str: WideString): WideString;
function LowerCaseFirst(const Str: WideString): WideString;

function StripAccelChars(const Str: WideString): WideString;

const
  // for WrapText and PadText.
  LineBreakers:   WideString  = '.,!?";:'#10#13#9 +
                   {Japanese::} #$3000#$3001#$3002#$FF0C#$FF0E#$FF1A#$FF1B#$FF1F#$FF01#$2026#$2025#$FF5E;

type
  TStringUtilsLanguage = record
    VersionFormat:      WideString;
    ThousandsSeparator: WideString;

    IntervalFormat:     TGenericFormatLanguage;
    SizeFormat:         TGenericFormatLanguage;
  end;

var
  StringUtilsLanguage: TStringUtilsLanguage;

implementation

uses StrUtils, Math, ConvUtils;

const
  cHotkeyPrefix = '&'; // it's defined in Menus.pas but we don't require the whole unit because of it.

  { Un/EscapeString }
  EscapeChar:     WideChar        = '\';
  LastUnprintableChar             = 31;
  // note: chars in 0..31 are escaped in the function so don't list them here (except
  //       those who have their own \X instead of \xx char like #13).
  StdCharsToEscape:  WideString   = '\'#13#10#9;     // don't forget to escape the EscapeChar itself.
  StdEscapedChars:   WideString   = '\rnt';

function TryStrToIntStrict(const S: String; out Value: Integer): Boolean;
begin
  if (S <> '') and (S[1] > ' ') and (S[Length(S)] > ' ') then
    Result := TryStrToInt(S, Value)
    else
      Result := False;
end;
                   
function DetectEolnStyleIn(Str: WideString): WideString;
const
  CR = #13;
  LF = #10;
var
  LfPos: Integer;
begin
  LfPos := PosW(LF, Str, 2);

  if LfPos = 0 then
    if PosW(CR, Str, 2) = 0 then
      Result := LF
      else
        Result := CR
    else if Str[LfPos - 1] = CR then
      Result := CR + LF
      else
        Result := LF;
end;

function Explode(Delimiter, Str: WideString; Count: Integer = 0; SkipEmpty: Boolean = False): TWideStringArray;
var
  Current, P: Integer;
begin
  Current := 0;               
  SetLength(Result, 0);
  SetLength(Result, $FFFF);

  while True do
  begin
    Inc(Current);
    if current = 10000 then
    current := current + 1 - 1;
    if Current = $FFFF then
      raise EOutOfMemory.Create('Explode does not support more than 65536 pieces.');

    // note: we check for Pos = 0 here rather than in while because we need to count last
    //       empty piece (if any) as well.
    P := Pos(Delimiter, Str);
    if Length(Str) = 0 then
      Break;

    if (P > 0) and ((Count = 0) or (Current < Count)) then
    begin
      if not SkipEmpty or (P > 1) then
        Result[Current - 1] := Copy(Str, 1, P - 1)
        else
          Dec(Current);
      Str := Copy(Str, P + Length(Delimiter), MaxInt);
    end
      else
      begin
        Result[Current - 1] := Str;
        Break;
      end;
  end;

  if SkipEmpty and (Current <> 0) and (Result[Current - 1] = '') then
    Dec(Current);
  SetLength(Result, Current);
end;

function Split(Str: WideString; Splitter: WideString; var First, Second: WideString;
  const Escaper: WideString = ''): Boolean;
var
  Pos: Integer;
begin
  Pos := 0;
  while True do
  begin
    Pos := PosW(Splitter, Str, Pos + 1);
    if (Pos = 0) or (Escaper = '') or (Copy(Str, Pos - Length(Escaper), Length(Escaper)) <> Escaper) then
      Break
      else
      begin
        Delete(Str, Pos - Length(Escaper), Length(Escaper));
        Dec(Pos, Length(Escaper));
      end;
  end;

  Result := Pos > 0;
  if Result then
  begin
    First := Copy(Str, 1, Pos - 1);
    Second := Copy(Str, Length(First) + Length(Splitter) + 1, Length(Str));
  end
    else
    begin
      First := Str;
      Second := '';
    end;
end;

function CharsOfString(const Str: WideString): TWideStringArray;
var
  I: Word;
begin
  SetLength(Result, Length(Str));
  for I := 1 to Length(Str) do
    Result[I - 1] := Str[I]
end;

function Join(WSArray: array of WideString; Glue: WideString = ', '): WideString;
var
	I: Word;
begin
	Result := '';
	if Length(WSArray) <> 0 then
		for I := 0 to Length(WSArray) - 1 do
			Result := Result + Glue + WSArray[I];

	Result := Copy(Result, Length(Glue) + 1, $FFFF)
end;

function QuotedStr(const S: WideString; Quote: WideChar): WideString;
var
  P, Src, Dest: PWideChar;
  AddCount: Integer;
begin
  AddCount := 0;
  P := StrScanW(PWideChar(S), Quote);
  while P <> NIL do
  begin
    Inc(P);
    Inc(AddCount);
    P := StrScanW(P, Quote);
  end;
  if AddCount = 0 then
  begin
    Result := Quote + S + Quote;
    Exit;
  end;
  SetLength(Result, Length(S) + AddCount + 2);
  Dest := Pointer(Result);
  Dest^ := Quote;
  Inc(Dest);
  Src := Pointer(S);
  P := StrScanW(Src, Quote);
  repeat
    Inc(P);
    Move(Src^, Dest^, P - Src);
    Inc(Dest, P - Src);
    Dest^ := Quote;
    Inc(Dest);
    Src := P;
    P := StrScanW(Src, Quote);
  until P = NIL;
  P := StrEndW(Src);
  Move(Src^, Dest^, P - Src);
  Inc(Dest, P - Src);
  Dest^ := Quote;
end;

function ExtractQuotedStr(var Src: PWideChar; Quote: WideChar): WideString;
var
  P, Dest: PWideChar;
  DropCount: Integer;
begin
  Result := '';
  if (Src = NIL) or (Src^ <> Quote) then Exit;
  Inc(Src);
  DropCount := 1;
  P := Src;
  Src := StrScanW(Src, Quote);
  while Src <> NIL do   // count adjacent pairs of quote chars
  begin
    Inc(Src);
    if Src^ <> Quote then Break;
    Inc(Src);
    Inc(DropCount);
    Src := StrScanW(Src, Quote);
  end;
  if Src = NIL then Src := StrEndW(P);
  if ((Src - P) <= 1) then Exit;
  if DropCount = 1 then
    SetString(Result, P, Src - P - 1)
  else
  begin
    SetLength(Result, Src - P - DropCount);
    Dest := PWideChar(Result);
    Src := StrScanW(P, Quote);
    while Src <> NIL do
    begin
      Inc(Src);
      if Src^ <> Quote then Break;
      Move(P^, Dest^, Src - P);
      Inc(Dest, Src - P);
      Inc(Src);
      P := Src;
      Src := StrScanW(Src, Quote);
    end;
    if Src = NIL then Src := StrEndW(P);
    Move(P^, Dest^, Src - P - 1);
  end;
end;

function DequotedStr(const S: WideString; AQuote: WideChar): WideString;
var
  LText: PWideChar;
begin
  LText := PWideChar(S);
  Result := ExtractQuotedStr(LText, AQuote);
  if Result = '' then
    Result := S;
end;

function StrEndW(const Str: PWideChar): PWideChar;
begin
  Result := Str;
  while Result[0] <> #0 do
    Result := PWideChar( DWord(Result) + 2 );
end;

function StrScanW(Str: PWideChar; Chr: WideChar): PWideChar;
var
  I: Integer;
begin
  I := 0;
  while Str[I] <> Chr do
    if Str[I] = #0 then
    begin
      Result := NIL;
      Exit;
    end
      else
        Inc(I);

  Result := @Str[I];
end;

  function Quote(const Str, CharToQuote: WideString; StartAt: Integer = 1): WideString;
  var
    I: Integer;
  begin
    Result := Str;

    for I := Length(Str) downto StartAt do
      if Str[I] = CharToQuote then
        Insert(CharToQuote, Result, I);
  end;

  function Unquote(const Str, QuoteChar: WideString; out EndPos: Integer; StartAt: Integer): WideString;
  var
    I: Integer;
    QuoteMet: Boolean;
  begin
    QuoteMet := False;

    for I := StartAt to Length(Str) do
      if Str[I] = QuoteChar then
      begin
        if QuoteMet then
          Result := Result + QuoteChar;
        QuoteMet := not QuoteMet;
      end
        else if QuoteMet then
        begin
          EndPos := I;
          Exit;
        end
          else
            Result := Result + Str[I];

    if QuoteMet then
      EndPos := Length(Str) + 1
      else
        EndPos := -1;
  end;
  
  function Unquote(const Str, QuoteChar: WideString; StartAt: Integer = 1): WideString;
  var
    EndPos: Integer;
  begin
    Result := Unquote(Str, QuoteChar, EndPos, StartAt);
    if EndPos < 2 then
      raise EConvertError.CreateFmt('Cannot Unquote(%s) - no ending %s found.', [Copy(Str, StartAt, 100), QuoteChar]);
  end;

  function PascalQuote(const Str: WideString): WideString;                                   
  var
    I: Integer;
    Opened: Boolean;
  begin
    Result := '';
    Opened := False;

    for I := 1 to Length(Str) do
      if Ord(Str[I]) < Ord(' ') then
      begin
        if Opened then Result := Result + '''';
        Opened := False;

        Result := Result + '#' + IntToStr(Ord(Str[I]));
      end
        else
        begin
          if not Opened then Result := Result + '''';
          Opened := True;

          if Str[I] = '''' then
            Result := Result + ''''''''
            else
              Result := Result + Str[I];
        end;

    if Opened then Result := Result + '''';
  end;

  function PascalUnquote(const Str: WideString; var Pos: Integer): WideString;

    function EscapedChar(out Output: WideChar): Boolean;
    var
      I, Code: Integer;
    begin
      for I := Pos + 8 downto Pos + 1 do
        if TryStrToIntStrict(Copy(Str, Pos + 1, I - Pos - 1), Code) then
        begin
          Pos := I + 2;
          Result := True;
          Output := WideChar(Code);
          Exit;
        end;

      Result := False;
    end;

  var
    Opened: Boolean;
    CharByCode: WideChar;
  begin
    Opened := False;

    while Pos <= Length(Str) do
    begin
      if Str[Pos] = '''' then
      begin
        if Opened and (Pos + 1 <= Length(Str)) and (Str[Pos + 1] = '''') then
        begin
          Result := Result + '''';
          Inc(Pos);
        end;
        Opened := not Opened;
      end
        else if Opened then
          Result := Result + Str[Pos]
          else if (Str[Pos] <> '#') then
            Exit
            else if EscapedChar(CharByCode) then
              Result := Result + CharByCode
              else
                Exit;

      Inc(Pos);
    end;

    if not Opened then
      Pos := -1;
  end;

  function PascalUnquote(Str: WideString; MustStartWithQuote: Boolean = True): WideString;
  var
    Pos: Integer;
  begin
    if Str = '' then
      Result := ''
      else
      begin
        Pos := 1;
        Result := PascalUnquote(Str, Pos);

        if Pos <> -1 then
          if Pos < Length(Str) then
            raise EConvertError.CreateFmt('There is still data after closing apostrophe ('') of' +
                                          ' the Pascal-quoted string "%s".', [Str])
          else
            raise EConvertError.CreateFmt('Pascal-quoted string "%s" must end with' +
                                          ' an apostrophe ('').', [Str]);
      end;                               
  end;

function TrimStringArray(WSArray: TWideStringArray): TWideStringArray;
var
  I: Word;
begin
  Result := WSArray;
  if Length(Result) <> 0 then
    for I := 0 to Length(Result) - 1 do
      Result[I] := sysutils.Trim(Result[I])
end;

function Trim(Str: WideString; const Chars: WideString): WideString;
var
  Start, Finish: Word;
begin
  Start := 1;
  while (Start <= Length(Str)) and (PosW(Str[Start], Chars) <> 0) do
    Inc(Start);
  Finish := Length(Str);
  while (Finish > Start) and (PosW(Str[Finish], Chars) <> 0) do
    Dec(Finish);
  Result := Copy(Str, Start, Finish - Start + 1)
end;

function TrimLeft(Str: WideString; const Chars: WideString): WideString;
var
  Start: Word;
begin
  Start := 1;
  while (Start <= Length(Str)) and (PosW(Str[Start], Chars) <> 0) do
    Inc(Start);
  Result := Copy(Str, Start, $FFFF)
end;

function TrimRight(Str: WideString; const Chars: WideString): WideString;
var
  Finish: Word;
begin
  Finish := Length(Str);
  while (Finish >= 1) and (PosW(Str[Finish], Chars) <> 0) do
    Dec(Finish);
  Result := Copy(Str, 1, Finish)
end;

function ConsistsOfChars(const Str, Chars: WideString): Boolean;
var
  I: Integer;
begin            
  Result := False;

  for I := 1 to Length(Str) do
    if PosW(Str[I], Chars) = 0 then
      Exit;

  Result := True;
end;

function EscapeString(const Str: WideString; CharsToEscape: WideString = ''): WideString;
var
  EscapedChars: WideString;
  I, Esc: Word;

  function PaddedIntToStr(const Int: Word): WideString;
  begin
    Result := IntToStr(Int);
    if Int < 10 then
      Insert('0', Result, 1);
  end;
begin
  EscapedChars  := StdEscapedChars  + CharsToEscape;
  CharsToEscape := StdCharsToEscape + CharsToEscape;

  Result := '';
  for I := 1 to Length(Str) do
  begin
    Esc := PosW(Str[I], CharsToEscape);
    if Esc <> 0 then
      Result := Result + EscapeChar + EscapedChars[Esc]
      else if Word(Str[I]) <= LastUnprintableChar then
        Result := Result + EscapeChar + PaddedIntToStr( Word(Str[I]) )
        else
          Result := Result + Str[I]
  end;
end;

function UnescapeString(const Str: WideString; CharsToEscape: WideString = ''): WideString;
var
  EscapedChars: WideString;
  I, Esc: Word;
  Escaping: Boolean;
  Error: Integer;
begin
  EscapedChars  := StdEscapedChars  + CharsToEscape;
  CharsToEscape := StdCharsToEscape + CharsToEscape;

  Result := '';
  Escaping := False;

  I := 1;
  while I <= Length(Str) do
  begin
    if Escaping then
    begin
      Escaping := False;
      Esc := PosW(Str[I], EscapedChars);
      if Esc <> 0 then
        { \t\n\r etc. }
        Result := Result + CharsToEscape[Esc]
        else
        begin
          Error := 1;

          { \31 }
          if I < Length(Str) then
          begin
            Val(Char(Str[I]) + Char(Str[I + 1]), Esc, Error);
            if Error = 0 then
            begin
              Result := Result + WideChar(Esc);
              Inc(I);
            end;
          end;

          { \x - unnecesary escaping, reinsert }
          if Error <> 0 then
            Result := Result + EscapeChar + Str[I];
        end;
    end
      else if Str[I] = EscapeChar then
        Escaping := True
        else
          Result := Result + Str[I];

    Inc(I);
  end;

  if Escaping then
    Result := Result + EscapeChar;
end;

function IsDelimiter(const Delimiters, S: WideString; Index: Integer): Boolean;
begin
  Result := (Index > 0) and (Index <= Length(S)) and (PosW(S[Index], Delimiters) <> 0)
end;

function RemoveNonWordChars(const Str: WideString; DoNotRemove: WideString = ''): WideString;
var
  I: Word;
begin
  Result := Str;
  if Length(Result) <> 0 then
    for I := Length(Result) downto 1 do
      if not IsDelimiter(DoNotRemove, Result, I) and
         (((Word(Result[I]) <> Word(' ')) and (Word(Result[I]) <= Word('/'))) or
          ((Word(Result[I]) >= Word(':')) and (Word(Result[I]) <= Word('?'))) or
          ((Word(Result[I]) >= Word('[')) and (Word(Result[I]) <= Word('`'))) or
          ((Word(Result[I]) >= Word('{')) and (Word(Result[I]) <= Word('}')))) then
        Delete(Result, I, 1)
end;

function IsQuoteChar(const Chr: WideChar): Boolean;
begin
  Result := (Chr = '"') or (Chr = #$201C) or (Chr = #$300C {Japanese "bracket"-quote});
end;

function GenericPadText(const Str: WideString; const NewLine, PadStr: WideString;
  const LineLengths: array of Integer): WideString;
var
  I, LastDelim, LastBreak, PadCount, CurrentLine: Integer;
  Delimiter: WideString;
begin
  Result := Str;

  I := 1;
  LastDelim := 0;
  LastBreak := 1;

  CurrentLine := 0;
  if Length(LineLengths) = 0 then
    Exit;

  while I <= Length(Result) do
  begin
    if I - LastBreak < LineLengths[CurrentLine] then
    begin
      if IsDelimiter(LineBreakers, Result, I) then
        LastDelim := I;
      Inc(I);
    end
      else
      begin
        if LastDelim = 0 then
          LastDelim := I - 1;

        while (LastDelim + 1 <= Length(Result)) and (Result[LastDelim + 1] = ' ') do
          Delete(Result, LastDelim + 1, 1);

        while (LastDelim > 0) and (Result[LastDelim] = ' ') do
        begin
          Delete(Result, LastDelim, 1);
          Dec(LastDelim);
        end;

        if LastDelim >= Length(Result) then
          Exit;

        if PadStr = '' then
          Delimiter := ''
          else
          begin
            PadCount := Max(0, LineLengths[CurrentLine] - (LastDelim - LastBreak) - 1);
            Delimiter := StrRepeat(PadStr, PadCount);
            Delimiter := Copy(Delimiter, 1, PadCount); // since PadStr can be more than one char we trim it.
          end;

        Delimiter := Delimiter + NewLine;

        Insert(Delimiter, Result, LastDelim + 1);
        LastBreak := LastDelim + 1 + Length(Delimiter);
        I := LastBreak;
        LastDelim := 0;

        if CurrentLine + 1 < Length(LineLengths) then
        begin
          Inc(CurrentLine);
          if LineLengths[CurrentLine] = 0 then
            Exit;
        end;
      end;
  end
end;

function WrapText(const Str: WideString; const Delimiter: WideString; const MaxWidth: Word): WideString;
var
  LineLengths: array of Integer;
begin
  SetLength(LineLengths, 1);
  LineLengths[0] := MaxWidth;
  Result := GenericPadText(Str, Delimiter, '', LineLengths);
end;

function PadText(const Str: WideString; const NewLine, PadStr: WideString; const MaxWidth: Word): WideString;
var
  LineLengths: array of Integer;
begin
  SetLength(LineLengths, 1);
  LineLengths[0] := MaxWidth;
  Result := GenericPadText(Str, NewLine, PadStr, LineLengths);
end;

function PadTextWithVariableLineLength(const Str: WideString; const NewLine, PadStr: WideString;
  const LineLengths: array of Integer): WideString;
begin
  Result := GenericPadText(Str, NewLine, PadStr, LineLengths);
end;

function StrPad(const Str: WideString; ToLength: Integer; PadChar: WideChar = ' '): WideString;
begin
  Result := Str + StrRepeat(PadChar, ToLength - Length(Str));
end;

function StrPadLeft(const Str: WideString; ToLength: Integer; PadChar: WideChar = ' '): WideString;
begin
  Result := StrRepeat(PadChar, ToLength - Length(Str)) + Str;
end;

function StrRepeat(const Str: WideString; Times: Integer): WideString;
begin
  Result := '';
  if Str <> '' then
    for Times := Times downto 1 do
      Result := Result + Str;
end;

function StrReplace(const Str: WideString; FromChars, ToChars: WideString): WideString;
var
  I, Pos: Word;
begin
  Result := Str;
  for I := 1 to Length(Result) do
  begin
    Pos := PosW(Result[I], FromChars);
    if Pos <> 0 then
      if Pos <= Length(ToChars) then
        Result[I] := ToChars[I]
        else
          Delete(Result, I, 1);
  end;
end;

// a port of StrUtils.pas' function.
function StrReplace(const S, OldPattern, NewPattern: WideString;
  Flags: TReplaceFlags): WideString;
var
  SearchStr, Patt, NewStr: WideString;
  Offset: Integer;
begin
  if rfIgnoreCase in Flags then
  begin
    SearchStr := WideUpperCase(S);
    Patt := WideUpperCase(OldPattern);
  end else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := PosW(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then
    begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

function FormatVersion(Version: Word): WideString;
begin
  Result := WideFormat(StringUtilsLanguage.VersionFormat, [Hi(Version), Lo(Version)])
end;

function FormatDate(Date: DWord): WideString;
var
  StrDate: String;
begin
  StrDate := IntToHex(Date, 8);
  Result := DateTimeToStr(EncodeDate(StrToInt(Copy(StrDate, 5, 4)),   {year}
                                     StrToInt(Copy(StrDate, 3, 2)),   {month}
                                     StrToInt(Copy(StrDate, 1, 2)))); {day}
end;

function FormatNumber(Number: DWord): WideString;
var
  I: ShortInt;
  Str: WideString;
begin
  Str := IntToStr(Number); { 0123`456`789 }
  Result := Str;

  for I := Length(Str)  downto 2 do
    if (Length(Str) - I + 1) mod 3 = 0 then
      Result := Copy(Result, 1, I - 1) + StringUtilsLanguage.ThousandsSeparator + Copy(Result, I, $FF);
end;

function GenericFormat(Number: Single; const Language: TGenericFormatLanguage): WideString;
var
  I: Byte;
  NumberStr: String;
begin
  with Language do
    for I := 0 to Length(Measures) - 1 do
      if (Number < Measures[I].Capacity) or (I = Length(Measures) - 1) then
      begin
        // a reason why I use WideForamt + Format instead of just WideFormat is explained
        //   on top of the unit (see TGenericFormatLanguage type).
        if I = 0 then
          NumberStr := Format(BasicNumberFormat, [Number])
          else
            NumberStr := Format(NumberFormat, [Number]);

        Result := WideFormat(ResultFormat, [NumberStr, Measures[I].Measure]);
        Break;
      end
        else
          Number := Number / Measures[I].Capacity;
end;

function FormatInterval(Millisecs: DWord): WideString;
begin
  Result := GenericFormat(Millisecs, StringUtilsLanguage.IntervalFormat);
end;

function FormatSize(Bytes: DWord): WideString;
begin
  Result := GenericFormat(Bytes, StringUtilsLanguage.SizeFormat);
end;

function PosW(const Substr, Str: WideString; Start: Word = 1): Integer;
var
  StrPos, SubstrPos: Integer;
begin                   
  Result := 0;
  if Substr = '' then
    Exit;

	StrPos := Max(1, Start);
  SubstrPos := 1;

  while StrPos <= Length(Str) do
  begin
    if Substr[SubstrPos] <> Str[StrPos] then
    begin
      Dec(StrPos, SubstrPos - 1);
      SubstrPos := 1;
    end
      else
        if SubstrPos >= Length(Substr) then
        begin
          Result := StrPos - SubstrPos + 1;
          Exit
        end
        else
          Inc(SubstrPos);
          
    Inc(StrPos);
  end;
end;

function PosLast(const Substr, Str: String; Start: Word = 1): Integer;
var
  LastPos: Integer;
begin
  Result := 0;
  LastPos := Min(0, Start - 1);
  while True do
  begin
    LastPos := PosEx(Substr, Str, LastPos + 1);
    if LastPos = 0 then
      Break
      else
        Result := LastPos;
  end;
end;

function PosLastW(const Substr, Str: WideString; Start: Word = 1): Integer;
var
  LastPos: Integer;
begin
  Result := 0;
  LastPos := Min(0, Start - 1);
  while True do
  begin
    LastPos := PosW(Substr, Str, LastPos + 1);
    if LastPos = 0 then
      Break
      else
        Result := LastPos;
  end;
end;

function MaskMatch(const Str, Mask: WideString): Boolean;
begin
  Result := MaskMatchInfo(Str, Mask).Matched
end;

function MaskMatchInfo(const Str, Mask: WideString; StartingPos: Word = 1): TMaskMatchInfo;
var
  BeginningAnyMatch, EndingAnyMatch: Word;
  Info: TMaskMatchInfo;

  function Match(const StrI, MaskI: Word): Boolean;
	begin
    if MaskI > Length(Mask) then
      Result := StrI = Length(Str) + 1
      else if StrI > Length(Str) then
        Result := MaskI > EndingAnyMatch
        else if (Mask[MaskI] = '*') or (Mask[MaskI] = '+') then
          Result := Match(Succ(StrI), Succ(MaskI)) or Match(Succ(StrI), MaskI) or
                    ((Mask[MaskI] = '*') and (Match(StrI, Succ(MaskI)))) or (MaskI = Length(Mask))
          else
	 	        Result := ((Mask[MaskI] = Str[StrI]) or (Mask[MaskI] = '?')) and
                      Match(Succ(StrI), Succ(MaskI));

    if Result and ((MaskI <= Length(Mask)) and (Mask[MaskI] <> '*')) then
    begin
      Info.StrPos := Min(Info.StrPos, StrI);
      Info.MatchLength := Max(Info.MatchLength, StrI)
    end
	end;

begin
  Info.StrPos := $FFFF;
  Info.MatchLength := 0;

  BeginningAnyMatch := 1;
  while (BeginningAnyMatch < Length(Mask)) and (Mask[BeginningAnyMatch] = '*') do
    Inc(BeginningAnyMatch);

  EndingAnyMatch := Length(Mask);
  while (EndingAnyMatch > 0) and (Mask[EndingAnyMatch] = '*') do
    Dec(EndingAnyMatch);

  Info.Matched := Match(StartingPos, 1);

  if Info.StrPos = $FFFF then
    Info.MatchLength := 0
    else
      Dec(Info.MatchLength, Info.StrPos - 1);
  Result := Info
end;

function CompareStr(const S1, S2: WideString; Flags: DWord = 0): Integer;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, Flags, PWideChar(S1), Length(S1), PWideChar(S2), Length(S2)) - 2
end;

function CompareText(const S1, S2: WideString): Integer;
begin
  Result := CompareStr(S1, S2, NORM_IGNORECASE);
end;

function CallOnEachLineIn(Str: WideString; const Callback: TCallOnEachLineInCallback;
  const UserData: Pointer = NIL): DWord;
var
  I, PrevNewLine: DWord;
begin
  Result := 0;
  PrevNewLine := 1;

  if (Str <> '') and (Str[Length(Str)] <> #10) and (Str[Length(Str)] <> #13) then
    Str := Str + #10;

  for I := 1 to Length(Str) do
    if (Str[I] = #10) or (Str[I] = #13) then
    begin
      Inc(Result);
      if I > PrevNewLine then
        if Callback(Copy(Str, PrevNewLine, I - PrevNewLine), UserData) then
          Break;
      PrevNewLine := I + 1;
    end;
end;

function CallOnEachLineIn(Str: WideString; const Callback: TCallOnEachLineInCallback_OO;
  const UserData: DWord = 0): DWord;
var
  Options: TCallOnEachLineOptions_OO;
begin
  Options.Callback := Callback;
  Options.UserData := UserData;
  Options.EOLN := #10;
  Result := CallOnEachLineIn(Str, Options);
end;

function CallOnEachLineIn(Str: WideString; Options: TCallOnEachLineOptions_OO): DWord;
var
  I, PrevNewLine: Integer;
  EOLN: WideString;
begin
  Result := 0;
  PrevNewLine := 1;
  EOLN := Options.EOLN;

  if (Str <> '') and (Copy(Str, Length(Str) + Length(EOLN), Length(EOLN)) <> EOLN) then
    Str := Str + EOLN;

  for I := 1 to Length(Str) do
    if Copy(Str, I, Length(EOLN)) = EOLN then
    begin
      Inc(Result);
      if I > PrevNewLine then
        if Options.Callback(TrimRight( Copy(Str, PrevNewLine, I - PrevNewLine), #13#10 ), Options.UserData) then
          Break;
      PrevNewLine := I + Length(EOLN);
    end;
end;

function UpperCase(const Str: WideString): WideString;
begin
  Result := WideUpperCase(Str);
end;

function LowerCase(const Str: WideString): WideString;
begin
  Result := WideLowerCase(Str);
end;

function UpperCaseFirst(const Str: WideString): WideString;
begin
  Result := UpperCase(Str[1]) + Copy(Str, 2, Length(Str));
end;

function LowerCaseFirst(const Str: WideString): WideString;
begin
  Result := LowerCase(Str[1]) + Copy(Str, 2, Length(Str));
end;

function StripAccelChars(const Str: WideString): WideString;
var
  I: Word;
begin
  Result := Str;
  for I := 1 to Length(Str) do
    if Str[I] = cHotkeyPrefix then
      Delete(Result, I, 1)
end;

function GetDefaultThousandsSeparator: WideString;
begin
  SetLength(Result, 10);
  SetLength(Result, GetLocaleInfoW(GetUserDefaultLCID, LOCALE_STHOUSAND, PWideChar(Result), 10));
end;

initialization
  with StringUtilsLanguage do
  begin
    VersionFormat       := 'v%d.%d';
    ThousandsSeparator  := GetDefaultThousandsSeparator;

    with IntervalFormat do
    begin
      BasicNumberFormat := '%1.0f';
      NumberFormat := '%1.2f';
      ResultFormat := '%s%s';

      SetLength(Measures, 6);
      Measures[0].Capacity := 1000;
      Measures[0].Measure := 'ms';
      Measures[1].Capacity := 60;
      Measures[1].Measure := 's';
      Measures[2].Capacity := 60;
      Measures[2].Measure := 'm';
      Measures[3].Capacity := 24;
      Measures[3].Measure := 'h';
      Measures[4].Capacity := 7;
      Measures[4].Measure := 'd';
      Measures[5].Capacity := $FFFF;
      Measures[5].Measure := 'w';
    end;

    with SizeFormat do
    begin
      BasicNumberFormat := '%1.0f';
      NumberFormat := '%1.2f';
      ResultFormat := '%s %s';

      SetLength(Measures, 4);
      Measures[0].Capacity := 1024;
      Measures[0].Measure := 'bytes';
      Measures[1].Capacity := 1024;
      Measures[1].Measure := 'Kb';
      Measures[2].Capacity := 1024;
      Measures[2].Measure := 'Mb';
      Measures[3].Capacity := $FFFF;
      Measures[3].Measure := 'Gb';
    end;
  end;

end.
