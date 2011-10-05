program StringUtilsTest;

{$APPTYPE CONSOLE}

uses
  StringsW,
  Windows,
  SysUtils,
  MMSystem,
  StringUtils;

function Assert(Success: Boolean; Msg: String): Boolean;
begin
  Result := Success;
  if not Success then
  begin
    WriteLn('Assertion failed: ', Msg);
    ReadLn;
    Halt(1)
  end
end;

procedure TestMaskMatch;
  procedure AssertMatch(const MatchResult: TMaskMatchInfo; PosShouldBe, LengthShouldBe: Byte);
  begin
    Assert(MatchResult.Matched, 'MaskMatchInfo, Matched');
    if LengthShouldBe <> 0 then
      Assert(MatchResult.StrPos = PosShouldBe, 'MaskMatchInfo, StrPos');
    Assert(MatchResult.MatchLength = LengthShouldBe, 'MaskMatchInfo, Length')
  end;

begin
  AssertMatch(MaskMatchInfo('qwertyu', 'q*u'), 1, 7);
  Assert(not MaskMatch('qwertyu', 'q *u'), 'MaskMatch');

  AssertMatch(MaskMatchInfo('qwertyu', 'q?????u'), 1, 7);
  AssertMatch(MaskMatchInfo('qwertyu', 'q????*u'), 1, 7);
  AssertMatch(MaskMatchInfo('qwertyu', 'q?????*u'), 1, 7);
  AssertMatch(MaskMatchInfo('qwertyu', 'qwertyu*'), 1, 7);
  AssertMatch(MaskMatchInfo('qwertyu', '*q*'), 1, 1);
  AssertMatch(MaskMatchInfo('qwertyu', 'qwertyu**'), 1, 7);

  AssertMatch(MaskMatchInfo('1', '*?*'), 1, 1);
  AssertMatch(MaskMatchInfo('12', '*?*'), 2, 1);
  Assert(not MaskMatch('1', '*??*'), 'MaskMatch');
  AssertMatch(MaskMatchInfo('', '*'), 0, 0);
  AssertMatch(MaskMatchInfo('', '**'), 0, 0);

  Assert(not MaskMatch('', '123'), 'MaskMatch');
  Assert(not MaskMatch('123', ''), 'MaskMatch');
  AssertMatch(MaskMatchInfo('', ''), 1, 0);
  Assert(not MaskMatch('1*3', '123'), 'MaskMatch');
  AssertMatch(MaskMatchInfo('1*3', '1?3'), 1, 3);
  AssertMatch(MaskMatchInfo('123', '*'), 0, 0);

  Assert(not MaskMatch('qwertyu', 'q*wty'), 'MaskMatch');
  Assert(not MaskMatch('qwertyu', 'qwertyu?'), 'MaskMatch');

  Assert(not MaskMatch('', '+'), 'MaskMatch');
  Assert(not MaskMatch('1234', '1+234'), 'MaskMatch');
  AssertMatch(MaskMatchInfo('1234', '1+4'), 1, 4);

  AssertMatch(MaskMatchInfo('"as above" mark', '*above*'), 5, 5);

  // inspired by http://www.delphifaq.com/faq/delphi/strings/f112.shtml
	AssertMatch(MaskMatchInfo('auto.bmp', 'a*.bmp'), 1, 8);
	Assert(not MaskMatch('auto.bmp', 'a*x.bmp'), 'MaskMatch');
	AssertMatch(MaskMatchInfo('auto.bmp', 'a*o.bmp'), 1, 8);
	Assert(not MaskMatch('auto.bmp', 'a*tu.bmp'), 'MaskMatch');
	AssertMatch(MaskMatchInfo('auto.bmp', 'a*o.b*p'), 1, 8);
	AssertMatch(MaskMatchInfo('auto.bmp', 'a**o.b*p'), 1, 8);
	AssertMatch(MaskMatchInfo('auto.bmp', 'a*****o.b*p'), 1, 8);
	AssertMatch(MaskMatchInfo('auto.bmp', 'a*****o+*??'), 1, 8);
	Assert(not MaskMatch('auto.bmp', 'a*****o+????'), 'MaskMatch');
	AssertMatch(MaskMatchInfo('auto.bmp', 'a*o++p'), 1, 8);

	AssertMatch(MaskMatchInfo('auto.bmp', '*ut*.**'), 2, 4);
	AssertMatch(MaskMatchInfo('auto.bmp', '*ut***'), 2, 2);
	Assert(not MaskMatch('auto.bmp', '*ut* **'), 'MaskMatch');
	AssertMatch(MaskMatchInfo('auto.bmp', '***.+'), 5, 4);
	AssertMatch(MaskMatchInfo('auto.bmp', '**?*'), 3, 1);
	AssertMatch(MaskMatchInfo('auto.bmp', '***'), 1, 0);
	AssertMatch(MaskMatchInfo('auto.bmp.foo', '*ut*.*'), 2, 4);
	AssertMatch(MaskMatchInfo('autout', '*ut'), 5, 2);

	Assert(not MaskMatch('auto', 'auto?'), 'MaskMatch');
	Assert(not MaskMatch('auto', '?auto'), 'MaskMatch');
	AssertMatch(MaskMatchInfo('auto', 'aut?'), 1, 4);
	AssertMatch(MaskMatchInfo('auto', '?uto'), 1, 4);
	AssertMatch(MaskMatchInfo('auto.bmp', 'auto?bmp'), 1, 8);
	AssertMatch(MaskMatchInfo('auto', '????'), 1, 4);
	Assert(not MaskMatch('auto', '???'), 'MaskMatch');
	Assert(not MaskMatch('auto', '?????'), 'MaskMatch');
end;

procedure TestSplit;
const
  TestKey       = '  key \= still key  ';
  TestSplitter  = ' = ';
  TestValue     = '  value \= and value  ';
var
  Key, Value: WideString;
begin
  Split(TestKey + TestSplitter + TestValue, TestSplitter, Key, Value);
  Assert(Key   = TestKey, 'Split');
  Assert(Value = TestValue, 'Split');

  Split('=value', '=', Key, Value);
  Assert(Key   = '', 'Split');
  Assert(Value = 'value', 'Split');

  Split('\=value', '=', Key, Value);
  Assert(Key   = '', 'Split');
  Assert(Value = '\=value', 'Split');

  Split('\=val=ue', '=', Key, Value);
  Assert(Key   = '\=val', 'Split');
  Assert(Value = 'ue', 'Split');

  Split('=val=ue', '=', Key, Value);
  Assert(Key   = '', 'Split');
  Assert(Value = 'val=ue', 'Split');

  Split('key \= val=ue', '=', Key, Value, True);
  Assert(Key   = 'key \', 'Split');
  Assert(Value = ' val=ue', 'Split');
end;

procedure TestExplode;
  procedure AssertArray(const A: TWideStringArray; B: array of const);
  var
    I: Word;
  begin
    Assert(Length(A) = Length(B), 'AssertArray, Length');
    if Length(B) <> 0 then
      for I := 0 to Length(B) - 1 do
        if B[I].VType = vtChar then
          Assert(A[I] = B[I].VChar, 'AssertArray, ' + IntToStr(I))
          else
            Assert(A[I] = PChar(B[I].VAnsiString), 'AssertArray, ' + IntToStr(I))
  end;
begin                                                
  AssertArray(Explode('', 'qwerty'), ['qwerty']);
  AssertArray(Explode('r', 'qwerty'), ['qwe', 'ty']);
  AssertArray(Explode(' ', 'a  w o r dd'), ['a', '', 'w', 'o', 'r', 'dd']);
  AssertArray(Explode(' ', 'a  w o r dd', 3), ['a', '', 'w o r dd']);
  AssertArray(Explode('  ', 'a  w o r dd'), ['a', 'w o r dd']);          
  AssertArray(Explode('==', 'sitting== on the == floor'), ['sitting', ' on the ', ' floor']);
  AssertArray(Explode('==', 'sitting= on the = floor'), ['sitting= on the = floor']);
  AssertArray(Explode('==', '=='), ['', '']);
  
  AssertArray(Explode('|', '||a|b||||c|d||||e|', 0, True), ['a', 'b', 'c', 'd', 'e']);
end;

procedure TestVeryLongExplode;
const
  PartsCount = 1000;
var
  Str: WideString;
  I: DWord;
  Parts: TWideStringArray;
  Timer: DWord;
begin
  Str := '';
  for I := 0 to PartsCount - 1 do
    Str := Str + IntToHex(I, 8) + '|';

  Timer := timeGetTime;
  Parts := Explode('|', Str);
  Timer := timeGetTime - Timer;
  WriteLn(Format('Exploded string of length %d into %d parts in %1.3f seconds.', [Length(Str), Length(Parts), Timer / 1000])); 

  Assert(Length(Parts) = PartsCount + 1, 'TestVeryLongExplode');
  Assert(Parts[PartsCount] = '', 'TestVeryLongExplode');

  for I := 0 to PartsCount - 1 do
    Assert(Parts[I] = IntToHex(I, 8), 'TestVeryLongExplode');
end;

procedure TextWrapAndPadText;
const
  TestStr    = 'A bunch of pumpkins went flying to the kingdom.';
  WrappedStr = 'A bunch of'#13#10 +
               'pumpkins went'#13#10 +
               'flying to the'#13#10 +
               'kingdom.';
  PaddedStr  = 'A bunch of     '#10 +
               'pumpkins went  '#10 +
               'flying to the  '#10 +
               'kingdom.';
  PaddedStrUnevenPadChar =
               'A bunch of-=-='#10 +
               'pumpkins went-'#10 +
               'flying to the-'#10 +
               'kingdom.';

  TestStr_2  = 'Line text Line text Line text';
  PaddedStrUnevenPadChar_2 =
              'Line text   '#10 +
              'Line text   '#10 +
              'Line text';                
  PaddedStrUnevenPadChar_2_no_EOLNs =
              'Line text   ' +
              'Line text   ' +
              'Line text';
  PaddedStrVariableLineLength =
              'Line text ' +
              'Line    ' +
              'text    ' +
              'Line    ' +
              'text';
var
  LineLengths: array of Integer;
begin
  Assert(PadText('ww ww', '-', '_', 4) = 'ww__-ww', 'PadText');
  Assert(PadText('aa  bb  cc  dd  ee  ff  gg  hh', '-', '_', 5) = 'aa___-bb___-cc___-dd___-ee___-ff___-gg___-hh', 'PadText');
  Assert(PadText('aa..bb..cc..dd..ee..ff..gg..hh', '-', '_', 5) = 'aa.._-bb.._-cc.._-dd.._-ee.._-ff.._-gg.._-hh', 'PadText');
  Assert(PadText('aa..bb  cc..dd  ee..ff  gg..hh', '-', '_', 5) = 'aa.._-bb___-cc.._-dd___-ee.._-ff___-gg.._-hh', 'PadText');

  Assert(PadText('ww                           ', '-', '_', 4) = 'ww', 'PadText');
  Assert(PadText('                             ', '-', '_', 4) = '', 'PadText');
  Assert(PadText('tobecutwithin', '-', '_', 5) = 'tobec-utwit-hin', 'PadText');
  Assert(PadText('tobe cutwithin', '-', '_', 5) = 'tobe_-cutwi-thin', 'PadText');

  Assert(PadText('111111111111111111111111111111111111111111 111111111111111111111111111111111111111111 1111111111111', '-', '_', 50) =
                 '111111111111111111111111111111111111111111________-111111111111111111111111111111111111111111________-1111111111111', 'PadText');
  Assert(PadText('  self-reflection, trying to justify myself........==', '-', '_', 50) =
           '  self-reflection, trying to justify myself.......-.==', 'PadText');

  Assert(WrapText(TestStr, #13#10, 15) = WrappedStr, 'WrapText');
  Assert(PadText(TestStr, #10, ' ', 15) = PaddedStr, 'PadText');
  Assert(PadText(TestStr, #10, '-=', 14) = PaddedStrUnevenPadChar, 'PadText');

  Assert(PadText('123 567.', '', ' ', 7) = '123    567.', 'PadText');
  Assert(PadText('tooloooong.', '', ' ', 7) = 'tooloooong.', 'PadText');
  Assert(PadText('tooloooong.', #10, ' ', 7) = 'toolooo'#10'ong.', 'PadText');

  Assert(PadText(TestStr_2, #10, '  ', 12) = PaddedStrUnevenPadChar_2, 'PadText');
  Assert(PadText(TestStr_2, '', '  ', 12) = PaddedStrUnevenPadChar_2_no_EOLNs, 'PadText');

  SetLength(LineLengths, 2);
  LineLengths[0] := 10;
  LineLengths[1] := 8;
  Assert(PadTextWithVariableLineLength(TestStr_2, '', '  ', LineLengths) =
         PaddedStrVariableLineLength, 'PadTextWithVariableLineLength');

  Assert(PadText('a for us', '', '|', 3) = 'a||forus', 'PadText');
  Assert(PadText('a for us', '', '||', 3) = 'a||forus', 'PadText');
  Assert(PadText('a for us', '-', '||', 3) = 'a||-for-us', 'PadText');
  Assert(PadText('a for us', '--', '||', 3) = 'a||--for--us', 'PadText');    
  Assert(PadText('a for uss', '--', '||', 3) = 'a||--for--uss', 'PadText');
  Assert(PadText('a for usss', '--', '||', 3) = 'a||--for--uss--s', 'PadText');

  { Test data got from Hanabira 2. Translation credit goes to Kamyu Aaru. }
  SetLength(LineLengths, 2);
  LineLengths[0] := 45;
  LineLengths[1] := 47;
  Assert(PadTextWithVariableLineLength(
    'It was the second homeroom since I became a second year at Saint Michael''s School for Girls.',
    '', ' ', LineLengths) = 'It was the second homeroom since I became a  ' +
                            'second year at Saint Michael''s School for      ' +
                            'Girls.',
        'Hanabira 2');
  Assert(PadTextWithVariableLineLength(
    'Our class was electing the person who would become the representative of the students of our classroom for a year.',
    '', ' ', LineLengths) = 'Our class was electing the person who would  ' +
                            'become the representative of the students of   ' +
                            'our classroom for a year.',
        'Hanabira 2');
  Assert(PadTextWithVariableLineLength(
    'I look at the blackboard, sigh, and lower my gaze a little.',
    '', ' ', LineLengths) = 'I look at the blackboard, sigh, and lower my ' +
                            'gaze a little.',
        'Hanabira 2');
end;

procedure TestTrimFunctions;
const
  TestStr = '   a str ing   ';
begin              
  Assert(Trim(TestStr) = 'a str ing', 'Trim');
  Assert(TrimLeft(TestStr) = 'a str ing   ', 'TrimLeft');
  Assert(TrimRight(TestStr) = '   a str ing', 'TrimRight');

  Assert(Trim('') = '', 'Trim');
  Assert(TrimLeft('') = '', 'TrimLeft');
  Assert(TrimRight('') = '', 'TrimRight');

  Assert(Trim(' ') = '', 'Trim');
  Assert(TrimLeft(' ') = '', 'TrimLeft');
  Assert(TrimRight(' ') = '', 'TrimRight');

  Assert(Trim('       ') = '', 'Trim');
  Assert(TrimLeft('       ') = '', 'TrimLeft');
  Assert(TrimRight('       ') = '', 'TrimRight');

  Assert(Trim(' | ') = '|', 'Trim');
  Assert(TrimLeft(' | ') = '| ', 'TrimLeft');
  Assert(TrimRight(' | ') = ' |', 'TrimRight');
end;

procedure TestEscaping;
const
  StringWithUnnecesaryEscaping  = 'no \escape \what\so\ever';
  LastUnprintableChar           = 31;
var
  I: Byte;
  Raw, Escaped, EscapedChar: String;
begin
  Assert(EscapeString('new'#10'line') = 'new\nline', 'EscapeString');
  Assert(EscapeString('new'#10#13'line') = 'new\n\rline', 'EscapeString');
  Assert(EscapeString('new'#13#13'line') = 'new\r\rline', 'EscapeString');
  Assert(EscapeString('new'#13#10'line') = 'new\r\nline', 'EscapeString');
  Assert(EscapeString('one'#0#1#31#32'two') = 'one\00\01\31 two', 'EscapeString');

  Assert(EscapeString(#13'new'#10'line'#9#13'and'#10#10#9'more!'#10) =
           '\rnew\nline\t\rand\n\n\tmore!\n', 'EscapeString');

  Assert(EscapeString('custom = escaping', '=') = 'custom \= escaping', 'EscapeString');
  Assert(EscapeString('custom = escaping', '= ') = 'custom\ \=\ escaping', 'EscapeString');

  Assert(UnescapeString('new\nline') = 'new'#10'line', 'UnescapeString');
  Assert(UnescapeString('new\n\rline') = 'new'#10#13'line', 'UnescapeString');
  Assert(UnescapeString('new\r\rline') = 'new'#13#13'line', 'UnescapeString');
  Assert(UnescapeString('new\r\nline') = 'new'#13#10'line', 'UnescapeString');
  Assert(UnescapeString('one\00\01\31 two') = 'one'#0#1#31#32'two', 'UnescapeString');

  Assert(UnescapeString('escaping the end\') = 'escaping the end\', 'UnescapeString');
  Assert(UnescapeString('one code escaping\1') = 'one code escaping\1', 'UnescapeString');
  Assert(UnescapeString('two codes escaping\12') = 'two codes escaping'#12, 'UnescapeString');
  Assert(UnescapeString(StringWithUnnecesaryEscaping) = StringWithUnnecesaryEscaping, 'UnescapeString');

  Assert(UnescapeString('\rnew\nline\t\rand\n\n\tmore!\n') =
           #13'new'#10'line'#9#13'and'#10#10#9'more!'#10, 'UnescapeString');

  Assert(UnescapeString('custom \= escaping', '=') = 'custom = escaping', 'UnescapeString');
  Assert(UnescapeString('custom\ \=\ escaping', '= ') = 'custom = escaping', 'UnescapeString');

  Raw := '';
  Escaped := '';
  for I := 0 to LastUnprintableChar do
  begin
    Raw := Raw + Chr(I) + Chr( Random(30) + LastUnprintableChar + 1 );

    case I of
    09: EscapedChar := 't';
    10: EscapedChar := 'n';
    13: EscapedChar := 'r';
    else
      EscapedChar := Format('%.2d', [I]);
    end;

    Escaped := Escaped + '\' + EscapedChar + Raw[Length(Raw)];
  end;
  Raw := Raw + Raw;
  Escaped := Escaped + Escaped;

  Assert(EscapeString(Raw) = Escaped, 'EscapeString');
  Assert(UnescapeString(Escaped) = Raw, 'UnescapeString');
end;

procedure TestPosFunctions;
begin
  Assert(PosLast ('123', '012301230') = 6, 'PosLast');
  Assert(PosLast ('123', '') = 0, 'PosLast');
  Assert(PosLast ('1234', '012301230') = 0, 'PosLast');
  Assert(PosLastW('123', '012301230') = 6, 'PosLastW');
  Assert(PosLastW('123', '') = 0, 'PosLastW');
  Assert(PosLastW('1234', '012301230') = 0, 'PosLastW');
end;

procedure TestFormattingFunctions;
  function FormatInterval(Millisecs: DWord): WideString;
  begin
    Result := StringReplace( StringUtils.FormatInterval(Millisecs), '.', ',', [] );
  end;
  function FormatSize(Bytes: DWord): WideString;
  begin
    Result := StringReplace( StringUtils.FormatSize(Bytes), '.', ',', [] );
  end;

begin
  StringUtilsLanguage.ThousandsSeparator := ' ';
  Assert(FormatNumber(0) = '0', 'FormatNumber');
  Assert(FormatNumber(1) = '1', 'FormatNumber');
  Assert(FormatNumber(123) = '123', 'FormatNumber');
  Assert(FormatNumber(1234) = '1 234', 'FormatNumber');
  Assert(FormatNumber(12345) = '12 345', 'FormatNumber');
  Assert(FormatNumber(123456) = '123 456', 'FormatNumber');
  Assert(FormatNumber(1234567) = '1 234 567', 'FormatNumber');
  Assert(FormatNumber(12345678) = '12 345 678', 'FormatNumber');
  Assert(FormatNumber(123456789) = '123 456 789', 'FormatNumber');
  Assert(FormatNumber(1234567890) = '1 234 567 890', 'FormatNumber');
  Assert(FormatNumber(4294967295) = '4 294 967 295', 'FormatNumber');     

  Assert(FormatInterval(0) = '0ms', 'FormatInterval');
  Assert(FormatInterval(1) = '1ms', 'FormatInterval');

  Assert(FormatInterval(1000) = '1,00s', 'FormatInterval');
  Assert(FormatInterval(1001) = '1,00s', 'FormatInterval');
  Assert(FormatInterval(1091) = '1,09s', 'FormatInterval');
  Assert(FormatInterval(3001) = '3,00s', 'FormatInterval');
  Assert(FormatInterval(3301) = '3,30s', 'FormatInterval');

  Assert(FormatInterval(60001) = '1,00m', 'FormatInterval');
  Assert(FormatInterval(180851) = '3,01m', 'FormatInterval');
  Assert(FormatInterval(45 * 60000) = '45,00m', 'FormatInterval');
                                                                     
  Assert(FormatInterval(60 * 60000) = '1,00h', 'FormatInterval');
  Assert(FormatInterval(90 * 60000) = '1,50h', 'FormatInterval');
  Assert(FormatInterval(48 * 3600000) = '2,00d', 'FormatInterval');
  Assert(FormatInterval(6 * 24 * 3600000) = '6,00d', 'FormatInterval');

  Assert(FormatInterval(7 * 24 * 3600000) = '1,00w', 'FormatInterval');
  Assert(FormatInterval(8 * 24 * 3600000) = '1,14w', 'FormatInterval');
  Assert(FormatInterval(9 * 24 * 3600000) = '1,29w', 'FormatInterval');
  Assert(FormatInterval(20 * 24 * 3600000) = '2,86w', 'FormatInterval');
  Assert(FormatInterval($FFFFFFFF) = '7,10w', 'FormatInterval');

  Assert(FormatSize(0) = '0 bytes', 'FormatSize');
  Assert(FormatSize(1) = '1 bytes', 'FormatSize');
  Assert(FormatSize(800) = '800 bytes', 'FormatSize');

  Assert(FormatSize(1023) = '1023 bytes', 'FormatSize');
  Assert(FormatSize(1024) = '1,00 Kb', 'FormatSize');
  Assert(FormatSize(1025) = '1,00 Kb', 'FormatSize');        
  Assert(FormatSize(8000) = '7,81 Kb', 'FormatSize');

  Assert(FormatSize(1024 * 1024) = '1,00 Mb', 'FormatSize');
  Assert(FormatSize(5 * 1024 * 1024 - 1) = '5,00 Mb', 'FormatSize');
  Assert(FormatSize(5 * 1024 * 1024) = '5,00 Mb', 'FormatSize');
  Assert(FormatSize(5 * 1024 * 1024 + 1) = '5,00 Mb', 'FormatSize');

  Assert(FormatSize(1024 * 1024 * 1024) = '1,00 Gb', 'FormatSize');
  Assert(FormatSize($FFFFFFFF) = '4,00 Gb', 'FormatSize');
end;

procedure TestPosW;
begin
  Assert( PosW('0', '123') = 0, 'PosW' );
  Assert( PosW('4', '123') = 0, 'PosW' );

  Assert( PosW('1', '123') = 1, 'PosW' );
  Assert( PosW('12', '123') = 1, 'PosW' );
  Assert( PosW('123', '123') = 1, 'PosW' );
  Assert( PosW('1234', '123') = 0, 'PosW' );

  Assert( PosW('2', '123') = 2, 'PosW' );
  Assert( PosW('23', '123') = 2, 'PosW' );
  Assert( PosW('2345', '123') = 0, 'PosW' );

  Assert( PosW('3', '123') = 3, 'PosW' );


  Assert( PosW('12', '112') = 2, 'PosW' );
  Assert( PosW('1122', '1121112221122') = 5, 'PosW' );
end;

procedure TestQuoteAndUnquote;
  procedure Test(Str, Quoted: String; ExpectedEndPos: Integer = -1; Unquoted: String = '');
  var
    EndPos: Integer;
  begin
    Assert( Quoted = Quote(Str, ''''), 'Quote' );
    if ExpectedEndPos = -1 then
      try
        Unquote(Quoted, '''');
        Assert(False, 'Unquote error');
      except
      end
      else
      begin
        Assert( Unquoted = Unquote(Str, '''', EndPos, 1), 'Unquote' );
        Assert( EndPos = ExpectedEndPos, 'Unquote EndPos' );
      end;
  end;
begin
  Test( 'abc '' def " ''''123', 'abc '''' def " ''''''''123', 6, 'abc ' );
  Test( 'abc '''' def " ''''123', 'abc '''''''' def " ''''''''123' );
  Test( 'abc '''' def " ''123', 'abc '''''''' def " ''''123', 15, 'abc '' def " ' ); 
  Test( 'a''b', 'a''''b', 3, 'a' );
  Test( '''b', '''''b', 2, '' );     
  Test( '''', '''''', 2, '' );
  Test( 'a''', 'a''''', 3, 'a' );
end;

begin
  TestMaskMatch;    
  TestExplode;
  TestSplit;
  TestVeryLongExplode;
  TextWrapAndPadText;
  TestTrimFunctions;
  TestEscaping;
  TestPosFunctions;
  TestFormattingFunctions;
  TestPosW;                  
  TestQuoteAndUnquote;

  Assert('a b C D e@f  J  k  אבגדהו¨‎    ' =
         RemoveNonWordChars('a, b. C! D? e@f ^ J + k <> אבגדהו¨‎... - _ [...] \/'), 'RemoveNonWordChars, #1');

  Assert('123 123 123 ' = StrRepeat('123 ', 3), 'StrRepeat');
  Assert('' = StrRepeat('123 ', 0), 'StrRepeat');
  Assert('' = StrRepeat('', 3), 'StrRepeat');

  // 1. Not necessary to check StrReplace thoroughly since it's a direct port of
  //    StrUtils.pas standard unit. So we only test if Unicode is preserved correctly.
  // 2. "WideString() + ..." - not pretty but it lets us avoid Delphi Unicode convertion mess.
  Assert( StrReplace( WideString(#$3042) + WideString(#$3042) + WideString(#$3043),
                      WideString(#$3042) + WideString(#$3043),
                      WideString(#$3044), [])
          = WideString(#$3042) + WideString(#$3044), 'StrReplace'
         );

  WriteLn('All ok.');
  ReadLn
end.