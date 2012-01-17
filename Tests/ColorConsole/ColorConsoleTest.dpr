program ColorConsoleTest;

{ ColorConsole unit - part of D7X library :: by Proger_XP
  Public domain :: http://proger.i-forge.net/ColorConsole  }

{$APPTYPE CONSOLE}

uses SysUtils, ColorConsole, Utils;

procedure WriteUsingCC(const Str: WideString);
begin
  try
    WriteColored(Str);
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;
end;

const
  TestStrings: array[0..12] of String = (
    '{w@b  {10}{wi@b ColorConsole demo} :: {yi@b http://proger.i-forge.net/ColorConsole}{NL}}',
    'Hello, {wi world}!',
    'Escaped: {}, {{, {ri {}}',
    'TAB -{ {TAB}-}{x2} variable',
    'String by hex codes: {#b0#b1#B6}',
    'Repeat {x20}20 times',
    '| Abs-X fill to 45th char {45}| useful for tables |',
    '-{80}{i A ruler:}  {wi 1234567890}{80}-{80}',
    '-{79}'#13#10'{i Checking forced newlines:}    {wi .}{79}{NL}-{79}',
    'Nested {@ri red bk ={x2} {g green text}} - normal here.',
    '{i@ Repeating & nested {wi -{70}}}{x2}',

    '{NL}{gi {g ***} Bug-tests {g ***}}',
    '{@wi ----------------------------------------------------------------------------------------------------'#$D#$A'===}');
var
  I: Integer;
begin
  WriteUsingCC(TestStrings[0]);
  WriteLn;

  if ParamStrW(1) <> '' then
  begin
    I := 1;
    repeat
      WriteUsingCC(ParamStrW(I));
      Inc(I);
    until ParamStrW(I) = '';
  end
    else
      for I := 1 to Length(TestStrings) - 1 do
      begin
        WriteUsingCC(TestStrings[I]);
        WriteLn;
      end;

  ReadLn;
end.
