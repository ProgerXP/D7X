program SimpatTest;

{$APPTYPE CONSOLE}

{$IFOPT C-}
  {$MESSAGE ERROR 'Please run tests with Assertions $C+ enabled.}
{$ENDIF}

uses
  Windows, SysUtils, Simpat;

procedure BasicT;
  procedure Matches(Expr, S: String; Mismatches: Boolean = False);
  begin
    Assert(TSimpat.Match(Expr, S) = not Mismatches, Expr + ' & ' + S);
  end;

  procedure Mismatches(Expr, S: String; Mismatches: Boolean = False);
  begin
    Matches(Expr, S, True);
  end;

begin
  Matches('ANY', 'a');
  Matches('z|ANY', 'a');   
  Matches('ANY', 'ab');
  Mismatches('a|b', 'c');
  Matches('a|b|', 'c');     
  Matches('a|b|', 'cc');
  Matches('NL', '');
  Matches('NL', SimpatDefaultEOLN);
  Mismatches('abc', 'bc');
  Mismatches('bc', 'abc');
  Matches('a b c', 'abc');
  Matches('[ab]+c', 'bbc');
  Mismatches('[ab]+c', 'bbb');
  Mismatches('[ab]+c', 'ccc');
  Matches('[ab]*c', 'ccc');
  Mismatches('[^ab]+c', 'ccc');
  Mismatches('[^ab]*c', 'ccc');   // range consumes entire input, nothing left for last 'c'.
  Matches('[^ab]+a', 'cca');
  Mismatches('[^ab]+a', 'aaa');
  Matches('[^ab]*a', 'aaa');
  Matches('[a-c]*', '');
  Matches('[a-c]*', 'a');
  Mismatches('[a-c]+', '');
  Matches('[a-c]+', 'a');
  Mismatches('[a-c]', 'd');
  Mismatches('[a-c]', 'A');
  Matches('[a-c-]+', 'a-a-c-');
  Matches('ID', 'a');
  Mismatches('id', 'a');  // case-sensitive char classes.
  Matches('ID', 'ID');
  Mismatches('ID', '.');
  Matches('[\ID]+', 'ID');
  Matches('I D', 'ID');
  Mismatches('I D', 'DI');
  Matches('[\ID]+', 'DI');
  Mismatches('[\ID]+', 'id');
  Mismatches('[\ID]+', 'FG');
  Mismatches('EOLN', '');
  Mismatches('EOF', SimpatDefaultEOLN);
  Matches('EOLN', SimpatDefaultEOLN);
  Matches('EOF', '');
  Matches('[DIG.^]+ EOF', '999^');
  Matches('DIG DIG? DIG?', '9z9');

  Matches('a||b', 'a|b');
  Matches('a|| b', 'a|b');
  Mismatches('a||b', 'a||b');
  try
    TSimpat.Match('a| |b', 'c');
    Assert(False);
  except
    on ESimpatHasEmptyChoice do {pass} else Assert(False);
  end;
end;

begin
  try
    BasicT;

    WriteLn('All fine.');
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
      ReadLn;
    end;
  end;
end.
