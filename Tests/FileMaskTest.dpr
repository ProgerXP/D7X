program FileMaskTest;

{$APPTYPE CONSOLE}

uses
  StringsW,
  FileMask,
  Windows,
  SysUtils,
  Utils;

procedure TestSingle;      
var
  M: TFileMask;  
  I: Integer;

  procedure CheckB(SkipFirst :Boolean = False);
  var
    I: Integer;
  begin
    if not SkipFirst then
      Assert((M.Next = 'Base64Test.dpr') and (M.Current = 'Base64Test.dpr'));
    Assert((M.Next = 'BufStreamTest.dpr') and (M.Current = 'BufStreamTest.dpr'));
    for I := 1 to 100 do
      Assert((M.Next = '') and (M.Current = ''));
  end;

begin
  M := TFileMask.Create;

  M.Mask := 'b*.d?r*';
  CheckB;

  M.CaseSensitive := True;
  M.ToStart;
  for I := 1 to 100 do
    Assert((M.Next = '') and (M.Current = ''));

  M.Mask := 'B*.d?r*';
  Assert(M.Current = 'Base64Test.dpr');
  CheckB(True);
  M.ToStart;
  CheckB;

  M.Free;
end;

procedure TestRecursive;
var
  M: TFileMask;
  I: Integer;
  S: TStringsW;
begin
  M := TFileMask.Create;  
  S := TStringListW.Create;

  M.Mask := '..\bas*';
  M.MaxRecursionDepth := 2;
  while M.Next <> '' do
    Assert((ChangeFileExt(M.Current, '') = '..\Tests\Base64Test') or
           (M.Current = '..\Units\Base64.pas'), M.Current);
  Assert(M.Next = '');
  
  M.MaxRecursionDepth := 1;
  M.ToStart;
  Assert(M.Next = '');

  M.Recursive := True;
  M.Mask := 'RPNitDemo*.d?r';
  Assert(M.Next = 'RPNit\RPNitDemo.dpr');
  Assert(M.Next = '');
  
  M.Mask := 'RPNit';
  Assert(M.Next = 'RPNit');  
  Assert(M.Next = '');

  M.Mask := 'RPNit\';
  FindMask('RPNit\*', S);
  for I := 0 to S.Count - 1 do
    Assert(M.Next = M.Mask + S[I]);
  Assert(M.Next = '');

  M.MaxRecursionDepth := 1;
  M.Mask := '*';
  S.Clear;
  FindMask('*', S);
  for I := 0 to S.Count - 1 do
    Assert(M.Next = S[I], M.Current + ' <> ' + S[I]);
  Assert(M.Next = '');

  M.Recursive := True;
  M.ToStart;
  S.Clear;
  FindAll('', '*', S);
  for I := 0 to S.Count - 1 do
  begin
    while DirectoryExists(M.Next) do {loop};
    Assert((M.Current = S[I]), M.Current + ' <> ' + S[I]);
  end;
  Assert(M.Next = '');

  S.Free;
  M.Free;
end;      

procedure TestMultiple;
var
  M: TFileMasks;
  I: Integer;
begin
  M := TFileMasks.Create;

  M.Masks := 'b*.dp?*, *Util?Test.dpr';
  Assert((M.MaskCount = 2) and (M.AsString('|') = 'b*.dp?*|*Util?Test.dpr'));
  Assert((M.Current = 'Base64Test.dpr'));
  Assert(M.CurrentMask = 0);
  Assert((M.Next = 'BufStreamTest.dpr') and (M.Current = 'BufStreamTest.dpr'));
  Assert(M.CurrentMask = 0);
  Assert((M.Next = 'StringUtilsTest.dpr') and (M.Current = 'StringUtilsTest.dpr'));
  Assert(M.CurrentMask = 1);
  Assert((M.Next = 'UtilsTest.dpr') and (M.Current = 'UtilsTest.dpr'));
  Assert(M.CurrentMask = 1);
  for I := 1 to 100 do
    Assert((M.Next = '') and (M.Current = ''));

  M.CaseSensitive := True;
  M.ToStart;
  Assert((M.Next = 'StringUtilsTest.dpr') and (M.Current = 'StringUtilsTest.dpr'));
  Assert(M.CurrentMask = 1);
  Assert((M.Next = 'UtilsTest.dpr') and (M.Current = 'UtilsTest.dpr'));
  Assert(M.CurrentMask = 1);
  for I := 1 to 100 do
    Assert((M.Next = '') and (M.Current = ''));

  M.Masks := 'B*.dp?*';
  Assert((M.MaskCount = 1) and (M.AsString('|') = 'B*.dp?*'));
  Assert((M.Current = 'Base64Test.dpr'));
  Assert(M.CurrentMask = 0);
  Assert((M.Next = 'BufStreamTest.dpr') and (M.Current = 'BufStreamTest.dpr'));
  Assert(M.CurrentMask = 0);
  for I := 1 to 100 do
    Assert((M.Next = '') and (M.Current = ''));

  M.Recursive := True;
  M.SetFromString('..\Base64.p?? !!! ..\RPNitDemo.dpr', '!!!');
  Assert((M.MaskCount = 2) and (M.AsString('|') = '..\Base64.p??|..\RPNitDemo.dpr'));
  Assert((M.Current = '..\Units\Base64.pas'));
  Assert(M.CurrentMask = 0);
  Assert((M.Next = '..\Tests\RPNit\RPNitDemo.dpr') and (M.Current = '..\Tests\RPNit\RPNitDemo.dpr'));
  Assert(M.CurrentMask = 1);
  for I := 1 to 100 do
    Assert((M.Next = '') and (M.Current = ''));

  M.Free;
end;

begin
  ChDir('Tests');
  Assert(FileExists('Base64Test.dpr'));
  Assert(FileExists('BufStreamTest.dpr'));
  Assert(FileExists('StringUtilsTest.dpr'));
  Assert(FileExists('UtilsTest.dpr'));
  Assert(FileExists('RPNit\RPNitDemo.dpr'));
  Assert(FileExists('..\Units\Base64.pas'));

  TestSingle;
  TestRecursive;
  TestMultiple;

  WriteLn('All ok.');
  ReadLn
end.