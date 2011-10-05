program UtilsTest;

{$APPTYPE CONSOLE}

uses
  StringsW,
  Windows,
  SysUtils,
  MMSystem,
  StrUtils,
  Utils;

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

procedure TestIntToBin;
begin
  Assert(IntToBin($1FFFF)       = '00000000 00000001 11111111 11111111', 'IntToBin');
  Assert(IntToBin($0103, 16, 3) = '0 000 000 100 000 011', 'IntToBin');         
  Assert(IntToBin($0103, 2, 99) = '11', 'IntToBin');    
end;

procedure TestFileNameFunctions;
const
  TestFileName = 'c:\dir\to\file.ext';
begin
  Assert(ExtractFilePath(TestFileName) = 'c:\dir\to\', 'ExtractFilePath');
  Assert(ExtractFilePath('') = '', 'ExtractFilePath');
  Assert(ExtractFilePath('c:\') = 'c:\', 'ExtractFilePath');
  Assert(ExtractFilePath('c:') = 'c:', 'ExtractFilePath');
  Assert(ExtractFilePath('dir\dir\') = 'dir\dir\', 'ExtractFilePath');
  Assert(ExtractFilePath('dir\.dir\') = 'dir\.dir\', 'ExtractFilePath');
  Assert(ExtractFilePath('dir\dir\.') = 'dir\dir\', 'ExtractFilePath');
  Assert(ExtractFilePath('dir\dir\..\') = 'dir\dir\..\', 'ExtractFilePath');
  Assert(ExtractFilePath('dir\di.r\') = 'dir\di.r\', 'ExtractFilePath');
  Assert(ExtractFilePath('file.ext') = '', 'ExtractFilePath');

  Assert(ExtractFileName(TestFileName) = 'file.ext', 'ExtractFileName');
  Assert(ExtractFileName('') = '', 'ExtractFileName');
  Assert(ExtractFileName('c:\') = '', 'ExtractFileName');
  Assert(ExtractFileName('c:') = '', 'ExtractFileName');
  Assert(ExtractFileName('dir\dir\') = '', 'ExtractFileName');
  Assert(ExtractFileName('file.ext') = 'file.ext', 'ExtractFileName');
  Assert(ExtractFileName('file.') = 'file.', 'ExtractFileName');
  Assert(ExtractFileName('file') = 'file', 'ExtractFileName');
  Assert(ExtractFileName('dir\file') = 'file', 'ExtractFileName');
  Assert(ExtractFileName('dir\file.') = 'file.', 'ExtractFileName');
  Assert(ExtractFileName('dir\.file') = '.file', 'ExtractFileName');
  Assert(ExtractFileName('di.r\file.ext') = 'file.ext', 'ExtractFileName');

  Assert(ExtractFileExt(TestFileName) = '.ext', 'ExtractFileExt');
  Assert(ExtractFileExt('') = '', 'ExtractFileExt');
  Assert(ExtractFileExt('c:\') = '', 'ExtractFileExt');
  Assert(ExtractFileExt('c:') = '', 'ExtractFileExt');
  Assert(ExtractFileExt('dir\dir\') = '', 'ExtractFileExt');
  Assert(ExtractFileExt('file.ext') = '.ext', 'ExtractFileExt');
  Assert(ExtractFileExt('dir\file') = '', 'ExtractFileExt');
  Assert(ExtractFileExt('dir\file.') = '.', 'ExtractFileExt');
  Assert(ExtractFileExt('dir\.file') = '.file', 'ExtractFileExt');
  Assert(ExtractFileExt('di.r\file.ext') = '.ext', 'ExtractFileExt');

  Assert(ChangeFileExt(TestFileName, '.new') = 'c:\dir\to\file.new', 'ChangeFileExt');
  Assert(ChangeFileExt('', '.new') = '.new', 'ChangeFileExt');
  Assert(ChangeFileExt('c:\', '.new') = 'c:\.new', 'ChangeFileExt');
  Assert(ChangeFileExt('c:', '.new') = 'c:.new', 'ChangeFileExt');
  Assert(ChangeFileExt('dir\dir\', '.new') = 'dir\dir\.new', 'ChangeFileExt');
  Assert(ChangeFileExt('file.ext', '.new') = 'file.new', 'ChangeFileExt');
  Assert(ChangeFileExt('dir\file', '.new') = 'dir\file.new', 'ChangeFileExt');
  Assert(ChangeFileExt('dir\file.', '.new') = 'dir\file.new', 'ChangeFileExt');
  Assert(ChangeFileExt('dir\.file', '.new') = 'dir\.new', 'ChangeFileExt');
  Assert(ChangeFileExt('di.r\file.ext', '.new') = 'di.r\file.new', 'ChangeFileExt');
  Assert(ChangeFileExt('di.r\file.ext', 'new') = 'di.r\filenew', 'ChangeFileExt');
  Assert(ChangeFileExt('di.r\file.ext', '.') = 'di.r\file.', 'ChangeFileExt');      
  Assert(ChangeFileExt('di.r\file.ext', '..new') = 'di.r\file..new', 'ChangeFileExt');
  Assert(ChangeFileExt('di.r\file.ext', '..') = 'di.r\file..', 'ChangeFileExt');
end;

procedure TestParamStrFrom;
begin
  Assert(ParamStrFrom('module.exe arg1 "arg 2" arg3', 0) = 'module.exe', 'ParamStrFrom');
  Assert(ParamStrFrom('module.exe arg1 "arg 2" arg3', 1) = 'arg1', 'ParamStrFrom');
  Assert(ParamStrFrom('module.exe arg1 "arg 2" arg3', 2) = 'arg 2', 'ParamStrFrom');
  Assert(ParamStrFrom('module.exe arg1 "arg 2" arg3', 3) = 'arg3', 'ParamStrFrom');
  Assert(ParamStrFrom('module.exe arg1 "arg 2" arg3', 4) = '', 'ParamStrFrom');
                                                          
  Assert(ParamStrFrom('module.exe  arg1  "arg 2"  arg3', 0) = 'module.exe', 'ParamStrFrom');
  Assert(ParamStrFrom('module.exe  arg1  "arg 2"  arg3', 1) = 'arg1', 'ParamStrFrom');
  Assert(ParamStrFrom('module.exe  arg1  "arg 2"  arg3', 2) = 'arg 2', 'ParamStrFrom');
  Assert(ParamStrFrom('module.exe  arg1  "arg 2"  arg3', 3) = 'arg3', 'ParamStrFrom');
  Assert(ParamStrFrom('module.exe  arg1  "arg 2"  arg3', 4) = '', 'ParamStrFrom');

  Assert(ParamStrFrom('arg" 1', 0) = 'arg 1', 'ParamStrFrom');
  Assert(ParamStrFrom('arg" 1" arg2', 0) = 'arg 1', 'ParamStrFrom');
  Assert(ParamStrFrom('arg" 1" arg2', 1) = 'arg2', 'ParamStrFrom');
  Assert(ParamStrFrom('arg1"" 2" arg"', 0) = 'arg1', 'ParamStrFrom');
  Assert(ParamStrFrom('arg1"" 2" arg', 1) = '2 arg', 'ParamStrFrom');
  Assert(ParamStrFrom('arg1"" 2" arg"', 1) = '2 arg', 'ParamStrFrom');
end;

begin
  TestIntToBin;
  TestFileNameFunctions;
  TestParamStrFrom;

  Assert(IncludeTrailingBackslash('123')  = '123\', 'IncludeTrailingBackslash');
  Assert(IncludeTrailingBackslash('123\') = '123\', 'IncludeTrailingBackslash');
  Assert(ExcludeTrailingBackslash('123')  = '123',  'ExcludeTrailingBackslash');
  Assert(ExcludeTrailingBackslash('123\') = '123',  'ExcludeTrailingBackslash');

  Assert(MaskForBytes(0) = 0, 'MaskForBytes');
  Assert(MaskForBytes(1) = $FF, 'MaskForBytes');
  Assert(MaskForBytes(2) = $FFFF, 'MaskForBytes');
  Assert(MaskForBytes(3) = $FFFFFF, 'MaskForBytes');
  Assert(MaskForBytes(4) = $FFFFFFFF, 'MaskForBytes');

  WriteLn('All ok.');
  ReadLn
end.