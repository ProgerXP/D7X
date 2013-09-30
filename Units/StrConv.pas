unit StrConv;

interface

uses SysUtils, Windows;

type
  TCodepage = type DWord;

const
  CP_INVALID    = TCodepage(-1);
  CP_ANSI       = CP_ACP;
  CP_OEM        = CP_OEMCP;
  CP_SHIFTJIS   = 932;
  CP_LATIN1     = 1250;

function MinStrConvBufSize(SrcCodepage: TCodepage; Str: String): Integer; overload;
function MinStrConvBufSize(DestCodepage: TCodepage; Wide: WideString): Integer; overload;
function ToWideString(SrcCodepage: TCodepage; Str: String; BufSize: Integer = -1): WideString;
// If Fail = False replaces unconvertable symbols with default OS symbol (usually '?').
function FromWideString(DestCodepage: TCodepage; Str: WideString; BufSize: Integer = -1; Fail: Boolean = False): String;

function CharsetToID(Str: String): TCodepage;
function IdToCharset(ID: TCodepage; GetDescription: Boolean = False): String;

implementation

function MinStrConvBufSize(SrcCodepage: TCodepage; Str: String): Integer;
begin
  Result := MultiByteToWideChar(SrcCodepage, 0, PChar(Str), -1, NIL, 0)
end;

function MinStrConvBufSize(DestCodepage: TCodepage; Wide: WideString): Integer;
begin
  Result := WideCharToMultiByte(DestCodepage, 0, PWideChar(Wide), -1, NIL, 0, NIL, NIL)
end;

function ToWideString(SrcCodepage: TCodepage; Str: String; BufSize: Integer = -1): WideString;
begin
  if BufSize = -1 then
    BufSize := MinStrConvBufSize(SrcCodepage, Str);
  SetLength(Result, BufSIze * 2);
  MultiByteToWideChar(SrcCodepage, 0, PChar(Str), -1, PWideChar(Result), BufSIze);
  SetLength(Result, BufSize - 1)
end;

function FromWideString(DestCodepage: TCodepage; Str: WideString; BufSize: Integer = -1; Fail: Boolean = False): String;
var
  DefCharUsedPtr: PBool;
begin
  if BufSize = -1 then
    BufSize := MinStrConvBufSize(DestCodepage, Str);
  SetLength(Result, BufSIze);
  if Fail then
    DefCharUsedPtr := @Fail
  else
    DefCharUsedPtr := NIL;
  WideCharToMultiByte(DestCodepage, 0, PWideChar(Str), -1, PChar(Result), BufSize, NIL, DefCharUsedPtr);
  SetLength(Result, BufSize - 1);
  if Fail then
    raise EConvertError.CreateFmt('Codepage %d cannot represent all symbols of given string ''%s''.', [DestCodepage, Copy(Str, 1, 50)]);
end;

function CharsetToID(Str: String): TCodepage;
var
  Key: HKEY;
  ValueType, BufSize: DWord;
  Alias: String[255];
begin
  Result := CP_INVALID;

  if RegOpenKeyEx(HKEY_CLASSES_ROOT, PChar('MIME\Database\Charset\' + LowerCase(Str)), 0, KEY_QUERY_VALUE, Key) = ERROR_SUCCESS then
    try
      BufSize := SizeOf(Result);
      ValueType := REG_DWORD;
      if (RegQueryValueEx(Key, 'InternetEncoding', NIL, @ValueType, @Result, @BufSize) <> ERROR_SUCCESS) or
         (BufSize <> SizeOf(Result)) then
      begin
        BufSize := SizeOf(Alias);
        ValueType := REG_SZ;
        if RegQueryValueEx(Key, 'AliasForCharset', NIL, @ValueType, @Alias[1], @BufSize) = ERROR_SUCCESS then
          Result := CharsetToID(Copy(Alias, 1, BufSIze - 1 {= last #0}));
      end;
    finally
      RegCloseKey(Key);
    end;
end;

function IdToCharset(ID: TCodepage; GetDescription: Boolean = False): String;
var
  Key: HKEY;
  ValueType, BufSize: DWord;
  Field: PChar;
begin
  Result := '';

  if RegOpenKeyEx(HKEY_CLASSES_ROOT, PChar('MIME\Database\Codepage\' + IntToStr(ID)), 0, KEY_QUERY_VALUE, Key) = ERROR_SUCCESS then
    try
      SetLength(Result, 4096);
      BufSize := SizeOf(Result);

      if GetDescription then
        Field := 'Description'
        else
          Field := 'BodyCharset';

      ValueType := REG_SZ;
      if RegQueryValueEx(Key, Field, NIL, @ValueType, @Result[1], @BufSize) = ERROR_SUCCESS then
        SetLength(Result, BufSize - 1)
        else
          Result := '';
    finally
      RegCloseKey(Key);
    end;
end;

end.