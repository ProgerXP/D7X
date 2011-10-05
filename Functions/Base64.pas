
unit Base64;

interface

{
  References:
    * crypt32.dll (XP version) definitions got from http://www.delphilab.ru/content/view/151/84/
    * Pascal implementation got from (and slightly modified) here:
        http://www.delphisources.ru/pages/faq/base/base64_encode_decode.html
}            

uses Windows;

const
  // Flags:
  // Base64, with certificate beginning and ending headers
  CRYPT_STRING_BASE64HEADER = 0;
  // Base64, without headers
  CRYPT_STRING_BASE64 = 1;
  // Pure binary copy
  CRYPT_STRING_BINARY = 2;
  // Base64, with request beginning and ending headers
  CRYPT_STRING_BASE64REQUESTHEADER = 3;
  // Hexadecimal only
  CRYPT_STRING_HEX = 4;
  // Hexadecimal, with ASCII character display
  CRYPT_STRING_HEXASCII= 5;
  // Base64, with X.509 CRL beginning and ending headers
  CRYPT_STRING_BASE64X509CRLHEADER = 9;
  // Hexadecimal, with address display
  CRYPT_STRING_HEXADDR = 10;
  // Hexadecimal, with ASCII character and address display
  CRYPT_STRING_HEXASCIIADDR = 11;
  // A raw hex string.
  CRYPT_STRING_HEXRAW = 12;

procedure TryToUseCrypt32;
procedure DoNotUseCrypt32;

{ these functions will use Windows' base64 support or use built-in functions in this unit. }
function Base64Encode(Binary: PChar; Length: DWord; out ResultLength: DWord): PChar; overload;
function Base64Decode(Str: PChar; Length: DWord; out ResultLength: DWord): PChar; overload;

{ avoid using the following functions unless you're sure the OS supports them (crypt32.dll). }
function Base64XPEncode(s: PChar; length: dword; out ResultLength: DWord;
  Flags: dword = CRYPT_STRING_BASE64): PChar; overload;
function Base64XPDecode(s: PChar; length: dword; out ResultLength: DWord;
  Flags: dword = CRYPT_STRING_BASE64): PChar; overload;

{ these are slower than XP's (although they generate the same base64) but they can be used on any OS. }
function Base64BuiltInEncode(const inStr: PChar; length: dword; out ResultLength: DWord): PChar; overload;
function Base64BuiltInDecode(const CinLine: PChar; length: dword; out ResultLength: DWord): PChar; overload;

function Base64Encode(Binary: String): String; overload;
function Base64Decode(Str: String): String; overload;
function Base64XPEncode(s: string; Flags: dword = CRYPT_STRING_BASE64): string; overload;
function Base64XPDecode(s: string; Flags: dword = CRYPT_STRING_BASE64): string; overload;
function Base64BuiltInEncode(const inStr: string): string; overload;
function Base64BuiltInDecode(const CinLine: string): string; overload;

implementation

type
  TCryptStringToBinary = function (pszString: PChar; cchString: dword; dwFlags: dword;
         pbBinary: pointer; var pcbBinary: dword; var pdwSkip: dword;
         var pdwFlags: dword): boolean; stdcall;

  TCryptBinaryToString = function (pbBinary: pointer; cbBinary: dword; dwFlags: dword;
         pszString: PChar; var pcchString: dword): boolean; stdcall;   

const
  Crypt32_DLL = 'crypt32.dll';

var
  Crypt32: HMODULE = 0;
  CryptStringToBinary: TCryptStringToBinary;
  CryptBinaryToString: TCryptBinaryToString;

procedure DoNotUseCrypt32;
begin
  if Crypt32 <> 0 then
  begin
    FreeLibrary(Crypt32);
    Crypt32 := 0;
  end;

  CryptStringToBinary := NIL;
  CryptBinaryToString := NIL;
end;                                

procedure TryToUseCrypt32;
begin
  DoNotUseCrypt32;

  try
    Crypt32 := LoadLibrary(Crypt32_DLL);
    if Crypt32 <> 0 then
    begin
      CryptStringToBinary := GetProcAddress(Crypt32, 'CryptStringToBinaryA');
      CryptBinaryToString := GetProcAddress(Crypt32, 'CryptBinaryToStringA');
    end;
  except     
    DoNotUseCrypt32;
  end;
end;

function Base64Encode(Binary: PChar; length: dword; out ResultLength: DWord): PChar;
begin
  if Assigned(CryptBinaryToString) then
    Result := Base64XPEncode(Binary, length, ResultLength)
    else
      Result := Base64BuiltInEncode(Binary, length, ResultLength);
end;

function Base64Decode(Str: PChar; length: dword; out ResultLength: DWord): PChar;
begin
  if Assigned(CryptStringToBinary) then
    Result := Base64XPDecode(Str, length, ResultLength)
    else
      Result := Base64BuiltInDecode(Str, length, ResultLength);
end;

function Base64XPEncode(s: PChar; length: dword; out ResultLength: DWord; Flags: dword = CRYPT_STRING_BASE64): PChar;
begin
  CryptBinaryToString(s, length, Flags, nil, ResultLength);
  getmem(result, ResultLength);
  CryptBinaryToString(s, length, Flags, pointer(result), ResultLength);
end;
 
function Base64XPDecode(s: PChar; length: dword; out ResultLength: DWord; Flags: dword = CRYPT_STRING_BASE64): PChar;
var
  skip: dword;
begin
  CryptStringToBinary(s, length, Flags, nil, ResultLength, skip, Flags);
  getmem(result, ResultLength);
  CryptStringToBinary(s, length, Flags, pointer(result), ResultLength, skip, Flags);
end;

function Base64BuiltInEncode(const inStr: PChar; length: dword; out ResultLength: DWord): PChar;
const
  LINE_LIMIT = 48;
var
  i: dword;

  function Encode_Byte(b: Byte): char;
  const
    Base64Code: string[64] =
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  begin
    Result := Base64Code[(b and $3F)+1];
  end;

  function AddChar: Integer;
  begin
    Result := ResultLength;
    Inc(ResultLength);
  end;

  procedure PutNewLine;
  begin
    Result[AddChar] := #13;
    Result[AddChar] := #10;
  end;

begin
  i := 0;
  ResultLength := 0;
  GetMem(Result, Length * 2);

  if Length <> 0 then
    try
      while i < Length do
      begin
        Result[AddChar] := Encode_Byte(Byte(inStr[i]) shr 2);
        Result[AddChar] := Encode_Byte((Byte(inStr[i]) shl 4) or (Byte(inStr[i+1]) shr 4));
        if i+1 < Length then
          Result[AddChar] := Encode_Byte((Byte(inStr[i+1]) shl 2) or (Byte(inStr[i+2]) shr 6))
        else
          Result[AddChar] := '=';
        if i+2 < Length then
          Result[AddChar] := Encode_Byte(Byte(inStr[i+2]))
        else
          Result[AddChar] := '=';

        Inc(i, 3);

        if I mod LINE_LIMIT = 0 then
          PutNewLine;
      end;

      PutNewLine; // WinXP encoder puts newline at the end; we do the same.
      ReallocMem(Result, ResultLength);
    except
      FreeMem(Result, Length * 2);
      Result := NIL;
      raise;
    end;
end;

function Base64BuiltInDecode(const CinLine: PChar; length: dword; out ResultLength: DWord): PChar;
const
  RESULT_ERROR = -2;
var
  inLineIndex: Integer;
  c: Char;
  x: SmallInt;
  c4: Word;
  StoredC4: array[0..3] of SmallInt;
  InLineLength: Integer;         

  function AddChar: Integer;
  begin
    Result := ResultLength;
    Inc(ResultLength);
  end;
begin
  Result := '';
  inLineIndex := 0;
  c4 := 0;
  InLineLength := Length;

  ResultLength := 0;
  GetMem(Result, Length);
  
  try
    while inLineIndex < InLineLength do
    begin
      while (inLineIndex < InLineLength) and (c4 < 4) do
      begin
        c := CinLine[inLineIndex];
        case c of
          '+'     : x := 62;
          '/'     : x := 63;
          '0'..'9': x := Ord(c) - (Ord('0')-52);
          '='     : x := -1;
          'A'..'Z': x := Ord(c) - Ord('A');
          'a'..'z': x := Ord(c) - (Ord('a')-26);
        else
          x := RESULT_ERROR;
        end;
        if x <> RESULT_ERROR then
        begin
          StoredC4[c4] := x;
          Inc(c4);
        end;
        Inc(inLineIndex);
      end;

      if c4 = 4 then
      begin
        c4 := 0;
        Result[AddChar] := Char((StoredC4[0] shl 2) or (StoredC4[1] shr 4));
        if StoredC4[2] = -1 then 
          inLineIndex := InLineLength
          else
          begin
            Result[AddChar] := Char((StoredC4[1] shl 4) or (StoredC4[2] shr 2));
            if StoredC4[3] = -1 then
              inLineIndex := InLineLength
              else
                Result[AddChar] := Char((StoredC4[2] shl 6) or (StoredC4[3]));
          end;
      end;
    end;

    ReallocMem(Result, ResultLength);
  except
    FreeMem(Result, Length);
    Result := NIL;
    raise;
  end;
end;

function Base64Encode(Binary: String): String;
var
  Buf: Pointer;
  Size: DWord;
begin
  Buf := Base64Encode(@Binary[1], Length(Binary), Size);
  SetLength(Result, Size);
  Move(Buf^, Result[1], Size);
  FreeMem(Buf, Size);
end;

function Base64Decode(Str: String): String;
var
  Buf: Pointer;
  Size: DWord;
begin
  Buf := Base64Decode(@Str[1], Length(Str), Size);
  SetLength(Result, Size);
  Move(Buf^, Result[1], Size);
  FreeMem(Buf, Size);
end;

function Base64XPEncode(s: string; Flags: dword = CRYPT_STRING_BASE64): string;
var
  Buf: Pointer;
  Size: DWord;
begin
  Buf := Base64XPEncode(@s[1], Length(s), Size);
  SetLength(Result, Size);
  Move(Buf^, Result[1], Size);
  FreeMem(Buf, Size);
end;

function Base64XPDecode(s: string; Flags: dword = CRYPT_STRING_BASE64): string;
var
  Buf: Pointer;
  Size: DWord;
begin
  Buf := Base64XPDecode(@s[1], Length(s), Size);
  SetLength(Result, Size);
  Move(Buf^, Result[1], Size);
  FreeMem(Buf, Size);
end;

function Base64BuiltInEncode(const inStr: string): string;
var
  Buf: Pointer;
  Size: DWord;
begin
  Buf := Base64BuiltInEncode(@inStr[1], Length(inStr), Size);
  SetLength(Result, Size);
  Move(Buf^, Result[1], Size);
  FreeMem(Buf, Size);
end;

function Base64BuiltInDecode(const CinLine: string): string;
var
  Buf: Pointer;
  Size: DWord;
begin
  Buf := Base64BuiltInDecode(@CinLine[1], Length(CinLine), Size);
  SetLength(Result, Size);
  Move(Buf^, Result[1], Size);
  FreeMem(Buf, Size);
end;

initialization
  TryToUseCrypt32;

finalization
  DoNotUseCrypt32;

end.
