unit IniFilesW;

interface

uses
  Classes, Windows, SysUtils, IniFiles, StringsW, Registry;

type
  TCustomIniFileW = class
  protected
    FFileName: WideString;

    procedure SetStrings(List: TStringsW); virtual; abstract;
  public
    constructor Create(const FileName: WideString = ''); virtual;
    property FileName: WideString read FFileName;

    procedure LoadFromString(const Str: WideString);
    procedure LoadFromStream(Stream: TStream);

    procedure Clear; virtual; abstract;
    procedure Reload; virtual; abstract;
    function SectionExists(const Section: WideString): Boolean;
    function ReadString(const Section, Ident, Default: WideString): WideString; virtual; abstract;
    procedure WriteString(const Section, Ident, Value: WideString); virtual; abstract;
    function ReadInteger(const Section, Ident: WideString; Default: DWord): DWord; virtual;
    procedure WriteInteger(const Section, Ident: WideString; Value: DWord); virtual;
    function ReadBool(const Section, Ident: WideString; Default: Boolean): Boolean; virtual;
    procedure WriteBool(const Section, Ident: WideString; Value: Boolean); virtual;
    function ReadBinaryStream(const Section, Name: WideString; Value: TStream): Integer; virtual;
    function ReadDate(const Section, Name: WideString; Default: TDateTime): TDateTime; virtual;
    function ReadDateTime(const Section, Name: WideString; Default: TDateTime): TDateTime; virtual;
    function ReadFloat(const Section, Name: WideString; Default: Double): Double; virtual;
    function ReadTime(const Section, Name: WideString; Default: TDateTime): TDateTime; virtual;
    procedure WriteBinaryStream(const Section, Name: WideString; Value: TStream); virtual;
    procedure WriteDate(const Section, Name: WideString; Value: TDateTime); virtual;
    procedure WriteDateTime(const Section, Name: WideString; Value: TDateTime); virtual;
    procedure WriteFloat(const Section, Name: WideString; Value: Double); virtual;
    procedure WriteTime(const Section, Name: WideString; Value: TDateTime); virtual;
    procedure ReadSection(const Section: WideString; Strings: TStringsW;
      Clear: Boolean = True); virtual; abstract;
    procedure ReadSections(Strings: TStringsW); overload; virtual; abstract;
    procedure ReadSectionValues(const Section: WideString; Strings: TStringsW;
      Clear: Boolean = True); overload; virtual; abstract;
    procedure EraseSection(const Section: WideString); virtual; abstract;
    procedure DeleteKey(const Section, Ident: WideString); virtual; abstract;
    procedure UpdateFile; virtual; abstract;
    function ValueExists(const Section, Ident: WideString): Boolean;

    function FirstSection: WideString; virtual; // errors if empty.
    procedure ReadSections(Strings: TStrings); overload;
    procedure ReadSectionValues(const Section: WideString; Strings: TStrings;
      Clear: Boolean = True); overload;
    procedure ReadSectionsPrefixed(Prefix: WideString; Sections: TStrings; StripPrefix: Boolean = False); overload;
    procedure ReadSectionsPrefixed(Prefix: WideString; Sections: TStringsW; StripPrefix: Boolean = False); overload;
    procedure CopyAllFrom(Ini: TCustomIniFileW); virtual;
    procedure CopySectionFrom(Ini: TCustomIniFileW; const Section: WideString); virtual;
  end;

  TMemIniFileW = class (TCustomIniFileW)
  protected
    FSections: THash;

    function AddSection(const Section: WideString): TStringsW;
    function GetCaseSensitive: Boolean;
    procedure LoadValues;
    procedure SetCaseSensitive(Value: Boolean);
  public
    constructor Create(const FileName: WideString = ''); override;
    destructor Destroy; override;

    procedure DeleteKey(const Section, Ident: WideString); override;
    procedure EraseSection(const Section: WideString); override;
    procedure GetStrings(List: TStringsW);
    procedure ReadSection(const Section: WideString; Strings: TStringsW;
      Clear: Boolean = True); override;
    procedure ReadSections(Strings: TStringsW); override;
    procedure ReadSectionValues(const Section: WideString; Strings: TStringsW;
      Clear: Boolean = True); override;
    function ReadString(const Section, Ident, Default: WideString): WideString; override;
    procedure Rename(const FileName: WideString; Reload: Boolean);
    procedure SetStrings(List: TStringsW); override;
    procedure UpdateFile; override;
    procedure WriteString(const Section, Ident, Value: WideString); override;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    function FirstSection: WideString; override;

    procedure Clear; override;
    procedure Reload; override;
    procedure CopyAllFrom(Ini: TCustomIniFileW); override;
    function SectionCount: Integer;
    function ValueCount(const Section: WideString): Integer;
  end;

  // TIniFileW is useful when INI is shared since it doesn't buffer data but
  // reads/writes directory to disk.
  TIniFileW = class (TCustomIniFileW)
  public
    destructor Destroy; override;

    function ReadString(const Section, Ident, Default: WideString): WideString; override;
    procedure WriteString(const Section, Ident, Value: WideString); override;
    procedure ReadSection(const Section: WideString; Strings: TStringsW;
      Clear: Boolean = True); override;
    procedure ReadSections(Strings: TStringsW); override;
    procedure ReadSectionValues(const Section: WideString; Strings: TStringsW;
      Clear: Boolean = True); override;
    procedure EraseSection(const Section: WideString); override;
    procedure DeleteKey(const Section, Ident: WideString); override;
    procedure UpdateFile; override;

    procedure Clear; override;
    procedure Reload; override;
  end;

  // todo: implement WideString version of TRegIniFile (and maybe of TRegistry as well).
  TRegistryIniFileW = class (TCustomIniFileW)
  protected
    FRegIniFile: TRegIniFile;
  public
    constructor Create(const KeyName: WideString); overload; override;
    constructor Create(const KeyName: WideString; AAccess: DWord); reintroduce; overload;
    destructor Destroy; override;
    function ReadDate(const Section, Name: WideString; Default: TDateTime): TDateTime; override;
    function ReadDateTime(const Section, Name: WideString; Default: TDateTime): TDateTime; override;
    function ReadInteger(const Section, Ident: WideString; Default: DWord): DWord; override;
    function ReadFloat(const Section, Name: WideString; Default: Double): Double; override;
    function ReadString(const Section, Ident, Default: WideString): WideString; override;
    function ReadTime(const Section, Name: WideString; Default: TDateTime): TDateTime; override;
    function ReadBinaryStream(const Section, Name: WideString; Value: TStream): Integer; override;
    procedure WriteDate(const Section, Name: WideString; Value: TDateTime); override;
    procedure WriteDateTime(const Section, Name: WideString; Value: TDateTime); override;
    procedure WriteFloat(const Section, Name: WideString; Value: Double); override;
    procedure WriteInteger(const Section, Ident: WideString; Value: DWord); override;
    procedure WriteString(const Section, Ident, Value: WideString); override;
    procedure WriteTime(const Section, Name: WideString; Value: TDateTime); override;
    procedure WriteBinaryStream(const Section, Name: WideString; Value: TStream); override;
    procedure ReadSection(const Section: WideString; Strings: TStringsW;
      Clear: Boolean = True); overload; override;
    procedure ReadSection(const Section: WideString; Strings: TStrings); reintroduce; overload;
    procedure ReadSections(Strings: TStringsW); overload; override;
    procedure ReadSections(Strings: TStrings); overload;
    procedure ReadSectionValues(const Section: WideString; Strings: TStringsW;
      Clear: Boolean = True); overload; override;
    procedure ReadSectionValues(const Section: WideString; Strings: TStrings); overload;
    procedure EraseSection(const Section: WideString); override;
    procedure DeleteKey(const Section, Ident: WideString); override;
    procedure UpdateFile; override;

    property RegIniFile: TRegIniFile read FRegIniFile;
  end;

implementation

uses StringUtils, Utils, FileStreamW;

type
  TOpenRegistry = class (TRegistry);
  TOpenHash = class (THash);

const
  KeyValueSepar       = '=';
  // From RtlConsts.pas:
  SIniFileWriteError  = 'Unable to write to %s';

{ TCustomIniFileW }

constructor TCustomIniFileW.Create(const FileName: WideString);
begin
  FFileName := FileName;
end;

procedure TCustomIniFileW.LoadFromStream(Stream: TStream);
var
  List: TStringListW;
begin
  List := TStringListW.Create;
  try
    List.LoadFromStream(Stream);
    SetStrings(List);
  finally
    List.Free;
  end;
end;

procedure TCustomIniFileW.LoadFromString(const Str: WideString);
var
  List: TStringListW;
begin
  List := TStringListW.Create;
  try
    List.Text := Str;
    SetStrings(List);
  finally
    List.Free
  end
end;

function TCustomIniFileW.ReadBinaryStream(const Section, Name: WideString; Value: TStream): Integer;
var
  Text: String;
  Stream: TMemoryStream;
  Pos: Integer;
begin
  Text := ReadString(Section, Name, '');
  if Text <> '' then
  begin
    if Value is TMemoryStream then
      Stream := TMemoryStream(Value)
    else
      Stream := TMemoryStream.Create;

    try
      Pos := Stream.Position;
      Stream.SetSize(Stream.Size + Length(Text) div 2);
      Classes.HexToBin(PChar(Text), PChar(Integer(Stream.Memory) + Stream.Position), Length(Text) div 2);
      Stream.Position := Pos;
      if Value <> Stream then
        Value.CopyFrom(Stream, Length(Text) div 2);
      Result := Stream.Size - Pos;
    finally
      if Value <> Stream then
        Stream.Free;
    end;
  end
  else
    Result := 0;
end;

function TCustomIniFileW.ReadBool(const Section, Ident: WideString; Default: Boolean): Boolean;
begin
  Result := ReadInteger(Section, Ident, Ord(Default)) <> 0;
end;

function TCustomIniFileW.ReadDate(const Section, Name: WideString; Default: TDateTime): TDateTime;
var
  DateStr: WideString;
begin
  DateStr := ReadString(Section, Name, '');
  Result := Default;
  if DateStr <> '' then
  try
    Result := StrToDate(DateStr);
  except
    on EConvertError do
      // Ignore EConvertError exceptions
    else
      raise;
  end;
end;

function TCustomIniFileW.ReadDateTime(const Section, Name: WideString; Default: TDateTime): TDateTime;
var
  DateStr: WideString;
begin
  DateStr := ReadString(Section, Name, '');
  Result := Default;
  if DateStr <> '' then
  try
    Result := StrToDateTime(DateStr);
  except
    on EConvertError do
      // Ignore EConvertError exceptions
    else
      raise;
  end;
end;

function TCustomIniFileW.ReadFloat(const Section, Name: WideString; Default: Double): Double;
var
  FloatStr: WideString;
begin
  FloatStr := ReadString(Section, Name, '');
  Result := Default;
  if FloatStr <> '' then
  try
    Result := StrToFloat(FloatStr);
  except
    on EConvertError do
      // Ignore EConvertError exceptions
    else
      raise;
  end;
end;

function TCustomIniFileW.ReadInteger(const Section, Ident: WideString; Default: DWord): DWord;
var
  IntStr: String;
begin
  IntStr := ReadString(Section, Ident, '');
  if (Length(IntStr) > 2) and (IntStr[1] = '0') and
     ((IntStr[2] = 'X') or (IntStr[2] = 'x')) then
    IntStr := '$' + Copy(IntStr, 3, Maxint);
  Result := StrToIntDef(IntStr, Default);
end;

function TCustomIniFileW.FirstSection: WideString;
var
  Sections: TStringListW;
begin
  Sections := TStringListW.Create;
  try
    ReadSections(Sections);
    Result := Sections[0];
  finally
    Sections.Free;
  end;
end;

procedure TCustomIniFileW.ReadSections(Strings: TStrings);
var
  Unic: TStringListW;
begin
  Unic := TStringListW.Create;
  try
    ReadSections(Unic);
    Unic.CopyTo(Strings)
  finally
    Unic.Free
  end;
end;

procedure TCustomIniFileW.ReadSectionValues(const Section: WideString; Strings: TStrings;
  Clear: Boolean = True);
var
  UnicList: TStringListW;
begin
  UnicList := TStringListW.Create;
  try
    ReadSectionValues(Section, UnicList);
    if Clear then
      Strings.Clear;
    UnicList.AppendTo(Strings);
  finally
    UnicList.Free
  end;
end;

procedure TCustomIniFileW.ReadSectionsPrefixed(Prefix: WideString; Sections: TStrings; StripPrefix: Boolean = False);
var
  Unic: TStringListW;
begin
  Unic := TStringListW.Create;
  try
    ReadSectionsPrefixed(Prefix, Unic, StripPrefix);
    Unic.AppendTo(Sections);
  finally
    Unic.Free;
  end;
end;

procedure TCustomIniFileW.ReadSectionsPrefixed(Prefix: WideString; Sections: TStringsW; StripPrefix: Boolean = False);
var
  All: TStringListW;
  I: Integer;
begin
  All := TStringListW.Create;
  try
    ReadSections(All);

    for I := 0 to All.Count - 1 do
      if PosW(Prefix, All[I]) <> 0 then
        if StripPrefix then
          Sections.Add( Copy(All[I], Length(Prefix) + 1, $FFFF) )
          else
            Sections.Add(All[I]);
  finally
    All.Free;
  end;
end;

function TCustomIniFileW.ReadTime(const Section, Name: WideString; Default: TDateTime): TDateTime;
var
  TimeStr: WideString;
begin
  TimeStr := ReadString(Section, Name, '');
  Result := Default;
  if TimeStr <> '' then
  try
    Result := StrToTime(TimeStr);
  except
    on EConvertError do
      // Ignore EConvertError exceptions
    else
      raise;
  end;
end;

function TCustomIniFileW.SectionExists(const Section: WideString): Boolean;
var
  S: TStringListW;
begin
  S := TStringListW.Create;
  try
    ReadSection(Section, S);
    Result := S.Count > 0;
  finally
    S.Free;
  end;
end;

function TCustomIniFileW.ValueExists(const Section, Ident: WideString): Boolean;
var
  S: TStringListW;
begin
  S := TStringListW.Create;
  try
    ReadSection(Section, S);
    Result := S.IndexOf(Ident) > -1;
  finally
    S.Free;
  end;
end;

procedure TCustomIniFileW.WriteBinaryStream(const Section, Name: WideString; Value: TStream);
var
  Text: String;
  Stream: TMemoryStream;
begin
  SetLength(Text, (Value.Size - Value.Position) * 2);
  if Length(Text) > 0 then
  begin
    if Value is TMemoryStream then
      Stream := TMemoryStream(Value)
    else
      Stream := TMemoryStream.Create;

    try
      if Stream <> Value then
      begin
        Stream.CopyFrom(Value, Value.Size - Value.Position);
        Stream.Position := 0;
      end;
      Classes.BinToHex(PChar(Integer(Stream.Memory) + Stream.Position), PChar(Text),
        Stream.Size - Stream.Position);
    finally
      if Value <> Stream then
        Stream.Free;
    end;
  end;
  WriteString(Section, Name, Text);
end;

procedure TCustomIniFileW.WriteBool(const Section, Ident: WideString;
  Value: Boolean);
const
  Values: array[Boolean] of String = ('0', '1');
begin
  WriteString(Section, Ident, Values[Value]);
end;

procedure TCustomIniFileW.WriteDate(const Section, Name: WideString;
  Value: TDateTime);
begin
  WriteString(Section, Name, DateToStr(Value));
end;

procedure TCustomIniFileW.WriteDateTime(const Section, Name: WideString; Value: TDateTime);
begin
  WriteString(Section, Name, DateTimeToStr(Value));
end;

procedure TCustomIniFileW.WriteFloat(const Section, Name: WideString;
  Value: Double);
begin
  WriteString(Section, Name, FloatToStr(Value));
end;

procedure TCustomIniFileW.WriteInteger(const Section, Ident: WideString;
  Value: DWord);
begin
  WriteString(Section, Ident, IntToStr(Value));
end;

procedure TCustomIniFileW.WriteTime(const Section, Name: WideString;
  Value: TDateTime);
begin
  WriteString(Section, Name, TimeToStr(Value));
end;

procedure TCustomIniFileW.CopyAllFrom(Ini: TCustomIniFileW);
var
  S: TStringListW;
  I: Integer;
begin
  S := TStringListW.Create;
  try
    Ini.ReadSections(S);
    for I := 0 to S.Count - 1 do
      CopySectionFrom(Ini, S[I]);
  finally
    S.Free;
  end;
end;

procedure TCustomIniFileW.CopySectionFrom(Ini: TCustomIniFileW; const Section: WideString);
var
  S: TStringListW;
  I: Integer;
begin
  S := TStringListW.Create;
  try
    Ini.ReadSectionValues(Section, S);
    for I := 0 to S.Count - 1 do
      WriteString(Section, S.Names[I], S.ValueFromIndex[I]);
  finally
    S.Free;
  end;
end;

{ TMemIniFileW }

constructor TMemIniFileW.Create(const FileName: WideString);
begin
  inherited;
  FSections := THash.Create;
  LoadValues;
end;

destructor TMemIniFileW.Destroy;
begin
  if FSections <> NIL then
    Clear;
  FSections.Free;
  inherited;
end;

function TMemIniFileW.AddSection(const Section: WideString): TStringsW;
begin
  Result := THash.Create;
  try
    THash(Result).CaseSensitive := CaseSensitive;
    FSections.AddObject(Section, Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TMemIniFileW.Clear;
var
  I: Integer;
begin
  for I := 0 to FSections.Count - 1 do
    TObject(FSections.Objects[I]).Free;
  FSections.Clear;
end;

procedure TMemIniFileW.Reload;
begin
  LoadValues;
end;

procedure TMemIniFileW.DeleteKey(const Section, Ident: WideString);
var
  I, J: Integer;
  Strings: TStringsW;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
  begin
    Strings := TStringsW(FSections.Objects[I]);
    J := Strings.IndexOfName(Ident);
    if J >= 0 then
      Strings.Delete(J);
  end;
end;

procedure TMemIniFileW.EraseSection(const Section: WideString);
var
  I: Integer;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
  begin
    TStringsW(FSections.Objects[I]).Free;
    FSections.Delete(I);
  end;
end;

function TMemIniFileW.GetCaseSensitive: Boolean;
begin
  Result := FSections.CaseSensitive;
end;

procedure TMemIniFileW.GetStrings(List: TStringsW);
var
  I, J: Integer;
  Strings: TStringsW;
begin
  List.BeginUpdate;
  try
    for I := 0 to FSections.Count - 1 do
    begin
      List.Add('[' + FSections[I] + ']');
      Strings := TStringsW(FSections.Objects[I]);
      for J := 0 to Strings.Count - 1 do List.Add(Strings[J]);
      List.Add('');
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TMemIniFileW.LoadValues;
var
  F: TFileStreamW;
begin
  if (FileName <> '') and FileExists(FileName) then
  begin
    F := TFileStreamW.Create(FIleName, fmOpenRead);
    try
      LoadFromStream(F);
    finally
      F.Free;
    end
  end
  else
    Clear;
end;

procedure TMemIniFileW.ReadSection(const Section: WideString; Strings: TStringsW;
  Clear: Boolean = True);
var
  I, J: Integer;
  SectionStrings: TStringsW;
begin
  Strings.BeginUpdate;
  try
    if Clear then
      Strings.Clear;

    I := FSections.IndexOf(Section);
    if I >= 0 then
    begin
      SectionStrings := TStringsW(FSections.Objects[I]);
      for J := 0 to SectionStrings.Count - 1 do
        Strings.Add(SectionStrings.Names[J]);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TMemIniFileW.ReadSections(Strings: TStringsW);
begin
  Strings.Assign(FSections);
end;

procedure TMemIniFileW.ReadSectionValues(const Section: WideString; Strings: TStringsW;
  Clear: Boolean = True);
var
  I: Integer;
begin
  Strings.BeginUpdate;
  try
    if Clear then
      Strings.Clear;

    I := FSections.IndexOf(Section);
    if I >= 0 then
      TStringsW(FSections.Objects[I]).AppendTo(Strings);
  finally
    Strings.EndUpdate;
  end;
end;

function TMemIniFileW.ReadString(const Section, Ident, Default: WideString): WideString;
var
  I: Integer;
  Strings: TStringsW;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
  begin
    Strings := TStringsW(FSections.Objects[I]);
    I := Strings.IndexOfName(Ident);
    if I >= 0 then
    begin
      Result := Copy(Strings[I], Length(Ident) + 2, Maxint);
      Exit;
    end;
  end;
  Result := Default;
end;

procedure TMemIniFileW.Rename(const FileName: WideString; Reload: Boolean);
begin
  FFileName := FileName;
  if Reload then
    LoadValues;
end;

procedure TMemIniFileW.SetCaseSensitive(Value: Boolean);
var
  I: Integer;
begin
  if Value <> FSections.CaseSensitive then
  begin
    FSections.CaseSensitive := Value;
    for I := 0 to FSections.Count - 1 do
      with TOpenHash(FSections.Objects[I]) do
      begin
        CaseSensitive := Value;
        Changed;
      end;
    TOpenHash(FSections).Changed;
  end;
end;

procedure TMemIniFileW.SetStrings(List: TStringsW);
var
  I, J: Integer;
  S: WideString;
  Strings: TStringsW;
begin
  Clear;
  Strings := NIL;
  for I := 0 to List.Count - 1 do
  begin
    S := Trim(List[I]);
    if (S <> '') and (S[1] <> ';') and (S[1] <> '#') then
      if (S[1] = '[') and (S[Length(S)] = ']') then
      begin
        Delete(S, 1, 1);
        SetLength(S, Length(S)-1);
        Strings := AddSection(Trim(S));
      end
      else
        if Strings <> NIL then
        begin
          J := PosW(KeyValueSepar, S);
          if J > 0 then // remove spaces before and after '='
            Strings.Add(Trim(Copy(S, 1, J-1)) + KeyValueSepar + Trim(Copy(S, J+1, MaxInt)) )
          else
            Strings.Add(S);
        end;
  end;
end;

procedure TMemIniFileW.UpdateFile;
var
  List: TStringListW;
  F: TFileStreamW;
begin
  List := TStringListW.Create;
  F := TFileStreamW.Create(FileName, fmCreate);

  try
    GetStrings(List);
    List.SaveToStream(F);
  finally
    F.Free;
    List.Free
  end;
end;

procedure TMemIniFileW.WriteString(const Section, Ident, Value: WideString);
var
  I: Integer;
  S: WideString;
  Strings: TStringsW;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
    Strings := TStringsW(FSections.Objects[I])
  else
    Strings := AddSection(Section);
  S := Ident + KeyValueSepar + Value;
  I := Strings.IndexOfName(Ident);
  if I >= 0 then
    Strings[I] := S
  else
    Strings.Add(S);
end;

function TMemIniFileW.FirstSection: WideString;
begin
  Result := FSections[0];
end;

procedure TMemIniFileW.CopyAllFrom(Ini: TCustomIniFileW);
var
  S: TStringListW;
begin
  if Ini.InheritsFrom(TMemIniFileW) then
  begin
    S := TStringListW.Create;
    try
      (Ini as TMemIniFileW).GetStrings(S);
      SetStrings(S);
    finally
      S.Free;
    end;
  end
    else
      inherited;
end;

function TMemIniFileW.SectionCount: Integer;
begin
  Result := FSections.Count;
end;

function TMemIniFileW.ValueCount(const Section: WideString): Integer;
var
  I: Integer;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
    Result := TStringsW(FSections.Objects[I]).Count
  else
    Result := 0;
end;

{ TIniFileW }

destructor TIniFileW.Destroy;
begin
  UpdateFile;
  inherited;
end;

function TIniFileW.ReadString(const Section, Ident, Default: WideString): WideString;
var
  Buffer: array[0..2047] of WideChar;
  Len: DWord;
begin
  Len := GetPrivateProfileStringW(PWideChar(Section), PWideChar(Ident), PWideChar(Default), Buffer,
                                  Length(Buffer), PWideChar(FFileName));
  Result := Copy(Buffer, 1, Len);
end;

procedure TIniFileW.WriteString(const Section, Ident, Value: WideString);
begin
  if not WritePrivateProfileStringW(PWideChar(Section), PWideChar(Ident),
                                   PWideChar(Value), PWideChar(FFileName)) then
    raise EIniFileException.CreateFmt(SIniFileWriteError, [FileName]);
end;

procedure TIniFileW.ReadSections(Strings: TStringsW);
const
  BufLength = 16384;
var
  Buffer, P: PWideChar;
begin
  GetMem(Buffer, BufLength * 2);
  try
    Strings.BeginUpdate;
    try
      Strings.Clear;
      if GetPrivateProfileStringW(NIL, NIL, NIL, Buffer, BufLength,
        PWideChar(FFileName)) <> 0 then
      begin
        P := Buffer;
        while P^ <> #0 do
        begin
          Strings.Add(P);
          Inc(P, LStrLenW(P) + 1);
        end;
      end;
    finally
      Strings.EndUpdate;
    end;
  finally
    FreeMem(Buffer, BufLength * 2);
  end;
end;

procedure TIniFileW.ReadSection(const Section: WideString; Strings: TStringsW;
  Clear: Boolean = True);
const
  BufLength = 16384;
var
  Buffer, P: PWideChar;
begin
  GetMem(Buffer, BufLength * 2);
  try
    Strings.BeginUpdate;
    try
      if Clear then
        Strings.Clear;

      if GetPrivateProfileStringW(PWideChar(Section), NIL, NIL, Buffer, BufLength,
        PWideChar(FFileName)) <> 0 then
      begin
        P := Buffer;
        while P^ <> #0 do
        begin
          Strings.Add(P);
          Inc(P, LStrLenW(P) + 1);
        end;
      end;
    finally
      Strings.EndUpdate;
    end;
  finally
    FreeMem(Buffer, BufLength * 2);
  end;
end;

procedure TIniFileW.ReadSectionValues(const Section: WideString; Strings: TStringsW;
  Clear: Boolean = True);
var
  KeyList: TStringListW;
  I: Integer;
begin
  KeyList := TStringListW.Create;
  try
    ReadSection(Section, KeyList);
    Strings.BeginUpdate;
    try
      if Clear then
        Strings.Clear;

      for I := 0 to KeyList.Count - 1 do
        Strings.Add(KeyList[I] + KeyValueSepar + ReadString(Section, KeyList[I], ''))
    finally
      Strings.EndUpdate;
    end;
  finally
    KeyList.Free;
  end;
end;

procedure TIniFileW.EraseSection(const Section: WideString);
begin
  if not WritePrivateProfileStringW(PWideChar(Section), NIL, NIL, PWideChar(FFileName)) then
    raise EIniFileException.CreateFmt(SIniFileWriteError, [FileName]);
end;

procedure TIniFileW.DeleteKey(const Section, Ident: WideString);
begin
  WritePrivateProfileStringW(PWideChar(Section), PWideChar(Ident), NIL, PWideChar(FFileName));
end;

procedure TIniFileW.UpdateFile;
begin
  WritePrivateProfileStringW(NIL, NIL, NIL, PWideChar(FFileName));
end;

procedure TIniFileW.Clear;
begin
  DeleteFileW(PWideChar(FFileName));
  Reload;
end;

procedure TIniFileW.Reload;
begin
  { TIniFileW doesn't buffer anything, each value is already read from disk by means of WinAPI calls. }
end;

{ TRegistryIniFileW }

constructor TRegistryIniFileW.Create(const KeyName: WideString);
begin
  Create(FileName, KEY_ALL_ACCESS);
end;

constructor TRegistryIniFileW.Create(const KeyName: WideString; AAccess: DWord);
begin
  inherited Create(FileName);
  FRegIniFile := TRegIniFile.Create(FileName, AAccess);
end;

destructor TRegistryIniFileW.Destroy;
begin
  FRegIniFile.Free;
  inherited;
end;

function TRegistryIniFileW.ReadString(const Section, Ident, Default: WideString): WideString;
begin
  Result := FRegIniFile.ReadString(Section, Ident, Default);
end;

function TRegistryIniFileW.ReadDate(const Section, Name: WideString; Default: TDateTime): TDateTime;
var
  Key, OldKey: HKEY;
begin
  with TOpenRegistry(FRegIniFile) do
  begin
    Key := GetKey(Section);
    if Key <> 0 then
    try
      OldKey := CurrentKey;
      SetCurrentKey(Key);
      try
        if ValueExists(Name) then
          Result := ReadDate(Name)
        else Result := Default;
      finally
        SetCurrentKey(OldKey);
      end;
    finally
      RegCloseKey(Key);
    end else Result := Default;
  end;
end;

function TRegistryIniFileW.ReadDateTime(const Section, Name: WideString; Default: TDateTime): TDateTime;
var
  Key, OldKey: HKEY;
begin
  with TOpenRegistry(FRegIniFile) do
  begin
    Key := GetKey(Section);
    if Key <> 0 then
    try
      OldKey := CurrentKey;
      SetCurrentKey(Key);
      try
        if ValueExists(Name) then
          Result := ReadDateTime(Name)
        else Result := Default;
      finally
        SetCurrentKey(OldKey);
      end;
    finally
      RegCloseKey(Key);
    end else Result := Default;
  end;
end;

function TRegistryIniFileW.ReadFloat(const Section, Name: WideString; Default: Double): Double;
var
  Key, OldKey: HKEY;
begin
  with TOpenRegistry(FRegIniFile) do
  begin
    Key := GetKey(Section);
    if Key <> 0 then
    try
      OldKey := CurrentKey;
      SetCurrentKey(Key);
      try
        if ValueExists(Name) then
          Result := ReadFloat(Name)
        else Result := Default;
      finally
        SetCurrentKey(OldKey);
      end;
    finally
      RegCloseKey(Key);
    end else Result := Default;
  end;
end;

function TRegistryIniFileW.ReadInteger(const Section, Ident: WideString; Default: DWord): DWord;
var
  Key, OldKey: HKEY;
begin
  with TOpenRegistry(FRegIniFile) do
  begin
    Key := GetKey(Section);
    if Key <> 0 then
    try
      OldKey := CurrentKey;
      SetCurrentKey(Key);
      try
        Result := Default;
        if ValueExists(Ident) then
          if GetDataType(Ident) = rdString then
            Result := StrToIntDef(ReadString(Ident), Default)
          else Result := ReadInteger(Ident);
      finally
        SetCurrentKey(OldKey);
      end;
    finally
      RegCloseKey(Key);
    end
    else Result := Default;
  end;
end;

function TRegistryIniFileW.ReadTime(const Section, Name: WideString; Default: TDateTime): TDateTime;
var
  Key, OldKey: HKEY;
begin
  with TOpenRegistry(FRegIniFile) do
  begin
    Key := GetKey(Section);
    if Key <> 0 then
    try
      OldKey := CurrentKey;
      SetCurrentKey(Key);
      try
        if ValueExists(Name) then
          Result := ReadTime(Name)
        else Result := Default;
      finally
        SetCurrentKey(OldKey);
      end;
    finally
      RegCloseKey(Key);
    end else Result := Default;
  end;
end;

function TRegistryIniFileW.ReadBinaryStream(const Section, Name: WideString; Value: TStream): Integer;
var
  RegData: TRegDataType;
  Info: TRegDataInfo;
  Key, OldKey: HKEY;
  Stream: TMemoryStream;
begin
  Result := 0;
  with RegIniFile do
  begin
    Key := TOpenRegistry(FRegIniFile).GetKey(Section);
    if Key <> 0 then
    try
      OldKey := CurrentKey;
      TOpenRegistry(FRegIniFile).SetCurrentKey(Key);
      try
        if ValueExists(Name) then
        begin
          if GetDataInfo(Name, Info) then
          begin
            Result := Info.DataSize;
            RegData := Info.RegData;
            if Value is TMemoryStream then
              Stream := TMemoryStream(Value)
            else Stream := TMemoryStream.Create;
            try
              if (RegData = rdBinary) or (RegData = rdUnknown) then
              begin
                Stream.Size := Stream.Position + Info.DataSize;
                Result := ReadBinaryData(Name,
                  Pointer(Integer(Stream.Memory) + Stream.Position)^, Stream.Size);
                if Stream <> Value then Value.CopyFrom(Stream, Stream.Size - Stream.Position);
              end;
            finally
              if Stream <> Value then Stream.Free;
            end;
          end;
        end;
      finally
        TOpenRegistry(FRegIniFile).SetCurrentKey(OldKey);
      end;
    finally
      RegCloseKey(Key);
    end;
  end;
end;

procedure TRegistryIniFileW.WriteDate(const Section, Name: WideString; Value: TDateTime);
var
  Key, OldKey: HKEY;
begin
  with TOpenRegistry(FRegIniFile) do
  begin
    CreateKey(Section);
    Key := GetKey(Section);
    if Key <> 0 then
    try
      OldKey := CurrentKey;
      SetCurrentKey(Key);
      try
        WriteDate(Name, Value);
      finally
        SetCurrentKey(OldKey);
      end;
    finally
      RegCloseKey(Key);
    end;
  end;
end;

procedure TRegistryIniFileW.WriteDateTime(const Section, Name: WideString; Value: TDateTime);
var
  Key, OldKey: HKEY;
begin
  with TOpenRegistry(FRegIniFile) do
  begin
    CreateKey(Section);
    Key := GetKey(Section);
    if Key <> 0 then
    try
      OldKey := CurrentKey;
      SetCurrentKey(Key);
      try
        WriteDateTime(Name, Value);
      finally
        SetCurrentKey(OldKey);
      end;
    finally
      RegCloseKey(Key);
    end;
  end;
end;

procedure TRegistryIniFileW.WriteFloat(const Section, Name: WideString; Value: Double);
var
  Key, OldKey: HKEY;
begin
  with TOpenRegistry(FRegIniFile) do
  begin
    CreateKey(Section);
    Key := GetKey(Section);
    if Key <> 0 then
    try
      OldKey := CurrentKey;
      SetCurrentKey(Key);
      try
        WriteFloat(Name, Value);
      finally
        SetCurrentKey(OldKey);
      end;
    finally
      RegCloseKey(Key);
    end;
  end;
end;

procedure TRegistryIniFileW.WriteInteger(const Section, Ident: WideString; Value: DWord);
var
  Key, OldKey: HKEY;
begin
  with TOpenRegistry(FRegIniFile) do
  begin
    CreateKey(Section);
    Key := GetKey(Section);
    if Key <> 0 then
    try
      OldKey := CurrentKey;
      SetCurrentKey(Key);
      try
        if ValueExists(Ident) and (GetDataType(Ident) = rdString) then
          WriteString(Ident, IntToStr(Value))
        else WriteInteger(Ident, Value);
      finally
        SetCurrentKey(OldKey);
      end;
    finally
      RegCloseKey(Key);
    end;
  end;
end;

procedure TRegistryIniFileW.WriteTime(const Section, Name: WideString; Value: TDateTime);
var
  Key, OldKey: HKEY;
begin
  with TOpenRegistry(FRegIniFile) do
  begin
    CreateKey(Section);
    Key := GetKey(Section);
    if Key <> 0 then
    try
      OldKey := CurrentKey;
      SetCurrentKey(Key);
      try
        WriteTime(Name, Value);
      finally
        SetCurrentKey(OldKey);
      end;
    finally
      RegCloseKey(Key);
    end;
  end;
end;

procedure TRegistryIniFileW.WriteBinaryStream(const Section, Name: WideString;
  Value: TStream);
var
  Key, OldKey: HKEY;
  Stream: TMemoryStream;
begin
  with RegIniFile do
  begin
    CreateKey(Section);
    Key := TOpenRegistry(FRegIniFile).GetKey(Section);
    if Key <> 0 then
    try
      OldKey := CurrentKey;
      if Value is TMemoryStream then
        Stream := TMemoryStream(Value)
      else Stream := TMemoryStream.Create;
      try
        if Stream <> Value then
        begin
          Stream.CopyFrom(Value, Value.Size - Value.Position);
          Stream.Position := 0;
        end;
        TOpenRegistry(FRegIniFile).SetCurrentKey(Key);
        try
          WriteBinaryData(Name, Pointer(Integer(Stream.Memory) + Stream.Position)^,
            Stream.Size - Stream.Position);
        finally
          TOpenRegistry(FRegIniFile).SetCurrentKey(OldKey);
        end;
      finally
        if Value <> Stream then Stream.Free;
      end;
    finally
      RegCloseKey(Key);
    end;
  end;
end;

procedure TRegistryIniFileW.WriteString(const Section, Ident, Value: WideString);
begin
  FRegIniFile.WriteString(Section, Ident, Value);
end;

procedure TRegistryIniFileW.ReadSection(const Section: WideString; Strings: TStrings);
begin
  FRegIniFile.ReadSection(Section, Strings);
end;

procedure TRegistryIniFileW.ReadSection(const Section: WideString; Strings: TStringsW;
  Clear: Boolean = True);
var
  AnsiList: TStringList;
begin
  AnsiList := TStringList.Create;
  try
    ReadSection(Section, AnsiList);

    if Clear then
      Strings.Clear;
    Strings.AddStrings(AnsiList);
  finally
    AnsiList.Free;
  end;
end;

procedure TRegistryIniFileW.ReadSections(Strings: TStrings);
begin
  FRegIniFile.ReadSections(Strings);
end;

procedure TRegistryIniFileW.ReadSections(Strings: TStringsW);
var
  AnsiList: TStringList;
begin
  AnsiList := TStringList.Create;
  try
    ReadSections(AnsiList);
    Strings.Assign(AnsiList);
  finally
    AnsiList.Free;
  end;
end;

procedure TRegistryIniFileW.ReadSectionValues(const Section: WideString; Strings: TStrings);
begin
  FRegIniFile.ReadSectionValues(Section, Strings);
end;

procedure TRegistryIniFileW.ReadSectionValues(const Section: WideString; Strings: TStringsW;
  Clear: Boolean = True);
var
  AnsiList: TStringList;
begin
  AnsiList := TStringList.Create;
  try
    ReadSectionValues(Section, AnsiList);

    if Clear then
      Strings.Clear;
    Strings.AddStrings(AnsiList);
  finally
    AnsiList.Free;
  end;
end;

procedure TRegistryIniFileW.EraseSection(const Section: WideString);
begin
  FRegIniFile.EraseSection(Section);
end;

procedure TRegistryIniFileW.DeleteKey(const Section, Ident: WideString);
begin
  FRegIniFile.DeleteKey(Section, Ident);
end;

procedure TRegistryIniFileW.UpdateFile;
begin
  { Do nothing }
end;

end.
