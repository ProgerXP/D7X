unit DPFile;

interface

uses DataProvider, DPStream, FileStreamW, Windows, Classes;

type
  TDPFile = class (TDPStream)
  protected
    FFileName: WideString;

    function GetID: WideString; override;  
    property FreeStream;
  public
    constructor Create(const FileName: WideString; ReadOnly: Boolean = False); reintroduce;
    constructor CreateNew(const FileName: WideString);
    constructor CreateOrOpen(const FileName: WideString);
  end;
  
implementation

uses SysUtils, Utils;

constructor TDPFile.Create(const FileName: WideString; ReadOnly: Boolean = False);
var
  Stream: TFileStreamW;
begin
  ReadOnly := ReadOnly or not IsWritable(FileName);
  if ReadOnly then
    Stream := TFileStreamW.Create(FileName, fmOpenRead or fmShareDenyWrite)
    else
      Stream := TFileStreamW.Create(FileName, fmOpenReadWrite or fmShareExclusive);

  inherited Create(Stream);

  FFileName := FileName;
  FReadOnly := ReadOnly;

  FFreeStream := True;
end;

constructor TDPFile.CreateNew(const FileName: WideString);
begin
  inherited Create( TFileStreamW.Create(FileName, fmCreate or fmShareDenyWrite) );
  
  FFileName := FileName;                                                          
  FFreeStream := True;
end;

constructor TDPFile.CreateOrOpen(const FileName: WideString);
begin
  if FileExists(FileName) then
    Create(FileName, False)
    else
      CreateNew(FileName);
end;

function TDPFile.GetID: WideString;
begin
  Result := FFileName;
end;

end.
