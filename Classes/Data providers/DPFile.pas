unit DPFile;

interface

uses DataProvider, DPStream, FileStreamW, Windows, Classes;

type
  TDPFile = class (TDPStream)
  protected
    FFileName: WideString;

    function GetID: WideString; override;
  public
    constructor Create(const FileName: WideString; ReadOnly: Boolean = False); reintroduce;
    constructor CreateNew(const FileName: WideString);
  end;
  
implementation

uses SysUtils, Utils;

constructor TDPFile.Create(const FileName: WideString; ReadOnly: Boolean = False);
var
  Stream: TFileStreamW;
begin
  ReadOnly := ReadOnly or not IsWritable(FileName);
  if ReadOnly then
    Stream := TFileStreamW.Create(FileName, fmOpenRead or fmShareDenyNone)
    else
      Stream := TFileStreamW.Create(FileName, fmOpenReadWrite or fmShareDenyWrite);

  inherited Create(Stream);

  FFileName := FileName;
  FReadOnly := ReadOnly;
end;

constructor TDPFile.CreateNew(const FileName: WideString);
begin
  inherited Create( TFileStreamW.Create(FileName, fmCreate or fmShareDenyWrite) );
  FFileName := FileName;
end;

function TDPFile.GetID: WideString;
begin
  Result := FFileName;
end;

end.
