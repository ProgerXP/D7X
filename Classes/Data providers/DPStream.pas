unit DPStream;

interface

{
  A wrapper for any standard TStream object. Note that if there is a more suitable
  provider (such as TDPFile for files on disk) it's more optimal to use it instead.
}

uses DataProvider, Windows, Classes;

type
  TDPStream = class (TDataProvider)
  protected
    FStream: TStream;
    FFreeStream: Boolean;

    function GetID: WideString; override;

    function GetSize: DWord; override;
    procedure SetSize(NewSize: DWord); override;

    procedure DoSeek(Position: DWord); override;
    function DoTell: DWord; override;
    function DoRead(Dest: Pointer; Size: DWord): DWord; override;
    function DoWrite(Source: Pointer; Size: DWord): DWord; override;
  public
    // Stream is opened as writable; use Lock to make the DP readonly.
    constructor Create(Stream: TStream); reintroduce;
    destructor Destroy; override;

    property FreeStream: Boolean read FFreeStream write FFreeStream default False;
  end;

implementation

uses SysUtils, Utils;

constructor TDPStream.Create(Stream: TStream);
begin
  inherited Create;

  FStream := Stream;
  FFreeStream := False;
end;

destructor TDPStream.Destroy;
begin
  if FFreeStream then
    FStream.Free;
  inherited;
end;

function TDPStream.GetSize;
begin
  Result := FStream.Size;
end;

procedure TDPStream.SetSize(NewSize: DWord);
begin
  FStream.Size := NewSize;
end;

procedure TDPStream.DoSeek(Position: DWord);
begin
  FStream.Position := Position;
end;
                                 
function TDPStream.DoTell;
begin
  Result := FStream.Position;
end;

function TDPStream.DoRead;
begin
  Result := FStream.Read(Dest^, Size);
  FStream.Seek(-1 * Result, soFromCurrent);
end;

function TDPStream.DoWrite;
begin
  Result := FStream.Write(Source^, Size);  
  FStream.Seek(-1 * Result, soFromCurrent);
end;

function TDPStream.GetID: WideString;
begin
  Result := Copy(FStream.ClassName, 2, $FF);
end;

end.
