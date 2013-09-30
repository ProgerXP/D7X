unit DPFreezer;

interface

{ Note: Freezer is meant to be created & opened. There should not be not opened yet created Freezers. }

uses DataProvider, Windows, Classes;

type
  TDPFreezer = class (TDataProvider)
  protected
    FFreezer: TMemoryStream;
    FFrozenData: TDataProvider;
    FID: WideString;

    function GetID: WideString; override;

    function GetSize: DWord; override;
    procedure SetSize(NewSize: DWord); override;

    procedure DoSeek(Position: DWord); override;

    // note: lower visibility is intended here.
    constructor Create; override;
    destructor Destroy; override;
  public
    constructor Freeze(DataToFreeze: TDataProvider);
    procedure UnfreezeAndDestroy; overload;
    procedure UnfreezeAndDestroy(DataToUpdate: TDataProvider); overload;

    function IsOpened: Boolean; override;

    function Tell: DWord; override;

    function Read(Dest: Pointer; Size: DWord): DWord; override;
    function Write(Source: Pointer; Size: DWord): DWord; override;

    property FrozenData: TDataProvider read FFrozenData;
  end;

implementation

uses SysUtils;

constructor TDPFreezer.Create;
begin
  inherited;
  FFreezer := NIL;
  FID := 'empty freezer';
end;

destructor TDPFreezer.Destroy;
begin
  if FFreezer <> NIL then
    FFreezer.Free;
  inherited;
end;

function TDPFreezer.GetSize;
begin
  Result := FFreezer.Size;
end;

procedure TDPFreezer.SetSize(NewSize: DWord);
begin
  FFreezer.Size := NewSize;
end;

function TDPFreezer.IsOpened;
begin
  Result := FFreezer <> NIL;
end;

function TDPFreezer.Tell;
begin
  Result := FFreezer.Position
end;

procedure TDPFreezer.DoSeek(Position: DWord);
begin
  FFreezer.Position := Position;
end;

function TDPFreezer.Read;
begin
  Result := FFreezer.Read(Dest^, Size);
end;

function TDPFreezer.Write;
begin
  Result := FFreezer.Write(Source^, Size);
end;

function TDPFreezer.GetID: WideString;
begin
  Result := FID;
end;

constructor TDPFreezer.Freeze(DataToFreeze: TDataProvider);
begin
  Create;
  FFrozenData := DataToFreeze;
  FID := DataToFreeze.ID;

  FFreezer := TMemoryStream.Create;
  Replicate(DataToFreeze);
end;

procedure TDPFreezer.UnfreezeAndDestroy;
begin
  UnfreezeAndDestroy(FFrozenData);
end;

procedure TDPFreezer.UnfreezeAndDestroy(DataToUpdate: TDataProvider);
begin
  DataToUpdate.Replicate(Self);

  FFreezer.Free;
  FFreezer := NIL;
  Destroy;
end;

end.
