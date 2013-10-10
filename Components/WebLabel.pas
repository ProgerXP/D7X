unit WebLabel;

{
  A component of D7X library by Proger_XP.
  Version of 10 October 2013.
}

interface

uses
  Windows, ShellAPI, SysUtils, Classes, Graphics, Controls, StdCtrls, Messages, Forms;

type
  TWebLabel = class (TLabel)
  protected
    FURL: String;
    FUseHoverFont: Boolean;
    FHoverFont: TFont;
    FOriginalFont: TFont;

    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Cursor default crHandPoint;
    property URL: String read FURL write FURL;
    property HoverFont: TFont read FHoverFont write FHoverFont;
    property UseHoverFont: Boolean read FUseHoverFont write FUseHoverFont default True;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('D7X', [TWebLabel]);
end;

{ TWebLabel }

constructor TWebLabel.Create(AOwner: TComponent);
begin
  inherited;

  Cursor := crHandPoint;
  Font.Color := clNavy;
  Font.Style := [fsUnderline];

  FOriginalFont := TFont.Create;
  FHoverFont := TFont.Create;
  FHoverFont.Assign(Font);
  FHoverFont.Color := clBlue;
  FUseHoverFont := True;
end;

destructor TWebLabel.Destroy;
begin
  FHoverFont.Free;
  FOriginalFont.Free;
  inherited;
end;

procedure TWebLabel.Click;
begin
  inherited;

  if Enabled and (fURL <> '') and
     (ShellExecute(ValidParentForm(Self).Handle, nil, PChar(FURL), nil, nil, SW_NORMAL) <= 32) then
    RaiseLastOSError;
end;

procedure TWebLabel.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  if Enabled and FUseHoverFont and not (csDesigning in ComponentState) then
  begin
    FOriginalFont.Assign(Font);
    Font.Assign(FHoverFont);
  end;
end;

procedure TWebLabel.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if Enabled and FUseHoverFont and not (csDesigning in ComponentState) then
    Font.Assign(FOriginalFont);
end;

end.