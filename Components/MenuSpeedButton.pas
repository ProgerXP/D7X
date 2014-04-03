unit MenuSpeedButton;

interface

{ note: you can set where a menu should appear: on left or right sides or
        centered, using TPopupMenu.Alignment property. }

uses
  SysUtils, Classes, Controls, Buttons, Menus;

type
  TMenuSpeedButton = class (TSpeedButton)
  protected
    FMenu, FMenuAlt: TPopupMenu;
    FClicksDefault: Boolean;
    FUnderControl: TControl;

    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    procedure Click; override;
    function ClickDefault(Menu: TPopupMenu): TMenuItem;
    procedure PopItUp(Menu: TPopupMenu; Under: TControl);  // nil Under = Self.
  published
    property PopupMenu: TPopupMenu read FMenu write FMenu;
    property PopupMenuAlt: TPopupMenu read FMenuAlt write FMenuAlt;
    property ClicksDefault: Boolean read FClicksDefault write FClicksDefault default False;
    property UnderControl: TControl read FUnderControl write FUnderControl;
  end;

procedure Register;

implementation

uses Windows, Forms;
                    
procedure Register;
begin
  RegisterComponents('D7X', [TMenuSpeedButton]);
end;

// Coltrols.pas: 4695
procedure TMenuSpeedButton.Click;
begin
  inherited Click;

  if FClicksDefault and Assigned(FMenuAlt) then
    ClickDefault(FMenuAlt)
    else
      PopItUp(FMenu, FUnderControl);
end;

function TMenuSpeedButton.ClickDefault(Menu: TPopupMenu): TMenuItem;
var
  I: Integer;
begin
  Result := nil;

  if Menu <> nil then
    for I := 0 to Menu.Items.Count - 1 do
      if Assigned(Menu.Items[I].OnClick) then
      begin
        Result := Menu.Items[I];
        if Menu.Items[I].Default then
          Break;
      end;

  if Result <> nil then
    Result.Click;
end;

procedure TMenuSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbRight then
    PopItUp(FMenuAlt, FUnderControl);
end;

procedure TMenuSpeedButton.PopItUp(Menu: TPopupMenu; Under: TControl);
begin
  if Under = nil then
    Under := Self;

  if Menu <> nil then
    with Under, ClientToScreen(Point(0, Height)) do
      case Menu.Alignment of
      paLeft:   Menu.Popup(X, Y);
      paCenter: Menu.Popup(X + Width div 2, Y);
      else
        Menu.Popup(X + Width, Y);
      end
end;

end.
