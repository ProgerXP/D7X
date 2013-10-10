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
    FUnderControl: TControl;

    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    procedure Click; override;
    procedure PopItUp(Menu: TPopupMenu; Under: TControl);  // nil Under = Self.
  published
    property PopupMenu: TPopupMenu read FMenu write FMenu;
    property PopupMenuAlt: TPopupMenu read FMenuAlt write FMenuAlt;
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
  PopItUp(FMenu, FUnderControl);
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
