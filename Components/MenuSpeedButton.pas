unit MenuSpeedButton;

interface

{ note: you can set where a menu should appear: on left or right sides or
        centered, using TPopupMenu.Alignment property. }

uses
  SysUtils, Classes, Controls, Buttons, Menus;

type
  TMenuSpeedButton = class (TSpeedButton)
  protected
    FMenu: TPopupMenu;
    FShowUnderControl: TControl;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  published
    property PopupMenu: TPopupMenu read FMenu write FMenu;
    property ShowUnderControl: TControl read FShowUnderControl write FShowUnderControl;
  end;

procedure Register;

implementation

uses Windows, Forms;
                    
procedure Register;
begin
  RegisterComponents('D7X', [TMenuSpeedButton]);
end;

constructor TMenuSpeedButton.Create;
begin               
  inherited;
   
  FMenu := NIL;
  FShowUnderControl := NIL
end;

// Coltrols.pas: 4695
procedure TMenuSpeedButton.Click;
begin
  if not (csDesigning in ComponentState) and (ActionLink <> nil) then
    ActionLink.Execute(Self)
  else
  begin
    if Assigned(OnClick) and ((Action = NIL) or (@OnClick <> @Action.OnExecute)) then
      OnClick(Self);

    if FMenu <> NIL then
    begin
      ShowUnderControl := Self;

      with ShowUnderControl, ClientToScreen(Point(0, Height)) do
        case FMenu.Alignment of
        paLeft: FMenu.Popup(X, Y);
        paCenter: FMenu.Popup(X + Width div 2, Y)
          else
            FMenu.Popup(X + Width, Y)
        end
    end
  end
end;

end.
