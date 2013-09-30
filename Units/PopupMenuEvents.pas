unit PopupMenuEvents;

interface

{ Got basic algorithm from http://delphi.about.com/od/adptips2006/qt/popuplistex.htm }

uses Messages;

type                                                       
  TMessage = Messages.TMessage;
  TMenuEvent = procedure (var Message: TMessage) of object;

var
  { Occuring order is: enter, close, exit. }
  OnMenuEnter: TMenuEvent;
  OnMenuClose: TMenuEvent;
  OnMenuExit:  TMenuEvent;

procedure ClearMenuEventHandlers;

implementation

uses Forms, Menus;

type
  TPopupListWithEvents = class (TPopupList)
  protected
    procedure Signal(const Callback: TMenuEvent; var Message: TMessage);
    procedure WndProc(var Message: TMessage); override;
  end;

{ TPopupListWithEvents }

procedure TPopupListWithEvents.Signal(const Callback: TMenuEvent;
  var Message: TMessage);
begin
  if Assigned(Callback) then
    Callback(Message);
end;

procedure TPopupListWithEvents.WndProc(var Message: TMessage);
begin
  case message.Msg Of
  WM_ENTERMENULOOP: Signal(OnMenuEnter, Message);
  WM_EXITMENULOOP:  Signal(OnMenuExit,  Message);
  WM_MENUSELECT:
    with TWMMenuSelect(Message) do
      if (Menu = 0) and (Menuflag = $FFFF) then
        Signal(OnMenuClose, Message);
  end;

  inherited;
end;

procedure ClearMenuEventHandlers;
begin
  OnMenuEnter := NIL;
  OnMenuClose := NIL;
  OnMenuExit  := NIL;
end;

initialization
  ClearMenuEventHandlers;

  Popuplist.Free;
  PopupList := TPopupListWithEvents.Create;

end.
