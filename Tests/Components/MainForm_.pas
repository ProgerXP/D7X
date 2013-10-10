unit MainForm_;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, UnicodeDialogs, ExtCtrls, Menus,
  MenuSpeedButton, PNGSpeedButton, ComCtrls, WebLabel;

type
  TMainForm = class(TForm)
    od: TOpenDialogW;
    sd: TSaveDialogW;
    edDialog: TEdit;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Bevel1: TBevel;
    PNGSpeedButton1: TPNGSpeedButton;
    MenuSpeedButton1: TMenuSpeedButton;
    pm: TPopupMenu;
    est1: TMenuItem;
    N1: TMenuItem;
    Item11: TMenuItem;
    Item21: TMenuItem;
    Submenu1: TMenuItem;
    Submenuitem11: TMenuItem;
    Submenuitem12: TMenuItem;
    StatusBar1: TStatusBar;
    MenuSpeedButton2: TMenuSpeedButton;
    pmAlt: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    WebLabel1: TWebLabel;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure MenuSpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  if od.Execute then
    edDialog.Text := od.FileName;
end;

procedure TMainForm.SpeedButton2Click(Sender: TObject);
begin
  if sd.Execute then
    edDialog.Text := sd.FileName;
end;

procedure TMainForm.MenuSpeedButton1Click(Sender: TObject);
begin
  pm.Items[0].Caption := Sender.ClassName + ' test...';
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TControl then
      with TControl(Controls[I]) do
        if Hint = '' then
          Hint := Controls[I].ClassName;
end;

end.
