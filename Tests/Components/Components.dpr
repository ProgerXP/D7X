program Components;

uses
  Forms,
  MainForm_ in 'MainForm_.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
