program RPNitDemo;

uses
  Forms,
  RPNitForm_ in 'RPNitForm_.pas' {RPNitForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRPNitForm, RPNitForm);
  Application.Run;
end.
