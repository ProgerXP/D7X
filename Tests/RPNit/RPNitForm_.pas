unit RPNitForm_;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, AppEvnts;

type
  TRPNitForm = class(TForm)
    BitBtn1: TBitBtn;
    edExpr: TEdit;
    lsVars: TListBox;
    lbResult: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    od: TOpenDialog;
    sd: TSaveDialog;
    ApplicationEvents: TApplicationEvents;
    Button4: TButton;
    procedure lsVarsDblClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure Button4Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    procedure AddVar(Name, Value: String);
  end;

var
  RPNitForm: TRPNitForm;

implementation

uses RPNit, StringUtils;

{$R *.dfm}

procedure EnsureFloatStr(Str: String);
begin
  StrToFloatRPN(Str);
end;

{ TRPNitForm }

procedure TRPNitForm.lsVarsDblClick(Sender: TObject);
var
  Name, Value: WIdeString;
  ValueAnsi: String;
begin
  if lsVars.ItemIndex <> -1 then
    if Split(lsVars.Items[lsVars.ItemIndex], '=', Name, Value) then
    begin
      Name := Trim(Name);
      ValueAnsi := Trim(Value);

      if (InputQuery('Set variable', 'Enter the value for "' + Name + '" variable:', ValueAnsi)) then
      begin
        EnsureFloatStr(ValueAnsi);
        lsVars.Items[lsVars.ItemIndex] := Name + ' = ' + ValueAnsi;
      end;
    end;
end;

procedure TRPNitForm.Button1Click(Sender: TObject);
var
  List: TStringList;
  I: Integer;
begin
  if od.Execute then
  begin
    List := TStringList.Create;
    try
      List.LoadFromFile(od.FileName);

      lsVars.Items.BeginUpdate;
      try
        lsVars.Clear;
        
        for I := 0 to List.Count - 1 do
          AddVar(List.Names[I], List.ValueFromIndex[I]);
      finally
        lsVars.Items.EndUpdate;
      end;
    finally
      List.Free;
    end;
  end;
end;

procedure TRPNitForm.Button2Click(Sender: TObject);
begin
  if sd.Execute then
    lsVars.Items.SaveToFile(sd.FileName);
end;

procedure TRPNitForm.Button3Click(Sender: TObject);
begin
  lsVars.Clear;
end;

procedure TRPNitForm.AddVar(Name, Value: String);
var
  I: Integer;
begin
  Name := Trim(Name);
  Value := Trim(Value);

  if not ConsistsOfChars(LowerCase(Name), RpnVarChars) then
    MessageBox(Handle, PChar(String('Invalid variable name "' + Name + '".' +
                                    ' Allowed characters are:'#10 + RpnVarChars)),
               'Invalid variable name', mb_IconStop)
    else
    begin
      EnsureFloatStr(Value);

      for I := 0 to lsVars.Count - 1 do
        if lsVars.Items.Names[I] = Name then
        begin
          MessageBox(Handle, PChar('Variable "' + Name + '" is already defined (value ' + lsVars.Items.ValueFromIndex[I] + ').'),
                     'Duplicated variable name', mb_IconStop);
          Exit;
        end;

      lsVars.Items.Add(Name + ' = ' + Value);
    end;
end;

procedure TRPNitForm.BitBtn1Click(Sender: TObject);
var
  Vars: TRpnVariables;
begin
  Vars := TRpnVariables.Create;
  try
    Vars.CopyAsKeyValuesFrom(lsVars.Items);
    lbResult.Caption := Format('Result = %1.5f', [EvalRPN(edExpr.Text, Vars)] );
  finally
    Vars.Free;
  end;
end;

procedure TRPNitForm.ApplicationEventsException(Sender: TObject;
  E: Exception);
begin
  if E.InheritsFrom(EInvalidRpnFloatStr) then
    MessageBox(Handle, PChar(String('Invalid floating point value "' + EInvalidRpnFloatStr(E).Num + '".'#10 +
                                    'Valid decimal part separators are period and comma.')),
                       'Invalid floating point value', mb_IconStop)
    else
      MessageBox(Handle, PChar(E.Message), PChar(E.ClassName + ' error'), mb_IconStop);
end;

procedure TRPNitForm.Button4Click(Sender: TObject);
var
  ValueAnsi: String;
  Name, Value: WideString;
begin
  ValueAnsi := '';
  if InputQuery('Add variable', 'Enter a string in format "variable = value":', ValueAnsi) then
    if Split(ValueAnsi, '=', Name, Value) then
      AddVar(Name, Value)
      else
        MessageBox(Handle, 'Invalid format. Example:'#10 + 'pi = 3,14', 'Invalid variable string format', mb_IconStop);
end;

procedure TRPNitForm.FormShow(Sender: TObject);
begin
  edExpr.SetFocus;
end;

end.
