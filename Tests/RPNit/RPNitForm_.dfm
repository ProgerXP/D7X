object RPNitForm: TRPNitForm
  Left = 191
  Top = 140
  Width = 225
  Height = 287
  BorderWidth = 8
  Caption = 'RPNit unit for D7X'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    201
    237)
  PixelsPerInch = 96
  TextHeight = 13
  object lbResult: TLabel
    Left = 0
    Top = 32
    Width = 46
    Height = 13
    Caption = 'Variables:'
  end
  object BitBtn1: TBitBtn
    Left = 0
    Top = 0
    Width = 25
    Height = 25
    Default = True
    TabOrder = 0
    OnClick = BitBtn1Click
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      18000000000000030000120B0000120B00000000000000000000FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE4E4E4FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF6F
      72834D5993C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFF6A6F84506CF65D7CFE364AA6C3C3C3FFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF6B6E862140F022
      44FE2243FE2B4EFE1E309EC1C1C1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF6264830525F61032FE1131FD1536FE1030FE082AFE0817ACBABA
      BBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF6F718A1C39EC3452FB405BF35C
      75FB7C91FE5C75F84964F83252FE16279CC0C0C0FFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFF3748B9546BFC4F66EB6E84F36C82F0445CD77D92FE7F91F3778BF27489
      FE2F3DA2B2B2B3FFFFFFFFFFFF00FF0000FF0000FF009CECFF00FF0000FF0000
      FF00FFEC9C354AC78093FC96A4F18999EE8999FE333D96B5B5B6FFFFFFBFFFFF
      00FF00FFFFDF2C3BA000AFBF00FF00FFFFBFFFFFFFFF0000FF0000FF0000FF00
      009AA7EBA3AEFE414883FFFFFFFFFFFF9CECFF00FF0000FF0000FF00FFEC9CFF
      FFFFFFFFFFFF0000FF0000FF0000FF0000BFC6EE7F8DF24B4F7DFFFFFFFFFFFF
      FFFFFF74D8FF00FF00FFD874FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB9B9BC1626
      9E475BE11A288CE4E4E4FFFFFFFFFFFFBFFFFF00807400FF00FFD874FFFFFFFF
      FFFFFFFFFFFF0000FF0000FF0000FF000027338FE6E6E6FFFFFFFFFFFFDFFFFF
      00979C00FF00DFFFDF00FF00FFD874FFFFFFFFFFFFFF0000FF0000FF0000FF00
      00FFFFFFFFFFFFFFFFFFFFFFFF00807400FF0000FF009CECBF00FF0000FF00DF
      C448FFFFFFFFFFFFFFFFFFE1E1E1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
  end
  object edExpr: TEdit
    Left = 32
    Top = 0
    Width = 169
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = '5 1 2 - * var +'
  end
  object lsVars: TListBox
    Left = 0
    Top = 48
    Width = 201
    Height = 165
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    Items.Strings = (
      'var = 1,3')
    TabOrder = 2
    OnDblClick = lsVarsDblClick
  end
  object Button1: TButton
    Left = 40
    Top = 220
    Width = 41
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Load'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 81
    Top = 220
    Width = 41
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 160
    Top = 220
    Width = 41
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Clear'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 0
    Top = 220
    Width = 33
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Add'
    TabOrder = 6
    OnClick = Button4Click
  end
  object od: TOpenDialog
    DefaultExt = 'lst'
    Filter = 'Text files (*.lst; *.txt)|*.lst; *.txt|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a file to load variables from'
    Top = 192
  end
  object sd: TSaveDialog
    DefaultExt = 'lst'
    Filter = 'Text files (*.lst; *.txt)|*.lst; *.txt|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofNoReadOnlyReturn, ofEnableSizing]
    Title = 'Select a file to save variables to'
    Left = 32
    Top = 192
  end
  object ApplicationEvents: TApplicationEvents
    OnException = ApplicationEventsException
    Left = 144
    Top = 48
  end
end
