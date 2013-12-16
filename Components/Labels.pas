unit Labels;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Windows, Graphics, Messages;

type
  TCustomLabelW = class (TCustomLabel)
  protected
    FCaption: WideString;

    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
    procedure DrawEnabledLabel(const Text: WideString; var Rect: TRect; Flags: DWord); virtual;

    procedure SetCaption(Value: WideString);
  public
    constructor Create(AOwner: TComponent); override;
    property Caption: WideString read FCaption write SetCaption;
  published
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;

  TLabelW = class (TCustomLabelW)
  published
    property AutoSize;
    property Caption;
  end;

  TShadowLabel = class (TLabelW)
  protected
    FShadowDistanceX: Byte;
    FShadowDistanceY: Byte;

    FShadowColor: TColor;

    procedure DrawEnabledLabel(const Text: WideString; var Rect: TRect; Flags: DWord); override;

    procedure SetShadowDistanceX(const Value: Byte);
    procedure SetShadowDistanceY(const Value: Byte);

    procedure SetShadowColor(const Value: TColor);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ShadowDistanceX: Byte read FShadowDistanceX write SetShadowDistanceX;
    property ShadowDistanceY: Byte read FShadowDistanceY write SetShadowDistanceY;

    property ShadowColor: TColor read FShadowColor write SetShadowColor;
  end;

  TPathLabel = class (TCustomLabelW)
  protected
    FPath: WideString;
    FMissingPart: WideString;

    procedure Resize; override;

    procedure UpdateCaption;
    function DoesTextFit(const Text: WideString): Boolean;
    function GetShortPath: WideString;

    procedure SetPath(const Value: WideString);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Path: WideString read FPath write SetPath;
    property MissingPart: WideString read FMissingPart write FMissingPart;
  end;

procedure Register;

implementation

uses DrawingUtils;

procedure Register;
begin
  RegisterComponents('D7X', [TLabelW, TShadowLabel, TPathLabel]);
end;

{ TCustomLabelW }

constructor TCustomLabelW.Create(AOwner: TComponent);
begin
  inherited;
  inherited Caption := '';
end;

procedure TCustomLabelW.SetCaption(Value: WideString);
begin
  FCaption := Value;
  AdjustBounds;
  Repaint
end;

// StdCtrls: 1449
procedure TCustomLabelW.DoDrawText;
var
  Text: WideString;
begin
  Text := Caption;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
  if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
  Flags := DrawTextBiDiModeFlags(Flags);
  Canvas.Font := Font;
  if not Enabled then
  begin
    OffsetRect(Rect, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Flags);
    OffsetRect(Rect, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Flags);
  end
    else
      DrawEnabledLabel(Text, Rect, Flags);
end;

procedure TCustomLabelW.DrawEnabledLabel(const Text: WideString; var Rect: TRect; Flags: DWord);
begin
  DrawTextW(Canvas.Handle, PWideChar(text), Length(Text), Rect, Flags)
end;

{ TShadowLabel }

constructor TShadowLabel.Create;
begin
  FShadowDistanceX := 3;
  FShadowDistanceY := 3;

  FShadowColor := clBtnHighlight;

  inherited
end;

procedure TShadowLabel.DrawEnabledLabel(const Text: WideString; var Rect: TRect; Flags: DWord);
begin
  OffsetRect(Rect, FShadowDistanceX, FShadowDistanceY);
  Canvas.Font.Color := FShadowColor;
  DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Flags);
  OffsetRect(Rect, -FShadowDistanceX, -FShadowDistanceY);
  Canvas.Font := Font;
  DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Flags);
end;

procedure TShadowLabel.SetShadowDistanceX;
begin
  FShadowDistanceX := Value;
  Repaint;
end;

procedure TShadowLabel.SetShadowDistanceY;
begin
  FShadowDistanceY := Value;
  Repaint;
end;

procedure TShadowLabel.SetShadowColor;
begin
  FShadowColor := Value;
  Repaint;
end;

{ TPathLabel }

constructor TPathLabel.Create(AOwner: TComponent);
begin
  inherited;
  AutoSize := False;
  FMissingPart := '...';
end;

procedure TPathLabel.SetPath(const Value: WideString);
begin
  FPath := Value;
  HInt := Value;
  UpdateCaption;
end;

procedure TPathLabel.Resize;
begin
  inherited;
  UpdateCaption;
end;

procedure TPathLabel.UpdateCaption;
begin
  if (FPath = '') or DoesTextFit(FPath) then
    Caption := FPAth
    else
      Caption := GetShortPath;
end;

function TPathLabel.DoesTextFit(const Text: WideString): Boolean;
begin
  Result := TextWidth(Canvas.Handle, Text) < ClientWidth;
end;

function TPathLabel.GetShortPath: WideString;
var
  LastFitChar, LeadingChars: Integer;
  ShortPath: WideString;
begin
  if FPath[2] = ':' then
    Result := Copy(Path, 1, 3)
    else if FPath[1] = '\' then
      Result := '\'
      else
        Result := '';

  LeadingChars := Length(Result);
  Result := Result + FMissingPart;

  LastFitChar := Length(FPath);
  ShortPath := '';

  while (LastFitChar > LeadingChars) and DoesTextFit(Result + ShortPath) do
  begin
    ShortPath := FPath[LastFitChar] + ShortPath;
    Dec(LastFitChar);
  end;

  Result := Result + Copy(ShortPath, 2, $FFFF);
end;

end.