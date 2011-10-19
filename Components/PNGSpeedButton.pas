unit PNGSpeedButton;

{ Component made by Proger_XP | http://Proger.i-Forge.net }

interface

{ Note: don't use Glyph (of TBitmap type), it won't work properly. Use the PNG property instead. }

uses
  PNGImage, SysUtils, Classes, Controls, Buttons, Windows;

type
  TPercent = -1..100;

  TPNGSpeedButton = class (TSpeedButton)
  protected
    // note: only current locale's characters could be handled as accel chars (&c[har]).
    FCaption: WideString;
    FGrayWhenDisabled: Boolean;
    FPNG, FPicToPaint: TPNGObject;

    FClockGray: TPercent;
    FClockGrayClockWise: Boolean;
    FClockGrayInverse: Boolean;

    FCombine1, FCombine2, FCombineWith: TPNGObject;
    FCombinedSpacing: Integer;

    procedure Loaded; override;
    procedure SetEnabled(Value: Boolean); override;

    procedure Paint; override;
      procedure PaintCaptionIn(R: TRect); virtual;
    procedure GetPaintRects(out Glyph, Text: TRect); virtual;
    function CaptionExtent: TSize;

    procedure SetCaption(const Value: WideString);
    procedure SetGrayWhenDisabled(const Value: Boolean);
    procedure SetPNG(const Value: TPNGObject);
    procedure SetClockGray(const Value: TPercent);
    procedure SetClockGrayClockWise(const Value: Boolean);
    procedure SetClockGrayInverse(const Value: Boolean);

    procedure SetCombine1(const Value: TPNGObject);
    procedure SetCombine2(const Value: TPNGObject);
    procedure SetCombineWith(const Value: TPNGObject);
    procedure SetCombinedSpacing(const Value: Integer);
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;

    procedure UpdatePicToPaint;
  published
    property Caption: WideString read FCaption write SetCaption;
    property Glyph: TPNGObject read FPNG write SetPNG;
    property GrayWhenDisabled: Boolean read FGrayWhenDisabled write SetGrayWhenDisabled default True;

    property ClockGray: TPercent read FClockGray write SetClockGray default -1;
    property ClockGrayClockWise: Boolean read FClockGrayClockWise write SetClockGrayClockWise default True;
    property ClockGrayInverse: Boolean read FClockGrayInverse write SetClockGrayInverse default True;

    property Combine1: TPNGObject read FCombine1 write SetCombine1;
    property Combine2: TPNGObject read FCombine2 write SetCombine2;
    property CombineWith: TPNGObject read FCombineWith write SetCombineWith;
    property CombinedSpacing: Integer read FCombinedSpacing write SetCombinedSpacing default 2;
  end;

procedure Register;

implementation

uses StringUtils, Math, Graphics;

procedure Register;
begin
  RegisterComponents('D7X', [TPNGSpeedButton]);
end;

procedure CleanTransparentPngTo(var PNG: TPNGObject; NewWidth, NewHeight: Integer);
var
  BasePtr: Pointer;
begin
  PNG.Free;
  PNG := TPNGObject.CreateBlank(COLOR_RGBALPHA, 16, NewWidth, NewHeight);

  BasePtr := PNG.AlphaScanline[0];
  ZeroMemory(BasePtr, PNG.Header.Width * PNG.Header.Height);
end;

procedure DrawPngWithAlpha(Src, Dest: TPNGObject; const R: TRect);
var
  X, Y: Integer;
  Alpha: PByte;
begin
  Src.Draw(Dest.Canvas, R);

  // I have no idea why standard implementation of TPNGObject.Draw doesn't apply transparency.
  for Y := R.Top to R.Bottom - 1 do
    for X := R.Left to R.Right - 1 do
    begin
      Alpha := @Dest.AlphaScanline[Y]^[X];
      Alpha^ := Min(255, Alpha^ + Src.AlphaScanline[Y - R.Top]^[X - R.Left]);
    end;
end;

function RgbToGray(C: TColor): TColor;
begin
  C := Round(C and $FF * 0.3 + C and $00FF00 shr 8 * 0.59 + C shr 16 * 0.11);
  Result := RGB(C, C, C);
end;

procedure ToGrayscale(PNG: TPNGObject);
var
  X, Y: Integer;
begin
  for X := 0 to PNG.Width - 1 do
    for Y := 0 to PNG.Height - 1 do
      PNG.Pixels[X, Y] := RgbToGray(PNG.Pixels[X, Y]);
end;

procedure GrayscaleClock(PNG: TPNGObject; Percent: TPercent; Clockwise: Boolean = True;
  Inverse: Boolean = True);
var
  Angle: Extended;
  Mask: TBitMap;
  X, Y: Integer;
  Color: TColor;
begin
  Mask := TBitmap.Create;
  try
    Mask.Width := PNG.Width;
    Mask.Height := PNG.Height;
    Mask.Canvas.Brush.Color := clWhite;
    Mask.Canvas.FillRect( Rect(0, 0, Mask.Width, Mask.Height) );

    SetArcDirection(Mask.Canvas.Handle, AD_CLOCKWISE);

    if not Clockwise then
      Percent := -1 * Percent;
    Angle := (50 - Percent) / 100.0 * 2.0 * 3.1416;   // rad to grad.
    with PNG do
      Mask.Canvas.Pie(0, 0, Width, Height, Width div 2, 0,
                      Round(Width div 2 + Width div 2 * Sin(Angle)),
                      Round(Height div 2 + Height div 2 * Cos(Angle)));

    Mask.Canvas.Brush.Color := clBlack;
    Mask.Canvas.FloodFill(PNG.Width div 2, 2, clBlack, fsBorder);

    Color := Sign(Byte(Inverse)) * clWhite;
    with PNG do
      for X := 0 to Width - 1 do
        for Y := 0 to Height - 1 do
          if Mask.Canvas.Pixels[X, Y] = Color then
            PNG.Pixels[X, Y] := RgbToGray(PNG.Pixels[X, Y]);
  finally
    Mask.Free;
  end;
end;

{ TPNGSpeedButton }

constructor TPNGSpeedButton.Create(AComponent: TComponent);
begin
  inherited;

  FCombinedSpacing := 2;
  FGrayWhenDisabled := True;

  FClockGray := -1;
  FClockGrayClockWise := True;
  FClockGrayInverse := True;

  FPNG := TPNGObject.Create;
  FPicToPaint := TPNGObject.Create;

  FCombine1 := TPNGObject.Create;
  FCombine2 := TPNGObject.Create;
  FCombineWith := TPNGObject.Create
end;

destructor TPNGSpeedButton.Destroy;
begin
  FCombineWith.Free;
  FCombine2.Free;
  FCombine1.Free;

  FPicToPaint.Free;
  FPNG.Free;

  inherited
end;

function TPNGSpeedButton.CaptionExtent: TSize;
var
  Text: WideString;
begin
  Result.cx := 0;
  Result.cy := 0;
  Text := StripAccelChars(FCaption);
  GetTextExtentPoint32W(Canvas.Handle, PWideChar(Text), Length(Text), Result);
end;

procedure TPNGSpeedButton.GetPaintRects(out Glyph, Text: TRect);
var
  Spacing, Width, Height: Integer;
  IsDown: Byte;
  TextSize: TSize;
begin
  TextSize := CaptionExtent;

  if Caption = '' then
    Spacing := 0
    else
      Spacing := Self.Spacing;

  case Layout of
  blGlyphLeft:
    begin
      Glyph.TopLeft := Point((ClientWidth - FPicToPaint.Width - Spacing - TextSize.cx) div 2,
                             (ClientHeight - FPicToPaint.Height) div 2);
      Text.TopLeft := Point(Glyph.Left + FPicToPaint.Width + Spacing, Glyph.Top);
    end;
  blGlyphRight:
    begin
      Width := FPicToPaint.Width + Spacing + TextSize.cx;
      Glyph.TopLeft := Point((ClientWidth + Width) div 2 - FPicToPaint.Width,
                             (ClientHeight - FPicToPaint.Height) div 2);
      Text.TopLeft := Point((ClientWidth - Width) div 2, Glyph.Top);
    end;
  blGlyphTop:
    begin
      Glyph.TopLeft := Point((ClientWidth - FPicToPaint.Width) div 2,
                             (ClientHeight - FPicToPaint.Height - Spacing - TextSize.cy) div 2);
      Text.TopLeft := Point((ClientWidth - TextSize.cx) div 2, Glyph.Top + FPicToPaint.Height + Spacing);
    end;
  blGlyphBottom:
    begin
      Height := FPicToPaint.Height + Spacing + TextSize.cy;
      Glyph.TopLeft := Point((ClientWidth - FPicToPaint.Width) div 2,
                             (ClientHeight + Height) div 2 - FPicToPaint.Height);
      Text.TopLeft := Point((ClientWidth - TextSize.cx) div 2, (ClientHeight - Height) div 2);
    end;
  end;

  Glyph.BottomRight := Point(Glyph.Left + FPicToPaint.Width, Glyph.Top + FPicToPaint.Height);

  IsDown := Byte(Down or (FState = bsDown)) * 2;
  OffsetRect(Glyph, IsDown, IsDown);
  OffsetRect(Text, IsDown, IsDown);
end;

procedure TPNGSpeedButton.Paint;
var
  GlyphRect, TextRect: TRect;
begin
  GetPaintRects(GlyphRect, TextRect);
  inherited;
  FPicToPaint.Draw(Canvas, GlyphRect);
  PaintCaptionIn(TextRect);
end;

  procedure TPNGSpeedButton.PaintCaptionIn(R: TRect);
  begin
    DrawTextW(Canvas.Handle, PWideChar(FCaption), Length(FCaption), R, DT_NOCLIP)
  end;

procedure TPNGSpeedButton.SetCaption(const Value: WideString);
begin
  FCaption := Value;
  Repaint
end;

procedure TPNGSpeedButton.SetPNG(const Value: TPNGObject);
begin
  FPNG.Assign(Value);
  inherited Glyph := NIL;  // it needs to be recreated so it won't look like a white button background.
  UpdatePicToPaint;
end;

procedure TPNGSpeedButton.SetCombine1(const Value: TPNGObject);
begin
  FCombine1.Assign(Value);
  UpdatePicToPaint;
end;

procedure TPNGSpeedButton.SetCombine2(const Value: TPNGObject);
begin
  FCombine2.Assign(Value);
  UpdatePicToPaint;
end;

procedure TPNGSpeedButton.SetCombineWith(const Value: TPNGObject);
begin
  FCombineWith.Assign(Value);
  UpdatePicToPaint;
end;

procedure TPNGSpeedButton.UpdatePicToPaint;
var
  R: TRect;
  Spacing: Integer;
begin
  FPicToPaint.Assign(FPNG);

  if not FCombine1.Empty then
  begin
    if not FCombine2.Empty or not FCombineWith.Empty then
      Spacing := CombinedSpacing
      else
        Spacing := 0;

    CleanTransparentPngTo(FPicToPaint, FCombine1.Width + FCombine2.Width + Spacing * 2, FCombine1.Height);
    DrawPngWithAlpha( FCombine1, FPicToPaint, Rect(0, 0, FCombine1.Width, FCombine1.Height) );

    R := Rect(0, 0, FCombine2.Width, FCombine2.Height);
    OffsetRect(R, FCombine1.Width + CombinedSpacing * 2, 0);
    DrawPngWithAlpha(FCombine2, FPicToPaint, R);

    R := Rect(0, 0, FCombineWith.Width, FCombineWith.Height);
    OffsetRect(R, (FPicToPaint.Width + CombinedSpacing - FCombineWith.Width) div 2,
                  (FPicToPaint.Height - FCombineWith.Height) div 2);
    DrawPngWithAlpha(FCombineWith, FPicToPaint, R);
  end;

  if FClockGray > -1 then
    GrayscaleClock(FPicToPaint, FClockGray, FClockGrayClockWise, FClockGrayInverse)
    else if not Enabled and FGrayWhenDisabled then
      ToGrayscale(FPicToPaint);

  Repaint;
end;

procedure TPNGSpeedButton.SetCombinedSpacing(const Value: Integer);
begin
  FCombinedSpacing := Value;
  UpdatePicToPaint;
end;

procedure TPNGSpeedButton.SetGrayWhenDisabled(const Value: Boolean);
begin
  FGrayWhenDisabled := Value;
  Repaint;
end;

procedure TPNGSpeedButton.Loaded;
begin
  inherited;
  UpdatePicToPaint;
end;

procedure TPNGSpeedButton.SetClockGray(const Value: TPercent);
begin
  if FClockGray <> Value then
  begin
    FClockGray := Value;
    UpdatePicToPaint;
  end;
end;

procedure TPNGSpeedButton.SetClockGrayClockWise(const Value: Boolean);
begin
  if FClockGrayClockWise <> Value then
  begin
    FClockGrayClockWise := Value;
    UpdatePicToPaint;
  end;
end;

procedure TPNGSpeedButton.SetEnabled(Value: Boolean);
begin
  if Value <> Enabled then
  begin
    inherited;
    if FGrayWhenDisabled then
      UpdatePicToPaint;
  end;
end;

procedure TPNGSpeedButton.SetClockGrayInverse(const Value: Boolean);
begin
  if FClockGrayInverse <> Value then
  begin
    FClockGrayInverse := Value;
    UpdatePicToPaint;
  end;
end;

end.
