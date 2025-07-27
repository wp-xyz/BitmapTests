unit BitmapTransparentTests;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
 {$IFNDEF FPC}
  Windows,
 {$ENDIF}
  Classes, SysUtils,
  Graphics, Controls, ExtCtrls, Forms,
  {$IFDEF FPC}
   fpcunit, testutils, testregistry;
  {$ELSE}
   TestFrameWork;
  {$ENDIF}

type
  TTestMethod = (tmDraw, tmAssign);

  { Basic test cases }

  TTransparentBitmapTest= class(TTestCase)
  private
    FPixelFormat: TPixelFormat;
    FTestMethod: TTestMethod;
    procedure BitmapPaintHandler(Sender: TObject);

  protected
    FTestBitmap: TBitmap;
    procedure DoTestTransparent(ACodeMode, ATransparentCode: Integer);
    procedure GetBitmapScreenshot(ATransparentCode: Integer; AScreenshot: TBitmap);

  protected
    // Do not set Transparent --> opaque
    procedure NotTransparent; virtual;

    // -- (Red) border is transparent ---
    // Set Transparent, but not Transparentcolor and not TransparentMode
    procedure BorderTransparent_AfterCreate; virtual;
    procedure BorderTransparent_AfterSetSize; virtual;
    procedure BorderTransparent_AfterHandle; virtual;
    procedure BorderTransparent_AtEnd; virtual;

    // Always sets TransparentColor
    // Set Transparent BEFORE TransparentColor
    procedure BorderTransparent_AfterCreate_T_TColor; virtual;
    procedure BorderTransparent_AfterSetSize_T_TColor; virtual;
    procedure BorderTransparent_AfterHandle_T_TColor; virtual;
    procedure BorderTransparent_AtEnd_BeforeTransparentColor; virtual;
    // Set Transparent AFTER TransparentColor
    procedure BorderTransparent_AfterCreate_TColor_T; virtual;
    procedure BorderTransparent_AfterSetSize_TColor_T; virtual;
    procedure BorderTransparent_AfterHandle_TColor_T; virtual;
    procedure BorderTransparent_AtEnd_TColor_T; virtual;
    // Set Transparent, then TransparentMode=tmFixed, finally TransparentColor
    procedure BorderTransparent_AfterCreate_T_TMode_TColor; virtual;
    procedure BorderTransparent_AfterSetSize_T_TMode_TColor; virtual;
    procedure BorderTransparent_AfterHandle_T_TMode_TColor; virtual;
    procedure BorderTransparent_AtEnd_T_TMode_TColor; virtual;
    // Set TransparentMode=tmFixed, then Transparent, finally TransparentColor
    procedure BorderTransparent_AfterCreate_TMode_T_TColor; virtual;
    procedure BorderTransparent_AfterSetSize_TMode_T_TColor; virtual;
    procedure BorderTransparent_AfterHandle_TMode_T_TColor; virtual;
    procedure BorderTransparent_AtEnd_TMode_T_TColor; virtual;
    // Set TransparentMode=tmFixed, then TransparentColor, finally Transparent
    procedure BorderTransparent_AfterCreate_TMode_TColor_T; virtual;
    procedure BorderTransparent_AfterSetSize_TMode_TColor_T; virtual;
    procedure BorderTransparent_AfterHandle_TMode_TColor_T; virtual;
    procedure BorderTransparent_AtEnd_TMode_TColor_T; virtual;

    // -- (Blue) inner rectangle is transparent ---

    // Always set TransparentColor
    // Set Transparent BEFORE TransparentColor
    procedure InnerRectTransparent_AfterCreate_BeforeTransparentColor; virtual;
    procedure InnerRectTransparent_AfterSetSize_BeforeTransparentColor; virtual;
    procedure InnerRectTransparent_AfterHandle_BeforeTransparentColor; virtual;
    procedure InnerRectTransparent_AtEnd_BeforeTransparentColor; virtual;
    // Set Transparent AFTER TransparentColor
    procedure InnerRectTransparent_AfterCreate_AfterTransparentColor; virtual;
    procedure InnerRectTransparent_AfterSetSize_AfterTransparentColor; virtual;
    procedure InnerRectTransparent_AfterHandle_AfterTransparentColor; virtual;
    procedure InnerRectTransparent_AtEnd_AfterTransparentColor; virtual;
    // Set Transparent, then TransparentMode=tmFixed, finally TransparentColor
    procedure InnerRectTransparent_AfterCreate_T_TMode_TColor; virtual;
    procedure InnerRectTransparent_AfterSetSize_T_TMode_TColor; virtual;
    procedure InnerRectTransparent_AfterHandle_T_TMode_TColor; virtual;
    procedure InnerRectTransparent_AtEnd_T_TMode_TColor; virtual;
    // Set TransparentMode=tmFixed, then Transparent, finally TransparentColor
    procedure InnerRectTransparent_AfterCreate_TMode_T_TColor; virtual;
    procedure InnerRectTransparent_AfterSetSize_TMode_T_TColor; virtual;
    procedure InnerRectTransparent_AfterHandle_TMode_T_TColor; virtual;
    procedure InnerRectTransparent_AtEnd_TMode_T_TColor; virtual;
    // Set TransparentMode=tmFixed, then TransparentColor, finally Transparent
    procedure InnerRectTransparent_AfterCreate_TMode_TColor_T; virtual;
    procedure InnerRectTransparent_AfterSetSize_TMode_TColor_T; virtual;
    procedure InnerRectTransparent_AfterHandle_TMode_TColor_T; virtual;
    procedure InnerRectTransparent_AtEnd_TMode_TColor_T; virtual;
  end;


  { Test cases for drawing a 24-bit bitmap on a canvas }

  TCustomDrawTransparentBitmapTest_24Bit = class(TTransparentBitmapTest)
  public
    {$IFDEF FPC}
    constructor Create; override;
    {$ELSE}
    constructor Create(AMethodName: String); override;
    {$ENDIF}
  end;

  // Using Transparent = false
  TDrawTransparentBitmapTest_24Bit_NotTransparent = class(TCustomDrawTransparentBitmapTest_24Bit)
  published
    procedure NotTransparent; override;
  end;

  // Set Transparent = true, TransparentColor and TransparentMode not used.
  TDrawTransparentBitmapTest_24Bit_Transparent = class(TCustomDrawTransparentBitmapTest_24Bit)
  published
    // The outer red rectangle should be transparent.
    procedure BorderTransparent_AfterCreate; override;
    procedure BorderTransparent_AfterSetSize; override;
    procedure BorderTransparent_AfterHandle; override;
    procedure BorderTransparent_AtEnd; override;

    // The InnerRect tests are not useful here because they would require
    // setting the TransparentColor.
  end;

  // Set Transparent = true, then TransparentColor. TransparentMode is not used
  TDrawTransparentBitmapTest_24Bit_T_TColor = class(TCustomDrawTransparentBitmapTest_24Bit)
  published
    // The outer red border should be transparent
    procedure BorderTransparent_AfterCreate_T_TColor; override;
    procedure BorderTransparent_AfterSetSize_T_TColor; override;
    procedure BorderTransparent_AfterHandle_T_TColor; override;
    procedure BorderTransparent_AtEnd_BeforeTransparentColor; override;

    // The inner blue rectangle should be transparent
    procedure InnerRectTransparent_AfterCreate_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AfterSetSize_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AfterHandle_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AtEnd_BeforeTransparentColor; override;
  end;

  // Set TransparentColor, then set Transparent = true. TransparentMode is not used
  TDrawTransparentBitmapTest_24Bit_TColor_T = class(TCustomDrawTransparentBitmapTest_24Bit)
  published
    // The outer red border should be transparent
    procedure BorderTransparent_AfterCreate_TColor_T; override;
    procedure BorderTransparent_AfterSetSize_TColor_T; override;
    procedure BorderTransparent_AfterHandle_TColor_T; override;
    procedure BorderTransparent_AtEnd_TColor_T; override;

    // The inner blue rectangle should be transparent
    procedure InnerRectTransparent_AfterCreate_AfterTransparentColor; override;
    procedure InnerRectTransparent_AfterSetSize_AfterTransparentColor; override;
    procedure InnerRectTransparent_AfterHandle_AfterTransparentColor; override;
    procedure InnerRectTransparent_AtEnd_AfterTransparentColor; override;
  end;

  // First set Transparent, then TransparentMode, finally TransparentColor
  TDrawTransparentBitmapTest_24Bit_T_TMode_TColor = class(TCustomDrawTransparentBitmapTest_24Bit)
  published
    // The outer red border should be transparent
    procedure BorderTransparent_AfterCreate_T_TMode_TColor; override;
    procedure BorderTransparent_AfterSetSize_T_TMode_TColor; override;
    procedure BorderTransparent_AfterHandle_T_TMode_TColor; override;
    procedure BorderTransparent_AtEnd_T_TMode_TColor; override;

    // The inner blue rectangle should be transparent
    procedure InnerRectTransparent_AfterCreate_T_TMode_TColor; override;
    procedure InnerRectTransparent_AfterSetSize_T_TMode_TColor; override;
    procedure InnerRectTransparent_AfterHandle_T_TMode_TColor; override;
    procedure InnerRectTransparent_AtEnd_T_TMode_TColor; override;
  end;

  // First set TransparentMode, then Transparent, finally TransparentColor
  TDrawTransparentBitmapTest_24Bit_TMode_T_TColor = class(TCustomDrawTransparentBitmapTest_24Bit)
  published
    // The outer red border should be transparent
    procedure BorderTransparent_AfterCreate_TMode_T_TColor; override;
    procedure BorderTransparent_AfterSetSize_TMode_T_TColor; override;
    procedure BorderTransparent_AfterHandle_TMode_T_TColor; override;
    procedure BorderTransparent_AtEnd_TMode_T_TColor; override;

    // The inner blue rectangle should be transparent
    procedure InnerRectTransparent_AfterCreate_TMode_T_TColor; override;
    procedure InnerRectTransparent_AfterSetSize_TMode_T_TColor; override;
    procedure InnerRectTransparent_AfterHandle_TMode_T_TColor; override;
    procedure InnerRectTransparent_AtEnd_TMode_T_TColor; override;
  end;

  // First set TransparentMode, then TransparentColor, finally Transparent
  TDrawTransparentBitmapTest_24Bit_TMode_TColor_T = class(TCustomDrawTransparentBitmapTest_24Bit)
  published
    // The outer red border should be transparent
    procedure BorderTransparent_AfterCreate_TMode_TColor_T; override;
    procedure BorderTransparent_AfterSetSize_TMode_TColor_T; override;
    procedure BorderTransparent_AfterHandle_TMode_TColor_T; override;
    procedure BorderTransparent_AtEnd_TMode_TColor_T; override;

    // The inner blue rectangle should be transparent
    procedure InnerRectTransparent_AfterCreate_TMode_TColor_T; override;
    procedure InnerRectTransparent_AfterSetSize_TMode_TColor_T; override;
    procedure InnerRectTransparent_AfterHandle_TMode_TColor_T; override;
    procedure InnerRectTransparent_AtEnd_TMode_TColor_T; override;
  end;


  { Test cases for assigning a 24-bpp bitmap to an image }

  TCustomAssignTransparentBitmapTest_24Bit = class(TTransparentBitmapTest)
  public
    {$IFDEF FPC}
    constructor Create; override;
    {$ELSE}
    constructor Create(AMethodName: String); override;
    {$ENDIF}
  end;

  TAssignTransparentBitmapTest_24Bit_NotTransparent = class(TCustomAssignTransparentBitmapTest_24Bit)
  published
    // Set Transparent = false
    procedure NotTransparent; override;
  end;

  // Set Transparent = true, TransparentColor and TransparentMode not used.
  TAssignTransparentBitmapTest_24Bit_Transparent = class(TCustomAssignTransparentBitmapTest_24Bit)
  published
    procedure BorderTransparent_AfterCreate; override;
    procedure BorderTransparent_AfterSetSize; override;
    procedure BorderTransparent_AfterHandle; override;
    procedure BorderTransparent_AtEnd; override;

    // The InnerRect tests are not useful here because they would require
    // setting the TransparentColor.
  end;

  // Set Transparent = true, then TransparentColor. TransparentMode is not used
  TAssignTransparentBitmapTest_24Bit_T_TColor = class(TCustomAssignTransparentBitmapTest_24Bit)
  published
    // The outer red border should be transparent
    procedure BorderTransparent_AfterCreate_T_TColor; override;
    procedure BorderTransparent_AfterSetSize_T_TColor; override;
    procedure BorderTransparent_AfterHandle_T_TColor; override;
    procedure BorderTransparent_AtEnd_BeforeTransparentColor; override;

    // The inner blue rectangle should be transparent
    procedure InnerRectTransparent_AfterCreate_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AfterSetSize_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AfterHandle_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AtEnd_BeforeTransparentColor; override;
  end;

  // First set TransparentColor, then Transparent. TransparentMode is not used
  TAssignTransparentBitmapTest_24Bit_TColor_T = class(TCustomAssignTransparentBitmapTest_24Bit)
  published
    // The outer red border should be transparent
    procedure BorderTransparent_AfterCreate_TColor_T; override;
    procedure BorderTransparent_AfterSetSize_TColor_T; override;
    procedure BorderTransparent_AfterHandle_TColor_T; override;
    procedure BorderTransparent_AtEnd_TColor_T; override;

    // The inner blue rectangle should be transparent
    procedure InnerRectTransparent_AfterCreate_AfterTransparentColor; override;
    procedure InnerRectTransparent_AfterSetSize_AfterTransparentColor; override;
    procedure InnerRectTransparent_AfterHandle_AfterTransparentColor; override;
    procedure InnerRectTransparent_AtEnd_AfterTransparentColor; override;
  end;

  // First set Transparent, then TransparentMode, finally TransparentColor
  TAssignTransparentBitmapTest_24Bit_T_TMode_TColor = class(TCustomAssignTransparentBitmapTest_24Bit)
  published
    // The outer red border should be transparent
    procedure BorderTransparent_AfterCreate_T_TMode_TColor; override;
    procedure BorderTransparent_AfterSetSize_T_TMode_TColor; override;
    procedure BorderTransparent_AfterHandle_T_TMode_TColor; override;
    procedure BorderTransparent_AtEnd_T_TMode_TColor; override;

    // The inner blue rectangle should be transparent
    procedure InnerRectTransparent_AfterCreate_T_TMode_TColor; override;
    procedure InnerRectTransparent_AfterSetSize_T_TMode_TColor; override;
    procedure InnerRectTransparent_AfterHandle_T_TMode_TColor; override;
    procedure InnerRectTransparent_AtEnd_T_TMode_TColor; override;
  end;

  // First set TransparentMode, then Transparent, finally TransparentColor
  TAssignTransparentBitmapTest_24Bit_TMode_T_TColor = class(TCustomDrawTransparentBitmapTest_24Bit)
  published
    // The outer red border should be transparent
    procedure BorderTransparent_AfterCreate_TMode_T_TColor; override;
    procedure BorderTransparent_AfterSetSize_TMode_T_TColor; override;
    procedure BorderTransparent_AfterHandle_TMode_T_TColor; override;
    procedure BorderTransparent_AtEnd_TMode_T_TColor; override;

    // The inner blue rectangle should be transparent
    procedure InnerRectTransparent_AfterCreate_TMode_T_TColor; override;
    procedure InnerRectTransparent_AfterSetSize_TMode_T_TColor; override;
    procedure InnerRectTransparent_AfterHandle_TMode_T_TColor; override;
    procedure InnerRectTransparent_AtEnd_TMode_T_TColor; override;
  end;

  // First set TransparentMode, then TransparentColor, finally Transparent
  TAssignTransparentBitmapTest_24Bit_TMode_TColor_T = class(TCustomDrawTransparentBitmapTest_24Bit)
  published
    // The outer red border should be transparent
    procedure BorderTransparent_AfterCreate_TMode_TColor_T; override;
    procedure BorderTransparent_AfterSetSize_TMode_TColor_T; override;
    procedure BorderTransparent_AfterHandle_TMode_TColor_T; override;
    procedure BorderTransparent_AtEnd_TMode_TColor_T; override;

    // The inner blue rectangle should be transparent
    procedure InnerRectTransparent_AfterCreate_TMode_TColor_T; override;
    procedure InnerRectTransparent_AfterSetSize_TMode_TColor_T; override;
    procedure InnerRectTransparent_AfterHandle_TMode_TColor_T; override;
    procedure InnerRectTransparent_AtEnd_TMode_TColor_T; override;
  end;


  { Test cases for drawing a 32-bpp bitmap on a canvas }

  TCustomDrawTransparentBitmapTest_32Bit = class(TTransparentBitmapTest)
  public
    {$IFDEF FPC}
    constructor Create; override;
    {$ELSE}
    constructor Create(AMethodName: String); override;
    {$ENDIF}
  end;

  // Use Transparent = false --> opaque
  TDrawTransparentBitmapTest_32Bit_NotTransparent = class(TCustomDrawTransparentBitmapTest_32Bit)
  published
    procedure NotTransparent; override;
  end;

  // Set Transparent = true, TransparentColor and TransparentMode not used.
  TDrawTransparentBitmapTest_32Bit_Transparent = class(TCustomDrawTransparentBitmapTest_32Bit)
  published
    procedure BorderTransparent_AfterCreate; override;
    procedure BorderTransparent_AfterSetSize; override;
    procedure BorderTransparent_AfterHandle; override;
    procedure BorderTransparent_AtEnd; override;

    // The InnerRect tests are not useful here because they would require
    // setting the TransparentColor.
  end;

  // Set Transparent = true, then TransparentColor. TransparentMode is not used
  TDrawTransparentBitmapTest_32Bit_T_TColor = class(TCustomDrawTransparentBitmapTest_32Bit)
  published
    // The outer red border should be transparent
    procedure BorderTransparent_AfterCreate_T_TColor; override;
    procedure BorderTransparent_AfterSetSize_T_TColor; override;
    procedure BorderTransparent_AfterHandle_T_TColor; override;
    procedure BorderTransparent_AtEnd_BeforeTransparentColor; override;

    // The inner blue rectangle should be transparent
    procedure InnerRectTransparent_AfterCreate_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AfterSetSize_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AfterHandle_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AtEnd_BeforeTransparentColor; override;
  end;

  // Set TransparentColor, then set Transparent = true. TransparentMode is not used
  TDrawTransparentBitmapTest_32Bit_TColor_T = class(TCustomDrawTransparentBitmapTest_32Bit)
  published
    // The outer red border should be transparent
    procedure BorderTransparent_AfterCreate_TColor_T; override;
    procedure BorderTransparent_AfterSetSize_TColor_T; override;
    procedure BorderTransparent_AfterHandle_TColor_T; override;
    procedure BorderTransparent_AtEnd_TColor_T; override;

    // The inner blue rectangle should be transparent
    procedure InnerRectTransparent_AfterCreate_AfterTransparentColor; override;
    procedure InnerRectTransparent_AfterSetSize_AfterTransparentColor; override;
    procedure InnerRectTransparent_AfterHandle_AfterTransparentColor; override;
    procedure InnerRectTransparent_AtEnd_AfterTransparentColor; override;
  end;


  { Test cases for assigning a 32-bpp bitmap to a TImage }

  TCustomAssignTransparentBitmapTest_32Bit = class(TTransparentBitmapTest)
  public
    {$IFDEF FPC}
    constructor Create; override;
    {$ELSE}
    constructor Create(AMethodName: String); override;
    {$ENDIF}
  end;

  TAssignTransparentBitmapTest_32Bit_NotTransparent = class(TCustomAssignTransparentBitmapTest_32Bit)
  published
    // Do not set Transparent --> opaque
    procedure NotTransparent; override;
  end;

  // Set Transparent = true, TransparentColor and TransparentMode not used.
  TAssignTransparentBitmapTest_32Bit_Transparent = class(TCustomDrawTransparentBitmapTest_32Bit)
  published
    procedure BorderTransparent_AfterCreate; override;
    procedure BorderTransparent_AfterSetSize; override;
    procedure BorderTransparent_AfterHandle; override;
    procedure BorderTransparent_AtEnd; override;

    // The InnerRect tests are not useful here because they would require
    // setting the TransparentColor.
  end;

  // Set Transparent = true, then TransparentColor. TransparentMode is not used
  TAssignTransparentBitmapTest_32Bit_T_TColor = class(TCustomAssignTransparentBitmapTest_32Bit)
  published
    // The outer red border should be transparent
    procedure BorderTransparent_AfterCreate_T_TColor; override;
    procedure BorderTransparent_AfterSetSize_T_TColor; override;
    procedure BorderTransparent_AfterHandle_T_TColor; override;
    procedure BorderTransparent_AtEnd_BeforeTransparentColor; override;

    // The inner blue rectangle should be transparent
    procedure InnerRectTransparent_AfterCreate_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AfterSetSize_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AfterHandle_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AtEnd_BeforeTransparentColor; override;
  end;

  // First set TransparentColor, then Transparent. TransparentMode is not used
  TAssignTransparentBitmapTest_32Bit_TColor_T = class(TCustomAssignTransparentBitmapTest_32Bit)
  published
    // The outer red border should be transparent
    procedure BorderTransparent_AfterCreate_TColor_T; override;
    procedure BorderTransparent_AfterSetSize_TColor_T; override;
    procedure BorderTransparent_AfterHandle_TColor_T; override;
    procedure BorderTransparent_AtEnd_TColor_T; override;

    // The inner blue rectangle should be transparent
    procedure InnerRectTransparent_AfterCreate_AfterTransparentColor; override;
    procedure InnerRectTransparent_AfterSetSize_AfterTransparentColor; override;
    procedure InnerRectTransparent_AfterHandle_AfterTransparentColor; override;
    procedure InnerRectTransparent_AtEnd_AfterTransparentColor; override;
  end;



implementation

{$IFDEF FPC}
uses
  LCLIntf, LCLType;
{$ENDIF}

const
  EXPECTED_IMG_DIR = '../expected_images/';

function CompareBitmaps(ActualBitmap, ExpectedBitmap: TBitmap;
  out AMsg: String): Boolean;
var
  w, h: Integer;
  x, y: Integer;
  actC, expC: TColor;
begin
  Result := false;
  w := ExpectedBitmap.Width;
  h := ExpectedBitmap.Height;
  for y := 0 to h-1 do
    for x := 0 to w-1 do
    begin
      actC := ActualBitmap.Canvas.Pixels[x, y];
      expC := ExpectedBitmap.Canvas.Pixels[x, y];
      if actC <> expC then
      begin
        AMsg := Format('At pixel x=%d, y=%d: ActualColor=$%.8x; ExpectedColor=$%.8x', [
          x, y, actC, expC
        ]);
        exit;
      end;
    end;
  Result := true;
  AMsg := '';
end;


{ TTransparentBitmapTest }

procedure TTransparentBitmapTest.BitmapPaintHandler(Sender: TObject);
begin
  (Sender as TForm).Canvas.Draw(0, 0, FTestBitmap);
end;

procedure TTransparentBitmapTest.GetBitmapScreenShot(ATransparentCode: Integer;
  AScreenShot: TBitmap);
var
  F: TForm;
  img: TImage;
begin
  F := TForm.CreateNew(nil);
  F.Width := FTestBitmap.Width + 100;
  F.Height := FTestBitmap.Height + 100;
  F.Color := clWhite;
  case FTestMethod of
    tmDraw:
      F.OnPaint := {$IFDEF FPC}@{$ENDIF}BitmapPaintHandler;
    tmAssign:
      begin
        img := TImage.Create(F);
        img.Parent := F; //pnl;
        img.Align := alClient;
        img.Picture.Assign(FTestBitmap);
        img.Transparent := (ATransparentCode <> 0);
      end;
  end;
  F.Show;
  AScreenshot.Width := FTestBitmap.Width;
  AScreenshot.Height := FTestBitmap.Height;
  {$IFDEF FPC}
  F.PaintTo(AScreenshot.Canvas, -GetSystemMetrics(SM_CXSIZEFRAME), -GetSystemMetrics(SM_CYCAPTION)-GetSystemMetrics(SM_CYSIZEFRAME));
  {$ELSE}
  F.PaintTo(AScreenshot.Canvas, -1, -1);
  {$ENDIF}
  AScreenshot.SaveToFile('screenshot.bmp');
  F.Free;
end;

{ ACodeMode identifies the code which created the expected image
    0: blue rect inside red border, no transparency ("expected_blue_red.bmp")
    1: transparent border ("expected_red_transparent.bmp")
    2: transparent rect in center ("expected_blue_transparent.bmp")

  ATransparentMode identifies where Transparent is set to TRUE
    0: Transparent stays at FALSE
    1: immediately after TBitmap.Create
    2: immediately after SetSize
    3: after handle creation (i.e. background filling)
    4: at end
   11: like 1, set Transparent, then TransparentColor
   12: like 2, set Transparent, then TransparentColor
   13: like 3, set Transparent, then TransparentColor
   14: like 4, set Transparent, then TransparentColor
   21: like 1, set TransparentColor, then set Transparent
   22: like 2, set TransparentColor, then set Transparent
   23: like 3, set TransparentColor, then set Transparent
   24: like 4, set TransparentColor, then set Transparent
   31..34: like 1..4, set Transparent, TransparentMode=tmfixed, then TransparentColor
   41..44: like 1..4, set TransparentMode=tmFixed, Transparent, then TransparentColor
   51..54: like 1..4, set TransparentMode=tmFixed, TransparentColor, finally Transparent }
procedure TTransparentBitmapTest.DoTestTransparent(ACodeMode, ATransparentCode: Integer);

  procedure ApplyTransparent;
  begin
    case ATransparentCode div 10 of
      0:  // Set only Transparent
        begin
          FTestBitmap.Transparent := true;
        end;
      1:  // Set Transparent BEFORE TransparentColor
        begin
          FTestBitmap.Transparent := True;
          case ACodeMode of
            0: ; //
            1: FTestBitmap.TransparentColor := clRed;
            2: FTestBitmap.TransparentColor := clblue;
          end;
        end;
      2: // Set Transparent AFTER TransparentColor
        begin
          case ACodeMode of
            0: ; //
            1: FTestBitmap.TransparentColor := clRed;
            2: FTestBitmap.TransparentColor := clblue;
          end;
          FTestBitmap.Transparent := True;
        end;
      3: // Set Transparent, TransparentMode=tmfixed, then TransparentColor
        begin
          FTestBitmap.Transparent := True;
          FTestBitmap.TransparentMode := tmFixed;
          case ACodeMode of
            0: ;
            1: FTestBitmap.TransparentColor := clRed;
            2: FTestBitmap.TransparentColor := clBlue;
          end;
        end;
      4: // Set TransparentMode=tmfixed, Transparent, then TransparentColor
        begin
          FTestBitmap.TransparentMode := tmFixed;
          FTestBitmap.Transparent := True;
          case ACodeMode of
            0: ;
            1: FTestBitmap.TransparentColor := clRed;
            2: FTestBitmap.TransparentColor := clBlue;
          end;
        end;
      5: // Set TransparentMode=tmFixed, TransparentColor, Transparent
        begin
          FTestBitmap.TransparentMode := tmFixed;
          case ACodeMode of
            0: ;
            1: FTestBitmap.TransparentColor := clRed;
            2: FTestBitmap.TransparentColor := clBlue;
          end;
          FTestBitmap.Transparent := True;
        end;
    end;
  end;

const
  ExpectedBmpFiles: array[0..2] of String = (
    'expected_redBorder_blueRect.bmp',
    'expected_red_transparent.bmp',    // TransparentColor is red, i.e. auto
    'expected_blue_transparent.bmp'    // must also set TransparentColor to clBlue
  );
var
  expectedBmp: TBitmap;
  actualBmp: TBitmap;
  equal: Boolean;
  msg: String;
begin
  FreeAndNil(FTestBitmap);
  expectedBmp := TBitmap.Create;
  try
    expectedBmp.LoadFromFile(EXPECTED_IMG_DIR + ExpectedBmpFiles[ACodeMode]);
    FTestBitmap := TBitmap.Create;
    try
      FTestBitmap.PixelFormat := FPixelFormat;

      // Mode 1: Set transparent immediately after TBitmap.Create;
      if (ATransparentCode mod 10 = 1) then
        ApplyTransparent;

      FTestBitmap.Width := expectedBmp.Width;
      FTestBitmap.Height := expectedBmp.Height;
      // Mode 2: Set transparent after TBitmap.SetSize;
      if ATransparentCode mod 10 = 2 then
        ApplyTransparent ;

      FTestBitmap.Canvas.Brush.Color := clRed;
      FTestBitmap.Canvas.FillRect(Rect(0, 0, FTestBitmap.Width, FTestBitmap.Height));
      // Mode 3: Set transparent after creating handle
      if ATransparentCode mod 10 = 3 then
        ApplyTransparent;

      FTestBitmap.Canvas.Brush.Color := clBlue;
      FTestBitmap.Canvas.FillRect(Rect(8, 8, 24, 24));

      // Mode 4: Set transparent after creating handle
      if ATransparentCode mod 10 = 4 then
        ApplyTransparent;

      FTestBitmap.SaveToFile('testbitmap.bmp');

      actualBmp := TBitmap.Create;
      try
        GetBitmapScreenshot(ATransparentCode, actualBmp);
        equal := CompareBitmaps(actualBmp, expectedBmp, msg);
        CheckEquals(true, equal, 'Bitmaps do not match (' + msg + ').')
      finally
        actualBmp.Free;
      end;
    finally
      FreeAndNil(FTestBitmap);
    end;
  finally
    expectedBmp.Free;
  end;
end;

// Test image without transparency
procedure TTransparentBitmapTest.NotTransparent;
begin
  DoTestTransparent(0, 0);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation
procedure TTransparentBitmapTest.BorderTransparent_AfterCreate;
begin
  DoTestTransparent(1, 1);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TTransparentBitmapTest.BorderTransparent_AfterCreate_T_TColor;
begin
  DoTestTransparent(1, 11);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TTransparentBitmapTest.BorderTransparent_AfterCreate_TColor_T;
begin
  DoTestTransparent(1, 21);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation. Sets also
// TransparentMode and -Color in this order:
//   - Transparent first, then TransparentMode=tmFixed, finally TransparentColor
procedure TTransparentBitmapTest.BorderTransparent_AfterCreate_T_TMode_TColor;
begin
  DoTestTransparent(1, 31);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation. Sets also
// TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then Transparent, finally TransparentColor
procedure TTransparentBitmapTest.BorderTransparent_AfterCreate_TMode_T_TColor;
begin
  DoTestTransparent(1, 41);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation. Sets also
// TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then TransparentColor, finally Transparent
procedure TTransparentBitmapTest.BorderTransparent_AfterCreate_TMode_TColor_T;
begin
  DoTestTransparent(1, 51);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize
procedure TTransparentBitmapTest.BorderTransparent_AfterSetSize;
begin
  DoTestTransparent(1, 2);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TTransparentBitmapTest.BorderTransparent_AfterSetSize_T_TColor;
begin
  DoTestTransparent(1, 12);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TTransparentBitmapTest.BorderTransparent_AfterSetSize_TColor_T;
begin
  DoTestTransparent(1, 22);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set  after Bitmap.SetSize.
// Sets also TransparentMode and -Color in this order:
//   - Transparent first, then TransparentMode=tmFixed, finally TransparentColor
procedure TTransparentBitmapTest.BorderTransparent_AfterSetSize_T_TMode_TColor;
begin
  DoTestTransparent(1, 32);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set  after Bitmap.SetSize.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then Transparent, finally TransparentColor
procedure TTransparentBitmapTest.BorderTransparent_AfterSetSize_TMode_T_TColor;
begin
  DoTestTransparent(1, 42);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set  after Bitmap.SetSize.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then TransparentColor, finally Transparent
procedure TTransparentBitmapTest.BorderTransparent_AfterSetSize_TMode_TColor_T;
begin
  DoTestTransparent(1, 52);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after handle creation
procedure TTransparentBitmapTest.BorderTransparent_AfterHandle;
begin
  DoTestTransparent(1, 3);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after handle creation. Sets also TransparentColor.
// Transparent is set before TransparentColor
procedure TTransparentBitmapTest.BorderTransparent_AfterHandle_T_TColor;
begin
  DoTestTransparent(1, 13);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after handle creation. Sets also TransparentColor.
// Transparent is set after TransparentColor
procedure TTransparentBitmapTest.BorderTransparent_AfterHandle_TColor_T;
begin
  DoTestTransparent(1, 23);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Handle createion.
// Sets also TransparentMode and -Color in this order:
//   - Transparent first, then TransparentMode=tmFixed, finally TransparentColor
procedure TTransparentBitmapTest.BorderTransparent_AfterHandle_T_TMode_TColor;
begin
  DoTestTransparent(1, 33);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Handle createion.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then Transparent, finally TransparentColor
procedure TTransparentBitmapTest.BorderTransparent_AfterHandle_TMode_T_TColor;
begin
  DoTestTransparent(1, 43);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Handle createion.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then TransparentColor, finally Transparent
procedure TTransparentBitmapTest.BorderTransparent_AfterHandle_TMode_TColor_T;
begin
  DoTestTransparent(1, 53);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end
procedure TTransparentBitmapTest.BorderTransparent_AtEnd;
begin
  DoTestTransparent(1, 4);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TTransparentBitmapTest.BorderTransparent_AtEnd_BeforeTransparentColor;
begin
  DoTestTransparent(1, 14);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TTransparentBitmapTest.BorderTransparent_AtEnd_TColor_T;
begin
  DoTestTransparent(1, 24);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end after all drawing.
// Sets also TransparentMode and -Color in this order:
//   - Transparent first, then TransparentMode=tmFixed, finally TransparentColor
procedure TTransparentBitmapTest.BorderTransparent_AtEnd_T_TMode_TColor;
begin
  DoTestTransparent(1, 34);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end after all drawing.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then Transparent, finally TransparentColor
procedure TTransparentBitmapTest.BorderTransparent_AtEnd_TMode_T_TColor;
begin
  DoTestTransparent(1, 44);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end after all drawing.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then TransparentColor, finally Transparent
procedure TTransparentBitmapTest.BorderTransparent_AtEnd_TMode_TColor_T;
begin
  DoTestTransparent(1, 54);
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after bitmap construction. Sets also TransparentColor.
// Transparent is set before TransparentColor
procedure TTransparentBitmapTest.InnerRectTransparent_AfterCreate_BeforeTransparentColor;
begin
  DoTestTransparent(2, 11);
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set immediately after bitmap construction.
// Sets also TransparentColor.
// Transparent is set after TransparentColor
procedure TTransparentBitmapTest.InnerRectTransparent_AfterCreate_AfterTransparentColor;
begin
  DoTestTransparent(2, 21);
end;

// Inner rectangle (blue) must be transparent (i.e. become white in this setup)
// Transparent-related properties are set immediately after Bitmap creation.
// Sets also TransparentMode and -Color in this order:
//   - Transparent first, then TransparentMode=tmFixed, finally TransparentColor
procedure TTransparentBitmapTest.InnerRectTransparent_AfterCreate_T_TMode_TColor;
begin
  DoTestTransparent(2, 31);
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set immediately after Bitmap creation.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then Transparent, finally TransparentColor
procedure TTransparentBitmapTest.InnerRectTransparent_AfterCreate_TMode_T_TColor;
begin
  DoTestTransparent(2, 41);
end;

// Inner rectangle (blue) must be transparent (= white in this setup)
// Transparent-related properties are set immediately after Bitmap creation.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then TransparentColor, finally Transparent
procedure TTransparentBitmapTest.InnerRectTransparent_AfterCreate_TMode_TColor_T;
begin
  DoTestTransparent(2, 51);
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after setting bitmap size. Set also TransparentColor
// Transparent is set before TransparentColor
procedure TTransparentBitmapTest.InnerRectTransparent_AfterSetSize_BeforeTransparentColor;
begin
  DoTestTransparent(2, 12);
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after setting bitmap size. Set also TransparentColor
// Transparent is set after TransparentColor
procedure TTransparentBitmapTest.InnerRectTransparent_AfterSetSize_AfterTransparentColor;
begin
  DoTestTransparent(2, 22);
end;

// Inner rectangle (blue) must be transparent (i.e. become white in this setup)
// Transparent-related properties are setting bitmap size.
// Sets also TransparentMode and -Color in this order:
//   - Transparent first, then TransparentMode=tmFixed, finally TransparentColor
procedure TTransparentBitmapTest.InnerRectTransparent_AfterSetSize_T_TMode_TColor;
begin
  DoTestTransparent(2, 32);
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are setting bitmap size.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then Transparent, finally TransparentColor
procedure TTransparentBitmapTest.InnerRectTransparent_AfterSetSize_TMode_T_TColor;
begin
  DoTestTransparent(2, 42);
end;

// Inner rectangle (blue) must be transparent (= white in this setup)
// Transparent-related properties are setting bitmap size.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then TransparentColor, finally Transparent
procedure TTransparentBitmapTest.InnerRectTransparent_AfterSetSize_TMode_TColor_T;
begin
  DoTestTransparent(2, 52);
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after handle creation. Sets also TransparentColor.
// Transparent is set before TransparentColor
procedure TTransparentBitmapTest.InnerRectTransparent_AfterHandle_BeforeTransparentColor;
begin
  DoTestTransparent(2, 13);
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after handle creation.
// Sets also TransparentColor.
// Transparent is set after TransparentColor
procedure TTransparentBitmapTest.InnerRectTransparent_AfterHandle_AfterTransparentColor;
begin
  DoTestTransparent(2, 23);
end;

// Inner rectangle (blue) must be transparent (i.e. become white in this setup)
// Transparent-related properties are set after Handle creation.
// Sets also TransparentMode and -Color in this order:
//   - Transparent first, then TransparentMode=tmFixed, finally TransparentColor
procedure TTransparentBitmapTest.InnerRectTransparent_AfterHandle_T_TMode_TColor;
begin
  DoTestTransparent(2, 33);
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set after Handle creation.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then Transparent, finally TransparentColor
procedure TTransparentBitmapTest.InnerRectTransparent_AfterHandle_TMode_T_TColor;
begin
  DoTestTransparent(2, 43);
end;

// Inner rectangle (blue) must be transparent (= white in this setup)
// Transparent-related properties are set after Handle creation.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then TransparentColor, finally Transparent
procedure TTransparentBitmapTest.InnerRectTransparent_AfterHandle_TMode_TColor_T;
begin
  DoTestTransparent(2, 53);
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set at end. Also sets TransparentColor.
// Transparent is set before TransparentColor
procedure TTransparentBitmapTest.InnerRectTransparent_AtEnd_BeforeTransparentColor;
begin
  DoTestTransparent(2, 14);
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set at end. Also sets TransparentColor.
// Transparent is set after TransparentColor
procedure TTransparentBitmapTest.InnerRectTransparent_AtEnd_AfterTransparentColor;
begin
  DoTestTransparent(2, 24);
end;

// Inner rectangle (blue) must be transparent (i.e. become white in this setup)
// Transparent-related properties are set at the end after all drawing.
// Sets also TransparentMode and -Color in this order:
//   - Transparent first, then TransparentMode=tmFixed, finally TransparentColor
procedure TTransparentBitmapTest.InnerRectTransparent_AtEnd_T_TMode_TColor;
begin
  DoTestTransparent(2, 34);
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set at the end after all drawing.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then Transparent, finally TransparentColor
procedure TTransparentBitmapTest.InnerRectTransparent_AtEnd_TMode_T_TColor;
begin
  DoTestTransparent(2, 44);
end;

// Inner rectangle (blue) must be transparent (= white in this setup)
// Transparent-related properties are set at the end after all drawing.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then TransparentColor, finally Transparent
procedure TTransparentBitmapTest.InnerRectTransparent_AtEnd_TMode_TColor_T;
begin
  DoTestTransparent(2, 54);
end;


{------------------------------------------------------------------------------}
{                           Custom test classes                                }
{------------------------------------------------------------------------------}
// Draw the test image on a 24bpp bitmap canvas
{$IFDEF FPC}
constructor TCustomDrawTransparentBitmapTest_24Bit.Create;
{$ELSE}
constructor TCustomDrawTransparentBitmapTest_24Bit.Create(AMethodName: String);
{$ENDIF}
begin
  inherited;
  FPixelFormat := pf24Bit;
  FTestMethod := tmDraw;
end;

// Assign the 24-bpp test bitmap to a TPicture
{$IFDEF FPC}
constructor TCustomAssignTransparentBitmapTest_24Bit.Create;
{$ELSE}
constructor TCustomAssignTransparentBitmapTest_24Bit.Create(AMethodName: String);
{$ENDIF}
begin
  inherited;
  FPixelFormat := pf24Bit;
  FTestMethod := tmAssign;
end;

// Draw the test image on a 32-bpp bitmap canvas
{$IFDEF FPC}
constructor TCustomDrawTransparentBitmapTest_32Bit.Create;
{$ELSE}
constructor TCustomDrawTransparentBitmapTest_32Bit.Create(AMethodName: String);
{$ENDIF}
begin
  inherited;
  FPixelFormat := pf32Bit;
  FTestMethod := tmDraw;
end;

// Assign the 32bpp test bitmap to a TPicture
{$IFDEF FPC}
constructor TCustomAssignTransparentBitmapTest_32Bit.Create;
{$ELSE}
constructor TCustomAssignTransparentBitmapTest_32Bit.Create(AMethodName: String);
{$ENDIF}
begin
  inherited;
  FPixelFormat := pf32Bit;
  FTestMethod := tmAssign;
end;

{ Include files for published test cases }

{$include tests_not_transparent.inc}
{$include tests_24bit_transparent.inc}
{$include tests_24bit_t_tcolor.inc}
{$include tests_24bit_tcolor_t.inc}
{$include tests_24bit_t_tmode_tcolor.inc}
{$include tests_24bit_tmode_t_tcolor.inc}
{$include tests_24bit_tmode_tcolor_t.inc}

{$include tests_32bit_transparent.inc}
{$include tests_32bit_t_tcolor.inc}
{$include tests_32bit_tcolor_t.inc}

initialization
 {$IFDEF FPC}
  RegisterTest('24Bit Tests/Not Transparent', TDrawTransparentBitmapTest_24Bit_NotTransparent);
  RegisterTest('24Bit Tests/Not Transparent', TAssignTransparentBitmapTest_24Bit_NotTransparent);
  RegisterTest('24Bit Tests/Set Transparent = true', TDrawTransparentBitmapTest_24Bit_Transparent);
  RegisterTest('24Bit Tests/Set Transparent = true', TAssignTransparentBitmapTest_24Bit_Transparent);
  RegisterTest('24Bit Tests/First set Transparent, then TransparentColor', TDrawTransparentBitmapTest_24Bit_T_TColor);
  RegisterTest('24Bit Tests/First set Transparent, then TransparentColor', TAssignTransparentBitmapTest_24Bit_T_TColor);
  RegisterTest('24Bit Tests/First set TransparentColor, then Transparent = true', TDrawTransparentBitmapTest_24Bit_TColor_T);
  RegisterTest('24Bit Tests/First set TransparentColor, then Transparent = true', TAssignTransparentBitmapTest_24Bit_TColor_T);
  RegisterTest('24Bit Tests/First set Transparent, then TransparentMode, finally TransparentColor', TDrawTransparentBitmapTest_24Bit_T_TMode_TColor);
  RegisterTest('24Bit Tests/First set Transparent, then TransparentMode, finally TransparentColor', TAssignTransparentBitmapTest_24Bit_T_TMode_TColor);
  RegisterTest('24Bit Tests/First set TransparentMode, then Transparent, finally TransparentColor', TDrawTransparentBitmapTest_24Bit_TMode_T_TColor);
  RegisterTest('24Bit Tests/First set TransparentMode, then Transparent, finally TransparentColor', TAssignTransparentBitmapTest_24Bit_TMode_T_TColor);
  RegisterTest('24Bit Tests/First set TransparentMode, then TransparentColor, finally Transparent', TDrawTransparentBitmapTest_24Bit_TMode_TColor_T);
  RegisterTest('24Bit Tests/First set TransparentMode, then TransparentColor, finally Transparent', TAssignTransparentBitmapTest_24Bit_TMode_TColor_T);

  RegisterTest('32Bit Tests/Transparent = false', TDrawTransparentBitmapTest_32Bit_NotTransparent);
  RegisterTest('32Bit Tests/Transparent = false', TAssignTransparentBitmapTest_32Bit_NotTransparent);
  RegisterTest('32Bit Tests/Transparent = true', TDrawTransparentBitmapTest_32Bit_Transparent);
  RegisterTest('32Bit Tests/Transparent = true', TAssignTransparentBitmapTest_32Bit_Transparent);
  RegisterTest('32Bit Tests/First set Transparent, then TransparentColor', TDrawTransparentBitmapTest_32Bit_T_TColor);
  RegisterTest('32Bit Tests/First set Transparent, then TransparentColor', TAssignTransparentBitmapTest_32Bit_T_TColor);
  RegisterTest('32Bit Tests/First set TransparentColor, then Transparent = true', TDrawTransparentBitmapTest_32Bit_TColor_T);
  RegisterTest('32Bit Tests/First set TransparentColor, then Transparent = true', TAssignTransparentBitmapTest_32Bit_TColor_T);

 {$ELSE}
 // FIX ME
  TestFramework.RegisterTest('pf24Bit Tests', TDrawTransparentBitmapTest_24Bit.Suite);
  TestFramework.RegisterTest('pf24Bit Tests', TAssignTransparentBitmapTest_24Bit.Suite);
  TestFramework.RegisterTest('pf32Bit Tests', TDrawTransparentBitmapTest_32Bit.Suite);
  TestFramework.RegisterTest('pf32Bit Tests', TAssignTransparentBitmapTest_32Bit.Suite);
 {$ENDIF} 

end.

