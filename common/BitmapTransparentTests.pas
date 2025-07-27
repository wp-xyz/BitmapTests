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
    procedure NoTransparency; virtual;

    // -- (Red) border is transparent ---
    // Set Transparent, but not Transparentcolor and not TransparentMode
    procedure BorderTransparent_AfterCreate; virtual;
    procedure BorderTransparent_AfterSetSize; virtual;
    procedure BorderTransparent_AfterHandle; virtual;
    procedure BorderTransparent_AtEnd; virtual;

    // Always sets TransparentColor
    // Set Transparent BEFORE TransparentColor
    procedure BorderTransparent_AfterCreate_BeforeTransparentColor; virtual;
    procedure BorderTransparent_AfterSetSize_BeforeTransparentColor; virtual;
    procedure BorderTransparent_AfterHandle_BeforeTransparentColor; virtual;
    procedure BorderTransparent_AtEnd_BeforeTransparentColor; virtual;
    // Set Transparent AFTER TransparentColor
    procedure BorderTransparent_AfterCreate_AfterTransparentColor; virtual;
    procedure BorderTransparent_AfterSetSize_AfterTransparentColor; virtual;
    procedure BorderTransparent_AfterHandle_AfterTransparentColor; virtual;
    procedure BorderTransparent_AtEnd_AfterTransparentColor; virtual;
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
  end;


  { Test cases for drawing a 24-bit bitmap on a canvas }

  TDrawTransparentBitmapTest_24Bit = class(TTransparentBitmapTest)
  public
    {$IFDEF FPC}
    constructor Create; override;
    {$ELSE}
    constructor Create(AMethodName: String); override;
    {$ENDIF}

  published
    // Do not set Transparent --> opaque
    procedure NoTransparency; override;

    // -- (Red) border is transparent ---

    // Set Transparent, but not TransparentColor
    procedure BorderTransparent_AfterCreate; override;
    procedure BorderTransparent_AfterSetSize; override;
    procedure BorderTransparent_AfterHandle; override;
    procedure BorderTransparent_AtEnd; override;

    // Always sets TransparentColor
    // Set Transparent before TransparentColor
    procedure BorderTransparent_AfterCreate_BeforeTransparentColor; override;
    procedure BorderTransparent_AfterSetSize_BeforeTransparentColor; override;
    procedure BorderTransparent_AfterHandle_BeforeTransparentColor; override;
    procedure BorderTransparent_AtEnd_BeforeTransparentColor; override;
    // Set Transparent after TransparentColor
    procedure BorderTransparent_AfterCreate_AfterTransparentColor; override;
    procedure BorderTransparent_AfterSetSize_AfterTransparentColor; override;
    procedure BorderTransparent_AfterHandle_AfterTransparentColor; override;
    procedure BorderTransparent_AtEnd_AfterTransparentColor; override;
    // Set Transparent, then TransparentMode=tmFixed, finally TransparentColor
    procedure BorderTransparent_AfterCreate_T_TMode_TColor; override;
    procedure BorderTransparent_AfterSetSize_T_TMode_TColor; override;
    procedure BorderTransparent_AfterHandle_T_TMode_TColor; override;
    procedure BorderTransparent_AtEnd_T_TMode_TColor; override;
    // Set TransparentMode=tmFixed, then Transparent, finally TransparentColor
    procedure BorderTransparent_AfterCreate_TMode_T_TColor; override;
    procedure BorderTransparent_AfterSetSize_TMode_T_TColor; override;
    procedure BorderTransparent_AfterHandle_TMode_T_TColor; override;
    procedure BorderTransparent_AtEnd_TMode_T_TColor; override;
    // Set TransparentMode=tmFixed, then TransparentColor, finally Transparent
    procedure BorderTransparent_AfterCreate_TMode_TColor_T; override;
    procedure BorderTransparent_AfterSetSize_TMode_TColor_T; override;
    procedure BorderTransparent_AfterHandle_TMode_TColor_T; override;
    procedure BorderTransparent_AtEnd_TMode_TColor_T; override;

    // -- (Blue) inner rectangle is transparent ---

    // Always sets TransparentColor
    // Transparent is set before TransparentColor
    procedure InnerRectTransparent_AfterCreate_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AfterSetSize_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AfterHandle_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AtEnd_BeforeTransparentColor; override;
    // Transparent is set after TransparentColor
    procedure InnerRectTransparent_AfterCreate_AfterTransparentColor; override;
    procedure InnerRectTransparent_AfterSetSize_AfterTransparentColor; override;
    procedure InnerRectTransparent_AfterHandle_AfterTransparentColor; override;
    procedure InnerRectTransparent_AtEnd_AfterTransparentColor; override;

  end;


  { Test cases for assigning a 24-bit map to an image }

  TAssignTransparentBitmapTest_24Bit = class(TTransparentBitmapTest)
  public
    {$IFDEF FPC}
    constructor Create; override;
    {$ELSE}
    constructor Create(AMethodName: String); override;
    {$ENDIF}

  published
    // Do not set Transparent
    procedure NoTransparency; override;

    // -- (Red) border is transparent ---

    // Set Transparent, but not TransparentColor
    procedure BorderTransparent_AfterCreate; override;
    procedure BorderTransparent_AfterSetSize; override;
    procedure BorderTransparent_AfterHandle; override;
    procedure BorderTransparent_AtEnd; override;

    // Always sets TransparentColor
    // Transparent is set BEFORE TransparentColor
    procedure BorderTransparent_AfterCreate_BeforeTransparentColor; override;
    procedure BorderTransparent_AfterSetSize_BeforeTransparentColor; override;
    procedure BorderTransparent_AfterHandle_BeforeTransparentColor; override;
    procedure BorderTransparent_AtEnd_BeforeTransparentColor; override;
    // Transparent is set AFTER TransparentColor
    procedure BorderTransparent_AfterCreate_AfterTransparentColor; override;
    procedure BorderTransparent_AfterSetSize_AFterTransparentColor; override;
    procedure BorderTransparent_AfterHandle_AfterTransparentColor; override;
    procedure BorderTransparent_AtEnd_AfterTransparentColor; override;

    // -- (Blue) inner rectangle is transparent ---

    // Always sets TransparentColor
    // Transparent is set BEFORE TransparentColor
    procedure InnerRectTransparent_AfterCreate_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AfterSetSize_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AfterHandle_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AtEnd_BeforeTransparentColor; override;
    // Transparent is set AFTER TransparentColor
    procedure InnerRectTransparent_AfterCreate_AfterTransparentColor; override;
    procedure InnerRectTransparent_AfterSetSize_AfterTransparentColor; override;
    procedure InnerRectTransparent_AfterHandle_AfterTransparentColor; override;
    procedure InnerRectTransparent_AtEnd_AfterTransparentColor; override;

  end;


  { Test cases for drawing a 32-bit map on a canvas }

  TDrawTransparentBitmapTest_32Bit = class(TTransparentBitmapTest)
  public
    {$IFDEF FPC}
    constructor Create; override;
    {$ELSE}
    constructor Create(AMethodName: String); override;
    {$ENDIF}

  published
    // Do not set Transparent --> opaque
    procedure NoTransparency; override;

    // -- (Red) border is transparent ---

    // Set Transparent, but not TransparentColor
    procedure BorderTransparent_AfterCreate; override;
    procedure BorderTransparent_AfterSetSize; override;
    procedure BorderTransparent_AfterHandle; override;
    procedure BorderTransparent_AtEnd; override;

    // Always sets TransparentColor
    // Transparent is set BEFORE TransparentColor
    procedure BorderTransparent_AfterCreate_BeforeTransparentColor; override;
    procedure BorderTransparent_AfterSetSize_BeforeTransparentColor; override;
    procedure BorderTransparent_AfterHandle_BeforeTransparentColor; override;
    procedure BorderTransparent_AtEnd_BeforeTransparentColor; override;
    // Transparent is set AFTER TransparentColor
    procedure BorderTransparent_AfterCreate_AfterTransparentColor; override;
    procedure BorderTransparent_AfterSetSize_AfterTransparentColor; override;
    procedure BorderTransparent_AfterHandle_AfterTransparentColor; override;
    procedure BorderTransparent_AtEnd_AfterTransparentColor; override;

    // -- (Blue) inner rectangle is transparent ---

    // Always sets TransparentColor
    // Transparent is set BEFORE TransparentColor
    procedure InnerRectTransparent_AfterCreate_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AfterSetSize_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AfterHandle_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AtEnd_BeforeTransparentColor; override;
    // Transparent is set AFTER TransparentColor
    procedure InnerRectTransparent_AfterCreate_AfterTransparentColor; override;
    procedure InnerRectTransparent_AfterSetSize_AfterTransparentColor; override;
    procedure InnerRectTransparent_AfterHandle_AfterTransparentColor; override;
    procedure InnerRectTransparent_AtEnd_AfterTransparentColor; override;

  end;


  { Test cases for assigning a 32-bit map on a canvas }

  TAssignTransparentBitmapTest_32Bit = class(TTransparentBitmapTest)
  public
    {$IFDEF FPC}
    constructor Create; override;
    {$ELSE}
    constructor Create(AMethodName: String); override;
    {$ENDIF}

  published
    // Do not set Transparent --> opaque
    procedure NoTransparency; override;

    // -- (Red) border is transparent ---

    // Set Transparent, but not TransparentColor.
    procedure BorderTransparent_AfterCreate; override;
    procedure BorderTransparent_AfterSetSize; override;
    procedure BorderTransparent_AfterHandle; override;
    procedure BorderTransparent_AtEnd; override;

    // Always sets TransparentColor
    // Transparent is set BEFORE TransparentColor
    procedure BorderTransparent_AfterCreate_BeforeTransparentColor; override;
    procedure BorderTransparent_AfterSetSize_BeforeTransparentColor; override;
    procedure BorderTransparent_AfterHandle_BeforeTransparentColor; override;
    procedure BorderTransparent_AtEnd_BeforeTransparentColor; override;
    // Transparent is set AFTER TransparentColor
    procedure BorderTransparent_AfterCreate_AfterTransparentColor; override;
    procedure BorderTransparent_AfterSetSize_AfterTransparentColor; override;
    procedure BorderTransparent_AfterHandle_AfterTransparentColor; override;
    procedure BorderTransparent_AtEnd_AfterTransparentColor; override;


    // -- (Blue) inner rectangle is transparent ---

    // Always sets TransparentColor
    // Transparent is set BEFORE TransparentColor
    procedure InnerRectTransparent_AfterCreate_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AfterSetSize_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AfterHandle_BeforeTransparentColor; override;
    procedure InnerRectTransparent_AtEnd_BeforeTransparentColor; override;
    // Transparent is set AFTEr TransparentColor
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
procedure TTransparentBitmapTest.NoTransparency;
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
procedure TTransparentBitmapTest.BorderTransparent_AfterCreate_BeforeTransparentColor;
begin
  DoTestTransparent(1, 11);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TTransparentBitmapTest.BorderTransparent_AfterCreate_AfterTransparentColor;
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
procedure TTransparentBitmapTest.BorderTransparent_AfterSetSize_BeforeTransparentColor;
begin
  DoTestTransparent(1, 12);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TTransparentBitmapTest.BorderTransparent_AfterSetSize_AfterTransparentColor;
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
procedure TTransparentBitmapTest.BorderTransparent_AfterHandle_BeforeTransparentColor;
begin
  DoTestTransparent(1, 13);
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after handle creation. Sets also TransparentColor.
// Transparent is set after TransparentColor
procedure TTransparentBitmapTest.BorderTransparent_AfterHandle_AfterTransparentColor;
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
procedure TTransparentBitmapTest.BorderTransparent_AtEnd_AfterTransparentColor;
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
// Transparent is set immediately after bitmap construction. Sets also TransparentColor.
// Transparent is set after TransparentColor
procedure TTransparentBitmapTest.InnerRectTransparent_AfterCreate_AfterTransparentColor;
begin
  DoTestTransparent(2, 21);
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

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after handle creation. Sets also TransparentColor.
// Transparent is set before TransparentColor
procedure TTransparentBitmapTest.InnerRectTransparent_AfterHandle_BeforeTransparentColor;
begin
  DoTestTransparent(2, 13);
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after handle creation. Sets also TransparentColor.
// Transparent is set after TransparentColor
procedure TTransparentBitmapTest.InnerRectTransparent_AfterHandle_AfterTransparentColor;
begin
  DoTestTransparent(2, 23);
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


{-------------------------------------------------------------------------------
                        TDrawTransparentBitmapTest_24Bit
-------------------------------------------------------------------------------}

{$IFDEF FPC}
constructor TDrawTransparentBitmapTest_24Bit.Create;
{$ELSE}
constructor TDrawTransparentBitmapTest_24Bit.Create(AMethodName: String);
{$ENDIF}
begin
  inherited;
  FPixelFormat := pf24Bit;
  FTestMethod := tmDraw
end;

// Test image without transparency
procedure TDrawTransparentBitmapTest_24Bit.NoTransparency;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AfterCreate;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation. Sets also TransparentColor.
// Transparent is set before TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AfterCreate_BeforeTransparentColor;
begin
  inherited;
end;

// Transparent is set immediately after Bitmap creation. Sets also TransparentColor.
// Transparent is set after TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AfterCreate_AfterTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation.
// Sets also TransparentMode and -Color in this order:
//   - Transparent first, then TransparentMode=tmFixed, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AfterCreate_T_TMode_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then Transparent, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AfterCreate_TMode_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then TransparentColor, finally Transparent
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AfterCreate_TMode_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AfterSetSize;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize. Sets also TransparentColor.
// Transparent is set before TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AfterSetSize_BeforeTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize. Sets also TransparentColor.
// Transparent is set after TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AfterSetSize_AfterTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap.SetSize.
// Sets also TransparentMode and -Color in this order:
//   - Transparent first, then TransparentMode=tmFixed, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AfterSetSize_T_TMode_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap.SetSize.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then Transparent, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AfterSetSize_TMode_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap.SetSize.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then TransparentColor, finally Transparent
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AfterSetSize_TMode_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after handle creation
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AfterHandle;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Handle creation. Sets also TransparentColor.
// Transparent is set before TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AfterHandle_BeforeTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Handle creation. Sets also TransparentColor.
// Transparent is set after TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AfterHandle_AfterTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set Handle creation.
// Sets also TransparentMode and -Color in this order:
//   - Transparent first, then TransparentMode=tmFixed, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AfterHandle_T_TMode_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Handle creation.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then Transparent, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AfterHandle_TMode_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Handle creation.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed first, then TransparentColor, finally Transparent
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AfterHandle_TMode_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AtEnd;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end. Sets also TransparentColor.
// Transparent is set before TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AtEnd_BeforeTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end. Sets also TransparentColor.
// Transparent is set after TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AtEnd_AfterTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at the end after all painting.
// Sets also TransparentMode and -Color in this order:
//   - Transparent first, then TransparentMode=tmFixed, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AtEnd_T_TMode_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at the end after all painting.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed, then Transparent, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AtEnd_TMode_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at the end after all painting.
// Sets also TransparentMode and -Color in this order:
//   - TransparentMode=tmFixed, then TransparentColor, finally Transparent
procedure TDrawTransparentBitmapTest_24Bit.BorderTransparent_AtEnd_TMode_TColor_T;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after bitmap construction. Sets also TransparentColor.
// Transparent is set before TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.InnerRectTransparent_AfterCreate_BeforeTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after bitmap construction. Sets also TransparentColor.
// Transparent is set after TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.InnerRectTransparent_AfterCreate_AfterTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after setting bitmap size. Set also TransparentColor
// Transparent is set BEFORE TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.InnerRectTransparent_AfterSetSize_BeforeTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after setting bitmap size. Set also TransparentColor
// Transparent is set AFTER TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.InnerRectTransparent_AfterSetSize_AfterTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after handle creation. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.InnerRectTransparent_AfterHandle_BeforeTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after handle creation. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.InnerRectTransparent_AfterHandle_AfterTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set at end. Also sets TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.InnerRectTransparent_AtEnd_BeforeTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set at end. Also sets TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TDrawTransparentBitmapTest_24Bit.InnerRectTransparent_AtEnd_AfterTransparentColor;
begin
  inherited;
end;


{------------------------------------------------------------------------------}
{                    TAssignTransparentBitmapTest_24Bit                        }
{------------------------------------------------------------------------------}

{$IFDEF FPC}
constructor TAssignTransparentBitmapTest_24Bit.Create;
{$ELSE}
constructor TAssignTransparentBitmapTest_24Bit.Create(AMethodName: String);
{$ENDIF}
begin
  inherited;
  FPixelFormat := pf24Bit;
  FTestMethod := tmAssign;
end;

// Test image without transparency
procedure TAssignTransparentBitmapTest_24Bit.NoTransparency;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation
procedure TAssignTransparentBitmapTest_24Bit.BorderTransparent_AfterCreate;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TAssignTransparentBitmapTest_24Bit.BorderTransparent_AfterCreate_BeforeTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TAssignTransparentBitmapTest_24Bit.BorderTransparent_AfterCreate_AfterTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize
procedure TAssignTransparentBitmapTest_24Bit.BorderTransparent_AfterSetSize;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TAssignTransparentBitmapTest_24Bit.BorderTransparent_AfterSetSize_BeforeTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TAssignTransparentBitmapTest_24Bit.BorderTransparent_AfterSetSize_AfterTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after handle creation
procedure TAssignTransparentBitmapTest_24Bit.BorderTransparent_AfterHandle;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after handle creation. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TAssignTransparentBitmapTest_24Bit.BorderTransparent_AfterHandle_BeforeTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after handle creation. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TAssignTransparentBitmapTest_24Bit.BorderTransparent_AfterHandle_AfterTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end
procedure TAssignTransparentBitmapTest_24Bit.BorderTransparent_AtEnd;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TAssignTransparentBitmapTest_24Bit.BorderTransparent_AtEnd_BeforeTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TAssignTransparentBitmapTest_24Bit.BorderTransparent_AtEnd_AfterTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after bitmap construction. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor.
procedure TAssignTransparentBitmapTest_24Bit.InnerRectTransparent_AfterCreate_BeforeTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after bitmap construction. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor.
procedure TAssignTransparentBitmapTest_24Bit.InnerRectTransparent_AfterCreate_AfterTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after setting bitmap size. Set also TransparentColor
// Transparent is set BEFORE TransparentColor
procedure TAssignTransparentBitmapTest_24Bit.InnerRectTransparent_AfterSetSize_BeforeTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after setting bitmap size. Set also TransparentColor
// Transparent is set AFTER TransparentColor
procedure TAssignTransparentBitmapTest_24Bit.InnerRectTransparent_AfterSetSize_AfterTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after handle creation. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TAssignTransparentBitmapTest_24Bit.InnerRectTransparent_AfterHandle_BeforeTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after handle creation. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TAssignTransparentBitmapTest_24Bit.InnerRectTransparent_AfterHandle_AfterTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set at end. Also sets TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TAssignTransparentBitmapTest_24Bit.InnerRectTransparent_AtEnd_BeforeTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set at the end.
// Transparent is set AFTER TransparentColor
procedure TAssignTransparentBitmapTest_24Bit.InnerRectTransparent_AtEnd_AfterTransparentColor;
begin
  inherited;
end;


{ TDrawTransparentBitmapTest_32Bit }

{$IFDEF FPC}
constructor TDrawTransparentBitmapTest_32Bit.Create;
{$ELSE}
constructor TDrawTransparentBitmapTest_32Bit.Create(AMethodName: String);
{$ENDIF}
begin
  inherited;
  FPixelFormat := pf32Bit;
  FTestMethod := tmDraw;
end;

// Test image without transparency
procedure TDrawTransparentBitmapTest_32Bit.NoTransparency;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation
procedure TDrawTransparentBitmapTest_32Bit.BorderTransparent_AfterCreate;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TDrawTransparentBitmapTest_32Bit.BorderTransparent_AfterCreate_BeforeTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TDrawTransparentBitmapTest_32Bit.BorderTransparent_AfterCreate_AfterTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize
procedure TDrawTransparentBitmapTest_32Bit.BorderTransparent_AfterSetSize;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TDrawTransparentBitmapTest_32Bit.BorderTransparent_AfterSetSize_BeforeTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TDrawTransparentBitmapTest_32Bit.BorderTransparent_AfterSetSize_AfterTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after handle creation
procedure TDrawTransparentBitmapTest_32Bit.BorderTransparent_AfterHandle;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after handle creation. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TDrawTransparentBitmapTest_32Bit.BorderTransparent_AfterHandle_BeforeTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after handle creation. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TDrawTransparentBitmapTest_32Bit.BorderTransparent_AfterHandle_AfterTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end
procedure TDrawTransparentBitmapTest_32Bit.BorderTransparent_AtEnd;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end. Sets also TransparentColor.
// Transparent is set before TransparentColor
procedure TDrawTransparentBitmapTest_32Bit.BorderTransparent_AtEnd_BeforeTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TDrawTransparentBitmapTest_32Bit.BorderTransparent_AtEnd_AfterTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after bitmap construction. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TDrawTransparentBitmapTest_32Bit.InnerRectTransparent_AfterCreate_BeforeTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after bitmap construction. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TDrawTransparentBitmapTest_32Bit.InnerRectTransparent_AfterCreate_AfterTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after setting bitmap size. Set also TransparentColor
// Transparent is set BEFORE TransparentColor
procedure TDrawTransparentBitmapTest_32Bit.InnerRectTransparent_AfterSetSize_BeforeTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after setting bitmap size. Set also TransparentColor
// Transparent is set AFTER TransparentColor
procedure TDrawTransparentBitmapTest_32Bit.InnerRectTransparent_AfterSetSize_AfterTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after handle creation. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TDrawTransparentBitmapTest_32Bit.InnerRectTransparent_AfterHandle_BeforeTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after handle creation. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TDrawTransparentBitmapTest_32Bit.InnerRectTransparent_AfterHandle_AfterTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set at end. Also sets TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TDrawTransparentBitmapTest_32Bit.InnerRectTransparent_AtEnd_BeforeTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set at end. Also sets TransparentColor.
// Transparent is set AFTEr TransparentColor
procedure TDrawTransparentBitmapTest_32Bit.InnerRectTransparent_AtEnd_AfterTransparentColor;
begin
  inherited;
end;


{ TAssignTransparentBitmapTest_32Bit }

{$IFDEF FPC}
constructor TAssignTransparentBitmapTest_32Bit.Create;
{$ELSE}
constructor TAssignTransparentBitmapTest_32Bit.Create(AMethodName: String);
{$ENDIF}
begin
  inherited;
  FPixelFormat := pf32Bit;
  FTestMethod := tmAssign;
end;

// Test image without transparency
procedure TAssignTransparentBitmapTest_32Bit.NoTransparency;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation
procedure TAssignTransparentBitmapTest_32Bit.BorderTransparent_AfterCreate;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TAssignTransparentBitmapTest_32Bit.BorderTransparent_AfterCreate_BeforeTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TAssignTransparentBitmapTest_32Bit.BorderTransparent_AfterCreate_AfterTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize
procedure TAssignTransparentBitmapTest_32Bit.BorderTransparent_AfterSetSize;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TAssignTransparentBitmapTest_32Bit.BorderTransparent_AfterSetSize_BeforeTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TAssignTransparentBitmapTest_32Bit.BorderTransparent_AfterSetSize_AfterTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after handle creation
procedure TAssignTransparentBitmapTest_32Bit.BorderTransparent_AfterHandle;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after handle creation. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor.
procedure TAssignTransparentBitmapTest_32Bit.BorderTransparent_AfterHandle_BeforeTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after handle creation. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor.
procedure TAssignTransparentBitmapTest_32Bit.BorderTransparent_AfterHandle_AfterTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end
procedure TAssignTransparentBitmapTest_32Bit.BorderTransparent_AtEnd;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TAssignTransparentBitmapTest_32Bit.BorderTransparent_AtEnd_BeforeTransparentColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TAssignTransparentBitmapTest_32Bit.BorderTransparent_AtEnd_AfterTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after bitmap construction. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TAssignTransparentBitmapTest_32Bit.InnerRectTransparent_AfterCreate_BeforeTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after b(/ itmap construction. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TAssignTransparentBitmapTest_32Bit.InnerRectTransparent_AfterCreate_AfterTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after setting bitmap size. Set also TransparentColor
// Transparent is set BEFORE TransparentColor
procedure TAssignTransparentBitmapTest_32Bit.InnerRectTransparent_AfterSetSize_BeforeTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after setting bitmap size. Set also TransparentColor
// Transparent is set AFTER TransparentColor
procedure TAssignTransparentBitmapTest_32Bit.InnerRectTransparent_AfterSetSize_AfterTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after handle creation. Sets also TransparentColor.
// Transparent is set BEFORE TransparentColor;
procedure TAssignTransparentBitmapTest_32Bit.InnerRectTransparent_AfterHandle_BeforeTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set immediately after handle creation. Sets also TransparentColor.
// Transparent is set AFTER TransparentColor;
procedure TAssignTransparentBitmapTest_32Bit.InnerRectTransparent_AfterHandle_AfterTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set at end. Also sets TransparentColor.
// Transparent is set BEFORE TransparentColor
procedure TAssignTransparentBitmapTest_32Bit.InnerRectTransparent_AtEnd_BeforeTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent is set at end. Also sets TransparentColor.
// Transparent is set AFTER TransparentColor
procedure TAssignTransparentBitmapTest_32Bit.InnerRectTransparent_AtEnd_AfterTransparentColor;
begin
  inherited;
end;

initialization
 {$IFDEF FPC}
  RegisterTest('pf24Bit Tests', TDrawTransparentBitmapTest_24Bit);
  RegisterTest('pf24Bit Tests', TAssignTransparentBitmapTest_24Bit);
  RegisterTest('pf32Bit Tests', TDrawTransparentBitmapTest_32Bit);
  RegisterTest('pf32Bit Tests', TAssignTransparentBitmapTest_32Bit);
 {$ELSE}
  TestFramework.RegisterTest('pf24Bit Tests', TDrawTransparentBitmapTest_24Bit.Suite);
  TestFramework.RegisterTest('pf24Bit Tests', TAssignTransparentBitmapTest_24Bit.Suite);
  TestFramework.RegisterTest('pf32Bit Tests', TDrawTransparentBitmapTest_32Bit.Suite);
  TestFramework.RegisterTest('pf32Bit Tests', TAssignTransparentBitmapTest_32Bit.Suite);
 {$ENDIF} 

end.

