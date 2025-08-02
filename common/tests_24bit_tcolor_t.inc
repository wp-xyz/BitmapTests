{ tests_24Bit_tcolor_t.inc }

{ Contains all 24-bpp drawing tests which first set TransparentColor
  and then Transparent = true }

{ included by BitmapTransparentTests.pas }

{------------------------------------------------------------------------------}
{ 24-bit draw tests which first set TransparentColor and then Transparent      }
{------------------------------------------------------------------------------}

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set immediately after Bitmap creation
// TransparentColor is set first, then Transparent
procedure TDrawTransparentBitmapTest_24Bit_TColor_T.BorderTransparent_AfterCreate_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set after Bitmap.SetSize
// TransparentColor is set first, then Transparent
procedure TDrawTransparentBitmapTest_24Bit_TColor_T.BorderTransparent_AfterSetSize_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set after handle creation
// TransparentColor is set first, then Transparent
procedure TDrawTransparentBitmapTest_24Bit_TColor_T.BorderTransparent_AfterHandle_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set at the end, after all drawing operations.
// TransparentColor is set first, then Transparent
procedure TDrawTransparentBitmapTest_24Bit_TColor_T.BorderTransparent_AtEnd_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Bitmap is loaded from file.
// Transparent and TransparentColor are set BEFORE loading the bitmap.
// TransparentColor is set first, then Transparent
procedure TDrawTransparentBitmapTest_24Bit_TColor_T.BorderTransparent_BeforeLoad_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Bitmap is loaded from file.
// Transparent and TransparentColor are set AFTER loading the bitmap.
// TransparentColor is set first, then Transparent
procedure TDrawTransparentBitmapTest_24Bit_TColor_T.BorderTransparent_AfterLoad_TColor_T;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set immediately after Bitmap creation
// TransparentColor is set first, then Transparent
procedure TDrawTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_AfterCreate_TColor_T;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set after Bitmap.SetSize
// TransparentColor is set first, then Transparent
procedure TDrawTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_AfterSetSize_TColor_T;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set after handle creation.
// TransparentColor is set first, then Transparent
procedure TDrawTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_AfterHandle_TColor_T;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set at the end, after all drawing operations.
// TransparentColor is set frist, then Transparent
procedure TDrawTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_AtEnd_TColor_T;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Bitmap is loaded from file.
// Transparent and TransparentColor are set BEFORE loading the bitmap.
// TransparentColor is set first, then Transparent
procedure TDrawTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_BeforeLoad_TColor_T;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Bitmap is loaded from file.
// Transparent and TransparentColor are set AFTER loading the bitmap.
// TransparentColor is set first, then Transparent
procedure TDrawTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_AfterLoad_TColor_T;
begin
  inherited;
end;


{------------------------------------------------------------------------------}
{ 24-bit Assign tests which first set TransparentColor, then Transparent       }
{------------------------------------------------------------------------------}

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set immediately after Bitmap creation
// TransparentColor is set first, then Transparent
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.BorderTransparent_AfterCreate_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set after Bitmap.SetSize
// TransparentColor is set first, then Transparent
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.BorderTransparent_AfterSetSize_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set after handle creation
// TransparentColor is set first, then Transparent
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.BorderTransparent_AfterHandle_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set at the end, after all drawing operations.
// TransparentColor is set first, then Transparent
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.BorderTransparent_AtEnd_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Image is loaded from file. Transparent is set BEFORE loading the image.
// TransparentColor is set first, then Transparent
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.BorderTransparent_BeforeLoad_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Image is loaded from file. Transparent is set AFTER loading the image.
// TransparentColor is set first, then Transparent
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.BorderTransparent_AfterLoad_TColor_T;
begin
  inherited;
end;


// Border part must be transparent (= white in this setup)
// Image is loaded from file. Transparent is set BEFORE loading the image.
// TransparentColor is set first, then Transparent
procedure TAssignTransparentBitmapTest_24Bit_T_TColor.BorderTransparent_BeforeLoad_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Image is loaded from file. Transparent is set AFTER loading the image.
// TransparentColor is set first, then Transparent
procedure TAssignTransparentBitmapTest_24Bit_T_TColor.BorderTransparent_AfterLoad_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set immediately after Bitmap creation
// TransparentColor is set first, then Transparent
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_AfterCreate_TColor_T;
begin
  inherited
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set after Bitmap.SetSize
// TransparentColor is set first, then Transparent
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_AfterSetSize_TColor_T;
begin
  inherited
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set after handle creation.
// TransparentColor is set first, then Transparent
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_AfterHandle_TColor_T;
begin
  inherited
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set at the end, after all drawing operations.
// TransparentColor is set first, then Transparent
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_AtEnd_TColor_T;
begin
  inherited
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Image is loaded from file. Transparent is set BEFORE loading the image.
// TransparentColor is set first, then Transparent
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_BeforeLoad_TColor_T;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Image is loaded from file. Transparent is set AFTER loading the image.
// TransparentColor is set first, then Transparent
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_AfterLoad_TColor_T;
begin
  inherited;
end;

                                        (*
{------------------------------------------------------------------------------}
{                   24-bit Assign tests with Transparent = TRUE                }
{------------------------------------------------------------------------------}

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation
procedure TAssignTransparentBitmapTest_24Bit_Transparent.BorderTransparent_AfterCreate;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize
procedure TAssignTransparentBitmapTest_24Bit_Transparent.BorderTransparent_AfterSetSize;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after handle creation
procedure TAssignTransparentBitmapTest_24Bit_Transparent.BorderTransparent_AfterHandle;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end
procedure TAssignTransparentBitmapTest_24Bit_Transparent.BorderTransparent_AtEnd;
begin
  inherited;
end;
*)
