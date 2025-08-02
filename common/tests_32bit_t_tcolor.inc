{ tests_32Bit_t_tcolor.inc }

{ Contains all 32-bpp drawing tests which first set Transparent = TRUE
  and then TransparentColor }

{ included by BitmapTransparentTests.pas }

{------------------------------------------------------------------------------}
{ 32-bit draw tests which first set Transparent=true and then TransparentColor }
{------------------------------------------------------------------------------}

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set immediately after Bitmap creation
// Transparent is set first, then TransparentColor
procedure TDrawTransparentBitmapTest_32Bit_T_TColor.BorderTransparent_AfterCreate_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set after Bitmap.SetSize
// Transparent is set first, then TransparentColor
procedure TDrawTransparentBitmapTest_32Bit_T_TColor.BorderTransparent_AfterSetSize_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set after handle creation
// Transparent is set first, then TransparentColor
procedure TDrawTransparentBitmapTest_32Bit_T_TColor.BorderTransparent_AfterHandle_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set at the end, after all drawing operations.
// Transparent is set first, then TransparentColor
procedure TDrawTransparentBitmapTest_32Bit_T_TColor.BorderTransparent_AtEnd_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Bitmap is loaded from file.
// Transparent and TransparentColor are set BEFORE loading the bitmap.
// Transparent is set first, then TransparentColor
procedure TDrawTransparentBitmapTest_32Bit_T_TColor.BorderTransparent_BeforeLoad_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Bitmap is loaded from file.
// Transparent and TransparentColor are set AFTER loading the bitmap.
// Transparent is set first, then TransparentColor
procedure TDrawTransparentBitmapTest_32Bit_T_TColor.BorderTransparent_AfterLoad_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set immediately after Bitmap creation
// Transparent is set first, then TransparentColor
procedure TDrawTransparentBitmapTest_32Bit_T_TColor.InnerRectTransparent_AfterCreate_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set after Bitmap.SetSize
// Transparent is set before TransparentColor
procedure TDrawTransparentBitmapTest_32Bit_T_TColor.InnerRectTransparent_AfterSetSize_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set after handle creation.
// Transparent is set before TransparentColor
procedure TDrawTransparentBitmapTest_32Bit_T_TColor.InnerRectTransparent_AfterHandle_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set at the end, after all drawing operations.
// Transparent is set before TransparentColor
procedure TDrawTransparentBitmapTest_32Bit_T_TColor.InnerRectTransparent_AtEnd_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Bitmap is loaded from file.
// Transparent and TransparentColor are set BEFORE loading the bitmap.
// Transparent is set first, then TransparentColor
procedure TDrawTransparentBitmapTest_32Bit_T_TColor.InnerRectTransparent_BeforeLoad_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Bitmap is loaded from file.
// Transparent and TransparentColor are set AFTEr loading the bitmap.
// Transparent is set first, then TransparentColor
procedure TDrawTransparentBitmapTest_32Bit_T_TColor.InnerRectTransparent_AfterLoad_T_TColor;
begin
  inherited;
end;


{------------------------------------------------------------------------------}
{ 32-bit Assign tests which first set Transparent=true and then TranspColor    }
{------------------------------------------------------------------------------}

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set immediately after Bitmap creation
// Transparent is set first, then TransparentColor
procedure TAssignTransparentBitmapTest_32Bit_T_TColor.BorderTransparent_AfterCreate_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set after Bitmap.SetSize
// Transparent is set first, then TransparentColor
procedure TAssignTransparentBitmapTest_32Bit_T_TColor.BorderTransparent_AfterSetSize_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set after handle creation
// Transparent is set first, then TransparentColor
procedure TAssignTransparentBitmapTest_32Bit_T_TColor.BorderTransparent_AfterHandle_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set at the end, after all drawing operations.
// Transparent is set first, then TransparentColor
procedure TAssignTransparentBitmapTest_32Bit_T_TColor.BorderTransparent_AtEnd_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Image is loaded from file. Transparent is set BEFORE loading the image.
// Transparent is set first, then TransparentColor
procedure TAssignTransparentBitmapTest_32Bit_T_TColor.BorderTransparent_BeforeLoad_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Image is loaded from file. Transparent is set AFTER loading the image.
// Transparent is set first, then TransparentColor
procedure TAssignTransparentBitmapTest_32Bit_T_TColor.BorderTransparent_AfterLoad_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set immediately after Bitmap creation
// Transparent is set first, then TransparentColor
procedure TAssignTransparentBitmapTest_32Bit_T_TColor.InnerRectTransparent_AfterCreate_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set after Bitmap.SetSize
// Transparent is set before TransparentColor
procedure TAssignTransparentBitmapTest_32Bit_T_TColor.InnerRectTransparent_AfterSetSize_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set after handle creation.
// Transparent is set before TransparentColor
procedure TAssignTransparentBitmapTest_32Bit_T_TColor.InnerRectTransparent_AfterHandle_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set at the end, after all drawing operations.
// Transparent is set before TransparentColor
procedure TAssignTransparentBitmapTest_32Bit_T_TColor.InnerRectTransparent_AtEnd_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Image is loaded from file. Transparent is set BEFORE loading the image.
// Transparent is set first, then TransparentColor
procedure TAssignTransparentBitmapTest_32Bit_T_TColor.InnerRectTransparent_BeforeLoad_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Image is loaded from file. Transparent is set AFTER loading the image.
// Transparent is set first, then TransparentColor
procedure TAssignTransparentBitmapTest_32Bit_T_TColor.InnerRectTransparent_AfterLoad_T_TColor;
begin
  inherited;
end;


