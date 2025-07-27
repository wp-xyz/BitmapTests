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

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set immediately after Bitmap creation
// TransparentColor is set first, then Transparent
procedure TDrawTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_AfterCreate_AfterTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set after Bitmap.SetSize
// TransparentColor is set first, then Transparent
procedure TDrawTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_AfterSetSize_AfterTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set after handle creation.
// TransparentColor is set first, then Transparent
procedure TDrawTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_AfterHandle_AfterTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set at the end, after all drawing operations.
// TransparentColor is set frist Transparent
procedure TDrawTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_AtEnd_AfterTransparentColor;
begin
  inherited;
end;


{------------------------------------------------------------------------------}
{ 24-bit Assign tests which first set TransparentColor, then Transparent       }
{------------------------------------------------------------------------------}

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set immediately after Bitmap creation
// Transparent is set first, then TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.BorderTransparent_AfterCreate_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set after Bitmap.SetSize
// Transparent is set first, then TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.BorderTransparent_AfterSetSize_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set after handle creation
// Transparent is set first, then TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.BorderTransparent_AfterHandle_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set at the end, after all drawing operations.
// Transparent is set first, then TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.BorderTransparent_AtEnd_TColor_T;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set immediately after Bitmap creation
// Transparent is set first, then TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_AfterCreate_AfterTransparentColor;
begin
  inherited
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set after Bitmap.SetSize
// Transparent is set before TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_AfterSetSize_AfterTransparentColor;
begin
  inherited
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set after handle creation.
// Transparent is set before TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_AfterHandle_AfterTransparentColor;
begin
  inherited
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set at the end, after all drawing operations.
// Transparent is set before TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_TColor_T.InnerRectTransparent_AtEnd_AfterTransparentColor;
begin
  inherited
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
