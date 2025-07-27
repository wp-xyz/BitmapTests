{ tests_24Bit_t_tcolor.inc }

{ Contains all 24-bpp drawing tests which first set Transparent = TRUE
  and then TransparentColor }

{ included by BitmapTransparentTests.pas }

{------------------------------------------------------------------------------}
{ 24-bit draw tests which first set Transparent=true and then TransparentColor }
{------------------------------------------------------------------------------}

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set immediately after Bitmap creation
// Transparent is set first, then TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_T_TColor.BorderTransparent_AfterCreate_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set after Bitmap.SetSize
// Transparent is set first, then TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_T_TColor.BorderTransparent_AfterSetSize_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set after handle creation
// Transparent is set first, then TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_T_TColor.BorderTransparent_AfterHandle_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set at the end, after all drawing operations.
// Transparent is set first, then TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_T_TColor.BorderTransparent_AtEnd_BeforeTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set immediately after Bitmap creation
// Transparent is set first, then TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_T_TColor.InnerRectTransparent_AfterCreate_BeforeTransparentColor;
begin
  inherited
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set after Bitmap.SetSize
// Transparent is set before TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_T_TColor.InnerRectTransparent_AfterSetSize_BeforeTransparentColor;
begin
  inherited
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set after handle creation.
// Transparent is set before TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_T_TColor.InnerRectTransparent_AfterHandle_BeforeTransparentColor;
begin
  inherited
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set at the end, after all drawing operations.
// Transparent is set before TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_T_TColor.InnerRectTransparent_AtEnd_BeforeTransparentColor;
begin
  inherited
end;


{------------------------------------------------------------------------------}
{ 24-bit Assign tests which first set Transparent=true and then TranspColor    }
{------------------------------------------------------------------------------}

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set immediately after Bitmap creation
// Transparent is set first, then TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_T_TColor.BorderTransparent_AfterCreate_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set after Bitmap.SetSize
// Transparent is set first, then TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_T_TColor.BorderTransparent_AfterSetSize_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set after handle creation
// Transparent is set first, then TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_T_TColor.BorderTransparent_AfterHandle_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent and TransparentColor are set at the end, after all drawing operations.
// Transparent is set first, then TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_T_TColor.BorderTransparent_AtEnd_BeforeTransparentColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set immediately after Bitmap creation
// Transparent is set first, then TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_T_TColor.InnerRectTransparent_AfterCreate_BeforeTransparentColor;
begin
  inherited
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set after Bitmap.SetSize
// Transparent is set before TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_T_TColor.InnerRectTransparent_AfterSetSize_BeforeTransparentColor;
begin
  inherited
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set after handle creation.
// Transparent is set before TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_T_TColor.InnerRectTransparent_AfterHandle_BeforeTransparentColor;
begin
  inherited
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent and TransparentColor are set at the end, after all drawing operations.
// Transparent is set before TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_T_TColor.InnerRectTransparent_AtEnd_BeforeTransparentColor;
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
