{ tests_24Bit_t_tmode_tcolor.inc }

{ Contains all 24-bpp drawing tests which first set Transparent = TRUE,
  then set the TransparentMode to fmFixed, finally set TransparentColor }

{ included by BitmapTransparentTests.pas }

{------------------------------------------------------------------------------}
{ 24-bit draw tests for the execution order                                    }
{    Transparent-TransparentMode-TransparentColor                              }
{------------------------------------------------------------------------------}

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set immediately after Bitmap creation
// Transparent is set first, then TransparentMode, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_T_TMode_TColor.BorderTransparent_AfterCreate_T_TMode_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set after Bitmap.SetSize
// Transparent is set first, then TransparentMode, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_T_TMode_TColor.BorderTransparent_AfterSetSize_T_TMode_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set after handle creation
// Transparent is set first, then TransparentMode, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_T_TMode_TColor.BorderTransparent_AfterHandle_T_TMode_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set at the end, after all drawing operations.
// Transparent is set first, then TransparentMode, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_T_TMode_TColor.BorderTransparent_AtEnd_T_TMode_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set immediately after Bitmap creation
// Transparent is set first, then TransarentMode, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_T_TMode_TColor.InnerRectTransparent_AfterCreate_T_TMode_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set after Bitmap.SetSize
// Transparent is set first, then TransparentMode, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_T_TMode_TColor.InnerRectTransparent_AfterSetSize_T_TMode_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set after handle creation.
// Transparent is set first, then TransparentMode, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_T_TMode_TColor.InnerRectTransparent_AfterHandle_T_TMode_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set at the end, after all drawing operations.
// Transparent is set first, then TransparentMode, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_T_TMode_TColor.InnerRectTransparent_AtEnd_T_TMode_TColor;
begin
  inherited;
end;


{------------------------------------------------------------------------------}
{ 24-bit draw Assign for the execution order                                   }
{    Transparent-TransparentMode-TransparentColor                              }
{------------------------------------------------------------------------------}

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set immediately after Bitmap creation
// Transparent is set first, then TransparentMode, finally TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_T_TMode_TColor.BorderTransparent_AfterCreate_T_TMode_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set after Bitmap.SetSize
// Transparent is set first, then TransparentMode, finally TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_T_TMode_TColor.BorderTransparent_AfterSetSize_T_TMode_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set after handle creation
// Transparent is set first, then TransparentMode, finally TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_T_TMode_TColor.BorderTransparent_AfterHandle_T_TMode_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set at the end, after all drawing operations.
// Transparent is set first, then TransparentMode, finally TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_T_TMode_TColor.BorderTransparent_AtEnd_T_TMode_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set immediately after Bitmap creation
// Transparent is set first, then TransarentMode, finally TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_T_TMode_TColor.InnerRectTransparent_AfterCreate_T_TMode_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set after Bitmap.SetSize
// Transparent is set first, then TransparentMode, finally TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_T_TMode_TColor.InnerRectTransparent_AfterSetSize_T_TMode_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set after handle creation.
// Transparent is set first, then TransparentMode, finally TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_T_TMode_TColor.InnerRectTransparent_AfterHandle_T_TMode_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set at the end, after all drawing operations.
// Transparent is set first, then TransparentMode, finally TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_T_TMode_TColor.InnerRectTransparent_AtEnd_T_TMode_TColor;
begin
  inherited;
end;

