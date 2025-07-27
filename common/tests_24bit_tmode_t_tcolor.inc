{ tests_24Bit_tmode_t_tcolor.inc }

{ Contains all 24-bpp drawing tests which first set TransparentMode to fmFixed,
  then Transparent to true, finally set the TransparentColor }

{ included by BitmapTransparentTests.pas }

{------------------------------------------------------------------------------}
{ 24-bit draw tests for the execution order                                    }
{    TransparentMode-Transparent-TransparentColor                              }
{------------------------------------------------------------------------------}

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set immediately after Bitmap creation
// TransparentMode is set first, then Transparent, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_TMode_T_TColor.BorderTransparent_AfterCreate_TMode_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set after Bitmap.SetSize
// TransparentMode is set first, then Transparent, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_TMode_T_TColor.BorderTransparent_AfterSetSize_TMode_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set after handle creation
// TransparentMode is set first, then Transparent, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_TMode_T_TColor.BorderTransparent_AfterHandle_TMode_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set at the end, after all drawing operations.
// TransparentMode is set first, then Transparent, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_TMode_T_TColor.BorderTransparent_AtEnd_TMode_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set immediately after Bitmap creation
// TransparentMode is set first, then Transarent, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_TMode_T_TColor.InnerRectTransparent_AfterCreate_TMode_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set after Bitmap.SetSize
// TransparentMode is set first, then Transparent, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_TMode_T_TColor.InnerRectTransparent_AfterSetSize_TMode_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set after handle creation.
// TransparentMode is set first, then Transparent, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_TMode_T_TColor.InnerRectTransparent_AfterHandle_TMode_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set at the end, after all drawing operations.
// TransparentMode is set first, then Transparent, finally TransparentColor
procedure TDrawTransparentBitmapTest_24Bit_TMode_T_TColor.InnerRectTransparent_AtEnd_TMode_T_TColor;
begin
  inherited;
end;


{------------------------------------------------------------------------------}
{ 24-bit draw Assign with this execution order:                                }
{    TransparentMode-Transparent-TransparentColor                              }
{------------------------------------------------------------------------------}

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set immediately after Bitmap creation
// TransparentMode is set first, then Transparent, finally TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_TMode_T_TColor.BorderTransparent_AfterCreate_TMode_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set after Bitmap.SetSize
// TransparentMode is set first, then Transparent, finally TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_TMode_T_TColor.BorderTransparent_AfterSetSize_TMode_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set after handle creation
// TransparentMode is set first, then Transparent, finally TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_TMode_T_TColor.BorderTransparent_AfterHandle_TMode_T_TColor;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set at the end, after all drawing operations.
// TransparentMode is set first, then Transparent, finally TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_TMode_T_TColor.BorderTransparent_AtEnd_TMode_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set immediately after Bitmap creation
// TransparentMode is set first, then Transarent, finally TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_TMode_T_TColor.InnerRectTransparent_AfterCreate_TMode_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set after Bitmap.SetSize
// TransparentMode is set first, then Transparent, finally TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_TMode_T_TColor.InnerRectTransparent_AfterSetSize_TMode_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set after handle creation.
// TransparentMode is set first, then Transparent, finally TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_TMode_T_TColor.InnerRectTransparent_AfterHandle_TMode_T_TColor;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set at the end, after all drawing operations.
// TransparentMode is set first, then TransparentMode, finally TransparentColor
procedure TAssignTransparentBitmapTest_24Bit_TMode_T_TColor.InnerRectTransparent_AtEnd_TMode_T_TColor;
begin
  inherited;
end;

