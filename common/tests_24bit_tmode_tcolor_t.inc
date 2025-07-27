{ tests_24Bit_tmode_tcolor_t.inc }

{ Contains all 24-bpp drawing tests which first set TransparentMode to tmFixed,
  then set the TransparentColor, and finally set Transparent to TRUE }

{ included by BitmapTransparentTests.pas }

{------------------------------------------------------------------------------}
{ 24-bit draw tests for the execution order                                    }
{    TransparentMode-TransparentColor-Transparent                              }
{------------------------------------------------------------------------------}

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set immediately after Bitmap creation
// TransparentMode is set first, then TransparentColor, finally Transparent
procedure TDrawTransparentBitmapTest_24Bit_TMode_TColor_T.BorderTransparent_AfterCreate_TMode_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set after Bitmap.SetSize
// TransparentMode is set first, then TransparentColor, finally Transparent
procedure TDrawTransparentBitmapTest_24Bit_TMode_TColor_T.BorderTransparent_AfterSetSize_TMode_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set after handle creation
// TransparentMode is set first, then TransparentColor, finally Transparent
procedure TDrawTransparentBitmapTest_24Bit_TMode_TColor_T.BorderTransparent_AfterHandle_TMode_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set at the end, after all drawing operations.
// TransparentMode is set first, then TransparentColor, finally Transparent
procedure TDrawTransparentBitmapTest_24Bit_TMode_TColor_T.BorderTransparent_AtEnd_TMode_TColor_T;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set immediately after Bitmap creation
// TransparentMode is set first, then TransparentColor, finally Transparent
procedure TDrawTransparentBitmapTest_24Bit_TMode_TColor_T.InnerRectTransparent_AfterCreate_TMode_TColor_T;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set after Bitmap.SetSize
// TransparentMode is set first, then TransparentColor, finally Transparent
procedure TDrawTransparentBitmapTest_24Bit_TMode_TColor_T.InnerRectTransparent_AfterSetSize_TMode_TColor_T;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set after handle creation.
// TransparentMode is set first, then TransparentColor, finally Transparent
procedure TDrawTransparentBitmapTest_24Bit_TMode_TColor_T.InnerRectTransparent_AfterHandle_TMode_TColor_T;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set at the end, after all drawing operations.
// TransparentMode is set first, then Transparent, finally Transparent
procedure TDrawTransparentBitmapTest_24Bit_TMode_TColor_T.InnerRectTransparent_AtEnd_TMode_TColor_T;
begin
  inherited;
end;


{------------------------------------------------------------------------------}
{ 24-bit draw Assign for the execution order                                   }
{    TransparentMode-TransparentColor-Transparent                              }
{------------------------------------------------------------------------------}

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set immediately after Bitmap creation
// TransparentMode is set first, then TransparentColor, finally Transparent
procedure TAssignTransparentBitmapTest_24Bit_TMode_TColor_T.BorderTransparent_AfterCreate_TMode_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set after Bitmap.SetSize
// TransparentMode is set first, then TransparentColor, finally Transparent
procedure TAssignTransparentBitmapTest_24Bit_TMode_TColor_T.BorderTransparent_AfterSetSize_TMode_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set after handle creation
// TransparentMode is set first, then TransparentColor, finally Transparent
procedure TAssignTransparentBitmapTest_24Bit_TMode_TColor_T.BorderTransparent_AfterHandle_TMode_TColor_T;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent-related properties are set at the end, after all drawing operations.
// TransparentMode is set first, then TransparentColor, finally Transparent
procedure TAssignTransparentBitmapTest_24Bit_TMode_TColor_T.BorderTransparent_AtEnd_TMode_TColor_T;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set immediately after Bitmap creation
// TransparentMode is set first, then TransarentColor, finally Transparent
procedure TAssignTransparentBitmapTest_24Bit_TMode_TColor_t.InnerRectTransparent_AfterCreate_TMode_TColor_T;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set after Bitmap.SetSize
// TransparentMode is set first, then TransparentColor, finally Transparent
procedure TAssignTransparentBitmapTest_24Bit_TMode_TColor_T.InnerRectTransparent_AfterSetSize_TMode_TColor_T;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set after handle creation.
// TransparentMode is set first, then TransparentColor, finally Transparent
procedure TAssignTransparentBitmapTest_24Bit_TMode_TColor_T.InnerRectTransparent_AfterHandle_TMode_TColor_T;
begin
  inherited;
end;

// Inner rectangle (blue) must be transparent (i.e. becomes white in this setup)
// Transparent-related properties are set at the end, after all drawing operations.
// TransparentMode is set first, then TransparentColor, finally Transparent
procedure TAssignTransparentBitmapTest_24Bit_TMode_TColor_T.InnerRectTransparent_AtEnd_TMode_TColor_T;
begin
  inherited;
end;

