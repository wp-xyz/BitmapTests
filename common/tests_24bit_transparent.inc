{ Tests_24Bit_Transparent.inc }
{ Contains all 24-bpp drawing tests having Transparent = TRUE }
{ included by BitmapTransparentTests.pas }

{------------------------------------------------------------------------------}
{                     24-bit draw tests with Transparent = TRUE                }
{------------------------------------------------------------------------------}

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation
procedure TDrawTransparentBitmapTest_24Bit_Transparent.BorderTransparent_AfterCreate;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize
procedure TDrawTransparentBitmapTest_24Bit_Transparent.BorderTransparent_AfterSetSize;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after handle creation
procedure TDrawTransparentBitmapTest_24Bit_Transparent.BorderTransparent_AfterHandle;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end
procedure TDrawTransparentBitmapTest_24Bit_Transparent.BorderTransparent_AtEnd;
begin
  inherited;
end;

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

