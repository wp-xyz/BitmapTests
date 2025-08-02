{ Tests_32Bit_Transparent.inc }
{ Contains all 32-bpp drawing tests having Transparent = TRUE }
{ included by BitmapTransparentTests.pas }

{------------------------------------------------------------------------------}
{                     32-bit draw tests with Transparent = TRUE                }
{------------------------------------------------------------------------------}

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation
procedure TDrawTransparentBitmapTest_32Bit_Transparent.BorderTransparent_AfterCreate;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize
procedure TDrawTransparentBitmapTest_32Bit_Transparent.BorderTransparent_AfterSetSize;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after handle creation
procedure TDrawTransparentBitmapTest_32Bit_Transparent.BorderTransparent_AfterHandle;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end
procedure TDrawTransparentBitmapTest_32Bit_Transparent.BorderTransparent_AtEnd;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Test bitmap is loaded from file
// Transparent is set before loading
procedure TDrawTransparentBitmapTest_32Bit_Transparent.BorderTransparent_BeforeLoad;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Test bitmap is loaded from file
// Transparent is set after loading
procedure TDrawTransparentBitmapTest_32Bit_Transparent.BorderTransparent_AfterLoad;
begin
  inherited;
end;

{------------------------------------------------------------------------------}
{                   32-bit Assign tests with Transparent = TRUE                }
{------------------------------------------------------------------------------}

// Border part must be transparent (= white in this setup)
// Transparent is set immediately after Bitmap creation
procedure TAssignTransparentBitmapTest_32Bit_Transparent.BorderTransparent_AfterCreate;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after Bitmap.SetSize
procedure TAssignTransparentBitmapTest_32Bit_Transparent.BorderTransparent_AfterSetSize;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set after handle creation
procedure TAssignTransparentBitmapTest_32Bit_Transparent.BorderTransparent_AfterHandle;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Transparent is set at end
procedure TAssignTransparentBitmapTest_32Bit_Transparent.BorderTransparent_AtEnd;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Image is loaded from file
// Transparent is set BEFORE loading image
procedure TAssignTransparentBitmapTest_32Bit_Transparent.BorderTransparent_BeforeLoad;
begin
  inherited;
end;

// Border part must be transparent (= white in this setup)
// Image is loaded from file
// Transparent is set AFTER loading image
procedure TAssignTransparentBitmapTest_32Bit_Transparent.BorderTransparent_AfterLoad;
begin
  inherited;
end;

