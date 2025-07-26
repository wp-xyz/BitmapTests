program BitmapTestProject_Delphi;

uses
  TestFramework,
  Forms,
  GUITestRunner,
  TextTestRunner,
  BitmapTransparentTests; // in '..\common\bitmaptransparenttests';
  
{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
