program BitmapTestProject;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner,
  BitmapTransparentTests;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

