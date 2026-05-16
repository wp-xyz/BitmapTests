program BitmapTestProject;

{$mode objfpc}{$H+}

uses
  InterfaceBase, Interfaces, Forms, GuiTestRunner,
  BitmapTransparentTests;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  TestRunner.Caption := TestRunner.Caption + ' - [' + GetLCLWidgetTypeName + ']';
  Application.Run;
end.

