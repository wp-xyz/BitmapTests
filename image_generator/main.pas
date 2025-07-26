unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
const
  DIR = '../expected_images/';
  BACKGROUND_COLOR = clWhite;
var
  bmp: TCustomBitmap;
begin
  ForceDirectories(DIR);
  bmp := TBitmap.Create;
  try
    bmp.SetSize(32, 32);
    bmp.Canvas.Brush.Color := clRed;   // red background
    bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);
    bmp.SaveToFile(DIR+'expected_bkgr.bmp');
    Image1.Picture.Assign(bmp);

    bmp.Canvas.Brush.Color := clBlue;  // blue inserted rectangle
    bmp.Canvas.FillRect(8, 8, 24, 24);
    bmp.SaveToFile(DIR+'expected_blueRect.bmp');
    Image2.Picture.Assign(bmp);

    bmp.canvas.Brush.Color := BACKGROUND_COLOR;  // color-transparent inserted rectangle
    bmp.Canvas.FillRect(8, 8, 24, 24);
    bmp.SaveToFile(DIR+'expected_blue_transparent.bmp');
    Image3.Picture.Assign(bmp);

    bmp.Canvas.FillRect(0, 0, 32, 32);
    bmp.Canvas.Brush.Color := clBlue;
    bmp.Canvas.FillRect(8, 8, 24, 24);
    bmp.SaveToFile(DIR+'expected_red_transparent.bmp');
    Image4.Picture.Assign(bmp);
  finally
    bmp.Free;
  end;


end;

end.

