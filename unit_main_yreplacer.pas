unit unit_main_yreplacer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  { TForm1 }

  TForm1 = class(TForm)
    Label1:TLabel;
    Timer1:TTimer;
    procedure Timer1Timer(Sender:TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ConvertImage(Sender: TObject);
  end;

var Form1: TForm1;

implementation

{$R *.lfm}

uses unit_YCbCr, BGRABitmap, BGRABitmapTypes, FPWriteJPEG;

{ TForm1 }

procedure TForm1.ConvertImage(Sender: TObject);
var image_input, image_output: TBGRABitmap;
    writer: TFPWriterJPEG;
    split_image_filtered, split_image_original: TCustomImage;
    fname_filtered, fname_original: String;
begin
  if ParamCount < 2 then
  begin
    fname_filtered := 'denoised.png'; // default
    fname_original := 'original.jpg'; // default
  end
  else
  begin
    fname_filtered := ParamStr(1);
    fname_original := ParamStr(2);
  end;

  if (not FileExists(fname_filtered)) or (not FileExists(fname_original)) then
  begin
    ShowMessage(
      'Combines Cb and Cr from Input with Y Channel from Output. Saves as Y_<output>.jpg'#13#10 +
      'Usage: ' + ParamStr(0) + ' <input> <output>'#13#10 +
      'Example: ' + ParamStr(0) + ' denoised.png original.jpg');
    Application.Terminate;
    Exit;
  end;

  image_input := TBGRABitmap.Create(fname_filtered);
  image_output := TBGRABitmap.Create(fname_original);

  extract_channels(image_input, split_image_filtered); // also sets array size!
  extract_channels(image_output, split_image_original); // also sets array size!

  add_noise(split_image_filtered.Width, split_image_filtered.Height, 1, false, split_image_filtered.Cb, split_image_filtered.Cr);

  // adjust Cb slightly to remove the yellow taint
  combine_channels_individual(1.002, split_image_original.Width, split_image_original.Height, split_image_original.Y, split_image_filtered.Cb, split_image_filtered.Cr, image_output);

  writer := TFPWriterJPEG.Create;
  writer.CompressionQuality := 100;
  writer.ProgressiveEncoding := false;

  image_output.SaveToFile('Y_' + ExtractFileNameWithoutExt(fname_original) + '.jpg', writer);

  FreeAndNil(image_input);
  FreeAndNil(image_output);
  FreeAndNil(writer);
end;

procedure TForm1.Timer1Timer(Sender:TObject);
begin
  Timer1.Enabled := false;
  Application.ProcessMessages;
  ConvertImage(Sender);
  Application.Terminate;
end;

end.

