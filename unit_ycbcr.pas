unit unit_YCbCr;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, LCLType, LCLIntf, GraphType, Math, BGRABitmap, BGRABitmapTypes;

Const
  CAccuracy = 65535;

type
  TPixelData = array of array of Word; // change this according to accuracy, for 255 change it to byte...
  TCustomImage = record
                   Width, Height: Integer;
                   Y, Cb, Cr: TPixelData;
                 end;

procedure extract_channels(source: TBGRABitmap; var destination: TCustomImage);
procedure combine_channels(Cb_Correction_Factor: Real; source: TCustomImage; var destination: TBGRABitmap);
procedure combine_channels_individual(Cb_Correction_Factor: Real; Width, Height: Integer; source_Y, source_Cb, Source_Cr: TPixelData; var destination: TBGRABitmap);
procedure render_individual_channels(source: TCustomImage; var destination: TImage);
procedure render_individual_channels_RGB(source: TImage; var destination: TImage);
procedure extract_average_4x4_blocks(Width, Height: Integer; source: TPixelData; var destination: TPixelData);
procedure replace_average_4x4_blocks(Width, Height: Integer; scale: Real; source: TPixelData; var destination: TPixelData);
procedure redistribute_4x4_blocks(Width, Height: Integer; var source: TPixelData);
procedure median_3x3(Width, Height: Integer; var source: TPixelData);
procedure weighted_average_3x3(Width, Height: Integer; var source: TPixelData);
procedure add_noise(Width, Height: Integer; percentage: Real; uniform: Boolean; var source_Cb, source_Cr: TPixelData);

implementation

function construct_rgb(x: Byte): TGraphicsColor;
begin
  result := x shl 16 + x shl 8 + x;
end;

function clamp_byte(x: Integer): Byte;
begin
  result := min(255, max(0, x));
end;

{
  extract color space
  http://avisynth.nl/index.php/Colorimetry

  ( 0.0 <= [Y,R,G,B] <= 1.0),
  (-1.0 <= [Cb,Cr]   <= 1.0) and
  Kr + Kg + Kb = 1

  // Rec.709
  Kr = 0.2126;
  Kg = 0.7152;
  Kb = 0.0722;

  // FCC
  Kr = 0.3;
  Kg = 0.59;
  Kb = 0.11;
}

const // Rec.601
  Kr = 0.299;
  Kg = 0.587;
  Kb = 0.114;

procedure extract_channels(source: TBGRABitmap; var destination: TCustomImage);
var lv, lv2: Integer;
    R, G, B: Real;
    Y, Cb, Cr: Real;
    p: PBGRAPixel;
begin
  destination.Width := source.Width;
  destination.Height := source.Height;

  SetLength(destination.Y, source.Width, source.Height);
  SetLength(destination.Cb, source.Width, source.Height);
  SetLength(destination.Cr, source.Width, source.Height);

  for lv2 := 0 to source.Height - 1 do
  begin
    p := source.Scanline[lv2];
    for lv := 0 to source.Width - 1 do
    begin
      // normalize to 0..1
      R := p^.red / 255;
      G := p^.green / 255;
      B := p^.blue / 255;
      inc(p);

      Y  := Kr * R + Kg * G + Kb * B;
      Cb := (B - Y) / (1 - Kb);
      Cr := (R - Y) / (1 - Kr);

      destination.Y[lv, lv2] := round(Y*CAccuracy);
      destination.Cb[lv, lv2] := round((Cb + 1)*CAccuracy / 2);
      destination.Cr[lv, lv2] := round((Cr + 1)*CAccuracy / 2);
    end;
  end;
end;

procedure combine_channels(Cb_Correction_Factor: Real; source: TCustomImage; var destination: TBGRABitmap);
begin
  combine_channels_individual(Cb_Correction_Factor, source.Width, source.Height, source.Y, source.Cb, source.Cr, destination);
end;

procedure combine_channels_individual(Cb_Correction_Factor: Real; Width, Height: Integer; source_Y, source_Cb, Source_Cr: TPixelData; var destination: TBGRABitmap);
var lv, lv2: Integer;
    R, G, B: Real;
    Y, Cb, Cr: Real;
    p: PBGRAPixel;
begin
  for lv2 := 0 to Height - 1 do
  begin
    p := destination.Scanline[lv2];
    for lv := 0 to Width - 1 do
    begin
      Y := source_Y[lv, lv2] / CAccuracy;
      Cb := source_Cb[lv, lv2]*2*Cb_Correction_Factor / CAccuracy - 1;
      Cr := source_Cr[lv, lv2]*2 / CAccuracy - 1;

      R := Y + Cr * (1 - Kr);
      G := Y - Cb * (1 - Kb) * Kb / Kg - Cr * (1 - Kr) * Kr / Kg;
      B := Y + Cb * (1 - Kb);

      p^.red := clamp_byte(round(R*255));
      p^.green := clamp_byte(round(G*255));
      p^.blue := clamp_byte(round(B*255));
      p^.alpha := 255;

      inc(p);
    end;
  end;
end;

procedure render_individual_channels(source: TCustomImage; var destination: TImage);
var lv, lv2: Integer;
begin
  destination.Hide;

  destination.Width := source.Width * 3;
  destination.Height := source.Height;

  for lv2 := 0 to source.Height - 1 do
  for lv := 0 to source.Width - 1 do
  begin
    destination.Canvas.Pixels[lv, lv2] := construct_rgb(clamp_byte(source.Y[lv, lv2] shr 8));
    destination.Canvas.Pixels[lv + source.Width * 1, lv2] := construct_rgb(clamp_byte(source.Cb[lv, lv2] shr 8));
    destination.Canvas.Pixels[lv + source.Width * 2, lv2] := construct_rgb(clamp_byte(source.Cr[lv, lv2] shr 8));
  end;

  destination.Show;
end;

procedure render_individual_channels_RGB(source: TImage; var destination: TImage);
var lv, lv2: Integer;
begin
  destination.Hide;

  destination.Width := source.Width * 3;
  destination.Height := source.Height;

  for lv2 := 0 to source.Height - 1 do
  for lv := 0 to source.Width - 1 do
  begin
    destination.Canvas.Pixels[lv, lv2] := construct_rgb(source.Canvas.Pixels[lv, lv2] and $0000FF); // R
    destination.Canvas.Pixels[lv + source.Width * 1, lv2] := construct_rgb(source.Canvas.Pixels[lv, lv2] and $00FF00 shr 8); // G
    destination.Canvas.Pixels[lv + source.Width * 2, lv2] := construct_rgb(source.Canvas.Pixels[lv, lv2] and $FF0000 shr 16); // B
  end;

  destination.Show;
end;

procedure extract_average_4x4_blocks(Width, Height: Integer; source: TPixelData; var destination: TPixelData);
var lv, lv2, i, j, average: Integer;
begin
  SetLength(destination, Width div 4, Height div 4);

  for lv2 := 0 to Height div 4 - 1 do
  begin
    for lv := 0 to Width div 4 - 1 do
    begin
      average := 0;
      for j := 0 to 3 do // y
      for i := 0 to 3 do // x
      inc(average, source[lv*4 + i, lv2*4 + j]);

      // save as 1/4th sized picture
      destination[lv, lv2] := average shr 4;
    end;
  end;
end;

procedure replace_average_4x4_blocks(Width, Height: Integer; scale: Real; source: TPixelData; var destination: TPixelData);
var lv, lv2, i, j, average, new_average: Integer;
begin
  for lv2 := 0 to Height div 4 - 1 do
  begin
    for lv := 0 to Width div 4 - 1 do
    begin
      average := 0;
      for j := 0 to 3 do // y
      for i := 0 to 3 do // x
      inc(average, destination[lv*4 + i, lv2*4 + j]);

      average := average shr 4;
      new_average := source[lv, lv2];

      for j := 0 to 3 do // y
      for i := 0 to 3 do // x
      destination[lv*4 + i, lv2*4 + j] := round((destination[lv*4 + i, lv2*4 + j] - average) * scale + new_average);
    end;
  end;
end;

procedure redistribute_4x4_blocks(Width, Height: Integer; var source: TPixelData);
var outline_values, outline_count: array[1..16] of integer;

procedure add_outline_value(color_a, color_b: word);
var lv, diff: integer;
begin
  diff := color_a - color_b;
  for lv := 1 to 16 do
  begin
    if (outline_values[lv] = diff) or (outline_values[lv] = -1) then // either found or at the end of the existing values
    begin
      outline_values[lv] := diff;
      inc(outline_count[lv]);
      break;
    end;
  end;
end;

var lv, lv2, i, j, Height4, Width4, value, value_count, amount: Integer;
    temp: TPixelData;
begin
  temp := copy(source, 0, Length(source));
  Height4 := Height div 4 - 1;
  Width4 := Width div 4 - 1;

  for lv2 := 0 to Height4 do
  begin
    for lv := 0 to Width4 do
    begin
      // collect outline pixel difference
      for i:= 1 to 16 do
      begin
        outline_values[i] := -1;
        outline_count[i] := 0;
      end;

      // left
      if lv > 0 then
        for i := 0 to 3 do
        add_outline_value(temp[lv*4 - 1, lv2*4 + i], temp[lv*4, lv2*4 + i]);

      // right
      if lv < width4 then
        for i := 0 to 3 do
        add_outline_value(temp[lv*4 + 4, lv2*4 + i], temp[lv*4 + 3, lv2*4 + i]);

      // top
      if lv2 > 0 then
        for i := 0 to 3 do
        add_outline_value(temp[lv*4 + i, lv2*4 - 1], temp[lv*4 + i, lv2*4]);

      // bottom
      if lv2 < Height4 then
        for i := 0 to 3 do
        add_outline_value(temp[lv*4 + i, lv2*4 + 4], temp[lv*4 + i, lv2*4 + 3]);

      // collect statistic
      value := outline_values[1];
      amount := outline_count[1];
      value_count := 1;

      // find minimum, apply average on same amounts
      for i := 2 to 16 do
      begin
        if outline_count[i] > amount then
        begin
          amount := outline_count[i];
          value := outline_values[i];
          value_count := 1;
        end
        else
        if outline_count[i] = amount then
        begin
          amount := outline_count[i];
          inc(value, outline_values[i]);
          inc(value_count);
        end;
      end;

      value := value div value_count;

      // apply delta...
      if value <> 0 then
      begin
        for i := 0 to 3 do
        for j := 0 to 3 do
        source[lv*4 + i, lv2*4 + j] := min(CAccuracy, max(0, source[lv*4 + i, lv2*4 + j] + value));
      end;
    end;
  end;
end;

procedure median_3x3(Width, Height: Integer; var source: TPixelData);
var median_array: array[1..9] of Word;

procedure array_swap(idx1: Byte);
var temp: Word;
begin
  temp := median_array[idx1];
  median_array[idx1] := median_array[idx1 + 1];
  median_array[idx1 + 1] := temp;
end;

var temp: TPixelData;
    lv, lv2, i, j, median_idx: Integer;
    sorted: Boolean;
begin
  temp := copy(source, 0, Length(source));

  for lv2 := 1 to Height - 2 do
  begin
    for lv := 1 to Width - 2 do
    begin
      // ### 3x3 median filter (sort by value and take middle element) ###
      // populate array
      median_idx := 1;
      for j := -1 to 1 do // y
      for i := -1 to 1 do // x
      begin
        median_array[median_idx] := temp[lv - i, lv2 - j];
        inc(median_idx);
      end;

      // bubblesort arrays
      repeat
        sorted := true;

        for i := 1 to 9 - 1 do
        if median_array[i] > median_array[i + 1] then
        begin
          array_swap(i);
          sorted := false;
        end;
      until sorted;

      source[lv, lv2] := median_array[5]; // take the middle
    end;
  end;
end;


procedure weighted_average_3x3(Width, Height: Integer; var source: TPixelData);
var temp: TPixelData;
    lv, lv2, average_3x3: Integer;
begin
  temp := copy(source, 0, Length(source));

  // another two weighted average passes on the final image...
  // using weights 1 2 1
  //               2 4 2
  //               1 2 1
  for lv2 := 1 to Height - 2 do
  begin
    for lv := 1 to Width - 2 do
    begin
      average_3x3 :=
        temp[lv - 1, lv2 - 1] +
        temp[lv + 0, lv2 - 1] shl 1 +
        temp[lv + 1, lv2 - 1] +

        temp[lv - 1, lv2 + 0] shl 1 +
        temp[lv + 0, lv2 + 0] shl 2 +
        temp[lv + 1, lv2 + 0] shl 1 +

        temp[lv - 1, lv2 + 1] +
        temp[lv + 0, lv2 + 1] shl 1 +
        temp[lv + 1, lv2 + 1];

      average_3x3 := average_3x3 shr 4;

      source[lv, lv2] := average_3x3;
    end;
  end;
end;


procedure add_noise(Width, Height: Integer; percentage: Real; uniform: Boolean; var source_Cb, source_Cr: TPixelData);
var lv, lv2, percentage10, noise : Integer;
begin
  percentage10 := round(percentage * 10);

  for lv2 := 0 to Height - 1 do
  for lv := 0 to Width - 1 do
  begin
    noise := (Random(percentage10) - Random(percentage10))*CAccuracy div 1000;
    source_Cb[lv, lv2] := min(CAccuracy, max(0, source_Cb[lv, lv2] + noise));

    if not uniform then noise := (Random(percentage10) - Random(percentage10))*CAccuracy div 1000;
    source_Cr[lv, lv2] := min(CAccuracy, max(0, source_Cr[lv, lv2] + noise));
  end;
end;

begin
  Randomize;
end.

