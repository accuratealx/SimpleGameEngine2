{
Пакет             Simple Game Engine 2
Файл              sgeGraphicColors.pas
Версия            1.2
Создан            27.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Цвета
}
{$Include Defines.inc}

unit sgeGraphicColor;

{$mode objfpc}{$H+}

interface


type
  //Цвет [0..1]
  TsgeColor = record
    Red,
    Green,
    Blue,
    Alpha: Single;
  end;


  //2 цвета
  TsgeTwoColor = record
    Color1,
    Color2: TsgeColor;
  end;


  //4 цвета
  TsgeQuadColor = record
    Color1,
    Color2,
    Color3,
    Color4: TsgeColor;
  end;


  //Цвет [0..255]
  TsgeRGBA = record
    Red,
    Green,
    Blue,
    Alpha: Byte;
  end;


const
  //Портировано из Delphi7
  cBlack      : TsgeColor = (Red: 0;    Green: 0;    Blue: 0;    Alpha: 1);
  cMaroon     : TsgeColor = (Red: 0.5;  Green: 0;    Blue: 0;    Alpha: 1);
  cGreen      : TsgeColor = (Red: 0;    Green: 0.5;  Blue: 0;    Alpha: 1);
  cOlive      : TsgeColor = (Red: 0.5;  Green: 0.5;  Blue: 0;    Alpha: 1);
  cNavy       : TsgeColor = (Red: 0;    Green: 0;    Blue: 0.5;  Alpha: 1);
  cPurple     : TsgeColor = (Red: 0.5;  Green: 0;    Blue: 0.5;  Alpha: 1);
  cTeal       : TsgeColor = (Red: 0;    Green: 0.5;  Blue: 0.5;  Alpha: 1);
  cDarkGray   : TsgeColor = (Red: 0.25; Green: 0.25; Blue: 0.25; Alpha: 1);
  cGray       : TsgeColor = (Red: 0.5;  Green: 0.5;  Blue: 0.5;  Alpha: 1);
  cLightGray  : TsgeColor = (Red: 0.75; Green: 0.75; Blue: 0.75; Alpha: 1);
  cRed        : TsgeColor = (Red: 1;    Green: 0;    Blue: 0;    Alpha: 1);
  cLime       : TsgeColor = (Red: 0;    Green: 1;    Blue: 0;    Alpha: 1);
  cYellow     : TsgeColor = (Red: 1;    Green: 1;    Blue: 0;    Alpha: 1);
  cBlue       : TsgeColor = (Red: 0;    Green: 0;    Blue: 1;    Alpha: 1);
  cFuchsia    : TsgeColor = (Red: 1;    Green: 0;    Blue: 1;    Alpha: 1);
  cAqua       : TsgeColor = (Red: 0;    Green: 1;    Blue: 1;    Alpha: 1);
  cWhite      : TsgeColor = (Red: 1;    Green: 1;    Blue: 1;    Alpha: 1);

  //Прозрачный
  cTransparentBlack : TsgeColor = (Red: 0;    Green: 0;    Blue: 0;    Alpha: 0);
  cTransparentWhite : TsgeColor = (Red: 1;    Green: 1;    Blue: 1;    Alpha: 0);
  cTransparentRed   : TsgeColor = (Red: 1;    Green: 0;    Blue: 0;    Alpha: 0);
  cTransparentGreen : TsgeColor = (Red: 0;    Green: 1;    Blue: 0;    Alpha: 0);
  cTransparentBlue  : TsgeColor = (Red: 0;    Green: 0;    Blue: 1;    Alpha: 0);



function sgeRGBAToColor(R, G, B, A: Byte): TsgeColor;
function sgeRGBAToColor(Color: TsgeRGBA): TsgeColor;
function sgeColorToRGBA(R, G, B, A: Single): TsgeRGBA;
function sgeColorToRGBA(Color: TsgeColor): TsgeRGBA;
function sgeGetColor(R, G, B: Single; A: Single = 1): TsgeColor;
function sgeGetRGBA(R, G, B, A: Byte): TsgeRGBA;
function sgeChangeColorAlpha(Color: TsgeColor; Alpha: Single): TsgeColor;
function sgeGetRandomColor(Alpha: Single = 1): TsgeColor;
function sgeGetTwoColor(Color1, Color2: TsgeColor): TsgeTwoColor;
function sgeGetQuadColor(Color1, Color2, Color3, Color4: TsgeColor): TsgeQuadColor;



implementation


function sgeRGBAToColor(R, G, B, A: Byte): TsgeColor;
begin
  Result.Red := R / 255;
  Result.Green := G / 255;
  Result.Blue := B / 255;
  Result.Alpha := A / 255;
end;


function sgeRGBAToColor(Color: TsgeRGBA): TsgeColor;
begin
  Result := sgeRGBAToColor(Color.Red, Color.Green, Color.Blue, Color.Alpha);
end;


function sgeColorToRGBA(R, G, B, A: Single): TsgeRGBA;
begin
  Result.Red := Round(R * 255);
  Result.Green := Round(G * 255);
  Result.Blue := Round(B * 255);
  Result.Alpha := Round(A * 255);
end;


function sgeColorToRGBA(Color: TsgeColor): TsgeRGBA;
begin
  Result := sgeColorToRGBA(Color.Red, Color.Green, Color.Blue, Color.Alpha);
end;


function sgeGetColor(R, G, B, A: Single): TsgeColor;
begin
  //R
  if R < 0 then
    R := 0;
  if R > 1 then
    R := 1;

  //G
  if G < 0 then
    G := 0;
  if G > 1 then
    G := 1;

  //B
  if B < 0 then
    B := 0;
  if B > 1 then
    B := 1;

  //A
  if A < 0 then
    A := 0;
  if A > 1 then
    A := 1;

  //Result
  Result.Red := R;
  Result.Green := G;
  Result.Blue := B;
  Result.Alpha := A;
end;


function sgeGetRGBA(R, G, B, A: Byte): TsgeRGBA;
begin
  Result.Red := R;
  Result.Green := G;
  Result.Blue := B;
  Result.Alpha := A;
end;


function sgeChangeColorAlpha(Color: TsgeColor; Alpha: Single): TsgeColor;
begin
  if Alpha < 0 then
    Alpha := 0;
  if Alpha > 1 then
    Alpha := 1;

  Result := Color;
  Result.Alpha := Alpha;
end;


function sgeGetRandomColor(Alpha: Single = 1): TsgeColor;
begin
  Result.Red := Random;
  Result.Green := Random;
  Result.Blue := Random;
  Result.Alpha := Alpha;
end;


function sgeGetTwoColor(Color1, Color2: TsgeColor): TsgeTwoColor;
begin
  Result.Color1 := Color1;
  Result.Color2 := Color2;
end;


function sgeGetQuadColor(Color1, Color2, Color3, Color4: TsgeColor): TsgeQuadColor;
begin
  Result.Color1 := Color1;
  Result.Color2 := Color2;
  Result.Color3 := Color3;
  Result.Color4 := Color4;
end;



end.

