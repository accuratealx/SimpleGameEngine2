{
Пакет             Simple Game Engine 2
Файл              .pas
Версия            1.0
Создан            12.03.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Свойство элемента отображения: Поворот
}
{$Include Defines.inc}

unit sgeDisplayElementItemPropertyRotate;

{$mode ObjFPC}{$H+}

interface

type
  TsgeDisplayElementItemPropertyRotate = class
  private
    FAngle: Single; //Угол в радианах

    procedure SetAngleDegree(AAngle: Single);
    function  GetAngleDegree: Single;
  public
    constructor Create(Angle: Single = 0; X: Single = 0; Y: Single = 0);

    property Angle: Single read FAngle write FAngle;
    property AngleDegree: Single read GetAngleDegree write SetAngleDegree;
  end;


implementation

const
  PI = 3.1415926538;


procedure TsgeDisplayElementItemPropertyRotate.SetAngleDegree(AAngle: Single);
begin
  FAngle := AAngle * (PI / 180.0);
end;


function TsgeDisplayElementItemPropertyRotate.GetAngleDegree: Single;
begin
  Result := round(FAngle * (180.0 / PI));
end;


constructor TsgeDisplayElementItemPropertyRotate.Create(Angle: Single; X: Single; Y: Single);
begin
  FAngle := Angle;
end;



end.

