{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLTypes.pas
Версия            1.0
Создан            12.03.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Типы
}
{$Include Defines.inc}

unit sgeGraphicOpenGLTypes;

{$mode ObjFPC}{$H+}

interface

type
  //Тип вершин
  TsgeGraphicOpenGLVertexType = (
    vtPoint,      //Отдельные точки
    vtLine,       //Отдельные линии
    vtLineLoop,   //Соединенные линии
    vtTriangle    //Отдельные треугольники
  );


  //Информация о слое
  TsgeLayerInfo = record
    PosX: Single;   //Положение по X
    PosY: Single;   //Положение по Y
    ScaleX: Single; //Масштаб по X
    ScaleY: Single; //Масштаб по Y
  end;


implementation



end.

