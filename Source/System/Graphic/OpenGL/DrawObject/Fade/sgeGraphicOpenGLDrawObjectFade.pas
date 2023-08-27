{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectFade.pas
Версия            1.0
Создан            27.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Затемнение экрана
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectFade;

{$mode ObjFPC}{$H+}

interface

uses
  sgeColor, sgeGraphicOpenGLDrawObjectFadeItemList, sgeGraphicOpenGLDrawObjectFadeItem,
  sgeGraphicOpenGL, sgeGraphicOpenGLShaderProgram, sgeGraphicOpenGLVertexArrayObject, sgeGraphicOpenGLBuffer;

type
  TsgeGraphicOpenGLDrawObjectFade = class
  private
    FFadeList: TsgeGraphicOpenGLDrawObjectFadeItemList;
    FVAO: TsgeGraphicOpenGLVertexArrayObject;
    FShaderProgram: TsgeGraphicOpenGLShaderProgram;
    FVertexBuffer: TsgeGraphicOpenGLBuffer;

  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(Mode: TsgeFadeMode; Color: TsgeColor; Time: Cardinal = 1000; ID: Integer = 0; TimeProc: TsgeFadeProc = nil);

    procedure Draw(Graphic: TsgeGraphicOpenGL);
  end;


implementation

uses
  sgeGraphicOpenGLTypes, sgeGraphicOpenGLShaderProgramTable, sgeGraphicOpenGLCoordBuffer;


constructor TsgeGraphicOpenGLDrawObjectFade.Create;
const
  SHADER_NAME = 'Fade';
var
  Buff: TsgeGraphicOpenGLCoordBuffer;
begin
  //Найти шейдерную программу в таблице
  FShaderProgram := OpenGLShaderProgramTable.Get(SHADER_NAME);

  //Создать вершинный буфер
  FVertexBuffer := TsgeGraphicOpenGLBuffer.Create;

  //Создать VAO
  FVAO := TsgeGraphicOpenGLVertexArrayObject.Create;

  //Привязать буфер вершин к VAO
  FVAO.Attach;
  FVertexBuffer.Attach;
  FVAO.BindVertexCoord(FVertexBuffer);

  //Заполнить вершины
  Buff := TsgeGraphicOpenGLCoordBuffer.Create;
  Buff.AddQuad(1, -1, -1, 1);
  FVertexBuffer.SetData(Buff);
  Buff.Free;

  //Создать список затемнений
  FFadeList := TsgeGraphicOpenGLDrawObjectFadeItemList.Create(True);
end;


destructor TsgeGraphicOpenGLDrawObjectFade.Destroy;
begin
  //Удалить буфер вершин
  FVertexBuffer.Free;

  //Удалить объект настройки вывода
  FVAO.Free;

  //Удалить список затемнений
  FFadeList.Free;
end;


procedure TsgeGraphicOpenGLDrawObjectFade.Add(Mode: TsgeFadeMode; Color: TsgeColor; Time: Cardinal; ID: Integer; TimeProc: TsgeFadeProc);
var
  Item: TsgeGraphicOpenGLDrawObjectFadeItem;
begin
  Item := TsgeGraphicOpenGLDrawObjectFadeItem.Create(Mode, Color, Time, ID, TimeProc);
  FFadeList.Add(Item);
end;


procedure TsgeGraphicOpenGLDrawObjectFade.Draw(Graphic: TsgeGraphicOpenGL);
var
  FadeItem: TsgeGraphicOpenGLDrawObjectFadeItem;
  Color: TsgeColor;
begin
  //Нет переходов, выход
  if FFadeList.Count = 0 then
    Exit;

  //Ссылка на текущий переход
  FadeItem := FFadeList.Item[0];

  //Получить цвет
  Color := FadeItem.GetColor;

  //Проверить на завершение
  if FadeItem.Status = fsCompleted then
    FFadeList.Delete(0);

  //Выбрать объект
  FVAO.Attach;

  //Активировать программу
  FShaderProgram.Attach;

  //Передать параметры в программу
  FShaderProgram.SetColor(Color);

  //Нарисовать
  Graphic.DrawArray(vtTriangle, 0, 6);
end;



end.

