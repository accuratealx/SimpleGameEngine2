{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectItemSprite.pas
Версия            1.0
Создан            29.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Спрайт
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectItemSprite;

{$mode ObjFPC}{$H+}

interface

uses
  sgeDisplayElementItemBase,
  sgeGraphicOpenGLBuffer, sgeGraphicOpenGLSprite,
  sgeGraphicOpenGLDrawObjectItemBase;


type
  TsgeGraphicOpenGLDrawObjectItemSprite = class(TsgeGraphicOpenGLDrawObjectItemBase)
  protected
    FGLSprite: TsgeGraphicOpenGLSprite;
    FTextureBuffer: TsgeGraphicOpenGLBuffer;

    procedure SetTexBuffer; virtual;

    function  GetShaderProgramName: String; override;
    procedure UserInit; override;
    procedure UserDone; override;
    procedure UserDrawBegin; override;
    procedure UserDrawEnd; override;

    procedure UpdateTextureBuffer(X1, Y1, X2, Y2: Single);
  public
    procedure Update(Element: TsgeDisplayElementItemBase); override;

  end;


implementation

uses
  sgeDisplayElementItemSprite,
  sgeGraphicOpenGLSpriteTable, sgeGraphicOpenGLCoordBuffer;


procedure TsgeGraphicOpenGLDrawObjectItemSprite.SetTexBuffer;
begin
  UpdateTextureBuffer(0, 1, 1, 0);
end;


function TsgeGraphicOpenGLDrawObjectItemSprite.GetShaderProgramName: String;
begin
  Result := 'Sprite';
end;


procedure TsgeGraphicOpenGLDrawObjectItemSprite.UserInit;
begin
  //Подготовить буфер с текстурными координатами
  FTextureBuffer := TsgeGraphicOpenGLBuffer.Create;

  //Привязать буфер координат
  FTextureBuffer.Attach;
  FVAO.BindTextureCoord(FTextureBuffer);
end;


procedure TsgeGraphicOpenGLDrawObjectItemSprite.UserDone;
begin
  //Удалить координатный буфер
  FTextureBuffer.Free;

  //Удалить спрайт из таблицы
  OpenGLSpriteTable.Delete(TsgeDisplayElementItemSprite(FElement).Sprite);
end;


procedure TsgeGraphicOpenGLDrawObjectItemSprite.UserDrawBegin;
begin
  //Привязать спрайт
  FGLSprite.Attach;
end;


procedure TsgeGraphicOpenGLDrawObjectItemSprite.UserDrawEnd;
begin
  //Отвязать спрайт
  FGLSprite.Detach;
end;


procedure TsgeGraphicOpenGLDrawObjectItemSprite.UpdateTextureBuffer(X1, Y1, X2, Y2: Single);
var
  Buff: TsgeGraphicOpenGLCoordBuffer;
begin
  //Создать промежуточный буфер
  Buff := TsgeGraphicOpenGLCoordBuffer.Create;

  //Задать текстурные координаты
  Buff.Clear;
  Buff.AddQuad(X1, Y1, X2, Y2);

  //Залить данные текстурных координат в видеокарту
  FTextureBuffer.SetData(Buff);

  //Удалить промежуточный буфер
  Buff.Free;
end;


procedure TsgeGraphicOpenGLDrawObjectItemSprite.Update(Element: TsgeDisplayElementItemBase);
begin
  //Метод предка
  inherited Update(Element);

  //Найти спрайт в таблице
  FGLSprite := OpenGLSpriteTable.Add(TsgeDisplayElementItemSprite(Element).Sprite);

  //Обновить текстурный буфер
  SetTexBuffer;
end;



end.

