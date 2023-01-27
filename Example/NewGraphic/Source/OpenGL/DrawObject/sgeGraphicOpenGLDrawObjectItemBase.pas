{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectItemBase.pas
Версия            1.0
Создан            27.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Базовый
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectItemBase;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeGraphicOpenGL, sgeGraphicOpenGLTypes, sgeGraphicOpenGLShaderProgram,
  sgeGraphicOpenGLVertexArrayObject, sgeGraphicOpenGLBuffer;

type
  TsgeGraphicOpenGLDrawObjectItemBase = class
  protected
    FVAO: TsgeGraphicOpenGLVertexArrayObject;
    FVertexBuffer: TsgeGraphicOpenGLBuffer;
    FShaderProgram: TsgeGraphicOpenGLShaderProgram;


    FPosition: TsgeFloatPoint;
  public
    constructor Create(ShaderProgram: TsgeGraphicOpenGLShaderProgram);
    destructor  Destroy; override;

    procedure Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeLayerInfo); virtual; abstract;
  end;

implementation

uses
  sgeErrors;

const
  _UNITNAME = 'GraphicOpenGLDrawObjectItemBase';

  Err_EmptyShaderProgram = 'EmptyShaderProgram';


constructor TsgeGraphicOpenGLDrawObjectItemBase.Create(ShaderProgram: TsgeGraphicOpenGLShaderProgram);
begin
  if ShaderProgram = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyShaderProgram);

  //Запомнить программу
  FShaderProgram := ShaderProgram;

  //Создать вершинный буфер
  FVertexBuffer := TsgeGraphicOpenGLBuffer.Create;

  //Создать VAO
  FVAO := TsgeGraphicOpenGLVertexArrayObject.Create;


  FVAO.Attach;
  FVertexBuffer.Attach;
  FVAO.BindPosition(FVertexBuffer);
end;


destructor TsgeGraphicOpenGLDrawObjectItemBase.Destroy;
begin
  FVertexBuffer.Free;
  FVAO.Free;
end;



end.

