{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLShaderProgram.pas
Версия            1.0
Создан            21.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Шедерная программа
}
{$Include Defines.inc}

unit sgeGraphicOpenGLShaderProgram;

{$mode ObjFPC}{$H+}

interface

uses
  dglOpenGL,
  sgeTypes, sgeMemoryStream,
  sgeGraphicOpenGLTypes, sgeGraphicColor, sgeGraphicOpenGLShader;

type
  TsgeGraphicOpenGLShaderProgram = class
  private
    FName: String;
    FHandle: GLHandle;

    function  GetParamIndex(Name: String): Integer;
    procedure Prepare(AVertex, AFragment: GLHandle);
  public
    constructor Create(Name: String; VertexShader, FragmentShader: TsgeGraphicOpenGLShader);
    constructor Create(Name: String; VertexShader, FragmentShader: TsgeMemoryStream);
    destructor  Destroy; override;

    procedure SetValue(Name: String; Value: Boolean);
    procedure SetValue(Name: String; Value: Single);
    procedure SetValue(Name: String; Value: Integer);

    procedure SetColor(Color: TsgeColor);
    procedure SetPos(X, Y: Single);
    procedure SetPos(Pos: TsgeFloatPoint);
    procedure SetScreenSize(Size: TsgeFloatPoint);
    procedure SetLayer(Layer: TsgeLayerInfo);
    procedure SetLayer(X, Y: Single; Scale: Single);


    procedure Attach;
    procedure Detach;

    property Name: String read FName;
    property Handle: GLHandle read FHandle;
  end;


implementation

uses
  sgeSystemUtils, sgeErrors;

const
  _UNITNAME = 'GraphicOpenGLShaderProgram';

  Err_EmptyVertexShader = 'EmptyVertexShader';
  Err_EmptyFragmentShader = 'EmptyFragmentShader';
  Err_LinkError = 'LinkError';
  Err_ParamNotFound = 'ParamNotFound';


function TsgeGraphicOpenGLShaderProgram.GetParamIndex(Name: String): Integer;
begin
  //Найти индекс переменной
  Result := glGetUniformLocation(FHandle, PChar(Name));

  //Проверить на ошибку
  if Result = -1 then
    raise EsgeException.Create(_UNITNAME, Err_ParamNotFound, Name);
end;


procedure TsgeGraphicOpenGLShaderProgram.Prepare(AVertex, AFragment: GLHandle);
var
  T: GLint;
  S: string;
begin
  //Выделить память под программу
  if FHandle = 0 then
    FHandle := glCreateProgram();

  //Привязать вершинный шейдер
  glAttachShader(FHandle, AVertex);

  //Привязать фрагментный шейдер
  glAttachShader(FHandle, AFragment);

  //Собрать все до кучи
  glLinkProgram(FHandle);

  //Проверить на ошибки
  glGetProgramiv(FHandle, GL_LINK_STATUS, @T);
  if T = 0 then
  begin
    //Узнать длину строки с ошибкой
    glGetProgramiv(FHandle, GL_INFO_LOG_LENGTH, @T);

    //Прочитать сообщение
    S := '';
    SetLength(S, T - 1);
    glGetProgramInfoLog(FHandle, T - 1, nil, @s[1]);

    raise EsgeException.Create(_UNITNAME, Err_LinkError, S);
  end;
end;


constructor TsgeGraphicOpenGLShaderProgram.Create(Name: String; VertexShader, FragmentShader: TsgeGraphicOpenGLShader);
begin
  //Проверить на существование объектов
  if VertexShader = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyVertexShader);

  if FragmentShader = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyFragmentShader);

  //Слинковать
  Prepare(VertexShader.Handle, FragmentShader.Handle);
end;


constructor TsgeGraphicOpenGLShaderProgram.Create(Name: String; VertexShader, FragmentShader: TsgeMemoryStream);
var
  VertexS, FragmentS: TsgeGraphicOpenGLShader;
begin
  //Проверить на существование объектов
  if VertexShader = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyVertexShader);

  if FragmentShader = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyFragmentShader);

  VertexS := nil;
  FragmentS := nil;
  try

    try
      //Создать объекты
      VertexS := TsgeGraphicOpenGLShader.Create(stVertex, VertexShader);
      FragmentS := TsgeGraphicOpenGLShader.Create(stFragment, FragmentShader);

      //Слинковать
      Prepare(VertexS.Handle, FragmentS.Handle);
    except
      on E: EsgeException do
        raise EsgeException.Create(_UNITNAME, Err_LinkError, FName, E.Message);
    end;

  finally
    VertexS.Free;
    FragmentS.Free;
  end;
end;


destructor TsgeGraphicOpenGLShaderProgram.Destroy;
begin
  if FHandle <> 0 then
    glDeleteProgram(FHandle);
end;


procedure TsgeGraphicOpenGLShaderProgram.SetValue(Name: String; Value: Boolean);
begin
  glUniform1i(GetParamIndex(Name), GLuint(Value));
end;


procedure TsgeGraphicOpenGLShaderProgram.SetValue(Name: String; Value: Single);
begin
  glUniform1f(GetParamIndex(Name), Value);
end;


procedure TsgeGraphicOpenGLShaderProgram.SetValue(Name: String; Value: Integer);
begin
  glUniform1i(GetParamIndex(Name), Value);
end;


procedure TsgeGraphicOpenGLShaderProgram.SetColor(Color: TsgeColor);
begin
  glUniform4fv(GetParamIndex('Color'), 1, @Color);
end;


procedure TsgeGraphicOpenGLShaderProgram.SetPos(X, Y: Single);
var
  Pos: TsgeFloatPoint;
begin
  Pos.X := X;
  Pos.Y := Y;
  glUniform2fv(GetParamIndex('Pos'), 1, @Pos);
end;


procedure TsgeGraphicOpenGLShaderProgram.SetPos(Pos: TsgeFloatPoint);
begin
  glUniform2fv(GetParamIndex('Pos'), 1, @Pos);
end;


procedure TsgeGraphicOpenGLShaderProgram.SetScreenSize(Size: TsgeFloatPoint);
begin
  glUniform2fv(GetParamIndex('ScreenSize'), 1, @Size);
end;


procedure TsgeGraphicOpenGLShaderProgram.SetLayer(Layer: TsgeLayerInfo);
begin
  glUniform3fv(GetParamIndex('Layer'), 1, @Layer);
end;


procedure TsgeGraphicOpenGLShaderProgram.SetLayer(X, Y: Single; Scale: Single);
var
  Layer: TsgeLayerInfo;
begin
  Layer.X := X;
  Layer.Y := Y;
  Layer.Scale := Scale;
  glUniform3fv(GetParamIndex('Layer'), 1, @Layer);
end;


procedure TsgeGraphicOpenGLShaderProgram.Attach;
begin
  glUseProgram(FHandle);
end;


procedure TsgeGraphicOpenGLShaderProgram.Detach;
begin
  glUseProgram(0);
end;



end.

