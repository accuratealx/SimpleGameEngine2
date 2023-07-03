{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLShaderProgram.pas
Версия            1.2
Создан            21.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Шейдерная программа
}
{$Include Defines.inc}

unit sgeGraphicOpenGLShaderProgram;

{$mode ObjFPC}{$H+}

interface

uses
  dglOpenGL,
  sgeTypes, sgeMemoryStream,
  sgeGraphicColor,
  sgeGraphicOpenGLShader;

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
    constructor Create(Name: String; VertexShader, FragmentShader: String);
    constructor Create(Name: String; Source: TsgeMemoryStream);
    constructor Create(Name: String; Source: String);
    destructor  Destroy; override;

    procedure SetValue(Name: String; Value: Boolean);
    procedure SetValue(Name: String; Value: Single);
    procedure SetValue(Name: String; Value: Integer);

    procedure SetScreenSize(Value: TsgeFloatPoint);
    procedure SetLayer(Value: TsgeFloatRect);
    procedure SetPos(Value: TsgeFloatPoint);
    procedure SetColor(Value: TsgeColor);
    procedure SetScale(Value: TsgeFloatPoint);
    procedure SetOrigin(Value: TsgeFloatPoint);
    procedure SetAngle(Value: Single);

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

  Err_EmptySource = 'EmptySource';
  Err_EmptyVertexShader = 'EmptyVertexShader';
  Err_EmptyFragmentShader = 'EmptyFragmentShader';
  Err_LinkError = 'LinkError';
  Err_ParamNotFound = 'ParamNotFound';
  Err_ProgramSeparatorNotFound = 'ProgramSeparatorNotFound';


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

  //Запомнить имя
  FName := Name;

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

  //Запомнить имя
  FName := Name;

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


constructor TsgeGraphicOpenGLShaderProgram.Create(Name: String; VertexShader, FragmentShader: String);
var
  VertexS, FragmentS: TsgeGraphicOpenGLShader;
begin
  //Запомнить имя
  FName := Name;

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


constructor TsgeGraphicOpenGLShaderProgram.Create(Name: String; Source: TsgeMemoryStream);
begin
  //Проверить на существование объектов
  if Source = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySource);

  //Создать из строки
  Create(Name, Source.ToString);
end;


constructor TsgeGraphicOpenGLShaderProgram.Create(Name: String; Source: String);
const
  PROGRAM_SEPARATOR = 'PROGRAM_SEPARATOR';
var
  Vertex, Fragment: String;
  Idx: Integer;
begin
  //Найти расположение разделителя
  Idx := sgePos(PROGRAM_SEPARATOR, Source);
  if Idx = 0 then
    raise EsgeException.Create(_UNITNAME, Err_ProgramSeparatorNotFound, Name);

  //Выделить вертексную программу
  Vertex := sgeTrim(Copy(Source, 1, Idx - 1));

  //Выделить вершинную программу
  Fragment := sgeTrim(Copy(Source, Idx + SizeOf(PROGRAM_SEPARATOR), Length(Source) - Idx + SizeOf(PROGRAM_SEPARATOR)));

  //Создать из строк
  Create(Name, Vertex, Fragment);
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


procedure TsgeGraphicOpenGLShaderProgram.SetScreenSize(Value: TsgeFloatPoint);
begin
  glUniform2fv(GetParamIndex('ScreenSize'), 1, @Value);
end;


procedure TsgeGraphicOpenGLShaderProgram.SetLayer(Value: TsgeFloatRect);
begin
  glUniform4fv(GetParamIndex('Layer'), 1, @Value);
end;


procedure TsgeGraphicOpenGLShaderProgram.SetPos(Value: TsgeFloatPoint);
begin
  glUniform2fv(GetParamIndex('Pos'), 1, @Value);
end;


procedure TsgeGraphicOpenGLShaderProgram.SetColor(Value: TsgeColor);
begin
  glUniform4fv(GetParamIndex('Color'), 1, @Value);
end;


procedure TsgeGraphicOpenGLShaderProgram.SetScale(Value: TsgeFloatPoint);
begin
  glUniform2fv(GetParamIndex('Scale'), 1, @Value);
end;


procedure TsgeGraphicOpenGLShaderProgram.SetOrigin(Value: TsgeFloatPoint);
begin
  glUniform2fv(GetParamIndex('Origin'), 1, @Value);
end;


procedure TsgeGraphicOpenGLShaderProgram.SetAngle(Value: Single);
begin
  glUniform1fv(GetParamIndex('Angle'), 1, @Value);
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

