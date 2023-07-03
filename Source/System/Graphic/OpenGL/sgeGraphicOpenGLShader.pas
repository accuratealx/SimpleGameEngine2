{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLShader.pas
Версия            1.0
Создан            21.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Класс шейдера
}
{$Include Defines.inc}

unit sgeGraphicOpenGLShader;

{$mode ObjFPC}{$H+}

interface

uses
  sgeMemoryStream,
  dglOpenGL;

type
  //Тип шейдера
  TsgeGraphicOpenGLShaderType = (stVertex, stFragment);


  //Класс шейдера
  TsgeGraphicOpenGLShader = class
  private
    FHandle: GLHandle;
    FShaderType: TsgeGraphicOpenGLShaderType;
    FSource: String;

    procedure Prepare(AType: TsgeGraphicOpenGLShaderType; ASource: String);
  public
    constructor Create(ShaderType: TsgeGraphicOpenGLShaderType; Source: String);
    constructor Create(ShaderType: TsgeGraphicOpenGLShaderType; Source: TsgeMemoryStream);
    destructor  Destroy; override;

    property Handle: GLHandle read FHandle;
    property ShaderType: TsgeGraphicOpenGLShaderType read FShaderType;
    property Source: String read FSource;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'GraphicOpenGLShader';

  Err_CompileVertexError = 'CompileVertexError';
  Err_CompileFragmentError = 'CompileFragmentError';


procedure TsgeGraphicOpenGLShader.Prepare(AType: TsgeGraphicOpenGLShaderType; ASource: String);
var
  T, Len: GLint;
  Src: PChar;
  S, ErrStr: string;
begin
  //Определить тип
  case AType of
    stVertex:
      T := GL_VERTEX_SHADER;

    stFragment:
      T := GL_FRAGMENT_SHADER;
  end;

  //Выделить память под шейдер
  if FHandle = 0 then
    FHandle := glCreateShader(T);

  //Установить исходник шейдера
  Len := Length(ASource);
  Src := PChar(FSource);
  glShaderSource(FHandle, 1, @Src, @Len);

  //Собрать шейдер
  glCompileShader(FHandle);

  //Проверить на ошибки
  glGetShaderiv(FHandle, GL_COMPILE_STATUS, @T);
  if T = 0 then
  begin
    //Узнать длину строки с ошибкой
    glGetShaderiv(FHandle, GL_INFO_LOG_LENGTH, @T);

    //Прочитать сообщение
    S := '';
    SetLength(S, T - 1);
    glGetShaderInfoLog(FHandle, T - 1, nil, @s[1]);

    case FShaderType of
      stVertex:
        ErrStr := Err_CompileVertexError;

      stFragment:
        ErrStr := Err_CompileFragmentError;
    end;

    raise EsgeException.Create(_UNITNAME, ErrStr, S);
  end;
end;


constructor TsgeGraphicOpenGLShader.Create(ShaderType: TsgeGraphicOpenGLShaderType; Source: String);
begin
  //Сохранить параметры
  FShaderType := ShaderType;
  FSource := Source;

  //Создать шейдер
  Prepare(FShaderType, FSource);
end;


constructor TsgeGraphicOpenGLShader.Create(ShaderType: TsgeGraphicOpenGLShaderType; Source: TsgeMemoryStream);
begin
  //Сохранить параметры
  FShaderType := ShaderType;
  FSource := Source.ToString;

  //Создать шейдер
  Prepare(FShaderType, FSource);
end;


destructor TsgeGraphicOpenGLShader.Destroy;
begin
  if FHandle <> 0 then
  begin
    FHandle := 0;
    glDeleteShader(FHandle);
  end;
end;



end.

