{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLShaderProgramTable.pas
Версия            1.0
Создан            31.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Хэш-таблица шейдерных программ
}
{$Include Defines.inc}

unit sgeGraphicOpenGLShaderProgramTable;

{$mode ObjFPC}{$H+}

interface

uses
  Contnrs,
  sgeGraphicOpenGLShaderProgram;

type
  TsgeGraphicOpenGLShaderProgramTable = class
  private
    FTable: TFPHashObjectList;

  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(ShaderProgram: TsgeGraphicOpenGLShaderProgram);
    procedure Delete(Name: String);
    procedure Clear;
    function  Get(Name: String): TsgeGraphicOpenGLShaderProgram;
  end;

var
  OpenGLShaderProgramTable: TsgeGraphicOpenGLShaderProgramTable;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'GraphicOpenGLShaderProgramTable';

  Err_EmptyProgram = 'EmptyProgram';
  Err_ProgramNotFound = 'ProgramNotFound';
  Err_ProgramExist = 'ProgramExist';


constructor TsgeGraphicOpenGLShaderProgramTable.Create;
begin
  FTable := TFPHashObjectList.Create(True);
end;


destructor TsgeGraphicOpenGLShaderProgramTable.Destroy;
begin
  FTable.Free;
end;


procedure TsgeGraphicOpenGLShaderProgramTable.Add(ShaderProgram: TsgeGraphicOpenGLShaderProgram);
begin
  if ShaderProgram = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyProgram);

  //Добавить в список
  if FTable.FindIndexOf(ShaderProgram.Name) = -1 then
    FTable.Add(ShaderProgram.Name, ShaderProgram)
  else
    raise EsgeException.Create(_UNITNAME, Err_ProgramExist, ShaderProgram.Name);
end;


procedure TsgeGraphicOpenGLShaderProgramTable.Delete(Name: String);
var
  Idx: Integer;
begin
  //Найти индекс
  Idx := FTable.FindIndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_ProgramNotFound, Name);

  //Удалить элемент
  FTable.Delete(Idx);
end;


procedure TsgeGraphicOpenGLShaderProgramTable.Clear;
begin
  FTable.Clear;
end;


function TsgeGraphicOpenGLShaderProgramTable.Get(Name: String): TsgeGraphicOpenGLShaderProgram;
var
  Idx: Integer;
begin
  //Найти индекс
  Idx := FTable.FindIndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_ProgramNotFound, Name);

  //Результат
  Result := TsgeGraphicOpenGLShaderProgram(FTable.Items[Idx]);
end;


initialization
begin
  OpenGLShaderProgramTable := TsgeGraphicOpenGLShaderProgramTable.Create;
end;


finalization
begin
  OpenGLShaderProgramTable.Free;
end;



end.

