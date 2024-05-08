{
Пакет             Simple Game Engine 2
Файл              .pas
Версия            1.0
Создан            05.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Графика: Шейдер: Добавить
}
{$Include Defines.inc}

unit sgeEventGraphicShaderAdd;

{$mode ObjFPC}{$H+}

interface

uses
  sgeMemoryStream, sgeEventBase;


const
  Event_Graphic_ShaderAdd = 'Graphic.Shader.Add';


type
  TsgeEventGraphicShaderAdd = class(TsgeEventBase)
  private
    FShaderName: String;
    FShaderStream: TsgeMemoryStream;
  protected
    function GetName: ShortString; override;
  public
    constructor Create(ShaderName: String; Stream: TsgeMemoryStream);

    function Copy: TsgeEventBase; override;

    property ShaderName: String read FShaderName;
    property ShaderStream: TsgeMemoryStream read FShaderStream;
  end;

implementation


function TsgeEventGraphicShaderAdd.GetName: ShortString;
begin
  Result := Event_Graphic_ShaderAdd;
end;


constructor TsgeEventGraphicShaderAdd.Create(ShaderName: String; Stream: TsgeMemoryStream);
begin
  FShaderName := ShaderName;
  FShaderStream := Stream;
end;


function TsgeEventGraphicShaderAdd.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicShaderAdd.Create(FShaderName, FShaderStream);
end;



end.

