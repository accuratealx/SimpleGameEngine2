{
Пакет             Simple Game Engine 2
Файл              sgeShellLineList.pas
Версия            1.2
Создан            10.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс списка строк линий оболочки
}
{$Include Defines.inc}

unit sgeShellLineList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateCollection,
  sgeColor,
  sgeShellLine;


type
  TsgeShellLineList = class(specialize TsgeTemplateCollection<TsgeShellLine>)
  private
    FMaxLines: Word;

    procedure CheckNumberItems;

    procedure SetMaxLines(ALines: Word);
  public
    constructor Create(MaxLines: Word = 1024);

    function Add: TsgeShellLine;
    function Add(Text: String; Color: TsgeColor): TsgeShellLine;
    function Add(Text: String; Color: TsgeColor; BGColor: TsgeColor): TsgeShellLine;

    property MaxLines: Word read FMaxLines write SetMaxLines;
  end;



implementation


procedure TsgeShellLineList.CheckNumberItems;
begin
  //Удалить лишние строки с начала
  while FCount > FMaxLines do
    Delete(0);
end;


procedure TsgeShellLineList.SetMaxLines(ALines: Word);
begin
  FMaxLines := ALines;

  //Проверить количество элементов
  CheckNumberItems;
end;


constructor TsgeShellLineList.Create(MaxLines: Word);
begin
  inherited Create(True);

  SetMaxLines(MaxLines);
end;


function TsgeShellLineList.Add: TsgeShellLine;
begin
  Result := TsgeShellLine.Create;

  inherited Add(Result);

  CheckNumberItems;
end;


function TsgeShellLineList.Add(Text: String; Color: TsgeColor): TsgeShellLine;
begin
  Result := TsgeShellLine.Create;
  Result.Add(Text, Color);

  inherited Add(Result);

  CheckNumberItems;
end;


function TsgeShellLineList.Add(Text: String; Color: TsgeColor; BGColor: TsgeColor): TsgeShellLine;
begin
  Result := TsgeShellLine.Create;
  Result.Add(Text, Color, BGColor);

  inherited Add(Result);

  CheckNumberItems;
end;


end.

