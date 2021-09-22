{
Пакет             Simple Game Engine 2
Файл              sgeGUIFormList.pas
Версия            1.1
Создан            04.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Список форм
}
{$Include Defines.inc}

unit sgeGUIFormList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateObjectCollection, sgeGUIForm;


type
  TsgeGUIFormListTemplate = specialize TsgeTemplateObjectCollection<TsgeGUIForm>;


  TsgeGUIFormList = class(TsgeGUIFormListTemplate)
  private
  public
    function  IndexOf(Form: TsgeGUIForm): Integer;

    procedure Delete(Index: Integer);
    procedure Delete(Form: TsgeGUIForm);
  end;


implementation



function TsgeGUIFormList.IndexOf(Form: TsgeGUIForm): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to FCount - 1 do
    if FList[i] = Form then Exit(i);
end;


procedure TsgeGUIFormList.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;


procedure TsgeGUIFormList.Delete(Form: TsgeGUIForm);
var
  Idx: Integer;
begin
  Idx := IndexOf(Form);
  if Idx <> -1 then inherited Delete(Idx);
end;



end.

