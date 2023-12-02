{
Пакет             Simple Game Engine 2
Файл              sgeGUIFormList.pas
Версия            1.3
Создан            04.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Список форм
}
{$Include Defines.inc}

unit sgeGUIFormList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateCollection, sgeGUIForm;


type
  TsgeGUIFormList = class(specialize TsgeTemplateCollection<TsgeGUIForm>)
  private
  public
    procedure ToTopIndex(Form: TsgeGUIForm);                        //Переместить форму в конец списка

    function  IndexOf(Form: TsgeGUIForm): Integer;

    procedure Delete(Index: Integer);
    procedure Delete(Form: TsgeGUIForm);
  end;


implementation


procedure TsgeGUIFormList.ToTopIndex(Form: TsgeGUIForm);
var
  Idx: Integer;
begin
  Idx := IndexOf(Form);
  if Idx <> -1 then
  begin
    //Удалить из текущей позиции
    Delete(Idx);

    //Добавить форму в хвост
    Add(Form);
  end;
end;


function TsgeGUIFormList.IndexOf(Form: TsgeGUIForm): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to FCount - 1 do
    if FList[i] = Form then
      Exit(i);
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
  if Idx <> -1 then
    inherited Delete(Idx);
end;



end.

