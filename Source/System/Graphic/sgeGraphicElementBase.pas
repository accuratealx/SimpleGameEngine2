{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementBase.pas
Версия            1.0
Создан            09.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс базового элемента вывода
}
{$Include Defines.inc}

unit sgeGraphicElementBase;

{$mode objfpc}{$H+}

interface

uses
  sgeGraphic;


const
  Err_CantCreateGraphicElement = 'CantCreateGraphicElement';


type
  TsgeGraphicElementBase = class
  private
    FVisible: Boolean;                                    //Флаг видимости
    FNeedUpdate: Boolean;                                 //Флаг обновления данных
    FNeedDelete: Boolean;                                 //Флаг удаления

  protected
    procedure AfterConstruction; override;

  public
    procedure ApplySettings;
    procedure Update;
    procedure Delete;

    //Пользовательские функции
    procedure UpdateData; virtual; abstract;                  //ООбновление данных
    procedure Draw(Graphic: TsgeGraphic); virtual; abstract;  //Отрисовка элемента

    property Visible: Boolean read FVisible write FVisible;
    property NeedUpdate: Boolean read FNeedUpdate;
    property NeedDelete: Boolean read FNeedDelete;
  end;


implementation


procedure TsgeGraphicElementBase.AfterConstruction;
begin
  FVisible := True;
end;


procedure TsgeGraphicElementBase.ApplySettings;
begin
  //Применить данные
  UpdateData;

  //Снять флаг обновления
  FNeedUpdate := False;
end;


procedure TsgeGraphicElementBase.Update;
begin
  if FNeedUpdate = True then Exit;
  FNeedUpdate := True;
end;


procedure TsgeGraphicElementBase.Delete;
begin
  FNeedDelete := True;
end;


end.

