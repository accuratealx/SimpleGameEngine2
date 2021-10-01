{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyDrawMethod.pas
Версия            1.0
Создан            01.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: режим вывода спрайта
}
{$Include Defines.inc}

unit sgeGUIPropertyDrawMethod;

{$mode objfpc}{$H+}

interface

uses
  sgeGUIProperty, sgeGUIPropertySegmentOffset;


type
  //Режим вывода спрайта (Одним вызовом, как кнопку)
  TsgeGUIPropertyDrawMethodMode = (dmmNormal, dmmSegment);


  TsgeGUIPropertyDrawMethod = class(TsgeGUIProperty)
  private
    FMode: TsgeGUIPropertyDrawMethodMode;
    FOffset: TsgeGUIPropertySegmentOffset;                          //Смещение для сегментного вывода

    procedure SetMode(AMode: TsgeGUIPropertyDrawMethodMode);
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;

    property Mode: TsgeGUIPropertyDrawMethodMode read FMode write SetMode;
    property Offset: TsgeGUIPropertySegmentOffset read FOffset;
  end;



implementation



procedure TsgeGUIPropertyDrawMethod.SetMode(AMode: TsgeGUIPropertyDrawMethodMode);
begin
  if FMode = AMode then Exit;

  FMode := AMode;
  UpdateParent;
end;


constructor TsgeGUIPropertyDrawMethod.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FOffset := TsgeGUIPropertySegmentOffset.Create(AOwner);
end;


destructor TsgeGUIPropertyDrawMethod.Destroy;
begin
  FOffset.Free;

  inherited Destroy;
end;



end.

