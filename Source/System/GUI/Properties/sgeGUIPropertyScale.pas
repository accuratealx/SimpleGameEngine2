{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyScale.pas
Версия            1.1
Создан            30.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Масштаб
}
{$Include Defines.inc}

unit sgeGUIPropertyScale;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes,
  sgeGUIProperty, sgeGUIPropertyFloatPoint;


type
  //Способ масштабирования
  TsgeGUIPropertyScaleMode = (
    smNormal,   //По размеру спрайта
    smStretch,  //Растянуть под елемент
    smFitIn,    //Вписать в элемент
    smFitOut,   //Описать элемент
    smUserSize, //Пользовательский размер
    smUserScale //Пользовательский масштаб
  );


  TsgeGUIPropertyScale = class(TsgeGUIProperty)
  private
    FMode: TsgeGUIPropertyScaleMode;
    FUserSize: TsgeGUIPropertyFloatPoint;
    FUserScale: TsgeGUIPropertyFloatPoint;

    procedure SetMode(AMode: TsgeGUIPropertyScaleMode);

    function GetUserSize: TsgeGUIPropertyFloatPoint;
    function GetUserScale: TsgeGUIPropertyFloatPoint;
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;

    property Mode: TsgeGUIPropertyScaleMode read FMode write SetMode;
    property UserSize: TsgeGUIPropertyFloatPoint read GetUserSize;
    property UserScale: TsgeGUIPropertyFloatPoint read GetUserScale;
  end;


  TsgeGUIPropertyScaleExt = class(TsgeGUIPropertyScale)
  public
    function GetSize(ElementWidth, ElementHeight, SpriteWidth, SpriteHeight: Integer): TsgeIntPoint;
  end;



implementation


procedure TsgeGUIPropertyScale.SetMode(AMode: TsgeGUIPropertyScaleMode);
begin
  if FMode = AMode then
    Exit;

  FMode := AMode;
  UpdateParent;
end;


function TsgeGUIPropertyScale.GetUserSize: TsgeGUIPropertyFloatPoint;
begin
  Result := FUserSize;
end;


function TsgeGUIPropertyScale.GetUserScale: TsgeGUIPropertyFloatPoint;
begin
  Result := FUserScale;
end;


constructor TsgeGUIPropertyScale.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FUserSize := TsgeGUIPropertyFloatPoint.Create(AOwner, 0, 0);
  FUserScale := TsgeGUIPropertyFloatPoint.Create(AOwner, 1, 1);

  FMode := smStretch;
end;


destructor TsgeGUIPropertyScale.Destroy;
begin
  FUserScale.Free;
  FUserSize.Free;

  inherited Destroy;
end;


function TsgeGUIPropertyScaleExt.GetSize(ElementWidth, ElementHeight, SpriteWidth, SpriteHeight: Integer): TsgeIntPoint;
begin
  case FMode of
    smNormal:
      Result := sgeGetIntPoint(SpriteWidth, SpriteHeight);

    smStretch:
      Result := sgeGetIntPoint(ElementWidth, ElementHeight);

    smFitIn:
      Result := sgeFitRectIn(ElementWidth, ElementHeight, SpriteWidth, SpriteHeight);

    smFitOut:
      Result := sgeFitRectOut(ElementWidth, ElementHeight, SpriteWidth, SpriteHeight);

    smUserSize:
      Result := sgeGetIntPoint(Round(FUserSize.X), Round(FUserSize.Y));

    smUserScale:
      Result := sgeGetIntPoint(Round(SpriteWidth * FUserScale.X), Round(SpriteHeight * FUserScale.Y));
  end;
end;



end.

